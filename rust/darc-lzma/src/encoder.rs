//! Top-level encoder: the optimal parser driving the verified symbol layer.
//!
//! A port of `LzmaEnc_CodeOneBlock` (`LzmaEnc.c:2400`) plus `GetOptimum`
//! (via [`crate::optimum`]). The symbol-coding methods (literal / matched-literal
//! / length / distance / rep) are unchanged from the round-trip-verified layer;
//! the optimal parser replaces the earlier greedy parse to achieve byte-exact
//! output against `LzmaEnc_MemEncode`.

// The DP mirrors the C's explicit index loops, where indices double as values
// (e.g. a rep index is also the encoded rep distance).
#![allow(clippy::needless_range_loop)]

use crate::matchfinder::{Match, MatchFinder};
use crate::optimum::{
    self, INFINITY_PRICE, MARK_LIT, NUM_OPTS, Optimal, Prices, REP_LEN_COUNT, len_price_update,
    len_to_pos_state, lit_get_price, lit_matched_get_price,
};
use crate::price::ProbPrices;
use crate::props::LzmaProps;
use crate::rangecoder::RangeEncoder;
use crate::state::{
    END_POS_MODEL_INDEX, LITERAL_NEXT_STATES, MATCH_LEN_MAX, MATCH_LEN_MIN, MATCH_NEXT_STATES,
    NUM_ALIGN_BITS, NUM_FULL_DISTANCES, NUM_POS_SLOT_BITS, NUM_STATES, PROB_INIT_VALUE,
    REP_NEXT_STATES, SHORT_REP_NEXT_STATES, START_POS_MODEL_INDEX,
};

const NUM_LIT_STATES: u32 = 7;
const NUM_POS_STATES_MAX: usize = 16;
const POS_SLOT_TABLE_LEN: usize = 1 << NUM_POS_SLOT_BITS;
const ALIGN_TABLE_LEN: usize = 1 << NUM_ALIGN_BITS;
const ALIGN_MASK: u32 = (1 << NUM_ALIGN_BITS) - 1;
const NUM_REPS: usize = 4;

const STATE_LIT_AFTER_MATCH: u32 = 4;
const STATE_LIT_AFTER_REP: u32 = 5;
const STATE_MATCH_AFTER_LIT: u32 = 7;
const STATE_REP_AFTER_LIT: u32 = 8;

#[inline]
fn is_lit_state(s: u32) -> bool {
    s < NUM_LIT_STATES
}

/// Length-coder probability model (`CLenEnc`).
pub struct LenEnc {
    pub choice: u16,
    pub choice2: u16,
    pub low: [[u16; 8]; NUM_POS_STATES_MAX],
    pub mid: [[u16; 8]; NUM_POS_STATES_MAX],
    pub high: [u16; 256],
}

impl LenEnc {
    fn new() -> Self {
        LenEnc {
            choice: PROB_INIT_VALUE,
            choice2: PROB_INIT_VALUE,
            low: [[PROB_INIT_VALUE; 8]; NUM_POS_STATES_MAX],
            mid: [[PROB_INIT_VALUE; 8]; NUM_POS_STATES_MAX],
            high: [PROB_INIT_VALUE; 256],
        }
    }

    fn encode(&mut self, rc: &mut RangeEncoder, sym: u32, pos_state: usize) {
        if sym < 8 {
            rc.encode_bit(&mut self.choice, 0);
            rc.encode_tree(&mut self.low[pos_state], 3, sym);
        } else {
            rc.encode_bit(&mut self.choice, 1);
            if sym < 16 {
                rc.encode_bit(&mut self.choice2, 0);
                rc.encode_tree(&mut self.mid[pos_state], 3, sym - 8);
            } else {
                rc.encode_bit(&mut self.choice2, 1);
                rc.encode_tree(&mut self.high, 8, sym - 16);
            }
        }
    }
}

/// Encoder probability models.
pub struct EncProbs {
    pub is_match: [[u16; NUM_POS_STATES_MAX]; NUM_STATES],
    pub is_rep: [u16; NUM_STATES],
    pub is_rep_g0: [u16; NUM_STATES],
    pub is_rep_g1: [u16; NUM_STATES],
    pub is_rep_g2: [u16; NUM_STATES],
    pub is_rep0_long: [[u16; NUM_POS_STATES_MAX]; NUM_STATES],
    pub pos_slot: [[u16; POS_SLOT_TABLE_LEN]; 4],
    pub spec_pos: [u16; NUM_FULL_DISTANCES as usize],
    pub align: [u16; ALIGN_TABLE_LEN],
    pub len: LenEnc,
    pub rep_len: LenEnc,
    pub literal: Vec<u16>,
}

impl EncProbs {
    fn new(lc: u32, lp: u32) -> Self {
        EncProbs {
            is_match: [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES],
            is_rep: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g0: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g1: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g2: [PROB_INIT_VALUE; NUM_STATES],
            is_rep0_long: [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES],
            pos_slot: [[PROB_INIT_VALUE; POS_SLOT_TABLE_LEN]; 4],
            spec_pos: [PROB_INIT_VALUE; NUM_FULL_DISTANCES as usize],
            align: [PROB_INIT_VALUE; ALIGN_TABLE_LEN],
            len: LenEnc::new(),
            rep_len: LenEnc::new(),
            literal: vec![PROB_INIT_VALUE; 0x300usize << (lc + lp)],
        }
    }
}

// ---- pure-price helpers (free fns to keep borrows simple) ----

#[inline]
fn price_short_rep(pr: &EncProbs, pp: &ProbPrices, state: usize, ps: usize) -> u32 {
    pp.price_0(pr.is_rep_g0[state]) + pp.price_0(pr.is_rep0_long[state][ps])
}

#[inline]
fn price_rep_0(pr: &EncProbs, pp: &ProbPrices, state: usize, ps: usize) -> u32 {
    pp.price_1(pr.is_match[state][ps])
        + pp.price_1(pr.is_rep0_long[state][ps])
        + pp.price_1(pr.is_rep[state])
        + pp.price_0(pr.is_rep_g0[state])
}

#[inline]
fn price_pure_rep(
    pr: &EncProbs,
    pp: &ProbPrices,
    rep_index: usize,
    state: usize,
    ps: usize,
) -> u32 {
    let prob = pr.is_rep_g0[state];
    if rep_index == 0 {
        pp.price_0(prob) + pp.price_1(pr.is_rep0_long[state][ps])
    } else {
        let mut price = pp.price_1(prob);
        let prob = pr.is_rep_g1[state];
        if rep_index == 1 {
            price += pp.price_0(prob);
        } else {
            price += pp.price_1(prob);
            price += pp.price(pr.is_rep_g2[state], (rep_index - 2) as u32);
        }
        price
    }
}

struct Encoder<'a> {
    rc: RangeEncoder,
    probs: EncProbs,
    prices: Prices,
    mf: MatchFinder<'a>,
    input: &'a [u8],
    opt: Vec<Optimal>,
    matches: Vec<u32>,
    mf_buf: Vec<Match>,
    state: u32,
    reps: [u32; 4],
    lc: u32,
    lp_mask: u32,
    pb_mask: u32,
    pb: u32,
    /// `props.write_end_mark`, consulted only in `finish`.
    props_write_end_mark: bool,
    num_fast_bytes: u32,
    additional_offset: u32,
    num_avail: u32,
    longest_match_len: u32,
    num_pairs: u32,
    back_res: u32,
    opt_cur: usize,
    opt_end: usize,
}

impl<'a> Encoder<'a> {
    fn new(input: &'a [u8], props: &LzmaProps) -> Self {
        let dist_table_size = {
            let mut i = (END_POS_MODEL_INDEX / 2) as usize;
            while i < 32 {
                if props.dict_size <= (1u32 << i) {
                    break;
                }
                i += 1;
            }
            (i * 2) as u32
        };
        Encoder {
            rc: RangeEncoder::new(),
            probs: EncProbs::new(props.lc as u32, props.lp as u32),
            prices: Prices::new(dist_table_size, props.fb),
            mf: MatchFinder::new(input, props),
            input,
            opt: vec![Optimal::default(); NUM_OPTS],
            matches: Vec::with_capacity(MATCH_LEN_MAX as usize * 2 + 2),
            mf_buf: Vec::with_capacity(MATCH_LEN_MAX as usize),
            state: 0,
            reps: [1, 1, 1, 1],
            lc: props.lc as u32,
            lp_mask: (1u32 << props.lp) - 1,
            pb_mask: (1u32 << props.pb) - 1,
            pb: props.pb as u32,
            props_write_end_mark: props.write_end_mark,
            num_fast_bytes: props.fb,
            additional_offset: 0,
            num_avail: 0,
            longest_match_len: 0,
            num_pairs: 0,
            back_res: 0,
            opt_cur: 0,
            opt_end: 0,
        }
    }

    #[inline]
    fn lit_base(&self, pos: u32, prev: u32) -> usize {
        let lit_state = (((pos & self.lp_mask) << self.lc) + (prev >> (8 - self.lc))) as usize;
        0x300 * lit_state
    }

    // ---- symbol encoders (verified layer) ----

    fn encode_literal(&mut self, pos: usize) {
        let ps = (pos as u32 & self.pb_mask) as usize;
        self.rc
            .encode_bit(&mut self.probs.is_match[self.state as usize][ps], 0);
        let byte = u32::from(self.input[pos]);
        let prev = if pos == 0 {
            0
        } else {
            u32::from(self.input[pos - 1])
        };
        let base = self.lit_base(pos as u32, prev);
        let table = &mut self.probs.literal[base..base + 0x300];
        if self.state < NUM_LIT_STATES {
            self.rc.encode_tree(table, 8, byte);
        } else {
            let match_byte = u32::from(self.input[pos - self.reps[0] as usize]);
            let mut offs = 0x100u32;
            let mut sym = byte | 0x100;
            let mut mb = match_byte;
            loop {
                mb <<= 1;
                let idx = (offs + (mb & offs) + (sym >> 8)) as usize;
                let bit = (sym >> 7) & 1;
                sym <<= 1;
                offs &= !(mb ^ sym);
                self.rc.encode_bit(&mut table[idx], bit);
                if sym >= 0x10000 {
                    break;
                }
            }
        }
        self.state = u32::from(LITERAL_NEXT_STATES[self.state as usize]);
    }

    fn encode_distance(&mut self, len: u32, dist0: u32) {
        let len_to_pos = len_to_pos_state(len);
        let pos_slot = optimum::get_pos_slot(dist0);
        self.rc.encode_tree(
            &mut self.probs.pos_slot[len_to_pos],
            NUM_POS_SLOT_BITS,
            pos_slot,
        );
        if pos_slot >= START_POS_MODEL_INDEX {
            let num_direct = (pos_slot >> 1) - 1;
            let base = (2 | (pos_slot & 1)) << num_direct;
            let footer = dist0 - base;
            if pos_slot < END_POS_MODEL_INDEX {
                self.rc.encode_tree_reverse(
                    &mut self.probs.spec_pos[base as usize..],
                    num_direct,
                    footer,
                );
            } else {
                self.rc
                    .encode_direct_bits(footer >> NUM_ALIGN_BITS, num_direct - NUM_ALIGN_BITS);
                self.rc.encode_tree_reverse(
                    &mut self.probs.align,
                    NUM_ALIGN_BITS,
                    footer & ALIGN_MASK,
                );
            }
        }
    }

    fn encode_match(&mut self, pos: usize, dist1: u32, len: u32) {
        let ps = (pos as u32 & self.pb_mask) as usize;
        self.rc
            .encode_bit(&mut self.probs.is_match[self.state as usize][ps], 1);
        self.rc
            .encode_bit(&mut self.probs.is_rep[self.state as usize], 0);
        let len_symbol = len - MATCH_LEN_MIN;
        self.probs.len.encode(&mut self.rc, len_symbol, ps);
        self.encode_distance(len, dist1 - 1);
        self.reps[3] = self.reps[2];
        self.reps[2] = self.reps[1];
        self.reps[1] = self.reps[0];
        self.reps[0] = dist1;
        self.state = u32::from(MATCH_NEXT_STATES[self.state as usize]);
    }

    fn encode_rep(&mut self, pos: usize, idx: usize, len: u32) {
        let ps = (pos as u32 & self.pb_mask) as usize;
        let s = self.state as usize;
        self.rc.encode_bit(&mut self.probs.is_match[s][ps], 1);
        self.rc.encode_bit(&mut self.probs.is_rep[s], 1);
        if idx == 0 {
            self.rc.encode_bit(&mut self.probs.is_rep_g0[s], 0);
            self.rc.encode_bit(&mut self.probs.is_rep0_long[s][ps], 1);
        } else {
            self.rc.encode_bit(&mut self.probs.is_rep_g0[s], 1);
            let dist = self.reps[idx];
            if idx == 1 {
                self.rc.encode_bit(&mut self.probs.is_rep_g1[s], 0);
            } else {
                self.rc.encode_bit(&mut self.probs.is_rep_g1[s], 1);
                if idx == 2 {
                    self.rc.encode_bit(&mut self.probs.is_rep_g2[s], 0);
                } else {
                    self.rc.encode_bit(&mut self.probs.is_rep_g2[s], 1);
                    self.reps[3] = self.reps[2];
                }
                self.reps[2] = self.reps[1];
            }
            self.reps[1] = self.reps[0];
            self.reps[0] = dist;
        }
        let len_symbol = len - MATCH_LEN_MIN;
        self.probs.rep_len.encode(&mut self.rc, len_symbol, ps);
        self.state = u32::from(REP_NEXT_STATES[self.state as usize]);
    }

    fn encode_short_rep(&mut self, pos: usize) {
        let ps = (pos as u32 & self.pb_mask) as usize;
        let s = self.state as usize;
        self.rc.encode_bit(&mut self.probs.is_match[s][ps], 1);
        self.rc.encode_bit(&mut self.probs.is_rep[s], 1);
        self.rc.encode_bit(&mut self.probs.is_rep_g0[s], 0);
        self.rc.encode_bit(&mut self.probs.is_rep0_long[s][ps], 0);
        self.state = u32::from(SHORT_REP_NEXT_STATES[self.state as usize]);
    }

    // ---- match finder driver ----

    /// `ReadMatchDistances`: fill `self.matches`, return the longest match length
    /// (extended to the true length when it reaches `numFastBytes`).
    fn read_match_distances(&mut self) -> u32 {
        self.additional_offset += 1;
        self.num_avail = self.mf.num_available();
        let cur_idx = self.mf.pos_index();
        self.mf.get_matches(&mut self.mf_buf);
        self.matches.clear();
        for m in &self.mf_buf {
            self.matches.push(m.len);
            self.matches.push(m.dist);
        }
        let num_pairs = self.matches.len() as u32;
        self.num_pairs = num_pairs;
        if num_pairs == 0 {
            return 0;
        }
        let len = self.matches[num_pairs as usize - 2];
        if len != self.num_fast_bytes {
            return len;
        }
        let num_avail = self.num_avail.min(MATCH_LEN_MAX) as usize;
        let back = self.matches[num_pairs as usize - 1] as usize + 1;
        let mut p2 = len as usize;
        while p2 != num_avail && self.input[cur_idx + p2] == self.input[cur_idx + p2 - back] {
            p2 += 1;
        }
        p2 as u32
    }

    fn move_pos(&mut self, num: u32) {
        if num != 0 {
            self.additional_offset += num;
            self.mf.skip(num);
        }
    }

    /// `Backward`: trace the optimal path back into `opt[optCur..optEnd]`, return
    /// the first op's length and set `back_res`.
    fn backward(&mut self, mut cur: usize) -> u32 {
        let mut wr = cur + 1;
        self.opt_end = wr;
        loop {
            let mut dist = self.opt[cur].dist;
            let mut len = self.opt[cur].len;
            let extra = self.opt[cur].extra;
            cur -= len as usize;
            if extra != 0 {
                wr -= 1;
                self.opt[wr].len = len;
                cur -= extra as usize;
                len = extra;
                if extra == 1 {
                    self.opt[wr].dist = dist;
                    dist = MARK_LIT;
                } else {
                    self.opt[wr].dist = 0;
                    len -= 1;
                    wr -= 1;
                    self.opt[wr].dist = MARK_LIT;
                    self.opt[wr].len = 1;
                }
            }
            if cur == 0 {
                self.back_res = dist;
                self.opt_cur = wr;
                return len;
            }
            wr -= 1;
            self.opt[wr].dist = dist;
            self.opt[wr].len = len;
        }
    }

    /// `WriteEndMarker` (`LzmaEnc.c:2100`): a match with length symbol 0 and an
    /// all-ones distance, which the decoder recognises as end-of-payload.
    ///
    /// The C's four blocks map onto this crate's range-coder API rather than
    /// being transliterated bit by bit:
    ///
    /// * `isMatch[state][posState] = 1`, `isRep[state] = 0` — a plain match
    /// * `LenEnc_Encode(.., 0, posState)` — length symbol 0
    /// * the pos-slot loop walks `m = (m << 1) + 1` encoding a 1 each time, which
    ///   is exactly `encode_tree` of the all-ones symbol over `NUM_POS_SLOT_BITS`
    /// * `EncodeDirectBits(((1 << (30 - kNumAlignBits)) - 1), 30 - kNumAlignBits)`
    /// * the align loop is `encode_tree_reverse` of the all-ones symbol
    fn write_end_marker(&mut self, pos: u32) {
        let ps = (pos & self.pb_mask) as usize;
        let s = self.state as usize;
        self.rc.encode_bit(&mut self.probs.is_match[s][ps], 1);
        self.rc.encode_bit(&mut self.probs.is_rep[s], 0);
        self.state = u32::from(MATCH_NEXT_STATES[s]);
        self.probs.len.encode(&mut self.rc, 0, ps);
        self.rc.encode_tree(
            &mut self.probs.pos_slot[0],
            NUM_POS_SLOT_BITS,
            (1 << NUM_POS_SLOT_BITS) - 1,
        );
        let num_bits = 30 - NUM_ALIGN_BITS;
        self.rc.encode_direct_bits((1u32 << num_bits) - 1, num_bits);
        self.rc
            .encode_tree_reverse(&mut self.probs.align, NUM_ALIGN_BITS, ALIGN_MASK);
    }

    fn finish(mut self, now_pos: u32) -> Vec<u8> {
        // `Flush` (LzmaEnc.c:2190) writes the marker BEFORE flushing the range
        // coder, which is why the marker changes the stream tail rather than
        // simply appending to it.
        if self.props_write_end_mark {
            self.write_end_marker(now_pos);
        }
        self.rc.finish()
    }
}

/// Encode `input` to a raw LZMA stream, byte-identical to `LzmaEnc_MemEncode`.
pub fn encode(input: &[u8], props: &LzmaProps) -> Vec<u8> {
    let enc = Encoder::new(input, props);
    enc.run()
}

include!("optimum_dp.rs");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decoder::decode_raw;

    fn roundtrip(input: &[u8]) {
        let props = LzmaProps::for_level(8, (input.len() as u32).max(1));
        let stream = encode(input, &props);
        assert_eq!(stream.first().copied(), Some(0), "stream starts with 0x00");
        let decoded = decode_raw(&stream, &props.decoder_props(), input.len());
        assert_eq!(
            decoded,
            input,
            "round-trip failed for {}-byte input",
            input.len()
        );
    }

    #[test]
    fn roundtrip_edge_cases() {
        roundtrip(b"");
        roundtrip(b"A");
        roundtrip(b"AB");
        roundtrip(&[0u8; 4096]);
        roundtrip(&[0xFFu8; 1000]);
    }

    #[test]
    fn roundtrip_repetitive() {
        roundtrip(&b"the quick brown fox jumps over the lazy dog. ".repeat(64));
        roundtrip(&b"abcabcabcabc".repeat(100));
    }

    #[test]
    fn roundtrip_far_and_random() {
        roundtrip(&(0..6000u32).map(|i| i as u8).collect::<Vec<_>>());
        let data: Vec<u8> = (0..4000u32)
            .map(|i| (i.wrapping_mul(2654435761) >> 24) as u8)
            .collect();
        roundtrip(&data);
        let mut mixed = data.clone();
        mixed.extend_from_slice(&data);
        roundtrip(&mixed);
    }
}
