//! Raw LZMA decoder — a clean port of `LzmaDec` (`LzmaDec.c`), used to round-trip
//! the encoder's output in self-tests. Compiled into test builds always, and into
//! normal builds only with the `decode` feature.
//!
//! Decodes a raw stream (no `.lzma` header, no end marker) given the 5 decoder
//! property bytes and the known output length — the inverse of [`crate::encode`]
//! together with [`crate::decoder_props`]. The ASM-optimized flat probability
//! layout of the C is replaced with logical named arrays; the *coding order* is
//! what must match, and it is verified out-of-tree against the C reference (see
//! `docs/comparing-against-the-c-oracle.md`).
//!
//! Output fits in a single linear buffer (CHD reduces the dictionary to the hunk
//! size), so no circular dictionary window is needed.

use crate::state::{
    BIT_MODEL_TOTAL, END_POS_MODEL_INDEX, MATCH_LEN_MIN, NUM_ALIGN_BITS, NUM_BIT_MODEL_TOTAL_BITS,
    NUM_FULL_DISTANCES, NUM_LEN_TO_POS_STATES, NUM_MOVE_BITS, NUM_POS_SLOT_BITS, NUM_STATES,
    PROB_INIT_VALUE, START_POS_MODEL_INDEX, TOP_VALUE,
};

const NUM_LIT_STATES: u32 = 7;
const NUM_POS_STATES_MAX: usize = 16; // 1 << LZMA_PB_MAX
const POS_SLOT_TABLE_LEN: usize = 1 << NUM_POS_SLOT_BITS; // 64
const ALIGN_TABLE_LEN: usize = 1 << NUM_ALIGN_BITS; // 16
const LEN_LOW_TABLE_LEN: usize = 8; // 1 << kLenNumLowBits
const LEN_HIGH_TABLE_LEN: usize = 256; // 1 << kLenNumHighBits

/// Length decoder probabilities (`CLenDec` equivalent).
struct LenProbs {
    choice: u16,
    choice2: u16,
    low: [[u16; LEN_LOW_TABLE_LEN]; NUM_POS_STATES_MAX],
    mid: [[u16; LEN_LOW_TABLE_LEN]; NUM_POS_STATES_MAX],
    high: [u16; LEN_HIGH_TABLE_LEN],
}

impl LenProbs {
    fn new() -> Self {
        LenProbs {
            choice: PROB_INIT_VALUE,
            choice2: PROB_INIT_VALUE,
            low: [[PROB_INIT_VALUE; LEN_LOW_TABLE_LEN]; NUM_POS_STATES_MAX],
            mid: [[PROB_INIT_VALUE; LEN_LOW_TABLE_LEN]; NUM_POS_STATES_MAX],
            high: [PROB_INIT_VALUE; LEN_HIGH_TABLE_LEN],
        }
    }
}

/// All decoder probability models, in logical (not ASM-flat) layout.
struct Probs {
    is_match: [[u16; NUM_POS_STATES_MAX]; NUM_STATES],
    is_rep: [u16; NUM_STATES],
    is_rep_g0: [u16; NUM_STATES],
    is_rep_g1: [u16; NUM_STATES],
    is_rep_g2: [u16; NUM_STATES],
    is_rep0_long: [[u16; NUM_POS_STATES_MAX]; NUM_STATES],
    pos_slot: [[u16; POS_SLOT_TABLE_LEN]; NUM_LEN_TO_POS_STATES as usize],
    spec_pos: [u16; NUM_FULL_DISTANCES as usize],
    align: [u16; ALIGN_TABLE_LEN],
    len: LenProbs,
    rep_len: LenProbs,
    literal: Vec<u16>,
}

impl Probs {
    fn new(lc: u32, lp: u32) -> Self {
        Probs {
            is_match: [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES],
            is_rep: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g0: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g1: [PROB_INIT_VALUE; NUM_STATES],
            is_rep_g2: [PROB_INIT_VALUE; NUM_STATES],
            is_rep0_long: [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES],
            pos_slot: [[PROB_INIT_VALUE; POS_SLOT_TABLE_LEN]; NUM_LEN_TO_POS_STATES as usize],
            spec_pos: [PROB_INIT_VALUE; NUM_FULL_DISTANCES as usize],
            align: [PROB_INIT_VALUE; ALIGN_TABLE_LEN],
            len: LenProbs::new(),
            rep_len: LenProbs::new(),
            literal: vec![PROB_INIT_VALUE; 0x300usize << (lc + lp)],
        }
    }
}

/// LZMA range decoder over an in-memory stream.
struct RangeDecoder<'a> {
    input: &'a [u8],
    pos: usize,
    range: u32,
    code: u32,
}

impl<'a> RangeDecoder<'a> {
    /// `RC_INIT`: the first stream byte is the always-zero cache byte and is
    /// ignored; the next 4 bytes (big-endian) seed `code`.
    fn new(input: &'a [u8]) -> Self {
        let b = |i: usize| input.get(i).copied().unwrap_or(0);
        let code = (u32::from(b(1)) << 24)
            | (u32::from(b(2)) << 16)
            | (u32::from(b(3)) << 8)
            | u32::from(b(4));
        RangeDecoder {
            input,
            pos: 5,
            range: 0xFFFF_FFFF,
            code,
        }
    }

    #[inline]
    fn next_byte(&mut self) -> u32 {
        let b = self.input.get(self.pos).copied().unwrap_or(0);
        self.pos += 1;
        u32::from(b)
    }

    /// `NORMALIZE` — lazy, applied at the start of each bit (as the C does).
    #[inline]
    fn normalize(&mut self) {
        if self.range < TOP_VALUE {
            self.range <<= 8;
            self.code = (self.code << 8) | self.next_byte();
        }
    }

    /// `IF_BIT_0` + `UPDATE_0`/`UPDATE_1`: decode one bit under `prob` and adapt it.
    #[inline]
    fn decode_bit(&mut self, prob: &mut u16) -> u32 {
        self.normalize();
        let ttt = u32::from(*prob);
        let bound = (self.range >> NUM_BIT_MODEL_TOTAL_BITS) * ttt;
        if self.code < bound {
            self.range = bound;
            *prob = (ttt + ((BIT_MODEL_TOTAL - ttt) >> NUM_MOVE_BITS)) as u16;
            0
        } else {
            self.range -= bound;
            self.code -= bound;
            *prob = (ttt - (ttt >> NUM_MOVE_BITS)) as u16;
            1
        }
    }

    /// `decodeDirectBits` (`LzmaDec.c:491`): `n` bits with no probability model.
    fn decode_direct_bits(&mut self, n: u32) -> u32 {
        let mut result = 0u32;
        for _ in 0..n {
            self.normalize();
            self.range >>= 1;
            self.code = self.code.wrapping_sub(self.range);
            let t = 0u32.wrapping_sub(self.code >> 31);
            self.code = self.code.wrapping_add(self.range & t);
            result = (result << 1).wrapping_add(t.wrapping_add(1));
        }
        result
    }

    /// Forward bit-tree decode (MSB first), `probs` indexed from 1.
    fn decode_tree(&mut self, probs: &mut [u16], num_bits: u32) -> u32 {
        let mut m = 1u32;
        for _ in 0..num_bits {
            let b = self.decode_bit(&mut probs[m as usize]);
            m = (m << 1) | b;
        }
        m - (1 << num_bits)
    }

    /// Reverse bit-tree decode (LSB first), `probs` indexed from 1.
    fn decode_tree_reverse(&mut self, probs: &mut [u16], num_bits: u32) -> u32 {
        let mut m = 1u32;
        let mut sym = 0u32;
        for i in 0..num_bits {
            let b = self.decode_bit(&mut probs[m as usize]);
            m = (m << 1) | b;
            sym |= b << i;
        }
        sym
    }
}

/// Decode the length symbol (0..=271) from a [`LenProbs`] at `pos_state`.
fn decode_len(rc: &mut RangeDecoder, len: &mut LenProbs, pos_state: usize) -> u32 {
    if rc.decode_bit(&mut len.choice) == 0 {
        rc.decode_tree(&mut len.low[pos_state], 3)
    } else if rc.decode_bit(&mut len.choice2) == 0 {
        8 + rc.decode_tree(&mut len.mid[pos_state], 3)
    } else {
        16 + rc.decode_tree(&mut len.high, 8)
    }
}

/// Decode `input` (a raw LZMA stream) into exactly `out_len` bytes, using the 5
/// decoder property bytes `props`.
pub fn decode_raw(input: &[u8], props: &[u8; 5], out_len: usize) -> Vec<u8> {
    // Unpack props byte 0: d = (pb*5 + lp)*9 + lc.
    let mut d = props[0] as u32;
    let lc = d % 9;
    d /= 9;
    let lp = d % 5;
    let pb = d / 5;
    let pb_mask = (1u32 << pb) - 1;
    let lp_mask = (1u32 << lp) - 1;

    let mut probs = Probs::new(lc, lp);
    let mut rc = RangeDecoder::new(input);

    let mut out: Vec<u8> = Vec::with_capacity(out_len);
    let mut state: u32 = 0;
    let mut reps: [u32; 4] = [1, 1, 1, 1]; // 1-based distances

    while out.len() < out_len {
        let pos_state = (out.len() as u32 & pb_mask) as usize;

        if rc.decode_bit(&mut probs.is_match[state as usize][pos_state]) == 0 {
            // ---- literal ----
            let prev = if out.is_empty() {
                0u32
            } else {
                u32::from(out[out.len() - 1])
            };
            let lit_state = (((out.len() as u32 & lp_mask) << lc) + (prev >> (8 - lc))) as usize;
            let base = 0x300 * lit_state;
            let table = &mut probs.literal[base..base + 0x300];

            let symbol = if state < NUM_LIT_STATES {
                let mut sym = 1u32;
                while sym < 0x100 {
                    let b = rc.decode_bit(&mut table[sym as usize]);
                    sym = (sym << 1) | b;
                }
                sym
            } else {
                // matched literal against the byte at (pos - rep0)
                let mut match_byte = u32::from(out[out.len() - reps[0] as usize]);
                let mut sym = 1u32;
                let mut offs = 0x100u32;
                while sym < 0x100 {
                    match_byte <<= 1;
                    let match_bit = offs;
                    offs &= match_byte;
                    let idx = (offs + match_bit + sym) as usize;
                    let b = rc.decode_bit(&mut table[idx]);
                    sym = (sym << 1) | b;
                    if b == 0 {
                        offs ^= match_bit;
                    }
                }
                sym
            };

            out.push((symbol & 0xFF) as u8);
            state = if state < 4 {
                0
            } else if state < 10 {
                state - 3
            } else {
                state - 6
            };
            continue;
        }

        // ---- match (IsMatch == 1) ----
        let len_symbol;
        if rc.decode_bit(&mut probs.is_rep[state as usize]) == 0 {
            // new (non-rep) match
            reps[3] = reps[2];
            reps[2] = reps[1];
            reps[1] = reps[0];
            len_symbol = decode_len(&mut rc, &mut probs.len, pos_state);
            state = if state < NUM_LIT_STATES { 7 } else { 10 };

            // decode distance
            let len_to_pos = len_symbol.min(NUM_LEN_TO_POS_STATES - 1) as usize;
            let pos_slot = rc.decode_tree(&mut probs.pos_slot[len_to_pos], NUM_POS_SLOT_BITS);
            let dist = if pos_slot < START_POS_MODEL_INDEX {
                pos_slot
            } else {
                let num_direct = (pos_slot >> 1) - 1;
                let mut dist = 2 | (pos_slot & 1);
                if pos_slot < END_POS_MODEL_INDEX {
                    dist <<= num_direct;
                    // SpecPos reverse decode, replicating the C in-place arithmetic.
                    let mut m = 1u32;
                    let mut node = dist + 1;
                    let mut nd = num_direct;
                    loop {
                        let b = rc.decode_bit(&mut probs.spec_pos[node as usize]);
                        if b == 0 {
                            node += m;
                            m += m;
                        } else {
                            m += m;
                            node += m;
                        }
                        nd -= 1;
                        if nd == 0 {
                            break;
                        }
                    }
                    node - m
                } else {
                    dist <<= num_direct;
                    let high = rc.decode_direct_bits(num_direct - NUM_ALIGN_BITS) << NUM_ALIGN_BITS;
                    dist += high;
                    dist + rc.decode_tree_reverse(&mut probs.align, NUM_ALIGN_BITS)
                }
            };

            if dist == 0xFFFF_FFFF {
                break; // end marker
            }
            reps[0] = dist + 1;
        } else {
            // rep match
            if rc.decode_bit(&mut probs.is_rep_g0[state as usize]) == 0 {
                if rc.decode_bit(&mut probs.is_rep0_long[state as usize][pos_state]) == 0 {
                    // short rep: copy one byte at rep0
                    let b = out[out.len() - reps[0] as usize];
                    out.push(b);
                    state = if state < NUM_LIT_STATES { 9 } else { 11 };
                    continue;
                }
            } else {
                let dist;
                if rc.decode_bit(&mut probs.is_rep_g1[state as usize]) == 0 {
                    dist = reps[1];
                } else if rc.decode_bit(&mut probs.is_rep_g2[state as usize]) == 0 {
                    dist = reps[2];
                    reps[2] = reps[1];
                } else {
                    dist = reps[3];
                    reps[3] = reps[2];
                    reps[2] = reps[1];
                }
                reps[1] = reps[0];
                reps[0] = dist;
            }
            len_symbol = decode_len(&mut rc, &mut probs.rep_len, pos_state);
            state = if state < NUM_LIT_STATES { 8 } else { 11 };
        }

        // ---- copy match ----
        let len = (len_symbol + MATCH_LEN_MIN) as usize;
        let rep0 = reps[0] as usize;
        let copy = len.min(out_len - out.len());
        for _ in 0..copy {
            let b = out[out.len() - rep0];
            out.push(b);
        }
    }

    out
}

#[cfg(test)]
mod tests {
    #[test]
    fn props_unpack() {
        // 0x5D = 93 = (2*5 + 0)*9 + 3 -> lc=3, lp=0, pb=2.
        let mut d = 0x5Du32;
        let lc = d % 9;
        d /= 9;
        let lp = d % 5;
        let pb = d / 5;
        assert_eq!((lc, lp, pb), (3, 0, 2));
    }
}
