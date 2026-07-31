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

use crate::matchfinder::{KEEP_ADD_BUFFER_BEFORE, Match, MatchFinder};
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
use crate::stream::{InStream, OutStream, SliceIn, StreamError, VecOut};

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
#[derive(Clone)]
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
///
/// This is also **exactly** `CSaveState`'s probability half (`LzmaEnc.c:359-381`,
/// copied by `COPY_LZMA_ENC_STATE` at `:501-515`): `posAlignEncoder`, `isRep`,
/// `isRepG0`, `isRepG1`, `isRepG2`, `isMatch`, `isRep0Long`, `posSlotEncoder`,
/// `posEncoders`, `lenProbs`, `repLenProbs`, `litProbs`. Keeping the two in one
/// struct is what makes [`SaveState`] provably the right *set* — the C's save list
/// is a hand-written macro, and adding a field to it (or forgetting one) changes
/// every chunk after the first uncompressed one.
#[derive(Clone)]
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

    /// The probability half of `LzmaEnc_Init` (`LzmaEnc.c:2762-2830`), in place.
    ///
    /// Re-running the constructor would do the same thing, but LZMA2 calls this once
    /// per block and the literal table is up to 24 KiB; the C reuses its allocation
    /// too (`LzmaEnc_Alloc` only reallocates when `lclp` changes, `:2700-2711`).
    fn reset(&mut self) {
        self.is_match = [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES];
        self.is_rep = [PROB_INIT_VALUE; NUM_STATES];
        self.is_rep_g0 = [PROB_INIT_VALUE; NUM_STATES];
        self.is_rep_g1 = [PROB_INIT_VALUE; NUM_STATES];
        self.is_rep_g2 = [PROB_INIT_VALUE; NUM_STATES];
        self.is_rep0_long = [[PROB_INIT_VALUE; NUM_POS_STATES_MAX]; NUM_STATES];
        self.pos_slot = [[PROB_INIT_VALUE; POS_SLOT_TABLE_LEN]; 4];
        self.spec_pos = [PROB_INIT_VALUE; NUM_FULL_DISTANCES as usize];
        self.align = [PROB_INIT_VALUE; ALIGN_TABLE_LEN];
        self.len = LenEnc::new();
        self.rep_len = LenEnc::new();
        self.literal.fill(PROB_INIT_VALUE);
    }
}

/// `CSaveState` (`LzmaEnc.c:359-381`) — what `LzmaEnc_SaveState` copies, and
/// nothing else.
///
/// The omissions are the load-bearing part, because the uncompressed-chunk path in
/// `Lzma2EncInt_EncodeSubblock` (`Lzma2Enc.c:193`) restores this and then keeps
/// encoding. **Not** saved, and therefore deliberately carried forward across a
/// restore: `nowPos64`, `additionalOffset`, every price table, `matchPriceCount`,
/// `repLenEncCounter`, and the match finder's entire state. Saving any of those
/// would rewind work the C does not rewind; saving fewer would leak an adapted
/// probability into a chunk the decoder will start fresh.
pub struct SaveState {
    state: u32,
    reps: [u32; NUM_REPS],
    probs: EncProbs,
}

/// The three out-parameters of `LzmaEnc_CodeOneMemBlock` (`LzmaEnc.c:2953`).
pub struct MemBlockOutcome {
    /// What `LzmaEnc_CodeOneBlock` returned.
    pub res: Result<(), StreamError>,
    /// `outStream.overflow` — the bounded sink truncated. Reported by the C as
    /// `SZ_ERROR_OUTPUT_EOF`, ahead of `res`.
    pub overflow: bool,
    /// `*unpackSize` on return: `nowPos64` delta, i.e. input consumed.
    pub unpack_size: u32,
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

/// `kPackReserve` (`LzmaEnc.c:278`) — the compressed-side slack the bounded break
/// keeps in hand. `kNumOpts * 8`, not `kNumOpts`: shrinking it lets a chunk run
/// past `maxPackSize`, widening it cuts every chunk early, and either way every
/// subsequent chunk boundary moves.
const K_PACK_RESERVE: u32 = NUM_OPTS as u32 * 8;

/// The uncompressed-side slack in the same break (`LzmaEnc.c:2665`):
/// `processed + kNumOpts + 300 >= maxUnpackSize`. The `300` is a literal in the C
/// with no name and no derivation; it is transcribed, not reconstructed.
const UNPACK_RESERVE: u32 = NUM_OPTS as u32 + 300;

/// The 128 KiB the *unbounded* path uses instead (`LzmaEnc.c:2670`). It only decides
/// how often `LzmaEnc_Encode2` (`:2999`) re-enters `CodeOneBlock`; nothing is reset
/// between those re-entries, so it cannot change a byte.
const UNBOUNDED_CHUNK: u32 = 1 << 17;

pub struct Encoder<'a> {
    rc: RangeEncoder<'a>,
    probs: EncProbs,
    prices: Prices,
    mf: MatchFinder<'a>,
    opt: Vec<Optimal>,
    matches: Vec<u32>,
    mf_buf: Vec<Match>,
    /// `p->saveState`. Allocated once, as the C allocates `saveState.litProbs`
    /// alongside `litProbs` (`LzmaEnc.c:2703`).
    save: SaveState,
    state: u32,
    reps: [u32; 4],
    lc: u32,
    lp_mask: u32,
    pb_mask: u32,
    /// `1 << pb`, the argument `LenPriceEnc_UpdateTables` takes.
    num_pos_states: usize,
    /// `p->nowPos64` — the stream position, carried *across* `code_one_block` calls.
    /// This is the field that makes the block coder re-entrant.
    now_pos64: u64,
    /// `p->finished`, set by `flush`.
    finished: bool,
    /// `p->result`, the latched error `CheckErrors` (`LzmaEnc.c:2161`) maintains.
    result: Result<(), StreamError>,
    /// `props.write_end_mark`, consulted only in `flush`.
    ///
    /// Mutable because `LzmaEnc_CodeOneMemBlock` force-clears it
    /// (`LzmaEnc.c:2967`): an LZMA2 chunk never carries an end marker, whatever the
    /// caller asked for.
    props_write_end_mark: bool,
    /// `p->fastMode` (`LzmaEnc.c:568`): use `get_optimum_fast` and skip every price
    /// table the optimal parser needs.
    fast_mode: bool,
    num_fast_bytes: u32,
    additional_offset: u32,
    num_avail: u32,
    longest_match_len: u32,
    num_pairs: u32,
    back_res: u32,
    opt_cur: usize,
    opt_end: usize,
}

/// `LzmaEnc_Alloc`'s `beforeSize` (`LzmaEnc.c:2686`, `:2729`):
///
/// ```text
///     UInt32 beforeSize = kNumOpts;
///     if (beforeSize + dictSize < keepWindowSize)
///       beforeSize = keepWindowSize - dictSize;
/// ```
///
/// `keep_window_size` is 0 for plain LZMA (`LzmaEnc_Prepare`, `:2877`) and
/// `LZMA2_KEEP_WINDOW_SIZE` for LZMA2 (`Lzma2Enc.c:566`, `:581`).
fn before_size(dict_size: u32, keep_window_size: u32) -> u32 {
    let before = KEEP_ADD_BUFFER_BEFORE;
    if before.saturating_add(dict_size) < keep_window_size {
        keep_window_size - dict_size
    } else {
        before
    }
}

impl<'a> Encoder<'a> {
    /// The parts of construction that do not depend on where the output goes.
    fn build(
        rc: RangeEncoder<'a>,
        source: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_window_size: u32,
        expected_data_size: u64,
    ) -> Self {
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
        let probs = EncProbs::new(props.lc as u32, props.lp as u32);
        Encoder {
            rc,
            save: SaveState {
                state: 0,
                reps: [1; NUM_REPS],
                probs: probs.clone(),
            },
            probs,
            prices: Prices::new(dist_table_size, props.fb),
            mf: MatchFinder::new_with_expected(
                source,
                props,
                before_size(props.history_size(), keep_window_size),
                expected_data_size,
            ),
            opt: vec![Optimal::default(); NUM_OPTS],
            matches: Vec::with_capacity(MATCH_LEN_MAX as usize * 2 + 2),
            mf_buf: Vec::with_capacity(MATCH_LEN_MAX as usize),
            state: 0,
            reps: [1, 1, 1, 1],
            lc: props.lc as u32,
            lp_mask: (1u32 << props.lp) - 1,
            pb_mask: (1u32 << props.pb) - 1,
            num_pos_states: 1usize << props.pb,
            now_pos64: 0,
            finished: false,
            result: Ok(()),
            props_write_end_mark: props.write_end_mark,
            fast_mode: props.fast_mode,
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

    /// `LzmaEnc_Prepare` (`LzmaEnc.c:2868`): a stream sink and `keepWindowSize = 0`.
    fn new(
        source: &'a mut dyn InStream,
        sink: &'a mut dyn OutStream,
        props: &LzmaProps,
    ) -> Self {
        // `LzmaEnc_Prepare` does not call `LzmaEnc_SetDataSize`, so the finder keeps
        // its constructed `(UInt64)(Int64)-1`.
        Self::build(RangeEncoder::new(sink), source, props, 0, u64::MAX)
    }

    /// `LzmaEnc_PrepareForLzma2` (`LzmaEnc.c:2879`): the bounded scratch sink, and a
    /// caller-chosen minimum window.
    ///
    /// The SOLID LZMA2 path reaches this with the data size unknown, because
    /// `Lzma2Enc_EncodeMt1` passes `LzmaEnc_SetDataSize` the sentinel whenever the
    /// block is SOLID (`Lzma2Enc.c:557-566`).
    pub(crate) fn new_for_lzma2(
        source: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_window_size: u32,
    ) -> Self {
        Self::new_for_lzma2_sized(source, props, keep_window_size, u64::MAX)
    }

    /// `LzmaEnc_MemPrepare` (`LzmaEnc.c:2889`) as the LZMA2 blocked path uses it:
    /// same preparation, but with `LzmaEnc_SetDataSize(p, srcLen)` (`:2896`) ahead of
    /// the allocation, which narrows the match finder's hash mask for a block
    /// shorter than the dictionary. The source is still a stream here rather than
    /// `directInput`; that difference is invisible in the output, the declared size
    /// is not.
    pub(crate) fn new_for_lzma2_sized(
        source: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_window_size: u32,
        expected_data_size: u64,
    ) -> Self {
        Self::build(
            RangeEncoder::new_bounded(),
            source,
            props,
            keep_window_size,
            expected_data_size,
        )
    }

    /// `GetPointerToCurrentPos(..) - 1` as a window index — the position the match
    /// finder most recently *processed*, which is the one `GetOptimum` is deciding
    /// about (`LzmaEnc.c:1252`, `:1578`, and the `p1` in `ReadMatchDistances`
    /// at `:1113`).
    ///
    /// Distinct from [`Encoder::emit_index`] and not interchangeable with it: inside
    /// the DP loop `additional_offset` grows by one per iteration alongside
    /// `position`, so only the constant `- 1` tracks the position being scored.
    /// The C writes the two forms literally, one per call site, and so does this.
    #[inline]
    fn parse_index(&self) -> usize {
        self.mf.cur_index() - 1
    }

    /// `GetPointerToCurrentPos(..) - additionalOffset` as a window index
    /// (`LzmaEnc.c:2414`, `:2465`, `:2949`) — where the byte the *symbol coder* is
    /// about to emit lives, `additional_offset` bytes behind the finder's lookahead.
    ///
    /// Both indices must be recomputed after anything that advances the match
    /// finder: the window slides and a stale index points at the wrong byte. See
    /// the module docs on [`crate::matchfinder`].
    #[inline]
    fn emit_index(&self) -> usize {
        self.mf.cur_index() - self.additional_offset as usize
    }

    /// The window. Never index it with a stream position.
    #[inline]
    fn win(&self) -> &[u8] {
        self.mf.win()
    }

    #[inline]
    fn lit_base(&self, pos: u32, prev: u32) -> usize {
        let lit_state = (((pos & self.lp_mask) << self.lc) + (prev >> (8 - self.lc))) as usize;
        0x300 * lit_state
    }

    // ---- symbol encoders (verified layer) ----

    /// `pos` is the absolute stream position, used for `posState` and the literal
    /// context; the *bytes* come from the window at [`Encoder::data_index`]. Keeping
    /// the two apart is the whole point — they coincide only before the window
    /// first slides.
    fn encode_literal(&mut self, pos: usize) {
        let ps = (pos as u32 & self.pb_mask) as usize;
        self.rc
            .encode_bit(&mut self.probs.is_match[self.state as usize][ps], 0);
        // Every byte is read before `table` takes a mutable borrow of `self.probs`:
        // `win()` borrows all of `self`, so a read after that point would not
        // compile. The C reads them up front for the same practical reason.
        let data = self.emit_index();
        let byte = u32::from(self.win()[data]);
        let prev = if data == 0 {
            0
        } else {
            u32::from(self.win()[data - 1])
        };
        let match_byte = if self.state < NUM_LIT_STATES {
            0
        } else {
            u32::from(self.win()[data - self.reps[0] as usize])
        };
        let base = self.lit_base(pos as u32, prev);
        let table = &mut self.probs.literal[base..base + 0x300];
        if self.state < NUM_LIT_STATES {
            self.rc.encode_tree(table, 8, byte);
        } else {
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
        self.mf.get_matches(&mut self.mf_buf);
        // AFTER get_matches, never before: get_matches advances the finder, which
        // can slide the window and invalidate any index taken earlier. The C reads
        // `p1 = GetPointerToCurrentPos(p) - 1` at this same point (LzmaEnc.c:1113),
        // and `- 1` is exact here because get_matches advanced by exactly one.
        let cur_idx = self.parse_index();
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
        while p2 != num_avail && self.win()[cur_idx + p2] == self.win()[cur_idx + p2 - back] {
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

    /// `Flush` (`LzmaEnc.c:2189`).
    fn flush(&mut self, now_pos: u32) -> Result<(), StreamError> {
        self.finished = true;
        // `Flush` writes the marker BEFORE flushing the range coder, which is why
        // the marker changes the stream tail rather than simply appending to it.
        if self.props_write_end_mark {
            self.write_end_marker(now_pos);
        }
        // RangeEnc_FlushData + RangeEnc_FlushStream. The result is read back out of
        // the coder by `check_errors`, exactly as the C reads `p->rc.res`.
        let _ = self.rc.finish();
        self.check_errors()
    }

    /// `CheckErrors` (`LzmaEnc.c:2161`).
    ///
    /// The order is the C's and it matters: the sink error is latched first, then
    /// the *source* error overwrites it. An input-stream error outranks the output,
    /// so a truncated read is never reported as a successfully written short stream.
    /// (The C collapses both to `SZ_ERROR_WRITE` / `SZ_ERROR_READ`; this keeps the
    /// caller's own code, which is strictly more information and changes no bytes.)
    //
    // `single_match` would rewrite the latch arms as `if let`, which this workspace
    // bans outright (see the totality gate in `.github/workflows/build.yml`).
    #[allow(clippy::single_match)]
    fn check_errors(&mut self) -> Result<(), StreamError> {
        match self.result {
            Err(e) => return Err(e),
            Ok(()) => {}
        }
        match self.rc.result() {
            Err(e) => self.result = Err(e),
            Ok(()) => {}
        }
        match self.mf.result() {
            Err(e) => self.result = Err(e),
            Ok(()) => {}
        }
        match self.result {
            Err(e) => {
                self.finished = true;
                Err(e)
            }
            Ok(()) => Ok(()),
        }
    }

    /// `LzmaEnc_Init` (`LzmaEnc.c:2762`).
    ///
    /// Note what is *not* here, because LZMA2 calls it per block and the difference
    /// shows up in the bytes: it does not touch `nowPos64`, the price tables,
    /// `matchPriceCount` or `repLenEncCounter` (those belong to `init_prices`), and
    /// it does not re-init the match finder — the C's `p->needInit` handles that,
    /// and here the finder is initialized by its own constructor.
    pub(crate) fn init(&mut self) {
        self.state = 0;
        self.reps = [1; NUM_REPS];
        self.rc.reinit();
        self.probs.reset();
        self.opt_end = 0;
        self.opt_cur = 0;
        for o in self.opt.iter_mut() {
            o.price = INFINITY_PRICE;
        }
        self.additional_offset = 0;
    }

    /// `LzmaEnc_InitPrices` (`LzmaEnc.c:2833`).
    ///
    /// LZMA2 runs this before **every** chunk (`LzmaEnc.c:2969`), even when the
    /// probabilities were not reset. That restarts both adaptive-refresh schedules —
    /// `repLenEncCounter` goes back to `REP_LEN_COUNT` and `FillDistancesPrices`
    /// zeroes `matchPriceCount` — so the refresh cadence inside a chunk is not the
    /// cadence a single long LZMA stream would have. It changes emitted bytes; it is
    /// not an optimization that can be hoisted out of the chunk loop.
    pub(crate) fn init_prices(&mut self) {
        // The C guards exactly these two on `!fastMode`. The tail below -- the
        // table sizes, the counter and both len-price tables -- runs either way.
        // Fast mode reads no price table, so this is speed, not bytes; but
        // "byte-identical yet 470x slower" has happened in this repo before, which
        // is why the guard is here rather than left as a comment.
        if !self.fast_mode {
            self.prices
                .fill_distances_prices(&self.probs.pos_slot, &self.probs.spec_pos);
            self.prices.fill_align_prices(&self.probs.align);
        }
        self.prices.len_enc.table_size = self.num_fast_bytes + 1 - MATCH_LEN_MIN;
        self.prices.rep_len_enc.table_size = self.num_fast_bytes + 1 - MATCH_LEN_MIN;
        self.prices.rep_len_enc_counter = REP_LEN_COUNT;
        self.update_len_prices();
        self.update_rep_len_prices();
    }

    /// `LenPriceEnc_UpdateTables(&p->lenEnc, 1 << p->pb, &p->lenProbs, ...)`.
    fn update_len_prices(&mut self) {
        len_price_update(
            &mut self.prices.len_enc,
            self.num_pos_states,
            self.probs.len.choice,
            self.probs.len.choice2,
            &self.probs.len.low,
            &self.probs.len.mid,
            &self.probs.len.high,
            &self.prices.pp,
        );
    }

    /// `LenPriceEnc_UpdateTables(&p->repLenEnc, 1 << p->pb, &p->repLenProbs, ...)`.
    fn update_rep_len_prices(&mut self) {
        len_price_update(
            &mut self.prices.rep_len_enc,
            self.num_pos_states,
            self.probs.rep_len.choice,
            self.probs.rep_len.choice2,
            &self.probs.rep_len.low,
            &self.probs.rep_len.mid,
            &self.probs.rep_len.high,
            &self.prices.pp,
        );
    }

    /// `LzmaEnc_SaveState` (`LzmaEnc.c:517`) — see [`SaveState`] for the field list
    /// and, more importantly, for what it deliberately omits.
    pub(crate) fn save_state(&mut self) {
        self.save.state = self.state;
        self.save.reps = self.reps;
        self.save.probs.clone_from(&self.probs);
    }

    /// `LzmaEnc_RestoreState` (`LzmaEnc.c:524`).
    pub(crate) fn restore_state(&mut self) {
        self.state = self.save.state;
        self.reps = self.save.reps;
        self.probs.clone_from(&self.save.probs);
    }

    /// What one [`Encoder::code_one_mem_block`] produced, split the way
    /// `LzmaEnc_CodeOneMemBlock` (`LzmaEnc.c:2953`) splits its three out-parameters.
    ///
    /// `overflow` is deliberately separate from `res`: the C returns
    /// `SZ_ERROR_OUTPUT_EOF` for it *in preference to* whatever `CodeOneBlock`
    /// returned (`:2984`), and `Lzma2EncInt_EncodeSubblock` (`Lzma2Enc.c:157`)
    /// treats that one code as "fall back to a copy chunk" rather than as failure.
    /// Collapsing them would turn a routine incompressible block into an error.
    pub(crate) fn code_one_mem_block(
        &mut self,
        re_init: bool,
        dest_len: usize,
        desired_pack_size: u32,
        max_unpack_size: u32,
    ) -> MemBlockOutcome {
        self.rc.bounded_arm(dest_len);
        // LzmaEnc.c:2967-2969. The forced clear is why an LZMA2 chunk never carries
        // an end marker even when the caller asked for one.
        self.props_write_end_mark = false;
        self.finished = false;
        self.result = Ok(());

        if re_init {
            self.init();
        }
        self.init_prices();
        // `RangeEnc_Init` again, after `LzmaEnc_Init` may already have done it
        // (`LzmaEnc.c:2970`). This is what makes every chunk payload start `0x00`.
        self.rc.reinit();

        let before = self.now_pos64;
        let res = self.code_one_block(desired_pack_size, max_unpack_size);
        let unpack_size = (self.now_pos64 - before) as u32;
        let overflow = match self.rc.bounded() {
            Some(b) => b.overflow,
            None => false,
        };
        MemBlockOutcome {
            res,
            overflow,
            unpack_size,
        }
    }

    /// The bytes the last [`Encoder::code_one_mem_block`] wrote — the C's
    /// `dest[0 .. *destLen]`.
    pub(crate) fn mem_block_payload(&self) -> &[u8] {
        match self.rc.bounded() {
            Some(b) => &b.data,
            None => &[],
        }
    }

    /// `LzmaEnc_GetCurBuf(p) - back`, as a bounds-checked slice of `len` bytes
    /// (`LzmaEnc.c:2946`; used at `Lzma2Enc.c:176` with `back == unpackSize`).
    ///
    /// The anchor is [`Encoder::emit_index`], **not** [`Encoder::parse_index`]: it is
    /// the byte the symbol coder is about to emit, so "N bytes behind it" is exactly
    /// the N bytes the last chunk consumed. Using the parse anchor would be off by
    /// `additional_offset` and the copy chunk would carry the wrong bytes — bytes a
    /// decoder would accept without complaint.
    pub(crate) fn cur_buf_behind(&self, back: usize, len: usize) -> Option<&[u8]> {
        let end = self.emit_index().checked_sub(back)?;
        self.mf.bytes_ending_at(end + len, len)
    }
}

/// Encode `source` to `sink` as a raw LZMA stream, byte-identical to the C
/// `LzmaEnc_Encode` for the same properties.
///
/// Memory is O(dictionary), not O(input): the match finder holds a sliding window
/// and the range coder stages 64 KiB at a time.
/// All five of `C_LZMA.cpp`'s match finders and both parsers are implemented, so
/// this no longer refuses any configuration; the errors it can return all come from
/// the caller's own streams.
pub fn encode_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    props: &LzmaProps,
) -> Result<(), StreamError> {
    let mut enc = Encoder::new(source, sink, props);
    enc.run()
}

/// Encode `input` to a raw LZMA stream, byte-identical to `LzmaEnc_MemEncode`.
///
/// The in-memory convenience wrapper over [`encode_stream`]; prefer that for
/// anything block-sized. The `Result` is here because the encoder is genuinely
/// fallible in general — with [`SliceIn`] and [`VecOut`] it cannot fail, but
/// hard-coding that into the return type would make the streaming path's errors
/// look optional.
pub fn encode(input: &[u8], props: &LzmaProps) -> Result<Vec<u8>, StreamError> {
    let mut source = SliceIn::new(input);
    let mut sink = VecOut::default();
    encode_stream(&mut source, &mut sink, props)?;
    Ok(sink.data)
}

include!("optimum_dp.rs");
include!("optimum_fast.rs");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decoder::decode_raw;

    fn roundtrip(input: &[u8]) {
        let props = LzmaProps::for_level(8, (input.len() as u32).max(1));
        let stream = match encode(input, &props) {
            Ok(s) => s,
            Err(e) => panic!("in-memory encode cannot fail, but returned {e:?}"),
        };
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
