//! Hardened, streaming, EOPM-terminated LZMA1 decoder — a port of
//! `Compression/LZMA/7z24/LzmaDec.c`.
//!
//! This is the decoder DArc's `lzma_decompress` needs, and it is deliberately a
//! second implementation rather than a hardening pass over [`crate::decoder`]:
//! that module is a **test oracle**. It takes a known output length, keeps the
//! whole output in one `Vec`, invents zero bytes past the end of its input, and
//! validates neither the props byte nor match distances. None of that is
//! survivable here, because every `unarc` and SFX target links the decoder
//! (`Unarc/makefile:19-20`) and parses attacker-supplied archives standalone with
//! `-D_NO_EXCEPTIONS`. A panic in this file is a vulnerability, not a bug.
//!
//! # What this module guarantees
//!
//! * **Total.** Every entry point returns [`Result`]. There is no `unwrap`,
//!   `expect`, `panic!`, slicing that a malformed stream can push out of range, or
//!   arithmetic that can underflow, outside `#[cfg(test)]`.
//! * **Streaming both ways** over [`InStream`] / [`OutStream`], driven the way
//!   `C_LZMA.cpp:184-227` drives the C: 64 KiB in, 64 KiB out, nothing else.
//! * **Bounded memory.** The dictionary is a circular window of `dicBufSize`
//!   (`LzmaDec.c:1315-1323`), not an output-sized `Vec`, so a 4 GiB payload
//!   decoded through a 64 MiB dictionary costs 64 MiB.
//! * **EOPM-terminated.** DArc's LZMA streams carry no uncompressed length
//!   (`C_LZMA.cpp`: "FreeArc streams with EOPM (unknown size)"), so termination is
//!   the `distance == 0xFFFFFFFF` marker at `LzmaDec.c:523-528`.
//! * **Honest about short input.** Truncation is reported as truncation
//!   ([`LzmaDecodeError::TruncatedInput`]); no byte is ever fabricated.
//!
//! # Error mapping
//!
//! The mapping to FreeArc's codes belongs to the caller (`darc-codecs`), not here
//! — this crate has no `Compression.h`. For reference, `C_LZMA.cpp:163-166`,
//! `:201-205` and `:217-226` map the C's `SRes` as follows, and
//! [`LzmaDecodeError`] documents the corresponding target per variant:
//!
//! | This crate | C | FreeArc code |
//! |---|---|---|
//! | [`LzmaDecodeError::UnsupportedProps`] | `SZ_ERROR_UNSUPPORTED` | `FREEARC_ERRCODE_INVALID_COMPRESSOR` |
//! | [`LzmaDecodeError::NotEnoughMemory`] | `SZ_ERROR_MEM` | `FREEARC_ERRCODE_NOT_ENOUGH_MEMORY` |
//! | [`LzmaDecodeError::DataError`] | `SZ_ERROR_DATA` | `FREEARC_ERRCODE_BAD_COMPRESSED_DATA` |
//! | [`LzmaDecodeError::TruncatedInput`] | `SZ_ERROR_INPUT_EOF` | `FREEARC_ERRCODE_BAD_COMPRESSED_DATA` |
//! | [`LzmaDecodeError::Internal`] | `SZ_ERROR_FAIL` | `FREEARC_ERRCODE_BAD_COMPRESSED_DATA` |
//! | [`LzmaDecodeError::Stream`] | — | the caller's own code, unchanged |
//!
//! # Layout note
//!
//! The probability array is kept **flat, in the C's exact order**
//! (`LzmaDec.c:139-151`), not split into named tables as [`crate::decoder`] does.
//! That is not nostalgia: the offsets are load-bearing for the bounds arguments
//! below (`LenChoice` deliberately reuses a slot the low-length tree never
//! touches, and `SpecPos` is sized so the `REV_BIT_VAR` walk cannot leave it), and
//! a reader checking this against the C should not have to re-derive them. The C
//! biases every offset by `kStartOffset = 1664` because `probs_1664` suits x86
//! addressing; the constants here are the same offsets rebased to `probs[0]`, so
//! `SpecPos` is 0 rather than -1664.

use crate::state::{
    BIT_MODEL_TOTAL, NUM_BIT_MODEL_TOTAL_BITS, NUM_MOVE_BITS, PROB_INIT_VALUE, TOP_VALUE,
};
use crate::stream::{InStream, OutStream, StreamError};

// ---------------------------------------------------------------------------
// Constants (LzmaDec.c / LzmaDec.h)
// ---------------------------------------------------------------------------

/// `RC_INIT_SIZE` (`LzmaDec.c:17`): one zero byte plus four code bytes.
const RC_INIT_SIZE: usize = 5;

/// `LZMA_REQUIRED_INPUT_MAX` (`LzmaDec.h:51`): the worst-case byte count one LZMA
/// symbol can need. The carry buffer is exactly this long.
const LZMA_REQUIRED_INPUT_MAX: usize = 20;

/// `LZMA_PROPS_SIZE` (`LzmaDec.h:26`).
pub const LZMA_PROPS_SIZE: usize = 5;

/// `kNumPosBitsMax` (`LzmaDec.c:88`).
const K_NUM_POS_BITS_MAX: u32 = 4;
/// `kNumPosStatesMax` (`LzmaDec.c:89`).
const K_NUM_POS_STATES_MAX: usize = 1 << K_NUM_POS_BITS_MAX;

/// `kLenNumLowBits` (`LzmaDec.c:91`).
const K_LEN_NUM_LOW_BITS: u32 = 3;
/// `kLenNumLowSymbols` (`LzmaDec.c:92`).
const K_LEN_NUM_LOW_SYMBOLS: u32 = 1 << K_LEN_NUM_LOW_BITS;
/// `kLenNumHighBits` (`LzmaDec.c:93`).
const K_LEN_NUM_HIGH_BITS: u32 = 8;
/// `kLenNumHighSymbols` (`LzmaDec.c:94`).
const K_LEN_NUM_HIGH_SYMBOLS: u32 = 1 << K_LEN_NUM_HIGH_BITS;

/// `LenLow` (`LzmaDec.c:96`), relative to a length coder's base.
const LEN_LOW: usize = 0;
/// `LenHigh` (`LzmaDec.c:97`), relative to a length coder's base.
const LEN_HIGH: usize = LEN_LOW + 2 * (K_NUM_POS_STATES_MAX << K_LEN_NUM_LOW_BITS);
/// `kNumLenProbs` (`LzmaDec.c:98`) = 512.
const K_NUM_LEN_PROBS: usize = LEN_HIGH + K_LEN_NUM_HIGH_SYMBOLS as usize;
/// `LenChoice` (`LzmaDec.c:100`). Shares slot 0 with the low tree, which starts
/// its walk at index 1 and so never reads it.
const LEN_CHOICE: usize = LEN_LOW;
/// `LenChoice2` (`LzmaDec.c:101`). Same trick at the mid tree's slot 0.
const LEN_CHOICE2: usize = LEN_LOW + (1 << K_LEN_NUM_LOW_BITS);

/// `kNumStates` (`LzmaDec.c:103`).
const K_NUM_STATES: u32 = 12;
/// `kNumStates2` (`LzmaDec.c:104`) — the padded stride used by the `IsMatch` and
/// `IsRep0Long` tables.
const K_NUM_STATES2: usize = 16;
/// `kNumLitStates` (`LzmaDec.c:105`).
const K_NUM_LIT_STATES: u32 = 7;

/// `kStartPosModelIndex` (`LzmaDec.c:107`).
const K_START_POS_MODEL_INDEX: u32 = 4;
/// `kEndPosModelIndex` (`LzmaDec.c:108`).
const K_END_POS_MODEL_INDEX: u32 = 14;
/// `kNumFullDistances` (`LzmaDec.c:109`) = 128, the size of the `SpecPos` table.
const K_NUM_FULL_DISTANCES: usize = 1 << (K_END_POS_MODEL_INDEX >> 1);

/// `kNumPosSlotBits` (`LzmaDec.c:111`).
const K_NUM_POS_SLOT_BITS: u32 = 6;
/// `kNumLenToPosStates` (`LzmaDec.c:112`).
const K_NUM_LEN_TO_POS_STATES: u32 = 4;

/// `kNumAlignBits` (`LzmaDec.c:114`).
const K_NUM_ALIGN_BITS: u32 = 4;
/// `kAlignTableSize` (`LzmaDec.c:115`).
const K_ALIGN_TABLE_SIZE: usize = 1 << K_NUM_ALIGN_BITS;

/// `kMatchMinLen` (`LzmaDec.c:117`).
const K_MATCH_MIN_LEN: u32 = 2;
/// `kMatchSpecLenStart` (`LzmaDec.c:118`) = 274. As a `remainLen` value it means
/// "the stream ended with the end-of-payload marker".
const K_MATCH_SPEC_LEN_START: u32 =
    K_MATCH_MIN_LEN + K_LEN_NUM_LOW_SYMBOLS * 2 + K_LEN_NUM_HIGH_SYMBOLS;
/// `kMatchSpecLen_Error_Data` (`LzmaDec.c:120`) = 512. `remainLen` at or above
/// this latches a data error (`LzmaDec.c:175-180`, `:959-962`).
const K_MATCH_SPEC_LEN_ERROR_DATA: u32 = 1 << 9;
/// `kMatchSpecLen_Error_Fail` (`LzmaDec.c:121`) = 511.
const K_MATCH_SPEC_LEN_ERROR_FAIL: u32 = K_MATCH_SPEC_LEN_ERROR_DATA - 1;

/// `LZMA_LIT_SIZE` (`LzmaDec.c:162`): probabilities per literal context.
const LZMA_LIT_SIZE: usize = 0x300;

/// `LZMA_DIC_MIN` (`LzmaDec.c:171`): the dictionary floor `LzmaProps_Decode`
/// clamps to (`LzmaDec.c:1268-1269`).
const LZMA_DIC_MIN: u32 = 1 << 12;

/// `kBadRepCode` (`LzmaDec.c:663-668`). A stream whose initial range-coder `code`
/// is at or above this would decode a rep-match as its very first symbol, which is
/// illegal — there is nothing to repeat. Checking it once at init is what lets the
/// main loop skip the test per symbol (`LzmaDec.c:344-345`).
const K_BAD_REP_CODE: u32 = 0xC000_0000 - 0x400;

// --- Flat probability-array offsets, rebased from `probs_1664` to `probs[0]`
// --- (`LzmaDec.c:139-151`; `kStartOffset = 1664` at `:129`).

/// `SpecPos` — 128 entries.
const P_SPEC_POS: usize = 0;
/// `IsRep0Long` — 16 states x 16 pos-states.
const P_IS_REP0_LONG: usize = P_SPEC_POS + K_NUM_FULL_DISTANCES;
/// `RepLenCoder` — 512 entries.
const P_REP_LEN_CODER: usize = P_IS_REP0_LONG + (K_NUM_STATES2 << K_NUM_POS_BITS_MAX);
/// `LenCoder` — 512 entries.
const P_LEN_CODER: usize = P_REP_LEN_CODER + K_NUM_LEN_PROBS;
/// `IsMatch` — 16 states x 16 pos-states.
const P_IS_MATCH: usize = P_LEN_CODER + K_NUM_LEN_PROBS;
/// `Align` — 16 entries.
const P_ALIGN: usize = P_IS_MATCH + (K_NUM_STATES2 << K_NUM_POS_BITS_MAX);
/// `IsRep` — 12 entries.
const P_IS_REP: usize = P_ALIGN + K_ALIGN_TABLE_SIZE;
/// `IsRepG0` — 12 entries.
const P_IS_REP_G0: usize = P_IS_REP + K_NUM_STATES as usize;
/// `IsRepG1` — 12 entries.
const P_IS_REP_G1: usize = P_IS_REP_G0 + K_NUM_STATES as usize;
/// `IsRepG2` — 12 entries.
const P_IS_REP_G2: usize = P_IS_REP_G1 + K_NUM_STATES as usize;
/// `PosSlot` — 4 length-states x 64 slots.
const P_POS_SLOT: usize = P_IS_REP_G2 + K_NUM_STATES as usize;
/// `Literal` — the variable-sized tail, `0x300 << (lc + lp)` entries.
const P_LITERAL: usize = P_POS_SLOT + ((K_NUM_LEN_TO_POS_STATES as usize) << K_NUM_POS_SLOT_BITS);
/// `NUM_BASE_PROBS` (`LzmaDec.c:151`, asserted to be 1984 at `:157-159`).
const NUM_BASE_PROBS: usize = P_LITERAL;

const _: () = assert!(NUM_BASE_PROBS == 1984, "Stop_Compiling_Bad_LZMA_PROBS");
const _: () = assert!(P_ALIGN == 1664, "Stop_Compiling_Bad_LZMA_kAlign");
const _: () = assert!(K_MATCH_SPEC_LEN_START == 274);
const _: () = assert!(K_BAD_REP_CODE == 0xBFFF_FC00);

/// The chunk sizes `C_LZMA.cpp:169-170` uses to drive the C decoder. Kept
/// identical so the number of `"read"` / `"write"` callbacks per block matches.
const IN_BUF: usize = 1 << 16;
const OUT_BUF: usize = 1 << 16;

// ---------------------------------------------------------------------------
// Errors, status, properties
// ---------------------------------------------------------------------------

/// Everything that can go wrong, split by what the caller must *do* about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LzmaDecodeError {
    /// The five property bytes are missing, short, or describe a configuration
    /// LZMA does not define — in practice `props[0] >= 9*5*5`
    /// (`LzmaDec.c:1273-1274`).
    ///
    /// `SZ_ERROR_UNSUPPORTED`; DArc maps it to
    /// `FREEARC_ERRCODE_INVALID_COMPRESSOR` (`C_LZMA.cpp:163-166`). It is a
    /// *configuration* fault, not a data fault: the method string, not the
    /// payload, is wrong.
    UnsupportedProps,
    /// The compressed data is corrupt: a match pointing outside the dictionary
    /// (`LzmaDec.c:537-542`), a rep-match as the first symbol
    /// (`LzmaDec.c:979-982`), a non-zero first stream byte
    /// (`LzmaDec.c:966-967`), or a non-drained range coder at the end mark
    /// (`LzmaDec.c:1005-1006`).
    ///
    /// `SZ_ERROR_DATA` -> `FREEARC_ERRCODE_BAD_COMPRESSED_DATA`
    /// (`C_LZMA.cpp:201-205`).
    DataError,
    /// The input ended while the decoder still needed bytes.
    ///
    /// Distinct from [`Self::DataError`] on purpose: the bytes present may be
    /// perfectly valid, and a caller that can supply more (a resumed download, a
    /// multi-volume archive) can act on it. `SZ_ERROR_INPUT_EOF`
    /// (`LzmaDec.c:1359-1360`); DArc has no separate code and folds it into
    /// `FREEARC_ERRCODE_BAD_COMPRESSED_DATA` (`C_LZMA.cpp:217-221`).
    TruncatedInput,
    /// The dictionary or probability array could not be allocated.
    ///
    /// `SZ_ERROR_MEM` -> `FREEARC_ERRCODE_NOT_ENOUGH_MEMORY`
    /// (`C_LZMA.cpp:163-166`, `:173-177`). Reachable from hostile input: the
    /// dictionary size is taken from the archived method string, so a stream may
    /// demand gigabytes. Allocation goes through `try_reserve_exact` precisely so
    /// that this is an error rather than an abort.
    NotEnoughMemory,
    /// The caller's own stream failed; the code is theirs and is passed through
    /// unchanged. See [`StreamError`].
    Stream(StreamError),
    /// An invariant of this port did not hold. `SZ_ERROR_FAIL`
    /// (`LzmaDec.c:1193-1195`), which the C describes as "internal error of code,
    /// memory corruption or hardware failure".
    ///
    /// Every bounds check that the C omits — because a pointer invariant makes it
    /// unnecessary — reports this instead of panicking. It should be unreachable;
    /// it is not a substitute for [`Self::DataError`].
    Internal,
}

impl From<StreamError> for LzmaDecodeError {
    fn from(e: StreamError) -> Self {
        LzmaDecodeError::Stream(e)
    }
}

/// `ELzmaStatus` (`LzmaDec.h:106-113`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    /// `LZMA_STATUS_FINISHED_WITH_MARK`: the end-of-payload marker was decoded and
    /// the range coder was drained.
    FinishedWithMark,
    /// `LZMA_STATUS_NOT_FINISHED`: the output limit was reached mid-stream.
    NotFinished,
    /// `LZMA_STATUS_NEEDS_MORE_INPUT`.
    NeedsMoreInput,
    /// `LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK`.
    MaybeFinishedWithoutMark,
}

/// `ELzmaFinishMode` (`LzmaDec.h:85-89`). It only has meaning when decoding
/// reaches the output limit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FinishMode {
    /// `LZMA_FINISH_ANY`: stop at the limit, no lookahead. This is what the
    /// streaming driver uses, because it never knows where the end is.
    Any,
    /// `LZMA_FINISH_END`: the block must be finished at the limit, so the decoder
    /// looks ahead one symbol to check for the end marker.
    End,
}

/// How a stream ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Finish {
    /// The end-of-payload marker was decoded (`LzmaDec.c:523-528`) and the range
    /// coder was drained (`code == 0`, `LzmaDec.c:1005-1006`). This is the normal
    /// outcome for a DArc stream.
    WithMark,
    /// The dictionary window filled exactly while the range coder happened to be
    /// drained (`LzmaDec.c:1020-1024`).
    ///
    /// The C's `LzmaDec_DecodeToBuf` driver treats this as success and
    /// `C_LZMA.cpp:212-216` follows it. It is reported separately here because it
    /// is *not* proof of a complete stream: it is `code == 0` observed at a window
    /// boundary, which a mid-stream coincidence can also produce (probability
    /// 2^-32 per boundary). A caller that knows the payload length should check it.
    MaybeWithoutMark,
}

/// What a successful decode consumed and produced.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DecodeSummary {
    /// Bytes pulled from the [`InStream`]. At least [`Self::input_consumed`]; the
    /// excess is whatever sat in the 64 KiB read buffer past the end marker.
    pub input_read: u64,
    /// Bytes the decoder actually consumed. For a well-formed stream this is
    /// exactly the compressed length, so a caller that concatenates streams can
    /// use it to find the next one.
    pub input_consumed: u64,
    /// Bytes written to the [`OutStream`].
    pub output_written: u64,
    /// Why decoding stopped.
    pub finish: Finish,
}

/// `CLzmaProps` (`LzmaDec.h:28-35`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LzmaDecProps {
    /// Literal context bits, 0..=8.
    pub lc: u32,
    /// Literal position bits, 0..=4.
    pub lp: u32,
    /// Position bits, 0..=4.
    pub pb: u32,
    /// Dictionary size in bytes, already clamped to [`LZMA_DIC_MIN`].
    pub dic_size: u32,
}

impl LzmaDecProps {
    /// `LzmaProps_Decode` (`LzmaDec.c:1258-1282`).
    ///
    /// The `d >= 9 * 5 * 5` rejection at `LzmaDec.c:1273-1274` is the check whose
    /// absence made [`crate::decoder`] unusable here. Without it, `props[0] = 255`
    /// yields `pb = 5`, a `pb_mask` of 31, and an out-of-range index into every
    /// 16-entry pos-state table. It is reachable end to end: `parse_LZMA` reads
    /// `pb` from the archived method string with a bare `parseInt` and no range
    /// check (`C_LZMA.cpp:381`), and `encode_props` truncates the result to a byte
    /// — `lzma:8m:pb9` is the entire exploit.
    pub fn parse(data: &[u8]) -> Result<Self, LzmaDecodeError> {
        // LzmaDec.c:1263-1264
        if data.len() < LZMA_PROPS_SIZE {
            return Err(LzmaDecodeError::UnsupportedProps);
        }
        // LzmaDec.c:1266 — little-endian dictionary size.
        let mut dic_size = u32::from(data[1])
            | (u32::from(data[2]) << 8)
            | (u32::from(data[3]) << 16)
            | (u32::from(data[4]) << 24);
        // LzmaDec.c:1268-1269
        if dic_size < LZMA_DIC_MIN {
            dic_size = LZMA_DIC_MIN;
        }

        // LzmaDec.c:1272-1274 — the validation.
        let d = u32::from(data[0]);
        if d >= 9 * 5 * 5 {
            return Err(LzmaDecodeError::UnsupportedProps);
        }

        // LzmaDec.c:1276-1279
        let lc = d % 9;
        let d = d / 9;
        let pb = d / 5;
        let lp = d % 5;

        // Implied by the check above (lc <= 8, lp <= 4, pb <= 4), and restated so
        // that every downstream shift and table bound has a local premise rather
        // than a remote one. `lc + lp <= 12` is what keeps the literal table's
        // `0x300 << (lc + lp)` from overflowing and what makes the index argument
        // in `decode_real` exact.
        if lc > 8 || lp > 4 || pb > 4 || lc + lp > 12 {
            return Err(LzmaDecodeError::UnsupportedProps);
        }

        Ok(LzmaDecProps {
            lc,
            lp,
            pb,
            dic_size,
        })
    }

    /// `LzmaProps_GetNumProbs` (`LzmaDec.c:164`).
    fn num_probs(&self) -> usize {
        NUM_BASE_PROBS + (LZMA_LIT_SIZE << (self.lc + self.lp))
    }

    /// The allocated window size, `LzmaDec_Allocate`'s rounding
    /// (`LzmaDec.c:1315-1323`).
    ///
    /// It is >= `dic_size`, and it is what `dicPos` wraps against — conflating the
    /// two is a class of bug this port keeps separate on purpose.
    fn dic_buf_size(&self) -> u64 {
        let dict_size = u64::from(self.dic_size);
        // LzmaDec.c:1317-1319
        let mask: u64 = if dict_size >= (1u64 << 30) {
            (1u64 << 22) - 1
        } else if dict_size >= (1u64 << 22) {
            (1u64 << 20) - 1
        } else {
            (1u64 << 12) - 1
        };
        // LzmaDec.c:1320-1322. In 64-bit arithmetic the `dicBufSize < dictSize`
        // wrap-guard cannot fire, but the clamp is kept so the shapes match.
        let rounded = (dict_size + mask) & !mask;
        if rounded < dict_size { dict_size } else { rounded }
    }
}

/// `ELzmaDummy` (`LzmaDec.c:704-710`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dummy {
    /// `DUMMY_INPUT_EOF` — the trial decode ran out of bytes.
    InputEof,
    /// `DUMMY_LIT`.
    Lit,
    /// `DUMMY_MATCH`.
    Match,
    /// `DUMMY_REP`.
    Rep,
}

/// `IS_DUMMY_END_MARKER_POSSIBLE` (`LzmaDec.c:713`).
///
/// Spelled out rather than written as a wildcard match: only a non-rep match can
/// carry the end marker, and the other three have to be named for that to stay
/// true when someone adds a variant.
fn is_end_marker_possible(d: Dummy) -> bool {
    match d {
        Dummy::Match => true,
        Dummy::InputEof => false,
        Dummy::Lit => false,
        Dummy::Rep => false,
    }
}

/// `dic[dicPos - dist + (dicPos < dist ? dicBufSize : 0)]`, the circular back
/// reference used at `LzmaDec.c:269`, `:291`, `:350`, `:565` and `:646`.
///
/// Returns `None` when `dist` exceeds the window, which the distance validation at
/// `LzmaDec.c:537-542` already excludes; the C simply wraps a `SizeT` there.
#[inline]
fn back_pos(dic_pos: usize, dist: usize, dic_buf_size: usize) -> Option<usize> {
    if dist > dic_buf_size {
        return None;
    }
    Some(if dic_pos < dist {
        // No overflow: `dist <= dic_buf_size`, so the subtraction is done first,
        // and `dic_pos < dist` keeps the sum below `dic_buf_size`.
        (dic_buf_size - dist) + dic_pos
    } else {
        dic_pos - dist
    })
}

// ---------------------------------------------------------------------------
// The decoder
// ---------------------------------------------------------------------------

/// `CLzmaDec` (`LzmaDec.h:53-74`) plus this port's input cursor.
///
/// `dic_pos` (`LzmaDec.h:61`) and `processed_pos` (`LzmaDec.h:65`) are separate
/// variables and must stay that way: the first is a position **inside the circular
/// window** and wraps at `dic_buf_size`; the second is the total byte count, is
/// never reset, and is what `pos_state`, the literal context and the distance
/// validation are computed from. [`crate::decoder`] conflated them, which is only
/// sound while the whole output fits in one buffer.
pub struct Decoder {
    prop: LzmaDecProps,
    /// Flat probability array in the C's order; see the module docs.
    probs: Vec<u16>,
    /// The circular dictionary window, `dic_buf_size` bytes.
    dic: Vec<u8>,
    dic_buf_size: usize,
    /// Write cursor **within the window**; wraps at `dic_buf_size`.
    dic_pos: usize,
    /// Index into whichever input slice is currently being decoded — the C's
    /// `p->buf` pointer (`LzmaDec.h:62`).
    buf_pos: usize,
    range: u32,
    code: u32,
    /// Total bytes decoded (mod 2^32, as in the C).
    processed_pos: u32,
    /// 0 until the window has been filled once, then `prop.dic_size`
    /// (`LzmaDec.c:671-684`).
    check_dic_size: u32,
    reps: [u32; 4],
    state: u32,
    /// Decoder status *and* pending match length; see the table at
    /// `LzmaDec.c:173-181`.
    remain_len: u32,
    /// `tempBuf` (`LzmaDec.h:73`): the carry buffer for a symbol split across two
    /// input chunks.
    temp_buf: [u8; LZMA_REQUIRED_INPUT_MAX],
    temp_buf_size: usize,
}

impl Decoder {
    /// Build a decoder from the five property bytes.
    ///
    /// Ports `LzmaProps_Decode` + `LzmaDec_AllocateProbs2` + `LzmaDec_Allocate`
    /// (`LzmaDec.c:1258`, `:1284`, `:1308`), then `LzmaDec_Init`
    /// (`LzmaDec.c:922-926`).
    pub fn new(props: &[u8]) -> Result<Self, LzmaDecodeError> {
        Self::with_props(LzmaDecProps::parse(props)?)
    }

    /// As [`Self::new`], for a caller that already parsed the properties.
    pub fn with_props(prop: LzmaDecProps) -> Result<Self, LzmaDecodeError> {
        let num_probs = prop.num_probs();

        let dic_buf_size = usize::try_from(prop.dic_buf_size())
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;

        // `LzmaDec_Allocate` returns SZ_ERROR_MEM when malloc fails
        // (`LzmaDec.c:1328-1333`). The dictionary size arrives from the archive,
        // so a hostile stream can ask for gigabytes; `try_reserve_exact` turns
        // that into an error instead of the abort a plain `vec![]` would take.
        let mut probs: Vec<u16> = Vec::new();
        probs
            .try_reserve_exact(num_probs)
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;
        probs.resize(num_probs, PROB_INIT_VALUE);

        let mut dic: Vec<u8> = Vec::new();
        dic.try_reserve_exact(dic_buf_size)
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;
        dic.resize(dic_buf_size, 0);

        let mut d = Decoder {
            prop,
            probs,
            dic,
            dic_buf_size,
            dic_pos: 0,
            buf_pos: 0,
            range: 0,
            code: 0,
            processed_pos: 0,
            check_dic_size: 0,
            reps: [1; 4],
            state: 0,
            remain_len: 0,
            temp_buf: [0; LZMA_REQUIRED_INPUT_MAX],
            temp_buf_size: 0,
        };
        d.reset();
        Ok(d)
    }

    /// `LzmaDec_Init` (`LzmaDec.c:922-926`) — start a new stream on the same
    /// allocation.
    pub fn reset(&mut self) {
        self.dic_pos = 0;
        // LzmaDec_InitDicAndState(p, True, True) (`LzmaDec.c:907-920`).
        self.processed_pos = 0;
        self.check_dic_size = 0;
        self.temp_buf_size = 0;
        self.remain_len = K_MATCH_SPEC_LEN_START + 2;
    }

    /// The properties in force.
    pub fn props(&self) -> LzmaDecProps {
        self.prop
    }

    /// The allocated window size (`LzmaDec.c:1315-1323`).
    pub fn dic_buf_size(&self) -> usize {
        self.dic_buf_size
    }

    // -----------------------------------------------------------------------
    // LZMA_DECODE_REAL (LzmaDec.c:234-611)
    // -----------------------------------------------------------------------

    /// `LzmaDec_DecodeReal_3` (`LzmaDec.c:234-611`).
    ///
    /// Decodes symbols into the window until `dic_pos == limit` or
    /// `buf_pos == buf_limit`, whichever comes first. The C reads input through a
    /// bare `*buf++` and relies on the caller having guaranteed
    /// `LZMA_REQUIRED_INPUT_MAX` readable bytes past `bufLimit`
    /// (`LzmaDec.c:203-211`); here every such read is bounds-checked and reports
    /// [`LzmaDecodeError::Internal`] if that guarantee were ever broken.
    ///
    /// On return `remain_len` carries the outcome per the table at
    /// `LzmaDec.c:173-181`.
    fn decode_real(
        &mut self,
        limit: usize,
        src: &[u8],
        buf_limit: usize,
    ) -> Result<(), LzmaDecodeError> {
        let (probs, dic) = (&mut self.probs[..], &mut self.dic[..]);
        let dic_buf_size = self.dic_buf_size;

        // LzmaDec.c:237-253 — everything hot is pulled into locals.
        let mut state = self.state;
        let mut rep0 = self.reps[0];
        let mut rep1 = self.reps[1];
        let mut rep2 = self.reps[2];
        let mut rep3 = self.reps[3];
        let pb_mask: u32 = (1u32 << self.prop.pb) - 1;
        let lc = self.prop.lc;
        let lp_mask: u32 = (0x100u32 << self.prop.lp) - (0x100u32 >> lc);
        let mut dic_pos = self.dic_pos;
        let mut processed_pos = self.processed_pos;
        // Read-only here: the promotion when the window first fills happens in
        // `decode_real2` (`LzmaDec.c:696-697`) and `write_rem` (`:636-637`).
        let check_dic_size = self.check_dic_size;
        let mut len: u32 = 0;
        let mut buf = self.buf_pos;
        let mut range = self.range;
        let mut code = self.code;

        // `NORMALIZE` (`LzmaDec.c:22`).
        macro_rules! norm {
            () => {
                if range < TOP_VALUE {
                    let b = match src.get(buf) {
                        Some(v) => *v,
                        None => return Err(LzmaDecodeError::Internal),
                    };
                    range <<= 8;
                    code = (code << 8) | u32::from(b);
                    buf += 1;
                }
            };
        }

        // `IF_BIT_0` + `UPDATE_0` / `UPDATE_1` (`LzmaDec.c:24-26`) fused into one
        // expression. The C splits them only so the two arms can carry different
        // side effects; the arithmetic is identical.
        macro_rules! bit {
            ($idx:expr) => {{
                let i: usize = $idx;
                let p = match probs.get_mut(i) {
                    Some(v) => v,
                    None => return Err(LzmaDecodeError::Internal),
                };
                let ttt = u32::from(*p);
                norm!();
                let bound = (range >> NUM_BIT_MODEL_TOTAL_BITS) * ttt;
                if code < bound {
                    range = bound;
                    *p = (ttt + ((BIT_MODEL_TOTAL - ttt) >> NUM_MOVE_BITS)) as u16;
                    0u32
                } else {
                    range -= bound;
                    code -= bound;
                    *p = (ttt - (ttt >> NUM_MOVE_BITS)) as u16;
                    1u32
                }
            }};
        }

        // `TREE_GET_BIT` (`LzmaDec.c:31`): index with the old node, then descend.
        macro_rules! tree_bit {
            ($base:expr, $i:ident) => {{
                let b = bit!($base + $i as usize);
                $i = ($i + $i) + b;
            }};
        }

        // `do { ... } while (dicPos < limit && buf < bufLimit)` (`LzmaDec.c:255`,
        // `:593`). The C's `continue` jumps to that condition, not past it, so the
        // body sits in a labelled block.
        'main: loop {
            'body: {
                // LzmaDec.c:260 — CALC_POS_STATE / COMBINED_PS_STATE (`:167-168`).
                let pos_state = (processed_pos & pb_mask) << 4;
                let combined = pos_state + state;

                // LzmaDec.c:262-263
                if bit!(P_IS_MATCH + combined as usize) == 0 {
                    // ---- literal (LzmaDec.c:264-321) ----
                    let mut prob = P_LITERAL;
                    // LzmaDec.c:268-269
                    if processed_pos != 0 || check_dic_size != 0 {
                        let prev_idx = if dic_pos == 0 { dic_buf_size } else { dic_pos } - 1;
                        let prev = u32::from(match dic.get(prev_idx) {
                            Some(v) => *v,
                            None => return Err(LzmaDecodeError::Internal),
                        });
                        // `processedPos << 8` discards high bits exactly as the C's
                        // UInt32 does; the low 8 bits it leaves are what `prev`
                        // fills, so the add cannot overflow.
                        let ctx = (((processed_pos << 8) + prev) & lp_mask) << lc;
                        prob += (3 * ctx) as usize;
                    }
                    processed_pos = processed_pos.wrapping_add(1);

                    let mut symbol: u32 = 1;
                    if state < K_NUM_LIT_STATES {
                        // LzmaDec.c:272-288
                        state -= if state < 4 { state } else { 3 };
                        while symbol < 0x100 {
                            tree_bit!(prob, symbol);
                        }
                    } else {
                        // LzmaDec.c:289-317 — literal coded against the byte at
                        // `rep0`, i.e. MATCHED_LITER_DEC (`:60-65`).
                        let mpos = match back_pos(dic_pos, rep0 as usize, dic_buf_size) {
                            Some(v) => v,
                            None => return Err(LzmaDecodeError::DataError),
                        };
                        let mut match_byte = u32::from(match dic.get(mpos) {
                            Some(v) => *v,
                            None => return Err(LzmaDecodeError::Internal),
                        });
                        let mut offs: u32 = 0x100;
                        state -= if state < 10 { 3 } else { 6 };
                        while symbol < 0x100 {
                            match_byte += match_byte;
                            let bitv = offs;
                            offs &= match_byte;
                            let prob_lit = prob + (offs + bitv + symbol) as usize;
                            let b = bit!(prob_lit);
                            if b == 0 {
                                offs ^= bitv;
                            }
                            symbol = (symbol + symbol) + b;
                        }
                    }

                    // LzmaDec.c:319-320
                    match dic.get_mut(dic_pos) {
                        Some(v) => *v = symbol as u8,
                        None => return Err(LzmaDecodeError::Internal),
                    }
                    dic_pos += 1;
                    break 'body;
                }

                // ---- match (LzmaDec.c:323-590) ----
                let len_coder_base;
                // LzmaDec.c:325-326
                if bit!(P_IS_REP + state as usize) == 0 {
                    // LzmaDec.c:328-331 — a plain match.
                    state += K_NUM_STATES;
                    len_coder_base = P_LEN_CODER;
                } else {
                    // LzmaDec.c:334-390 — a rep match.
                    if bit!(P_IS_REP_G0 + state as usize) == 0 {
                        // LzmaDec.c:339-355 — short rep (one byte at rep0).
                        if bit!(P_IS_REP0_LONG + combined as usize) == 0 {
                            // The `checkDicSize == 0 && processedPos == 0` case the
                            // C comments out at `:344-345` is excluded by the
                            // `kBadRepCode` check at init (`:979-982`); the
                            // `dicPos == limit` case at `:346-348` is excluded by
                            // the caller. Both premises hold here too.
                            let pos = match back_pos(dic_pos, rep0 as usize, dic_buf_size) {
                                Some(v) => v,
                                None => return Err(LzmaDecodeError::DataError),
                            };
                            let b = match dic.get(pos) {
                                Some(v) => *v,
                                None => return Err(LzmaDecodeError::Internal),
                            };
                            match dic.get_mut(dic_pos) {
                                Some(v) => *v = b,
                                None => return Err(LzmaDecodeError::Internal),
                            }
                            dic_pos += 1;
                            processed_pos = processed_pos.wrapping_add(1);
                            state = if state < K_NUM_LIT_STATES { 9 } else { 11 };
                            break 'body;
                        }
                    } else {
                        // LzmaDec.c:358-387 — rep1 / rep2 / rep3.
                        let distance;
                        if bit!(P_IS_REP_G1 + state as usize) == 0 {
                            distance = rep1;
                        } else {
                            if bit!(P_IS_REP_G2 + state as usize) == 0 {
                                distance = rep2;
                            } else {
                                distance = rep3;
                                rep3 = rep2;
                            }
                            rep2 = rep1;
                        }
                        rep1 = rep0;
                        rep0 = distance;
                    }
                    // LzmaDec.c:388-389
                    state = if state < K_NUM_LIT_STATES { 8 } else { 11 };
                    len_coder_base = P_REP_LEN_CODER;
                }

                // ---- length (LzmaDec.c:426-459) ----
                // GET_LEN_STATE is `posState`, already shifted (`LzmaDec.c:169`).
                {
                    if bit!(len_coder_base + LEN_CHOICE) == 0 {
                        let pl = len_coder_base + LEN_LOW + pos_state as usize;
                        len = 1;
                        tree_bit!(pl, len);
                        tree_bit!(pl, len);
                        tree_bit!(pl, len);
                        len -= 8;
                    } else if bit!(len_coder_base + LEN_CHOICE2) == 0 {
                        // LzmaDec.c:444-449 — note the deliberate absence of a
                        // `len -= 8`: leaving the tree's 8..15 in place *is* the
                        // `+ kLenNumLowSymbols` offset.
                        let pl = len_coder_base + LEN_LOW + pos_state as usize + 8;
                        len = 1;
                        tree_bit!(pl, len);
                        tree_bit!(pl, len);
                        tree_bit!(pl, len);
                    } else {
                        // LzmaDec.c:453-456
                        let pl = len_coder_base + LEN_HIGH;
                        len = 1;
                        while len < K_LEN_NUM_HIGH_SYMBOLS {
                            tree_bit!(pl, len);
                        }
                        len -= K_LEN_NUM_HIGH_SYMBOLS;
                        len += K_LEN_NUM_LOW_SYMBOLS * 2;
                    }
                }

                // ---- distance, for a non-rep match (LzmaDec.c:462-544) ----
                if state >= K_NUM_STATES {
                    let slot_state = if len < K_NUM_LEN_TO_POS_STATES {
                        len
                    } else {
                        K_NUM_LEN_TO_POS_STATES - 1
                    };
                    let ps_base = P_POS_SLOT + ((slot_state as usize) << K_NUM_POS_SLOT_BITS);
                    // TREE_6_DECODE (`LzmaDec.c:48-56`).
                    let mut distance: u32 = 1;
                    for _ in 0..6 {
                        tree_bit!(ps_base, distance);
                    }
                    distance -= 0x40;

                    // LzmaDec.c:468-530
                    if distance >= K_START_POS_MODEL_INDEX {
                        let pos_slot = distance;
                        let mut num_direct_bits = (distance >> 1) - 1;
                        distance = 2 | (distance & 1);
                        if pos_slot < K_END_POS_MODEL_INDEX {
                            // LzmaDec.c:473-487 — SpecPos, reverse bit tree.
                            // Bound: `distance` peaks at (3 << 5) + 1 + 30 = 127,
                            // exactly the last SpecPos slot.
                            distance <<= num_direct_bits;
                            let mut m: u32 = 1;
                            distance += 1;
                            loop {
                                // REV_BIT_VAR (`LzmaDec.c:36`).
                                let b = bit!(P_SPEC_POS + distance as usize);
                                if b == 0 {
                                    distance += m;
                                    m += m;
                                } else {
                                    m += m;
                                    distance += m;
                                }
                                num_direct_bits -= 1;
                                if num_direct_bits == 0 {
                                    break;
                                }
                            }
                            distance -= m;
                        } else {
                            // LzmaDec.c:490-522 — direct bits then the align tree.
                            num_direct_bits -= K_NUM_ALIGN_BITS;
                            loop {
                                norm!();
                                range >>= 1;
                                // LzmaDec.c:496-502, branch-free `if (code >=
                                // range) { code -= range; distance |= 1; }`.
                                code = code.wrapping_sub(range);
                                let t = 0u32.wrapping_sub(code >> 31);
                                distance = (distance << 1).wrapping_add(t.wrapping_add(1));
                                code = code.wrapping_add(range & t);
                                num_direct_bits -= 1;
                                if num_direct_bits == 0 {
                                    break;
                                }
                            }
                            distance <<= K_NUM_ALIGN_BITS;
                            {
                                // REV_BIT_CONST x3 then REV_BIT_LAST
                                // (`LzmaDec.c:37-38`, `:517-521`). `i` peaks at 15,
                                // the last Align slot.
                                let mut i: u32 = 1;
                                let b = bit!(P_ALIGN + i as usize);
                                i += if b == 0 { 1 } else { 2 };
                                let b = bit!(P_ALIGN + i as usize);
                                i += if b == 0 { 2 } else { 4 };
                                let b = bit!(P_ALIGN + i as usize);
                                i += if b == 0 { 4 } else { 8 };
                                let b = bit!(P_ALIGN + i as usize);
                                if b == 0 {
                                    i -= 8;
                                }
                                distance |= i;
                            }
                            // LzmaDec.c:523-528 — END_OF_PAYLOAD_MARKER.
                            if distance == 0xFFFF_FFFF {
                                len = K_MATCH_SPEC_LEN_START;
                                state -= K_NUM_STATES;
                                break 'main;
                            }
                        }
                    }

                    // LzmaDec.c:532-536
                    rep3 = rep2;
                    rep2 = rep1;
                    rep1 = rep0;
                    rep0 = distance + 1; // distance != 0xFFFFFFFF here
                    state = if state < K_NUM_STATES + K_NUM_LIT_STATES {
                        K_NUM_LIT_STATES
                    } else {
                        K_NUM_LIT_STATES + 3
                    };

                    // LzmaDec.c:537-543 — THE distance validation. Without it a
                    // match can point before the start of the stream, which is the
                    // `out[out.len() - rep]` underflow in `crate::decoder`.
                    let horizon = if check_dic_size == 0 {
                        processed_pos
                    } else {
                        check_dic_size
                    };
                    if distance >= horizon {
                        len += K_MATCH_SPEC_LEN_ERROR_DATA + K_MATCH_MIN_LEN;
                        break 'main;
                    }
                }

                // LzmaDec.c:546
                len += K_MATCH_MIN_LEN;

                // ---- copy (LzmaDec.c:548-590) ----
                {
                    // LzmaDec.c:553-562 — out of room: keep `len` and resume later.
                    // This is what carries an over-long match across an output
                    // boundary instead of truncating it.
                    let rem = limit.saturating_sub(dic_pos);
                    if rem == 0 {
                        break 'main;
                    }
                    let cur_len = if (rem as u64) < u64::from(len) {
                        rem
                    } else {
                        len as usize
                    };
                    let mut pos = match back_pos(dic_pos, rep0 as usize, dic_buf_size) {
                        Some(v) => v,
                        None => return Err(LzmaDecodeError::DataError),
                    };
                    processed_pos = processed_pos.wrapping_add(cur_len as u32);
                    len -= cur_len as u32;

                    // The C's two arms (`:570-579` fast, `:582-588` wrapping) are
                    // one forward byte-at-a-time copy with overlap allowed; only
                    // the second has to handle the source run crossing the end of
                    // the window. Split the same way, and take `copy_within` when
                    // source and destination do not overlap.
                    if cur_len <= dic_buf_size - pos {
                        let src_end = pos + cur_len;
                        let dst_end = dic_pos + cur_len;
                        if src_end > dic.len() || dst_end > dic.len() {
                            return Err(LzmaDecodeError::Internal);
                        }
                        if src_end <= dic_pos {
                            dic.copy_within(pos..src_end, dic_pos);
                        } else {
                            // Overlapping: must stay forward, byte by byte, so a
                            // run shorter than the match length repeats.
                            for k in 0..cur_len {
                                let b = match dic.get(pos + k) {
                                    Some(v) => *v,
                                    None => return Err(LzmaDecodeError::Internal),
                                };
                                match dic.get_mut(dic_pos + k) {
                                    Some(v) => *v = b,
                                    None => return Err(LzmaDecodeError::Internal),
                                }
                            }
                        }
                        dic_pos = dst_end;
                    } else {
                        for _ in 0..cur_len {
                            let b = match dic.get(pos) {
                                Some(v) => *v,
                                None => return Err(LzmaDecodeError::Internal),
                            };
                            match dic.get_mut(dic_pos) {
                                Some(v) => *v = b,
                                None => return Err(LzmaDecodeError::Internal),
                            }
                            dic_pos += 1;
                            pos += 1;
                            if pos == dic_buf_size {
                                pos = 0;
                            }
                        }
                    }
                }
            }

            // LzmaDec.c:593
            if !(dic_pos < limit && buf < buf_limit) {
                break 'main;
            }
        }

        // LzmaDec.c:595-607
        norm!();
        self.buf_pos = buf;
        self.range = range;
        self.code = code;
        self.remain_len = len;
        self.dic_pos = dic_pos;
        self.processed_pos = processed_pos;
        self.reps = [rep0, rep1, rep2, rep3];
        self.state = state;

        // LzmaDec.c:608-610
        if len >= K_MATCH_SPEC_LEN_ERROR_DATA {
            return Err(LzmaDecodeError::DataError);
        }
        Ok(())
    }

    /// `LzmaDec_DecodeReal2` (`LzmaDec.c:686-700`): clip `limit` so the window
    /// cannot be over-filled before it is first full, then promote
    /// `check_dic_size` once it is (`LzmaDec.c:696-697`).
    fn decode_real2(
        &mut self,
        limit: usize,
        src: &[u8],
        buf_limit: usize,
    ) -> Result<(), LzmaDecodeError> {
        let mut limit = limit;
        if self.check_dic_size == 0 {
            // The invariant at `LzmaDec.c:676-683` is `processedPos < dicSize`
            // whenever `checkDicSize == 0`; `saturating_sub` states it locally
            // rather than trusting it.
            let rem = self.prop.dic_size.saturating_sub(self.processed_pos) as usize;
            if limit.saturating_sub(self.dic_pos) > rem {
                limit = self.dic_pos + rem;
            }
        }
        let res = self.decode_real(limit, src, buf_limit);
        if self.check_dic_size == 0 && self.processed_pos >= self.prop.dic_size {
            self.check_dic_size = self.prop.dic_size;
        }
        res
    }

    /// `LzmaDec_WriteRem` (`LzmaDec.c:616-652`).
    ///
    /// Emits as much of a *pending* match as fits below `limit`. This is the half
    /// of requirement "carry an over-long match across a boundary" that runs on
    /// re-entry; `decode_real` produced the pending `remain_len` at `:569`.
    fn write_rem(&mut self, limit: usize) -> Result<(), LzmaDecodeError> {
        // LzmaDec.c:618-620
        let mut len = self.remain_len;
        if len == 0 {
            return Ok(());
        }
        let mut dic_pos = self.dic_pos;
        // LzmaDec.c:626-634
        let rem = limit.saturating_sub(dic_pos);
        if (rem as u64) < u64::from(len) {
            len = rem as u32;
            if len == 0 {
                return Ok(());
            }
        }

        // LzmaDec.c:636-637 — the window fills mid-match.
        if self.check_dic_size == 0 && self.prop.dic_size.saturating_sub(self.processed_pos) <= len {
            self.check_dic_size = self.prop.dic_size;
        }

        // LzmaDec.c:639-640
        self.processed_pos = self.processed_pos.wrapping_add(len);
        self.remain_len -= len;

        // LzmaDec.c:644-649
        let rep0 = self.reps[0] as usize;
        let dic_buf_size = self.dic_buf_size;
        for _ in 0..len {
            let pos = match back_pos(dic_pos, rep0, dic_buf_size) {
                Some(v) => v,
                None => return Err(LzmaDecodeError::DataError),
            };
            let b = match self.dic.get(pos) {
                Some(v) => *v,
                None => return Err(LzmaDecodeError::Internal),
            };
            match self.dic.get_mut(dic_pos) {
                Some(v) => *v = b,
                None => return Err(LzmaDecodeError::Internal),
            }
            dic_pos += 1;
        }
        self.dic_pos = dic_pos;
        Ok(())
    }

    // -----------------------------------------------------------------------
    // LzmaDec_TryDummy (LzmaDec.c:715-904)
    // -----------------------------------------------------------------------

    /// `LzmaDec_TryDummy` (`LzmaDec.c:715-904`): decode one symbol against a copy
    /// of the range-coder state, touching neither the probabilities nor the
    /// window, purely to find out **how many input bytes that symbol needs** and
    /// what kind it is.
    ///
    /// This is the mechanism that makes short input honest. Every range-coder
    /// renormalization here is guarded (`NORMALIZE_CHECK`, `LzmaDec.c:70`) and
    /// yields [`Dummy::InputEof`] rather than reading past the end, which is
    /// exactly what [`crate::decoder`]'s `unwrap_or(0)` does not do.
    ///
    /// Returns the symbol kind and the number of bytes it would consume.
    ///
    /// `#[allow(unused_assignments)]`: the final `NORMALIZE_CHECK`
    /// (`LzmaDec.c:900`) exists only for its **input-EOF test** — it is what makes
    /// the returned byte count cover the trailing renormalization — and the C
    /// likewise throws away the `range`/`code` it computes there, since this
    /// function writes nothing back into the decoder.
    #[allow(unused_assignments)]
    fn try_dummy(&self, src: &[u8], buf_limit: usize) -> Result<(Dummy, usize), LzmaDecodeError> {
        let probs = &self.probs[..];
        let dic = &self.dic[..];
        let mut range = self.range;
        let mut code = self.code;
        let mut buf: usize = 0;
        let mut state = self.state;
        // `bufOut` may not exceed the slice; a short slice simply means EOF sooner.
        let buf_limit = buf_limit.min(src.len());

        // `NORMALIZE_CHECK` (`LzmaDec.c:70`).
        macro_rules! norm_chk {
            () => {
                if range < TOP_VALUE {
                    if buf >= buf_limit {
                        return Ok((Dummy::InputEof, buf));
                    }
                    let b = match src.get(buf) {
                        Some(v) => *v,
                        None => return Ok((Dummy::InputEof, buf)),
                    };
                    range <<= 8;
                    code = (code << 8) | u32::from(b);
                    buf += 1;
                }
            };
        }

        // `IF_BIT_0_CHECK` + `UPDATE_*_CHECK` (`LzmaDec.c:72-74`): no probability
        // is written back, which is why this is a `&self` method.
        macro_rules! bit_chk {
            ($idx:expr) => {{
                let i: usize = $idx;
                let ttt = u32::from(match probs.get(i) {
                    Some(v) => *v,
                    None => return Err(LzmaDecodeError::Internal),
                });
                norm_chk!();
                let bound = (range >> NUM_BIT_MODEL_TOTAL_BITS) * ttt;
                if code < bound {
                    range = bound;
                    0u32
                } else {
                    range -= bound;
                    code -= bound;
                    1u32
                }
            }};
        }

        // `GET_BIT_CHECK` inside `TREE_DECODE_CHECK` (`LzmaDec.c:78-80`).
        macro_rules! tree_bit_chk {
            ($base:expr, $i:ident) => {{
                let b = bit_chk!($base + $i as usize);
                $i = ($i + $i) + b;
            }};
        }

        // LzmaDec.c:729-731
        let pb_mask: u32 = (1u32 << self.prop.pb) - 1;
        let pos_state = (self.processed_pos & pb_mask) << 4;
        let combined = pos_state + state;

        if bit_chk!(P_IS_MATCH + combined as usize) == 0 {
            // ---- literal (LzmaDec.c:733-766) ----
            let mut prob = P_LITERAL;
            // LzmaDec.c:737-740. Algebraically the same context as `:268-269`,
            // written the other way round upstream; both are ported as written so
            // a side-by-side read matches.
            if self.check_dic_size != 0 || self.processed_pos != 0 {
                let prev_idx = if self.dic_pos == 0 {
                    self.dic_buf_size
                } else {
                    self.dic_pos
                } - 1;
                let prev = u32::from(match dic.get(prev_idx) {
                    Some(v) => *v,
                    None => return Err(LzmaDecodeError::Internal),
                });
                let ctx = ((self.processed_pos & ((1u32 << self.prop.lp) - 1)) << self.prop.lc)
                    + (prev >> (8 - self.prop.lc));
                prob += LZMA_LIT_SIZE * ctx as usize;
            }

            let mut symbol: u32 = 1;
            if state < K_NUM_LIT_STATES {
                // LzmaDec.c:742-746
                while symbol < 0x100 {
                    tree_bit_chk!(prob, symbol);
                }
            } else {
                // LzmaDec.c:747-764
                let mpos = match back_pos(self.dic_pos, self.reps[0] as usize, self.dic_buf_size) {
                    Some(v) => v,
                    None => return Err(LzmaDecodeError::DataError),
                };
                let mut match_byte = u32::from(match dic.get(mpos) {
                    Some(v) => *v,
                    None => return Err(LzmaDecodeError::Internal),
                });
                let mut offs: u32 = 0x100;
                while symbol < 0x100 {
                    match_byte += match_byte;
                    let bitv = offs;
                    offs &= match_byte;
                    let b = bit_chk!(prob + (offs + bitv + symbol) as usize);
                    if b == 0 {
                        offs ^= bitv;
                    }
                    symbol = (symbol + symbol) + b;
                }
            }
            // LzmaDec.c:765, then :900-903
            norm_chk!();
            return Ok((Dummy::Lit, buf));
        }

        // ---- match or rep (LzmaDec.c:767-897) ----
        let res;
        let len_coder_base;
        // LzmaDec.c:772-773
        if bit_chk!(P_IS_REP + state as usize) == 0 {
            // LzmaDec.c:775-778
            state = 0;
            len_coder_base = P_LEN_CODER;
            res = Dummy::Match;
        } else {
            // LzmaDec.c:782-822
            res = Dummy::Rep;
            if bit_chk!(P_IS_REP_G0 + state as usize) == 0 {
                if bit_chk!(P_IS_REP0_LONG + combined as usize) == 0 {
                    // LzmaDec.c:791-793 — short rep: no length, no distance.
                    norm_chk!();
                    return Ok((Dummy::Rep, buf));
                }
            } else {
                // LzmaDec.c:800-819 — rep1 / rep2 / rep3 selection. Which one is
                // irrelevant to a dummy pass; only the bit count matters, so the
                // decoded values are dropped.
                if bit_chk!(P_IS_REP_G1 + state as usize) != 0 {
                    let _ = bit_chk!(P_IS_REP_G2 + state as usize);
                }
            }
            // LzmaDec.c:821-822
            state = K_NUM_STATES;
            len_coder_base = P_REP_LEN_CODER;
        }

        // ---- length (LzmaDec.c:824-855) ----
        let mut len: u32;
        if bit_chk!(len_coder_base + LEN_CHOICE) == 0 {
            let pl = len_coder_base + LEN_LOW + pos_state as usize;
            len = 1;
            while len < (1 << K_LEN_NUM_LOW_BITS) {
                tree_bit_chk!(pl, len);
            }
            len -= 1 << K_LEN_NUM_LOW_BITS;
        } else if bit_chk!(len_coder_base + LEN_CHOICE2) == 0 {
            let pl = len_coder_base + LEN_LOW + pos_state as usize + (1 << K_LEN_NUM_LOW_BITS);
            len = 1;
            while len < (1 << K_LEN_NUM_LOW_BITS) {
                tree_bit_chk!(pl, len);
            }
            len -= 1 << K_LEN_NUM_LOW_BITS;
            len += K_LEN_NUM_LOW_SYMBOLS;
        } else {
            let pl = len_coder_base + LEN_HIGH;
            len = 1;
            while len < K_LEN_NUM_HIGH_SYMBOLS {
                tree_bit_chk!(pl, len);
            }
            len -= K_LEN_NUM_HIGH_SYMBOLS;
            len += K_LEN_NUM_LOW_SYMBOLS * 2;
        }

        // ---- distance, only for a non-rep match (LzmaDec.c:857-896) ----
        if state < 4 {
            // `:861` writes `kNumLenToPosStates - 1` where the real decoder
            // (`:466`) writes `kNumLenToPosStates`. The two agree for every `len`:
            // they differ only at len == 3, where both clamp to 3. Ported as
            // written so the diff against upstream stays empty.
            let slot_state = if len < K_NUM_LEN_TO_POS_STATES - 1 {
                len
            } else {
                K_NUM_LEN_TO_POS_STATES - 1
            };
            let ps_base = P_POS_SLOT + ((slot_state as usize) << K_NUM_POS_SLOT_BITS);
            let mut pos_slot: u32 = 1;
            while pos_slot < (1 << K_NUM_POS_SLOT_BITS) {
                tree_bit_chk!(ps_base, pos_slot);
            }
            pos_slot -= 1 << K_NUM_POS_SLOT_BITS;

            if pos_slot >= K_START_POS_MODEL_INDEX {
                let mut num_direct_bits = (pos_slot >> 1) - 1;
                let prob_base;
                if pos_slot < K_END_POS_MODEL_INDEX {
                    // LzmaDec.c:870
                    prob_base = P_SPEC_POS + (((2 | (pos_slot & 1)) as usize) << num_direct_bits);
                } else {
                    // LzmaDec.c:874-884
                    num_direct_bits -= K_NUM_ALIGN_BITS;
                    loop {
                        norm_chk!();
                        range >>= 1;
                        // `code -= range & (((code - range) >> 31) - 1)`
                        let t = (code.wrapping_sub(range) >> 31).wrapping_sub(1);
                        code = code.wrapping_sub(range & t);
                        num_direct_bits -= 1;
                        if num_direct_bits == 0 {
                            break;
                        }
                    }
                    prob_base = P_ALIGN;
                    num_direct_bits = K_NUM_ALIGN_BITS;
                }
                // LzmaDec.c:886-894 — REV_BIT_CHECK (`:83-85`).
                let mut i: u32 = 1;
                let mut m: u32 = 1;
                loop {
                    let b = bit_chk!(prob_base + i as usize);
                    if b == 0 {
                        i += m;
                        m += m;
                    } else {
                        m += m;
                        i += m;
                    }
                    num_direct_bits -= 1;
                    if num_direct_bits == 0 {
                        break;
                    }
                }
            }
        }

        // LzmaDec.c:900-903
        norm_chk!();
        Ok((res, buf))
    }

    // -----------------------------------------------------------------------
    // LzmaDec_DecodeToDic (LzmaDec.c:952-1196)
    // -----------------------------------------------------------------------

    /// `LzmaDec_DecodeToDic` (`LzmaDec.c:952-1196`).
    ///
    /// Decodes into the window up to `dic_limit`, returning the status and the
    /// number of `src` bytes consumed.
    fn decode_to_dic(
        &mut self,
        dic_limit: usize,
        src: &[u8],
        finish_mode: FinishMode,
    ) -> Result<(Status, usize), LzmaDecodeError> {
        let mut in_size = src.len();
        let mut src_off: usize = 0;
        let mut src_len: usize = 0;

        // LzmaDec.c:959-999 — range-coder / state (re)initialization, and the
        // latched-error short circuit.
        if self.remain_len > K_MATCH_SPEC_LEN_START {
            // LzmaDec.c:961-962 — requirement "latched errors": once a data error
            // has been recorded, every re-entry fails the same way instead of
            // decoding from a poisoned state.
            if self.remain_len > K_MATCH_SPEC_LEN_START + 2 {
                return Err(if self.remain_len == K_MATCH_SPEC_LEN_ERROR_FAIL {
                    LzmaDecodeError::Internal
                } else {
                    LzmaDecodeError::DataError
                });
            }

            // LzmaDec.c:964-965
            while in_size > 0 && self.temp_buf_size < RC_INIT_SIZE {
                let b = match src.get(src_off) {
                    Some(v) => *v,
                    None => return Err(LzmaDecodeError::Internal),
                };
                match self.temp_buf.get_mut(self.temp_buf_size) {
                    Some(v) => *v = b,
                    None => return Err(LzmaDecodeError::Internal),
                }
                self.temp_buf_size += 1;
                src_off += 1;
                src_len += 1;
                in_size -= 1;
            }
            // LzmaDec.c:966-967 — the first byte of an LZMA stream is a zero the
            // range coder discards. Anything else is not an LZMA stream.
            if self.temp_buf_size != 0 && self.temp_buf[0] != 0 {
                return Err(LzmaDecodeError::DataError);
            }
            // LzmaDec.c:968-972
            if self.temp_buf_size < RC_INIT_SIZE {
                return Ok((Status::NeedsMoreInput, src_len));
            }
            // LzmaDec.c:973-977
            self.code = u32::from_be_bytes([
                self.temp_buf[1],
                self.temp_buf[2],
                self.temp_buf[3],
                self.temp_buf[4],
            ]);
            // LzmaDec.c:979-982 — kBadRepCode. A `code` this high decodes a
            // rep-match as the first symbol, which would repeat a byte that does
            // not exist.
            if self.check_dic_size == 0 && self.processed_pos == 0 && self.code >= K_BAD_REP_CODE {
                return Err(LzmaDecodeError::DataError);
            }
            // LzmaDec.c:984-985
            self.range = 0xFFFF_FFFF;
            self.temp_buf_size = 0;

            // LzmaDec.c:987-996
            if self.remain_len > K_MATCH_SPEC_LEN_START + 1 {
                let num_probs = self.prop.num_probs();
                if num_probs > self.probs.len() {
                    return Err(LzmaDecodeError::Internal);
                }
                for p in self.probs.iter_mut().take(num_probs) {
                    *p = PROB_INIT_VALUE;
                }
                self.reps = [1, 1, 1, 1];
                self.state = 0;
            }
            self.remain_len = 0;
        }

        // LzmaDec.c:1001 — `for (;;)`. Falling out of it is the C's
        // `SZ_ERROR_FAIL` path at `:1193-1195`.
        loop {
            // LzmaDec.c:1003-1009 — end mark seen; the range coder must be drained.
            if self.remain_len == K_MATCH_SPEC_LEN_START {
                if self.code != 0 {
                    return Err(LzmaDecodeError::DataError);
                }
                return Ok((Status::FinishedWithMark, src_len));
            }

            // LzmaDec.c:1011
            self.write_rem(dic_limit)?;

            let mut check_end_mark_now = false;
            // LzmaDec.c:1018-1035
            if self.dic_pos >= dic_limit {
                if self.remain_len == 0 && self.code == 0 {
                    return Ok((Status::MaybeFinishedWithoutMark, src_len));
                }
                match finish_mode {
                    FinishMode::Any => return Ok((Status::NotFinished, src_len)),
                    FinishMode::End => {}
                }
                if self.remain_len != 0 {
                    // RETURN_NOT_FINISHED_FOR_FINISH (`LzmaDec.c:946-949`), strict
                    // mode: the block was declared finished but is not.
                    return Err(LzmaDecodeError::DataError);
                }
                check_end_mark_now = true;
            }

            if self.temp_buf_size == 0 {
                // ---- decode straight out of `src` (LzmaDec.c:1039-1109) ----
                let buf_limit;
                let mut dummy_processed: Option<usize> = None;

                if in_size < LZMA_REQUIRED_INPUT_MAX || check_end_mark_now {
                    // LzmaDec.c:1046-1048
                    let tail = match src.get(src_off..) {
                        Some(v) => v,
                        None => return Err(LzmaDecodeError::Internal),
                    };
                    let (dummy_res, consumed) = self.try_dummy(tail, in_size)?;

                    match dummy_res {
                        // LzmaDec.c:1050-1061
                        Dummy::InputEof => {
                            if in_size >= LZMA_REQUIRED_INPUT_MAX {
                                break;
                            }
                            src_len += in_size;
                            self.temp_buf_size = in_size;
                            match (
                                self.temp_buf.get_mut(..in_size),
                                src.get(src_off..src_off + in_size),
                            ) {
                                (Some(dst), Some(s)) => dst.copy_from_slice(s),
                                (None, _) | (_, None) => return Err(LzmaDecodeError::Internal),
                            }
                            return Ok((Status::NeedsMoreInput, src_len));
                        }
                        Dummy::Lit | Dummy::Match | Dummy::Rep => {}
                    }

                    // LzmaDec.c:1063-1065
                    if consumed > LZMA_REQUIRED_INPUT_MAX {
                        break;
                    }
                    dummy_processed = Some(consumed);

                    // LzmaDec.c:1067-1076. The C also does `*srcLen +=
                    // dummyProcessed` before returning `SZ_ERROR_DATA`; a caller
                    // there can read the count out of an error. Ours cannot — the
                    // error carries no count — so the update is omitted rather
                    // than written and discarded.
                    if check_end_mark_now && !is_end_marker_possible(dummy_res) {
                        self.temp_buf_size = consumed;
                        match (
                            self.temp_buf.get_mut(..consumed),
                            src.get(src_off..src_off + consumed),
                        ) {
                            (Some(dst), Some(s)) => dst.copy_from_slice(s),
                            (None, _) | (_, None) => return Err(LzmaDecodeError::Internal),
                        }
                        return Err(LzmaDecodeError::DataError);
                    }

                    // LzmaDec.c:1078 — decode exactly one symbol.
                    buf_limit = src_off;
                } else {
                    // LzmaDec.c:1082 — leave a full symbol's worth of slack.
                    buf_limit = src_off + in_size - LZMA_REQUIRED_INPUT_MAX;
                }

                // LzmaDec.c:1084-1108
                self.buf_pos = src_off;
                let res = self.decode_real2(dic_limit, src, buf_limit);
                let processed = self.buf_pos.saturating_sub(src_off);

                match dummy_processed {
                    None => {
                        if processed > in_size {
                            break;
                        }
                    }
                    Some(d) => {
                        if d != processed {
                            break;
                        }
                    }
                }

                src_off += processed;
                in_size -= processed;
                src_len += processed;

                match res {
                    Ok(()) => {}
                    Err(e) => {
                        // LzmaDec.c:1105-1106 — latch.
                        self.remain_len = K_MATCH_SPEC_LEN_ERROR_DATA;
                        return Err(e);
                    }
                }
                continue;
            }

            // ---- a symbol is split across chunks (LzmaDec.c:1112-1189) ----
            let carried = self.temp_buf_size;
            let mut rem = carried;
            let mut ahead: usize = 0;
            let mut dummy_processed: Option<usize> = None;

            // LzmaDec.c:1121-1122
            while rem < LZMA_REQUIRED_INPUT_MAX && ahead < in_size {
                let b = match src.get(src_off + ahead) {
                    Some(v) => *v,
                    None => return Err(LzmaDecodeError::Internal),
                };
                match self.temp_buf.get_mut(rem) {
                    Some(v) => *v = b,
                    None => return Err(LzmaDecodeError::Internal),
                }
                rem += 1;
                ahead += 1;
            }

            // LzmaDec.c:1127-1155
            if rem < LZMA_REQUIRED_INPUT_MAX || check_end_mark_now {
                let tb = self.temp_buf;
                let (dummy_res, consumed) = self.try_dummy(&tb, rem)?;

                match dummy_res {
                    Dummy::InputEof => {
                        // LzmaDec.c:1133-1141
                        if rem >= LZMA_REQUIRED_INPUT_MAX {
                            break;
                        }
                        self.temp_buf_size = rem;
                        src_len += ahead;
                        return Ok((Status::NeedsMoreInput, src_len));
                    }
                    Dummy::Lit | Dummy::Match | Dummy::Rep => {}
                }

                // LzmaDec.c:1143-1146
                if consumed < carried {
                    break;
                }
                dummy_processed = Some(consumed);

                // LzmaDec.c:1148-1154; as above, the C's `*srcLen` update on this
                // error path has no counterpart in a `Result` that carries no count.
                if check_end_mark_now && !is_end_marker_possible(dummy_res) {
                    self.temp_buf_size = consumed;
                    return Err(LzmaDecodeError::DataError);
                }
            }

            // LzmaDec.c:1157-1188. `bufLimit == buf`, so `decode_real`'s loop
            // condition fails immediately and exactly one symbol is decoded.
            self.buf_pos = 0;
            let tb = self.temp_buf;
            let res = self.decode_real2(dic_limit, &tb, 0);
            let processed = self.buf_pos;

            match dummy_processed {
                None => {
                    if processed > LZMA_REQUIRED_INPUT_MAX || processed < carried {
                        break;
                    }
                }
                Some(d) => {
                    if d != processed {
                        break;
                    }
                }
            }

            // LzmaDec.c:1176-1181 — only the bytes beyond the carry came from `src`.
            let from_src = processed - carried;
            if from_src > in_size {
                break;
            }
            src_off += from_src;
            in_size -= from_src;
            src_len += from_src;
            self.temp_buf_size = 0;

            match res {
                Ok(()) => {}
                Err(e) => {
                    self.remain_len = K_MATCH_SPEC_LEN_ERROR_DATA;
                    return Err(e);
                }
            }
        }

        // LzmaDec.c:1193-1195
        self.remain_len = K_MATCH_SPEC_LEN_ERROR_FAIL;
        Err(LzmaDecodeError::Internal)
    }

    // -----------------------------------------------------------------------
    // The streaming driver
    // -----------------------------------------------------------------------

    /// Decode a whole stream from `source` into `sink`, stopping at the
    /// end-of-payload marker.
    ///
    /// Modelled on `C_LZMA.cpp:184-227`: a 64 KiB read buffer, a 64 KiB write
    /// chunk, and nothing but the equivalents of the `"read"` and `"write"`
    /// callback verbs. Unlike the C, this reports why it stopped
    /// ([`DecodeSummary::finish`]) and how much input the marker actually
    /// accounted for ([`DecodeSummary::input_consumed`]) instead of discarding
    /// both.
    ///
    /// Memory is `dicBufSize + 64 KiB + probs`, independent of the payload size.
    pub fn decode(
        &mut self,
        source: &mut dyn InStream,
        sink: &mut dyn OutStream,
    ) -> Result<DecodeSummary, LzmaDecodeError> {
        let mut in_buf: Vec<u8> = Vec::new();
        in_buf
            .try_reserve_exact(IN_BUF)
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;
        in_buf.resize(IN_BUF, 0);

        let mut in_pos: usize = 0;
        let mut in_avail: usize = 0;
        let mut eof = false;

        let mut input_read: u64 = 0;
        let mut input_consumed: u64 = 0;
        let mut output_written: u64 = 0;

        loop {
            // C_LZMA.cpp:186-191 — refill only when the buffer is spent.
            if in_pos == in_avail && !eof {
                let got = source.read(&mut in_buf)?;
                if got > in_buf.len() {
                    // An `InStream` claiming more than it was given.
                    return Err(LzmaDecodeError::Internal);
                }
                in_avail = got;
                in_pos = 0;
                input_read += got as u64;
                if got == 0 {
                    eof = true;
                }
            }

            // LzmaDec.c:1210-1211 — wrap the circular window.
            if self.dic_pos == self.dic_buf_size {
                self.dic_pos = 0;
            }
            let start = self.dic_pos;
            // LzmaDec.c:1213-1222 with an unbounded `outSize`, capped at the C
            // driver's OUT_BUF so a single `write` never exceeds 64 KiB.
            let limit = self.dic_buf_size.min(start + OUT_BUF);

            let chunk = match in_buf.get(in_pos..in_avail) {
                Some(v) => v,
                None => return Err(LzmaDecodeError::Internal),
            };
            // Always LZMA_FINISH_ANY: the payload length is unknown, so no output
            // boundary is ever known to be the last (`LzmaDec.h:91-104`).
            let (status, consumed) = self.decode_to_dic(limit, chunk, FinishMode::Any)?;
            in_pos += consumed;
            input_consumed += consumed as u64;

            // LzmaDec.c:1228-1232 — hand the newly written window slice out.
            let produced = self.dic_pos.saturating_sub(start);
            if produced != 0 {
                let out = match self.dic.get(start..self.dic_pos) {
                    Some(v) => v,
                    None => return Err(LzmaDecodeError::Internal),
                };
                sink.write(out)?;
                output_written += produced as u64;
            }

            match status {
                Status::FinishedWithMark => {
                    return Ok(DecodeSummary {
                        input_read,
                        input_consumed,
                        output_written,
                        finish: Finish::WithMark,
                    });
                }
                Status::MaybeFinishedWithoutMark => {
                    return Ok(DecodeSummary {
                        input_read,
                        input_consumed,
                        output_written,
                        finish: Finish::MaybeWithoutMark,
                    });
                }
                Status::NeedsMoreInput => {
                    // C_LZMA.cpp:217-221. The C calls this BAD_COMPRESSED_DATA;
                    // here it keeps its own name so a caller can tell "the file is
                    // short" from "the file is wrong".
                    if eof {
                        return Err(LzmaDecodeError::TruncatedInput);
                    }
                }
                Status::NotFinished => {}
            }

            // C_LZMA.cpp:222-226 — a stall with input still in hand would spin.
            if produced == 0 && consumed == 0 && in_pos < in_avail {
                return Err(LzmaDecodeError::Internal);
            }
        }
    }
}

/// Decode a raw LZMA1 stream from `source` to `sink`, given the five decoder
/// property bytes.
///
/// The one-call entry point DArc's `lzma_decompress` wants: no `.lzma` header is
/// expected (DArc rebuilds the property bytes from the method string), and the
/// stream is expected to end with an end-of-payload marker.
pub fn decode_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    props: &[u8],
) -> Result<DecodeSummary, LzmaDecodeError> {
    Decoder::new(props)?.decode(source, sink)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stream::{SliceIn, VecOut};
    use crate::{LzmaProps, encode};

    /// Deterministic xorshift bytes, same generator as `roundtrip_tests.rs`.
    fn prng(n: usize, seed: u32) -> Vec<u8> {
        let mut x = seed | 1;
        (0..n)
            .map(|_| {
                x ^= x << 13;
                x ^= x >> 17;
                x ^= x << 5;
                (x >> 24) as u8
            })
            .collect()
    }

    /// Encode `data` with an end-of-payload marker and a small dictionary, and
    /// return `(props, stream)`.
    fn encode_eopm(data: &[u8], dict: u32) -> ([u8; 5], Vec<u8>) {
        let mut props = LzmaProps::for_level(8, dict);
        props.write_end_mark = true;
        let stream = encode(data, &props).expect("encoder");
        (props.decoder_props(), stream)
    }

    fn decode_all(stream: &[u8], props: &[u8; 5]) -> Result<(Vec<u8>, DecodeSummary), LzmaDecodeError> {
        let mut input = SliceIn::new(stream);
        let mut output = VecOut::default();
        let summary = decode_stream(&mut input, &mut output, props)?;
        Ok((output.data, summary))
    }

    // -- round trips ------------------------------------------------------

    #[test]
    fn round_trips_a_corpus() {
        let text = b"the quick brown fox jumps over the lazy dog. ";
        let cases: Vec<(&str, Vec<u8>)> = vec![
            ("empty", Vec::new()),
            ("one", vec![0x42]),
            ("zeros_100", vec![0u8; 100]),
            ("zeros_65k", vec![0u8; 65536]),
            ("ff_1000", vec![0xFFu8; 1000]),
            ("text", text.repeat(64)),
            ("text_big", text.repeat(2000)),
            ("abc", b"abcabcabcabc".repeat(200)),
            ("counter", (0..70000u32).map(|i| i as u8).collect()),
            ("random_4k", prng(4096, 1)),
            ("random_65k", prng(65536, 99)),
        ];
        for (name, data) in cases {
            let (props, stream) = encode_eopm(&data, 65536);
            let (out, summary) = decode_all(&stream, &props).unwrap_or_else(|e| panic!("{name}: {e:?}"));
            assert_eq!(out, data, "{name}: payload mismatch");
            assert_eq!(summary.finish, Finish::WithMark, "{name}");
            assert_eq!(summary.output_written, data.len() as u64, "{name}");
        }
    }

    /// The dictionary is smaller than the payload, so `dic_pos` wraps and
    /// `check_dic_size` is promoted — the path a single linear output buffer never
    /// exercises.
    #[test]
    fn round_trips_through_a_wrapping_window() {
        let data: Vec<u8> = {
            let base = prng(30000, 5);
            let mut v = Vec::new();
            for _ in 0..8 {
                v.extend_from_slice(&base);
            }
            v
        };
        let (props, stream) = encode_eopm(&data, 4096);
        let dec = Decoder::new(&props).expect("props");
        assert!(
            dec.dic_buf_size() < data.len(),
            "window {} should be smaller than the {} byte payload",
            dec.dic_buf_size(),
            data.len()
        );
        let (out, summary) = decode_all(&stream, &props).expect("decode");
        assert_eq!(out, data);
        assert_eq!(summary.finish, Finish::WithMark);
    }

    /// A long run forces a match to be carried across an output-limit boundary
    /// (`LzmaDec_WriteRem`, `LzmaDec.c:616-650`).
    #[test]
    fn carries_a_match_across_the_output_limit() {
        let data = vec![0xA5u8; 1 << 18]; // 256 KiB of one byte -> maximal matches
        let (props, stream) = encode_eopm(&data, 4096);
        let (out, _) = decode_all(&stream, &props).expect("decode");
        assert_eq!(out.len(), data.len());
        assert!(out.iter().all(|b| *b == 0xA5));
    }

    // -- validation -------------------------------------------------------

    /// The headline defect: `props[0] == 255` gives `pb == 5`, a `pb_mask` of 31,
    /// and an out-of-range index into every 16-entry pos-state table.
    /// `LzmaDec.c:1273-1274` rejects it; this must too, and must not panic.
    #[test]
    fn rejects_props_byte_255() {
        let props = [255u8, 0x00, 0x00, 0x01, 0x00];
        assert_eq!(
            LzmaDecProps::parse(&props),
            Err(LzmaDecodeError::UnsupportedProps)
        );
        assert_eq!(
            Decoder::new(&props).err(),
            Some(LzmaDecodeError::UnsupportedProps)
        );
        // And through the front door, with a body that would otherwise decode.
        let mut input = SliceIn::new(&[0u8, 0, 0, 0, 0, 0, 0, 0]);
        let mut output = VecOut::default();
        assert_eq!(
            decode_stream(&mut input, &mut output, &props).err(),
            Some(LzmaDecodeError::UnsupportedProps)
        );
    }

    /// Every props byte at or above 9*5*5 is refused, and every byte below it is
    /// accepted with in-range lc/lp/pb.
    #[test]
    fn props_byte_boundary_is_exact() {
        for d in 0u32..=255 {
            let bytes = [d as u8, 0x00, 0x00, 0x01, 0x00];
            let parsed = LzmaDecProps::parse(&bytes);
            if d >= 9 * 5 * 5 {
                assert_eq!(parsed, Err(LzmaDecodeError::UnsupportedProps), "d={d}");
            } else {
                let p = parsed.unwrap_or_else(|e| panic!("d={d}: {e:?}"));
                assert!(p.lc <= 8 && p.lp <= 4 && p.pb <= 4, "d={d}: {p:?}");
                assert_eq!((p.pb * 5 + p.lp) * 9 + p.lc, d, "d={d}");
            }
        }
    }

    #[test]
    fn rejects_short_props() {
        for n in 0..LZMA_PROPS_SIZE {
            assert_eq!(
                LzmaDecProps::parse(&vec![0u8; n]),
                Err(LzmaDecodeError::UnsupportedProps),
                "len {n}"
            );
        }
    }

    /// `LzmaDec.c:1268-1269`: a tiny declared dictionary is clamped to
    /// `LZMA_DIC_MIN`, so the window is never degenerate.
    #[test]
    fn clamps_the_dictionary_to_the_minimum() {
        let props = [0x5Du8, 0x01, 0x00, 0x00, 0x00]; // dicSize = 1
        let p = LzmaDecProps::parse(&props).expect("props");
        assert_eq!(p.dic_size, LZMA_DIC_MIN);
        let d = Decoder::new(&props).expect("decoder");
        assert!(d.dic_buf_size() >= LZMA_DIC_MIN as usize);
    }

    /// `LzmaDec.c:979-982` (`kBadRepCode`): a rep-match cannot be the first symbol,
    /// because there is nothing behind position 0 to repeat.
    #[test]
    fn rejects_a_rep_match_as_the_first_symbol() {
        let props = [0x5Du8, 0x00, 0x00, 0x01, 0x00];
        // code == 0xC0000000, which is >= kBadRepCode (0xBFFFFC00).
        let stream = [0x00u8, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(
            decode_all(&stream, &props).err(),
            Some(LzmaDecodeError::DataError)
        );

        // The boundary itself, isolated. Feeding exactly RC_INIT_SIZE bytes stops
        // right after initialization, so the only check that can have fired is the
        // one at `LzmaDec.c:979-982`: at kBadRepCode it must reject, one below it
        // must ask for more input.
        for (code, want_reject) in [
            (K_BAD_REP_CODE - 1, false),
            (K_BAD_REP_CODE, true),
            (u32::MAX, true),
        ] {
            let mut init = vec![0u8];
            init.extend_from_slice(&code.to_be_bytes());
            let mut dec = Decoder::new(&props).expect("decoder");
            let got = dec.decode_to_dic(4096, &init, FinishMode::Any);
            if want_reject {
                assert_eq!(
                    got.err(),
                    Some(LzmaDecodeError::DataError),
                    "code {code:#010x} should be rejected"
                );
            } else {
                assert_eq!(
                    got.ok().map(|(s, _)| s),
                    Some(Status::NeedsMoreInput),
                    "code {code:#010x} should be accepted at init"
                );
            }
        }
    }

    /// `LzmaDec.c:966-967`: the first byte of the range coder is a discarded zero.
    #[test]
    fn rejects_a_nonzero_first_byte() {
        let props = [0x5Du8, 0x00, 0x00, 0x01, 0x00];
        let stream = [0x01u8, 0x00, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(
            decode_all(&stream, &props).err(),
            Some(LzmaDecodeError::DataError)
        );
    }

    /// A stream cut short reports truncation, not a plausible prefix of garbage.
    #[test]
    fn reports_truncation_rather_than_inventing_bytes() {
        let data = prng(20000, 42);
        let (props, stream) = encode_eopm(&data, 65536);
        assert!(stream.len() > 64);

        for cut in [1usize, 2, 3, 4, 5, 16, stream.len() / 4, stream.len() / 2] {
            let truncated = &stream[..cut];
            let got = decode_all(truncated, &props);
            match got {
                Err(LzmaDecodeError::TruncatedInput) => {}
                Err(e) => panic!("cut {cut}: expected TruncatedInput, got {e:?}"),
                Ok((out, s)) => panic!("cut {cut}: decoded {} bytes, {s:?}", out.len()),
            }
        }

        // Dropping only the tail of the end marker is the subtle case: everything
        // before it decodes fine, so a decoder that fabricates input would return
        // the full payload and call it a success.
        for drop in 1usize..=6 {
            let truncated = &stream[..stream.len() - drop];
            assert_eq!(
                decode_all(truncated, &props).err(),
                Some(LzmaDecodeError::TruncatedInput),
                "dropped {drop} tail bytes"
            );
        }
    }

    /// An empty input is truncation, not an empty payload.
    #[test]
    fn empty_input_is_truncation() {
        let props = [0x5Du8, 0x00, 0x00, 0x01, 0x00];
        assert_eq!(
            decode_all(&[], &props).err(),
            Some(LzmaDecodeError::TruncatedInput)
        );
    }

    /// Corrupting the body must not panic, whatever it produces — and the
    /// corruption must actually *reach* the decode path, or this test proves
    /// nothing. Hence the tallies: a run in which nothing was ever rejected would
    /// mean the mutations never landed anywhere load-bearing.
    #[test]
    fn corrupted_bodies_never_panic() {
        let data = prng(8000, 7);
        let (props, stream) = encode_eopm(&data, 65536);

        let mut ok = 0u32;
        let mut data_errors = 0u32;
        let mut truncations = 0u32;
        let mut internals = 0u32;

        let mut tally = |r: Result<(Vec<u8>, DecodeSummary), LzmaDecodeError>| match r {
            Ok(_) => ok += 1,
            Err(LzmaDecodeError::DataError) => data_errors += 1,
            Err(LzmaDecodeError::TruncatedInput) => truncations += 1,
            Err(LzmaDecodeError::Internal) => internals += 1,
            Err(LzmaDecodeError::UnsupportedProps) => panic!("props are fixed and valid"),
            Err(LzmaDecodeError::NotEnoughMemory) => panic!("4 KiB window"),
            Err(LzmaDecodeError::Stream(e)) => panic!("in-memory streams cannot fail: {e:?}"),
        };

        // Single-byte flips across the whole stream.
        for seed in 0..256u32 {
            let mut bad = stream.clone();
            let i = (seed as usize * 7919) % bad.len();
            bad[i] ^= 0xFF;
            tally(decode_all(&bad, &props));
        }
        // Pure noise behind a valid leading zero byte, so the range-coder init
        // passes and every failure comes from the symbol decoder proper.
        for seed in 0..256u32 {
            let mut noise = vec![0u8];
            noise.extend_from_slice(&prng(2000, seed | 1));
            tally(decode_all(&noise, &props));
        }

        assert_eq!(internals, 0, "an invariant of the port was violated");
        assert!(
            data_errors > 0,
            "no corruption was ever rejected: ok={ok} trunc={truncations}"
        );
        assert!(
            truncations > 0,
            "no corruption ever ran out of input: ok={ok} data={data_errors}"
        );
    }

    // -- the end marker ---------------------------------------------------

    /// Trailing bytes after the end marker are not consumed, and the reported
    /// consumption is exactly the compressed length — which is what lets a caller
    /// find the next stream in a concatenation.
    #[test]
    fn stops_at_the_marker_and_reports_the_consumed_length() {
        let data = prng(12000, 11);
        let (props, stream) = encode_eopm(&data, 65536);

        let (out, clean) = decode_all(&stream, &props).expect("clean decode");
        assert_eq!(out, data);
        assert_eq!(clean.input_consumed, stream.len() as u64);
        assert_eq!(clean.finish, Finish::WithMark);

        for trailing in [1usize, 5, 19, 20, 21, 100, 5000] {
            let mut padded = stream.clone();
            padded.extend_from_slice(&prng(trailing, 3));
            let (out, s) = decode_all(&padded, &props).expect("padded decode");
            assert_eq!(out, data, "trailing {trailing}");
            assert_eq!(s.finish, Finish::WithMark, "trailing {trailing}");
            assert_eq!(
                s.input_consumed,
                stream.len() as u64,
                "trailing {trailing}: consumed past the marker"
            );
            assert!(
                s.input_read >= s.input_consumed,
                "trailing {trailing}: read < consumed"
            );
        }
    }

    /// `FinishMode::End` makes the decoder look one symbol ahead at the output
    /// limit to confirm the end marker (`LzmaDec.c:1016-1034`, `:1044-1080`) — the
    /// `checkEndMarkNow` path, which `FinishMode::Any` never takes.
    #[test]
    fn finish_end_checks_for_the_marker_at_the_limit() {
        let data = b"the quick brown fox. ".repeat(50);
        let (props, stream) = encode_eopm(&data, 65536);

        // Limit == the true payload length: the very next symbol is the marker.
        let mut dec = Decoder::new(&props).expect("decoder");
        assert_eq!(dec.props().lc, 3);
        let (status, consumed) = dec
            .decode_to_dic(data.len(), &stream, FinishMode::End)
            .expect("finish end");
        assert_eq!(status, Status::FinishedWithMark);
        assert_eq!(consumed, stream.len());
        assert_eq!(&dec.dic[..data.len()], &data[..]);

        // Limit short of the end: FINISH_END asserts the block ends there, and it
        // does not, so strict mode rejects (`LzmaDec.c:946-949`).
        let mut dec = Decoder::new(&props).expect("decoder");
        assert_eq!(
            dec.decode_to_dic(data.len() / 2, &stream, FinishMode::End)
                .err(),
            Some(LzmaDecodeError::DataError)
        );
    }

    // -- stream plumbing --------------------------------------------------

    /// An `InStream` that hands out at most `n` bytes per call, to force the
    /// carry-buffer path (`LzmaDec.c:1112-1189`) on nearly every symbol.
    struct Dribble<'a> {
        data: &'a [u8],
        pos: usize,
        n: usize,
    }

    impl InStream for Dribble<'_> {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
            let take = buf.len().min(self.n).min(self.data.len() - self.pos);
            buf[..take].copy_from_slice(&self.data[self.pos..self.pos + take]);
            self.pos += take;
            Ok(take)
        }
    }

    #[test]
    fn decodes_through_a_one_byte_at_a_time_reader() {
        let data = b"the quick brown fox jumps over the lazy dog. ".repeat(120);
        let (props, stream) = encode_eopm(&data, 65536);
        for n in [1usize, 2, 3, 19, 20, 21] {
            let mut input = Dribble {
                data: &stream,
                pos: 0,
                n,
            };
            let mut output = VecOut::default();
            let s = decode_stream(&mut input, &mut output, &props)
                .unwrap_or_else(|e| panic!("n={n}: {e:?}"));
            assert_eq!(output.data, data, "n={n}");
            assert_eq!(s.finish, Finish::WithMark, "n={n}");
            assert_eq!(s.input_consumed, stream.len() as u64, "n={n}");
        }
    }

    /// An `OutStream` error is returned with the caller's own code intact.
    #[test]
    fn propagates_the_sinks_error_code() {
        struct Failing;
        impl OutStream for Failing {
            fn write(&mut self, _: &[u8]) -> Result<(), StreamError> {
                Err(StreamError(-1234))
            }
        }
        let data = prng(9000, 3);
        let (props, stream) = encode_eopm(&data, 65536);
        let mut input = SliceIn::new(&stream);
        assert_eq!(
            decode_stream(&mut input, &mut Failing, &props).err(),
            Some(LzmaDecodeError::Stream(StreamError(-1234)))
        );
    }

    /// An `InStream` error likewise.
    #[test]
    fn propagates_the_sources_error_code() {
        struct Failing;
        impl InStream for Failing {
            fn read(&mut self, _: &mut [u8]) -> Result<usize, StreamError> {
                Err(StreamError(-77))
            }
        }
        let props = [0x5Du8, 0x00, 0x00, 0x01, 0x00];
        let mut output = VecOut::default();
        assert_eq!(
            decode_stream(&mut Failing, &mut output, &props).err(),
            Some(LzmaDecodeError::Stream(StreamError(-77)))
        );
    }

    // -- latching ---------------------------------------------------------

    /// `LzmaDec.c:959-962`: after a data error, re-entry fails the same way
    /// instead of decoding onward from a poisoned state.
    #[test]
    fn a_data_error_latches() {
        let props = [0x5Du8, 0x00, 0x00, 0x01, 0x00];
        let mut dec = Decoder::new(&props).expect("decoder");
        // kBadRepCode start.
        let bad = [0x00u8, 0xC0, 0x00, 0x00, 0x00];
        assert_eq!(
            dec.decode_to_dic(4096, &bad, FinishMode::Any).err(),
            Some(LzmaDecodeError::DataError)
        );

        // Force the latch the way `decode_to_dic` does on a decode failure, then
        // check that a subsequent well-formed call short-circuits.
        dec.remain_len = K_MATCH_SPEC_LEN_ERROR_DATA;
        let (_, stream) = encode_eopm(b"hello hello hello", 4096);
        assert_eq!(
            dec.decode_to_dic(4096, &stream, FinishMode::Any).err(),
            Some(LzmaDecodeError::DataError)
        );
        // And the FAIL sentinel maps to Internal, not DataError.
        dec.remain_len = K_MATCH_SPEC_LEN_ERROR_FAIL;
        assert_eq!(
            dec.decode_to_dic(4096, &stream, FinishMode::Any).err(),
            Some(LzmaDecodeError::Internal)
        );
    }

    /// `reset` (`LzmaDec_Init`) clears the latch and the window position.
    #[test]
    fn reset_clears_the_latch() {
        let data = b"abcabcabcabcabcabc".repeat(50);
        let (props, stream) = encode_eopm(&data, 4096);
        let mut dec = Decoder::new(&props).expect("decoder");
        dec.remain_len = K_MATCH_SPEC_LEN_ERROR_DATA;
        dec.reset();
        let mut input = SliceIn::new(&stream);
        let mut output = VecOut::default();
        dec.decode(&mut input, &mut output).expect("after reset");
        assert_eq!(output.data, data);
    }

    // -- layout -----------------------------------------------------------

    /// The flat probability offsets must match the C's, or every table aliases.
    #[test]
    fn probability_layout_matches_the_c() {
        assert_eq!(P_SPEC_POS, 0);
        assert_eq!(P_IS_REP0_LONG, 128);
        assert_eq!(P_REP_LEN_CODER, 384);
        assert_eq!(P_LEN_CODER, 896);
        assert_eq!(P_IS_MATCH, 1408);
        assert_eq!(P_ALIGN, 1664);
        assert_eq!(P_IS_REP, 1680);
        assert_eq!(P_IS_REP_G0, 1692);
        assert_eq!(P_IS_REP_G1, 1704);
        assert_eq!(P_IS_REP_G2, 1716);
        assert_eq!(P_POS_SLOT, 1728);
        assert_eq!(P_LITERAL, 1984);
        assert_eq!(NUM_BASE_PROBS, 1984);
        assert_eq!(K_NUM_LEN_PROBS, 512);
        assert_eq!(LEN_CHOICE, 0);
        assert_eq!(LEN_CHOICE2, 8);
        assert_eq!(LEN_HIGH, 256);

        // The literal table is sized exactly for the widest index the context
        // formula at `LzmaDec.c:269` can produce.
        for lc in 0u32..=8 {
            for lp in 0u32..=4 {
                let p = LzmaDecProps {
                    lc,
                    lp,
                    pb: 0,
                    dic_size: LZMA_DIC_MIN,
                };
                let lp_mask = (0x100u32 << lp) - (0x100u32 >> lc);
                let widest = NUM_BASE_PROBS + (3 * (lp_mask << lc)) as usize + 0x2FF;
                assert!(
                    widest < p.num_probs(),
                    "lc={lc} lp={lp}: widest {widest} >= {}",
                    p.num_probs()
                );
            }
        }
    }

    /// `LzmaDec.c:1315-1323`: the window rounding, including the two thresholds.
    #[test]
    fn window_rounding_matches_the_c() {
        let cases: [(u32, u64); 6] = [
            (1 << 12, 1 << 12),
            (4097, 8192),
            ((1 << 22) + 1, (1 << 22) + (1 << 20)),
            (1 << 22, 1 << 22),
            ((1 << 30) + 1, (1 << 30) + (1 << 22)),
            (0xFFFF_FFFF, 0x1_0000_0000),
        ];
        for (dic_size, want) in cases {
            let p = LzmaDecProps {
                lc: 3,
                lp: 0,
                pb: 2,
                dic_size,
            };
            assert_eq!(p.dic_buf_size(), want, "dic_size {dic_size}");
        }
    }

    #[test]
    fn back_pos_wraps_and_refuses_over_long_distances() {
        assert_eq!(back_pos(10, 3, 100), Some(7));
        assert_eq!(back_pos(0, 1, 100), Some(99));
        assert_eq!(back_pos(2, 100, 100), Some(2));
        assert_eq!(back_pos(2, 101, 100), None);
    }
}
