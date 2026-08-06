//! Hardened, streaming LZMA2 decoder — a port of
//! `Compression/LZMA/7z24/Lzma2Dec.c`.
//!
//! LZMA2 is a **framing layer over LZMA1**: the payload is a sequence of chunks,
//! each carrying its own length pair and, optionally, its own `lc/lp/pb` and its
//! own instruction to reset the dictionary and/or the probability model. The
//! actual symbol decoding is [`crate::decode_stream`]'s, unchanged — this module
//! only parses chunk headers, dispatches the resets, and copies uncompressed
//! chunks straight into the dictionary.
//!
//! # Why this exists
//!
//! `lzma2` is registered as a DArc method but no preset selects it: it is reachable
//! only if a user types `-mlzma2`. That makes the *encoder* optional and the
//! decoder mandatory — DArc's standing rule is that a decoder must read every
//! archive that was ever written. That rule used to be enforced by
//! `Unarc/makefile`, which linked `C_LZMA2.o` into every SFX target; Unarc/ is
//! gone and `rust/darc-unarc` links this crate directly instead. The exposure
//! is unchanged and so is the discipline: `unarc` IS every SFX module, it is
//! prepended to archives and parses hostile input standalone, so **a panic in
//! this file is a vulnerability, not a bug.**
//!
//! Every length, control byte and property byte in an LZMA2 stream comes from the
//! archive. Accordingly:
//!
//! * no stream-derived value indexes a slice without a bounds check;
//! * no stream-derived value is subtracted without `checked_sub` or a proof-carrying
//!   `wrapping_sub` that reproduces the C's `UInt32` arithmetic;
//! * the only allocations are the dictionary (sized from the one prop byte, through
//!   [`crate::decode_stream::Decoder`]'s `try_reserve_exact`) and the two 64 KiB
//!   driver buffers, also `try_reserve`d;
//! * every entry point returns [`Result`]; there is no `unwrap`, `expect`, `panic!`
//!   or `if let` outside `#[cfg(test)]`.
//!
//! # What the consumer actually uses
//!
//! `Compression/LZMA/C_LZMA2.cpp:112-182` (`lzma2_decompress`) is the only consumer,
//! and it touches five entry points: `Lzma2Dec_Construct`, `Lzma2Dec_Allocate`,
//! `Lzma2Dec_Init`, `Lzma2Dec_DecodeToBuf` and `Lzma2Dec_Free`, with 64 KiB
//! buffers, `LZMA_FINISH_ANY`, and a stop on `LZMA_STATUS_FINISHED_WITH_MARK`.
//! [`decode_lzma2_stream`] is that loop; [`Lzma2Dec`] is the state it drives.
//!
//! # Deliberately not ported
//!
//! * `Lzma2Dec_Parse` (`Lzma2Dec.c:309-425`) and `Lzma2Dec_GetUnpackExtra`
//!   (`Lzma2Dec.h:94`) — the multi-threaded block scanner. Nothing in DArc calls
//!   them; they exist for 7-Zip's `Lzma2Dec_Mt`, which is not vendored. The
//!   `isExtraMode` field (`Lzma2Dec.h:18`, written at `Lzma2Dec.c:102` and `:382`)
//!   is read *only* by those two, so it is not modelled either.
//! * `Lzma2Decode` (`Lzma2Dec.c:471-491`) — the one-call, whole-buffer interface.
//!   `C_LZMA2.cpp` uses the streaming one, and a one-call variant would have to hold
//!   the entire payload in RAM, which is exactly what this crate's decoder exists to
//!   avoid.
//! * `Lzma2Dec_AllocateProbs` (`Lzma2Dec.c:71-76`) — probabilities without a
//!   dictionary, for the parse-only path above.

use crate::decode_stream::{
    Decoder, FinishMode, K_MATCH_SPEC_LEN_START, LZMA_PROPS_SIZE, LzmaDecodeError, Status,
};
use crate::stream::{InStream, OutStream};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// `LZMA2_CONTROL_COPY_RESET_DIC` (`Lzma2Dec.c:30`): the uncompressed-chunk control
/// byte that also resets the dictionary.
const CONTROL_COPY_RESET_DIC: u8 = 1;

/// `LZMA2_LCLP_MAX` (`Lzma2Dec.c:34`).
///
/// Two distinct jobs, both load-bearing: it is the cap on `lc + lp` that every
/// per-chunk property byte is checked against (`Lzma2Dec.c:154-155`), *and* it is
/// the `lc` written into the synthesized LZMA property byte at allocation time
/// (`Lzma2Dec.c:63`). See [`old_props`] for why the maximum is the right choice
/// there.
const LZMA2_LCLP_MAX: u32 = 4;

/// The 64 KiB buffers `C_LZMA2.cpp:127-128` drives the C decoder with. Kept
/// identical so the number of `"read"` / `"write"` callbacks per block matches.
const IN_BUF: usize = 1 << 16;
const OUT_BUF: usize = 1 << 16;

/// `LZMA2_IS_UNCOMPRESSED_STATE` (`Lzma2Dec.c:32`).
///
/// Bit 7 of the control byte is the whole discriminator: clear means "copy these
/// bytes", set means "run the LZMA decoder over them".
#[inline]
fn is_uncompressed(control: u8) -> bool {
    control & (1 << 7) == 0
}

// ---------------------------------------------------------------------------
// Lzma2Dec_GetOldProps (Lzma2Dec.c:57-68)
// ---------------------------------------------------------------------------

/// `Lzma2Dec_GetOldProps` (`Lzma2Dec.c:57-68`): turn LZMA2's single property byte
/// into the five LZMA1 property bytes the underlying decoder is allocated from.
///
/// Two things here look wrong and are not.
///
/// **The dictionary formula.** `LZMA2_DIC_SIZE_FROM_PROP` (`Lzma2Dec.c:35`) is
/// `(2 | (prop & 1)) << (prop / 2 + 11)`, i.e. a 1.5-bit mantissa on a power of
/// two: even `prop` gives `2 << (prop/2 + 11)`, odd gives `3 << (prop/2 + 11)`.
/// `prop == 40` is special-cased to `0xFFFFFFFF` (`Lzma2Dec.c:62`) because the
/// formula would overflow there, and `prop > 40` is rejected outright
/// (`Lzma2Dec.c:60-61`). For `prop <= 39` the shift amount is at most
/// `39 / 2 + 11 == 30` and the result at most `3 << 30`, so neither the shift count
/// nor the product leaves `u32`.
///
/// **`props[0] = LZMA2_LCLP_MAX`** (`Lzma2Dec.c:63`) — a synthesized `lc` of 4,
/// `lp` of 0, `pb` of 0, which is emphatically *not* the stream's real
/// configuration. It is deliberate: the probability array is sized once, at
/// allocation, from these bytes, but `lc`/`lp` are then overwritten per chunk by
/// each chunk's own property byte (`Lzma2Dec.c:145-158`). Sizing it for the
/// *maximum* `lc + lp` LZMA2 permits — and `Lzma2Dec.c:154-155` enforces
/// `lc + lp <= LZMA2_LCLP_MAX` on every chunk — is what guarantees the array is
/// large enough for whatever any later chunk asks for. Do not "fix" this to the
/// first chunk's real `lc`.
pub fn old_props(prop: u8) -> Result<[u8; LZMA_PROPS_SIZE], LzmaDecodeError> {
    // Lzma2Dec.c:60-61
    if prop > 40 {
        return Err(LzmaDecodeError::UnsupportedProps);
    }
    // Lzma2Dec.c:62
    let dic_size: u32 = if prop == 40 {
        0xFFFF_FFFF
    } else {
        (2u32 | u32::from(prop & 1)) << (u32::from(prop) / 2 + 11)
    };

    let mut props = [0u8; LZMA_PROPS_SIZE];
    // Lzma2Dec.c:63-67
    props[0] = LZMA2_LCLP_MAX as u8;
    props[1] = dic_size as u8;
    props[2] = (dic_size >> 8) as u8;
    props[3] = (dic_size >> 16) as u8;
    props[4] = (dic_size >> 24) as u8;
    Ok(props)
}

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

/// `ELzma2State` (`Lzma2Dec.c:43-55`).
///
/// The header walk is `Control -> Unpack0 -> Unpack1 -> [Pack0 -> Pack1 -> [Prop]]
/// -> Data -> DataCont`: the pack-size pair exists only for LZMA chunks
/// (`Lzma2Dec.c:132`) and the property byte only when `control & 0x40` is set
/// (`Lzma2Dec.c:143`). `Finished` and `Error` are terminal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lzma2State {
    /// `LZMA2_STATE_CONTROL`.
    Control,
    /// `LZMA2_STATE_UNPACK0` — bits 15..8 of `unpackSize - 1`.
    Unpack0,
    /// `LZMA2_STATE_UNPACK1` — bits 7..0 of `unpackSize - 1`.
    Unpack1,
    /// `LZMA2_STATE_PACK0` — bits 15..8 of `packSize - 1`.
    Pack0,
    /// `LZMA2_STATE_PACK1` — bits 7..0 of `packSize - 1`.
    Pack1,
    /// `LZMA2_STATE_PROP` — the chunk's `lc`/`lp`/`pb`.
    Prop,
    /// `LZMA2_STATE_DATA` — chunk payload, first entry (resets are dispatched here).
    Data,
    /// `LZMA2_STATE_DATA_CONT` — chunk payload, resumed.
    DataCont,
    /// `LZMA2_STATE_FINISHED` — the `0x00` end-of-stream control byte was read.
    Finished,
    /// `LZMA2_STATE_ERROR` — terminal; every later call fails the same way.
    Error,
}

/// What a call into the decoder ended on.
///
/// The three `ELzmaStatus` values `Lzma2Dec_DecodeToDic` can report
/// (`Lzma2Dec.h:41-46`). `LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK` never escapes
/// the LZMA2 layer — it is consumed internally at `Lzma2Dec.c:289` as the signal
/// that a chunk's LZMA stream ended exactly on its declared `unpackSize` — and
/// `LZMA_STATUS_NOT_SPECIFIED` is the C's "data error" sentinel, which is a
/// [`LzmaDecodeError`] here instead of a status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lzma2Status {
    /// `LZMA_STATUS_FINISHED_WITH_MARK`: the `0x00` end-of-stream control byte was
    /// read. This is what `C_LZMA2.cpp:167` stops on.
    FinishedWithMark,
    /// `LZMA_STATUS_NOT_FINISHED`: the output limit was reached mid-stream.
    NotFinished,
    /// `LZMA_STATUS_NEEDS_MORE_INPUT`.
    NeedsMoreInput,
}

/// What one [`Lzma2Dec::decode_to_buf`] call consumed and produced.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lzma2Progress {
    /// Why the call stopped.
    pub status: Lzma2Status,
    /// Bytes taken from `src`.
    pub src_consumed: usize,
    /// Bytes written into `dest`.
    pub out_written: usize,
}

/// What a whole-stream decode consumed and produced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Lzma2DecodeSummary {
    /// Bytes pulled from the [`InStream`], including the leading property byte.
    /// At least [`Self::input_consumed`]; the excess is whatever sat in the 64 KiB
    /// read buffer past the end-of-stream control byte.
    pub input_read: u64,
    /// Bytes the decoder actually consumed, including the leading property byte.
    pub input_consumed: u64,
    /// Bytes written to the [`OutStream`].
    pub output_written: u64,
}

// ---------------------------------------------------------------------------
// LzmaDec helpers that LZMA2 reaches into (LzmaDec.c)
// ---------------------------------------------------------------------------

/// `LzmaDec_InitDicAndState` (`LzmaDec.c:907-920`), declared for LZMA2's benefit at
/// `Lzma2Dec.c:175` and called from `:239` and `:262`.
///
/// The interesting part is that it does **not** clear anything except the carry
/// buffer: it encodes the pending initialization in `remainLen`, which
/// `LzmaDec_DecodeToDic` (`LzmaDec.c:959-999`) then acts on at the top of its next
/// call.
///
/// * `kMatchSpecLenStart + 1` — re-initialize the range coder, keep the
///   probabilities, the reps and the state (`LzmaDec.c:987` tests `> +1`).
/// * `kMatchSpecLenStart + 2` — also reset probabilities, reps and state.
///
/// So `init_state` and `init_dic` both land on `+2`; `init_dic` additionally zeroes
/// `processedPos`/`checkDicSize`, which is what makes the *next* chunk behave as if
/// it were at the start of a stream (no literal context from the previous byte, and
/// a distance horizon of zero). Note it deliberately leaves `dicPos` alone: the
/// window keeps its bytes, only the accounting restarts.
fn init_dic_and_state(dec: &mut Decoder, init_dic: bool, init_state: bool) {
    // LzmaDec.c:909-910
    dec.remain_len = K_MATCH_SPEC_LEN_START + 1;
    dec.temp_buf_size = 0;

    // LzmaDec.c:912-917
    if init_dic {
        dec.processed_pos = 0;
        dec.check_dic_size = 0;
        dec.remain_len = K_MATCH_SPEC_LEN_START + 2;
    }
    // LzmaDec.c:918-919
    if init_state {
        dec.remain_len = K_MATCH_SPEC_LEN_START + 2;
    }
}

/// `LzmaDec_UpdateWithUncompressed` (`Lzma2Dec.c:166-173`): an uncompressed chunk's
/// bytes go straight into the dictionary window, and the decoder's accounting has to
/// move exactly as if the LZMA decoder had produced them — otherwise a *later*
/// chunk's match distances are validated against the wrong horizon.
///
/// The C's `memcpy` has no bound: the caller is expected to have clamped `size` to
/// the window's remaining room (`Lzma2Dec.c:242-243`). Here the slice is taken
/// through `get_mut` so a broken clamp is an [`LzmaDecodeError::Internal`] rather
/// than a heap overflow.
fn update_with_uncompressed(dec: &mut Decoder, src: &[u8]) -> Result<(), LzmaDecodeError> {
    let size = src.len();
    let size32 = u32::try_from(size).map_err(|_| LzmaDecodeError::Internal)?;

    // Lzma2Dec.c:168-169
    let end = dec
        .dic_pos
        .checked_add(size)
        .ok_or(LzmaDecodeError::Internal)?;
    let dst = dec
        .dic
        .get_mut(dec.dic_pos..end)
        .ok_or(LzmaDecodeError::Internal)?;
    dst.copy_from_slice(src);
    dec.dic_pos = end;

    // Lzma2Dec.c:170-171 — latch `checkDicSize` the moment the window first fills.
    // `wrapping_sub` reproduces the C's `UInt32` subtraction exactly. It cannot
    // actually wrap: `checkDicSize == 0` holds only while `processedPos < dicSize`
    // (`LzmaDec.c:676-683`), and wrapping here would read as "not yet full", which
    // is also the safe direction.
    if dec.check_dic_size == 0
        && u64::from(dec.prop.dic_size.wrapping_sub(dec.processed_pos)) <= size as u64
    {
        dec.check_dic_size = dec.prop.dic_size;
    }

    // Lzma2Dec.c:172
    dec.processed_pos = dec.processed_pos.wrapping_add(size32);
    Ok(())
}

// ---------------------------------------------------------------------------
// The decoder
// ---------------------------------------------------------------------------

/// `CLzma2Dec` (`Lzma2Dec.h:13-23`) — the chunk state machine wrapped around a
/// [`Decoder`].
pub struct Lzma2Dec {
    /// `p->state`.
    state: Lzma2State,
    /// `p->control` — the chunk's control byte, kept for the whole chunk because the
    /// uncompressed/LZMA discriminator and the reset flags are re-read from it in
    /// `Data`/`DataCont`.
    control: u8,
    /// `p->needInitLevel` (`Lzma2Dec.h:17`), initialized to `0xE0` at
    /// `Lzma2Dec.c:88`. The floor a control byte must reach before the stream is
    /// allowed to carry data; see [`Self::update_state`].
    need_init_level: u8,
    /// `p->packSize` — compressed bytes left in the current LZMA chunk.
    pack_size: u32,
    /// `p->unpackSize` — output bytes left in the current chunk.
    unpack_size: u32,
    /// `p->decoder`.
    decoder: Decoder,
}

impl Lzma2Dec {
    /// `Lzma2Dec_Construct` + `Lzma2Dec_Allocate` + `Lzma2Dec_Init`
    /// (`Lzma2Dec.h:26`, `Lzma2Dec.c:78-83`, `:85-94`) — the three calls
    /// `C_LZMA2.cpp:120-125` makes in a row.
    ///
    /// `prop_byte` is the single byte DArc writes ahead of the LZMA2 stream
    /// (`C_LZMA2.cpp:96-98` on the encode side, `:114-117` on the decode side).
    /// Values above 40 are rejected here, before anything is allocated
    /// (`Lzma2Dec.c:60-61`).
    pub fn new(prop_byte: u8) -> Result<Self, LzmaDecodeError> {
        let props = old_props(prop_byte)?;
        // `Decoder::new` is `LzmaProps_Decode` + `LzmaDec_Allocate` + `LzmaDec_Init`
        // and allocates through `try_reserve_exact`, so a `prop_byte` of 40 asking
        // for a 4 GiB window is `NotEnoughMemory`, never an abort.
        let decoder = Decoder::new(&props)?;
        let mut p = Lzma2Dec {
            state: Lzma2State::Control,
            control: 0,
            need_init_level: 0xE0,
            pack_size: 0,
            unpack_size: 0,
            decoder,
        };
        p.init();
        Ok(p)
    }

    /// `Lzma2Dec_Init` (`Lzma2Dec.c:85-94`) — start a new stream on the same
    /// allocation.
    pub fn init(&mut self) {
        self.state = Lzma2State::Control; // :87
        self.need_init_level = 0xE0; // :88
        // `p->isExtraMode = False` (:89) has no counterpart; see the module docs.
        self.unpack_size = 0; // :90
        self.decoder.reset(); // :93 — LzmaDec_Init
    }

    /// The allocated dictionary window, for callers that want to report it.
    pub fn dic_buf_size(&self) -> usize {
        self.decoder.dic_buf_size()
    }

    // -----------------------------------------------------------------------
    // Lzma2Dec_UpdateState (Lzma2Dec.c:97-163)
    // -----------------------------------------------------------------------

    /// `Lzma2Dec_UpdateState` (`Lzma2Dec.c:97-163`): fold one header byte into the
    /// state machine and return the next state.
    ///
    /// The chunk layout it walks (`Lzma2Dec.c:16-28`):
    ///
    /// ```text
    /// 00000000                -  end of stream
    /// 00000001 U U            -  uncompressed, reset dic
    /// 00000010 U U            -  uncompressed, no reset
    /// 100uuuuu U U P P        -  LZMA, no reset
    /// 101uuuuu U U P P        -  LZMA, reset state
    /// 110uuuuu U U P P S      -  LZMA, reset state + new props
    /// 111uuuuu U U P P S      -  LZMA, reset state + new props, reset dic
    /// ```
    ///
    /// **Both sizes are stored minus one** and are incremented on the low byte
    /// (`Lzma2Dec.c:130`, `:140`), so a chunk can never declare zero length.
    fn update_state(&mut self, b: u8) -> Lzma2State {
        match self.state {
            Lzma2State::Control => {
                // `p->isExtraMode = False` (:102) is not modelled.
                self.control = b; // :103
                if b == 0 {
                    return Lzma2State::Finished; // :106-107
                }
                if is_uncompressed(b) {
                    // :108
                    if b == CONTROL_COPY_RESET_DIC {
                        // :110-111 — an uncompressed chunk that resets the
                        // dictionary satisfies the "dictionary is initialized"
                        // half of the requirement but not the "props are set"
                        // half, so the floor drops to 0xC0 (LZMA + new props)
                        // rather than to 0.
                        self.need_init_level = 0xC0;
                    } else if b > 2 || self.need_init_level == 0xE0 {
                        // :112-113 — control 2 is "uncompressed, no reset", which
                        // presupposes an initialized dictionary. `needInitLevel`
                        // is still 0xE0 only at the very start of a stream, so
                        // this is exactly the rule that a stream may not open with
                        // control 2. Everything above 2 with bit 7 clear is
                        // undefined and rejected.
                        return Lzma2State::Error;
                    }
                } else {
                    // :117-118 — an LZMA chunk must carry at least as much
                    // initialization as is still outstanding: 0xE0 (reset dic +
                    // state + props) at the start of a stream, 0xC0 (state +
                    // props) after a dictionary-resetting copy chunk, 0 once a
                    // chunk has been decoded.
                    if b < self.need_init_level {
                        return Lzma2State::Error;
                    }
                    self.need_init_level = 0; // :119
                    self.unpack_size = u32::from(b & 0x1F) << 16; // :120
                }
                Lzma2State::Unpack0 // :122
            }

            Lzma2State::Unpack0 => {
                self.unpack_size |= u32::from(b) << 8; // :125
                Lzma2State::Unpack1 // :126
            }

            Lzma2State::Unpack1 => {
                // :129-130. `wrapping_add` is total; it cannot actually wrap. For
                // an LZMA chunk `unpackSize` was assigned at :120 and is at most
                // 0x1FFFFF here; for an uncompressed chunk the C ORs into whatever
                // was left over, and that is provably 0 — `Control` is only ever
                // reached with `unpackSize == 0` (:252, :290-293).
                self.unpack_size = (self.unpack_size | u32::from(b)).wrapping_add(1);
                // :132
                if is_uncompressed(self.control) {
                    Lzma2State::Data
                } else {
                    Lzma2State::Pack0
                }
            }

            Lzma2State::Pack0 => {
                self.pack_size = u32::from(b) << 8; // :135
                Lzma2State::Pack1 // :136
            }

            Lzma2State::Pack1 => {
                // :139-140. `packSize` was assigned at :135, so this is at most
                // 0xFFFF + 1; `wrapping_add` for totality only.
                self.pack_size = (self.pack_size | u32::from(b)).wrapping_add(1);
                // :143 — bit 6 of the control byte is "a property byte follows".
                if self.control & 0x40 != 0 {
                    Lzma2State::Prop
                } else {
                    Lzma2State::Data
                }
            }

            Lzma2State::Prop => {
                // :148-149 — the same packing LZMA1 uses, and the same rejection
                // as `LzmaProps_Decode` (`LzmaDec.c:1273-1274`). Without it,
                // `pb` could reach 5 and index past every 16-entry pos-state table.
                if b >= 9 * 5 * 5 {
                    return Lzma2State::Error;
                }
                let v = u32::from(b);
                let lc = v % 9; // :150
                let d = v / 9; // :151
                let pb = d / 5; // :152
                let lp = d % 5; // :153
                // :154-155 — LZMA2's extra rule on top of LZMA1's. This is what
                // makes the probability array sized at `old_props` time (lc = 4,
                // lp = 0) big enough for every chunk: `0x300 << (lc + lp)` can
                // never exceed `0x300 << LZMA2_LCLP_MAX`.
                if lc + lp > LZMA2_LCLP_MAX {
                    return Lzma2State::Error;
                }
                // :152, :156-157. The C assigns `pb` before the `lc + lp` test and
                // the other two after; the difference is unobservable because the
                // failing path goes to `Error`, which is terminal, so all three are
                // written together here.
                self.decoder.prop.lc = lc;
                self.decoder.prop.lp = lp;
                self.decoder.prop.pb = pb;
                Lzma2State::Data // :158
            }

            // :161-162 — the `default:` arm. Reaching it means a header byte was
            // offered while the machine was in a payload or terminal state, which
            // the caller (`:203`) already excludes.
            Lzma2State::Data
            | Lzma2State::DataCont
            | Lzma2State::Finished
            | Lzma2State::Error => Lzma2State::Error,
        }
    }

    // -----------------------------------------------------------------------
    // Lzma2Dec_DecodeToDic (Lzma2Dec.c:178-304)
    // -----------------------------------------------------------------------

    /// `Lzma2Dec_DecodeToDic` (`Lzma2Dec.c:178-304`): decode into the dictionary
    /// window up to `dic_limit`, returning the status and how many `src` bytes were
    /// consumed.
    ///
    /// Errors are latched: the C sets `p->state = LZMA2_STATE_ERROR` before
    /// returning `SZ_ERROR_DATA` (`:302-303`) and the loop condition at `:185`
    /// refuses to run again, so a caller that ignores the failure gets the same
    /// failure rather than a decode from a poisoned state. Every `break` below is
    /// one of the C's, and lands on that same tail.
    ///
    /// One deliberate difference: where the C funnels a failure of the inner LZMA
    /// decoder into `SZ_ERROR_DATA` (`:277-278`), this propagates the inner
    /// [`LzmaDecodeError`] unchanged. It is strictly more specific — the inner
    /// decoder distinguishes `DataError` from `Internal` — and every value it can
    /// return is still an error.
    fn decode_to_dic(
        &mut self,
        dic_limit: usize,
        src: &[u8],
        finish_mode: FinishMode,
    ) -> Result<(Lzma2Status, usize), LzmaDecodeError> {
        let in_size = src.len(); // :181
        let mut src_len: usize = 0; // :182

        // :185
        while self.state != Lzma2State::Error {
            // :189-193
            if self.state == Lzma2State::Finished {
                return Ok((Lzma2Status::FinishedWithMark, src_len));
            }

            // :195
            let dic_pos = self.decoder.dic_pos;
            let out_room = match dic_limit.checked_sub(dic_pos) {
                Some(v) => v,
                // The C computes `dicLimit - dicPos` as a `SizeT` at :219 and
                // relies on the caller never passing a limit below `dicPos`.
                // Ours says so out loud.
                None => {
                    self.state = Lzma2State::Error;
                    return Err(LzmaDecodeError::Internal);
                }
            };

            // :197-201
            if out_room == 0 {
                match finish_mode {
                    FinishMode::Any => return Ok((Lzma2Status::NotFinished, src_len)),
                    FinishMode::End => {}
                }
            }

            // ---- header bytes (:203-215) ----
            if self.state != Lzma2State::Data && self.state != Lzma2State::DataCont {
                // :205-209
                if src_len == in_size {
                    return Ok((Lzma2Status::NeedsMoreInput, src_len));
                }
                let b = match src.get(src_len) {
                    Some(v) => *v,
                    None => break,
                };
                src_len += 1; // :210
                self.state = self.update_state(b); // :211
                // :212-213 — the output is already full, so a chunk that is about
                // to want room cannot be started.
                if out_room == 0 && self.state != Lzma2State::Finished {
                    break;
                }
                continue; // :214
            }

            // ---- payload (:217-298) ----
            // :218 — `src_len <= in_size` is maintained above.
            let mut in_cur = in_size - src_len;
            let mut out_cur = out_room; // :219
            let mut cur_finish = FinishMode::Any; // :220

            // :222-226 — clamp the output to what the chunk still owes. Reaching
            // exactly `unpackSize` is what turns the finish mode strict, so the
            // inner decoder checks that the chunk's LZMA stream really ended there.
            if out_cur as u64 >= u64::from(self.unpack_size) {
                out_cur = self.unpack_size as usize;
                cur_finish = FinishMode::End;
            }

            if is_uncompressed(self.control) {
                // ---- uncompressed chunk (:228-253) ----
                // :230-234
                if in_cur == 0 {
                    return Ok((Lzma2Status::NeedsMoreInput, src_len));
                }

                // :236-240 — reset dispatch. An uncompressed chunk never resets the
                // probability model (`initState` is hard-coded `False`); it only
                // optionally resets the dictionary, and only for control 1.
                if self.state == Lzma2State::Data {
                    let init_dic = self.control == CONTROL_COPY_RESET_DIC;
                    init_dic_and_state(&mut self.decoder, init_dic, false);
                }

                // :242-245
                if in_cur > out_cur {
                    in_cur = out_cur;
                }
                if in_cur == 0 {
                    break;
                }

                // :247
                let chunk = match src.get(src_len..src_len + in_cur) {
                    Some(v) => v,
                    None => break,
                };
                match update_with_uncompressed(&mut self.decoder, chunk) {
                    Ok(()) => {}
                    Err(e) => {
                        self.state = Lzma2State::Error;
                        return Err(e);
                    }
                }

                // :249-251
                src_len += in_cur;
                let taken = match u32::try_from(in_cur) {
                    Ok(v) => v,
                    Err(_) => break,
                };
                self.unpack_size = match self.unpack_size.checked_sub(taken) {
                    Some(v) => v,
                    // Excluded by the `out_cur` clamp at :222-226; stated rather
                    // than assumed, because the C would wrap here.
                    None => break,
                };
                // :252
                self.state = if self.unpack_size == 0 {
                    Lzma2State::Control
                } else {
                    Lzma2State::DataCont
                };
            } else {
                // ---- LZMA chunk (:254-297) ----
                // :258-264 — reset dispatch. Bit 5 of the control byte
                // (`>= 0xA0`) resets the probability model, reps and state; bit 6
                // (`>= 0xC0`) additionally brings a property byte, already applied
                // in `Prop`; bit 7 with both (`>= 0xE0`) also restarts the
                // dictionary accounting.
                if self.state == Lzma2State::Data {
                    let init_dic = self.control >= 0xE0;
                    let init_state = self.control >= 0xA0;
                    init_dic_and_state(&mut self.decoder, init_dic, init_state);
                    self.state = Lzma2State::DataCont;
                }

                // :266-267 — never hand the inner decoder more than this chunk owns.
                if in_cur as u64 > u64::from(self.pack_size) {
                    in_cur = self.pack_size as usize;
                }

                let sub = match src.get(src_len..src_len + in_cur) {
                    Some(v) => v,
                    None => break,
                };
                let limit = match dic_pos.checked_add(out_cur) {
                    Some(v) => v,
                    None => break,
                };

                // :269
                let (status, consumed) = match self.decoder.decode_to_dic(limit, sub, cur_finish) {
                    Ok(v) => v,
                    // :277-278. The C performs the bookkeeping at :271-275 first
                    // and then breaks to the error tail, so none of it is
                    // observable on this path.
                    Err(e) => {
                        self.state = Lzma2State::Error;
                        return Err(e);
                    }
                };

                // :271-273
                src_len += consumed;
                let eaten = match u32::try_from(consumed) {
                    Ok(v) => v,
                    Err(_) => break,
                };
                self.pack_size = match self.pack_size.checked_sub(eaten) {
                    Some(v) => v,
                    None => break,
                };

                // :274-275
                let out_done = match self.decoder.dic_pos.checked_sub(dic_pos) {
                    Some(v) => v,
                    None => break,
                };
                let made = match u32::try_from(out_done) {
                    Ok(v) => v,
                    Err(_) => break,
                };
                self.unpack_size = match self.unpack_size.checked_sub(made) {
                    Some(v) => v,
                    None => break,
                };

                // :280-285
                match status {
                    Status::NeedsMoreInput => {
                        // A chunk that ran out of compressed bytes *and* has none
                        // left to give is corrupt, not short.
                        if self.pack_size == 0 {
                            break;
                        }
                        return Ok((Lzma2Status::NeedsMoreInput, src_len));
                    }
                    Status::FinishedWithMark
                    | Status::NotFinished
                    | Status::MaybeFinishedWithoutMark => {}
                }

                // :287-294 — the chunk made no progress in either direction. That
                // is only legitimate as the clean end of a chunk: the inner stream
                // stopped exactly on the declared `unpackSize` with its range coder
                // drained, and the declared `packSize` was spent to the byte.
                if consumed == 0 && out_done == 0 {
                    let ended_cleanly = status == Status::MaybeFinishedWithoutMark
                        && self.unpack_size == 0
                        && self.pack_size == 0;
                    if !ended_cleanly {
                        break;
                    }
                    self.state = Lzma2State::Control;
                }
                // :296 — the C clears `*status` here so a stale value cannot escape
                // through a later `return`. Ours is a local, so nothing can.
            }
        }

        // :301-303
        self.state = Lzma2State::Error;
        Err(LzmaDecodeError::DataError)
    }

    // -----------------------------------------------------------------------
    // Lzma2Dec_DecodeToBuf (Lzma2Dec.c:430-468)
    // -----------------------------------------------------------------------

    /// `Lzma2Dec_DecodeToBuf` (`Lzma2Dec.c:430-468`): drive [`Self::decode_to_dic`]
    /// around the circular window, copying each newly written run out into `dest`.
    ///
    /// The only structural change from the C is that the window wrap
    /// (`:441-442`) and the copy-out (`:459`) are bounds-checked. `dest` plays the
    /// role of the C's `dest`/`*destLen` pair, and is the caller's staging buffer —
    /// [`Self::decode`] owns a 64 KiB one exactly as `C_LZMA2.cpp:130` does, which
    /// is what makes a failed call discard its partial output the way
    /// `C_LZMA2.cpp:156-160` does.
    pub fn decode_to_buf(
        &mut self,
        dest: &mut [u8],
        src: &[u8],
        finish_mode: FinishMode,
    ) -> Result<Lzma2Progress, LzmaDecodeError> {
        let out_total = dest.len(); // :432
        let mut out_pos: usize = 0; // :433
        let mut in_off: usize = 0; // :433

        // :435
        loop {
            // :441-442 — the window is circular; a full one restarts at 0.
            if self.decoder.dic_pos == self.decoder.dic_buf_size() {
                self.decoder.dic_pos = 0;
            }
            let dic_pos = self.decoder.dic_pos; // :443
            let mut cur_finish = FinishMode::Any; // :444
            // :445
            let mut out_cur = self
                .decoder
                .dic_buf_size()
                .checked_sub(dic_pos)
                .ok_or(LzmaDecodeError::Internal)?;
            let out_room = out_total
                .checked_sub(out_pos)
                .ok_or(LzmaDecodeError::Internal)?;

            // :447-451
            if out_cur >= out_room {
                out_cur = out_room;
                cur_finish = finish_mode;
            }

            let tail = src.get(in_off..).ok_or(LzmaDecodeError::Internal)?;
            let limit = dic_pos
                .checked_add(out_cur)
                .ok_or(LzmaDecodeError::Internal)?;

            // :453. The C keeps `*status` from the last pass and discards the
            // earlier ones; here it is simply a binding that only the returning
            // pass reads.
            let (status, consumed) = self.decode_to_dic(limit, tail, cur_finish)?;

            // :455-457
            in_off += consumed;

            // :458-462
            let produced = self
                .decoder
                .dic_pos
                .checked_sub(dic_pos)
                .ok_or(LzmaDecodeError::Internal)?;
            let out_end = out_pos
                .checked_add(produced)
                .ok_or(LzmaDecodeError::Internal)?;
            let window_end = dic_pos
                .checked_add(produced)
                .ok_or(LzmaDecodeError::Internal)?;
            let written = dest
                .get_mut(out_pos..out_end)
                .ok_or(LzmaDecodeError::Internal)?;
            let fresh = self
                .decoder
                .dic
                .get(dic_pos..window_end)
                .ok_or(LzmaDecodeError::Internal)?;
            written.copy_from_slice(fresh);
            out_pos = out_end;

            // :463-466. `res != 0` is the `?` above. Each further pass produces at
            // least one byte, so `out_pos` strictly increases and the loop ends.
            if produced == 0 || out_pos == out_total {
                return Ok(Lzma2Progress {
                    status,
                    src_consumed: in_off,
                    out_written: out_pos,
                });
            }
        }
    }

    // -----------------------------------------------------------------------
    // The streaming driver (C_LZMA2.cpp:127-176)
    // -----------------------------------------------------------------------

    /// Decode a whole LZMA2 stream from `source` into `sink`, stopping at the
    /// end-of-stream control byte.
    ///
    /// This is `C_LZMA2.cpp:127-176` with the two callback verbs replaced by the
    /// stream traits: a 64 KiB read buffer, a 64 KiB staging buffer,
    /// `LZMA_FINISH_ANY`, and a stop on `LZMA_STATUS_FINISHED_WITH_MARK`. The
    /// property byte is *not* read here — see [`decode_lzma2_stream`], which mirrors
    /// `C_LZMA2.cpp:114-117`.
    ///
    /// Memory is the dictionary plus 128 KiB, independent of the payload size.
    pub fn decode(
        &mut self,
        source: &mut dyn InStream,
        sink: &mut dyn OutStream,
    ) -> Result<Lzma2DecodeSummary, LzmaDecodeError> {
        // C_LZMA2.cpp:129-135 — both buffers, or NOT_ENOUGH_MEMORY.
        let mut in_buf: Vec<u8> = Vec::new();
        in_buf
            .try_reserve_exact(IN_BUF)
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;
        in_buf.resize(IN_BUF, 0);
        let mut out_buf: Vec<u8> = Vec::new();
        out_buf
            .try_reserve_exact(OUT_BUF)
            .map_err(|_| LzmaDecodeError::NotEnoughMemory)?;
        out_buf.resize(OUT_BUF, 0);

        let mut in_pos: usize = 0;
        let mut in_avail: usize = 0;
        let mut summary = Lzma2DecodeSummary::default();

        // C_LZMA2.cpp:140
        loop {
            // C_LZMA2.cpp:141-146 — refill only when the buffer is spent.
            if in_pos == in_avail {
                let got = source.read(&mut in_buf)?;
                if got > in_buf.len() {
                    // An `InStream` claiming more than it was given.
                    return Err(LzmaDecodeError::Internal);
                }
                in_avail = got;
                in_pos = 0;
                summary.input_read += got as u64;
            }

            let src = in_buf
                .get(in_pos..in_avail)
                .ok_or(LzmaDecodeError::Internal)?;

            // C_LZMA2.cpp:148-153. Always `LZMA_FINISH_ANY`: the payload length is
            // unknown, so no output boundary is ever known to be the last.
            let progress = self.decode_to_buf(&mut out_buf, src, FinishMode::Any)?;

            // C_LZMA2.cpp:154
            in_pos += progress.src_consumed;
            summary.input_consumed += progress.src_consumed as u64;

            // C_LZMA2.cpp:162-165
            if progress.out_written != 0 {
                let out = out_buf
                    .get(..progress.out_written)
                    .ok_or(LzmaDecodeError::Internal)?;
                sink.write(out)?;
                summary.output_written += progress.out_written as u64;
            }

            match progress.status {
                // C_LZMA2.cpp:167
                Lzma2Status::FinishedWithMark => return Ok(summary),
                // C_LZMA2.cpp:168-171. The C calls this BAD_COMPRESSED_DATA; here
                // it keeps its own name so a caller can tell "the archive is short"
                // from "the archive is wrong".
                Lzma2Status::NeedsMoreInput => {
                    if in_avail == 0 {
                        return Err(LzmaDecodeError::TruncatedInput);
                    }
                }
                Lzma2Status::NotFinished => {}
            }

            // C_LZMA2.cpp:172-175 — a pass that neither consumed nor produced would
            // spin forever.
            if progress.out_written == 0 && progress.src_consumed == 0 {
                return Err(LzmaDecodeError::DataError);
            }
        }
    }
}

/// Decode a complete DArc LZMA2 stream: one property byte followed by the LZMA2
/// stream itself.
///
/// This is the whole of `lzma2_decompress` (`C_LZMA2.cpp:112-182`). The wire format
/// is documented at `C_LZMA2.cpp:3-4`: the stream is self-terminating, so there is
/// no length prefix and nothing to read past the `0x00` control byte.
pub fn decode_lzma2_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
) -> Result<Lzma2DecodeSummary, LzmaDecodeError> {
    // C_LZMA2.cpp:114-117 — one read, and it must deliver the byte. A short read
    // is not end-of-stream in general (`stream.rs:37-41`), but a one-byte buffer
    // has no short-but-non-empty case: `got` is 0 or 1, and anything else is an
    // `InStream` claiming more than it was given.
    let mut prop = [0u8; 1];
    let got = source.read(&mut prop)?;
    let prop_byte = match got {
        1 => prop[0],
        0 => return Err(LzmaDecodeError::TruncatedInput),
        _ => return Err(LzmaDecodeError::Internal),
    };

    // C_LZMA2.cpp:119-125
    let mut dec = Lzma2Dec::new(prop_byte)?;
    let mut summary = dec.decode(source, sink)?;
    summary.input_read += 1;
    summary.input_consumed += 1;
    Ok(summary)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stream::{SliceIn, VecOut};
    use crate::{LzmaProps, encode};

    /// An LZMA2 property byte whose dictionary (2 << 16 = 128 KiB) comfortably
    /// covers every test payload.
    const P128K: u8 = 10;

    /// Deterministic xorshift bytes, same generator as `decode_stream.rs`'s tests.
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

    /// Run a whole stream: `prop` then `body`.
    fn run(prop: u8, body: &[u8]) -> Result<Vec<u8>, LzmaDecodeError> {
        let mut stream = Vec::with_capacity(body.len() + 1);
        stream.push(prop);
        stream.extend_from_slice(body);
        let mut input = SliceIn::new(&stream);
        let mut output = VecOut::default();
        decode_lzma2_stream(&mut input, &mut output)?;
        Ok(output.data)
    }

    /// An uncompressed chunk: `control` must be 1 (reset dic) or 2 (no reset).
    fn copy_chunk(control: u8, data: &[u8]) -> Vec<u8> {
        assert!(!data.is_empty() && data.len() <= 0x10000);
        let n = (data.len() - 1) as u32;
        let mut v = vec![control, (n >> 8) as u8, n as u8];
        v.extend_from_slice(data);
        v
    }

    /// A real LZMA chunk, built with this crate's encoder. `control` picks the reset
    /// flags; anything with bit 6 set carries the property byte.
    fn lzma_chunk(control: u8, data: &[u8], lc: u8, lp: u8, pb: u8, dict: u32) -> Vec<u8> {
        let mut props = LzmaProps::for_level(5, dict);
        props.lc = lc;
        props.lp = lp;
        props.pb = pb;
        props.dict_size = dict;
        props.write_end_mark = false;
        let body = encode(data, &props).expect("encode");
        assert!(!data.is_empty() && data.len() <= 0x200000);
        assert!(!body.is_empty() && body.len() <= 0x10000);
        let unpack = (data.len() - 1) as u32;
        let pack = (body.len() - 1) as u32;
        let mut v = vec![
            control | (unpack >> 16) as u8,
            (unpack >> 8) as u8,
            unpack as u8,
            (pack >> 8) as u8,
            pack as u8,
        ];
        if control & 0x40 != 0 {
            v.push((pb * 5 + lp) * 9 + lc);
        }
        v.extend_from_slice(&body);
        v
    }

    // -- Lzma2Dec_GetOldProps (Lzma2Dec.c:57-68) --------------------------

    #[test]
    fn old_props_matches_the_c_formula() {
        for prop in 0u8..=40 {
            let p = old_props(prop).unwrap_or_else(|e| panic!("prop {prop}: {e:?}"));
            // Lzma2Dec.c:63 — always the maximum lc, never the stream's real one.
            assert_eq!(p[0], 4, "prop {prop}");
            let dic = u32::from_le_bytes([p[1], p[2], p[3], p[4]]);
            let expect = if prop == 40 {
                0xFFFF_FFFF
            } else {
                (2u32 | u32::from(prop & 1)) << (u32::from(prop) / 2 + 11)
            };
            assert_eq!(dic, expect, "prop {prop}");
        }
        // A couple of anchors read straight off the formula.
        assert_eq!(u32::from_le_bytes(old_props(0).unwrap()[1..5].try_into().unwrap()), 4096);
        assert_eq!(u32::from_le_bytes(old_props(1).unwrap()[1..5].try_into().unwrap()), 6144);
        assert_eq!(
            u32::from_le_bytes(old_props(39).unwrap()[1..5].try_into().unwrap()),
            3 << 30
        );
    }

    /// `Lzma2Dec.c:60-61`. The allocation gate: nothing is reserved for a prop byte
    /// this decoder cannot describe.
    #[test]
    fn rejects_a_prop_byte_above_40() {
        for prop in 41u8..=255 {
            assert_eq!(
                old_props(prop),
                Err(LzmaDecodeError::UnsupportedProps),
                "prop {prop}"
            );
            assert_eq!(
                Lzma2Dec::new(prop).err(),
                Some(LzmaDecodeError::UnsupportedProps),
                "prop {prop}"
            );
        }
        // And through the front door, with a body that would otherwise decode.
        assert_eq!(
            run(41, &copy_chunk(1, b"hello")).err(),
            Some(LzmaDecodeError::UnsupportedProps)
        );
        // 40 is the last accepted value (`Lzma2Dec.c:60-62`). It is *not*
        // constructed here: it asks for a 0xFFFFFFFF-byte window, and
        // `Decoder::with_props` would try_reserve and zero 4 GiB. That it is
        // accepted by the gate, and what dictionary it names, is asserted in
        // `old_props_matches_the_c_formula` instead.
        assert!(old_props(40).is_ok());
    }

    // -- round trips ------------------------------------------------------

    #[test]
    fn an_empty_stream_is_just_the_end_marker() {
        assert_eq!(run(P128K, &[0x00]).expect("decode"), Vec::<u8>::new());
    }

    #[test]
    fn round_trips_uncompressed_chunks() {
        let a = b"the quick brown fox ".repeat(3);
        let b = prng(5000, 7);
        let mut body = copy_chunk(1, &a); // reset dic
        body.extend_from_slice(&copy_chunk(2, &b)); // no reset
        body.push(0x00);

        let mut want = a.clone();
        want.extend_from_slice(&b);
        assert_eq!(run(P128K, &body).expect("decode"), want);
    }

    /// A copy chunk of exactly 65536 bytes — the largest an uncompressed chunk can
    /// declare (`unpackSize - 1` is two bytes) and exactly the driver's staging
    /// buffer, so the copy-out at `Lzma2Dec.c:459` lands on its boundary.
    #[test]
    fn round_trips_a_full_size_uncompressed_chunk() {
        let data = prng(0x10000, 3);
        let mut body = copy_chunk(1, &data);
        body.push(0x00);
        assert_eq!(run(P128K, &body).expect("decode"), data);
    }

    /// The dictionary is 4 KiB (prop byte 0) and the payload is 64 KiB, so `dicPos`
    /// wraps repeatedly — the `Lzma2Dec.c:441-442` path.
    #[test]
    fn round_trips_through_a_wrapping_window() {
        let data = prng(0x10000, 11);
        let mut body = copy_chunk(1, &data);
        body.push(0x00);
        let dec = Lzma2Dec::new(0).expect("alloc");
        assert!(dec.dic_buf_size() < data.len());
        assert_eq!(run(0, &body).expect("decode"), data);
    }

    #[test]
    fn round_trips_a_real_lzma_chunk() {
        let data = b"the quick brown fox jumps over the lazy dog. ".repeat(300);
        let mut body = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        body.push(0x00);
        assert_eq!(run(P128K, &body).expect("decode"), data);
    }

    /// Two LZMA chunks and a copy chunk in one stream. Each LZMA chunk resets the
    /// dictionary accounting (control `0xE0`), which is what lets them be encoded
    /// independently; that also exercises `init_dic_and_state(true, true)` twice and
    /// the `Data -> DataCont -> Control` walk three times.
    #[test]
    fn round_trips_a_mixed_multi_chunk_stream() {
        let a = b"aaaaabbbbbcccccddddd".repeat(100);
        let b = prng(3000, 21);
        let c = b"zzzz".repeat(500);

        let mut body = lzma_chunk(0xE0, &a, 3, 0, 2, 65536);
        body.extend_from_slice(&copy_chunk(2, &b));
        body.extend_from_slice(&lzma_chunk(0xE0, &c, 0, 0, 0, 65536));
        body.push(0x00);

        let mut want = a.clone();
        want.extend_from_slice(&b);
        want.extend_from_slice(&c);
        assert_eq!(run(P128K, &body).expect("decode"), want);
    }

    /// Every `lc`/`lp` pair LZMA2 permits (`lc + lp <= 4`, `Lzma2Dec.c:154-155`)
    /// must decode, which is the point of sizing the probability array from
    /// `LZMA2_LCLP_MAX` at `Lzma2Dec.c:63`.
    #[test]
    fn every_permitted_lc_lp_pair_decodes() {
        let data = b"mixed content 0123456789 ".repeat(40);
        for lc in 0u8..=4 {
            for lp in 0u8..=(4 - lc) {
                let mut body = lzma_chunk(0xE0, &data, lc, lp, 1, 65536);
                body.push(0x00);
                assert_eq!(
                    run(P128K, &body).expect("decode"),
                    data,
                    "lc={lc} lp={lp}"
                );
            }
        }
    }

    // -- needInitLevel validation (Lzma2Dec.c:110-119) --------------------

    /// `Lzma2Dec.c:112-113`. `needInitLevel` is `0xE0` at `Lzma2Dec.c:88`, so
    /// control 2 — "uncompressed, no dictionary reset" — cannot be the first chunk:
    /// there is no dictionary to not reset.
    #[test]
    fn rejects_a_stream_opening_with_control_2() {
        let body = copy_chunk(2, b"hello");
        assert_eq!(run(P128K, &body).err(), Some(LzmaDecodeError::DataError));

        // ... but it is fine once a chunk has established one.
        let mut ok = copy_chunk(1, b"hello");
        ok.extend_from_slice(&copy_chunk(2, b"world"));
        ok.push(0x00);
        assert_eq!(run(P128K, &ok).expect("decode"), b"helloworld".to_vec());
    }

    /// `Lzma2Dec.c:112-113`, the other half: bit 7 clear and a value above 2 is
    /// undefined.
    #[test]
    fn rejects_undefined_uncompressed_controls() {
        for control in 3u8..=0x7F {
            let mut body = vec![control, 0x00, 0x00];
            body.extend_from_slice(b"data");
            assert_eq!(
                run(P128K, &body).err(),
                Some(LzmaDecodeError::DataError),
                "control {control:#04x}"
            );
        }
    }

    /// `Lzma2Dec.c:117-118`. At the start of a stream an LZMA chunk must carry the
    /// full `0xE0` initialization; anything less leaves the dictionary, the
    /// probability model or the properties undefined.
    #[test]
    fn rejects_an_lzma_chunk_below_the_needed_init_level() {
        let data = b"payload payload payload".repeat(8);
        for control in [0x80u8, 0x9F, 0xA0, 0xBF, 0xC0, 0xDF] {
            let mut body = lzma_chunk(control, &data, 3, 0, 2, 65536);
            body.push(0x00);
            assert_eq!(
                run(P128K, &body).err(),
                Some(LzmaDecodeError::DataError),
                "control {control:#04x}"
            );
        }
        // 0xE0 is the floor and is accepted.
        let mut ok = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        ok.push(0x00);
        assert_eq!(run(P128K, &ok).expect("decode"), data);
    }

    /// `Lzma2Dec.c:110-111`: a dictionary-resetting copy chunk lowers the floor to
    /// `0xC0`, not to 0 — the properties are still unset.
    #[test]
    fn a_copy_reset_lowers_the_floor_to_0xc0_only() {
        let data = b"payload payload payload".repeat(8);
        for control in [0x80u8, 0xA0, 0xBF] {
            let mut body = copy_chunk(1, b"seed");
            body.extend_from_slice(&lzma_chunk(control, &data, 3, 0, 2, 65536));
            body.push(0x00);
            assert_eq!(
                run(P128K, &body).err(),
                Some(LzmaDecodeError::DataError),
                "control {control:#04x}"
            );
        }
    }

    // -- property-byte validation (Lzma2Dec.c:148-155) --------------------

    /// `Lzma2Dec.c:148-149`. Identical in effect to `LzmaDec.c:1273-1274`: without
    /// it `pb` reaches 5 and every 16-entry pos-state table is indexed out of range.
    #[test]
    fn rejects_a_chunk_property_byte_at_or_above_225() {
        let data = b"payload".repeat(4);
        let good = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        for prop in [225u8, 226, 240, 255] {
            let mut body = good.clone();
            body[5] = prop;
            body.push(0x00);
            assert_eq!(
                run(P128K, &body).err(),
                Some(LzmaDecodeError::DataError),
                "prop {prop}"
            );
        }
    }

    /// `Lzma2Dec.c:154-155`. LZMA1 allows `lc + lp` up to 12; LZMA2 does not,
    /// because the probability array was sized for 4 at `Lzma2Dec.c:63`.
    #[test]
    fn rejects_a_chunk_property_byte_with_lc_plus_lp_above_4() {
        let data = b"payload".repeat(4);
        let good = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        let mut rejected = 0;
        let mut accepted = 0;
        for prop in 0u8..225 {
            let v = u32::from(prop);
            let lc = v % 9;
            let lp = (v / 9) % 5;
            let mut body = good.clone();
            body[5] = prop;
            body.push(0x00);
            let res = run(P128K, &body);
            if lc + lp > 4 {
                assert_eq!(
                    res.err(),
                    Some(LzmaDecodeError::DataError),
                    "prop {prop} (lc={lc} lp={lp}) should be refused"
                );
                rejected += 1;
            } else {
                // Accepted by the header check; the body was encoded for a
                // different lc/lp/pb so it need not decode, but it must not be
                // refused *by this rule* and must not panic.
                drop(res);
                accepted += 1;
            }
        }
        assert!(rejected > 0 && accepted > 0);
        // Sanity: the boundary itself.
        // lc = 5, lp = 0, pb = 0 -> (0 * 5 + 0) * 9 + 5 = 5.
        let mut body = good.clone();
        body[5] = 5;
        body.push(0x00);
        assert_eq!(run(P128K, &body).err(), Some(LzmaDecodeError::DataError));
    }

    // -- truncation -------------------------------------------------------

    /// The six header bytes of a `0xE0` LZMA chunk are control, unpack-hi,
    /// unpack-lo, pack-hi, pack-lo, props. Cutting the stream at each of those
    /// boundaries must be reported as truncation, never as corruption and never as
    /// a successful short decode.
    #[test]
    fn truncation_at_every_header_boundary_is_reported_as_truncation() {
        let data = b"payload payload".repeat(8);
        let full = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        for cut in 0..=6usize {
            let body = full.get(..cut).expect("header is at least 6 bytes");
            assert_eq!(
                run(P128K, body).err(),
                Some(LzmaDecodeError::TruncatedInput),
                "cut after {cut} header bytes"
            );
        }
        // The three header bytes of an uncompressed chunk, likewise.
        let copy = copy_chunk(1, b"abcdef");
        for cut in 0..=3usize {
            let body = copy.get(..cut).expect("header is at least 3 bytes");
            assert_eq!(
                run(P128K, body).err(),
                Some(LzmaDecodeError::TruncatedInput),
                "copy chunk cut after {cut} bytes"
            );
        }
    }

    /// A stream that stops in the middle of a chunk body, both kinds.
    #[test]
    fn truncation_inside_a_chunk_body_is_reported_as_truncation() {
        let data = b"payload payload".repeat(8);
        let full = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        assert_eq!(
            run(P128K, full.get(..full.len() - 1).expect("non-empty")).err(),
            Some(LzmaDecodeError::TruncatedInput)
        );

        let copy = copy_chunk(1, &prng(4000, 31));
        assert_eq!(
            run(P128K, copy.get(..copy.len() - 1000).expect("non-empty")).err(),
            Some(LzmaDecodeError::TruncatedInput)
        );

        // A stream with no end-of-stream control byte at all.
        let mut no_marker = copy_chunk(1, b"abc");
        no_marker.truncate(no_marker.len());
        assert_eq!(
            run(P128K, &no_marker).err(),
            Some(LzmaDecodeError::TruncatedInput)
        );
    }

    /// An empty stream cannot even supply the property byte
    /// (`C_LZMA2.cpp:115-117`).
    #[test]
    fn an_empty_input_is_truncation() {
        let mut input = SliceIn::new(&[]);
        let mut output = VecOut::default();
        assert_eq!(
            decode_lzma2_stream(&mut input, &mut output).err(),
            Some(LzmaDecodeError::TruncatedInput)
        );
    }

    // -- packSize -----------------------------------------------------------

    /// The stored `packSize - 1` of a chunk built by [`lzma_chunk`].
    ///
    /// The payload these tests use is deliberately incompressible, so `packSize` is
    /// in the thousands and every `+/- 200` below is unambiguous. An earlier version
    /// used a highly compressible payload, `packSize` came out under 20, and
    /// `stored - 20` wrapped — which release-mode arithmetic hid and a debug build
    /// caught. Hence the assertion.
    fn stored_pack_size(body: &[u8]) -> u32 {
        let v = (u32::from(body[3]) << 8) | u32::from(body[4]);
        assert!(
            (1000..0xF000).contains(&v),
            "test payload must give a mid-range packSize, got {v}"
        );
        v
    }

    fn set_pack_size(body: &mut [u8], v: u32) {
        assert!(v <= 0xFFFF);
        body[3] = (v >> 8) as u8;
        body[4] = v as u8;
    }

    /// A `packSize` larger than the chunk really needs: the inner stream ends on its
    /// declared `unpackSize` with bytes still owed, which `Lzma2Dec.c:289-292`
    /// refuses. The trailing end-of-stream byte guarantees the input is *not*
    /// short, so this must be a data error, not truncation.
    #[test]
    fn rejects_a_pack_size_that_overruns_the_chunk() {
        let data = prng(2000, 41);
        let mut body = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        let stored = stored_pack_size(&body);
        set_pack_size(&mut body, stored + 1);
        body.push(0x00);
        assert_eq!(run(P128K, &body).err(), Some(LzmaDecodeError::DataError));
    }

    /// A `packSize` that runs off the end of the *input*, in both shapes it can
    /// take. Which one applies turns on whether the chunk's declared `unpackSize`
    /// was already satisfied when the bytes ran out — the C draws exactly the same
    /// line, at `Lzma2Dec.c:280-285` versus `:287-292`.
    #[test]
    fn rejects_a_pack_size_that_overruns_the_input() {
        let data = prng(2000, 43);
        let good = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        let stored = stored_pack_size(&good);

        // (a) The whole chunk body is present, so the inner stream reaches its
        // declared `unpackSize` and then stops with `packSize` still owed. Nothing
        // is short — the stream is simply wrong — and `Lzma2Dec.c:289-292` refuses
        // it. This is a data error even though the input also ends here.
        let mut a = good.clone();
        set_pack_size(&mut a, stored + 200);
        assert_eq!(run(P128K, &a).err(), Some(LzmaDecodeError::DataError));

        // (b) The body is short as well, so the inner decoder really does want more
        // bytes and `packSize` still has room for them (`Lzma2Dec.c:282-284`
        // forwards NEEDS_MORE_INPUT rather than failing). The driver then sees an
        // exhausted source (`C_LZMA2.cpp:168-171`).
        let mut b = good.clone();
        set_pack_size(&mut b, stored + 200);
        b.truncate(b.len() - 20);
        assert_eq!(run(P128K, &b).err(), Some(LzmaDecodeError::TruncatedInput));
    }

    /// A `packSize` smaller than the chunk really needs: the inner decoder asks for
    /// more input while the chunk has none left to give (`Lzma2Dec.c:282-283`).
    #[test]
    fn rejects_a_pack_size_that_undershoots_the_chunk() {
        let data = prng(2000, 47);
        let mut body = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        let stored = stored_pack_size(&body);
        set_pack_size(&mut body, stored - 200);
        body.push(0x00);
        assert_eq!(run(P128K, &body).err(), Some(LzmaDecodeError::DataError));
    }

    /// An `unpackSize` that does not match what the chunk really decodes to.
    #[test]
    fn rejects_a_wrong_unpack_size() {
        let data = prng(2000, 53);
        let good = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        let stored =
            (u32::from(good[0] & 0x1F) << 16) | (u32::from(good[1]) << 8) | u32::from(good[2]);
        assert_eq!(stored, data.len() as u32 - 1);
        for delta in [1i32, -1, 64] {
            let mut body = good.clone();
            let moved = stored.wrapping_add(delta as u32);
            body[0] = 0xE0 | (moved >> 16) as u8;
            body[1] = (moved >> 8) as u8;
            body[2] = moved as u8;
            body.push(0x00);
            assert!(
                run(P128K, &body).is_err(),
                "delta {delta} should not decode"
            );
        }
    }

    // -- error latching and totality ---------------------------------------

    /// `Lzma2Dec.c:185`, `:302`. Once the state machine has failed it must keep
    /// failing rather than decode from a poisoned state.
    #[test]
    fn a_data_error_latches() {
        let mut dec = Lzma2Dec::new(P128K).expect("alloc");
        let mut out = vec![0u8; 4096];
        // Control 2 as the very first chunk.
        assert_eq!(
            dec.decode_to_buf(&mut out, &[2u8, 0, 0, 1, 2, 3], FinishMode::Any)
                .err(),
            Some(LzmaDecodeError::DataError)
        );
        // A subsequent, perfectly valid stream must not be decoded.
        let good = copy_chunk(1, b"hello");
        assert_eq!(
            dec.decode_to_buf(&mut out, &good, FinishMode::Any).err(),
            Some(LzmaDecodeError::DataError)
        );
        // ... until it is reinitialized.
        dec.init();
        let p = dec
            .decode_to_buf(&mut out, &good, FinishMode::Any)
            .expect("decode");
        assert_eq!(p.out_written, 5);
        assert_eq!(out.get(..5), Some(&b"hello"[..]));
    }

    /// The headline requirement: nothing derived from the stream may panic. Sweeps
    /// single-byte corruptions of every header field of several valid streams, plus
    /// a pile of arbitrary bytes.
    #[test]
    fn corrupt_streams_never_panic() {
        let data = b"the quick brown fox jumps over the lazy dog. ".repeat(8);
        let mut streams: Vec<Vec<u8>> = Vec::new();

        let mut a = lzma_chunk(0xE0, &data, 3, 0, 2, 65536);
        a.push(0x00);
        streams.push(a);

        let mut b = copy_chunk(1, &data);
        b.extend_from_slice(&copy_chunk(2, b"tail"));
        b.push(0x00);
        streams.push(b);

        let mut ok = 0usize;
        let mut bad = 0usize;
        for stream in &streams {
            // The first 24 bytes cover every header field of the first chunk plus
            // the start of its body, which is where all the validation lives.
            for i in 0..stream.len().min(24) {
                for patch in [0x00u8, 0x01, 0x02, 0x7F, 0x80, 0xC0, 0xE0, 0xFF] {
                    let mut m = stream.clone();
                    match m.get_mut(i) {
                        Some(v) => *v = patch,
                        None => continue,
                    }
                    match run(P128K, &m) {
                        Ok(_) => ok += 1,
                        Err(_) => bad += 1,
                    }
                }
            }
            // Truncations at every length.
            for n in 0..stream.len() {
                match run(P128K, stream.get(..n).unwrap_or(&[])) {
                    Ok(_) => ok += 1,
                    Err(_) => bad += 1,
                }
            }
        }

        // Arbitrary bytes. The prop bytes are kept to small dictionaries on
        // purpose: 39 and 40 name 3 GiB and 4 GiB windows, and constructing those
        // hundreds of times measures the allocator, not this decoder. That 41 and
        // above are refused *before* any allocation is `rejects_a_prop_byte_above_40`.
        for seed in 0..24u32 {
            let noise = prng(200, seed * 7 + 1);
            for prop in [0u8, 1, 10, 41, 255] {
                match run(prop, &noise) {
                    Ok(_) => ok += 1,
                    Err(_) => bad += 1,
                }
            }
        }

        assert!(bad > 0, "the sweep must actually reach the error paths");
        assert!(ok + bad > 700, "the sweep must actually run");
    }

    /// Both stream errors are the caller's own codes, passed through unchanged.
    #[test]
    fn propagates_the_stream_error_codes() {
        struct FailingSink;
        impl OutStream for FailingSink {
            fn write(&mut self, _: &[u8]) -> Result<(), crate::StreamError> {
                Err(crate::StreamError(-77))
            }
        }
        struct FailingSource;
        impl InStream for FailingSource {
            fn read(&mut self, _: &mut [u8]) -> Result<usize, crate::StreamError> {
                Err(crate::StreamError(-99))
            }
        }

        let mut body = copy_chunk(1, b"hello");
        body.push(0x00);
        let mut stream = vec![P128K];
        stream.extend_from_slice(&body);

        let mut input = SliceIn::new(&stream);
        assert_eq!(
            decode_lzma2_stream(&mut input, &mut FailingSink).err(),
            Some(LzmaDecodeError::Stream(crate::StreamError(-77)))
        );
        let mut out = VecOut::default();
        assert_eq!(
            decode_lzma2_stream(&mut FailingSource, &mut out).err(),
            Some(LzmaDecodeError::Stream(crate::StreamError(-99)))
        );
    }

    /// One byte at a time, so every header byte and every chunk body crosses a
    /// buffer boundary and the `NEEDS_MORE_INPUT` resumption path is used
    /// throughout.
    #[test]
    fn decodes_through_a_one_byte_at_a_time_reader() {
        struct Dribble<'a> {
            data: &'a [u8],
            pos: usize,
        }
        impl InStream for Dribble<'_> {
            fn read(&mut self, buf: &mut [u8]) -> Result<usize, crate::StreamError> {
                match (self.data.get(self.pos), buf.get_mut(0)) {
                    (Some(b), Some(slot)) => {
                        *slot = *b;
                        self.pos += 1;
                        Ok(1)
                    }
                    (Some(_), None) | (None, Some(_)) | (None, None) => Ok(0),
                }
            }
        }

        let a = b"the quick brown fox ".repeat(50);
        let b = prng(1500, 5);
        let mut body = lzma_chunk(0xE0, &a, 3, 0, 2, 65536);
        body.extend_from_slice(&copy_chunk(2, &b));
        body.push(0x00);
        let mut stream = vec![P128K];
        stream.extend_from_slice(&body);

        let mut input = Dribble {
            data: &stream,
            pos: 0,
        };
        let mut output = VecOut::default();
        let summary = decode_lzma2_stream(&mut input, &mut output).expect("decode");

        let mut want = a.clone();
        want.extend_from_slice(&b);
        assert_eq!(output.data, want);
        assert_eq!(summary.output_written, want.len() as u64);
        assert_eq!(summary.input_consumed, stream.len() as u64);
    }

    /// The summary must account for the property byte and stop at the end marker,
    /// leaving anything after it unconsumed.
    #[test]
    fn stops_at_the_end_marker_and_reports_the_consumed_length() {
        let data = b"hello world".repeat(10);
        let mut body = copy_chunk(1, &data);
        body.push(0x00);
        let consumed_expected = body.len() as u64 + 1;

        let mut stream = vec![P128K];
        stream.extend_from_slice(&body);
        stream.extend_from_slice(b"TRAILING GARBAGE THAT MUST NOT BE READ");

        let mut input = SliceIn::new(&stream);
        let mut output = VecOut::default();
        let summary = decode_lzma2_stream(&mut input, &mut output).expect("decode");
        assert_eq!(output.data, data);
        assert_eq!(summary.input_consumed, consumed_expected);
        assert!(summary.input_read >= summary.input_consumed);
    }
}
