//! LZMA2 encoder — a port of `Compression/LZMA/7z24/Lzma2Enc.c`.
//!
//! LZMA2 is a **container** around the LZMA encoder, not a different algorithm. It
//! cuts the stream into chunks of at most 2 MiB of input / 64 KiB of output, gives
//! each one a 5- or 6-byte header, and can substitute a verbatim copy for any chunk
//! that did not compress. That is the whole format; every compressed byte still
//! comes out of [`crate::encoder`].
//!
//! ## Scope
//!
//! All three configurations `Lzma2Enc_Encode2` can reach from `lzma2_compress`:
//!
//! | `numBlockThreads_Reduced` | `blockSize` | C path | here |
//! |---|---|---|---|
//! | 1 | SOLID | `Lzma2Enc_EncodeMt1`, `inStream` | [`Lzma2Enc::encode_stream`] → one block |
//! | 1 | a size | `Lzma2Enc_EncodeMt1`, `inStream` | the same loop, several blocks |
//! | > 1 | a size | `MtCoder_Code` → `Lzma2Enc_MtCallback_Code` | [`crate::lzma2_mt`] |
//!
//! Which one DArc gets is decided by `GetCompressionThreads()`, since
//! `C_LZMA2.cpp:86-87` sets both thread counts from it: at one thread
//! `Lzma2EncProps_Normalize` (`Lzma2Enc.c:305-309`) turns AUTO into SOLID, and above
//! one it computes a real block size and `Lzma2Enc_Encode2` (`:739`) hands the whole
//! stream to `MtCoder`.
//!
//! **The multi-threaded stream is the ordered concatenation of independently encoded
//! blocks, and nothing more.** `MtCoder` hands out blocks in index order
//! (`MtCoder.c:118`, `:170-173`) and `Lzma2Enc_MtCallback_Write` (`Lzma2Enc.c:695`)
//! writes them back in index order, so the thread count changes the output only
//! through `blockSize` — which is why the C's own bytes are identical for every
//! thread count above one, and why encoding those blocks sequentially or in parallel
//! is the same stream.
//!
//! ## Why the blocked paths are not just "the SOLID loop, restarted"
//!
//! Two things change with the block, both of them observable in the bytes:
//!
//! * **`expectedDataSize`.** `Lzma2Enc_MtCallback_Code` encodes from memory, so
//!   `LzmaEnc_MemPrepare` calls `LzmaEnc_SetDataSize(p, srcLen)` (`LzmaEnc.c:2896`)
//!   with *that block's* length. `MatchFinder_Create` then narrows the hash **mask**
//!   — not merely the allocation — when the declared size is under the dictionary
//!   (`LzFind.c:434-439`), which changes which positions collide and so which
//!   matches are found. Measured against the C: 12 of 330 single-block
//!   configurations differ on this alone. The `inStream` path instead declares
//!   `blockSize` for every block (`Lzma2Enc.c:562-564`), including the short last
//!   one, so the two blocked paths are not interchangeable either.
//! * **The output limit.** On the memory path `packSizeLimit` is
//!   `outLim - packTotal` (`Lzma2Enc.c:594`) and shrinks as the block fills, and the
//!   copy loop's `destPos` accumulates across chunks instead of resetting at each
//!   write (`:186` vs `:189`). Both are ported rather than argued to be
//!   non-binding.
//!
//! What is *not* reproduced is `MtCoder`'s threading structure: no read thread, no
//! block semaphore, no ring of buffers. Only the block boundaries it produces
//! (`MtCoder.c:118`, `:140` — `blockSize` verbatim, no adjustment) and the order it
//! writes them in.
//!
//! ## The five places a plausible port emits different bytes
//!
//! 1. **`LzmaEnc_InitPrices` runs for every chunk** (`LzmaEnc.c:2969`), even the
//!    ones that do not reset the probability model. It restarts `repLenEncCounter`
//!    and zeroes `matchPriceCount`, so the adaptive price-refresh cadence restarts
//!    at every chunk boundary. Hoisting it out of the loop "because the probs did
//!    not change" changes the parse.
//! 2. **`RangeEnc_Init` runs for every chunk** (`LzmaEnc.c:2970`), so each chunk's
//!    payload begins with the range coder's initial cache byte `0x00`, and that byte
//!    is counted in the chunk's `packSize`.
//! 3. **The bounded break** (`LzmaEnc.c:2662`) uses `kNumOpts + 300` on the input
//!    side and `kPackReserve = kNumOpts * 8` on the output side. Both are magic
//!    numbers with no derivation; either one wrong moves every chunk boundary.
//! 4. **The copy path restores the saved state and deliberately does not set
//!    `needInitState`** (`Lzma2Enc.c:190-193`; the assignment is commented out on
//!    purpose). Setting it would force a state reset on the next chunk and change
//!    its control byte *and* its payload.
//! 5. **`LzmaEnc_SaveState` copies a specific field list** (`LzmaEnc.c:360-381`)
//!    that excludes `nowPos64`, `additionalOffset`, the price tables,
//!    `matchPriceCount`, `repLenEncCounter` and the match finder. Restoring more
//!    would rewind work the C does not rewind.

use crate::encoder::Encoder;
use crate::props::{LzmaProps, MatchFinderKind};
use crate::state::MATCH_LEN_MAX;
use crate::stream::{InStream, OutStream, SliceIn, StreamError, VecOut};

// ---------------------------------------------------------------------------
// Constants (Lzma2Enc.c:18-32)
// ---------------------------------------------------------------------------

/// `LZMA2_CONTROL_LZMA` (`Lzma2Enc.c:18`) — bit 7 marks a compressed chunk.
const CONTROL_LZMA: u8 = 1 << 7;
/// `LZMA2_CONTROL_COPY_NO_RESET` (`Lzma2Enc.c:19`).
const CONTROL_COPY_NO_RESET: u8 = 2;
/// `LZMA2_CONTROL_COPY_RESET_DIC` (`Lzma2Enc.c:20`).
const CONTROL_COPY_RESET_DIC: u8 = 1;
/// `LZMA2_CONTROL_EOF` (`Lzma2Enc.c:21`) — the stream terminator.
const CONTROL_EOF: u8 = 0;
/// `LZMA2_LCLP_MAX` (`Lzma2Enc.c:23`).
const LCLP_MAX: i32 = 4;
/// `LZMA2_PACK_SIZE_MAX` (`Lzma2Enc.c:27`) — `desiredPackSize` per chunk.
const PACK_SIZE_MAX: u32 = 1 << 16;
/// `LZMA2_COPY_CHUNK_SIZE` (`Lzma2Enc.c:28`).
const COPY_CHUNK_SIZE: u32 = PACK_SIZE_MAX;
/// `LZMA2_UNPACK_SIZE_MAX` (`Lzma2Enc.c:29`) — `maxUnpackSize` per chunk.
const UNPACK_SIZE_MAX: u32 = 1 << 21;
/// `LZMA2_KEEP_WINDOW_SIZE` (`Lzma2Enc.c:30`).
///
/// This is a **correctness** parameter, not a memory hint: the copy chunk re-reads
/// up to `LZMA2_UNPACK_SIZE_MAX` bytes behind the encoder's cursor
/// (`Lzma2Enc.c:176`), so the match finder's window must still hold them. It is
/// threaded into `MatchFinder_Create` through `LzmaEnc_Alloc`'s `beforeSize`
/// (`LzmaEnc.c:2729`).
const KEEP_WINDOW_SIZE: u32 = UNPACK_SIZE_MAX;
/// `LZMA2_CHUNK_SIZE_COMPRESSED_MAX` (`Lzma2Enc.c:32`) — `tempBufLzma`'s size, and
/// so the `packSizeLimit` every subblock is given on the stream path
/// (`Lzma2Enc.c:585`).
const CHUNK_SIZE_COMPRESSED_MAX: usize = (1 << 16) + 16;

/// `LZMA2_ENC_PROPS_BLOCK_SIZE_AUTO` (`Lzma2Enc.h:11`).
pub const BLOCK_SIZE_AUTO: u64 = 0;
/// `LZMA2_ENC_PROPS_BLOCK_SIZE_SOLID` (`Lzma2Enc.h:12`) — `(UInt64)(Int64)-1`.
pub const BLOCK_SIZE_SOLID: u64 = u64::MAX;

/// `MTCODER_THREADS_MAX` (`MtCoder.h:19`) with `Z7_ST` **undefined**, which is how
/// `Compression/LZMA/makefile` builds `Lzma2Enc.c` — it passes no `-DZ7_ST`. Using
/// the `Z7_ST` value of 1 here would silently make every configuration look
/// single-threaded and so look supported.
const MTCODER_THREADS_MAX: i32 = 64;

/// `kReduceMin` (`LzmaEnc.c:84`).
const K_REDUCE_MIN: u32 = 1 << 12;
/// `kLzmaMaxHistorySize` (`LzmaEnc.c:41`).
const K_LZMA_MAX_HISTORY_SIZE: u32 = 15 << 28;

/// `LZMA2_DIC_SIZE_FROM_PROP` (`Lzma2Enc.c:25`).
#[inline]
fn dic_size_from_prop(p: u32) -> u32 {
    (2 | (p & 1)) << (p / 2 + 11)
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Why an LZMA2 encode stopped.
///
/// Every "not implemented" case is its own variant on purpose. Substituting a
/// nearby configuration would produce an archive no other build reproduces, and it
/// would look like success — the failure mode this crate exists to avoid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lzma2Error {
    /// `SZ_ERROR_PARAM` from `Lzma2Enc_SetProps` (`Lzma2Enc.c:470`): `lc + lp > 4`.
    /// LZMA2 is stricter than plain LZMA here, which allows `lc <= 8, lp <= 4`
    /// independently.
    LcLpTooLarge,
    /// `SZ_ERROR_PARAM`, from either of two places: `LzmaEnc_SetProps`
    /// (`LzmaEnc.c:537`) rejecting `lc > 8`, `lp > 4` or `pb > 4`, or
    /// `Lzma2Enc_Encode2` (`Lzma2Enc.c:770-777`) rejecting a `blockSize` that does
    /// not fit a `size_t` or whose output buffer size overflows. The second is
    /// unreachable on a 64-bit target, where `blockSize` is capped at 256 MiB by
    /// `Lzma2EncProps_Normalize`.
    LzmaParam,
    /// No longer produced. `numBlockThreads_Reduced > 1` is implemented — see
    /// [`crate::lzma2_mt`]. Retained so that callers matching exhaustively on this
    /// enum keep compiling.
    MultiThreadedBlocks,
    /// Not implemented: `numThreads > 1` with a binary-tree finder and the optimal
    /// parser selects the multi-threaded match finder (`LzmaEnc.c:2695`).
    ///
    /// This is `lzmaProps.numThreads`, which is **not** the block thread count:
    /// `Lzma2EncProps_Normalize` derives it as `numTotalThreads / numBlockThreads`
    /// (`Lzma2Enc.c:276`), so DArc's `numTotalThreads == numBlockThreads_Max`
    /// (`C_LZMA2.cpp:86-87`) always yields 1 and never reaches this refusal.
    MultiThreadedMatchFinder,
    /// No longer produced. A non-SOLID `blockSize` is implemented on both the
    /// `inStream` and the `MtCoder` path. Retained for the same reason as
    /// [`Lzma2Error::MultiThreadedBlocks`].
    NonSolidBlock,
    /// Not implemented: `btMode` with `numHashBytes >= 5` selects
    /// `Bt5_MatchFinder_GetMatches` (`LzFind.c:1697`), which this crate has no port
    /// of. Reachable only from a hand-built [`RawLzmaProps`]; DArc's five
    /// `matchFinder` IDs never produce it.
    Bt5MatchFinder,
    /// `SZ_ERROR_OUTPUT_EOF`.
    OutputEof,
    /// `SZ_ERROR_FAIL` (`Lzma2Enc.c:627`): the block consumed a different number of
    /// bytes than the limited input stream delivered.
    Fail,
    /// The copy chunk asked for bytes the match finder's window no longer holds.
    /// Unreachable while `KEEP_WINDOW_SIZE` is threaded through correctly; it exists
    /// so that a mistake there refuses instead of copying whatever the slide left
    /// behind.
    WindowTooNarrow,
    /// The caller's own stream raised an error, carrying the caller's code.
    Stream(StreamError),
}

// ---------------------------------------------------------------------------
// Properties
// ---------------------------------------------------------------------------

/// `CLzmaEncProps` (`LzmaEnc.h:22-45`) *before* normalization, with the SDK's
/// "unset" sentinels.
///
/// This is deliberately the un-normalized form rather than [`LzmaProps`]:
/// `Lzma2EncProps_Normalize` runs `LzmaEncProps_Normalize` **twice**, with
/// `reduceSize` temporarily overwritten in between (`Lzma2Enc.c:287-296`), and that
/// dance is only expressible on the raw struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawLzmaProps {
    /// `level`; `< 0` means unset (defaults to 5).
    pub level: i32,
    /// `dictSize`; `0` means unset.
    pub dict_size: u32,
    /// `reduceSize`; `u64::MAX` is the C's `(UInt64)(Int64)-1` "unknown".
    pub reduce_size: u64,
    /// `lc`; `< 0` unset.
    pub lc: i32,
    /// `lp`; `< 0` unset.
    pub lp: i32,
    /// `pb`; `< 0` unset.
    pub pb: i32,
    /// `algo`; `< 0` unset. `0` is the fast (greedy/lazy) parser.
    pub algo: i32,
    /// `fb`; `< 0` unset.
    pub fb: i32,
    /// `btMode`; `< 0` unset.
    pub bt_mode: i32,
    /// `numHashBytes`; `< 0` unset.
    pub num_hash_bytes: i32,
    /// `mc`; `0` is the "auto" sentinel resolved by `LzmaEncProps_Normalize`.
    pub mc: u32,
    /// `numThreads`; `< 0` unset.
    pub num_threads: i32,
    /// `writeEndMark`. LZMA2 ignores it — `LzmaEnc_CodeOneMemBlock` force-clears it
    /// (`LzmaEnc.c:2967`) — but it is part of the struct the C copies around.
    pub write_end_mark: bool,
}

impl RawLzmaProps {
    /// `LzmaEncProps_Init` (`LzmaEnc.c:57`).
    pub fn init() -> Self {
        RawLzmaProps {
            level: 5,
            dict_size: 0,
            reduce_size: u64::MAX,
            lc: -1,
            lp: -1,
            pb: -1,
            algo: -1,
            fb: -1,
            bt_mode: -1,
            num_hash_bytes: -1,
            mc: 0,
            num_threads: -1,
            write_end_mark: false,
        }
    }

    /// `LzmaEncProps_Normalize` (`LzmaEnc.c:68`).
    ///
    /// Idempotent, which is what lets the C call it three times over the same
    /// struct (`Lzma2Enc_SetProps:468`, `Lzma2EncProps_Normalize:294`,
    /// `LzmaEnc_SetProps:536`) without drift.
    pub fn normalize(&mut self) {
        let mut level = self.level;
        if level < 0 {
            level = 5;
        }
        self.level = level;
        let lvl = level as u32;

        if self.dict_size == 0 {
            // sizeof(size_t) is 8 on every target this crate builds for, so the
            // C's `sizeof(size_t) / 2 + 4` is 8 and `... + 24` is 28.
            self.dict_size = if lvl <= 4 {
                1u32 << (lvl * 2 + 16)
            } else if lvl <= 8 {
                1u32 << (lvl + 20)
            } else {
                1u32 << 28
            };
        }

        if u64::from(self.dict_size) > self.reduce_size {
            // The cast is exact: it is only reached when reduceSize < dictSize, and
            // dictSize is a u32.
            let mut v = self.reduce_size as u32;
            if v < K_REDUCE_MIN {
                v = K_REDUCE_MIN;
            }
            if self.dict_size > v {
                self.dict_size = v;
            }
        }

        if self.lc < 0 {
            self.lc = 3;
        }
        if self.lp < 0 {
            self.lp = 0;
        }
        if self.pb < 0 {
            self.pb = 2;
        }
        if self.algo < 0 {
            self.algo = if lvl < 5 { 0 } else { 1 };
        }
        if self.fb < 0 {
            self.fb = if lvl < 7 { 32 } else { 64 };
        }
        if self.bt_mode < 0 {
            self.bt_mode = if self.algo == 0 { 0 } else { 1 };
        }
        if self.num_hash_bytes < 0 {
            self.num_hash_bytes = if self.bt_mode != 0 { 4 } else { 5 };
        }
        if self.mc == 0 {
            // The `>> (btMode ? 0 : 1)` is the one PROVENANCE.md records as having
            // been inlined wrongly before: a hash chain gets HALF a tree's depth.
            self.mc = (16 + ((self.fb as u32) >> 1)) >> u32::from(self.bt_mode == 0);
        }
        if self.num_threads < 0 {
            self.num_threads = if self.bt_mode != 0 && self.algo != 0 {
                2
            } else {
                1
            };
        }
    }

    /// `LzmaEncProps_GetDictSize` (`LzmaEnc.c:111`) — normalize a *copy* and read
    /// its dictionary.
    pub fn dict_size_normalized(&self) -> u32 {
        let mut p = *self;
        p.normalize();
        p.dict_size
    }
}

/// `CLzma2EncProps` (`Lzma2Enc.h:16-23`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lzma2EncProps {
    /// The inner LZMA properties.
    pub lzma: RawLzmaProps,
    /// `blockSize`: [`BLOCK_SIZE_AUTO`], [`BLOCK_SIZE_SOLID`], or a byte count.
    pub block_size: u64,
    /// `numBlockThreads_Reduced` — an *output* of normalization.
    pub num_block_threads_reduced: i32,
    /// `numBlockThreads_Max`.
    pub num_block_threads_max: i32,
    /// `numTotalThreads`.
    pub num_total_threads: i32,
}

impl Lzma2EncProps {
    /// `Lzma2EncProps_Init` (`Lzma2Enc.c:231`).
    pub fn init() -> Self {
        Lzma2EncProps {
            lzma: RawLzmaProps::init(),
            block_size: BLOCK_SIZE_AUTO,
            num_block_threads_reduced: -1,
            num_block_threads_max: -1,
            num_total_threads: -1,
        }
    }

    /// `Lzma2EncProps_Normalize` (`Lzma2Enc.c:240`).
    ///
    /// Two things here are easy to lose. First, `t1n` is the *normalized* thread
    /// count of an untouched copy (`:244-248`) and is used in the arithmetic while
    /// `t1` — the raw, possibly-`-1` field — is what gets stored back. Second, the
    /// `reduceSize = blockSize` dance at `:287-296`: `reduceSize` is temporarily
    /// overwritten so that `LzmaEncProps_Normalize` clamps `dictSize` to the block
    /// size, then restored, so the *dictionary* shrinks but the caller's declared
    /// file size does not.
    //
    // `manual_clamp` would fold the two block-size bounds into `.clamp()`. They are
    // not a clamp here: a THIRD bound (`blockSize < dictSize`) follows them and can
    // push the value back above `kMaxSize`, so the C's order is load-bearing.
    #[allow(clippy::manual_clamp)]
    pub fn normalize(&mut self) {
        let t1n = {
            let mut copy = self.lzma;
            copy.normalize();
            copy.num_threads
        };

        let mut t1 = self.lzma.num_threads;
        let mut t2 = self.num_block_threads_max;
        let mut t3 = self.num_total_threads;

        if t2 > MTCODER_THREADS_MAX {
            t2 = MTCODER_THREADS_MAX;
        }

        if t3 <= 0 {
            if t2 <= 0 {
                t2 = 1;
            }
            t3 = t1n * t2;
        } else if t2 <= 0 {
            t2 = t3 / t1n;
            if t2 == 0 {
                t1 = 1;
                t2 = t3;
            }
            if t2 > MTCODER_THREADS_MAX {
                t2 = MTCODER_THREADS_MAX;
            }
        } else if t1 <= 0 {
            t1 = t3 / t2;
            if t1 == 0 {
                t1 = 1;
            }
        } else {
            t3 = t1n * t2;
        }

        self.lzma.num_threads = t1;

        let mut t2r = t2;

        let file_size = self.lzma.reduce_size;

        if self.block_size != BLOCK_SIZE_SOLID
            && self.block_size != BLOCK_SIZE_AUTO
            && (self.block_size < file_size || file_size == u64::MAX)
        {
            self.lzma.reduce_size = self.block_size;
        }

        self.lzma.normalize();

        self.lzma.reduce_size = file_size;

        t1 = self.lzma.num_threads;

        if self.block_size == BLOCK_SIZE_SOLID {
            t2 = 1;
            t2r = 1;
            t3 = t1;
        } else if self.block_size == BLOCK_SIZE_AUTO && t2 <= 1 {
            // "if there is no block multi-threading, we use SOLID block"
            self.block_size = BLOCK_SIZE_SOLID;
        } else {
            if self.block_size == BLOCK_SIZE_AUTO {
                const K_MIN_SIZE: u64 = 1 << 20;
                const K_MAX_SIZE: u64 = 1 << 28;
                let dict_size = u64::from(self.lzma.dict_size);
                let mut block_size = dict_size << 2;
                if block_size < K_MIN_SIZE {
                    block_size = K_MIN_SIZE;
                }
                if block_size > K_MAX_SIZE {
                    block_size = K_MAX_SIZE;
                }
                if block_size < dict_size {
                    block_size = dict_size;
                }
                block_size += K_MIN_SIZE - 1;
                block_size &= !(K_MIN_SIZE - 1);
                self.block_size = block_size;
            }

            if t2 > 1 && file_size != u64::MAX {
                let mut num_blocks = file_size / self.block_size;
                if num_blocks * self.block_size != file_size {
                    num_blocks += 1;
                }
                if num_blocks < t2 as u64 {
                    t2r = num_blocks as i32;
                    if t2r == 0 {
                        t2r = 1;
                    }
                    t3 = t1 * t2r;
                }
            }
        }

        self.num_block_threads_max = t2;
        self.num_block_threads_reduced = t2r;
        self.num_total_threads = t3;
    }
}

// ---------------------------------------------------------------------------
// The encoder handle
// ---------------------------------------------------------------------------

/// The default ceiling on memory held by blocks being encoded at once, in bytes.
///
/// See [`Lzma2Enc::with_mt_memory_budget`] for what it bounds and why there has to
/// be a bound at all.
pub const DEFAULT_MT_MEMORY_BUDGET: u64 = 1 << 30;

/// A configured LZMA2 encoder — the C's `CLzma2EncHandle` after
/// `Lzma2Enc_SetProps`.
pub struct Lzma2Enc {
    props: Lzma2EncProps,
    /// The normalized, validated LZMA properties handed to [`Encoder`].
    lzma: LzmaProps,
    /// `p->propsByte`, i.e. `LzmaEnc_WriteProperties`'s byte 0
    /// (`Lzma2Enc.c:100`) — **not** the LZMA2 properties byte.
    props_byte: u8,
    /// The in-flight memory ceiling for the multi-block path. Affects speed and
    /// peak memory only; the emitted bytes do not depend on it.
    mt_memory_budget: u64,
}

impl Lzma2Enc {
    /// `Lzma2Enc_SetProps` (`Lzma2Enc.c:465`) plus the validation
    /// `LzmaEnc_SetProps` (`LzmaEnc.c:532`) applies when the block encoder is
    /// created, and the refusals for everything this port does not implement.
    pub fn new(props: &Lzma2EncProps) -> Result<Self, Lzma2Error> {
        // Lzma2Enc.c:467-471 -- the lc+lp check runs on a normalized COPY, before
        // Lzma2EncProps_Normalize touches the real thing.
        {
            let mut check = props.lzma;
            check.normalize();
            if check.lc + check.lp > LCLP_MAX {
                return Err(Lzma2Error::LcLpTooLarge);
            }
        }

        let mut props = *props;
        props.normalize();

        let lzma = resolve_lzma_props(&props.lzma)?;

        // Lzma2EncInt_InitStream (Lzma2Enc.c:92): the first byte of
        // LzmaEnc_WriteProperties, cached once for the whole stream. Note "for the
        // whole stream", not per block: `propsAreSet` is per coder and the props
        // never change, so every block's mode-2/3 chunk carries this same byte.
        let props_byte = lzma.decoder_props()[0];

        Ok(Lzma2Enc {
            props,
            lzma,
            props_byte,
            mt_memory_budget: DEFAULT_MT_MEMORY_BUDGET,
        })
    }

    /// Cap the memory the multi-block path may hold in blocks being encoded at once.
    ///
    /// **It cannot change the output.** Blocks are independent and written in index
    /// order, so this only decides how many of them are in flight; one gives a purely
    /// sequential encode. The bound is needed because `blockSize` reaches 256 MiB
    /// (`Lzma2Enc.c:315`) and each in-flight block holds its input *and* its output
    /// buffer, so an unbounded `numBlockThreads` would ask for
    /// `threads * 2 * blockSize` — several gigabytes on an ordinary machine. The C
    /// has the same appetite and no such cap.
    ///
    /// At least one block is always in flight, whatever the budget.
    pub fn with_mt_memory_budget(mut self, bytes: u64) -> Self {
        self.mt_memory_budget = bytes;
        self
    }

    /// How many blocks [`Lzma2Enc::encode_stream`] will encode concurrently, after
    /// both `numBlockThreads_Reduced` and the memory budget have had their say.
    ///
    /// `1` means the encode runs on the calling thread with no spawning at all.
    pub fn mt_blocks_in_flight(&self) -> usize {
        match self.props.num_block_threads_reduced > 1 {
            false => 1,
            true => match self.block_geometry() {
                Err(_) => 1,
                Ok((block_size, out_lim)) => {
                    let threads = self.props.num_block_threads_reduced.max(1) as usize;
                    // Saturating rather than checked: `block_size + out_lim` is at
                    // most ~512 MiB, so the sum cannot wrap on any target that got
                    // this far.
                    let per_block = (block_size as u64).saturating_add(out_lim as u64);
                    let by_memory = (self.mt_memory_budget / per_block.max(1)).max(1);
                    threads.min(by_memory.min(usize::MAX as u64) as usize)
                }
            },
        }
    }

    /// `Lzma2Enc_Encode2`'s two block-size derivations (`Lzma2Enc.c:770-781`): the
    /// block size as a `size_t`, and `destBlockSize`, the per-block output buffer
    /// that becomes `outLim`.
    fn block_geometry(&self) -> Result<(usize, usize), Lzma2Error> {
        // Lzma2Enc.c:770-772.
        let block_size = match usize::try_from(self.props.block_size) {
            Ok(v) => v,
            Err(_) => return Err(Lzma2Error::LzmaParam),
        };
        // Normalization floors a real block size at 1 MiB (`Lzma2Enc.c:314`, `:318`),
        // so zero means the caller reached here with props this crate did not
        // normalize. Refuse rather than loop forever reading nothing.
        if block_size == 0 {
            return Err(Lzma2Error::LzmaParam);
        }
        // Lzma2Enc.c:775-777.
        let out_lim = match block_size
            .checked_add(block_size >> 10)
            .and_then(|v| v.checked_add(16))
        {
            Some(v) => v,
            None => return Err(Lzma2Error::LzmaParam),
        };
        Ok((block_size, out_lim))
    }

    /// `Lzma2Enc_WriteProperties` (`Lzma2Enc.c:485`) — the single LZMA2 property
    /// byte, which DArc writes ahead of the stream (`C_LZMA2.cpp:97`).
    ///
    /// It encodes the dictionary size only, as the smallest `i` with
    /// `dicSize <= (2 | (i & 1)) << (i / 2 + 11)`. `lc`/`lp`/`pb` are **not** in it —
    /// they travel in the per-chunk `propsByte` instead.
    pub fn props_byte(&self) -> u8 {
        let dic_size = self.props.lzma.dict_size_normalized();
        let mut i = 0u32;
        while i < 40 {
            if dic_size <= dic_size_from_prop(i) {
                break;
            }
            i += 1;
        }
        i as u8
    }

    /// The LZMA properties the inner encoder runs with, after all normalization and
    /// clamping. Exposed for tests and for callers that need the decoder props.
    pub fn lzma_props(&self) -> &LzmaProps {
        &self.lzma
    }

    /// `Lzma2Enc_Encode2` (`Lzma2Enc.c:716`) with `outBuf == NULL`,
    /// `inStream != NULL` — which is exactly how `C_LZMA2.cpp:101` calls it.
    ///
    /// Dispatches on `numBlockThreads_Reduced` exactly as `Lzma2Enc.c:739` does: at
    /// most one block thread runs [`Lzma2Enc::encode_mt1`], more than one runs the
    /// `MtCoder`-shaped path in [`crate::lzma2_mt`].
    ///
    /// The LZMA2 property byte is **not** written here; `Lzma2Enc_WriteProperties`
    /// is a separate call in the C and DArc writes its result itself.
    pub fn encode_stream(
        &self,
        source: &mut dyn InStream,
        sink: &mut dyn OutStream,
    ) -> Result<(), Lzma2Error> {
        match self.props.num_block_threads_reduced > 1 {
            true => {
                let (block_size, out_lim) = self.block_geometry()?;
                crate::lzma2_mt::encode_blocks_in_order(
                    self,
                    source,
                    sink,
                    block_size,
                    out_lim,
                    self.mt_blocks_in_flight(),
                )
            }
            false => self.encode_mt1(source, sink),
        }
    }

    /// `Lzma2Enc_EncodeMt1` (`Lzma2Enc.c:497`) with `outBuf == NULL`,
    /// `inStream != NULL`, `finished == True`.
    ///
    /// The outer `for(;;)` at `:545` is a *block* loop; with
    /// `blockSize == SOLID` the limited stream's limit is the sentinel that
    /// `LimitedSeqInStream_Read` (`:62`) reads as "no limit", so it runs exactly
    /// once. With a real block size it runs once per block, and each iteration
    /// re-prepares the LZMA encoder — a fresh dictionary, `nowPos64 = 0` and
    /// `reps = [1,1,1,1]` — which is what makes every block independently decodable.
    fn encode_mt1(
        &self,
        source: &mut dyn InStream,
        sink: &mut dyn OutStream,
    ) -> Result<(), Lzma2Error> {
        let block_size = self.props.block_size;
        // Lzma2Enc.c:557-564. `me->expectedDataSize` is the sentinel because DArc
        // never calls `Lzma2Enc_SetDataSize` (`C_LZMA2.cpp` has no such call), so
        // `expected` starts at "unknown" and is then clamped to the block size for
        // every block of a non-SOLID stream -- including the short last one, which
        // is where this path parts company with the memory path.
        let expected = match block_size {
            BLOCK_SIZE_SOLID => u64::MAX,
            _ => block_size,
        };

        let mut limited = LimitedIn {
            real: source,
            limit: block_size,
            processed: 0,
            finished: false,
        };

        // `me->tempBufLzma`, sized LZMA2_CHUNK_SIZE_COMPRESSED_MAX (Lzma2Enc.c:534).
        let mut chunk: Vec<u8> = Vec::with_capacity(CHUNK_SIZE_COMPRESSED_MAX);

        loop {
            // Lzma2EncInt_InitBlock (Lzma2Enc.c:106, called at :550).
            let mut st = Lzma2EncInt {
                props_byte: self.props_byte,
                need_init_state: true,
                need_init_prop: true,
                src_pos: 0,
            };
            // LimitedSeqInStream_Init (:49) then `limit = blockSize` (:552-553).
            limited.limit = block_size;
            limited.processed = 0;
            limited.finished = false;

            let res = {
                // LzmaEnc_SetDataSize (:566) then LzmaEnc_PrepareForLzma2 (:568).
                let mut enc = Encoder::new_for_lzma2_sized(
                    &mut limited,
                    &self.lzma,
                    KEEP_WINDOW_SIZE,
                    expected,
                );
                enc.init();
                enc.init_prices();

                // The inner `for(;;)` at Lzma2Enc.c:590.
                loop {
                    match encode_subblock(
                        &mut st,
                        &mut enc,
                        &mut chunk,
                        CHUNK_SIZE_COMPRESSED_MAX,
                        sink,
                        OutMode::Stream,
                    ) {
                        Err(e) => break Err(e),
                        // `if (packSize == 0) break;` (Lzma2Enc.c:616).
                        Ok(0) => break Ok(()),
                        Ok(_) => {}
                    }
                }
                // LzmaEnc_Finish (LzmaEnc.c:2900) releases the MT match finder's
                // stream and is a no-op single-threaded; dropping `enc` is the
                // equivalent.
            };
            res?;

            // Lzma2Enc.c:626-627. A debug assertion for the message, and the C's own
            // runtime check so that release builds refuse rather than emit a stream
            // whose chunk lengths do not add up to the input.
            debug_assert_eq!(
                st.src_pos, limited.processed,
                "block consumed {} bytes but the limited stream delivered {}",
                st.src_pos, limited.processed
            );
            if st.src_pos != limited.processed {
                return Err(Lzma2Error::Fail);
            }

            // Lzma2Enc.c:629. With SOLID this always holds -- the limit is the "no
            // limit" sentinel, so the only way the inner loop ended is end-of-input.
            // With a real block size it is the loop's exit test.
            if limited.finished {
                break;
            }
        }

        // Lzma2Enc.c:643-645, the `finished` (outBuf == NULL) arm. Once, after the
        // last block, not per block.
        sink.write(&[CONTROL_EOF]).map_err(Lzma2Error::Stream)
    }

    /// One block of the `MtCoder` path: `Lzma2Enc_MtCallback_Code`
    /// (`Lzma2Enc.c:657`) → `Lzma2Enc_EncodeMt1` with `inData`/`outBuf` and
    /// `finished` left to the caller, since the terminator is written once for the
    /// whole stream rather than per block.
    ///
    /// Returns the block's compressed bytes, which the caller must emit in block
    /// order — `Lzma2Enc_MtCallback_Write` (`:695`) is called by `MtCoder` in index
    /// order, and that ordering is the entire difference between this and a
    /// single-threaded encode.
    pub(crate) fn encode_one_block(
        &self,
        data: &[u8],
        out_lim: usize,
    ) -> Result<Vec<u8>, Lzma2Error> {
        let mut st = Lzma2EncInt {
            props_byte: self.props_byte,
            need_init_state: true,
            need_init_prop: true,
            src_pos: 0,
        };
        let mut src = SliceIn::new(data);
        // `me->outBufs[outBufIndex]`, `me->outBufSize` bytes (Lzma2Enc.c:665-675).
        // A Vec rather than a fixed buffer: `outLim` is the *limit*, and the C's
        // buffer is only ever written up to `packTotal`.
        let mut out = VecOut {
            data: Vec::with_capacity(data.len() / 3 + 64),
        };
        let mut chunk: Vec<u8> = Vec::with_capacity(CHUNK_SIZE_COMPRESSED_MAX);

        let res = {
            // LzmaEnc_MemPrepare (Lzma2Enc.c:583) -- and this is where the block's
            // own length becomes `expectedDataSize` (LzmaEnc.c:2896). The C also
            // switches the match finder to `directInput` here; that is invisible in
            // the output (the window is a copy of the same bytes) and is not
            // reproduced.
            let mut enc = Encoder::new_for_lzma2_sized(
                &mut src,
                &self.lzma,
                KEEP_WINDOW_SIZE,
                data.len() as u64,
            );
            enc.init();
            enc.init_prices();

            loop {
                // Lzma2Enc.c:592-594: on the outBuf path the limit is what is left of
                // the block's output buffer, so it shrinks as the block fills.
                let limit = out_lim.saturating_sub(out.data.len());
                match encode_subblock(
                    &mut st,
                    &mut enc,
                    &mut chunk,
                    limit,
                    &mut out,
                    OutMode::Memory,
                ) {
                    Err(e) => break Err(e),
                    Ok(0) => break Ok(()),
                    Ok(_) => {}
                }
            }
        };
        res?;

        // Lzma2Enc.c:626-627, with `inSizeCur` on the right-hand side because this is
        // the `inData` arm.
        if st.src_pos != data.len() as u64 {
            return Err(Lzma2Error::Fail);
        }
        Ok(out.data)
    }
}

/// Encode `source` to `sink` as a bare LZMA2 stream — no property byte.
///
/// The [`Lzma2Enc::encode_stream`] shape as a free function, mirroring
/// [`crate::encode_stream`]. Use [`Lzma2Enc::props_byte`] if you need the property
/// byte separately, or [`compress_stream`] for DArc's full wire format.
pub fn encode_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    props: &Lzma2EncProps,
) -> Result<(), Lzma2Error> {
    Lzma2Enc::new(props)?.encode_stream(source, sink)
}

/// The whole of DArc's `lzma2_compress` (`C_LZMA2.cpp:47-107`): the single LZMA2
/// property byte, then the stream.
///
/// The property byte is part of DArc's wire format, not of LZMA2 — `C_LZMA2.cpp:97`
/// writes it with its own callback before calling `Lzma2Enc_Encode2`. Anything
/// comparing this crate against DArc's C wants this function, not
/// [`encode_stream`].
pub fn compress_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    props: &Lzma2EncProps,
) -> Result<(), Lzma2Error> {
    let enc = Lzma2Enc::new(props)?;
    sink.write(&[enc.props_byte()]).map_err(Lzma2Error::Stream)?;
    enc.encode_stream(source, sink)
}

/// `LzmaEnc_SetProps`' validation and clamping (`LzmaEnc.c:532-585`), reduced to the
/// fields [`LzmaProps`] carries.
//
// `manual_clamp` would fold the `fb` floor and ceiling into `.clamp()`. Kept as the
// C's two sequential `if`s so the transcription stays line-for-line checkable.
#[allow(clippy::manual_clamp)]
fn resolve_lzma_props(raw: &RawLzmaProps) -> Result<LzmaProps, Lzma2Error> {
    let mut p = *raw;
    p.normalize();

    // LzmaEnc.c:537-540.
    if p.lc > 8 || p.lp > 4 || p.pb > 4 {
        return Err(Lzma2Error::LzmaParam);
    }
    // LzmaEnc.c:544-545. (The kDicLogSizeMaxCompress test at :549-553 compares a
    // 64-bit widening of a u32 against 1 << 33 and can never fire.)
    let mut dict_size = p.dict_size;
    if dict_size > K_LZMA_MAX_HISTORY_SIZE {
        dict_size = K_LZMA_MAX_HISTORY_SIZE;
    }
    // LzmaEnc.c:557-563.
    let mut fb = p.fb as u32;
    if fb < 5 {
        fb = 5;
    }
    if fb > MATCH_LEN_MAX {
        fb = MATCH_LEN_MAX;
    }

    // LzmaEnc.c:572-582, then MatchFinder_CreateVTable (LzFind.c:1663).
    let bt = p.bt_mode != 0;
    let nhb = p.num_hash_bytes;
    let mf = match (bt, nhb) {
        // btMode with numHashBytes >= 5 resolves to Bt5, which is unported. It is
        // checked FIRST because the C's `if (numHashBytes >= 5)` overrides the
        // btMode ladder above it.
        (true, n) if n >= 5 => return Err(Lzma2Error::Bt5MatchFinder),
        (true, n) if n < 2 => MatchFinderKind::Bt2,
        (true, 2) => MatchFinderKind::Bt2,
        (true, 3) => MatchFinderKind::Bt3,
        (true, _) => MatchFinderKind::Bt4,
        (false, n) if n >= 5 => MatchFinderKind::Hc5,
        (false, _) => MatchFinderKind::Hc4,
    };

    // LzmaEnc.c:2695: mtMode = multiThread && !fastMode && btMode. The MT match
    // finder emits a different parse and is not ported.
    let fast_mode = p.algo == 0;
    if p.num_threads > 1 && !fast_mode && bt {
        return Err(Lzma2Error::MultiThreadedMatchFinder);
    }

    Ok(LzmaProps {
        lc: p.lc as u8,
        lp: p.lp as u8,
        pb: p.pb as u8,
        dict_size,
        fb,
        mc: p.mc,
        mf,
        fast_mode,
        // LzmaEnc.c:2967 clears it anyway; setting it false here means the value is
        // never even momentarily wrong.
        write_end_mark: false,
    })
}

// ---------------------------------------------------------------------------
// Per-block state and the subblock coder
// ---------------------------------------------------------------------------

/// `CLzma2EncInt` (`Lzma2Enc.c:81`), minus the fields that are constant here
/// (`enc`, `propsAreSet`).
struct Lzma2EncInt {
    props_byte: u8,
    need_init_state: bool,
    need_init_prop: bool,
    src_pos: u64,
}

/// `CLimitedSeqInStream` (`Lzma2Enc.c:40`).
///
/// With `blockSize == SOLID` the limit is `u64::MAX`, the same sentinel the C tests
/// against, so this is a pass-through that only counts. The count is not
/// decoration: it is the right-hand side of the `srcPos == processed` invariant.
struct LimitedIn<'a> {
    real: &'a mut dyn InStream,
    limit: u64,
    processed: u64,
    finished: bool,
}

impl InStream for LimitedIn<'_> {
    /// `LimitedSeqInStream_Read` (`Lzma2Enc.c:56`).
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
        let mut size2 = buf.len();
        if self.limit != u64::MAX {
            let rem = self.limit - self.processed;
            if size2 as u64 > rem {
                size2 = rem as usize;
            }
        }
        if size2 == 0 {
            // The C leaves `finished` alone here: a zero-length request says nothing
            // about the underlying stream.
            return Ok(0);
        }
        let n = self.real.read(&mut buf[..size2])?;
        self.finished = n == 0;
        self.processed += n as u64;
        Ok(n)
    }
}

/// Which of `Lzma2EncInt_EncodeSubblock`'s two output disciplines to follow.
///
/// The C selects between them with `outStream != NULL` at three points
/// (`Lzma2Enc.c:171`, `:181-189`). They agree on every byte emitted; they disagree
/// on when the *limit* is consulted, and a block whose output buffer is nearly full
/// takes the copy path in one and not the other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutMode {
    /// `outStream != NULL`: each copy chunk is written as it is produced and
    /// `destPos` restarts, so the limit is tested against a single chunk
    /// (`Lzma2Enc.c:186`).
    Stream,
    /// `outBuf != NULL`: the block accumulates and `destPos` does not restart, so the
    /// limit is tested against everything this subblock has produced
    /// (`Lzma2Enc.c:189`).
    Memory,
}

/// `Lzma2EncInt_EncodeSubblock` (`Lzma2Enc.c:129`).
///
/// Returns `*packSizeRes` — the bytes written for this subblock, `0` meaning the
/// block is exhausted and the caller's loop should stop.
fn encode_subblock(
    st: &mut Lzma2EncInt,
    enc: &mut Encoder<'_>,
    chunk: &mut Vec<u8>,
    pack_size_limit: usize,
    sink: &mut dyn OutStream,
    mode: OutMode,
) -> Result<usize, Lzma2Error> {
    let lz_header_size = 5 + usize::from(st.need_init_prop);
    if pack_size_limit < lz_header_size {
        return Err(Lzma2Error::OutputEof);
    }
    // The C hands LzmaEnc `outBuf + lzHeaderSize` with this much room, reserving
    // the header in place. Here the header is prepended afterwards, which is the
    // same bytes in the same single write.
    let pack_capacity = pack_size_limit - lz_header_size;

    enc.save_state();
    let out = enc.code_one_mem_block(
        st.need_init_state,
        pack_capacity,
        PACK_SIZE_MAX,
        UNPACK_SIZE_MAX,
    );
    let unpack_size = out.unpack_size;
    let pack_size = enc.mem_block_payload().len();

    // LzmaEnc_CodeOneMemBlock's return value: overflow outranks res (LzmaEnc.c:2984).
    let res: Result<(), Lzma2Error> = match out.overflow {
        true => Err(Lzma2Error::OutputEof),
        false => out.res.map_err(Lzma2Error::Stream),
    };

    // Lzma2Enc.c:150. Note this returns `res`, not success: a read error on the
    // last chunk must not be laundered into a clean end of block.
    if unpack_size == 0 {
        return res.map(|()| 0usize);
    }

    // Lzma2Enc.c:153-161.
    let use_copy_block = match res {
        Ok(()) => pack_size + 2 >= unpack_size as usize || pack_size > (1 << 16),
        Err(Lzma2Error::OutputEof) => true,
        Err(e) => return Err(e),
    };

    if use_copy_block {
        // Lzma2Enc.c:163-195.
        let mut remaining = unpack_size;
        let mut pack_size_res = 0usize;
        // `size_t destPos` (Lzma2Enc.c:165). On the stream path it is reset after
        // every write (:186) so it is always 0 at the test; on the memory path it
        // accumulates (:189).
        let mut dest_pos = 0usize;
        while remaining > 0 {
            let u = remaining.min(COPY_CHUNK_SIZE);
            // `if (packSizeLimit - destPos < u + 3)` (Lzma2Enc.c:171). Saturating
            // where the C wraps: `destPos` can never exceed `packSizeLimit`, because
            // this very test ran before each increment, so the two agree -- and a
            // saturating subtraction refuses where a wrapped one would sail past.
            if pack_size_limit.saturating_sub(dest_pos) < u as usize + 3 {
                return Err(Lzma2Error::OutputEof);
            }
            chunk.clear();
            // Only the FIRST copy chunk of a block resets the dictionary, and the
            // test is on srcPos rather than on a "first iteration" flag -- a copy
            // chunk that follows an LZMA chunk must not reset it.
            chunk.push(match st.src_pos {
                0 => CONTROL_COPY_RESET_DIC,
                _ => CONTROL_COPY_NO_RESET,
            });
            chunk.push(((u - 1) >> 8) as u8);
            chunk.push((u - 1) as u8);
            // `LzmaEnc_GetCurBuf(p->enc) - unpackSize`, where `unpackSize` is the
            // *remaining* count, so the source walks forward across iterations.
            match enc.cur_buf_behind(remaining as usize, u as usize) {
                Some(src) => chunk.extend_from_slice(src),
                None => return Err(Lzma2Error::WindowTooNarrow),
            }
            remaining -= u;
            st.src_pos += u64::from(u);
            dest_pos += chunk.len();
            sink.write(chunk).map_err(Lzma2Error::Stream)?;
            match mode {
                // `*packSizeRes += destPos; destPos = 0;` (Lzma2Enc.c:183-186).
                OutMode::Stream => {
                    pack_size_res += dest_pos;
                    dest_pos = 0;
                }
                // `*packSizeRes = destPos;` (Lzma2Enc.c:189).
                OutMode::Memory => pack_size_res = dest_pos,
            }
        }

        // Lzma2Enc.c:193. The probability model is rolled back to before the
        // attempt, because the decoder will not have seen those symbols. Note what
        // is NOT done here: `needInitState` is left alone on purpose (the
        // assignment at :190 is commented out), and `nowPos64`, the price tables and
        // the match finder all keep moving forward.
        enc.restore_state();
        return Ok(pack_size_res);
    }

    // Lzma2Enc.c:197-225 -- the LZMA chunk.
    let u = unpack_size - 1;
    let pm = (pack_size - 1) as u32;
    let mode: u8 = match st.src_pos {
        0 => 3,
        _ => match (st.need_init_state, st.need_init_prop) {
            (true, true) => 2,
            (true, false) => 1,
            (false, _) => 0,
        },
    };
    // Mode 1 means "reset state, reuse props". `needInitState` is set only by
    // `Lzma2EncInt_InitBlock` (`Lzma2Enc.c:106-111`), which sets `needInitProp`
    // alongside it, and the only place either is cleared clears BOTH (`:214-215`).
    // So `needInitState && !needInitProp` cannot arise and mode 1 is unreachable.
    //
    // Mode 2, by contrast, IS reachable and must not be asserted away: a copy chunk
    // advances `srcPos` without clearing `needInitState` -- `Lzma2Enc.c:190`'s
    // `/* needInitState = True; */` is commented out precisely so that it does not --
    // so an incompressible prefix followed by compressible data emits mode 2 for the
    // first LZMA chunk. `a_copy_chunk_before_the_first_lzma_chunk_produces_mode_two`
    // is that case, and the independent differential harness observes it too.
    debug_assert_ne!(mode, 1, "mode 1 requires needInitState without needInitProp");

    chunk.clear();
    chunk.push(CONTROL_LZMA | (mode << 5) | (((u >> 16) & 0x1F) as u8));
    chunk.push((u >> 8) as u8);
    chunk.push(u as u8);
    chunk.push((pm >> 8) as u8);
    chunk.push(pm as u8);
    if st.need_init_prop {
        chunk.push(st.props_byte);
    }
    debug_assert_eq!(chunk.len(), lz_header_size);

    st.need_init_prop = false;
    st.need_init_state = false;
    chunk.extend_from_slice(enc.mem_block_payload());
    st.src_pos += u64::from(unpack_size);

    sink.write(chunk).map_err(Lzma2Error::Stream)?;
    Ok(chunk.len())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stream::{SliceIn, VecOut};

    /// The properties DArc's `LZMA2_METHOD` defaults to (`C_LZMA2.cpp:201-208`),
    /// with `numTotalThreads` forced to 1 so the configuration is the one this port
    /// implements.
    fn darc_default_props(threads: i32) -> Lzma2EncProps {
        let mut p = Lzma2EncProps::init();
        p.lzma.dict_size = 64 << 20;
        p.lzma.algo = 1;
        p.lzma.fb = 32;
        // matchFinder = 4 (kHT4) -> btMode 0, numHashBytes 5 -> Hc5.
        p.lzma.bt_mode = 0;
        p.lzma.num_hash_bytes = 5;
        p.lzma.mc = 0;
        p.lzma.pb = 2;
        p.lzma.lc = 3;
        p.lzma.lp = 0;
        p.lzma.write_end_mark = false;
        p.num_total_threads = threads;
        p.num_block_threads_max = threads;
        p
    }

    /// The claim the whole scope of this module rests on, checked rather than
    /// assumed: DArc's configuration resolves to SOLID **only** at one thread. At
    /// more, `Lzma2EncProps_Normalize` computes a real block size and the MT coder
    /// takes over — so refusing is the correct behaviour, not a gap to paper over.
    #[test]
    fn darc_resolves_to_solid_only_at_one_thread() {
        let mut one = darc_default_props(1);
        one.normalize();
        assert_eq!(one.block_size, BLOCK_SIZE_SOLID);
        assert_eq!(one.num_block_threads_reduced, 1);
        assert_eq!(one.lzma.num_threads, 1);

        let mut many = darc_default_props(8);
        many.normalize();
        assert_ne!(many.block_size, BLOCK_SIZE_SOLID);
        assert!(many.num_block_threads_reduced > 1);
        assert_eq!(
            Lzma2Enc::new(&darc_default_props(8)).err(),
            Some(Lzma2Error::MultiThreadedBlocks)
        );
    }

    #[test]
    fn refuses_lc_plus_lp_above_four() {
        let mut p = darc_default_props(1);
        p.lzma.lc = 3;
        p.lzma.lp = 2;
        assert_eq!(Lzma2Enc::new(&p).err(), Some(Lzma2Error::LcLpTooLarge));
        // Plain LZMA allows this pair; LZMA2 does not (Lzma2Enc.c:470).
        p.lzma.lc = 4;
        p.lzma.lp = 0;
        assert!(Lzma2Enc::new(&p).is_ok(), "lc=4, lp=0 sums to 4 and is allowed");
    }

    #[test]
    fn refuses_the_unported_bt5_finder() {
        let mut p = darc_default_props(1);
        p.lzma.bt_mode = 1;
        p.lzma.num_hash_bytes = 5;
        // numThreads would also be forced to 1 by the SOLID normalization, so this
        // reaches the finder check rather than the MT one.
        assert_eq!(Lzma2Enc::new(&p).err(), Some(Lzma2Error::Bt5MatchFinder));
    }

    /// `Lzma2Enc_WriteProperties` (`Lzma2Enc.c:485`). The expected values are
    /// computed from `LZMA2_DIC_SIZE_FROM_PROP` here rather than transcribed, and
    /// the anchors below are the ones a table would get wrong.
    #[test]
    fn props_byte_encodes_the_dictionary_size() {
        for (dict, expect) in [
            (4096u32, 0u8),
            (1 << 16, 8),
            (1 << 20, 16),
            (3 << 19, 17),
            (1 << 21, 18),
            (64 << 20, 28),
            (3 << 30, 39),
        ] {
            let mut p = darc_default_props(1);
            p.lzma.dict_size = dict;
            let enc = match Lzma2Enc::new(&p) {
                Ok(e) => e,
                Err(e) => panic!("{dict} rejected: {e:?}"),
            };
            assert_eq!(enc.props_byte(), expect, "dict {dict}");
            // And the byte round-trips: the encoded size is the smallest one that
            // still covers the dictionary.
            let i = u32::from(enc.props_byte());
            assert!(dic_size_from_prop(i) >= dict, "dict {dict} under-covered");
            assert!(i == 0 || dic_size_from_prop(i - 1) < dict, "dict {dict} not minimal");
        }
    }

    /// The chunk header of the very first chunk: control byte, sizes, props byte.
    /// This is where an off-by-one in `u`/`pm` or a wrong `mode` shift shows up.
    #[test]
    fn first_chunk_is_mode_three_and_the_sizes_are_minus_one() {
        let data = b"the quick brown fox jumps over the lazy dog. ".repeat(64);
        let mut p = darc_default_props(1);
        p.lzma.dict_size = 1 << 16;
        let enc = match Lzma2Enc::new(&p) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let mut src = SliceIn::new(&data);
        let mut sink = VecOut::default();
        assert_eq!(enc.encode_stream(&mut src, &mut sink), Ok(()));
        let out = sink.data;

        let control = out[0];
        assert_eq!(control & 0x80, CONTROL_LZMA, "not an LZMA chunk");
        assert_eq!((control >> 5) & 3, 3, "the first chunk must be mode 3");

        let unpack = (((control & 0x1F) as usize) << 16 | (out[1] as usize) << 8 | out[2] as usize) + 1;
        assert_eq!(unpack, data.len(), "unpack size must be the whole input");
        let pack = ((out[3] as usize) << 8 | out[4] as usize) + 1;
        assert_eq!(
            out[5],
            enc.lzma_props().decoder_props()[0],
            "mode 3 carries LzmaEnc_WriteProperties' byte 0, not the LZMA2 prop byte"
        );
        // header (6) + payload + the EOF terminator.
        assert_eq!(out.len(), 6 + pack + 1);
        assert_eq!(out[out.len() - 1], CONTROL_EOF);
        // Every chunk payload starts with the range coder's initial cache byte.
        assert_eq!(out[6], 0x00);
    }

    /// Input larger than `LZMA2_UNPACK_SIZE_MAX` must come out as several chunks,
    /// the first mode 3 and the rest mode 0 — and the unpack sizes must add up.
    #[test]
    fn multiple_chunks_continue_without_resetting_state() {
        // 5 MiB of mildly compressible data: big enough to force the 2 MiB unpack
        // cap, structured enough that no chunk falls back to a copy.
        let mut data = Vec::with_capacity(5 << 20);
        let mut s: u32 = 12345;
        while data.len() < (5 << 20) {
            s = s.wrapping_mul(1664525).wrapping_add(1013904223);
            let n = 4 + (s >> 28) as usize;
            let b = (s >> 20) as u8;
            data.extend(std::iter::repeat_n(b, n));
        }
        let mut p = darc_default_props(1);
        p.lzma.dict_size = 1 << 20;
        let enc = match Lzma2Enc::new(&p) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let mut src = SliceIn::new(&data);
        let mut sink = VecOut::default();
        assert_eq!(enc.encode_stream(&mut src, &mut sink), Ok(()));

        let controls = walk_chunks(&sink.data, &data);
        assert!(
            controls.len() > 1,
            "only {} chunk(s); the 2 MiB cap never bound",
            controls.len()
        );
        for (i, c) in controls.iter().enumerate() {
            assert_eq!(c & 0x80, CONTROL_LZMA, "chunk {i} is not an LZMA chunk");
        }
        assert_eq!((controls[0] >> 5) & 3, 3, "first chunk must reset dictionary and state");
        for (i, c) in controls.iter().enumerate().skip(1) {
            assert_eq!((c >> 5) & 3, 0, "chunk {i} unexpectedly reset state");
        }
    }

    /// Incompressible input takes the copy path, and the first copy chunk is the one
    /// that resets the dictionary.
    #[test]
    fn incompressible_input_takes_the_copy_path() {
        let data = noise(300_000, 0x1234_5678);
        let mut p = darc_default_props(1);
        p.lzma.dict_size = 1 << 16;
        let enc = match Lzma2Enc::new(&p) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let mut src = SliceIn::new(&data);
        let mut sink = VecOut::default();
        assert_eq!(enc.encode_stream(&mut src, &mut sink), Ok(()));

        // walk_chunks checks every copy payload against `data` at the right offset,
        // so a wrong `GetCurBuf - unpackSize` anchor fails here rather than silently
        // producing an archive that decodes to the wrong bytes.
        let controls = walk_chunks(&sink.data, &data);
        assert_eq!(
            controls[0], CONTROL_COPY_RESET_DIC,
            "the first copy chunk must reset the dictionary"
        );
        // Several *subblocks*, each producing one copy chunk. Note this does not
        // exercise the inner 64 KiB split at `Lzma2Enc.c:168-191`: the bounded break
        // caps a subblock's `packSize` near 49157, and `useCopyBlock` then needs
        // `packSize + 2 >= unpackSize`, so `unpackSize` can never reach
        // `LZMA2_COPY_CHUNK_SIZE`. That loop is dead when driven from
        // `lzma2_compress` -- ported faithfully anyway, but not constructible from
        // the encoder side, so no test tries.
        assert!(controls.len() > 1, "only one subblock; the copy path ran once");
    }

    /// **Mode 2 is reachable**, and this is the case that produces it: a copy chunk
    /// advances `srcPos` without clearing `needInitState` (`Lzma2Enc.c:190` is
    /// commented out on purpose), so the first *compressed* chunk after an
    /// incompressible prefix has `srcPos != 0`, `needInitState` and `needInitProp`
    /// all true — mode 2, carrying a fresh props byte.
    ///
    /// Recorded as a test because the natural reading of `Lzma2EncInt_InitBlock` is
    /// that only modes 3 and 0 can occur; asserting that would make this input
    /// panic.
    #[test]
    fn a_copy_chunk_before_the_first_lzma_chunk_produces_mode_two() {
        let mut data = noise(300_000, 0xC0FF_EE01);
        data.extend(b"the quick brown fox jumps over the lazy dog. ".repeat(12_000));
        let mut p = darc_default_props(1);
        p.lzma.dict_size = 1 << 20;
        let enc = match Lzma2Enc::new(&p) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let mut src = SliceIn::new(&data);
        let mut sink = VecOut::default();
        assert_eq!(enc.encode_stream(&mut src, &mut sink), Ok(()));

        let controls = walk_chunks(&sink.data, &data);
        assert_eq!(controls[0], CONTROL_COPY_RESET_DIC);
        let first_lzma = match controls.iter().position(|c| c & 0x80 != 0) {
            Some(i) => i,
            None => panic!("no LZMA chunk at all; the compressible tail never landed"),
        };
        assert_eq!(
            (controls[first_lzma] >> 5) & 3,
            2,
            "the first LZMA chunk after a copy prefix must be mode 2"
        );
        for (i, c) in controls.iter().enumerate().skip(first_lzma + 1) {
            assert_eq!(c & 0x80, CONTROL_LZMA, "chunk {i} fell back to copy again");
            assert_eq!((c >> 5) & 3, 0, "chunk {i} reset state again");
        }
    }

    /// Bytes with no exploitable structure: an xorshift32 stream, emitted four bytes
    /// at a time. A multiplicative sequence over consecutive integers looks random
    /// per byte but is not — LZMA compresses it, and a "copy path" test built on one
    /// silently exercises the LZMA path instead.
    fn noise(n: usize, seed: u32) -> Vec<u8> {
        let mut s = seed;
        let mut out = Vec::with_capacity(n + 4);
        while out.len() < n {
            s ^= s << 13;
            s ^= s >> 17;
            s ^= s << 5;
            out.extend_from_slice(&s.to_le_bytes());
        }
        out.truncate(n);
        out
    }

    /// Empty input: no chunks at all, just the terminator.
    #[test]
    fn empty_input_is_a_bare_terminator() {
        let enc = match Lzma2Enc::new(&darc_default_props(1)) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let mut src = SliceIn::new(b"");
        let mut sink = VecOut::default();
        assert_eq!(enc.encode_stream(&mut src, &mut sink), Ok(()));
        assert_eq!(sink.data, vec![CONTROL_EOF]);
    }

    /// A sink error is surfaced with the caller's own code, not swallowed.
    #[test]
    fn sink_errors_reach_the_caller() {
        struct Failing;
        impl OutStream for Failing {
            fn write(&mut self, _: &[u8]) -> Result<(), StreamError> {
                Err(StreamError(-42))
            }
        }
        let enc = match Lzma2Enc::new(&darc_default_props(1)) {
            Ok(e) => e,
            Err(e) => panic!("{e:?}"),
        };
        let data = b"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".repeat(10);
        let mut src = SliceIn::new(&data);
        let mut sink = Failing;
        assert_eq!(
            enc.encode_stream(&mut src, &mut sink),
            Err(Lzma2Error::Stream(StreamError(-42)))
        );
    }

    /// Walk an LZMA2 stream's chunk headers, returning every control byte, and
    /// check the structural invariants on the way through.
    ///
    /// The list is the one the out-of-tree differential harness gates on, so the two
    /// agree by construction rather than by coincidence:
    ///
    /// * **mode 1 never occurs** — `needInitState` without `needInitProp` cannot
    ///   arise (`Lzma2Enc.c:106-111` sets both, `:214-215` clears both);
    /// * **at most one block-init per block**, i.e. at most one chunk with
    ///   `mode >= 2`, and it is the *first* LZMA chunk;
    /// * **at most one dictionary reset per stream** — `mode == 3` or
    ///   `LZMA2_CONTROL_COPY_RESET_DIC`, both gated on `srcPos == 0`; exactly one
    ///   for any non-empty input;
    /// * **every LZMA chunk payload starts with `0x00`**, from the per-chunk
    ///   `RangeEnc_Init` (`LzmaEnc.c:2970`);
    /// * the unpack sizes sum to `data.len()`, the stream ends on exactly one
    ///   terminator, and every chunk respects its size caps;
    /// * every *copy* chunk's payload equals `data` at the offset it claims — the
    ///   check that catches a wrong `LzmaEnc_GetCurBuf - unpackSize` anchor, which
    ///   would otherwise produce a stream that decodes cleanly to the wrong bytes.
    ///
    /// Modes 2 and 3 are *not* asserted here to be reachable; that is a property of
    /// the corpus, and the two tests that establish it name it in their own asserts.
    fn walk_chunks(out: &[u8], data: &[u8]) -> Vec<u8> {
        let mut controls = Vec::new();
        let mut i = 0usize;
        let mut unpacked = 0usize;
        let mut dict_resets = 0usize;
        let mut block_inits = 0usize;
        let mut lzma_chunks = 0usize;
        loop {
            let c = out[i];
            if c == CONTROL_EOF {
                i += 1;
                break;
            }
            controls.push(c);
            if c & 0x80 == 0 {
                assert!(
                    c == CONTROL_COPY_RESET_DIC || c == CONTROL_COPY_NO_RESET,
                    "control byte {c:#04x} is neither a copy control nor an LZMA chunk"
                );
                if c == CONTROL_COPY_RESET_DIC {
                    dict_resets += 1;
                }
                let u = ((out[i + 1] as usize) << 8 | out[i + 2] as usize) + 1;
                assert!(u <= COPY_CHUNK_SIZE as usize, "copy chunk {u} over the cap");
                assert_eq!(
                    &out[i + 3..i + 3 + u],
                    &data[unpacked..unpacked + u],
                    "copy payload at input offset {unpacked} is not the input"
                );
                unpacked += u;
                i += 3 + u;
                continue;
            }
            let mode = (c >> 5) & 3;
            assert_ne!(mode, 1, "mode 1 must be unreachable");
            if mode == 3 {
                dict_resets += 1;
            }
            if mode >= 2 {
                block_inits += 1;
                assert_eq!(
                    lzma_chunks, 0,
                    "a block-init chunk (mode {mode}) followed {lzma_chunks} LZMA chunk(s)"
                );
            }
            lzma_chunks += 1;
            let u = (((c & 0x1F) as usize) << 16 | (out[i + 1] as usize) << 8 | out[i + 2] as usize)
                + 1;
            let pk = ((out[i + 3] as usize) << 8 | out[i + 4] as usize) + 1;
            // The props byte is present exactly when `needInitProp` was set, and
            // that implies mode 2 or 3: mode 0 and 1 both mean it was already
            // cleared (`Lzma2Enc.c:201`, `:211`).
            let hdr = 5 + usize::from(mode >= 2);
            assert!(u <= UNPACK_SIZE_MAX as usize, "chunk unpack size {u} over the cap");
            assert!(pk <= PACK_SIZE_MAX as usize + 16, "chunk pack size {pk} over the cap");
            // Every LZMA chunk payload begins with the range coder's initial cache.
            assert_eq!(out[i + hdr], 0x00, "chunk payload does not start with 0x00");
            unpacked += u;
            i += hdr + pk;
        }
        assert_eq!(i, out.len(), "stream did not end on its terminator");
        assert_eq!(unpacked, data.len(), "chunk unpack sizes do not sum to the input");
        assert!(block_inits <= 1, "{block_inits} block-inits in one SOLID block");
        // Both a mode-3 chunk and a COPY_RESET_DIC chunk are gated on `srcPos == 0`,
        // so with one block there is exactly one -- unless the input was empty, in
        // which case there are no chunks at all.
        let expect_resets = usize::from(!data.is_empty());
        assert_eq!(dict_resets, expect_resets, "wrong number of dictionary resets");
        controls
    }
}
