//! Match finder — a port of `CMatchFinder` (`LzFind.c`), restricted to the
//! single-threaded BT4 (binary-tree, 4-byte hash) configuration DArc uses.
//!
//! ## Streaming
//!
//! This is a **sliding window** over an [`InStream`], not a slice of the whole
//! input. Upstream `lzma-sdk-rs` held the entire input in memory, which is fine for
//! CHD hunks but not for DArc, where one `lzma_compress` call receives a whole
//! solid block. Memory here is O(dictionary), as in the C: a `block_size` window
//! plus the hash and the binary tree.
//!
//! The window is the reason the encoder cannot address bytes by stream position.
//! Every index below is an offset into `buf`, and the mapping from stream position
//! to window index is
//!
//! ```text
//!     buf[buffer_offset + k]  is stream byte  (pos - 1) + k
//! ```
//!
//! `buffer_offset` changes whenever [`MatchFinder::move_block`] slides the window,
//! so **an index captured before a call that advances the finder is stale
//! afterwards**. The C has the same hazard and handles it by recomputing
//! `data = GetPointerToCurrentPos(...) - additionalOffset` after every advance
//! (`LzmaEnc.c:1252`, `:1578`, `:2465`); this port recomputes at exactly those
//! points. Holding an index across an advance is the one mistake here that produces
//! plausible-looking output rather than a crash.
//!
//! ## Bit-exactness hazards (see CLAUDE.md)
//! - `pos` starts at **1** (`MatchFinder_Init_4`); hashes are zeroed (any order,
//!   since all to `kEmptyHashValue = 0`).
//! - `len_limit` comes from `MatchFinder_SetLimits`, not from
//!   `min(match_max_len, avail)` computed at the use site. The two agree, but
//!   `pos_limit` — which decides *when* the window refills — comes from the same
//!   function, so porting it wholesale is cheaper than proving each half.
//! - `cyclic_buffer_size` = `history_size + 1`, `cut_value` = `mc`,
//!   `hash_mask` = `GetHashMask(history_size)`.
//! - Match lists match C's `Bt4_MatchFinder_GetMatches` exactly (increasing
//!   length, closest distance per length).

pub mod bt4;
pub mod hash;
pub mod hc;
mod mt;

use hash::{CRC_SHIFT_2, FIX3_HASH_SIZE, FIX4_HASH_SIZE, FIX5_HASH_SIZE};

use crate::props::{LzmaProps, MatchFinderKind};
use crate::state::MATCH_LEN_MAX;
use crate::stream::{InStream, StreamError};

/// One `(len, dist)` candidate. `dist` is **0-based** (the LZMA encoded distance,
/// i.e. actual back-distance minus 1), matching the C `distances` array.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Match {
    pub len: u32,
    pub dist: u32,
}

const CRC_POLY: u32 = 0xEDB8_8320;

/// `kBlockMoveAlign` — `memmove` alignment in `MatchFinder_MoveBlock`.
const K_BLOCK_MOVE_ALIGN: u32 = 1 << 7;
/// `kBlockSizeAlign` — block allocation granularity.
const K_BLOCK_SIZE_ALIGN: u32 = 1 << 16;
/// `kMaxValForNormalize` (`LzFind.c:19`).
///
/// **Zero**, which means `pos` reaches it only by wrapping 2^32. So normalization
/// runs at 4 GiB of input and never below it — see [`MatchFinder::check_limits`].
const K_MAX_VAL_FOR_NORMALIZE: u32 = 0;
/// The smallest `p->numHashBytes` any supported finder uses, i.e. BT2's.
///
/// `keepSizeAfter` is floored at `numHashBytes` (`LzFind.c:387`); using the minimum
/// is safe because `KEEP_ADD_BUFFER_AFTER` alone is 274, so the floor never binds
/// for any finder. `check_limits` uses the *actual* per-finder value instead, since
/// there the comparison is against available bytes and does decide behaviour.
const NUM_HASH_BYTES_MIN: u32 = 2;
/// `beforeSize = kNumOpts` (`LzmaEnc.c:2688`), the encoder's lookbehind slack on
/// top of the dictionary. Equal to [`crate::optimum::NUM_OPTS`].
///
/// This is only the *floor*. `LzmaEnc_Alloc` widens it when a caller asks for a
/// minimum window (`LzmaEnc.c:2729`):
///
/// ```text
///     if (beforeSize + dictSize < keepWindowSize)
///       beforeSize = keepWindowSize - dictSize;
/// ```
///
/// which is why [`MatchFinder::new`] takes the resolved value as a parameter rather
/// than reading this constant. LZMA passes `keepWindowSize = 0`, so it lands here;
/// LZMA2 passes `LZMA2_KEEP_WINDOW_SIZE = 1 << 21` (`Lzma2Enc.c:30`) and gets a
/// wider lookbehind whenever the dictionary is under 2 MiB.
pub const KEEP_ADD_BUFFER_BEFORE: u32 = crate::optimum::NUM_OPTS as u32;
/// `LZMA_MATCH_LEN_MAX + 1` (`LzmaEnc.c:2752`), the lookahead slack `LzmaEnc`
/// requests on top of `numFastBytes`.
const KEEP_ADD_BUFFER_AFTER: u32 = MATCH_LEN_MAX + 1;

/// Latched when the hash worker stops before the encoder stops asking it for
/// positions — see [`mt`]'s failure section. `SZ_ERROR_THREAD` (`7zTypes.h:40`),
/// so a caller forwarding `SRes` codes stays consistent with the C.
const ERR_HASH_WORKER: StreamError = StreamError(12);

/// `MatchFinder_GetHashMask` (`LzFind.c:347`), all branches.
///
/// Not `MatchFinder_GetHashMask2` (`LzFind.c:321`), which looks nearly identical and
/// is **not** the one on this path: it omits both `hs >>= 1` steps and is reachable
/// only when `numHashOutBits != 0`, which `LzmaEnc` never sets. Using it doubles
/// every mask.
///
/// The per-`numHashBytes` differences are easy to get wrong and hard to catch:
///
/// * **2** returns immediately and never reads `history_size` at all.
/// * **3** *caps* at `(1 << 24) - 1`, where **4 and 5** take a second `>>= 1`. Those
///   two branches agree for every `history_size <= 1 << 26` and first diverge at
///   `1 << 27`, so a small-dictionary test cannot tell them apart.
/// * **5** additionally floors the mask at `(256 << CRC_SHIFT_2) - 1`.
fn get_hash_mask(num_hash_bytes: u32, history_size: u32) -> u32 {
    if num_hash_bytes == 2 {
        // LzFind.c:349 -- returns before the bit propagation, so history_size is
        // irrelevant and the 2-byte table is always exactly 1 << 16 entries.
        return (1 << 16) - 1;
    }
    let mut hs = history_size.saturating_sub(1);
    hs |= hs >> 1;
    hs |= hs >> 2;
    hs |= hs >> 4;
    hs |= hs >> 8;
    hs >>= 1;
    if hs >= (1 << 24) {
        if num_hash_bytes == 3 {
            hs = (1 << 24) - 1; // LzFind.c:362 -- a cap, not a shift
        } else {
            hs >>= 1; // LzFind.c:364
        }
    }
    hs |= (1 << 16) - 1; // "don't change it!" -- required for numHashBytes > 2
    if num_hash_bytes >= 5 {
        hs |= (256 << CRC_SHIFT_2) - 1; // LzFind.c:371
    }
    hs
}

/// `GetBlockSize` (`LzFind.c:286`) — the window allocation, including the C's
/// slide reserve.
///
/// The C also returns 0 to *reject* four cases: `keepSizeBefore` overflowing past
/// `historySize`, `blockSize` overflowing past `keepSizeBefore`,
/// `blockSize >= kBlockSizeMax`, and `rem < kBlockSizeReserveMin`. All four need
/// `history_size` above `kLzmaMaxHistorySize`, which cannot reach here because
/// [`LzmaProps::history_size`] applies the same clamp `LzmaEnc_SetProps` does. They
/// are therefore omitted rather than transliterated as an unreachable error path —
/// `block_size_never_hits_the_c_rejections` sweeps the domain to keep that true.
fn block_size(keep_size_before: u32, keep_size_after: u32) -> u32 {
    let block_size = keep_size_before + keep_size_after;
    let k_block_size_max = 0u32.wrapping_sub(K_BLOCK_SIZE_ALIGN);
    let rem = k_block_size_max - block_size;
    let shift = if block_size < (1u32 << 30) { 1 } else { 2 };
    let reserve = (block_size >> shift) + (1 << 12) + K_BLOCK_MOVE_ALIGN + K_BLOCK_SIZE_ALIGN;
    if reserve >= rem {
        k_block_size_max
    } else {
        (block_size + reserve) & !(K_BLOCK_SIZE_ALIGN - 1)
    }
}

/// `MatchFinder_Normalize3` (`LzFind.c:796`) — subtract `sub_value` from every
/// entry, saturating at `kEmptyHashValue` (0).
///
/// The C's SIMD paths and 128-byte alignment prologue are pure throughput: the
/// values written are `SASUB_32`'s (`LzFind.c:764`) either way.
fn normalize3(sub_value: u32, items: &mut [u32]) {
    for it in items.iter_mut() {
        *it = it.saturating_sub(sub_value);
    }
}

/// BT4 match finder over a sliding window on `stream`.
pub struct MatchFinder<'a> {
    stream: &'a mut dyn InStream,

    // ---- window (CLzInWindow) ----
    /// `bufBase`, exactly `block_size` bytes.
    buf: Vec<u8>,
    block_size: usize,
    /// `buffer - bufBase`: the window index of the current position.
    buffer_offset: usize,
    /// 1-based stream position of the byte the next `get_matches`/`skip` processes.
    pos: u32,
    /// 1-based stream position one past the last byte read into the window.
    stream_pos: u32,
    pos_limit: u32,
    len_limit: u32,
    keep_size_before: u32,
    keep_size_after: u32,
    stream_end_was_reached: bool,
    /// The first error the stream raised, latched as the C latches `p->result`.
    result: Result<(), StreamError>,

    // ---- search structures ----
    /// Which finder's hashing and search to run. Set once; every per-finder
    /// difference below is derived from it rather than from a flag.
    kind: MatchFinderKind,
    /// `p->numHashBytes` for `kind` -- read by `check_limits`, and it gates how many
    /// bytes past `cur` the hash may read.
    num_hash_bytes: u32,
    /// `p->fixedHashSize`: the length of the 2-/3-byte prefix of `hash`, and hence
    /// the base offset of this finder's main table.
    fixed_hash_size: usize,
    history_size: u32,
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    match_max_len: u32,
    cut_value: u32,
    hash_mask: u32,
    /// The hash tables — **empty when [`MatchFinder::mt`] is `Some`**, because the
    /// worker owns them then. Nothing on this side may index it without first
    /// establishing that no worker is running; every such site goes through
    /// `*_heads` below, whose `None` arm is the only place that reads it.
    hash: Vec<u32>,
    son: Vec<u32>,
    crc: [u32; 256],
    /// `kMaxValForNormalize` (`LzFind.c:19`) for this finder.
    ///
    /// A field rather than the constant only so that a test can move it within
    /// reach; [`MatchFinder::new`] and [`MatchFinder::new_mt`] both set it to
    /// [`K_MAX_VAL_FOR_NORMALIZE`], so nothing on a shipped path observes the
    /// difference.
    max_val_for_normalize: u32,
    /// The hash worker, when one is running. `None` is the single-threaded
    /// finder, unchanged.
    mt: Option<mt::MtHash>,
}

impl<'a> MatchFinder<'a> {
    /// Create and initialize the finder, filling the first window from `stream`.
    ///
    /// Mirrors `MatchFinder_Create` + `MatchFinder_Init` with `LzmaEnc`'s arguments
    /// (`LzmaEnc.c:2751`): `matchMaxLen = numFastBytes`,
    /// `keepAddBufferAfter = LZMA_MATCH_LEN_MAX + 1`.
    ///
    /// `keep_add_buffer_before` is `LzmaEnc_Alloc`'s `beforeSize`
    /// (`LzmaEnc.c:2686`, `:2729`) — [`KEEP_ADD_BUFFER_BEFORE`] for plain LZMA, and
    /// widened by LZMA2 so that the *uncompressed-chunk* path can still read up to
    /// 2 MiB behind the cursor after `MoveBlock` has slid the window. It is a
    /// correctness parameter, not a tuning knob: too small and the copy chunk reads
    /// bytes `MoveBlock` has already discarded.
    /// `expected_data_size` is `MFB.expectedDataSize`, which
    /// `MatchFinder_Construct` leaves at `(UInt64)(Int64)-1` (`LzFind.c:245`) and
    /// only `LzmaEnc_SetDataSize` (`LzmaEnc.c:610`) ever moves — from
    /// `LzmaEnc_MemPrepare`'s `srcLen` (`:2896`) on LZMA2's blocked path, and
    /// nowhere else. It is **not** a memory hint: `MatchFinder_Create` narrows the
    /// hash *mask*, not just the allocation, when the declared size is smaller than
    /// the dictionary (`LzFind.c:434-439`), so a smaller expectation genuinely
    /// changes which positions collide and therefore which matches are found. Pass
    /// `u64::MAX` for "unknown", which is what every path but that one declares.
    pub fn new(
        stream: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_add_buffer_before: u32,
        expected_data_size: u64,
    ) -> Self {
        Self::new_inner(
            stream,
            props,
            keep_add_buffer_before,
            expected_data_size,
            1,
            K_MAX_VAL_FOR_NORMALIZE,
        )
    }

    /// As [`MatchFinder::new`], but allowed to run the hash stage on a worker
    /// thread when `num_threads > 1`.
    ///
    /// This is `LzmaEnc.c:2695`'s decision, as a constructor:
    ///
    /// ```text
    ///     mtMode = multiThread && !fastMode && btMode != 0
    /// ```
    ///
    /// with `multiThread = (numThreads > 1)` (`:2694`). All three conjuncts are
    /// re-checked here rather than trusted to the caller, plus one the C does not
    /// need: `fb >= numHashBytes`. That last one is what makes `len_limit <
    /// numHashBytes` mean "the stream has fewer than `numHashBytes` bytes left",
    /// which is the invariant the worker uses to stop at the same position the
    /// consumer stops asking about (see [`mt`]). `LzmaEnc_SetProps` clamps `fb`
    /// into `5..=273` so it always holds in practice; falling back rather than
    /// asserting keeps a caller that skipped that clamp correct instead of hung.
    ///
    /// **The output is identical either way** — that is the whole design
    /// constraint, and `mt_matches_single_threaded_match_for_match` is the gate.
    /// So a caller may pass a thread count straight through from
    /// `GetCompressionThreads()` without it being an archive-format decision.
    pub fn new_mt(
        stream: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_add_buffer_before: u32,
        expected_data_size: u64,
        num_threads: u32,
    ) -> Self {
        Self::new_inner(
            stream,
            props,
            keep_add_buffer_before,
            expected_data_size,
            num_threads,
            K_MAX_VAL_FOR_NORMALIZE,
        )
    }

    fn new_inner(
        stream: &'a mut dyn InStream,
        props: &LzmaProps,
        keep_add_buffer_before: u32,
        expected_data_size: u64,
        num_threads: u32,
        max_val_for_normalize: u32,
    ) -> Self {
        let history_size = props.history_size();
        let cyclic_buffer_size = history_size + 1;
        let num_hash_bytes = props.mf.num_hash_bytes();
        // LzFind.c:432-442. `hs` sizes the table, `hsCur` is the mask actually used;
        // they differ exactly when `expectedDataSize < historySize`. The `> hs` guard
        // is the C's ("is it possible?"), kept rather than reasoned away.
        let hs = get_hash_mask(num_hash_bytes, history_size);
        let hash_mask = match expected_data_size < u64::from(history_size) {
            true => get_hash_mask(num_hash_bytes, expected_data_size as u32).min(hs),
            false => hs,
        };
        // `hashMask + 1 + fixedHashSize` (LzFind.c:444-455): the 2-/3-byte tables
        // form a prefix, and this finder's main table starts after them. Sized
        // exactly, with no padding, because `normalize3` walks the whole Vec and the
        // C walks exactly this span (LzFind.c:860). The C sizes it from `hs`, not
        // from the possibly-narrowed mask, and so does this.
        let fixed_hash_size = props.mf.fixed_hash_size();
        let hash_len = fixed_hash_size + hs as usize + 1;

        // MatchFinder_Create:379 -- "we need one additional byte in keepSizeBefore,
        // since we use MoveBlock() after (p->pos++) and before dictionary using".
        let keep_size_before = history_size + keep_add_buffer_before + 1;
        // keepAddBufferAfter += matchMaxLen, then floored at numHashBytes (which
        // never binds: KEEP_ADD_BUFFER_AFTER alone already exceeds 4).
        let keep_size_after = (KEEP_ADD_BUFFER_AFTER + props.fb).max(NUM_HASH_BYTES_MIN);
        let block_size = block_size(keep_size_before, keep_size_after) as usize;

        let mut crc = [0u32; 256];
        for (i, slot) in crc.iter_mut().enumerate() {
            let mut r = i as u32;
            for _ in 0..8 {
                r = (r >> 1) ^ (CRC_POLY & 0u32.wrapping_sub(r & 1));
            }
            *slot = r;
        }

        // `LzmaEnc.c:2694-2695`, plus the `fb` guard documented on `new_mt`.
        let mt_kind = match num_threads > 1 && !props.fast_mode && props.fb >= num_hash_bytes {
            true => mt::BtKind::of(props.mf),
            false => None,
        };
        let mt = match mt_kind {
            Some(kind) => mt::MtHash::new(mt::HashConfig {
                kind,
                hash_len,
                hash_mask,
                history_size,
                max_val_for_normalize,
                crc,
            }),
            None => None,
        };

        let mut mf = MatchFinder {
            stream,
            buf: vec![0u8; block_size],
            block_size,
            buffer_offset: 0,
            // MatchFinder_Init_4: pos = streamPos = 1, "it's smallest optimal
            // value. do not change it".
            pos: 1,
            stream_pos: 1,
            pos_limit: 0,
            len_limit: 0,
            keep_size_before,
            keep_size_after,
            stream_end_was_reached: false,
            result: Ok(()),
            kind: props.mf,
            num_hash_bytes,
            fixed_hash_size,
            history_size,
            // CYC_TO_POS_OFFSET is 0, so cyclicBufferPos initializes to pos.
            cyclic_buffer_pos: 1,
            cyclic_buffer_size,
            match_max_len: props.fb,
            cut_value: props.mc,
            hash_mask,
            // The worker owns `hash` when there is one, so this side allocates
            // nothing: total hash memory is the same as single-threaded, not
            // double. Every read of it is guarded by `mt.is_none()`.
            hash: match mt.is_some() {
                true => Vec::new(),
                false => vec![0u32; hash_len],
            },
            // `numSons = cyclicBufferSize; if (btMode) numSons <<= 1;`
            // (MatchFinder_Create). The hash chains keep one link per slot, not two,
            // so allocating the tree's size for them would work but doubles memory
            // for nothing -- and reading `son[i*2]` in a chain would be wrong.
            son: vec![
                0u32;
                if props.mf.bt_mode() {
                    2 * cyclic_buffer_size as usize
                } else {
                    cyclic_buffer_size as usize
                }
            ],
            crc,
            max_val_for_normalize,
            mt,
        };
        // MatchFinder_Init: hashes are already zero (kEmptyHashValue), so only the
        // first read and the limits remain.
        mf.read_block();
        mf.set_limits();
        mf
    }

    /// The window buffer. Index it with [`MatchFinder::cur_index`] and offsets from
    /// it — never with a stream position.
    #[inline]
    pub fn win(&self) -> &[u8] {
        &self.buf
    }

    /// `GetPointerToCurrentPos` as a window index.
    #[inline]
    pub fn cur_index(&self) -> usize {
        self.buffer_offset
    }

    /// `len` bytes of already-consumed input ending at window index `end`, or `None`
    /// if that span is not inside the window.
    ///
    /// The C's equivalent is plain pointer arithmetic — `LzmaEnc_GetCurBuf(enc) -
    /// unpackSize` at `Lzma2Enc.c:176`, followed by a `memcpy` of up to 64 KiB. That
    /// only stays inside the allocation because `beforeSize` was widened to
    /// `LZMA2_KEEP_WINDOW_SIZE` at create time; get that wrong and the C reads
    /// whatever `MoveBlock` left behind, silently. Here it is a bounds check with a
    /// `None`, so the same mistake refuses instead of copying rubbish into an
    /// archive.
    #[inline]
    pub fn bytes_ending_at(&self, end: usize, len: usize) -> Option<&[u8]> {
        let start = end.checked_sub(len)?;
        self.buf.get(start..end)
    }

    /// The window allocation, `blockSize` from `GetBlockSize` (`LzFind.c:286`).
    /// Exposed so the LZMA-value regression test can assert on it.
    #[inline]
    pub fn block_size(&self) -> usize {
        self.block_size
    }

    /// Bytes read but not yet consumed (`GET_AVAIL_BYTES`, `LzFind.c:24`).
    ///
    /// Wrapping, as in the C: `streamPos < pos` is explicitly allowed there.
    #[inline]
    pub fn num_available(&self) -> u32 {
        self.stream_pos.wrapping_sub(self.pos)
    }

    /// The stream error, if the input stream raised one. Checked by the encoder
    /// after each block, as `CheckErrors` does (`LzmaEnc.c:2400`).
    #[inline]
    pub fn result(&self) -> Result<(), StreamError> {
        self.result
    }

    /// `MatchFinder_ReadBlock` (`LzFind.c:123`): refill the window's tail.
    //
    // `single_match` would rewrite the three `self.mt` probes as `if let`, which
    // this workspace bans outright.
    #[allow(clippy::single_match)]
    fn read_block(&mut self) {
        if self.stream_end_was_reached || self.result.is_err() {
            return;
        }
        loop {
            let dest = self.buffer_offset + self.num_available() as usize;
            let size = self.block_size - dest;
            if size == 0 {
                // Reachable only if ReadBlock is called before NeedMove/MoveBlock;
                // the C returns here too rather than treating it as end of stream.
                return;
            }
            match self.stream.read(&mut self.buf[dest..dest + size]) {
                Err(e) => {
                    self.result = Err(e);
                    // No more bytes will follow, so let the worker finish rather
                    // than park in `recv` until the finder is dropped.
                    match self.mt.as_mut() {
                        Some(m) => m.end(),
                        None => {}
                    }
                    return;
                }
                Ok(0) => {
                    self.stream_end_was_reached = true;
                    match self.mt.as_mut() {
                        Some(m) => m.end(),
                        None => {}
                    }
                    return;
                }
                Ok(n) => {
                    // The worker's whole view of the input, fed in stream order.
                    // This is the only place bytes enter the window, so it is the
                    // only place they can enter the worker.
                    match self.mt.as_mut() {
                        Some(m) => m.feed(&self.buf[dest..dest + n]),
                        None => {}
                    }
                    self.stream_pos = self.stream_pos.wrapping_add(n as u32);
                    if self.num_available() > self.keep_size_after {
                        return;
                    }
                }
            }
        }
    }

    /// `MatchFinder_NeedMove` (`LzFind.c:208`).
    fn need_move(&self) -> bool {
        if self.stream_end_was_reached || self.result.is_err() {
            return false;
        }
        self.block_size - self.buffer_offset <= self.keep_size_after as usize
    }

    /// `MatchFinder_MoveBlock` (`LzFind.c:191`): slide the retained history and the
    /// unconsumed tail back to the start of the buffer.
    ///
    /// `buffer_offset` must be read before it is written — every term below is in
    /// terms of the *old* offset, which is why the C computes `offset` and
    /// `keepBefore` first.
    fn move_block(&mut self) {
        let align = K_BLOCK_MOVE_ALIGN as usize;
        let offset = self.buffer_offset - self.keep_size_before as usize;
        let keep_before = (offset & (align - 1)) + self.keep_size_before as usize;
        let src = offset & !(align - 1);
        let len = keep_before + self.num_available() as usize;
        self.buf.copy_within(src..src + len, 0);
        self.buffer_offset = keep_before;
    }

    /// `MatchFinder_SetLimits` (`LzFind.c:500`): recompute `len_limit` and the
    /// `pos_limit` at which the window next needs attention.
    fn set_limits(&mut self) {
        let mut n = self.max_val_for_normalize.wrapping_sub(self.pos);
        if n == 0 {
            // "we allow (pos == 0) at start even with (kMaxValForNormalize == 0)"
            n = u32::MAX;
        }
        let k = self.cyclic_buffer_size - self.cyclic_buffer_pos;
        if k < n {
            n = k;
        }

        let mut k = self.num_available();
        {
            let ksa = self.keep_size_after;
            let mut mm = self.match_max_len;
            if k > ksa {
                // Limit exactly to keepSizeAfter, which is what ReadBlock waits for.
                k -= ksa;
            } else if k >= mm {
                k -= mm;
                k += 1;
            } else {
                mm = k;
                if k != 0 {
                    k = 1;
                }
            }
            self.len_limit = mm;
        }
        if k < n {
            n = k;
        }
        self.pos_limit = self.pos + n;
    }

    /// `MatchFinder_CheckLimits` (`LzFind.c:833`). Called only after `pos++`.
    fn check_limits(&mut self) {
        if self.keep_size_after == self.num_available() {
            // Refill only in the exact state the C refills in.
            if self.need_move() {
                self.move_block();
            }
            self.read_block();
        }

        if self.pos == self.max_val_for_normalize && self.num_available() >= self.num_hash_bytes {
            // Unreachable below 4 GiB of input, since kMaxValForNormalize is 0 and
            // `pos` only gets there by wrapping. Ported for parity at that size;
            // `normalize3` itself is covered directly by unit test, and
            // `normalization_is_output_neutral` drives this branch by moving the
            // threshold down.
            let sub_value = self.pos.wrapping_sub(self.history_size).wrapping_sub(1);
            // MatchFinder_REDUCE_OFFSETS (LzFind.h:112).
            self.pos = self.pos.wrapping_sub(sub_value);
            self.stream_pos = self.stream_pos.wrapping_sub(sub_value);
            // A no-op when a worker owns `hash` (this side's copy is empty); the
            // worker runs the identical shift at the identical `pos`, so the two
            // frames move together.
            normalize3(sub_value, &mut self.hash);
            normalize3(sub_value, &mut self.son);
        }

        if self.cyclic_buffer_pos == self.cyclic_buffer_size {
            self.cyclic_buffer_pos = 0;
        }
        self.set_limits();
    }

    /// `MOVE_POS` (`LzFind.c:1091`): advance one byte, refilling at `pos_limit`.
    #[inline]
    fn move_pos(&mut self) {
        self.cyclic_buffer_pos += 1;
        self.buffer_offset += 1;
        self.pos += 1;
        if self.pos == self.pos_limit {
            self.check_limits();
        }
    }

    /// The hash worker stopped before the encoder stopped asking. Latch an error
    /// so the caller refuses the archive.
    ///
    /// The finder keeps answering — with "no candidate", see the `*_heads`
    /// callers — rather than stopping, because it is reached from a C ABI where an
    /// unwind is undefined behaviour. `LzmaEnc`'s `CheckErrors` reads
    /// [`MatchFinder::result`] and fails the encode, which is where this surfaces.
    //
    // `single_match` would rewrite the latch as `if let`, which this workspace
    // bans outright (see the totality gate in `.github/workflows/build.yml`).
    #[allow(clippy::single_match)]
    fn hash_worker_failed(&mut self) {
        match self.result {
            Ok(()) => self.result = Err(ERR_HASH_WORKER),
            Err(_) => {}
        }
    }

    /// `curMatch` for BT2's tree walk.
    ///
    /// Single-threaded this is `Bt2_MatchFinder_GetMatches`'s two `hash` lines
    /// (`LzFind.c:1155-1156`). With a worker it is `BtGetMatches`'s
    /// `pos - p->hashBuf[p->hashBufPos++]` (`LzFindMt.c:672`), turning the
    /// worker's distance back into a position in *this* thread's frame.
    fn bt2_heads(&mut self, cur: usize, pos: u32) -> u32 {
        // `Option<Option<RawHeads>>`: the outer layer is "is a worker running",
        // the inner one is "did it answer". Taken in one step so the borrow of
        // `self.mt` ends before the `None` arm below touches `self.hash`.
        let from_worker = self.mt.as_mut().map(|m| m.next());
        match from_worker {
            Some(Some(r)) => pos.wrapping_sub(r.dm),
            Some(None) => {
                self.hash_worker_failed();
                0
            }
            None => {
                let hv = mt::bt2_index(&self.buf, cur);
                let cur_match = self.hash[hv];
                self.hash[hv] = pos;
                cur_match
            }
        }
    }

    /// `(curMatch, d2)` for BT3 — `Bt3_MatchFinder_GetMatches`'s hash prologue
    /// (`LzFind.c:1183-1189`).
    fn bt3_heads(&mut self, cur: usize, pos: u32) -> (u32, u32) {
        // `Option<Option<RawHeads>>`: the outer layer is "is a worker running",
        // the inner one is "did it answer". Taken in one step so the borrow of
        // `self.mt` ends before the `None` arm below touches `self.hash`.
        let from_worker = self.mt.as_mut().map(|m| m.next());
        match from_worker {
            Some(Some(r)) => (pos.wrapping_sub(r.dm), r.d2),
            Some(None) => {
                self.hash_worker_failed();
                // `d2 = pos` can never satisfy `d2 < mmm` (`mmm <= pos`), and
                // `curMatch = 0` fails `GetMatchesSpec1`'s `cm_check < curMatch`.
                // So this is "no candidate", not a wrong candidate.
                (0, pos)
            }
            None => {
                let (h2i, hvi) = mt::bt3_indices(&self.crc, &self.buf, cur, self.hash_mask);
                let d2 = pos - self.hash[h2i];
                let cur_match = self.hash[hvi];
                self.hash[h2i] = pos;
                self.hash[hvi] = pos;
                (cur_match, d2)
            }
        }
    }

    /// `(curMatch, d2, d3)` for BT4 — `Bt4_MatchFinder_GetMatches`'s hash prologue
    /// (`LzFind.c:1234-1246`).
    fn bt4_heads(&mut self, cur: usize, pos: u32) -> (u32, u32, u32) {
        // `Option<Option<RawHeads>>`: the outer layer is "is a worker running",
        // the inner one is "did it answer". Taken in one step so the borrow of
        // `self.mt` ends before the `None` arm below touches `self.hash`.
        let from_worker = self.mt.as_mut().map(|m| m.next());
        match from_worker {
            Some(Some(r)) => (pos.wrapping_sub(r.dm), r.d2, r.d3),
            Some(None) => {
                self.hash_worker_failed();
                (0, pos, pos)
            }
            None => {
                let (h2i, h3i, h4i) = mt::bt4_indices(&self.crc, &self.buf, cur, self.hash_mask);
                let d2 = pos - self.hash[h2i];
                let d3 = pos - self.hash[h3i];
                let cur_match = self.hash[h4i];
                self.hash[h2i] = pos;
                self.hash[h3i] = pos;
                self.hash[h4i] = pos;
                (cur_match, d2, d3)
            }
        }
    }

    /// `Bt2_MatchFinder_GetMatches` (`LzFind.c:1151`).
    ///
    /// The whole function, which is the point: BT2 has **no** short-match pre-check,
    /// no `mmm`, no `UPDATE_maxLen`, and no early exit. Everything comes out of the
    /// tree walk, seeded with `max_len = 1` -- the literal in
    /// `GET_MATCHES_FOOTER_BT(1)` at `LzFind.c:1158`. Seeding 2 instead would
    /// suppress every length-2 match, because the walk's gate is `if (maxLen < len)`.
    fn get_matches_bt2(&mut self, out: &mut Vec<Match>) {
        out.clear();
        let len_limit = self.len_limit;
        if len_limit < 2 {
            self.move_pos();
            return;
        }
        let cur = self.buffer_offset;
        let pos = self.pos;

        let cur_match = self.bt2_heads(cur, pos);

        bt4::get_matches_spec1(
            len_limit,
            cur_match,
            pos,
            &self.buf,
            cur,
            &mut self.son,
            self.cyclic_buffer_pos,
            self.cyclic_buffer_size,
            self.cut_value,
            out,
            1,
        );
        self.move_pos();
    }

    /// `Bt2_MatchFinder_Skip` (`LzFind.c:1515`).
    fn skip_bt2(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 2 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;
            let cur_match = self.bt2_heads(cur, pos);
            bt4::skip_matches_spec(
                len_limit,
                cur_match,
                pos,
                &self.buf,
                cur,
                &mut self.son,
                self.cyclic_buffer_pos,
                self.cyclic_buffer_size,
                self.cut_value,
            );
            self.move_pos();
        }
    }

    /// `Bt3_MatchFinder_GetMatches` (`LzFind.c:1177`).
    ///
    /// BT3's pre-check is **not a reduced BT4 pre-check**, and the difference is
    /// output-visible rather than stylistic:
    ///
    /// * it has one candidate (`d2`), because `HASH3_CALC` produces no `h3`;
    /// * it calls `UPDATE_maxLen` **before** emitting, then pushes a single pair
    ///   whose length is already fully extended -- BT4 pushes `(2, d2-1)` first and
    ///   patches the length afterwards, and can leave an orphan length-2 pair behind
    ///   when `cur[2]` mismatches and `d3` fails;
    /// * so BT3 emits zero or one pair here, never two.
    ///
    /// `ReadMatchDistances` consumes the pair list positionally, so either shape
    /// error changes the parse immediately.
    ///
    /// One byte of comparison is enough before extending because `h2` keeps 10 bits
    /// of `crc[cur[0]] ^ cur[1]` including the bijective low 8 (`LzFind.c:37`), so a
    /// matching bucket plus a verified `cur[0]` implies `cur[1]` matches too.
    fn get_matches_bt3(&mut self, out: &mut Vec<Match>) {
        out.clear();
        let len_limit = self.len_limit;
        if len_limit < 3 {
            self.move_pos();
            return;
        }
        let cur = self.buffer_offset;
        let pos = self.pos;

        let (cur_match, d2) = self.bt3_heads(cur, pos);

        let mmm = self.cyclic_buffer_size.min(pos);
        let mut max_len = 2u32;

        if d2 < mmm && self.buf[cur - d2 as usize] == self.buf[cur] {
            // UPDATE_maxLen, scanning from `cur + max_len` == `cur + 2`.
            let diff = d2 as usize;
            let lim = cur + len_limit as usize;
            let mut c = cur + max_len as usize;
            while c != lim && self.buf[c - diff] == self.buf[c] {
                c += 1;
            }
            max_len = (c - cur) as u32;
            out.push(Match {
                len: max_len,
                dist: d2 - 1,
            });
            if max_len == len_limit {
                bt4::skip_matches_spec(
                    len_limit,
                    cur_match,
                    pos,
                    &self.buf,
                    cur,
                    &mut self.son,
                    self.cyclic_buffer_pos,
                    self.cyclic_buffer_size,
                    self.cut_value,
                );
                self.move_pos();
                return;
            }
        }

        bt4::get_matches_spec1(
            len_limit,
            cur_match,
            pos,
            &self.buf,
            cur,
            &mut self.son,
            self.cyclic_buffer_pos,
            self.cyclic_buffer_size,
            self.cut_value,
            out,
            max_len,
        );
        self.move_pos();
    }

    /// `Bt3_MatchFinder_Skip` (`LzFind.c:1538`).
    fn skip_bt3(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 3 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;

            // The C chains both assignments off `p->pos` (LzFind.c:1548); the
            // shared prologue does the same, and additionally *reads* `hash[h2]`,
            // which `Bt3_MatchFinder_Skip` has no use for. Reading a slot that is
            // about to be written changes nothing.
            let (cur_match, _d2) = self.bt3_heads(cur, pos);

            bt4::skip_matches_spec(
                len_limit,
                cur_match,
                pos,
                &self.buf,
                cur,
                &mut self.son,
                self.cyclic_buffer_pos,
                self.cyclic_buffer_size,
                self.cut_value,
            );
            self.move_pos();
        }
    }

    /// Find all matches at the current position into `out` (cleared first), then
    /// advance one byte.
    ///
    /// Exhaustive on the finder kind — `MatchFinder_CreateVTable` (`LzFind.c:1664`)
    /// as a `match` instead of a function-pointer table. Adding a variant will not
    /// compile until it has a body here, which is the property that stopped an
    /// unported finder from silently running BT4's search.
    pub fn get_matches(&mut self, out: &mut Vec<Match>) {
        match self.kind {
            MatchFinderKind::Bt2 => self.get_matches_bt2(out),
            MatchFinderKind::Bt3 => self.get_matches_bt3(out),
            MatchFinderKind::Bt4 => self.get_matches_bt4(out),
            MatchFinderKind::Hc4 => self.get_matches_hc4(out),
            MatchFinderKind::Hc5 => self.get_matches_hc5(out),
        }
    }

    /// Advance `num` positions, maintaining the search structure but recording
    /// nothing (`MatchFinder_Skip`).
    pub fn skip(&mut self, num: u32) {
        match self.kind {
            MatchFinderKind::Bt2 => self.skip_bt2(num),
            MatchFinderKind::Bt3 => self.skip_bt3(num),
            MatchFinderKind::Bt4 => self.skip_bt4(num),
            MatchFinderKind::Hc4 => self.skip_hc4(num),
            MatchFinderKind::Hc5 => self.skip_hc5(num),
        }
    }

    /// `Hc4_MatchFinder_GetMatches` (`LzFind.c:1362`).
    fn get_matches_hc4(&mut self, out: &mut Vec<Match>) {
        out.clear();
        let len_limit = self.len_limit;
        if len_limit < 4 {
            self.move_pos();
            return;
        }
        let cur = self.buffer_offset;
        let pos = self.pos;

        let (h2, h3, hv) = hc::hash4_calc(&self.crc, &self.buf, cur, self.hash_mask);
        self.hc_body(pos, cur, len_limit, h2, FIX3_HASH_SIZE + h3, FIX4_HASH_SIZE + hv, out, false);
    }

    /// `Hc5_MatchFinder_GetMatches` (`LzFind.c:1431`).
    fn get_matches_hc5(&mut self, out: &mut Vec<Match>) {
        out.clear();
        let len_limit = self.len_limit;
        if len_limit < 5 {
            self.move_pos();
            return;
        }
        let cur = self.buffer_offset;
        let pos = self.pos;

        let (h2, h3, hv) = hc::hash5_calc(&self.crc, &self.buf, cur, self.hash_mask);
        self.hc_body(pos, cur, len_limit, h2, FIX3_HASH_SIZE + h3, FIX5_HASH_SIZE + hv, out, true);
    }

    /// The shared tail of both hash-chain `GetMatches`, which differ only in their
    /// hash function, their length gate and their prologue.
    ///
    /// The chain's `maxLen == lenLimit` case is where it parts company with the tree:
    /// BT4 splices via `SkipMatchesSpec`, the chain merely plants
    /// `son[cyclicBufferPos] = curMatch` (`LzFind.c:1421`, `:1495`). Running the
    /// tree's version here would corrupt the chain rather than fail.
    #[allow(clippy::too_many_arguments)]
    fn hc_body(
        &mut self,
        pos: u32,
        cur: usize,
        len_limit: u32,
        h2i: usize,
        h3i: usize,
        hvi: usize,
        out: &mut Vec<Match>,
        five: bool,
    ) {
        // Reads before writes (LzFind.c:1376-1382).
        let d2 = pos - self.hash[h2i];
        let d3 = pos - self.hash[h3i];
        let cur_match = self.hash[hvi];
        self.hash[h2i] = pos;
        self.hash[h3i] = pos;
        self.hash[hvi] = pos;

        let mmm = self.cyclic_buffer_size.min(pos); // SET_mmm, LzFind.c:1171
        let prologue = match five {
            true => hc::hc5_prologue(len_limit, d2, d3, mmm, &self.buf, cur, out),
            false => hc::hc4_prologue(len_limit, d2, d3, mmm, &self.buf, cur, out),
        };
        match prologue {
            hc::Prologue::PlantAndStop => {
                self.son[self.cyclic_buffer_pos as usize] = cur_match;
            }
            hc::Prologue::Search { max_len } => hc::get_matches_spec(
                len_limit,
                cur_match,
                pos,
                &self.buf,
                cur,
                &mut self.son,
                self.cyclic_buffer_pos,
                self.cyclic_buffer_size,
                self.cut_value,
                out,
                max_len,
            ),
        }
        self.move_pos();
    }

    /// `Hc4_MatchFinder_Skip` (`LzFind.c:1619`).
    ///
    /// The chain has no `SkipMatchesSpec` analogue — skipping is O(1) per byte, just
    /// planting the chain link. The C batches the run (`HC_SKIP_HEADER`, `:1590`),
    /// capping it at `posLimit - pos` and bumping `cyclicBufferPos` up front; this
    /// per-position loop is equivalent because `len_limit` and `buffer` change only
    /// inside `check_limits`, which runs only at `pos == pos_limit` — exactly the
    /// batch cap.
    fn skip_hc4(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 4 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;
            let (h2, h3, hv) = hc::hash4_calc(&self.crc, &self.buf, cur, self.hash_mask);
            self.hc_skip_one(pos, h2, FIX3_HASH_SIZE + h3, FIX4_HASH_SIZE + hv);
        }
    }

    /// `Hc5_MatchFinder_Skip` (`LzFind.c:1635`).
    fn skip_hc5(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 5 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;
            let (h2, h3, hv) = hc::hash5_calc(&self.crc, &self.buf, cur, self.hash_mask);
            self.hc_skip_one(pos, h2, FIX3_HASH_SIZE + h3, FIX5_HASH_SIZE + hv);
        }
    }

    /// `HC_SKIP_FOOTER` (`LzFind.c:1610`) for one position.
    fn hc_skip_one(&mut self, pos: u32, h2i: usize, h3i: usize, hvi: usize) {
        let cur_match = self.hash[hvi];
        self.hash[h2i] = pos;
        self.hash[h3i] = pos;
        self.hash[hvi] = pos;
        self.son[self.cyclic_buffer_pos as usize] = cur_match;
        self.move_pos();
    }

    /// `Bt4_MatchFinder_GetMatches` (`LzFind.c:1226`).
    fn get_matches_bt4(&mut self, out: &mut Vec<Match>) {
        out.clear();
        // GET_MATCHES_HEADER2: lenLimit comes from SetLimits, not from avail.
        let len_limit = self.len_limit;
        if len_limit < 4 {
            self.move_pos();
            return;
        }

        let cur = self.buffer_offset;
        let pos = self.pos;

        let (cur_match, d2, d3) = self.bt4_heads(cur, pos);

        let mmm = self.cyclic_buffer_size.min(pos);
        let mut max_len = 3u32;

        // Short (2/3-byte) match handling — the C `for(;;)` block, run at most once.
        let mut short_recorded = false;
        let mut chosen_d = 0u32;
        if d2 < mmm && self.buf[cur - d2 as usize] == self.buf[cur] {
            out.push(Match {
                len: 2,
                dist: d2 - 1,
            });
            if self.buf[cur - d2 as usize + 2] == self.buf[cur + 2] {
                chosen_d = d2;
                short_recorded = true;
            } else if d3 < mmm && self.buf[cur - d3 as usize] == self.buf[cur] {
                out.push(Match {
                    len: 0,
                    dist: d3 - 1,
                });
                chosen_d = d3;
                short_recorded = true;
            }
        } else if d3 < mmm && self.buf[cur - d3 as usize] == self.buf[cur] {
            out.push(Match {
                len: 0,
                dist: d3 - 1,
            });
            chosen_d = d3;
            short_recorded = true;
        }

        if short_recorded {
            // UPDATE_maxLen: extend the chosen match from offset max_len (3).
            let diff = chosen_d as usize;
            let lim = cur + len_limit as usize;
            let mut c = cur + max_len as usize;
            while c != lim && self.buf[c - diff] == self.buf[c] {
                c += 1;
            }
            max_len = (c - cur) as u32;
            let last = out.len() - 1;
            out[last].len = max_len;
            if max_len == len_limit {
                bt4::skip_matches_spec(
                    len_limit,
                    cur_match,
                    pos,
                    &self.buf,
                    cur,
                    &mut self.son,
                    self.cyclic_buffer_pos,
                    self.cyclic_buffer_size,
                    self.cut_value,
                );
                self.move_pos();
                return;
            }
        }

        bt4::get_matches_spec1(
            len_limit,
            cur_match,
            pos,
            &self.buf,
            cur,
            &mut self.son,
            self.cyclic_buffer_pos,
            self.cyclic_buffer_size,
            self.cut_value,
            out,
            max_len,
        );
        self.move_pos();
    }

    /// `Bt4_MatchFinder_Skip` (`LzFind.c:1554`).
    fn skip_bt4(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 4 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;

            let (cur_match, _d2, _d3) = self.bt4_heads(cur, pos);

            bt4::skip_matches_spec(
                len_limit,
                cur_match,
                pos,
                &self.buf,
                cur,
                &mut self.son,
                self.cyclic_buffer_pos,
                self.cyclic_buffer_size,
                self.cut_value,
            );
            self.move_pos();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The four `return 0` rejections in `GetBlockSize` are omitted from
    /// [`block_size`] on the grounds that they need a history size above
    /// `kLzmaMaxHistorySize`. This is that claim, checked rather than assumed —
    /// if any of them becomes reachable, `block_size` is silently wrong instead of
    /// refusing, so the assumption has to be a test and not a comment.
    #[test]
    fn block_size_never_hits_the_c_rejections() {
        let k_block_size_max = 0u32.wrapping_sub(K_BLOCK_SIZE_ALIGN);
        let k_block_size_reserve_min: u32 = 1 << 24;

        let mut histories = vec![
            0,
            1,
            K_REDUCE_MIN_FOR_TEST,
            1 << 16,
            1 << 20,
            (1 << 30) - 1,
            1 << 30,
            crate::props::K_MAX_HISTORY_SIZE - 1,
            crate::props::K_MAX_HISTORY_SIZE,
        ];
        // Powers of two and their neighbours, where the shift in `reserve` flips.
        for bits in 0..32 {
            let v = 1u64 << bits;
            for delta in [-1i64, 0, 1] {
                let c = v as i64 + delta;
                if c >= 0 && c <= crate::props::K_MAX_HISTORY_SIZE as i64 {
                    histories.push(c as u32);
                }
            }
        }

        for &h in &histories {
            for fb in [1u32, 2, 5, 32, 64, 273] {
                let ksb = h + KEEP_ADD_BUFFER_BEFORE + 1;
                let ksa = (KEEP_ADD_BUFFER_AFTER + fb).max(NUM_HASH_BYTES_MIN);
                assert!(ksb >= h, "keepSizeBefore overflowed at history {h}");
                let bs = ksb + ksa;
                assert!(bs >= ksb, "blockSize overflowed at history {h}, fb {fb}");
                assert!(bs < k_block_size_max, "blockSize >= max at history {h}");
                assert!(
                    k_block_size_max - bs >= k_block_size_reserve_min,
                    "rem below kBlockSizeReserveMin at history {h}, fb {fb}"
                );
                // And the window must be large enough for MoveBlock's invariant:
                // buffer_offset > keep_size_before whenever need_move() fires.
                let out = block_size(ksb, ksa);
                assert!(
                    out as u64 >= (ksb as u64) + (ksa as u64),
                    "window smaller than keepBefore+keepAfter at history {h}, fb {fb}"
                );
            }
        }
    }

    const K_REDUCE_MIN_FOR_TEST: u32 = 1 << 12;

    /// [`MatchFinder::new`] grew a `keep_add_buffer_before` parameter where it used
    /// to hard-code [`KEEP_ADD_BUFFER_BEFORE`]. At that value nothing may move: the
    /// window geometry decides when `MoveBlock` runs, and `MoveBlock`'s alignment
    /// remainder is already known to be output-visible (PROVENANCE.md — dropping it
    /// diverges 11 of the 24 sliding-window comparisons).
    ///
    /// The expected sizes were **computed** from `GetBlockSize` rather than copied
    /// out of a passing run, so this fails if the formula changes, not merely if the
    /// parameter is misrouted.
    #[test]
    fn lzma_keep_add_buffer_before_reproduces_the_previous_block_size() {
        let table: [((u32, u32), usize); 7] = [
            ((4096, 32), 65536),
            ((4096, 64), 65536),
            ((65536, 64), 131072),
            ((1048576, 64), 1638400),
            ((1048576, 273), 1638400),
            ((16777216, 64), 25231360),
            ((67108864, 64), 100728832),
        ];
        let data = [0u8; 64];
        for ((dict, fb), expect) in table {
            let props = LzmaProps {
                lc: 3,
                lp: 0,
                pb: 2,
                dict_size: dict,
                fb,
                mc: 32,
                mf: crate::props::MatchFinderKind::Hc5,
                fast_mode: false,
                num_threads: 1,
                write_end_mark: true,
            };
            let mut src = crate::stream::SliceIn::new(&data);
            let mf = MatchFinder::new(&mut src, &props, KEEP_ADD_BUFFER_BEFORE, u64::MAX);
            assert_eq!(
                mf.block_size(),
                expect,
                "block_size moved for dict {dict}, fb {fb}"
            );
            // And the parameter is genuinely wired: a wider lookbehind must widen
            // the window, or the test above would pass on a hard-coded constant.
            let mut src2 = crate::stream::SliceIn::new(&data);
            let wide = MatchFinder::new(&mut src2, &props, (1 << 21) - dict.min(1 << 21), u64::MAX);
            assert!(
                wide.block_size() >= mf.block_size(),
                "keep_add_buffer_before is ignored: dict {dict}, fb {fb}"
            );
        }
    }

    /// `LZMA2_KEEP_WINDOW_SIZE` widens the lookbehind for small dictionaries, which
    /// is what lets the copy chunk read 2 MiB back. A 4 KiB dictionary must end up
    /// with a window big enough to hold that history plus the lookahead.
    #[test]
    fn a_widened_lookbehind_can_hold_the_lzma2_copy_chunk() {
        let keep_window: u32 = 1 << 21;
        let data = [0u8; 64];
        for dict in [4096u32, 1 << 16, 1 << 20] {
            let before = keep_window - dict;
            let props = LzmaProps {
                lc: 3,
                lp: 0,
                pb: 2,
                dict_size: dict,
                fb: 64,
                mc: 32,
                mf: crate::props::MatchFinderKind::Hc5,
                fast_mode: false,
                num_threads: 1,
                write_end_mark: false,
            };
            let mut src = crate::stream::SliceIn::new(&data);
            let mf = MatchFinder::new(&mut src, &props, before, u64::MAX);
            assert!(
                mf.keep_size_before as u64 >= keep_window as u64,
                "dict {dict}: keepSizeBefore {} below the 2 MiB copy-chunk reach",
                mf.keep_size_before
            );
            assert!(mf.block_size() as u64 > keep_window as u64, "dict {dict}");
        }
    }

    /// `get_hash_mask` against the C, for every `numHashBytes`.
    ///
    /// The expected values are **not transcribed** from reading `LzFind.c`; they were
    /// printed by a probe that `#include`s `LzFind.c` and calls the real (static)
    /// `MatchFinder_GetHashMask`. That matters here because two of the branches are
    /// one token apart -- nhb 3 caps at `(1 << 24) - 1` where nhb 4 takes a second
    /// `>>= 1` -- and they agree for every history size up to `1 << 26`, so a
    /// plausible-looking hand-written table would pass while being wrong.
    #[test]
    fn hash_mask_matches_the_c_for_every_num_hash_bytes() {
        // history, nhb2, nhb3, nhb4, nhb5
        let table: [(u32, u32, u32, u32, u32); 13] = [
            (0, 65535, 65535, 65535, 262143),
            (1, 65535, 65535, 65535, 262143),
            (4096, 65535, 65535, 65535, 262143),
            (19584, 65535, 65535, 65535, 262143),
            (65536, 65535, 65535, 65535, 262143),
            (1 << 20, 65535, 524287, 524287, 524287),
            (1 << 24, 65535, 8388607, 8388607, 8388607),
            ((1 << 24) + 1, 65535, 16777215, 16777215, 16777215),
            (1 << 26, 65535, 16777215, 16777215, 16777215),
            // The first history size where nhb 3 and nhb 4 part company.
            ((1 << 26) + 1, 65535, 16777215, 33554431, 33554431),
            (1 << 27, 65535, 16777215, 33554431, 33554431),
            (1 << 30, 65535, 16777215, 268435455, 268435455),
            (15 << 28, 65535, 16777215, 1073741823, 1073741823),
        ];
        for (hist, m2, m3, m4, m5) in table {
            assert_eq!(get_hash_mask(2, hist), m2, "nhb2 history {hist}");
            assert_eq!(get_hash_mask(3, hist), m3, "nhb3 history {hist}");
            assert_eq!(get_hash_mask(4, hist), m4, "nhb4 history {hist}");
            assert_eq!(get_hash_mask(5, hist), m5, "nhb5 history {hist}");
        }
        // Stated as an assertion so that collapsing the two branches fails here
        // rather than only in a large-dictionary differential run.
        assert_ne!(get_hash_mask(3, (1 << 26) + 1), get_hash_mask(4, (1 << 26) + 1));
    }

    #[test]
    fn normalize3_saturates_at_zero() {
        // SASUB_32 floors at kEmptyHashValue so that a stale reference can never
        // wrap into a valid-looking one.
        let mut items = [0u32, 1, 5, 6, 7, u32::MAX];
        normalize3(6, &mut items);
        assert_eq!(items, [0, 0, 0, 0, 1, u32::MAX - 6]);
    }

    // ---- the multi-threaded hash worker ----

    /// Deterministic pseudo-random bytes (xorshift32) — the incompressible half of
    /// the corpus, where almost every position reaches the tree walk.
    fn noise(n: usize, seed: u32) -> Vec<u8> {
        let mut s = seed | 1;
        (0..n)
            .map(|_| {
                s ^= s << 13;
                s ^= s >> 17;
                s ^= s << 5;
                (s >> 24) as u8
            })
            .collect()
    }

    /// Text-like data with long repeats — the compressible half, where the short
    /// (2-/3-byte) prologues and the `max_len == len_limit` early exits fire.
    fn textish(n: usize, seed: u32) -> Vec<u8> {
        const WORDS: [&str; 8] = [
            "the ", "quick ", "brown ", "fox ", "jumps ", "over ", "lazy ", "dog\n",
        ];
        let mut s = seed | 1;
        let mut v = Vec::with_capacity(n + 16);
        while v.len() < n {
            s ^= s << 13;
            s ^= s >> 17;
            s ^= s << 5;
            v.extend_from_slice(WORDS[(s >> 28) as usize & 7].as_bytes());
        }
        v.truncate(n);
        v
    }

    fn props_for(dict: u32, mf: MatchFinderKind) -> LzmaProps {
        LzmaProps {
            lc: 3,
            lp: 0,
            pb: 2,
            dict_size: dict,
            fb: 64,
            mc: mf.auto_mc(64),
            mf,
            num_threads: 1,
            fast_mode: false,
            write_end_mark: true,
        }
    }

    /// Every match the finder reports, flattened, plus the final `pos`.
    ///
    /// Match *lists*, not encoded bytes: the encoder consumes `get_matches`
    /// positionally, so two finders that agree here produce the same stream by
    /// construction, and a divergence is reported at the position it happens
    /// rather than after the range coder has smeared it.
    ///
    /// `parse` mixes `skip` into the drive the way the parser does. Both callers
    /// matter: `skip` and `get_matches` update the hash tables identically but
    /// consume the worker's records the same way, and an off-by-one in only one of
    /// them would desynchronize the queue.
    fn trace(
        props: &LzmaProps,
        data: &[u8],
        threads: u32,
        norm: u32,
        parse: bool,
    ) -> (Vec<u32>, u32) {
        let mut src = crate::stream::SliceIn::new(data);
        let mut mf = MatchFinder::new_inner(
            &mut src,
            props,
            KEEP_ADD_BUFFER_BEFORE,
            u64::MAX,
            threads,
            norm,
        );
        // "A green test may not run your code": assert the branch was taken.
        assert_eq!(
            mf.mt.is_some(),
            threads > 1 && props.mf.bt_mode(),
            "hash worker not in the expected state for {:?} at {threads} threads",
            props.mf
        );
        let mut out = Vec::new();
        let mut buf = Vec::new();
        while mf.num_available() != 0 {
            mf.get_matches(&mut buf);
            out.push(buf.len() as u32);
            for m in &buf {
                out.push(m.len);
                out.push(m.dist);
            }
            if parse {
                let longest = buf.last().map(|m| m.len).unwrap_or(0);
                let adv = longest.saturating_sub(1).min(mf.num_available());
                match adv {
                    0 => {}
                    n => mf.skip(n),
                }
            }
        }
        // A dead worker latches ERR_HASH_WORKER, so this also proves the queue
        // never ran dry early.
        assert_eq!(mf.result(), Ok(()), "finder latched an error");
        (out, mf.pos)
    }

    fn assert_same(a: &[u32], b: &[u32], what: &str) {
        for (i, (x, y)) in a.iter().zip(b.iter()).enumerate() {
            assert_eq!(x, y, "{what}: first divergence at trace element {i}");
        }
        assert_eq!(a.len(), b.len(), "{what}: trace lengths differ");
    }

    /// **The gate.** The threaded hash stage must find exactly the matches the
    /// single-threaded one does, at every position, for every binary-tree finder.
    ///
    /// Sizes are chosen so the window slides (`dict` 4 KiB gives a 64 KiB block,
    /// 64 KiB gives 128 KiB) — the `MoveBlock` path is where a producer that held
    /// an index across an advance would break, and it is the one the worker
    /// deliberately does not participate in.
    #[test]
    fn mt_matches_single_threaded_match_for_match() {
        let cases: [(u32, Vec<u8>, &str); 5] = [
            (1 << 12, textish(200_000, 1), "textish/4K"),
            (1 << 12, noise(200_000, 2), "noise/4K"),
            (1 << 16, textish(200_000, 3), "textish/64K"),
            (1 << 16, noise(120_000, 4), "noise/64K"),
            // Shorter than numHashBytes at every position but the first: the
            // boundary where the consumer stops asking and the worker stops
            // producing, which nothing coordinates explicitly.
            (1 << 12, textish(3, 5), "3 bytes"),
        ];
        let finders = [
            MatchFinderKind::Bt2,
            MatchFinderKind::Bt3,
            MatchFinderKind::Bt4,
        ];
        for mf in finders {
            for (dict, data, name) in &cases {
                let props = props_for(*dict, mf);
                for parse in [false, true] {
                    let (st, _) = trace(&props, data, 1, K_MAX_VAL_FOR_NORMALIZE, parse);
                    let (mt, _) = trace(&props, data, 2, K_MAX_VAL_FOR_NORMALIZE, parse);
                    assert_same(&st, &mt, &format!("{mf:?} {name} parse={parse}"));
                    assert!(!st.is_empty(), "{mf:?} {name}: empty trace proves nothing");
                }
            }
        }

        // The control. A sweep that cannot see a difference proves nothing, so
        // check that this comparison can: BT4 and BT2 must disagree on data that
        // has matches to disagree about.
        let data = textish(200_000, 1);
        let (bt2, _) = trace(&props_for(1 << 16, MatchFinderKind::Bt2), &data, 1, K_MAX_VAL_FOR_NORMALIZE, false);
        let (bt4, _) = trace(&props_for(1 << 16, MatchFinderKind::Bt4), &data, 1, K_MAX_VAL_FOR_NORMALIZE, false);
        assert!(
            bt2 != bt4,
            "BT2 and BT4 produced identical traces: assert_same cannot see a difference"
        );
    }

    /// `MatchFinder_CheckLimits`'s normalization is unreachable below 4 GiB of
    /// input, so it is driven here by moving `kMaxValForNormalize` down — the same
    /// trick the C keeps commented out at `LzFindMt.c:287`
    /// (`// #define kMtMaxValForNormalize ((1 << 21)) // for debug`).
    ///
    /// Two claims at once: the shift is output-neutral (it only saturates
    /// references already out of `mmm`'s range), and the worker shifts its half of
    /// the state in the same frame as the consumer shifts the other half. Get the
    /// second wrong and every distance after the first shift is garbage.
    #[test]
    fn normalization_is_output_neutral_and_the_worker_stays_in_frame() {
        let threshold: u32 = 1 << 17;
        let data = textish(300_000, 7);
        for mf in [
            MatchFinderKind::Bt2,
            MatchFinderKind::Bt3,
            MatchFinderKind::Bt4,
        ] {
            let props = props_for(1 << 12, mf);
            let (base, base_pos) = trace(&props, &data, 1, K_MAX_VAL_FOR_NORMALIZE, true);
            let (st, st_pos) = trace(&props, &data, 1, threshold, true);
            let (mt, mt_pos) = trace(&props, &data, 2, threshold, true);
            // It has to have actually fired, or both runs are the same run.
            assert!(
                base_pos > threshold,
                "{mf:?}: input too short to cross the threshold"
            );
            assert!(st_pos < threshold, "{mf:?}: single-threaded never normalized");
            assert!(mt_pos < threshold, "{mf:?}: threaded never normalized");
            assert_same(&base, &st, &format!("{mf:?} single-threaded normalization"));
            assert_same(&base, &mt, &format!("{mf:?} threaded normalization"));
        }
    }

    /// `LzmaEnc.c:2695` — `mtMode = multiThread && !fastMode && btMode != 0` — plus
    /// the `fb >= numHashBytes` guard this port adds. Each conjunct on its own,
    /// because a worker started for a hash chain would hash with the wrong
    /// function and a worker started with `fb < numHashBytes` would stop at a
    /// different position from the consumer and hang the queue.
    #[test]
    fn the_worker_starts_only_where_lzma_enc_would_start_one() {
        let data = noise(4096, 11);
        let started = |props: &LzmaProps, threads: u32| {
            let mut src = crate::stream::SliceIn::new(&data);
            let mf = MatchFinder::new_inner(
                &mut src,
                props,
                KEEP_ADD_BUFFER_BEFORE,
                u64::MAX,
                threads,
                K_MAX_VAL_FOR_NORMALIZE,
            );
            let on = mf.mt.is_some();
            // The consumer's own table is allocated exactly when it owns it.
            assert_eq!(mf.hash.is_empty(), on, "hash ownership disagrees with `mt`");
            on
        };

        assert!(started(&props_for(1 << 16, MatchFinderKind::Bt4), 2));
        assert!(!started(&props_for(1 << 16, MatchFinderKind::Bt4), 1));
        // btMode == 0.
        assert!(!started(&props_for(1 << 16, MatchFinderKind::Hc4), 2));
        assert!(!started(&props_for(1 << 16, MatchFinderKind::Hc5), 2));
        // fastMode.
        let mut fast = props_for(1 << 16, MatchFinderKind::Bt4);
        fast.fast_mode = true;
        assert!(!started(&fast, 2));
        // fb < numHashBytes.
        let mut short_fb = props_for(1 << 16, MatchFinderKind::Bt4);
        short_fb.fb = 3;
        assert!(!started(&short_fb, 2));
        // ... and BT2 with the same fb is fine, so the guard is per-finder.
        let mut short_fb2 = props_for(1 << 16, MatchFinderKind::Bt2);
        short_fb2.fb = 3;
        assert!(started(&short_fb2, 2));
    }

    /// A finder can be dropped mid-stream — LZMA2 builds one per block, and any
    /// error path abandons one — and the worker may be parked in *either* queue
    /// when that happens. `Drop` has to release both, so this drops at three
    /// points: before consuming anything (worker blocked writing, queue full),
    /// part way through (worker blocked writing), and after the feed has ended
    /// (worker blocked reading).
    ///
    /// A regression here hangs rather than fails, which is exactly why it needs a
    /// test at all — the same shape as the partial-batch deadlock that the
    /// flush-before-park comment in `mt.rs` records.
    #[test]
    fn dropping_mid_stream_shuts_the_worker_down() {
        let data = textish(300_000, 9);
        let props = props_for(1 << 12, MatchFinderKind::Bt4);
        for consume in [0usize, 1000, 300_000] {
            let mut src = crate::stream::SliceIn::new(&data);
            let mut mf = MatchFinder::new_inner(
                &mut src,
                &props,
                KEEP_ADD_BUFFER_BEFORE,
                u64::MAX,
                2,
                K_MAX_VAL_FOR_NORMALIZE,
            );
            assert!(mf.mt.is_some());
            let mut buf = Vec::new();
            for _ in 0..consume {
                match mf.num_available() {
                    0 => break,
                    _ => mf.get_matches(&mut buf),
                }
            }
            drop(mf);
        }
    }

    /// Not a gate — a measurement. Ignored by default because it takes tens of
    /// seconds and because a timing assertion on a shared machine is a flaky test,
    /// not a proof.
    ///
    /// ```text
    ///     cargo test -p darc-lzma --release -- --ignored --nocapture hash_worker_speedup
    /// ```
    ///
    /// It reports the *match finder* alone, which is an upper bound on the
    /// end-to-end gain: the optimal parser's own work sits on the consumer thread
    /// too, so it dilutes the ratio. The last line prints the whole-encode time for
    /// the same input so the dilution can be applied.
    #[test]
    #[ignore = "measurement, not a gate"]
    fn hash_worker_speedup() {
        use std::time::Instant;
        const N: usize = 24 << 20;
        // Half repetitive, half not: a finder benchmarked only on text spends its
        // time in the `max_len == len_limit` early exit and never shows the tree
        // walk's real cost.
        let mut data = textish(N / 2, 42);
        data.extend_from_slice(&noise(N / 2, 43));
        let props = props_for(1 << 24, MatchFinderKind::Bt4);

        // Its own driver rather than `trace`: at 24 MB the recorded trace is
        // hundreds of megabytes, which would measure the allocator as much as the
        // finder. A 64-bit FNV fold keeps the "both paths did the same work"
        // check without the memory.
        let run = |threads: u32| {
            let mut src = crate::stream::SliceIn::new(&data);
            let mut mf = MatchFinder::new_inner(
                &mut src,
                &props,
                KEEP_ADD_BUFFER_BEFORE,
                u64::MAX,
                threads,
                K_MAX_VAL_FOR_NORMALIZE,
            );
            assert_eq!(mf.mt.is_some(), threads > 1);
            let mut buf = Vec::new();
            let mut fold: u64 = 0xcbf2_9ce4_8422_2325;
            let t0 = Instant::now();
            while mf.num_available() != 0 {
                mf.get_matches(&mut buf);
                for m in &buf {
                    fold = (fold ^ u64::from(m.len)).wrapping_mul(0x100_0000_01b3);
                    fold = (fold ^ u64::from(m.dist)).wrapping_mul(0x100_0000_01b3);
                }
            }
            (t0.elapsed().as_secs_f64(), fold)
        };
        // Interleaved, so a machine that gets busier over the run does not read as
        // a speedup (or hide one).
        for i in 0..3 {
            let (t1, n1) = run(1);
            let (t2, n2) = run(2);
            assert_eq!(n1, n2, "the two paths did not produce the same matches");
            println!(
                "round {i}: 1 thread {t1:.3}s, 2 threads {t2:.3}s, speedup {:.3}x",
                t1 / t2
            );
        }
        let t0 = Instant::now();
        let enc = crate::encode(&data, &props);
        let n = match enc {
            Ok(v) => v.len(),
            Err(e) => panic!("encode failed: {e:?}"),
        };
        println!(
            "whole encode (1 thread): {:.3}s -> {n} bytes",
            t0.elapsed().as_secs_f64()
        );
    }

    /// A window that must slide: the finder is asked for more bytes than the block
    /// holds, so `move_block` runs and `buffer_offset` moves backwards. The check is
    /// the invariant the encoder depends on — `win()[cur_index() + k]` is still
    /// stream byte `(pos - 1) + k` after the slide.
    #[test]
    fn window_slides_and_keeps_its_indexing_invariant() {
        let n = 400_000usize;
        let data: Vec<u8> = (0..n).map(|i| (i * 7 + (i >> 9)) as u8).collect();
        let props = LzmaProps {
            lc: 3,
            lp: 0,
            pb: 2,
            dict_size: 1 << 12,
            fb: 32,
            mc: 32,
            mf: crate::props::MatchFinderKind::Bt4,
            num_threads: 1,
            fast_mode: false,
            write_end_mark: true,
        };
        let mut src = crate::stream::SliceIn::new(&data);
        let mut mf = MatchFinder::new(&mut src, &props, KEEP_ADD_BUFFER_BEFORE, u64::MAX);
        let first_block = mf.block_size;
        assert!(first_block < n, "block must be smaller than the input to slide");

        let mut out = Vec::new();
        let mut slides = 0usize;
        let mut prev_offset = mf.cur_index();
        for i in 0..n {
            // The invariant, at every single position.
            assert_eq!(
                mf.win()[mf.cur_index()],
                data[i],
                "window index desynchronized from the stream at byte {i}"
            );
            if mf.cur_index() < prev_offset {
                slides += 1;
            }
            prev_offset = mf.cur_index();
            mf.get_matches(&mut out);
        }
        assert!(slides > 4, "window never slid ({slides} slides): the test proves nothing");
        assert_eq!(mf.num_available(), 0, "did not consume the whole stream");
        assert_eq!(mf.result(), Ok(()));
    }
}
