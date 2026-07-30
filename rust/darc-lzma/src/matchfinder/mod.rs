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

use hash::{CRC_SHIFT_1, FIX3_HASH_SIZE, FIX4_HASH_SIZE, HASH2_SIZE, HASH3_SIZE};

use crate::props::LzmaProps;
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
/// `p->numHashBytes` for BT4.
const NUM_HASH_BYTES: u32 = 4;
/// `beforeSize = kNumOpts` (`LzmaEnc.c:2688`), the encoder's lookbehind slack on
/// top of the dictionary. Equal to [`crate::optimum::NUM_OPTS`].
const KEEP_ADD_BUFFER_BEFORE: u32 = crate::optimum::NUM_OPTS as u32;
/// `LZMA_MATCH_LEN_MAX + 1` (`LzmaEnc.c:2752`), the lookahead slack `LzmaEnc`
/// requests on top of `numFastBytes`.
const KEEP_ADD_BUFFER_AFTER: u32 = MATCH_LEN_MAX + 1;

/// `MatchFinder_GetHashMask` for `numHashBytes == 4`.
fn get_hash_mask(history_size: u32) -> u32 {
    let mut hs = history_size.saturating_sub(1);
    hs |= hs >> 1;
    hs |= hs >> 2;
    hs |= hs >> 4;
    hs |= hs >> 8;
    hs >>= 1;
    if hs >= (1 << 24) {
        hs >>= 1; // numHashBytes == 4 branch
    }
    hs |= (1 << 16) - 1;
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
    history_size: u32,
    cyclic_buffer_pos: u32,
    cyclic_buffer_size: u32,
    match_max_len: u32,
    cut_value: u32,
    hash_mask: u32,
    hash: Vec<u32>,
    son: Vec<u32>,
    crc: [u32; 256],
}

impl<'a> MatchFinder<'a> {
    /// Create and initialize the finder, filling the first window from `stream`.
    ///
    /// Mirrors `MatchFinder_Create` + `MatchFinder_Init` with `LzmaEnc`'s arguments
    /// (`LzmaEnc.c:2751`): `keepAddBufferBefore = kNumOpts`,
    /// `matchMaxLen = numFastBytes`, `keepAddBufferAfter = LZMA_MATCH_LEN_MAX + 1`.
    pub fn new(stream: &'a mut dyn InStream, props: &LzmaProps) -> Self {
        let history_size = props.history_size();
        let cyclic_buffer_size = history_size + 1;
        let hash_mask = get_hash_mask(history_size);
        let hash_len = FIX4_HASH_SIZE + hash_mask as usize + 1;

        // MatchFinder_Create:379 -- "we need one additional byte in keepSizeBefore,
        // since we use MoveBlock() after (p->pos++) and before dictionary using".
        let keep_size_before = history_size + KEEP_ADD_BUFFER_BEFORE + 1;
        // keepAddBufferAfter += matchMaxLen, then floored at numHashBytes (which
        // never binds: KEEP_ADD_BUFFER_AFTER alone already exceeds 4).
        let keep_size_after = (KEEP_ADD_BUFFER_AFTER + props.fb).max(NUM_HASH_BYTES);
        let block_size = block_size(keep_size_before, keep_size_after) as usize;

        let mut crc = [0u32; 256];
        for (i, slot) in crc.iter_mut().enumerate() {
            let mut r = i as u32;
            for _ in 0..8 {
                r = (r >> 1) ^ (CRC_POLY & 0u32.wrapping_sub(r & 1));
            }
            *slot = r;
        }

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
            history_size,
            // CYC_TO_POS_OFFSET is 0, so cyclicBufferPos initializes to pos.
            cyclic_buffer_pos: 1,
            cyclic_buffer_size,
            match_max_len: props.fb,
            cut_value: props.mc,
            hash_mask,
            hash: vec![0u32; hash_len],
            son: vec![0u32; 2 * cyclic_buffer_size as usize],
            crc,
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
                    return;
                }
                Ok(0) => {
                    self.stream_end_was_reached = true;
                    return;
                }
                Ok(n) => {
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
        let mut n = K_MAX_VAL_FOR_NORMALIZE.wrapping_sub(self.pos);
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

        if self.pos == K_MAX_VAL_FOR_NORMALIZE && self.num_available() >= NUM_HASH_BYTES {
            // Unreachable below 4 GiB of input, since kMaxValForNormalize is 0 and
            // `pos` only gets there by wrapping. Ported for parity at that size;
            // `normalize3` itself is covered directly by unit test.
            let sub_value = self.pos - self.history_size - 1;
            // MatchFinder_REDUCE_OFFSETS (LzFind.h:112).
            self.pos -= sub_value;
            self.stream_pos -= sub_value;
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

    /// Find all matches at the current position into `out` (cleared first), then
    /// advance one byte. Mirrors `Bt4_MatchFinder_GetMatches`.
    pub fn get_matches(&mut self, out: &mut Vec<Match>) {
        out.clear();
        // GET_MATCHES_HEADER2: lenLimit comes from SetLimits, not from avail.
        let len_limit = self.len_limit;
        if len_limit < 4 {
            self.move_pos();
            return;
        }

        let cur = self.buffer_offset;
        let pos = self.pos;

        // HASH4_CALC
        let temp = self.crc[self.buf[cur] as usize] ^ self.buf[cur + 1] as u32;
        let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
        let temp = temp ^ ((self.buf[cur + 2] as u32) << 8);
        let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
        let hv = ((temp ^ (self.crc[self.buf[cur + 3] as usize] << CRC_SHIFT_1)) & self.hash_mask)
            as usize;

        let h2i = h2;
        let h3i = FIX3_HASH_SIZE + h3;
        let h4i = FIX4_HASH_SIZE + hv;
        let d2 = pos - self.hash[h2i];
        let d3 = pos - self.hash[h3i];
        let cur_match = self.hash[h4i];
        self.hash[h2i] = pos;
        self.hash[h3i] = pos;
        self.hash[h4i] = pos;

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

    /// Advance `num` positions, maintaining the tree but recording nothing
    /// (`MatchFinder_Skip` for BT4).
    pub fn skip(&mut self, num: u32) {
        for _ in 0..num {
            let len_limit = self.len_limit;
            if len_limit < 4 {
                self.move_pos();
                continue;
            }
            let cur = self.buffer_offset;
            let pos = self.pos;

            let temp = self.crc[self.buf[cur] as usize] ^ self.buf[cur + 1] as u32;
            let h2 = (temp & (HASH2_SIZE as u32 - 1)) as usize;
            let temp = temp ^ ((self.buf[cur + 2] as u32) << 8);
            let h3 = (temp & (HASH3_SIZE as u32 - 1)) as usize;
            let hv = ((temp ^ (self.crc[self.buf[cur + 3] as usize] << CRC_SHIFT_1))
                & self.hash_mask) as usize;

            let cur_match = self.hash[FIX4_HASH_SIZE + hv];
            self.hash[h2] = pos;
            self.hash[FIX3_HASH_SIZE + h3] = pos;
            self.hash[FIX4_HASH_SIZE + hv] = pos;

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
                let ksa = (KEEP_ADD_BUFFER_AFTER + fb).max(NUM_HASH_BYTES);
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

    #[test]
    fn normalize3_saturates_at_zero() {
        // SASUB_32 floors at kEmptyHashValue so that a stale reference can never
        // wrap into a valid-looking one.
        let mut items = [0u32, 1, 5, 6, 7, u32::MAX];
        normalize3(6, &mut items);
        assert_eq!(items, [0, 0, 0, 0, 1, u32::MAX - 6]);
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
            write_end_mark: true,
        };
        let mut src = crate::stream::SliceIn::new(&data);
        let mut mf = MatchFinder::new(&mut src, &props);
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
