//! Match finders, ported from `Compression/Tornado/MatchFinder.cpp`.
//!
//! Encoder-only: nothing here is reachable from `tor_decompress`. A match finder
//! answers one question -- "how long a match can I find for the bytes at `p`,
//! and where" -- and every one of them also *mutates* the hash table as it
//! searches, so the search order is part of the answer, not an optimisation
//! detail.
//!
//! ## Why these have to be exact rather than merely good
//!
//! A match finder that returns a shorter match, or the same length at a
//! different distance, still produces a stream that decodes to the right bytes.
//! It is simply a worse stream. So none of the round-trip tests can tell a
//! faithful port from a plausible one -- only a byte-for-byte comparison
//! against the C on the same input can, which is what the differential harness
//! does. Everything here is transcribed rather than reimplemented for that
//! reason, including the bounds that look off by one:
//!
//! * `MatchFinder1`/`MatchFinder2` extend with `p+len+4 < bufend`, but
//!   `MatchFinderN` uses `p+len+4 <= bufend`. That is not a typo in either.
//! * The first comparison loop starts at `MINLEN-1`, not `MINLEN`, in the first
//!   two and at `MINLEN` in the third.
//!
//! ## Positions, not pointers
//!
//! The C stores 32-bit offsets from `base` in the hash table on 64-bit targets
//! and raw pointers on 32-bit ones (`toPtr`/`fromPtr`, :106-115). Offsets are
//! the only form that makes sense here, and they match the 64-bit path the
//! archiver is built for. The empty-slot marker is offset 1 rather than 0,
//! because lazy matching may extend a match one byte backwards and offset 0
//! would let it reach outside the buffer (:143-148).

use core::ffi::c_int;

/// Bytes past `bufend` that hashing may read, and therefore the slack the input
/// buffer must carry (`MAX_HASHED_BYTES`, :12).
pub const MAX_HASHED_BYTES: usize = 12;

const KB: usize = 1024;
const MB: usize = 1024 * 1024;

/// `lb` (Common.h:507) -- floor of the binary logarithm. Undefined for 0 there;
/// the callers all pass a positive size.
fn lb(n: u32) -> u32 {
    debug_assert!(n > 0, "lb(0) is undefined in the C too");
    31 - n.leading_zeros()
}

/// `roundup_to_power_of(n, 2)` (Common.h:529).
fn roundup_pow2(n: u32) -> u32 {
    if n <= 1 {
        return 1;
    }
    2u32 << lb(n - 1)
}

/// Little-endian 32-bit load (`value32`). Every target this is built for is
/// little-endian; the hash and every `val32equ` depend on it.
#[inline]
pub fn value32(buf: &[u8], at: usize) -> u32 {
    u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]])
}

#[inline]
fn val32equ(buf: &[u8], a: usize, b: usize) -> bool {
    buf[a..a + 4] == buf[b..b + 4]
}

/// `ChangePair` (:41): is the bigger distance more than 64x the smaller? Used to
/// reject a one-byte-longer match that costs more to encode than it saves.
#[inline]
fn change_pair(small_dist: usize, big_dist: usize) -> bool {
    big_dist / 64 > small_dist
}

/// What every match finder exposes to the compression loop.
pub trait MatchFinder {
    /// Shortest match this finder will report (`min_length`, :99).
    fn min_length(&self) -> u32;

    /// Find the longest match for `buf[p..]`, updating the hash table. Returns
    /// the length; the position is read back with [`get_matchptr`].
    ///
    /// [`get_matchptr`]: MatchFinder::get_matchptr
    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, prevlen: u32) -> u32;

    /// Position of the match reported by the last `find_matchlen`.
    fn get_matchptr(&self) -> usize;

    /// Record the positions covered by a match just emitted.
    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32);

    /// Reset every slot; called when a non-sliding buffer is refilled.
    fn clear_hash(&mut self);

    /// Fix up stored offsets after the window slid `shift` bytes back.
    fn shift(&mut self, shift: usize);

    /// Drop any match a lazy wrapper is holding.
    fn invalidate_match(&mut self) {}

    fn error(&self) -> Option<c_int>;
}

/// `BaseMatchFinder` (:80): the hash table and the geometry shared by all of
/// them.
pub struct Base {
    pub hash_size: usize,
    pub hash_shift: u32,
    pub hash_mask: u32,
    pub table: Vec<u32>,
    pub hash_row_width: usize,
    /// Position of the last match found (`q`).
    pub q: usize,
}

impl Base {
    /// `BaseMatchFinder::BaseMatchFinder` (:120).
    pub fn new(hashsize: u32, hash_row_width: i32) -> Self {
        let hash_row_width = hash_row_width.max(1) as usize;
        // HashSize counts u32 entries: (1 << lb(hashsize)) / sizeof(PtrVal).
        let hash_size = ((1u64 << lb(hashsize.max(4))) / 4) as usize;
        let hash_size = hash_size.max(1);
        let hash_shift = 32 - lb(hash_size as u32);
        let hash_mask =
            (hash_size as u32 - 1) & !(roundup_pow2(hash_row_width as u32) - 1);
        Base {
            hash_size,
            hash_shift,
            hash_mask,
            // The C mallocs and then clear_hash()es; every finder's constructor
            // does both, so filling with the empty marker here is the same.
            table: vec![1u32; hash_size + hash_row_width],
            hash_row_width,
            q: 1,
        }
    }

    /// `hash` (:103) -- the 4-byte hash. The multiply wraps at 32 bits.
    ///
    /// The shift is `32 - lb(HashSize)`, so a `HashSize` of 1 asks for a shift
    /// of 32 -- undefined in C and a panic in Rust. That only arises when
    /// `hashsize` is 0, which is preset 0 (STORING), where the C's own
    /// `lb(0)` is already undefined (`__builtin_clz(0)`, Common.h:514). The mask
    /// is 0 in that case, so every hash is slot 0 whatever the shift yields;
    /// producing 0 here keeps it defined without changing any reachable result.
    #[inline]
    pub fn hash(&self, x: u32) -> usize {
        let prod = x.wrapping_mul(123456791);
        let shifted = if self.hash_shift >= 32 { 0 } else { prod >> self.hash_shift };
        (shifted & self.hash_mask) as usize
    }

    /// `hashx` (:47) -- hashes 4..7 bytes for the wider finders.
    #[inline]
    pub fn hashx(&self, n: usize, buf: &[u8], p: usize) -> usize {
        let a = value32(buf, p).wrapping_mul(123456791);
        let h = match n {
            4 => a,
            5 => a.wrapping_add(value32(buf, p + 1).wrapping_mul(789567123)),
            6 => a.wrapping_add(value32(buf, p + 2).wrapping_mul(789567123)),
            7 => a.wrapping_add(value32(buf, p + 3).wrapping_mul(789567123)),
            _ => return 0,
        };
        let shifted = if self.hash_shift >= 32 { 0 } else { h >> self.hash_shift };
        (shifted & self.hash_mask) as usize
    }

    /// `clear_hash` (:137). Offset 1, not 0 -- see the module note.
    pub fn clear_hash(&mut self) {
        self.table.fill(1);
    }

    /// `shift` (:145). Entries that would fall below the buffer start are reset
    /// to the empty marker rather than going negative.
    pub fn shift(&mut self, shift: usize) {
        let s = shift as u32;
        for e in self.table.iter_mut() {
            *e = if *e > s { *e - s } else { 1 };
        }
    }

    /// The length limits `accept_match` applies to short matches (:60-72): a
    /// 4-byte match is only worth it within 48 KB, 5 within 192 KB, 6 within
    /// 1 MB. `MatchFinderN` applies the same three inline.
    #[inline]
    fn too_far(len: u32, dist: usize) -> bool {
        (len == 4 && dist >= 48 * KB) || (len == 5 && dist >= 192 * KB) || (len == 6 && dist >= MB)
    }
}

/// The two-stage extension loop shared by the finders: four bytes at a time
/// while that stays in bounds, then one at a time.
///
/// `strict` picks between the C's two spellings of the first bound --
/// `p+len+4 < bufend` in `MatchFinder1`/`MatchFinder2`, `p+len+4 <= bufend` in
/// `MatchFinderN`. They differ by one byte of reach and both are transcribed as
/// written.
#[inline]
fn extend(buf: &[u8], p: usize, q: usize, bufend: usize, from: u32, strict: bool) -> u32 {
    let mut len = from as usize;
    loop {
        let ok = if strict { p + len + 4 < bufend } else { p + len + 4 <= bufend };
        if !ok || !val32equ(buf, p + len, q + len) {
            break;
        }
        len += 4;
    }
    while p + len < bufend && buf[p + len] == buf[q + len] {
        len += 1;
    }
    len as u32
}

// ---------------------------------------------------------------------------
// MatchFinder1 -- one slot per hash value
// ---------------------------------------------------------------------------

/// `MatchFinder1` (:156). Used by presets -1 and -2.
pub struct MatchFinder1 {
    b: Base,
}

impl MatchFinder1 {
    pub fn new(hashsize: u32, hash_row_width: i32) -> Self {
        MatchFinder1 { b: Base::new(hashsize, hash_row_width) }
    }
}

impl MatchFinder for MatchFinder1 {
    fn min_length(&self) -> u32 {
        4
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let h = self.b.hash(value32(buf, p));
        let q = self.b.table[h] as usize;
        self.b.table[h] = p as u32;
        self.b.q = q;
        if val32equ(buf, p, q) {
            extend(buf, p, q, bufend, self.min_length() - 1, true)
        } else {
            self.min_length() - 1
        }
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    /// No hash update in the fastest mode -- the C's body is commented out
    /// (:178-184), which is a deliberate speed trade, not dead code.
    fn update_hash(&mut self, _buf: &[u8], _p: usize, _len: u32, _step: u32) {}

    fn clear_hash(&mut self) {
        self.b.clear_hash()
    }
    fn shift(&mut self, shift: usize) {
        self.b.shift(shift)
    }
    fn error(&self) -> Option<c_int> {
        None
    }
}

// ---------------------------------------------------------------------------
// MatchFinder2 -- two slots per hash value
// ---------------------------------------------------------------------------

/// `MatchFinder2` (:193). Used by preset -3.
pub struct MatchFinder2 {
    b: Base,
}

impl MatchFinder2 {
    pub fn new(hashsize: u32, hash_row_width: i32) -> Self {
        MatchFinder2 { b: Base::new(hashsize, hash_row_width) }
    }
}

impl MatchFinder for MatchFinder2 {
    fn min_length(&self) -> u32 {
        4
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let minlen = self.min_length();
        let h = self.b.hash(value32(buf, p));
        let q1 = self.b.table[h + 1] as usize;
        self.b.table[h + 1] = self.b.table[h];
        let q = self.b.table[h] as usize;
        self.b.table[h] = p as u32;
        self.b.q = q;

        if val32equ(buf, p, q) {
            let mut len = extend(buf, p, q, bufend, minlen - 1, true);
            // The second slot is only tried when its byte at the current match
            // end matches -- a cheap filter before the full comparison.
            if buf[p + len as usize] == buf[q1 + len as usize] {
                let mut len1 = 0usize;
                while p + len1 < bufend && buf[p + len1] == buf[q1 + len1] {
                    len1 += 1;
                }
                if len1 as u32 > len {
                    len = len1 as u32;
                    self.b.q = q1;
                }
            }
            len
        } else if val32equ(buf, p, q1) {
            self.b.q = q1;
            extend(buf, p, q1, bufend, minlen - 1, true)
        } else {
            minlen - 1
        }
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    /// `update_hash` (:226). `len` may be as low as 1 when lazy matching and the
    /// 3-byte hash are combined, so `p+len-2` can precede `p`.
    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, _step: u32) {
        let mut push = |b: &mut Base, at: usize| {
            let h = b.hash(value32(buf, at));
            b.table[h + 1] = b.table[h];
            b.table[h] = at as u32;
        };
        push(&mut self.b, p + 1);
        let e = p + len as usize;
        push(&mut self.b, e - 2);
        push(&mut self.b, e - 1);
    }

    fn clear_hash(&mut self) {
        self.b.clear_hash()
    }
    fn shift(&mut self, shift: usize) {
        self.b.shift(shift)
    }
    fn error(&self) -> Option<c_int> {
        None
    }
}

// ---------------------------------------------------------------------------
// MatchFinderN -- hash_row_width slots per hash value, N-byte hash
// ---------------------------------------------------------------------------

/// `MatchFinderN<N>` (:242). The general case behind presets -4 and up, reached
/// through the caching finders; kept here because its extension loop is the one
/// the others are compared against.
pub struct MatchFinderN {
    b: Base,
    n: usize,
}

impl MatchFinderN {
    pub fn new(n: usize, hashsize: u32, hash_row_width: i32) -> Self {
        MatchFinderN { b: Base::new(hashsize, hash_row_width), n }
    }

    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        let h = self.b.hashx(self.n, buf, p);
        for j in (1..self.b.hash_row_width).rev() {
            self.b.table[h + j] = self.b.table[h + j - 1];
        }
        self.b.table[h] = p as u32;
    }
}

impl MatchFinder for MatchFinderN {
    fn min_length(&self) -> u32 {
        4
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let minlen = self.min_length();
        let h = self.b.hashx(self.n, buf, p);
        let mut len = minlen - 1;
        let mut x0 = self.b.table[h];
        self.b.table[h] = p as u32;
        let q = x0 as usize;
        self.b.q = q;

        if val32equ(buf, p, q) {
            len = extend(buf, p, q, bufend, minlen, false);
            if Base::too_far(len, p - q) {
                len = minlen - 1;
            }
        }

        for j in 1..self.b.hash_row_width {
            let x1 = self.b.table[h + j];
            self.b.table[h + j] = x0;
            x0 = x1;
            let q1 = x1 as usize;
            // One 4-byte probe at the far end of the current best match, so a
            // candidate that cannot beat it is rejected without a full compare.
            let at = p + len as usize + 1 - minlen as usize;
            let at1 = q1 + len as usize + 1 - minlen as usize;
            if val32equ(buf, at, at1) {
                let mut len1 = 0usize;
                while p + len1 < bufend && buf[p + len1] == buf[q1 + len1] {
                    len1 += 1;
                }
                let mut len1 = len1 as u32;
                if Base::too_far(len1, p - q1) {
                    len1 = minlen - 1;
                }
                // A match one byte longer but 64x further away costs more to
                // encode than it saves, so it is not taken.
                if len1 > len && !(len1 == len + 1 && change_pair(p - self.b.q, p - q1)) {
                    len = len1;
                    self.b.q = q1;
                }
            }
        }
        len
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        if len > 1 {
            self.update_hash1(buf, p + 1);
        }
        let mut i = 2i64;
        // `i < len-1` in the C, with len unsigned -- len 0 or 1 would wrap and
        // run away, but update_hash is only called after a match of at least
        // min_length. Signed arithmetic here makes that harmless either way.
        while i < len as i64 - 1 {
            self.update_hash1(buf, p + i as usize);
            i += step.max(1) as i64;
        }
        if len > 3 {
            self.update_hash1(buf, p + len as usize - 1);
        }
    }

    fn clear_hash(&mut self) {
        self.b.clear_hash()
    }
    fn shift(&mut self, shift: usize) {
        self.b.shift(shift)
    }
    fn error(&self) -> Option<c_int> {
        None
    }
}
