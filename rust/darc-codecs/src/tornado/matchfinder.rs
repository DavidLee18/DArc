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

    /// Insert the single position `p`. Only the finders that `CombineMF` wraps
    /// define this in the C; the default panics in debug rather than silently
    /// skipping an insertion, which would change the parse without failing.
    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        let _ = (buf, p);
        debug_assert!(false, "update_hash1 called on a finder that does not define it");
    }

    /// Reset every slot; called when a non-sliding buffer is refilled.
    ///
    /// Takes the buffer because the caching finders store a key derived from
    /// the bytes at the empty-slot position, and `clear_hash` is called again
    /// mid-stream when `m.shift == -1` refills a non-sliding window -- at which
    /// point those bytes are real data, not the zeroed buffer.
    fn clear_hash(&mut self, buf: &[u8]);

    /// Fix up stored offsets after the window slid `shift` bytes back.
    fn shift(&mut self, shift: usize);

    /// Drop any match a lazy wrapper is holding.
    fn invalidate_match(&mut self) {}

    fn error(&self) -> Option<c_int>;
}

/// Forwarding impl so a finder can be chosen at run time and still be composed
/// into `CombineMF`, which is generic. The C picks between the three
/// `caching_finder` arms at compile time; here the arms differ only in what the
/// auxiliary side wraps, so one boxed child keeps them a single code path.
impl MatchFinder for Box<dyn MatchFinder + '_> {
    fn min_length(&self) -> u32 {
        (**self).min_length()
    }
    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, prevlen: u32) -> u32 {
        (**self).find_matchlen(buf, p, bufend, prevlen)
    }
    fn get_matchptr(&self) -> usize {
        (**self).get_matchptr()
    }
    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        (**self).update_hash(buf, p, len, step)
    }
    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        (**self).update_hash1(buf, p)
    }
    fn clear_hash(&mut self, buf: &[u8]) {
        (**self).clear_hash(buf)
    }
    fn shift(&mut self, shift: usize) {
        (**self).shift(shift)
    }
    fn invalidate_match(&mut self) {
        (**self).invalidate_match()
    }
    fn error(&self) -> Option<c_int> {
        (**self).error()
    }
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

    fn clear_hash(&mut self, _buf: &[u8]) {
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

    fn clear_hash(&mut self, _buf: &[u8]) {
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

    fn clear_hash(&mut self, _buf: &[u8]) {
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
// CachingMatchFinder -- 4 bytes of the string cached in the table itself
// ---------------------------------------------------------------------------

/// `accept_match` (:60). Returns the length, or **0** -- not `MINLEN-1` -- when
/// the candidate is rejected, which the caller turns into a literal.
///
/// The distance limits say a short match too far away is not worth its encoded
/// size. The `p <= bufend` test really is on `p` rather than `p+len`; it is
/// transcribed as written.
fn accept_match(len: u32, buf: &[u8], p: usize, q: usize, bufend: usize) -> u32 {
    if p > bufend {
        return 0;
    }
    let dist = p - q;
    let eq = |a: usize, b: usize, n: usize| buf[p + a..p + a + n] == buf[q + b..q + b + n];
    match len {
        4 => {
            if dist < 48 * KB && eq(0, 0, 4) {
                4
            } else {
                0
            }
        }
        5 => {
            if dist < 192 * KB && eq(0, 0, 4) && buf[p + 4] == buf[q + 4] {
                5
            } else {
                0
            }
        }
        6 => {
            if dist < MB && eq(0, 0, 4) && eq(4, 4, 2) {
                6
            } else {
                0
            }
        }
        7 => {
            if dist < 12 * MB && eq(0, 0, 4) && eq(4, 4, 3) {
                7
            } else {
                0
            }
        }
        8 => {
            if eq(0, 0, 4) && eq(4, 4, 4) {
                8
            } else {
                0
            }
        }
        9 => {
            if eq(0, 0, 4) && eq(4, 4, 4) && buf[p + 8] == buf[q + 8] {
                9
            } else {
                0
            }
        }
        _ => 0,
    }
}

/// `CachingMatchFinder<N>` (:359). Used by preset 4 (`caching_finder == 1`).
///
/// Each row slot is a *pair*: the position, and four cached bytes of the string
/// it points at (`key(p) = value32(p+N-1)`). Comparing the cached key against
/// the current one tells you how far the strings agree without touching the
/// buffer at all -- the low zero bytes of `cachedKey ^ key(p)` count the
/// matching bytes, which is what the five-state scan below dispatches on.
///
/// The states only ever advance (0 -> 4 -> 5 -> 6 -> 7), which is what makes the
/// C's `goto` chain expressible as a loop over a state variable rather than
/// needing real unstructured jumps.
pub struct CachingMatchFinder {
    b: Base,
    n: usize,
}

impl CachingMatchFinder {
    /// The C passes `_hash_row_width*2` to the base so the mask aligns rows to
    /// whole pairs, then overwrites `hash_row_width` with the unmultiplied value
    /// (:362-366).
    pub fn new(n: usize, hashsize: u32, hash_row_width: i32) -> Self {
        let mut b = Base::new(hashsize, hash_row_width.saturating_mul(2));
        b.hash_row_width = hash_row_width.max(1) as usize;
        CachingMatchFinder { b, n }
    }

    /// `key` (:374) -- the four bytes at `p+N-1`.
    #[inline]
    fn key(&self, buf: &[u8], p: usize) -> u32 {
        value32(buf, p + self.n - 1)
    }
}

impl MatchFinder for CachingMatchFinder {
    fn min_length(&self) -> u32 {
        4
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let minlen = self.min_length();
        let n = self.n as u32;
        let h = self.b.hash(value32(buf, p));
        let tabend = h + self.b.hash_row_width * 2;
        let mut table = h;
        let key_p = self.key(buf, p);

        // x1/v1 start as the values that will be written into the first slot.
        let mut x1 = p as u32;
        let mut v1 = key_p;
        let mut t: u32 = 0;

        // `next_pair` (:394): read a pair, write the previous one back, and xor
        // the cached key against the current one.
        macro_rules! next_pair {
            () => {{
                let x0 = x1;
                x1 = self.b.table[table];
                self.b.table[table] = x0;
                table += 1;
                let v0 = v1;
                v1 = self.b.table[table];
                self.b.table[table] = v0;
                table += 1;
                t = v1 ^ key_p;
            }};
        }

        let mut state = 0u32;
        loop {
            match state {
                // No match yet: one matching cached byte is enough to advance.
                0 => {
                    while table != tabend {
                        next_pair!();
                        if t & 0xff == 0 {
                            state = if t == 0 {
                                7
                            } else if t & 0xff00 != 0 {
                                4
                            } else if t & 0xff0000 != 0 {
                                5
                            } else {
                                6
                            };
                            break;
                        }
                    }
                    if state == 0 {
                        return minlen - 1;
                    }
                    self.b.q = x1 as usize;
                }
                4 => {
                    while table != tabend {
                        next_pair!();
                        if t & 0xffff == 0 {
                            state = if t == 0 {
                                7
                            } else if t & 0xff0000 != 0 {
                                5
                            } else {
                                6
                            };
                            break;
                        }
                    }
                    if state == 4 {
                        return accept_match(n, buf, p, self.b.q, bufend);
                    }
                    self.b.q = x1 as usize;
                }
                5 => {
                    while table != tabend {
                        next_pair!();
                        if t & 0xffffff == 0 {
                            state = if t == 0 { 7 } else { 6 };
                            break;
                        }
                    }
                    if state == 5 {
                        return accept_match(n + 1, buf, p, self.b.q, bufend);
                    }
                    self.b.q = x1 as usize;
                }
                6 => {
                    while table != tabend {
                        next_pair!();
                        if t == 0 {
                            state = 7;
                            break;
                        }
                    }
                    if state == 6 {
                        return accept_match(n + 2, buf, p, self.b.q, bufend);
                    }
                    self.b.q = x1 as usize;
                }
                _ => {
                    // A full key match: measure the real length in the buffer.
                    // Note both loops bound on `p+len < bufend`; the four-at-a-
                    // time loop has no `+4` here, unlike MatchFinder1's.
                    let mut len = minlen - 1;
                    if val32equ(buf, p, self.b.q) {
                        let q = self.b.q;
                        let mut l = (minlen - 1).min(4) as usize;
                        while p + l < bufend && val32equ(buf, p + l, q + l) {
                            l += 4;
                        }
                        while p + l < bufend && buf[p + l] == buf[q + l] {
                            l += 1;
                        }
                        len = l as u32;
                    }
                    while table != tabend {
                        next_pair!();
                        let q1 = x1 as usize;
                        if t == 0
                            && buf[p + len as usize] == buf[q1 + len as usize]
                            && val32equ(buf, p, q1)
                        {
                            let mut l1 = (minlen - 1).min(4) as usize;
                            while p + l1 < bufend && val32equ(buf, p + l1, q1 + l1) {
                                l1 += 4;
                            }
                            while p + l1 < bufend && buf[p + l1] == buf[q1 + l1] {
                                l1 += 1;
                            }
                            if l1 as u32 > len {
                                len = l1 as u32;
                                self.b.q = q1;
                            }
                        }
                    }
                    return len;
                }
            }
        }
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    /// `update_hash1` (:462): shift the row down by one *pair* and put the new
    /// position/key at the head.
    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        let mut push = |mf: &mut Self, at: usize| {
            let h = mf.b.hash(value32(buf, at));
            let mut j = mf.b.hash_row_width;
            // `for (int j=hash_row_width; j-=2; )` -- pre-decrement by two, stop
            // at zero. With an odd row width this walks past zero in the C; the
            // widths in use are all even.
            while j >= 2 {
                j -= 2;
                if j == 0 {
                    break;
                }
                mf.b.table[h + j] = mf.b.table[h + j - 2];
                mf.b.table[h + j + 1] = mf.b.table[h + j - 1];
            }
            mf.b.table[h] = at as u32;
            let k = mf.key(buf, at);
            mf.b.table[h + 1] = k;
        };
        if len > 1 {
            push(self, p + 1);
        }
        let mut i = 2i64;
        while i < len as i64 - 1 {
            push(self, p + i as usize);
            i += step.max(1) as i64;
        }
        if len > 3 {
            push(self, p + len as usize - 1);
        }
    }

    /// `clear_hash` (:482): positions get the empty marker, keys get the key of
    /// the byte the marker points at.
    fn clear_hash(&mut self, buf: &[u8]) {
        let k = self.key(buf, 1);
        for i in (0..self.b.table.len() - 1).step_by(2) {
            self.b.table[i] = 1;
            self.b.table[i + 1] = k;
        }
    }

    /// `shift` (:495): only the *position* half of each pair is rebased; the
    /// cached key belongs to the string, not to where it sits.
    fn shift(&mut self, shift: usize) {
        let s = shift as u32;
        for i in (0..self.b.table.len()).step_by(2) {
            self.b.table[i] = if self.b.table[i] > s { self.b.table[i] - s } else { 1 };
        }
    }

    fn error(&self) -> Option<c_int> {
        None
    }
}

// ---------------------------------------------------------------------------
// Wrappers: LazyMatching and Hash3
// ---------------------------------------------------------------------------

/// `value16` / `value24` (Common.h:250-251). `value24` loads **four** bytes and
/// masks, so it needs the same slack past `bufend` as a 32-bit load.
#[inline]
fn value16(buf: &[u8], at: usize) -> u32 {
    u16::from_le_bytes([buf[at], buf[at + 1]]) as u32
}

#[inline]
fn value24(buf: &[u8], at: usize) -> u32 {
    value32(buf, at) & 0xff_ffff
}

/// `LazyMatching<MatchFinder>` (:673). Looks one byte ahead and prefers the
/// match at `p+1` when it is enough better, which is what "lazy" means here.
///
/// Two details are load-bearing. The lookahead match may be extended one byte
/// *backwards* (`nextq[-1] == *p`), which is the reason the empty hash slot is
/// offset 1 rather than 0 -- at offset 0 that read would leave the buffer. And
/// `invalidate_match` resets `nextlen` to `MINLEN-1`, not to 0, because the
/// position has already been inserted into the hash and a fresh search could
/// return `q == p` (:753).
pub struct LazyMatching<M: MatchFinder> {
    mf: M,
    nextlen: u32,
    prevq: usize,
    /// Signed: `shift` rebases it unconditionally, and the C lets the pointer
    /// go below the buffer when no match is pending.
    nextq: isize,
}

impl<M: MatchFinder> LazyMatching<M> {
    pub fn new(mf: M) -> Self {
        LazyMatching { mf, nextlen: 0, prevq: 1, nextq: 1 }
    }
}

impl<M: MatchFinder> MatchFinder for LazyMatching<M> {
    fn min_length(&self) -> u32 {
        self.mf.min_length()
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, prevlen_in: u32) -> u32 {
        let minlen = self.min_length();
        if self.nextlen == 0 {
            self.nextlen = self.mf.find_matchlen(buf, p, bufend, prevlen_in);
            self.nextq = self.mf.get_matchptr() as isize;
        }
        let mut prevlen = self.nextlen;
        self.prevq = self.nextq.max(0) as usize;

        self.nextlen = self.mf.find_matchlen(buf, p + 1, bufend, prevlen);
        self.nextq = self.mf.get_matchptr() as isize;

        let nextdist = (p + 1) as isize - self.nextq;
        let prevdist = p as isize - self.prevq as isize;

        // Extend the lookahead match one char backwards if that beats the match
        // at p. The four alternatives are transcribed from :720-723.
        let can_extend_back = self.nextq >= 1 && buf[(self.nextq - 1) as usize] == buf[p];
        if self.nextlen >= minlen
            && can_extend_back
            && ((self.nextlen + 1 >= prevlen && nextdist < prevdist)
                || (self.nextlen + 1 == prevlen + 1
                    && !change_pair(prevdist.max(0) as usize, nextdist.max(0) as usize))
                || (self.nextlen + 1 > prevlen + 1)
                || (self.nextlen + 2 >= prevlen
                    && prevlen >= minlen
                    && change_pair(nextdist.max(0) as usize, prevdist.max(0) as usize)))
        {
            prevlen = self.nextlen + 1;
            self.prevq = (self.nextq - 1) as usize;
            return prevlen;
        }

        // Otherwise drop the current match entirely if the next one is better
        // (LZMA's rule, :732-735).
        if (self.nextlen >= prevlen && nextdist < prevdist / 4)
            || (self.nextlen == prevlen + 1
                && !change_pair(prevdist.max(0) as usize, nextdist.max(0) as usize))
            || (self.nextlen > prevlen + 1)
            || (self.nextlen + 1 >= prevlen
                && prevlen >= minlen
                && change_pair(nextdist.max(0) as usize, prevdist.max(0) as usize))
        {
            minlen - 1
        } else {
            prevlen
        }
    }

    fn get_matchptr(&self) -> usize {
        self.prevq
    }

    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        self.mf.update_hash(buf, p + 1, len.saturating_sub(1), step);
        self.nextlen = 0;
    }

    fn clear_hash(&mut self, buf: &[u8]) {
        self.mf.clear_hash(buf);
        self.nextlen = 0;
    }

    fn shift(&mut self, shift: usize) {
        self.mf.shift(shift);
        self.nextq -= shift as isize;
    }

    fn invalidate_match(&mut self) {
        self.mf.invalidate_match();
        // Not 0: p is already in the hash, so a fresh search could return q==p.
        self.nextlen = self.min_length() - 1;
    }

    fn error(&self) -> Option<c_int> {
        self.mf.error()
    }
}

/// `Hash3<MatchFinder, HASH3_LOG, HASH2_LOG, FULL_UPDATE>` (:763). Adds two
/// small direct-mapped tables for 3-byte and 2-byte matches, tried only when the
/// wrapped finder comes up short.
///
/// **This changes `min_length()` to 2**, which is not a detail: the compression
/// loop writes `mf.min_length()` into the stream header and passes it to the
/// coder as MINLEN, so wrapping a finder in `Hash3` changes what a given length
/// code means on both sides.
pub struct Hash3<M: MatchFinder> {
    mf: M,
    hash3_log: u32,
    hash2_log: u32,
    full_update: bool,
    t3: Vec<u32>,
    t2: Vec<u32>,
    q: usize,
}

impl<M: MatchFinder> Hash3<M> {
    pub fn new(mf: M, hash3_log: u32, hash2_log: u32, full_update: bool) -> Self {
        Hash3 {
            mf,
            hash3_log,
            hash2_log,
            full_update,
            t3: vec![1u32; 1 << hash3_log],
            t2: vec![1u32; 1 << hash2_log],
            q: 1,
        }
    }

    /// `hash` (:779) -- note there is no mask; the shift alone bounds it.
    #[inline]
    fn h3(&self, x: u32) -> usize {
        (x.wrapping_mul(234567913) >> (32 - self.hash3_log)) as usize
    }

    #[inline]
    fn h2(&self, x: u32) -> usize {
        (x.wrapping_mul(123456791) >> (32 - self.hash2_log)) as usize
    }

    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        let h = self.h3(value24(buf, p));
        self.t3[h] = p as u32;
        if self.full_update {
            let h = self.h2(value16(buf, p));
            self.t2[h] = p as u32;
        }
    }
}

impl<M: MatchFinder> MatchFinder for Hash3<M> {
    /// Forwards to the inherent `Hash3::update_hash1`. Without this the trait's
    /// DEFAULT body is what `CombineMF` reaches, because its auxiliary finder is
    /// a `Box<dyn MatchFinder>` -- and that default is a `debug_assert!` which
    /// compiles out in release, i.e. a silent no-op.
    ///
    /// That was a real divergence, not a theoretical one. `CombineMF` calls
    /// `mf2.update_hash1(p)` on its early exit (`len1 > mf1.MINLEN`), which is
    /// how the C seeds the auxiliary 2/3-byte tables at positions it skips. The
    /// port made 3397 of those insertions where the C made 4494; the first
    /// missed one was at p=0x268, and it surfaced ~28 KB later as a 3-byte match
    /// the C found and the port did not. Presets 7-11 are affected because they
    /// are the only ones that pair this finder with `CombineMF`.
    ///
    /// The debug_assert would have caught it -- the difftests build --release.
    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        Hash3::update_hash1(self, buf, p)
    }

    /// Two, not the wrapped finder's four.
    fn min_length(&self) -> u32 {
        2
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, prevlen: u32) -> u32 {
        let len = self.mf.find_matchlen(buf, p, bufend, prevlen);
        self.q = self.mf.get_matchptr();

        // `len < mf.MINLEN` -- the *wrapped* finder's minimum, not this one's.
        if len < self.mf.min_length() {
            let h = self.h3(value24(buf, p));
            let q = self.t3[h] as usize;
            self.t3[h] = p as u32;
            self.q = q;
            if p - q < 6 * KB && p + 3 <= bufend && value24(buf, p) == value24(buf, q) {
                let h2 = self.h2(value16(buf, p));
                self.t2[h2] = p as u32;
                return 3;
            }
            let h2 = self.h2(value16(buf, p));
            let q = self.t2[h2] as usize;
            self.t2[h2] = p as u32;
            self.q = q;
            if p - q < 256 && p + 2 <= bufend && value16(buf, p) == value16(buf, q) {
                return 2;
            }
            return self.min_length() - 1;
        }

        let h = self.h3(value24(buf, p));
        self.t3[h] = p as u32;
        let h2 = self.h2(value16(buf, p));
        self.t2[h2] = p as u32;
        len
    }

    fn get_matchptr(&self) -> usize {
        self.q
    }

    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        self.mf.update_hash(buf, p, len, step);
        if self.full_update {
            for i in 1..len as usize {
                self.update_hash1(buf, p + i);
            }
        } else {
            if len > 1 {
                self.update_hash1(buf, p + 1);
            }
            if len > 3 {
                self.update_hash1(buf, p + len as usize - 1);
            }
        }
    }

    fn clear_hash(&mut self, buf: &[u8]) {
        self.mf.clear_hash(buf);
        self.t3.fill(1);
        self.t2.fill(1);
    }

    fn shift(&mut self, shift: usize) {
        self.mf.shift(shift);
        let s = shift as u32;
        for e in self.t3.iter_mut().chain(self.t2.iter_mut()) {
            *e = if *e > s { *e - s } else { 1 };
        }
    }

    fn invalidate_match(&mut self) {
        self.mf.invalidate_match();
    }

    fn error(&self) -> Option<c_int> {
        self.mf.error()
    }
}

// ---------------------------------------------------------------------------
// ExactMatchFinder -- reports N-byte matches only
// ---------------------------------------------------------------------------

/// `ExactMatchFinder<N>` (:307). Returns exactly `N` on a hit and nothing
/// longer; it exists to feed short matches into `CombineMF`.
///
/// Two things separate it from `MatchFinderN`: `update_hash1` replaces only the
/// head of the row rather than shifting it (:303), and the scan **abandons the
/// row mid-shift** when a candidate is further than 48 KB away, leaving the
/// remaining entries unshifted (:323).
pub struct ExactMatchFinder {
    b: Base,
    n: usize,
}

impl ExactMatchFinder {
    pub fn new(n: usize, hashsize: u32, hash_row_width: i32) -> Self {
        ExactMatchFinder { b: Base::new(hashsize, hash_row_width), n }
    }
}

impl MatchFinder for ExactMatchFinder {
    fn min_length(&self) -> u32 {
        4
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let h = self.b.hashx(self.n, buf, p);
        let mut x0 = p as u32;
        for j in 0..self.b.hash_row_width {
            let x1 = self.b.table[h + j];
            self.b.table[h + j] = x0;
            x0 = x1;
            let q1 = x1 as usize;
            if p > q1 && p - q1 > 48 * KB {
                return self.min_length() - 1;
            }
            if val32equ(buf, p, q1) && p + self.n <= bufend {
                self.b.q = q1;
                return self.n as u32;
            }
        }
        self.min_length() - 1
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        let h = self.b.hashx(self.n, buf, p);
        self.b.table[h] = p as u32;
    }

    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        if len > 1 {
            self.update_hash1(buf, p + 1);
        }
        let mut i = 2i64;
        while i < len as i64 - 1 {
            self.update_hash1(buf, p + i as usize);
            i += step.max(1) as i64;
        }
        if len > 3 {
            self.update_hash1(buf, p + len as usize - 1);
        }
    }

    fn clear_hash(&mut self, _buf: &[u8]) {
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
// CycledCachingMatchFinder -- caching rows with a moving head instead of a shift
// ---------------------------------------------------------------------------

/// `CycledCachingMatchFinder<N>` (:507). Same cached-key idea as
/// `CachingMatchFinder`, but a row is a ring: `head[h]` names the newest slot
/// and inserting decrements it, so nothing is ever memmoved.
///
/// Consequences that matter for byte-identity:
///
/// * The empty marker is **0**, not 1, and `next_pair` stops the scan dead on
///   it -- so a partly-filled row is not scanned past its end.
/// * `min_length()` is `N`, which for the presets that use this is 5, 6 or 7,
///   not 4.
/// * `hashx` is called without a mask (`HashMask = ~0`); the shift alone bounds
///   the result to `head`'s size.
/// * Several states apply a `ChangePair` test that `CachingMatchFinder` does
///   not, refusing a longer candidate that sits far enough away to cost more
///   than it saves.
pub struct CycledCachingMatchFinder {
    b: Base,
    n: usize,
    head: Vec<u8>,
    head_size: usize,
}

impl CycledCachingMatchFinder {
    pub fn new(n: usize, hashsize: u32, hash_row_width: i32) -> Self {
        let mut row = hash_row_width.max(1) as usize;
        // "Simulate 2gb:256 hash with 2040mb:255 one" (:639-641).
        if hashsize == 0x8000_0000 && row.is_power_of_two() {
            row -= 1;
        }
        let head_size = 1usize << lb((hashsize as usize / (4 * row * 2)).max(1) as u32);
        let hash_size = head_size * row * 2;
        let mut b = Base::new(4, 1); // geometry replaced below
        b.hash_size = hash_size;
        b.hash_row_width = row;
        b.hash_shift = 32 - lb(head_size as u32);
        b.hash_mask = u32::MAX;
        b.table = vec![0u32; hash_size];
        b.q = 0;
        CycledCachingMatchFinder { b, n, head: vec![0u8; head_size], head_size }
    }

    #[inline]
    fn key(&self, buf: &[u8], p: usize) -> u32 {
        value32(buf, p + self.n - 1)
    }
}

impl MatchFinder for CycledCachingMatchFinder {
    /// `N` -- the C overrides this at MatchFinder.cpp:631 (`{return N;}`),
    /// inside a struct body that runs 507-632. Reading only the first ~90 lines
    /// of that body suggests it inherits BaseMatchFinder's 4; it does not, and
    /// forcing 4 here takes the sweep from 10 differing to 173.
    fn min_length(&self) -> u32 {
        self.n as u32
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, _prevlen: u32) -> u32 {
        let minlen = self.min_length();
        let n = self.n as u32;
        let row = self.b.hash_row_width;
        let h = self.b.hashx(self.n, buf, p);
        let i = {
            let cur = self.head[h];
            let next = if cur == 0 { (row - 1) as u8 } else { cur - 1 };
            self.head[h] = next;
            next as usize
        };
        let rowstart = h * row * 2;
        let rowend = rowstart + row * 2;
        let mut table = rowstart + i * 2;
        let tabend = table;
        let key_p = self.key(buf, p);

        self.b.table[table] = p as u32;
        table += 1;
        self.b.table[table] = key_p;
        table += 1;
        if table == rowend {
            table = rowstart;
        }

        let mut x1: u32 = 0;
        let mut t: u32 = 0;
        // `next_pair` (:542): read-only here -- the ring never shifts. `x1 == 0`
        // is the empty marker and ends the scan.
        macro_rules! next_pair {
            () => {{
                x1 = self.b.table[table];
                table += 1;
                if x1 == 0 {
                    break;
                }
                let v1 = self.b.table[table];
                table += 1;
                if table == rowend {
                    table = rowstart;
                }
                t = v1 ^ key_p;
            }};
        }

        let mut state = 0u32;
        loop {
            match state {
                0 => {
                    while table != tabend {
                        next_pair!();
                        if t & 0xff == 0 {
                            state = if t == 0 {
                                7
                            } else if t & 0xff00 != 0 {
                                4
                            } else if t & 0xff0000 != 0 {
                                5
                            } else {
                                6
                            };
                            break;
                        }
                    }
                    if state == 0 {
                        return minlen - 1;
                    }
                    if state != 7 {
                        self.b.q = x1 as usize;
                    }
                }
                4 => {
                    self.b.q = x1 as usize;
                    while table != tabend {
                        next_pair!();
                        if t & 0xffff == 0 {
                            if t == 0 {
                                state = 7;
                                break;
                            } else if t & 0xff0000 != 0 {
                                // Only take the longer candidate if it is not
                                // disproportionately further away.
                                if !change_pair(p - self.b.q, p - x1 as usize) {
                                    state = 5;
                                    break;
                                }
                            } else {
                                state = 6;
                                break;
                            }
                        }
                    }
                    if state == 4 {
                        return accept_match(n, buf, p, self.b.q, bufend);
                    }
                }
                5 => {
                    self.b.q = x1 as usize;
                    while table != tabend {
                        next_pair!();
                        if t & 0xffffff == 0 {
                            if t == 0 {
                                state = 7;
                                break;
                            } else if !change_pair(p - self.b.q, p - x1 as usize) {
                                state = 6;
                                break;
                            }
                        }
                    }
                    if state == 5 {
                        return accept_match(n + 1, buf, p, self.b.q, bufend);
                    }
                }
                6 => {
                    self.b.q = x1 as usize;
                    while table != tabend {
                        next_pair!();
                        if t == 0 {
                            let q1 = x1 as usize;
                            if val32equ(buf, p + self.n, q1 + self.n)
                                || !change_pair(p - self.b.q, p - q1)
                            {
                                state = 7;
                                break;
                            }
                        }
                    }
                    if state == 6 {
                        return accept_match(n + 2, buf, p, self.b.q, bufend);
                    }
                }
                _ => {
                    let mut len = minlen - 1;
                    self.b.q = x1 as usize;
                    let q = self.b.q;
                    if val32equ(buf, p, q) {
                        let mut l = (minlen - 1).min(4) as usize;
                        while p + l < bufend && val32equ(buf, p + l, q + l) {
                            l += 4;
                        }
                        while p + l < bufend && buf[p + l] == buf[q + l] {
                            l += 1;
                        }
                        len = l as u32;
                    }
                    while table != tabend {
                        next_pair!();
                        let q1 = x1 as usize;
                        if t == 0
                            && buf[p + len as usize] == buf[q1 + len as usize]
                            && val32equ(buf, p, q1)
                        {
                            let mut l1 = (minlen - 1).min(4) as usize;
                            while p + l1 < bufend && val32equ(buf, p + l1, q1 + l1) {
                                l1 += 4;
                            }
                            while p + l1 < bufend && buf[p + l1] == buf[q1 + l1] {
                                l1 += 1;
                            }
                            let l1 = l1 as u32;
                            if l1 > len && !(l1 == len + 1 && change_pair(p - self.b.q, p - q1)) {
                                len = l1;
                                self.b.q = q1;
                            }
                        }
                    }
                    return len;
                }
            }
        }
    }

    fn get_matchptr(&self) -> usize {
        self.b.q
    }

    /// `update_hash1` (:616): move the head back one slot and write there.
    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        let row = self.b.hash_row_width;
        let h = self.b.hashx(self.n, buf, p);
        let cur = self.head[h];
        let i = if cur == 0 { (row - 1) as u8 } else { cur - 1 };
        self.head[h] = i;
        let at = (h * row + i as usize) * 2;
        self.b.table[at] = p as u32;
        let k = self.key(buf, p);
        self.b.table[at + 1] = k;
    }

    /// `update_hash` (:625): every position inside the match, no step.
    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, _step: u32) {
        for i in 1..len as usize {
            self.update_hash1(buf, p + i);
        }
    }

    /// Both tables go to zero here, not to the offset-1 marker (:655).
    fn clear_hash(&mut self, _buf: &[u8]) {
        self.b.table.fill(0);
        self.head.fill(0);
        let _ = self.head_size;
    }

    fn shift(&mut self, shift: usize) {
        let s = shift as u32;
        for i in (0..self.b.table.len()).step_by(2) {
            self.b.table[i] = if self.b.table[i] > s { self.b.table[i] - s } else { 0 };
        }
    }

    fn error(&self) -> Option<c_int> {
        None
    }
}

// ---------------------------------------------------------------------------
// CombineMF
// ---------------------------------------------------------------------------

/// `CombineMF<MF1, MF2>` (:886). Tries the long finder first and falls back to
/// the short one, keeping whichever match is better.
///
/// `min_length()` is the *smaller* of the two, which is what reaches the stream
/// header -- so combining a `CycledCachingMatchFinder<5>` with a `Hash3`-wrapped
/// finder gives 2, not 5.
pub struct CombineMF<A: MatchFinder, B: MatchFinder> {
    mf1: A,
    mf2: B,
    q: usize,
}

impl<A: MatchFinder, B: MatchFinder> CombineMF<A, B> {
    pub fn new(mf1: A, mf2: B) -> Self {
        CombineMF { mf1, mf2, q: 0 }
    }
}

impl<A: MatchFinder, B: MatchFinder> MatchFinder for CombineMF<A, B> {
    fn min_length(&self) -> u32 {
        self.mf1.min_length().min(self.mf2.min_length())
    }

    fn find_matchlen(&mut self, buf: &[u8], p: usize, bufend: usize, prevlen: u32) -> u32 {
        let len1 = self.mf1.find_matchlen(buf, p, bufend, prevlen);
        let q1 = self.mf1.get_matchptr();

        // Strictly greater: a match of exactly MINLEN still gets the second
        // finder's opinion.
        if len1 > self.mf1.min_length() {
            self.mf2.update_hash1(buf, p);
            self.q = q1;
            return len1;
        }

        let len2 = self.mf2.find_matchlen(buf, p, bufend, prevlen);
        let q2 = self.mf2.get_matchptr();

        if len1 >= self.mf1.min_length()
            && len1 > len2
            && !(len2 >= self.mf2.min_length()
                && len1 == len2 + 1
                && change_pair(p.saturating_sub(q2), p.saturating_sub(q1)))
        {
            self.q = q1;
            len1
        } else {
            self.q = q2;
            len2
        }
    }

    fn get_matchptr(&self) -> usize {
        self.q
    }

    fn update_hash1(&mut self, buf: &[u8], p: usize) {
        self.mf1.update_hash1(buf, p);
        self.mf2.update_hash1(buf, p);
    }

    fn update_hash(&mut self, buf: &[u8], p: usize, len: u32, step: u32) {
        self.mf1.update_hash(buf, p, len, step);
        self.mf2.update_hash(buf, p, len, step);
    }

    fn clear_hash(&mut self, buf: &[u8]) {
        self.mf1.clear_hash(buf);
        self.mf2.clear_hash(buf);
    }

    fn shift(&mut self, shift: usize) {
        self.mf1.shift(shift);
        self.mf2.shift(shift);
        self.q = self.q.saturating_sub(shift);
    }

    fn invalidate_match(&mut self) {
        self.mf1.invalidate_match();
        self.mf2.invalidate_match();
    }

    fn error(&self) -> Option<c_int> {
        self.mf1.error().or_else(|| self.mf2.error())
    }
}
