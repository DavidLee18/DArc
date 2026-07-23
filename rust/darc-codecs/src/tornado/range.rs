//! Shindler range decoder and its semi-adaptive counter, ported from
//! `Compression/Tornado/EntropyCoder.cpp` (`TRangeDecoder` :555, `TCounter`
//! :602, `ArithDecoder` :703).
//!
//! Like the Huffman back-end this is semi-adaptive: nothing about the model
//! travels in the stream. Both sides start from an even split of `RANGE` points
//! across the alphabet, count every symbol as it is coded, and recompute the
//! coding table whenever `RANGE` new points have accumulated -- so the decoder
//! reproduces `Rescale` exactly or the two sides drift apart silently.
//!
//! The aging step is the subtle part: `livecnt[s] -= (livecnt[s]>1 &&
//! livecnt[s]<6)? 1 : livecnt[s]/6`, i.e. each block starts from roughly 5/6 of
//! the previous counts, with small counts decremented rather than divided so
//! they do not collapse to zero. `remainder` is then how many increments it
//! takes to bring the total back up to `RANGE`.
//!
//! All the arithmetic wraps at 32 bits in C; every operation here is an
//! explicit `wrapping_*` for the same reason it is in `tta.rs` -- Rust would
//! panic in debug on values the format produces routinely.

use super::stream::{InputByteStream, BAD};
use core::ffi::c_int;

/// `RANGE_BITS` / `RANGE` (EntropyCoder.cpp:599-600).
pub const RANGE_BITS: u32 = 14;
pub const RANGE: u32 = 1 << RANGE_BITS;
/// `INDEXES` -- entries in the fast symbol-lookup table (:598).
pub const INDEXES: usize = 2048;
/// `NUM` -- maximum alphabet (:597).
pub const NUM: usize = 2048;
/// Points per index slot; 8 with the constants above.
const PER_INDEX: u32 = RANGE / INDEXES as u32;

pub struct RangeDecoder<'a> {
    pub bytes: InputByteStream<'a>,
    range: u32,
    buffer: u32,
    /// Sticky corrupt-data flag. The C printed to stderr and called `exit(1)`
    /// here, which killed `arc t` on a damaged archive instead of reporting a
    /// bad block; the C has since been fixed to record an error the same way.
    err: Option<c_int>,
}

impl<'a> RangeDecoder<'a> {
    pub fn new(bytes: InputByteStream<'a>) -> Self {
        let mut d = RangeDecoder { bytes, range: 0xffff_ffff, buffer: 0, err: None };
        // Five bytes primed. The C leaves `buffer` uninitialised before this
        // loop, but forty bits of shifting evict every original bit, so
        // starting from zero is identical rather than merely close.
        for _ in 0..5 {
            d.buffer = (d.buffer << 8).wrapping_add(d.bytes.getc());
        }
        d
    }

    /// `GetCount` (:568).
    pub fn get_count(&mut self, bits: u32) -> u32 {
        self.range >>= bits;
        let mut count = if self.range == 0 { 0 } else { self.buffer / self.range };
        if count >= (1u32 << bits) {
            self.err = Some(BAD);
            count = (1u32 << bits) - 1;
        }
        count
    }

    /// `Update` (:576).
    pub fn update(&mut self, cum: u32, cnt: u32) {
        self.buffer = self.buffer.wrapping_sub(cum.wrapping_mul(self.range));
        self.range = self.range.wrapping_mul(cnt);
        while self.range < (1 << 24) {
            self.range <<= 8;
            self.buffer = (self.buffer << 8).wrapping_add(self.bytes.getc());
        }
    }

    pub fn error(&self) -> Option<c_int> {
        self.err.or_else(|| self.bytes.error())
    }
}

/// `TCounter<Decoder>` (:602).
pub struct Counter {
    n: usize,
    remainder: u32,
    cnt: Vec<u32>,
    cum: Vec<u32>,
    livecnt: Vec<u32>,
    index: Vec<u16>,
}

impl Counter {
    pub fn new(n: usize) -> Self {
        let mut c = Counter {
            n,
            remainder: 0,
            cnt: vec![0; n],
            cum: vec![0; n],
            livecnt: vec![0; n],
            index: vec![0; INDEXES + 1],
        };
        // RANGE points split evenly, remainder handed to the first symbols.
        let share = RANGE / n as u32;
        let extra = RANGE - share * n as u32;
        for s in 0..n {
            c.livecnt[s] = share + if (s as u32) < extra { 1 } else { 0 };
        }
        c.rescale();
        c
    }

    #[inline]
    pub fn cum(&self, s: usize) -> u32 {
        self.cum[s]
    }

    #[inline]
    pub fn cnt(&self, s: usize) -> u32 {
        self.cnt[s]
    }

    /// `Inc` (:613).
    #[inline]
    pub fn inc(&mut self, s: usize) {
        self.livecnt[s] += 1;
        // Rescale leaves remainder > 0 (the aged counts always sum to less than
        // RANGE), so this cannot underflow on a well-formed model; saturating
        // rather than wrapping keeps a corrupt one from panicking across the
        // C ABI instead of just rescaling early.
        self.remainder = self.remainder.saturating_sub(1);
        if self.remainder == 0 {
            self.rescale();
        }
    }

    /// `Rescale` (:646).
    fn rescale(&mut self) {
        self.remainder = RANGE;
        let mut total: u32 = 0;
        let mut ind: usize = 0;
        for s in 0..self.n {
            self.cnt[s] = self.livecnt[s];
            self.cum[s] = total;
            total += self.cnt[s];
            let lc = self.livecnt[s];
            self.livecnt[s] -= if lc > 1 && lc < 6 { 1 } else { lc / 6 };
            self.remainder = self.remainder.wrapping_sub(self.livecnt[s]);
            // index[k] ends up holding the first symbol whose range can cover
            // code k*PER_INDEX, which is what makes Decode's search short.
            while ind < INDEXES && self.cum[s] + self.cnt[s] >= PER_INDEX * ind as u32 + 1 {
                self.index[ind] = s as u16;
                ind += 1;
            }
        }
        // A well-formed table has total == RANGE and fills every index slot; if
        // it somehow did not, point the tail at the last symbol rather than
        // leaving stale entries behind.
        while ind < INDEXES {
            self.index[ind] = (self.n - 1) as u16;
            ind += 1;
        }
    }

    /// `Decode` (:624): jump to the index hint, then walk forward to the first
    /// symbol whose range end reaches `count`. Bounded here -- the C walk has
    /// no upper limit and a corrupt count would run it off the end of `cum[]`.
    #[inline]
    pub fn decode(&self, count: u32) -> usize {
        let mut s = self.index[(count / PER_INDEX) as usize] as usize;
        while s < self.n && self.cum[s] + self.cnt[s] < count + 1 {
            s += 1;
        }
        if s >= self.n {
            self.n - 1
        } else {
            s
        }
    }
}

/// `ArithDecoder<EOB>` (:703).
pub struct ArithDecoder<'a> {
    pub rc: RangeDecoder<'a>,
    c: Counter,
}

impl<'a> ArithDecoder<'a> {
    pub fn new(bytes: InputByteStream<'a>, elements: usize) -> Self {
        ArithDecoder { rc: RangeDecoder::new(bytes), c: Counter::new(elements) }
    }

    /// Decode one symbol and count it.
    pub fn decode(&mut self) -> usize {
        let count = self.rc.get_count(RANGE_BITS);
        let x = self.c.decode(count);
        self.rc.update(self.c.cum(x), self.c.cnt(x));
        self.c.inc(x);
        x
    }

    /// `getbits` (:722): raw bits ride the same coder with a flat count of 1,
    /// split into two pieces past 24 bits so the range never underflows.
    pub fn getbits(&mut self, n: u32) -> u32 {
        if n == 0 {
            return 0;
        }
        if n <= 24 {
            let x = self.rc.get_count(n);
            self.rc.update(x, 1);
            x
        } else {
            let x1 = self.rc.get_count(15);
            self.rc.update(x1, 1);
            let x2 = self.rc.get_count(n - 15);
            self.rc.update(x2, 1);
            (x2 << 15) + x1
        }
    }

    pub fn error(&self) -> Option<c_int> {
        self.rc.error()
    }
}
