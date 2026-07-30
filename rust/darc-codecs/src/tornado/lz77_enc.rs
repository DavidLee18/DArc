//! LZ77 literal/match encoders, ported from `Compression/Tornado/LZ77_Coder.cpp`
//! (`LZ77_ByteCoder` :12, `LZ77_BitCoder` :270, `LZ77_Coder` :401,
//! `LZ77_DynamicCoder` :586).
//!
//! The mirror of `lz77.rs`. Four back-ends behind one interface, matching the
//! four the decoder implements; which one runs is decided by `encoding_method`
//! and written into the stream header, so the pairing is not guesswork.
//!
//! ## Two distance conventions, one buffer
//!
//! `LZ77_ByteCoder` and `LZ77_BitCoder` encode `current - match`; `LZ77_Coder`
//! encodes `current - match - 1` and its decoder adds the 1 back. Callers pass
//! the first form and each back-end adjusts, so the off-by-one lives in the one
//! place that cares about it.
//!
//! ## The escape window at length 100
//!
//! `LZ77_Coder` steals four length values to signal a diffed data table.
//! `encode_table` calls `encode_match` with `IMPOSSIBLE_LEN+type`, and lengths
//! above 100 are shifted up by 4 to make room for 101..104. Any real length in
//! that window would collide, which is why the shift is unconditional rather
//! than applied only when a table is being sent.

use super::huffman::HuffmanEncoder;
use super::out_stream::{OutputBitStream, OutputByteStream};
use super::range::ArithEncoder;
use super::vle::Tables;
use super::{Coder, CODES, EOB_CODE, LEN_CODES, REPCHAR, REPDIST_CODES};
use crate::ffi::Io;
use core::ffi::c_int;

/// `IMPOSSIBLE_LEN` (LZ77_Coder.cpp:7).
pub const IMPOSSIBLE_LEN: i32 = i32::MAX / 2;

/// One encoder back-end. `encode` returns 1 for a match, 0 for a literal, as the
/// C's does -- the caller uses it only for statistics.
pub trait Lz77Encoder {
    /// Whether this back-end can carry `encode_table`; false for the two raw
    /// coders, which have no spare code space (`support_tables`, :66/:315/:476).
    fn support_tables(&self) -> bool;

    /// Encode the match at `cur` of length `len` reaching `dist` bytes back, or
    /// a literal when `len < minlen`. `buf[cur]` is the literal byte.
    ///
    /// `dist` is `current - match` in the C's terms. The end-of-stream token is
    /// encoded by calling this with `IMPOSSIBLE_LEN` and `IMPOSSIBLE_DIST`
    /// (Tornado.cpp:209), for which the C forms a match pointer far below the
    /// buffer -- out of bounds, and never dereferenced because the length keeps
    /// it off the literal path. Taking the distance as a number rather than a
    /// position means that pointer never has to exist.
    fn encode(&mut self, len: i32, buf: &[u8], cur: usize, dist: i32, minlen: i32) -> i32;

    /// Signal a diffed data table of `len` elements and row size `kind` (1..4).
    fn encode_table(&mut self, kind: i32, len: i32);

    /// Called after the input window slides, so back-ends holding positions can
    /// drop them (`shift_occurs`, :415).
    fn shift_occurs(&mut self);

    fn put8(&mut self, c: u32);
    fn put32(&mut self, c: u32);
    fn flush(&mut self);
    fn finish(&mut self);
    fn error(&self) -> Option<c_int>;
}

// ---------------------------------------------------------------------------
// BYTECODER
// ---------------------------------------------------------------------------

/// `LZ77_ByteCoder` (:12). Byte-aligned LZSS: two flag bits per item, batched
/// sixteen at a time into a word written *behind* the data it describes.
pub struct ByteCoder<'a> {
    out: OutputByteStream<'a>,
    flags: u32,
    /// Which bit pair the next item occupies. Starts at 0 so the very first
    /// `<<= 2` wraps to 0 and opens the first flag word (:33).
    flagbit: u32,
}

impl<'a> ByteCoder<'a> {
    pub fn new(io: &'a Io, chunk: usize, pad: usize) -> Self {
        // The anchor starts outside the buffer -- see out_stream.rs.
        ByteCoder { out: OutputByteStream::new(io, chunk, pad), flags: 0, flagbit: 0 }
    }
}

impl Lz77Encoder for ByteCoder<'_> {
    fn support_tables(&self) -> bool {
        false
    }

    fn encode(&mut self, len: i32, buf: &[u8], cur: usize, dist: i32, minlen: i32) -> i32 {
        // `flagbit <<= 2` wraps to zero after sixteen items, which is the signal
        // to close the current flag word and reserve the next.
        self.flagbit <<= 2;
        if self.flagbit == 0 {
            self.out.put_at_anchor(self.flags);
            self.flags = 0;
            self.flagbit = 1;
            self.out.set_anchor_here();
            self.out.advance(4);
        }

        if len < minlen {
            self.out.put8(buf[cur] as u32);
            return 0;
        }
        let dist = dist as u32;
        let len_over = (len - minlen) as u32;
        if len < minlen + 16 && dist < (1 << 12) {
            self.out.put16((len_over << 12) + dist);
            self.flags = self.flags.wrapping_add(self.flagbit);
        } else if len < minlen + 64 && dist < (1 << 18) {
            self.out.put24((len_over << 18) + dist);
            self.flags = self.flags.wrapping_add(self.flagbit.wrapping_mul(2));
        } else {
            let mut len = len_over;
            if dist >= (1 << 24) {
                self.out.put8(255);
                self.out.put8(dist >> 24);
            }
            if len >= 254 {
                self.out.put8(254);
                self.out.put24(len >> 8);
                len %= 256;
            }
            // `dist << 8` drops the top byte, which the escape above already
            // sent; the low byte of the word is the length.
            self.out.put32(len.wrapping_add(dist << 8));
            self.flags = self.flags.wrapping_add(self.flagbit.wrapping_mul(3));
        }
        1
    }

    fn encode_table(&mut self, _kind: i32, _len: i32) {
        // `unreachable!`, not `debug_assert!`. The whole body used to be a
        // debug_assert, which compiles out in release -- so a mis-dispatch here
        // silently skipped table encoding and produced a different stream with
        // nothing failing. That is exactly how the Tornado presets 7-11
        // divergence happened one module over (MatchFinder::update_hash1).
        //
        // The encode loop only calls this when `support_tables` is set, and the
        // preset sweep covers `notables` both ways, so this is unreachable in
        // practice -- and now says so in release too.
        unreachable!("encode_table on a coder without table support");
    }

    fn shift_occurs(&mut self) {}

    fn put8(&mut self, c: u32) {
        self.out.put8(c);
    }
    fn put32(&mut self, c: u32) {
        self.out.put32(c);
    }
    fn flush(&mut self) {
        self.out.flush();
    }
    fn finish(&mut self) {
        // The final, partial flag word still has to reach its reserved slot.
        self.out.put_at_anchor(self.flags);
        self.out.finish();
    }
    fn error(&self) -> Option<c_int> {
        self.out.error()
    }
}

// ---------------------------------------------------------------------------
// BITCODER
// ---------------------------------------------------------------------------

/// `LZ77_BitCoder` (:270). Nine bits per item: 0..255 is a literal, and
/// 256 + lcode*32 + dcode is a match followed by the two extra-bit groups.
pub struct BitCoder<'a> {
    out: OutputBitStream<'a>,
    t: Tables,
}

impl<'a> BitCoder<'a> {
    pub fn new(io: &'a Io, chunk: usize, pad: usize) -> Self {
        BitCoder { out: OutputBitStream::new(io, chunk, pad), t: Tables::for_encoding() }
    }
}

impl Lz77Encoder for BitCoder<'_> {
    fn support_tables(&self) -> bool {
        false
    }

    fn encode(&mut self, len: i32, buf: &[u8], cur: usize, dist: i32, minlen: i32) -> i32 {
        let len = len - minlen;
        if len < 0 {
            self.out.putbits(9, buf[cur] as u32);
            return 0;
        }
        let len = len as u32;
        let dist = dist as u32;

        let lcode = self.t.lc_code(len);
        let lbits = self.t.lc_extra[lcode];
        let lbase = self.t.lc_base[lcode];

        let dcode = self.t.dc_code(dist);
        let dbits = self.t.dc_extra[dcode];
        let dbase = self.t.dc_base[dcode];

        self.out.putbits(9, 256 + ((lcode as u32) << 5) + dcode as u32);
        self.out.putlowerbits(lbits as i32, len.wrapping_sub(lbase));
        self.out.putlowerbits(dbits as i32, dist.wrapping_sub(dbase));
        1
    }

    fn encode_table(&mut self, _kind: i32, _len: i32) {
        // `unreachable!`, not `debug_assert!`. The whole body used to be a
        // debug_assert, which compiles out in release -- so a mis-dispatch here
        // silently skipped table encoding and produced a different stream with
        // nothing failing. That is exactly how the Tornado presets 7-11
        // divergence happened one module over (MatchFinder::update_hash1).
        //
        // The encode loop only calls this when `support_tables` is set, and the
        // preset sweep covers `notables` both ways, so this is unreachable in
        // practice -- and now says so in release too.
        unreachable!("encode_table on a coder without table support");
    }

    fn shift_occurs(&mut self) {}

    fn put8(&mut self, c: u32) {
        self.out.bytes.put8(c);
    }
    fn put32(&mut self, c: u32) {
        self.out.bytes.put32(c);
    }
    fn flush(&mut self) {
        self.out.flush();
    }
    fn finish(&mut self) {
        self.out.finish();
    }
    fn error(&self) -> Option<c_int> {
        self.out.error()
    }
}

// ---------------------------------------------------------------------------
// Generic coder over an entropy back-end (HUFCODER / ARICODER)
// ---------------------------------------------------------------------------

/// The entropy layer under `LZ77_Coder`: one symbol, or n raw bits. Mirrors
/// `SymbolDecoder` in `lz77.rs`.
pub trait SymbolEncoder {
    fn encode(&mut self, x: usize);
    fn putlowerbits(&mut self, n: u32, x: u32);
    fn put8(&mut self, c: u32);
    fn put32(&mut self, c: u32);
    fn flush(&mut self);
    fn finish(&mut self);
    fn error(&self) -> Option<c_int>;
}

/// `HuffmanEncoder<EOB_CODE>` plus the bit stream it writes through. The C gets
/// this by inheritance; the two are held side by side here because the encoder
/// needs `&mut` access to both at once.
pub struct HufBackend<'a> {
    huf: HuffmanEncoder,
    bits: OutputBitStream<'a>,
}

impl<'a> HufBackend<'a> {
    pub fn new(io: &'a Io, chunk: usize, pad: usize) -> Self {
        HufBackend {
            huf: HuffmanEncoder::new(CODES, EOB_CODE),
            bits: OutputBitStream::new(io, chunk, pad),
        }
    }
}

impl SymbolEncoder for HufBackend<'_> {
    fn encode(&mut self, x: usize) {
        self.huf.encode(&mut self.bits, x);
    }
    fn putlowerbits(&mut self, n: u32, x: u32) {
        self.bits.putlowerbits(n as i32, x);
    }
    fn put8(&mut self, c: u32) {
        self.bits.bytes.put8(c);
    }
    fn put32(&mut self, c: u32) {
        self.bits.bytes.put32(c);
    }
    fn flush(&mut self) {
        self.bits.flush();
    }
    fn finish(&mut self) {
        self.bits.finish();
    }
    fn error(&self) -> Option<c_int> {
        self.bits.error()
    }
}

impl SymbolEncoder for ArithEncoder<'_> {
    fn encode(&mut self, x: usize) {
        ArithEncoder::encode(self, x);
    }
    fn putlowerbits(&mut self, n: u32, x: u32) {
        ArithEncoder::putlowerbits(self, n, x);
    }
    fn put8(&mut self, c: u32) {
        self.rc.bytes.put8(c);
    }
    fn put32(&mut self, c: u32) {
        self.rc.bytes.put32(c);
    }
    fn flush(&mut self) {
        self.rc.flush();
    }
    fn finish(&mut self) {
        self.rc.finish();
    }
    fn error(&self) -> Option<c_int> {
        self.rc.error()
    }
}

/// `LZ77_Coder<Coder>` (:401).
pub struct GenericEncoder<E: SymbolEncoder> {
    e: E,
    t: Tables,
    /// The four most recent distances, most recent first. The C keeps them in
    /// four named fields and shuffles them by sequential assignment.
    prev: [i32; REPDIST_CODES],
}

impl<E: SymbolEncoder> GenericEncoder<E> {
    pub fn new(e: E) -> Self {
        GenericEncoder { e, t: Tables::for_encoding(), prev: [-1; REPDIST_CODES] }
    }

    /// `encode_match` (:434).
    fn encode_match(&mut self, len: i32, dist: i32) {
        // Look for a repeated distance, shifting the history along as we go --
        // the C's chain of comma expressions does the shift eagerly, so by the
        // time the last test fails the history is already correct for a brand
        // new distance.
        let mut dcode = REPDIST_CODES; // sentinel: not a repeat
        let mut dbits = 0u32;
        let mut dbase = 0u32;
        {
            let old = self.prev;
            self.prev[0] = dist;
            if dist == old[0] {
                dcode = 0;
            } else {
                self.prev[1] = old[0];
                if dist == old[1] {
                    dcode = 1;
                } else {
                    self.prev[2] = old[1];
                    if dist == old[2] {
                        dcode = 2;
                    } else {
                        self.prev[3] = old[2];
                        if dist == old[3] {
                            dcode = 3;
                        }
                    }
                }
            }
        }
        if dcode == REPDIST_CODES {
            let c = self.t.dc_code(dist as u32);
            dbits = self.t.dc_extra[c];
            dbase = self.t.dc_base[c];
            dcode = c + REPDIST_CODES;
        }

        // Lengths above 100 are shifted up by 4 to clear 101..104 for the table
        // signals. A table signal also invalidates the most recent distance,
        // because it is an element count rather than a real distance and the
        // REPCHAR check would index the buffer with it.
        let mut len = len;
        if len > 100 {
            if len > IMPOSSIBLE_LEN {
                self.prev[0] = -1;
                if len <= IMPOSSIBLE_LEN + 4 {
                    len -= IMPOSSIBLE_LEN - 100;
                }
            } else {
                len += 4;
            }
        }

        let lcode = self.t.lc2_code(len as u32);
        let lbits = self.t.lc2_extra[lcode];
        let lbase = self.t.lc2_base[lcode];

        self.e.encode(256 + dcode * LEN_CODES + lcode);
        self.e.putlowerbits(lbits, (len as u32).wrapping_sub(lbase));
        self.e.putlowerbits(dbits, (dist as u32).wrapping_sub(dbase));
    }
}

impl<E: SymbolEncoder> Lz77Encoder for GenericEncoder<E> {
    fn support_tables(&self) -> bool {
        true
    }

    fn encode(&mut self, len: i32, buf: &[u8], cur: usize, dist: i32, minlen: i32) -> i32 {
        let len = len - minlen;
        if len < 0 {
            // REPCHAR: this byte equals the one a `prev[0]+1` distance back, so
            // it costs one symbol instead of a literal. The C writes the test as
            // `*current == current[-prevdist0-1] && prevdist0>=0`, evaluating the
            // load first; with prevdist0 == -1 that load is `current[0]`, so it
            // is in bounds and the comparison is true, and only the second
            // condition rejects it. Testing the guard first is the same result
            // without the pointless load.
            let back = self.prev[0];
            let repeat = back >= 0 && {
                let at = cur as i64 - back as i64 - 1;
                at >= 0 && buf[at as usize] == buf[cur]
            };
            if repeat {
                self.e.encode(REPCHAR);
            } else {
                self.e.encode(buf[cur] as usize);
            }
            return 0;
        }
        self.encode_match(len, dist - 1);
        1
    }

    fn encode_table(&mut self, kind: i32, len: i32) {
        self.encode_match(IMPOSSIBLE_LEN + kind, len - 1);
    }

    fn shift_occurs(&mut self) {
        // Invalidate the last distance: after the window slides it would point
        // at data that is no longer there, and the REPCHAR check indexes with it.
        self.prev[0] = -1;
    }

    fn put8(&mut self, c: u32) {
        self.e.put8(c);
    }
    fn put32(&mut self, c: u32) {
        self.e.put32(c);
    }
    fn flush(&mut self) {
        self.e.flush();
    }
    fn finish(&mut self) {
        self.e.finish();
    }
    fn error(&self) -> Option<c_int> {
        self.e.error()
    }
}

/// `LZ77_DynamicCoder` (:586) -- pick a back-end at run time.
///
/// The C constructs all four and gives the three unused ones a zero `chunk`,
/// which still allocates three output buffers it never writes to. Only the
/// selected one is built here; nothing observable depends on the other three
/// existing, since every method dispatches on `coder` before touching them.
pub enum DynamicCoder<'a> {
    Byte(ByteCoder<'a>),
    Bit(BitCoder<'a>),
    Huf(GenericEncoder<HufBackend<'a>>),
    Ari(GenericEncoder<ArithEncoder<'a>>),
}

impl<'a> DynamicCoder<'a> {
    /// Takes a classified [`Coder`], so there is no failure case left: STORING and
    /// unknown methods are rejected by `Coder::from_stream` at the boundary, which
    /// is where the C reports them too. This used to take a `u32` and return
    /// `Option`, whose `None` arm a newly added coder would have fallen into
    /// silently.
    pub fn new(coder: Coder, io: &'a Io, chunk: usize, pad: usize) -> Self {
        match coder {
            Coder::Byte => DynamicCoder::Byte(ByteCoder::new(io, chunk, pad)),
            Coder::Bit => DynamicCoder::Bit(BitCoder::new(io, chunk, pad)),
            Coder::Huf => DynamicCoder::Huf(GenericEncoder::new(HufBackend::new(io, chunk, pad))),
            Coder::Ari => DynamicCoder::Ari(GenericEncoder::new(ArithEncoder::new(
                OutputByteStream::new(io, chunk, pad),
                CODES,
            ))),
        }
    }

    fn inner(&mut self) -> &mut dyn Lz77Encoder {
        match self {
            DynamicCoder::Byte(c) => c,
            DynamicCoder::Bit(c) => c,
            DynamicCoder::Huf(c) => c,
            DynamicCoder::Ari(c) => c,
        }
    }
}

impl Lz77Encoder for DynamicCoder<'_> {
    fn support_tables(&self) -> bool {
        matches!(self, DynamicCoder::Huf(_) | DynamicCoder::Ari(_))
    }
    fn encode(&mut self, len: i32, buf: &[u8], cur: usize, dist: i32, minlen: i32) -> i32 {
        self.inner().encode(len, buf, cur, dist, minlen)
    }
    fn encode_table(&mut self, kind: i32, len: i32) {
        self.inner().encode_table(kind, len)
    }
    fn shift_occurs(&mut self) {
        self.inner().shift_occurs()
    }
    fn put8(&mut self, c: u32) {
        self.inner().put8(c)
    }
    fn put32(&mut self, c: u32) {
        self.inner().put32(c)
    }
    fn flush(&mut self) {
        self.inner().flush()
    }
    fn finish(&mut self) {
        self.inner().finish()
    }
    fn error(&self) -> Option<c_int> {
        match self {
            DynamicCoder::Byte(c) => c.error(),
            DynamicCoder::Bit(c) => c.error(),
            DynamicCoder::Huf(c) => c.error(),
            DynamicCoder::Ari(c) => c.error(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `SymbolEncoder` that records symbols instead of coding them, so the
    /// encoder's *choices* can be asserted directly.
    ///
    /// This exists because a round-trip cannot see them. The rep-distance
    /// history is the clearest case: break the shuffle so slots 2 and 3 are
    /// never populated, and the encoder simply stops using REPDIST codes 2 and 3
    /// and sends explicit distances instead. The stream stays perfectly valid
    /// and decodes to the right bytes -- it is just bigger. Every round-trip
    /// still passes; only a byte-for-byte comparison against the C, or a test
    /// like this one, notices.
    #[derive(Default)]
    struct Recorder {
        symbols: Vec<usize>,
    }

    impl SymbolEncoder for Recorder {
        fn encode(&mut self, x: usize) {
            self.symbols.push(x);
        }
        fn putlowerbits(&mut self, _n: u32, _x: u32) {}
        fn put8(&mut self, _c: u32) {}
        fn put32(&mut self, _c: u32) {}
        fn flush(&mut self) {}
        fn finish(&mut self) {}
        fn error(&self) -> Option<c_int> {
            None
        }
    }

    /// The distance code carried by a match symbol.
    fn dcode_of(symbol: usize) -> usize {
        (symbol - 256) / LEN_CODES
    }

    /// `encode_match`'s repeated-distance search, checked against the sequence
    /// the C's chain of comma expressions produces.
    ///
    /// The expectations below are derived by hand from LZ77_Coder.cpp:438-441,
    /// stepping the four assignments in order. The C shuffles the history
    /// *while* testing it, so by the time the last comparison fails the history
    /// has already been slid down for a brand-new distance -- which is why the
    /// fourth entry can match a value that was in slot 3 two matches earlier.
    #[test]
    fn repdist_history_matches_the_c_shuffle() {
        let mut enc = GenericEncoder::new(Recorder::default());
        // A sequence chosen to reach every REPDIST slot, including slot 3, which
        // needs four distinct distances in flight before a repeat.
        for dist in [10i32, 20, 10, 30, 20, 10, 40, 30] {
            enc.encode_match(8, dist);
        }
        let got: Vec<usize> = enc.e.symbols.iter().map(|s| dcode_of(*s)).collect();

        // REPDIST_CODES..  means "an explicit distance"; 0..3 are the reuse codes.
        let expect_reuse = [None, None, Some(1), None, Some(2), Some(2), None, Some(3)];
        for (i, (g, e)) in got.iter().zip(expect_reuse.iter()).enumerate() {
            match e {
                Some(code) => assert_eq!(*g, *code, "match {i}: expected REPDIST {code}, got {g}"),
                None => assert!(
                    *g >= REPDIST_CODES,
                    "match {i}: expected an explicit distance code, got REPDIST {g}"
                ),
            }
        }
    }

    /// A repeat of the most recent distance must take code 0 and must not
    /// disturb the rest of the history -- the C returns before touching it.
    #[test]
    fn repeating_the_last_distance_leaves_the_history_alone() {
        let mut enc = GenericEncoder::new(Recorder::default());
        for dist in [10i32, 20, 20, 20, 10] {
            enc.encode_match(8, dist);
        }
        let got: Vec<usize> = enc.e.symbols.iter().map(|s| dcode_of(*s)).collect();
        assert!(got[0] >= REPDIST_CODES && got[1] >= REPDIST_CODES);
        assert_eq!(got[2], 0, "second use of 20 should be REPDIST 0");
        assert_eq!(got[3], 0, "third use of 20 should be REPDIST 0");
        // 10 is still one slot down, untouched by the three 20s.
        assert_eq!(got[4], 1, "10 should still be REPDIST 1");
    }

    /// `encode_table` rides the length escape window: lengths 101..104 after the
    /// shift, so a table can never be confused with a real match.
    #[test]
    fn table_signals_land_in_the_escape_window() {
        for kind in 1..=4i32 {
            let mut enc = GenericEncoder::new(Recorder::default());
            enc.encode_table(kind, 17);
            let sym = enc.e.symbols[0];
            let lcode = sym % LEN_CODES;
            // The encoded length is IMPOSSIBLE_LEN+kind - (IMPOSSIBLE_LEN-100)
            // = 100+kind, whose length code is the one covering 101..104.
            let t = Tables::for_encoding();
            assert_eq!(lcode, t.lc2_code((100 + kind) as u32), "kind {kind} used the wrong length code");
        }
    }

    /// A table signal must clear the most recent distance, because the value it
    /// carries is an element count and the REPCHAR check would index the input
    /// buffer with it.
    #[test]
    fn table_signal_invalidates_the_last_distance() {
        let mut enc = GenericEncoder::new(Recorder::default());
        enc.encode_match(8, 40);
        enc.encode_table(2, 41); // dist field becomes 40 -- the same value
        enc.encode_match(8, 40);
        let got: Vec<usize> = enc.e.symbols.iter().map(|s| dcode_of(*s)).collect();
        assert!(
            got[2] >= REPDIST_CODES,
            "after a table signal the distance must be re-sent explicitly, got REPDIST {}",
            got[2]
        );
    }
}
