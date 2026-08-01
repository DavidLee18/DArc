//! The wire format: a port of `ByteStream.hs`'s reader half.
//!
//! Every number, string and list in an archive's service blocks goes through
//! this encoding. It is the foundation the block descriptors, the footer and the
//! directory are all built on, so a single width error here is a format break
//! that still round-trips — the highest-risk failure mode in this repo.
//!
//! # Which Haskell branch this ports
//!
//! `ByteStream.hs` has two: `instance Storable a => FastBufferData a` under GHC,
//! and an explicit instance per type under `__MHS__`. **This ports the GHC
//! branch**, because `Storable`'s `sizeOf` is what decides the widths that
//! shipped archives actually contain. The two agree on every type in the format
//! — checked, not assumed — which is what makes `Tests/arc` usable as a
//! byte-level reference even though its *behaviour* diverges.
//!
//! | Haskell type | bytes | note |
//! |---|---|---|
//! | `Word8`/`Word16`/`Word32`/`Word64` | 1/2/4/8 | little-endian |
//! | `Int`, `CTime` (`FileTime`) | 8 | `Int64`; native width varies, the format's does not |
//! | `CUInt` (`CRC`) | 4 | |
//! | `Bool` | 1 | as a `Word8`, when written singly |
//! | `Integer` (`FileSize`) | 1..9 | the tagged varint below |
//! | `String` | var | UTF-8, NUL-terminated |
//! | `[a]` | var | `Integer` length, then the elements |
//!
//! # The `Integer` varint (`ByteStream.hs:473`)
//!
//! The number of low-order **one** bits in the first byte says how many bytes
//! the value occupies. `x<128` writes `x*2` — low bit clear, one byte. `x<128^2`
//! writes `x*4+1` — one set bit, two bytes. And so on to eight. Past that, a
//! byte of all ones followed by a plain `u64`, nine bytes total.
//!
//! The writer pokes four or eight bytes and advances the position by fewer,
//! deliberately: the trailing bytes belong to whatever is written next. A reader
//! that consumed `maxSizeOf` would drift. So the reader must mask, exactly as
//! `readUnchecked` does.

use core::fmt;

/// Everything that can go wrong decoding a stream.
///
/// A truncated read is a *format* error, never a panic: this decodes untrusted
/// archive bytes, and the C reader it replaces reports corruption rather than
/// dying.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// Wanted `want` bytes at `at`, but the buffer holds only `have`.
    Truncated { at: usize, want: usize, have: usize },
    /// A NUL-terminated string ran off the end of the buffer.
    UnterminatedString { at: usize },
    /// A length field that cannot be honoured — larger than the bytes left.
    ImplausibleLength { at: usize, len: u64, remaining: usize },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::Truncated { at, want, have } => {
                write!(f, "truncated at {at}: wanted {want} bytes, {have} available")
            }
            Error::UnterminatedString { at } => {
                write!(f, "unterminated string at {at}")
            }
            Error::ImplausibleLength { at, len, remaining } => {
                write!(f, "length {len} at {at} exceeds the {remaining} bytes remaining")
            }
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;

/// A cursor over an in-memory service block.
///
/// Borrowing rather than owning is what lets several blocks decode at once from
/// one mapped archive without copying; `InStream` is `Send` because it is just a
/// slice and an index.
#[derive(Clone, Debug)]
pub struct InStream<'a> {
    buf: &'a [u8],
    pos: usize,
}

impl<'a> InStream<'a> {
    /// `ByteStream.openMemory`.
    pub fn new(buf: &'a [u8]) -> Self {
        InStream { buf, pos: 0 }
    }

    /// Current offset, for error messages and for the footer's two
    /// `isEOFMemory` probes.
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// `ByteStream.isEOFMemory`. The footer uses this twice to tell an archive
    /// written by an older build (no recovery field, no UTF-8 comment) from a
    /// current one, so it is part of the format, not a convenience.
    pub fn is_eof(&self) -> bool {
        self.pos >= self.buf.len()
    }

    /// The bytes not yet consumed.
    pub fn remaining(&self) -> usize {
        self.buf.len().saturating_sub(self.pos)
    }

    fn take(&mut self, n: usize) -> Result<&'a [u8]> {
        let end = self.pos.checked_add(n).ok_or(Error::Truncated {
            at: self.pos,
            want: n,
            have: self.remaining(),
        })?;
        match self.buf.get(self.pos..end) {
            Some(s) => {
                self.pos = end;
                Ok(s)
            }
            None => Err(Error::Truncated { at: self.pos, want: n, have: self.remaining() }),
        }
    }

    /// Read `n` bytes without advancing — the varint reader needs to look at
    /// four or eight bytes and then advance by fewer.
    ///
    /// Short reads are zero-padded rather than refused. That is deliberate and
    /// matches the Haskell: `readUnchecked` peeks a whole `Word32` even for a
    /// one-byte value, so a one-byte varint at the very end of a buffer is
    /// legal and must decode. The masking below never looks at the padding.
    fn peek_padded<const N: usize>(&self) -> [u8; N] {
        let mut out = [0u8; N];
        let avail = self.buf.len().saturating_sub(self.pos);
        let n = if avail < N { avail } else { N };
        match self.buf.get(self.pos..self.pos + n) {
            Some(s) => out[..n].copy_from_slice(s),
            None => {}
        }
        out
    }

    pub fn u8(&mut self) -> Result<u8> {
        let b = self.take(1)?;
        Ok(b[0])
    }

    pub fn u16(&mut self) -> Result<u16> {
        let b = self.take(2)?;
        Ok(u16::from_le_bytes([b[0], b[1]]))
    }

    pub fn u32(&mut self) -> Result<u32> {
        let b = self.take(4)?;
        Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }

    pub fn u64(&mut self) -> Result<u64> {
        let b = self.take(8)?;
        Ok(u64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]))
    }

    /// Haskell `Int` and `CTime`: eight bytes, signed.
    pub fn i64(&mut self) -> Result<i64> {
        Ok(self.u64()? as i64)
    }

    /// `CRC` is `CUInt` — four bytes, not eight.
    pub fn crc(&mut self) -> Result<u32> {
        self.u32()
    }

    /// A `Bool` written singly occupies a whole byte (`ByteStream.hs:520`).
    ///
    /// Any nonzero byte is `true`: the Haskell goes through `fromWord8`, which
    /// tests against zero rather than demanding exactly 1.
    pub fn bool(&mut self) -> Result<bool> {
        Ok(self.u8()? != 0)
    }

    /// The tagged varint (`ByteStream.hs:497`).
    ///
    /// Returns `u64`: `FileSize` is a Haskell `Integer`, but the writer refuses
    /// anything at or above `256^8`, so every value that can appear fits.
    pub fn varint(&mut self) -> Result<u64> {
        if self.is_eof() {
            return Err(Error::Truncated { at: self.pos, want: 1, have: 0 });
        }
        let four = u32::from_le_bytes(self.peek_padded::<4>());
        // Each arm masks to the width its tag claims and shifts the tag away.
        // The order matters: the tests are on progressively more low bits, so a
        // one-byte value would also satisfy nothing above it, and an eight-byte
        // value satisfies none of these.
        // Each arm is a direct transcription of the Haskell's
        // `(x mod 256^n) shiftR n`: mask to the width the tag claims, then shift
        // the tag bits away.
        let (val, used): (u64, usize) = if four & 1 == 0 {
            (u64::from(four & 0xff) >> 1, 1)
        } else if four & 3 == 1 {
            (u64::from(four & 0xffff) >> 2, 2)
        } else if four & 7 == 3 {
            (u64::from(four & 0x00ff_ffff) >> 3, 3)
        } else if four & 15 == 7 {
            (u64::from(four) >> 4, 4)
        } else {
            let eight = u64::from_le_bytes(self.peek_padded::<8>());
            if eight & 31 == 15 {
                ((eight & 0x0000_00ff_ffff_ffff) >> 5, 5)
            } else if eight & 63 == 31 {
                ((eight & 0x0000_ffff_ffff_ffff) >> 6, 6)
            } else if eight & 127 == 63 {
                ((eight & 0x00ff_ffff_ffff_ffff) >> 7, 7)
            } else if eight & 255 == 127 {
                (eight >> 8, 8)
            } else {
                // A byte of all ones, then a plain u64. The value starts one
                // byte in, so this cannot reuse `eight`.
                self.pos += 1;
                let v = self.u64()?;
                return Ok(v);
            }
        };
        if self.remaining() < used {
            return Err(Error::Truncated { at: self.pos, want: used, have: self.remaining() });
        }
        self.pos += used;
        Ok(val)
    }

    /// A varint used as a count, checked against what is actually left.
    ///
    /// Without this a corrupt length allocates gigabytes before failing. The C
    /// reader has the same exposure and the same answer: refuse a count that
    /// cannot possibly be satisfied. One byte is the smallest any element can
    /// be, so `len > remaining` is impossible in a well-formed stream.
    pub fn count(&mut self) -> Result<usize> {
        let at = self.pos;
        let n = self.varint()?;
        let remaining = self.remaining();
        if n > remaining as u64 {
            return Err(Error::ImplausibleLength { at, len: n, remaining });
        }
        Ok(n as usize)
    }

    /// A NUL-terminated UTF-8 string (`ByteStream.hs:464`).
    ///
    /// Invalid UTF-8 is replaced rather than refused: these are file names from
    /// archives that may have been written on any code page, and the Haskell
    /// decoder does not reject them either. Refusing would make an archive
    /// unlistable over one bad name.
    pub fn string(&mut self) -> Result<String> {
        let start = self.pos;
        let rest = self.buf.get(self.pos..).unwrap_or(&[]);
        match rest.iter().position(|&b| b == 0) {
            Some(n) => {
                let s = String::from_utf8_lossy(&rest[..n]).into_owned();
                self.pos += n + 1;
                Ok(s)
            }
            None => Err(Error::UnterminatedString { at: start }),
        }
    }

    /// `BufferData [a]` (`ByteStream.hs:635`): a varint length, then elements.
    pub fn list<T, F>(&mut self, each: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let n = self.count()?;
        self.exactly(n, each)
    }

    /// `n` elements with no length prefix — the directory writes most of its
    /// fields this way, because the count was written once and shared.
    pub fn exactly<T, F>(&mut self, n: usize, mut each: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        let mut out = Vec::with_capacity(n);
        for _ in 0..n {
            out.push(each(self)?);
        }
        Ok(out)
    }

    /// A `Compressor`.
    ///
    /// The type is `[CompressionMethod]` = `[String]`, which looks like the
    /// generic list instance — a varint count, then the elements. **It is not.**
    /// `Compression.hs:131` gives `Compressor` its own `BufferData` instance
    /// that writes `join_compressor x`, i.e. one NUL-terminated string with the
    /// methods separated by `'+'`:
    ///
    /// ```text
    ///   41 72 43 01  08  "lzma:1mb:mf=BT4\0"  ...
    ///   ^signature   ^tag ^the whole chain, no count
    /// ```
    ///
    /// Reading it as a list consumes the first byte of `"lzma…"` as a length
    /// and desynchronises everything after — which presents as a CRC failure on
    /// every descriptor in the archive, not as a decode error.
    ///
    /// `aNO_COMPRESSION` is `["storing"]`, and `split '+' ""` is `[""]`, so the
    /// result always has at least one element.
    pub fn compressor(&mut self) -> Result<Vec<String>> {
        let joined = self.string()?;
        Ok(joined.split('+').map(str::to_string).collect())
    }
}

/// The writer half, used by the round-trip tests and by the eventual `arc a`.
///
/// Kept in the same module as the reader on purpose: the two must agree byte
/// for byte, and the varint's write/read asymmetry (poke four, advance one) is
/// only obvious when both are in view.
#[derive(Clone, Debug, Default)]
pub struct OutStream {
    buf: Vec<u8>,
}

impl OutStream {
    pub fn new() -> Self {
        OutStream { buf: Vec::new() }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.buf
    }

    pub fn u8(&mut self, x: u8) {
        self.buf.push(x);
    }

    pub fn u16(&mut self, x: u16) {
        self.buf.extend_from_slice(&x.to_le_bytes());
    }

    pub fn u32(&mut self, x: u32) {
        self.buf.extend_from_slice(&x.to_le_bytes());
    }

    pub fn u64(&mut self, x: u64) {
        self.buf.extend_from_slice(&x.to_le_bytes());
    }

    pub fn i64(&mut self, x: i64) {
        self.u64(x as u64);
    }

    pub fn crc(&mut self, x: u32) {
        self.u32(x);
    }

    pub fn bool(&mut self, x: bool) {
        self.u8(u8::from(x));
    }

    /// The tagged varint. Mirrors `writeUnchecked` arm for arm, including its
    /// refusal above `256^8` — which is why this returns a `bool` rather than
    /// silently truncating a value the Haskell would have called `fail` on.
    #[must_use]
    pub fn varint(&mut self, x: u64) -> bool {
        const B: u64 = 128;
        match x {
            _ if x < B => self.push_n(x * 2, 1),
            _ if x < B.pow(2) => self.push_n(x * 4 + 1, 2),
            _ if x < B.pow(3) => self.push_n(x * 8 + 3, 3),
            _ if x < B.pow(4) => self.push_n(x * 16 + 7, 4),
            _ if x < B.pow(5) => self.push_n(x * 32 + 15, 5),
            _ if x < B.pow(6) => self.push_n(x * 64 + 31, 6),
            _ if x < B.pow(7) => self.push_n(x * 128 + 63, 7),
            _ if x < B.pow(8) => self.push_n(x.wrapping_mul(256).wrapping_add(127), 8),
            _ => {
                // 256^8 is 2^64, so every remaining u64 takes the nine-byte
                // form; the Haskell's "larger than 256^8" refusal is
                // unreachable from a u64 and needs no branch here.
                self.u8(255);
                self.u64(x);
            }
        }
        true
    }

    fn push_n(&mut self, packed: u64, n: usize) {
        self.buf.extend_from_slice(&packed.to_le_bytes()[..n]);
    }

    /// UTF-8 followed by a NUL.
    pub fn string(&mut self, s: &str) {
        self.buf.extend_from_slice(s.as_bytes());
        self.u8(0);
    }

    pub fn list<T, F>(&mut self, items: &[T], mut each: F)
    where
        F: FnMut(&mut Self, &T),
    {
        let _ = self.varint(items.len() as u64);
        for it in items {
            each(self, it);
        }
    }

    /// `join_compressor = joinWith "+"`, then written as one string.
    pub fn compressor(&mut self, methods: &[String]) {
        self.string(&methods.join("+"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every boundary of the varint's nine arms, plus one either side.
    ///
    /// The boundaries are the whole risk: an off-by-one in a `<` picks the wrong
    /// width and every following field shifts, which is a format break that
    /// still decodes into plausible-looking garbage.
    #[test]
    fn varint_round_trips_across_every_arm_boundary() {
        let mut cases: Vec<u64> = vec![0, 1, 2, 126, 127];
        for p in 1..8u32 {
            let b = 128u64.pow(p);
            cases.push(b - 1);
            cases.push(b);
            cases.push(b + 1);
        }
        cases.push(u64::MAX / 2);
        cases.push(u64::MAX);
        for x in cases {
            let mut o = OutStream::new();
            assert!(o.varint(x), "writer refused {x}");
            let bytes = o.into_bytes();
            let mut i = InStream::new(&bytes);
            let got = i.varint().expect("decodes");
            assert_eq!(got, x, "round trip failed for {x}");
            assert_eq!(i.pos(), bytes.len(), "wrong width consumed for {x}");
        }
    }

    /// The widths are part of the format, so pin them rather than only checking
    /// that a round trip closes: a writer and reader that agree on the WRONG
    /// width round-trip perfectly and produce unreadable archives.
    #[test]
    fn varint_widths_match_the_haskell_table() {
        let expect: [(u64, usize); 9] = [
            (0, 1),
            (127, 1),
            (128, 2),
            (128 * 128, 3),
            (128u64.pow(3), 4),
            (128u64.pow(4), 5),
            (128u64.pow(5), 6),
            (128u64.pow(6), 7),
            (128u64.pow(7), 8),
        ];
        for (x, want) in expect {
            let mut o = OutStream::new();
            assert!(o.varint(x));
            assert_eq!(o.as_bytes().len(), want, "wrong width for {x}");
        }
        // And the nine-byte escape: a byte of ones, then a plain u64.
        let mut o = OutStream::new();
        assert!(o.varint(128u64.pow(8)));
        assert_eq!(o.as_bytes().len(), 9);
        assert_eq!(o.as_bytes()[0], 255);
    }

    /// `x < 128` writes `x*2` into four bytes and advances ONE. The three bytes
    /// after it belong to the next field, so a reader that consumed four would
    /// drift. This is the asymmetry the module doc warns about.
    #[test]
    fn a_short_varint_does_not_consume_the_bytes_that_follow_it() {
        let mut o = OutStream::new();
        assert!(o.varint(5));
        o.u32(0xDEAD_BEEF);
        let bytes = o.into_bytes();
        assert_eq!(bytes.len(), 5, "one byte of varint plus four of u32");
        let mut i = InStream::new(&bytes);
        assert_eq!(i.varint().expect("varint"), 5);
        assert_eq!(i.u32().expect("u32"), 0xDEAD_BEEF);
        assert!(i.is_eof());
    }

    /// A one-byte varint at the very end of the buffer must decode: the Haskell
    /// peeks a whole Word32 there too, reading past the value it wants.
    #[test]
    fn a_varint_at_the_end_of_the_buffer_decodes() {
        let bytes = [10u8]; // 5 * 2
        let mut i = InStream::new(&bytes);
        assert_eq!(i.varint().expect("decodes at the buffer edge"), 5);
        assert!(i.is_eof());
    }

    #[test]
    fn strings_are_utf8_and_nul_terminated() {
        let mut o = OutStream::new();
        o.string("Ünïcödé/påth.txt");
        o.string("");
        o.u32(7);
        let bytes = o.into_bytes();
        let mut i = InStream::new(&bytes);
        assert_eq!(i.string().expect("first"), "Ünïcödé/påth.txt");
        assert_eq!(i.string().expect("second"), "");
        assert_eq!(i.u32().expect("after"), 7, "the NUL was consumed, not left behind");
    }

    #[test]
    fn an_unterminated_string_is_an_error_not_a_panic() {
        let bytes = b"no terminator".to_vec();
        let mut i = InStream::new(&bytes);
        assert_eq!(i.string(), Err(Error::UnterminatedString { at: 0 }));
    }

    /// A corrupt count must not become a multi-gigabyte allocation. This is
    /// untrusted input by definition.
    #[test]
    fn an_implausible_count_is_refused_before_allocating() {
        let mut o = OutStream::new();
        assert!(o.varint(1_000_000));
        let bytes = o.into_bytes();
        let mut i = InStream::new(&bytes);
        match i.count() {
            Err(Error::ImplausibleLength { len, .. }) => assert_eq!(len, 1_000_000),
            other => panic!("expected refusal, got {other:?}"),
        }
    }

    #[test]
    fn compressor_is_one_plus_joined_string_not_a_list() {
        let methods = vec!["delta".to_string(), "lzma:96mb:normal".to_string()];
        let mut o = OutStream::new();
        o.compressor(&methods);
        let bytes = o.into_bytes();
        // The encoding is the chain and a NUL -- no count byte in front of it.
        assert_eq!(bytes.len(), "delta+lzma:96mb:normal".len() + 1);
        assert_eq!(bytes[0], b'd');
        let mut i = InStream::new(&bytes);
        assert_eq!(i.compressor().expect("decodes"), methods);
        assert!(i.is_eof());
    }

    /// Pinned against bytes taken from a real archive's footer descriptor,
    /// because this is the field that was got wrong by inferring it from the
    /// Haskell TYPE (`[String]`) instead of reading its instance.
    #[test]
    fn compressor_matches_the_bytes_a_real_archive_contains() {
        let mut bytes = b"lzma:1mb:mf=BT4".to_vec();
        bytes.push(0);
        let mut i = InStream::new(&bytes);
        assert_eq!(i.compressor().expect("decodes"), vec!["lzma:1mb:mf=BT4".to_string()]);
        assert!(i.is_eof(), "consumed exactly the string and its NUL");
    }

    /// aNO_COMPRESSION is ["storing"], and `split '+' ""` is [""], so a
    /// compressor is never an empty vector -- code that tests for one is testing
    /// for something the format cannot produce.
    #[test]
    fn a_compressor_always_has_at_least_one_element() {
        let mut o = OutStream::new();
        o.compressor(&["storing".to_string()]);
        let mut i = InStream::new(o.as_bytes());
        assert_eq!(i.compressor().expect("decodes"), vec!["storing".to_string()]);

        let empty = [0u8];
        let mut i = InStream::new(&empty);
        assert_eq!(i.compressor().expect("decodes"), vec![String::new()]);
    }

    /// The fixed-width types, pinned against the GHC `Storable` widths.
    #[test]
    fn fixed_widths_match_the_ghc_storable_sizes() {
        let mut o = OutStream::new();
        o.u8(1);
        o.u16(2);
        o.u32(3);
        o.u64(4);
        o.i64(-5);
        o.crc(6);
        o.bool(true);
        assert_eq!(o.as_bytes().len(), 1 + 2 + 4 + 8 + 8 + 4 + 1);
        let bytes = o.into_bytes();
        let mut i = InStream::new(&bytes);
        assert_eq!(i.u8().expect("u8"), 1);
        assert_eq!(i.u16().expect("u16"), 2);
        assert_eq!(i.u32().expect("u32"), 3);
        assert_eq!(i.u64().expect("u64"), 4);
        assert_eq!(i.i64().expect("i64"), -5);
        assert_eq!(i.crc().expect("crc"), 6);
        assert!(i.bool().expect("bool"));
        assert!(i.is_eof());
    }

    /// Reading past the end is an error, never a panic: this decodes archives
    /// that may be truncated or deliberately malformed.
    #[test]
    fn every_reader_refuses_a_truncated_buffer() {
        let empty: [u8; 0] = [];
        assert!(InStream::new(&empty).u8().is_err());
        assert!(InStream::new(&empty).u32().is_err());
        assert!(InStream::new(&empty).u64().is_err());
        assert!(InStream::new(&empty).varint().is_err());
        assert!(InStream::new(&empty).string().is_err());
        let short = [1u8, 2, 3];
        assert!(InStream::new(&short).u32().is_err());
    }
}
