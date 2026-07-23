//! Input byte and bit streams, ported from `Compression/Tornado/EntropyCoder.cpp`
//! (`InputByteStream` :105, `InputBitStream` :223).
//!
//! All four Tornado back-ends read through one of these, so this is the layer
//! that decides what a truncated stream does.
//!
//! ## The C has no end-of-data bound, and this does
//!
//! `InputByteStream` keeps a `bufsize` buffer with `MAXELEM` bytes of slack at
//! the front, refilled whenever the read pointer passes `read_point`. It
//! records the callback's return value in `errcode` but never compares it to
//! what it asked for -- the `dataend` field that would have tracked it is
//! commented out at EntropyCoder.cpp:113. So once the input is exhausted the
//! callback returns 0 and the decoder keeps reading whatever those bytes
//! happened to be: the previous chunk's data, or uninitialised heap on the
//! first read. A truncated `-mtor` stream therefore decodes stale memory
//! rather than failing, and a corrupt one can spin emitting literals forever.
//!
//! Here the buffer past the valid data reads as zero, and running off the end
//! is an error rather than a silent success. That is deliberately *not*
//! bug-compatible, and it cannot change a valid stream: the only reads past
//! real data are the bit buffer's 4-byte lookahead and `get64`'s slack, and a
//! valid stream stops at its EOB code before any of those bits reach the
//! output. `EXHAUSTED_SLACK` is what keeps that lookahead legal.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA};
use core::ffi::c_int;

pub const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;

/// `MAXELEM` (EntropyCoder.cpp:22) -- the largest element `get64` can pull, and
/// the slack the C keeps at the buffer front so a read never straddles a chunk.
const MAXELEM: usize = 8;

/// `LARGE_BUFFER_SIZE` (Compression.h:41), the refill granularity. Not part of
/// the format -- it only decides how often the callback is asked for more.
const LARGE_BUFFER_SIZE: usize = 256 << 10;

/// How far past the last real byte a read may go before it is an error. The bit
/// buffer refills 32 bits at a time through `get32`, and `get64` reads 8, so a
/// stream that ends mid-lookahead is still valid; one that keeps asking well
/// past the end is not.
const EXHAUSTED_SLACK: usize = 64;

pub struct InputByteStream<'a> {
    io: &'a Io,
    buf: Vec<u8>,
    /// Read cursor within `buf`.
    pos: usize,
    /// One past the last byte the callback actually delivered.
    valid: usize,
    /// Refill fence: `buf.len() - MAXELEM` in the C layout.
    read_point: usize,
    /// Bytes served from beyond `valid`; past `EXHAUSTED_SLACK` this is a
    /// truncated stream rather than end-of-stream lookahead.
    overrun: usize,
    /// Sticky negative error from the callback.
    err: Option<c_int>,
}

impl<'a> InputByteStream<'a> {
    pub fn new(io: &'a Io, bufsize: usize) -> Self {
        // compress_all_at_once is false for the archiver's streaming path, so
        // the C picks LARGE_BUFFER_SIZE regardless of the header's bufsize.
        let _ = bufsize;
        let size = LARGE_BUFFER_SIZE;
        let mut s = InputByteStream {
            io,
            buf: vec![0u8; MAXELEM + size],
            pos: MAXELEM,
            valid: MAXELEM,
            // C: read_point = buf + bufsize, i.e. MAXELEM short of the end,
            // leaving room for a get64 that straddles the fence.
            read_point: size,
            overrun: 0,
            err: None,
        };
        s.refill();
        s
    }

    fn refill(&mut self) {
        let start = MAXELEM;
        let end = self.buf.len();
        // Anything the callback does not deliver must read as zero rather than
        // as the previous chunk, so the result never depends on stale bytes.
        self.buf[start..end].fill(0);
        match self.io.read(&mut self.buf[start..end]) {
            n if n < 0 => {
                self.err = Some(n);
                self.valid = start;
            }
            n => self.valid = start + n as usize,
        }
    }

    /// `fill()` (EntropyCoder.cpp:136): once the cursor passes the fence, carry
    /// the last MAXELEM bytes to the front and read the next chunk.
    #[inline]
    fn fill(&mut self) {
        if self.pos >= self.read_point {
            let tail = self.buf.len() - MAXELEM;
            self.buf.copy_within(tail.., 0);
            let consumed_valid = self.valid;
            self.refill();
            self.pos -= self.buf.len() - MAXELEM;
            // Carry forward how far past real data we had already gone.
            if consumed_valid < tail {
                self.overrun += tail - consumed_valid;
            }
        }
    }

    #[inline]
    fn take(&mut self, n: usize) -> &[u8] {
        self.fill();
        if self.pos + n > self.valid {
            self.overrun += (self.pos + n).saturating_sub(self.valid.max(self.pos));
        }
        let at = self.pos;
        self.pos += n;
        &self.buf[at..at + n]
    }

    /// Any hard error seen so far: a negative callback return, or reading so far
    /// past the end of the data that the stream must be truncated.
    pub fn error(&self) -> Option<c_int> {
        if let Some(e) = self.err {
            return Some(e);
        }
        if self.overrun > EXHAUSTED_SLACK {
            return Some(BAD);
        }
        None
    }

    #[inline]
    pub fn getc(&mut self) -> u32 {
        self.take(1)[0] as u32
    }

    #[inline]
    pub fn get8(&mut self) -> u32 {
        self.getc()
    }

    #[inline]
    pub fn get16(&mut self) -> u32 {
        let b = self.take(2);
        u16::from_le_bytes([b[0], b[1]]) as u32
    }

    #[inline]
    pub fn get24(&mut self) -> u32 {
        let b = self.take(3);
        u32::from_le_bytes([b[0], b[1], b[2], 0])
    }

    #[inline]
    pub fn get32(&mut self) -> u32 {
        let b = self.take(4);
        u32::from_le_bytes([b[0], b[1], b[2], b[3]])
    }
}

/// `InputBitStream` (EntropyCoder.cpp:223). The bit buffer is filled 32 bits at
/// a time from the low end and consumed from the low end, so `bitcount` is how
/// many valid bits sit in `bitbuf`.
pub struct InputBitStream<'a> {
    pub bytes: InputByteStream<'a>,
    bitbuf: u64,
    bitcount: i32,
}

impl<'a> InputBitStream<'a> {
    pub fn new(io: &'a Io, bufsize: usize) -> Self {
        Self::from_bytes(InputByteStream::new(io, bufsize))
    }

    /// Continue from an already-open byte stream. The six-byte header is read
    /// through one of these, and it has buffered payload behind it, so the
    /// back-end must inherit that buffer rather than open a second stream.
    pub fn from_bytes(bytes: InputByteStream<'a>) -> Self {
        InputBitStream { bytes, bitbuf: 0, bitcount: 0 }
    }

    /// `needbits` -- top up to at least `n` valid bits (n <= 32) and return them.
    #[inline]
    pub fn needbits(&mut self, n: i32) -> u32 {
        if self.bitcount <= 32 {
            self.bitbuf |= (self.bytes.get32() as u64) << self.bitcount;
            self.bitcount += 32;
        }
        mask64(self.bitbuf, n)
    }

    #[inline]
    pub fn dumpbits(&mut self, n: i32) {
        self.bitbuf >>= n;
        self.bitcount -= n;
    }

    #[inline]
    pub fn getbits(&mut self, n: i32) -> u32 {
        let x = self.needbits(n);
        self.dumpbits(n);
        x
    }

    pub fn error(&self) -> Option<c_int> {
        self.bytes.error()
    }
}

/// `mask(x,n)` (EntropyCoder.cpp:17). C computes `(1<<n)-1` in `int`, so n==32
/// would be undefined there; it is only ever called with n<=32 and the shift is
/// done here at 64 bits so the n==32 case is well defined.
#[inline]
pub fn mask64(x: u64, n: i32) -> u32 {
    if n >= 32 {
        x as u32
    } else {
        (x & ((1u64 << n) - 1)) as u32
    }
}
