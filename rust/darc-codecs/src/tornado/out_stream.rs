//! Output byte and bit streams, ported from `Compression/Tornado/EntropyCoder.cpp`
//! (`OutputByteStream` :31, `OutputBitStream` :160).
//!
//! The mirror of `stream.rs`. Every Tornado encoder writes through one of these,
//! and two details of the byte stream are load-bearing rather than incidental:
//!
//! **The last 512 bytes are never flushed early.** `LZ77_ByteCoder` reserves a
//! four-byte slot in the buffer, keeps encoding, and comes back to overwrite it
//! with a word of literal/match flags once sixteen items have been coded. That
//! slot has to still be in the buffer when it is written, so `flush` holds back
//! 512 bytes and rounds what it does write down to a 4096-byte boundary
//! (EntropyCoder.cpp:93). The reserved slot is the `anchor`, and it is shifted
//! along with the buffer contents whenever a flush moves them.
//!
//! **The anchor starts outside the buffer.** `LZ77_ByteCoder`'s constructor
//! points it at a scratch member (`garbage`, :81) so the very first flag word --
//! which describes items encoded before any slot was reserved -- lands somewhere
//! harmless. That is modelled here as a negative anchor whose writes are
//! discarded, rather than by keeping a dummy word inside the buffer, so a stray
//! write to it cannot corrupt real output.

use crate::ffi::Io;
use core::ffi::c_int;

/// Slack past the C's arithmetic so a `put64` at the very end of a full buffer
/// stays in bounds. The C relies on malloc rounding for the same thing.
const GUARD: usize = 64;

pub struct OutputByteStream<'a> {
    io: &'a Io,
    buf: Vec<u8>,
    /// Write cursor within `buf`.
    output: usize,
    /// Cursor at the last `quasiwrite`, so each flush reports only new bytes.
    last_qwrite: usize,
    /// Reserved slot for a flag word. Negative means the C's `garbage` scratch
    /// variable, whose contents are never read.
    anchor: isize,
    /// Minimum bytes a `flush` will hand to the callback.
    chunk: usize,
    err: Option<c_int>,
}

impl<'a> OutputByteStream<'a> {
    /// `chunk` is the least a flush will write; `pad` is the most that may be
    /// buffered between two flushes. Sizing is the C's (:48).
    pub fn new(io: &'a Io, chunk: usize, pad: usize) -> Self {
        OutputByteStream {
            io,
            buf: vec![0u8; chunk + pad + 512 + 4096 + GUARD],
            output: 0,
            last_qwrite: 0,
            anchor: -1,
            chunk,
            err: None,
        }
    }

    pub fn error(&self) -> Option<c_int> {
        self.err
    }

    /// Reserve the current position as the flag-word slot.
    #[inline]
    pub fn set_anchor_here(&mut self) {
        self.anchor = self.output as isize;
    }

    /// `advance` (:58) -- reserve `n` bytes without writing them.
    #[inline]
    pub fn advance(&mut self, n: usize) {
        if self.room(n) {
            self.output += n;
        }
    }

    /// Store a 32-bit little-endian value at the reserved slot. A negative
    /// anchor is the `garbage` scratch variable and the write is dropped.
    #[inline]
    pub fn put_at_anchor(&mut self, value: u32) {
        if self.anchor >= 0 {
            let at = self.anchor as usize;
            self.buf[at..at + 4].copy_from_slice(&value.to_le_bytes());
        }
    }

    #[inline]
    fn room(&mut self, n: usize) -> bool {
        if self.output + n > self.buf.len() {
            // Unreachable with the C's sizing: at most `pad` bytes are written
            // between flushes and the buffer holds chunk+pad+512+4096. Report it
            // rather than panicking across the ABI if the arithmetic ever slips.
            debug_assert!(false, "Tornado output buffer overflow");
            self.err = Some(crate::ffi::FREEARC_ERRCODE_GENERAL);
            return false;
        }
        true
    }

    #[inline]
    pub fn put8(&mut self, c: u32) {
        if !self.room(1) {
            return;
        }
        self.buf[self.output] = c as u8;
        self.output += 1;
    }

    #[inline]
    pub fn put16(&mut self, c: u32) {
        if !self.room(2) {
            return;
        }
        self.buf[self.output..self.output + 2].copy_from_slice(&(c as u16).to_le_bytes());
        self.output += 2;
    }

    /// `put24` writes a full 32-bit value and then advances only three bytes, so
    /// the fourth is overwritten by whatever comes next (:66).
    #[inline]
    pub fn put24(&mut self, c: u32) {
        if !self.room(4) {
            return;
        }
        self.buf[self.output..self.output + 4].copy_from_slice(&c.to_le_bytes());
        self.output += 3;
    }

    #[inline]
    pub fn put32(&mut self, c: u32) {
        if !self.room(4) {
            return;
        }
        self.buf[self.output..self.output + 4].copy_from_slice(&c.to_le_bytes());
        self.output += 4;
    }

    #[inline]
    pub fn put64(&mut self, c: u64) {
        if !self.room(8) {
            return;
        }
        self.buf[self.output..self.output + 8].copy_from_slice(&c.to_le_bytes());
        self.output += 8;
    }

    /// `flush` (:89). Reports progress, then writes out whole 4096-byte pages if
    /// that comes to at least `chunk`, keeping the last 512 bytes back for the
    /// byte coder's flag slot.
    pub fn flush(&mut self) {
        self.io.quasiwrite((self.output - self.last_qwrite) as i64);
        if self.output > 512 {
            let n = (self.output - 512) & !4095;
            if n >= self.chunk && n > 0 {
                self.write_out(n);
                self.buf.copy_within(n..self.output, 0);
                self.output -= n;
                self.anchor -= n as isize;
            }
        }
        self.last_qwrite = self.output;
    }

    /// `finish` (:81). Hands everything buffered to the callback.
    ///
    /// The C assigns the callback's return value to `errcode` and only maps a
    /// positive value back to OK, so a short write is silently accepted there.
    /// Here it is an IO error, matching `checked_write` (Compression.h) and
    /// every other codec in this crate: a short write means the output could not
    /// be stored, which is not a success under any reading.
    pub fn finish(&mut self) {
        let n = self.output;
        self.write_out(n);
    }

    fn write_out(&mut self, n: usize) {
        if self.err.is_some() {
            return;
        }
        match self.io.write_all(&self.buf[..n]) {
            Err(e) => {
                self.err = Some(e);
            }
            Ok(_) => {}
        }
    }
}

/// `OutputBitStream` (:160). Bits accumulate from the low end of a 64-bit buffer
/// and are emitted a whole word at a time, little-endian, so the decoder's
/// 32-bit `needbits` refills read them back in the same order.
pub struct OutputBitStream<'a> {
    pub bytes: OutputByteStream<'a>,
    bitbuf: u64,
    bitcount: i32,
}

impl<'a> OutputBitStream<'a> {
    pub fn new(io: &'a Io, chunk: usize, pad: usize) -> Self {
        OutputBitStream { bytes: OutputByteStream::new(io, chunk, pad), bitbuf: 0, bitcount: 0 }
    }

    /// `putbits` (:174). `x` must already be masked to `n` bits; use
    /// [`putlowerbits`](Self::putlowerbits) otherwise.
    ///
    /// The leftover after a word is emitted is `x >> (n - bitcount)`, which the C
    /// computes in `uint32`. That is undefined for a shift of 32, reachable only
    /// if `n` were 32 with the buffer exactly half full -- no caller comes close,
    /// since the widest field is a 30-bit extra-bits group. Shifting at 64 bits
    /// here gives the mathematically correct 0 for that case instead of whatever
    /// the target's shift instruction happens to do.
    #[inline]
    pub fn putbits(&mut self, n: i32, x: u32) {
        debug_assert!((0..=32).contains(&n), "putbits width {n} out of range");
        self.bitbuf |= (x as u64) << self.bitcount;
        self.bitcount += n;
        if self.bitcount >= 64 {
            let full = self.bitbuf;
            self.bytes.put64(full);
            self.bitcount -= 64;
            self.bitbuf = (x as u64) >> (n - self.bitcount);
        }
    }

    /// `putlowerbits` (:194): mask first, so callers may pass a wider value.
    #[inline]
    pub fn putlowerbits(&mut self, n: i32, x: u32) {
        self.putbits(n, mask32(x, n));
    }

    /// `finish` (:211): spill the partial word a byte at a time, then the bytes.
    pub fn finish(&mut self) {
        while self.bitcount > 0 {
            self.bytes.put8(self.bitbuf as u32 & 0xff);
            self.bitbuf >>= 8;
            self.bitcount -= 8;
        }
        self.bytes.finish();
    }

    pub fn flush(&mut self) {
        self.bytes.flush();
    }

    pub fn error(&self) -> Option<c_int> {
        self.bytes.error()
    }
}

/// `mask(x,n)` for a 32-bit value. n==32 would be undefined in the C's `int`
/// arithmetic; it is well defined here and no caller reaches it.
#[inline]
pub fn mask32(x: u32, n: i32) -> u32 {
    if n >= 32 {
        x
    } else {
        x & ((1u32 << n) - 1)
    }
}
