//! `PRIME_STREAM`, ported from `Compression/PPMD/PPMdType.h`.
//!
//! The buffered callback layer PPMd does all its I/O through. It matters to the
//! port for one reason beyond plumbing: `get()` returns `EOF` (-1) on
//! exhaustion, and the decoder's normalisation shifts that value into `code`.
//! Reproducing the exact byte the C ends up with there is what keeps a
//! truncated or trailing-edge stream decoding identically.

use crate::ffi::Io;

/// `BUFFER_SIZE` from PPMdType.h.
pub const BUFFER_SIZE: usize = 64 * 1024;

/// The C's `EOF`.
pub const EOF: i32 = -1;

pub struct PrimeStream<'a> {
    io: &'a Io,
    buf: Vec<u8>,
    /// Index into `buf`, the C's `p - Buf`.
    pos: usize,
    /// The C's `Count`: bytes left before a refill/flush is needed. It is
    /// decremented BEFORE the test, so it goes negative to signal end of data.
    count: i32,
    /// Bytes currently held for writing, the C's `p - Buf` on the output side.
    fill: usize,
    error: i32,
    writing: bool,
}

impl<'a> PrimeStream<'a> {
    pub fn new_reader(io: &'a Io) -> Self {
        PrimeStream {
            io,
            buf: vec![0u8; BUFFER_SIZE],
            pos: 0,
            count: 0,
            fill: 0,
            error: 0,
            writing: false,
        }
    }

    pub fn new_writer(io: &'a Io) -> Self {
        PrimeStream {
            io,
            buf: vec![0u8; BUFFER_SIZE],
            pos: 0,
            count: BUFFER_SIZE as i32,
            fill: 0,
            error: 0,
            writing: true,
        }
    }

    pub fn error(&self) -> i32 {
        self.error
    }

    /// The C's `atEOS()`: `Count < 0`.
    pub fn at_eos(&self) -> bool {
        self.count < 0
    }

    /// `PRIME_STREAM::get`. Returns `EOF` (-1) once the source is exhausted;
    /// the decoder shifts that into `code` rather than stopping, so the value
    /// is load-bearing rather than merely a status.
    pub fn get(&mut self) -> i32 {
        self.count -= 1;
        if self.count >= 0 {
            let b = self.buf[self.pos];
            self.pos += 1;
            b as i32
        } else {
            self.fill_buf()
        }
    }

    fn fill_buf(&mut self) -> i32 {
        if self.error < 0 {
            return EOF;
        }
        let n = self.io.read(&mut self.buf[..BUFFER_SIZE]);
        self.count = n;
        self.pos = 0;
        if n < 0 {
            self.error = n;
            return EOF;
        }
        self.count -= 1;
        if self.count >= 0 {
            let b = self.buf[self.pos];
            self.pos += 1;
            b as i32
        } else {
            EOF
        }
    }

    /// `PRIME_STREAM::put`.
    pub fn put(&mut self, c: u8) {
        self.count -= 1;
        if self.count >= 0 {
            self.buf[self.fill] = c;
            self.fill += 1;
        } else {
            self.flush();
            // The C recurses: `(flush(), put(c))`.
            self.count -= 1;
            self.buf[self.fill] = c;
            self.fill += 1;
        }
    }

    /// `PRIME_STREAM::flush`. Writes out whatever is buffered and resets.
    pub fn flush(&mut self) {
        if self.error >= 0 && self.fill > 0 {
            let n = self.io.write(&self.buf[..self.fill]);
            if n < 0 {
                self.error = n;
            }
        }
        self.fill = 0;
        self.pos = 0;
        self.count = BUFFER_SIZE as i32;
    }

    /// Flush on drop is NOT automatic in the C -- `ppmd_compress` calls it
    /// explicitly after the encoder flush -- so this is explicit too, to keep
    /// the write ordering identical.
    pub fn finish(&mut self) {
        if self.writing {
            self.flush();
        }
    }
}
