//! The output loop and entry point, ported from `Compression/Tornado/Tornado.cpp`
//! (`tor_decompress` :487, `tor_decompress0` :400, `WRITE_DATA_IF` :369).
//!
//! One loop serves all four back-ends. It writes into a circular window and
//! flushes to the output stream whenever the window fills or the data-table
//! list does; between flushes the window doubles as the LZ dictionary, which is
//! why matches can reach backwards past the last flush and why the tables are
//! re-diffed after every write.
//!
//! `offset` tracks how many bytes have scrolled past the window start, so a
//! match distance can be validated against the whole output rather than just
//! the window. `offset_overflow` records that this counter has wrapped, after
//! which the check is skipped rather than being wrong.

use super::lz77::{ByteDecoder, GenericDecoder, HufBackend, Lz77Decoder, IMPOSSIBLE_DIST, IMPOSSIBLE_LEN};
use super::lz77::BitDecoder;
use super::range::ArithDecoder;
use super::stream::{InputBitStream, InputByteStream, BAD};
use super::tables::DataTables;
use super::{ARICODER, BITCODER, BYTECODER, HUFCODER, PAD_FOR_TABLES};
use crate::ffi::{Io, OK};
use core::ffi::c_int;

/// `HUGE_BUFFER_SIZE` (Compression.h:45) -- the flush granularity, chosen so
/// the decoder does not seek the output constantly.
const HUGE_BUFFER_SIZE: usize = 8 << 20;

/// A cap on the window the header may ask for. `bufsize` is four attacker-
/// controlled bytes and the C hands it straight to malloc; Tornado's own
/// presets top out at 1 GB (`extra_dbits` reaches exactly that far), so
/// anything beyond it is corrupt rather than merely large.
const MAX_BUFSIZE: u32 = 1 << 30;

struct Window<'a> {
    io: &'a Io,
    buf: Vec<u8>,
    /// Index of the logical buffer start; `PAD_FOR_TABLES` bytes precede it.
    origin: usize,
    bufsize: usize,
    output: usize,
    write_start: usize,
    write_end: usize,
    offset: u64,
    offset_overflow: bool,
    tables: DataTables,
}

impl<'a> Window<'a> {
    fn new(io: &'a Io, bufsize: usize) -> Self {
        let origin = PAD_FOR_TABLES;
        Window {
            io,
            buf: vec![0u8; origin + bufsize + PAD_FOR_TABLES],
            origin,
            bufsize,
            output: origin,
            write_start: origin,
            write_end: origin + bufsize.min(HUGE_BUFFER_SIZE),
            offset: 0,
            offset_overflow: false,
            tables: DataTables::new(),
        }
    }

    /// The body of `WRITE_DATA_IF`: undiff the tables, write, re-diff, then wrap
    /// the window if it is full and set the next flush point.
    fn flush(&mut self) -> Result<(), c_int> {
        let (ws, out) = (self.write_start, self.output);
        self.tables.undiff_tables(&mut self.buf, ws, out);
        if out > ws {
            let n = self.io.write(&self.buf[ws..out]);
            if n < 0 {
                return Err(n);
            }
        }
        self.tables.diff_tables(&mut self.buf, ws, out);
        self.write_start = self.output;

        if self.output >= self.origin + self.bufsize {
            let advanced = self.output - self.origin;
            // The C compares against 1<<63 *before* adding, so the flag trails
            // the real overflow by one window; kept as-is because it only ever
            // makes the distance check stricter.
            self.offset_overflow |= self.offset > (1u64 << 63);
            self.offset += advanced as u64;
            self.write_start -= advanced;
            self.write_end -= advanced;
            self.tables.shift(&mut self.buf, self.output, self.origin);
            self.output -= advanced;
        }

        if self.write_start >= self.write_end {
            let room = self.origin + self.bufsize - self.write_start;
            self.write_end = self.write_start + room.min(HUGE_BUFFER_SIZE);
        }
        Ok(())
    }
}

macro_rules! flush_if {
    ($w:expr, $d:expr, $cond:expr) => {
        if $cond {
            match $d.error() {
                Some(e) => {
                    return Err(e);
                }
                None => {}
            }
            $w.flush()?;
        }
    };
}

fn decompress0<D: Lz77Decoder>(
    io: &Io,
    mut d: D,
    header_bufsize: u32,
    minlen: u32,
) -> Result<(), c_int> {
    match d.error() {
        Some(e) => {
            return Err(e);
        }
        None => {}
    }
    // compress_all_at_once is false on the archiver's streaming path, so the
    // window is grown to at least HUGE_BUFFER_SIZE exactly as the C does.
    let bufsize = (header_bufsize as usize).max(HUGE_BUFFER_SIZE);
    let mut w = Window::new(io, bufsize);

    loop {
        if d.is_literal() {
            let c = d.getchar();
            w.buf[w.output] = c;
            w.output += 1;
            flush_if!(w, d, w.output >= w.write_end);
            continue;
        }

        let mut len = d.getlen(minlen);
        let dist = d.getdist() as usize;

        // Both copy loops below are do/while on `--len`, so a zero length wraps
        // the counter and runs off the buffer. A real match is at least 1 and
        // EOF is signalled by IMPOSSIBLE_LEN, so this is unreachable from a
        // valid stream -- the C added the same guard.
        if len == 0 {
            return Err(BAD);
        }

        let at = w.output - w.origin;
        if at >= dist && (w.write_end - w.output) as u64 > len as u64 {
            // Fast path: the whole match fits before the next flush and does
            // not reach back past the window start. Byte-at-a-time because
            // overlapping matches are how run-length encoding falls out of LZ77.
            let mut p = w.output - dist;
            for _ in 0..len {
                w.buf[w.output] = w.buf[p];
                w.output += 1;
                p += 1;
            }
        } else if len < IMPOSSIBLE_LEN {
            if dist > w.bufsize
                || len as u64 > 2 * header_bufsize as u64
                || ((at as u64 + w.offset) < dist as u64 && !w.offset_overflow)
            {
                return Err(BAD);
            }
            // Slow path: the source may sit before the window start (so it
            // wraps to the end) and the copy may cross a flush point.
            // `output + bufsize - dist`, not `output - dist + bufsize`: the
            // window is up to 1 GB and a match may legitimately reach further
            // back than `output` has advanced, wrapping to the buffer end. C
            // computes that intermediate as a pointer, which may point before
            // the buffer and come back; the same expression on usize underflows
            // and panics across the C ABI. Only reachable once the stream is
            // larger than one flush chunk, which is why an 11 MB input caught
            // it and a 900 KB corpus did not.
            let mut p = if at >= dist { w.output - dist } else { w.output + w.bufsize - dist };
            loop {
                w.buf[w.output] = w.buf[p];
                w.output += 1;
                p += 1;
                if p == w.origin + w.bufsize {
                    p = w.origin;
                }
                flush_if!(w, d, w.output >= w.write_end);
                len -= 1;
                if len == 0 {
                    break;
                }
            }
        } else if len == IMPOSSIBLE_LEN && dist as u32 == IMPOSSIBLE_DIST {
            flush_if!(w, d, true);
            break;
        } else {
            // A data table: `len` past IMPOSSIBLE_LEN is the row width, `dist`
            // the row count.
            len -= IMPOSSIBLE_LEN;
            if len == 0 || (dist as u64) * (len as u64) > 2 * header_bufsize as u64 {
                return Err(BAD);
            }
            if len as usize > super::MAX_TABLE_ROW_AT_DECOMPRESSION {
                return Err(BAD);
            }
            w.tables.add(len as usize, w.output, dist);
            flush_if!(w, d, w.tables.filled());
        }
    }

    match d.error() {
        Some(e) => {
            return Err(e);
        }
        None => {}
    }
    Ok(())
}

/// `tor_decompress`: read the six-byte header and pick a back-end.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    let mut hdr = InputByteStream::new(io, 0);
    let method = hdr.get8();
    let minlen = hdr.get8();
    let bufsize = hdr.get32();
    match hdr.error() {
        Some(e) => {
            return Err(e);
        }
        None => {}
    }
    if bufsize == 0 || bufsize > MAX_BUFSIZE {
        return Err(BAD);
    }
    // The header stream has already buffered part of the payload, so the
    // back-end must continue from it rather than opening a second one.
    match method {
        BYTECODER => decompress0(io, ByteDecoder::new(hdr), bufsize, minlen),
        BITCODER => decompress0(io, BitDecoder::new(InputBitStream::from_bytes(hdr)), bufsize, minlen),
        HUFCODER => {
            decompress0(io, GenericDecoder::new(HufBackend::new(InputBitStream::from_bytes(hdr))), bufsize, minlen)
        }
        ARICODER => {
            decompress0(io, GenericDecoder::new(ArithDecoder::new(hdr, super::CODES)), bufsize, minlen)
        }
        _ => Err(BAD),
    }
}
