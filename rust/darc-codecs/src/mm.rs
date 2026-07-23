//! MM multimedia preprocessor decoder, ported from `Compression/MM/mm.cpp`
//! (`mm_decompress` and the four `undiff` routines it reaches).
//!
//! MM is not a compressor: it is a *filter* that subtracts each multimedia
//! sample from the previous sample of the same channel, so that whatever runs
//! after it in the `+`-chain sees small deltas instead of raw waveform. The
//! archiver reaches it as `-mmm`. Decoding is the inverse -- a running sum per
//! channel -- which is why this module is a fraction of the size of `tta.rs`
//! despite the two codecs sharing a directory.
//!
//! Only the decoder is ported, the same decode-first order used for REP, Dict,
//! LZP and TTA: a Rust build must *read* every existing `-mmm` archive before it
//! may write one. None of `mmdet.cpp` is needed here -- its 1,117 lines of
//! WAV-header and entropy autodetection sit behind
//! `#ifndef FREEARC_DECOMPRESS_ONLY` and only ever choose the *encoder's*
//! parameters, which then travel in the stream header.
//!
//! ## The stream
//!
//! A single flags byte, then either nothing more (`0` = the payload is stored
//! verbatim) or a two-byte `num_chan`/`word_size` pair. After that comes a
//! 4-byte `offset` and that many bytes of the original file header, copied
//! through untouched -- this is what lets the filter start on a sample
//! boundary for e.g. a `.wav` whose 44-byte header would otherwise skew every
//! channel. Then zero padding up to a multiple of the sample size, then the
//! deltas, to end of stream.
//!
//! Bit 0 of the flags byte is the only one implemented; bits 1-2 are reserved
//! for the unfinished byte/word reordering (`reorder_words` in mm.cpp is a
//! stub that returns its argument), and the C decoder rejects them.
//!
//! ## Widths, and why little-endian is exact
//!
//! Every accumulator is the width of one sample -- `u8`, `u16`, 24-bit, `u32` --
//! and all of them wrap. In C that is unsigned arithmetic on `uint16`/`uint32`
//! (and plain `char` for the 8-bit case, where the add is mod 256 either way);
//! here every one is a `wrapping_add`, because a Rust `+` would panic in debug
//! on input that the format not only permits but produces constantly.
//!
//! The C reads samples by casting the buffer to `uint16*`/`uint32*`, i.e. in
//! *native* order, so an MM stream is only portable across builds of the same
//! byte order. Every DArc target is little-endian, and the format's own
//! accessors (`value24`/`value32`, Common.h:285) are defined little-endian for
//! the non-Intel path, so `from_le_bytes` here is exact rather than merely
//! equivalent.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO, OK};
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;

/// `BUFFER_SIZE` (Compression.h:38). Part of the format, not a tuning knob:
/// the encoder emits its second and later blocks at `roundDown(BUFFER_SIZE,N)`,
/// and the decoder must ask for exactly the same amount.
const BUFFER_SIZE: usize = 64 << 10;

/// The header offset is a 32-bit field read into a C `int`, so the C decoder
/// happily takes a value that makes `malloc` fail or go negative. No encoder
/// can emit more than the 1 MB first-block size, so anything past this is
/// corrupt; rejecting it keeps a hostile stream from driving an allocation.
const MAX_OFFSET: u32 = 1 << 30;

// ---------------------------------------------------------------------------
// Stream helpers, mirroring the READ / READ4 / WRITE macros at
// Compression.h:83-130. Note WRITE is the *lenient* one -- it fails only on a
// negative return, because the write callback is documented to either take
// everything or fail -- while READ demands the exact count it asked for.
// ---------------------------------------------------------------------------

fn read_exact(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.read(buf) {
        n if n as usize == buf.len() => Ok(()),
        n if n >= 0 => Err(IO), // short read where the format demands a full one
        n => Err(n),
    }
}

fn read_u32(io: &Io) -> Result<u32, c_int> {
    let mut b = [0u8; 4];
    read_exact(io, &mut b)?;
    Ok(u32::from_le_bytes(b))
}

fn write_out(io: &Io, buf: &[u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.write(buf) {
        n if n < 0 => Err(n),
        _ => Ok(()),
    }
}

/// `roundDown`/`roundUp` (Common.h:494-503). The `b > 1` guard is theirs, and
/// it matters: it is the reason a stream declaring `word_size == 0` divides by
/// nothing and simply copies through instead of trapping.
fn round_down(a: usize, b: usize) -> usize {
    if b > 1 {
        a / b * b
    } else {
        a
    }
}

fn round_up(a: usize, b: usize) -> usize {
    if b > 1 {
        round_down(a - 1, b) + b
    } else {
        a
    }
}

// ---------------------------------------------------------------------------
// The inverse filters. Each walks the buffer in samples of `n` channels and
// replaces every element with a running per-channel sum. `base` holds those
// sums across chunk boundaries -- and across the whole stream, which is why it
// is threaded through the read loop rather than being local to one call.
//
// The trailing bytes of a partial sample are left untouched, exactly as the
// pointer conditions in C do (`p+N <= buf+bufsize`).
// ---------------------------------------------------------------------------

fn undiff1(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0;
    while p + n <= buf.len() {
        for i in 0..n {
            base[i] = base[i].wrapping_add(buf[p + i]);
            buf[p + i] = base[i];
        }
        p += n;
    }
}

fn undiff2(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 16-bit words
    while (p + n) * 2 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 2;
            let acc = u16::from_le_bytes([base[i * 2], base[i * 2 + 1]])
                .wrapping_add(u16::from_le_bytes([buf[at], buf[at + 1]]));
            base[i * 2..i * 2 + 2].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 2].copy_from_slice(&acc.to_le_bytes());
        }
        p += n;
    }
}

/// 24-bit samples, with 32-bit accumulators -- `base` is a `uint32*` in C even
/// here, hence the 4-byte stride. C reads each sample with `value24`, a masked
/// 32-bit load that touches one byte past the last sample (which is what the
/// `+1` on mm.cpp's `malloc` is for); reading three bytes explicitly gives the
/// same value with no over-read to justify.
fn undiff3(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in bytes
    while p + n * 3 <= buf.len() {
        for i in 0..n {
            let at = p + i * 3;
            let b = i * 4;
            let acc = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]])
                .wrapping_add(u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], 0]));
            base[b..b + 4].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 3].copy_from_slice(&acc.to_le_bytes()[..3]);
        }
        p += n * 3;
    }
}

fn undiff4(buf: &mut [u8], n: usize, base: &mut [u8]) {
    let mut p = 0; // in 32-bit words
    while (p + n) * 4 <= buf.len() {
        for i in 0..n {
            let at = (p + i) * 4;
            let b = i * 4;
            let acc = u32::from_le_bytes([base[b], base[b + 1], base[b + 2], base[b + 3]])
                .wrapping_add(u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]]));
            base[b..b + 4].copy_from_slice(&acc.to_le_bytes());
            buf[at..at + 4].copy_from_slice(&acc.to_le_bytes());
        }
        p += n;
    }
}

// ---------------------------------------------------------------------------

/// `mm_decompress`.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    let mut header = [0u8; 3];
    read_exact(io, &mut header[..1])?;
    let mut buf = vec![0u8; BUFFER_SIZE];

    // Autodetection found nothing at encode time, so the payload was stored.
    if header[0] == 0 {
        return copy_to_eof(io, &mut buf);
    }

    // Bits 1-2 are the reordering that was never finished; anything above is
    // reserved. The C decoder rejects both rather than guess.
    if (header[0] & !1) != 0 {
        return Err(BAD);
    }
    read_exact(io, &mut header[1..3])?;

    // The original file header, copied through untouched. C reads it into one
    // `malloc(offset)`; chunking it moves the same bytes without letting a
    // declared length pick the allocation size.
    let offset = read_u32(io)?;
    if offset > MAX_OFFSET {
        return Err(BAD);
    }
    let mut left = offset as usize;
    while left > 0 {
        let want = left.min(buf.len());
        read_exact(io, &mut buf[..want])?;
        write_out(io, &buf[..want])?;
        left -= want;
    }

    let num_chan = header[1] as usize;
    let word_size = header[2] as usize;
    let byte_size = (word_size + 7) / 8; // bytes per channel word, 0..=32
    let sample = num_chan * byte_size; // `N`: bytes per sample

    // With no channels but a nonzero word size, C's `p += N` never advances and
    // the undiff loop spins forever. Nothing can be bug-compatible with a hang,
    // so treat it as the corrupt header it is. (`word_size == 0` is *not* this
    // case: it makes `byte_size` zero, which C handles as a plain copy below.)
    if num_chan == 0 && byte_size != 0 {
        return Err(BAD);
    }

    // Padding the encoder wrote to align the deltas to a sample boundary. It
    // reads into `base` in C as well -- the bytes are zeros, so the running sums
    // stay zero, and a stream that puts something else there seeds them exactly
    // as C would.
    let base_stride = if byte_size == 3 { 4 } else { byte_size };
    let mut base = vec![0u8; num_chan * base_stride];
    let before = 3 + 4 + offset as usize;
    let pad = round_up(before, sample) - before;
    read_exact(io, &mut base[..pad])?;

    let chunk = round_down(BUFFER_SIZE, sample);
    loop {
        let got = io.read(&mut buf[..chunk]);
        if got < 0 {
            return Err(got);
        }
        if got == 0 {
            return Ok(());
        }
        let data = &mut buf[..got as usize];
        match byte_size {
            1 => undiff1(data, num_chan, &mut base),
            2 => undiff2(data, num_chan, &mut base),
            3 => undiff3(data, num_chan, &mut base),
            4 => undiff4(data, num_chan, &mut base),
            // No filter exists for 0 or for words wider than 32 bits; C's
            // `switch` has no default either, so the data passes through.
            _ => {}
        }
        write_out(io, data)?;
    }
}

fn copy_to_eof(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    loop {
        let got = io.read(buf);
        if got < 0 {
            return Err(got);
        }
        if got == 0 {
            return Ok(());
        }
        write_out(io, &buf[..got as usize])?;
    }
}
