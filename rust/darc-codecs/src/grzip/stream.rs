//! The stream wrapper, ported from `Compression/GRZip/C_GRZip.cpp`
//! (`grzip_decompress` :601, `GRZipMTDecompressor::main_cycle` :576).
//!
//! A GRZip stream is a bare sequence of blocks, each a 28-byte header followed
//! by its compressed body. There is no stream header and no terminator: the
//! read callback returning zero where a header would start *is* the end.
//!
//! The C decodes blocks on a worker pool and reassembles them in order. That is
//! a throughput choice, not a format one -- the bytes written are the same
//! blocks in the same order -- so this is a serial loop. Multithreading can be
//! added later without touching the format.
//!
//! Sizes come from `MTCompressor`'s allocation of `*(sint32*)(BlockSign+16) +
//! 1024` for the body and `*(sint32*)InBuf + 1024` for the output, so those two
//! header words are what bound each block here too.

use super::block::decompress_block;
use super::{GRZ_MAX_BLOCK_SIZE, GRZ_NOT_ENOUGH_MEMORY};
use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, OK};
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const HDR: usize = 28;

fn word(b: &[u8], at: usize) -> i32 {
    i32::from_le_bytes([b[at], b[at + 1], b[at + 2], b[at + 3]])
}

/// `grzip_decompress`.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    let mut sign = [0u8; HDR];
    loop {
        // A short read here is the end of the stream, not an error -- but only
        // if it is *empty*. Anything between 1 and 27 bytes is a truncated
        // header, which the C distinguishes the same way.
        let n = io.read(&mut sign);
        if n < 0 {
            return Err(n);
        }
        if n == 0 {
            return Ok(());
        }
        if n as usize != HDR {
            return Err(BAD);
        }
        // `GRZip_CheckBlockSign` checks only this one reserved word; the rest of
        // the header is validated by the block decoder.
        if word(&sign, 24) != 0 {
            return Err(BAD);
        }

        let packed = word(&sign, 16);
        let raw = word(&sign, 0);
        if packed < 0
            || raw < 0
            || packed as usize > GRZ_MAX_BLOCK_SIZE
            || raw as usize > GRZ_MAX_BLOCK_SIZE
        {
            return Err(BAD);
        }

        // Header plus body, exactly as the C assembles it before decoding: the
        // block decoder expects the 28-byte header in front of its input.
        let mut inbuf = Vec::new();
        if inbuf.try_reserve(HDR + packed as usize + 1024).is_err() {
            return Err(GRZ_NOT_ENOUGH_MEMORY);
        }
        inbuf.extend_from_slice(&sign);
        inbuf.resize(HDR + packed as usize, 0);
        if packed > 0 {
            let got = io.read(&mut inbuf[HDR..]);
            if got < 0 {
                return Err(got);
            }
            if got != packed {
                return Err(BAD);
            }
        }

        let mut out = Vec::new();
        if out.try_reserve(raw as usize + 1024).is_err() {
            return Err(GRZ_NOT_ENOUGH_MEMORY);
        }
        out.resize(raw as usize + 1024, 0);

        let written = decompress_block(&inbuf, &mut out).map_err(map_err)?;
        if written > 0 {
            let w = io.write(&out[..written]);
            if w < 0 {
                return Err(w);
            }
        }
    }
}

/// GRZip's own error codes are not FreeArc's; the C maps them at this boundary
/// and so does this.
fn map_err(e: c_int) -> c_int {
    match e {
        GRZ_NOT_ENOUGH_MEMORY => crate::ffi::FREEARC_ERRCODE_NOT_ENOUGH_MEMORY,
        _ => BAD, // every other GRZip failure is corrupt data
    }
}

// ---------------------------------------------------------------------------
// Encoder: grzip_compress (C_GRZip.cpp:519).
//
// The C runs this on a pool of worker threads, but the threading is pure
// parallelism: blocks are independent, and the writer emits them in order. A
// single-threaded loop produces the identical stream, which is the whole
// requirement here -- the format is just a concatenation of
// `28-byte header + body` blocks, exactly what `run` above consumes.
// ---------------------------------------------------------------------------

use super::block;
use super::{GRZ_COMPRESSION_MTF, GRZ_COMPRESSION_ST4};
use crate::ffi::FREEARC_ERRCODE_INVALID_COMPRESSOR;

const ABS_MAX_BYTE: usize = 256;
/// `ABS_MinBlockSize` (C_GRZip.cpp:370).
const ABS_MIN_BLOCK: usize = 24 * 1024;

/// `GRZip_GetAdaptiveBlockSize` (:372): decide how much of the buffer is worth
/// compressing as ONE block.
///
/// Walks forward in halving windows, comparing each window's own order-0 cost
/// against its cost under the statistics accumulated so far. When the blended
/// model is 25% worse than the local one the data has changed character, and
/// the block is cut short there so the next one starts fresh.
fn adaptive_block_size(input: &[u8], size: usize) -> usize {
    if size <= ABS_MIN_BLOCK {
        return size;
    }
    let mut tot = [0i32; ABS_MAX_BYTE];
    for &c in input[..ABS_MIN_BLOCK].iter() {
        tot[c as usize] += 1;
    }
    let mut pos = ABS_MIN_BLOCK;
    let mut bs = ABS_MIN_BLOCK / 2;
    while pos + bs < size {
        let mut freq = [0i32; ABS_MAX_BYTE];
        for &c in input[pos..pos + bs].iter() {
            freq[c as usize] += 1;
        }
        let sum = (bs + (pos >> 1)) as f64;
        let (mut avg, mut real) = (0.0f64, 0.0f64);
        for i in 0..ABS_MAX_BYTE {
            let fr = freq[i];
            if fr != 0 {
                real -= fr as f64 * (fr as f64 / bs as f64).log10();
                avg -= fr as f64 * ((fr + (tot[i] >> 1)) as f64 / sum).log10();
            }
        }
        if avg > 1.25 * real {
            if bs < 256 {
                return pos;
            }
            bs >>= 1;
            continue;
        }
        for i in 0..ABS_MAX_BYTE {
            tot[i] += freq[i];
        }
        pos += bs;
    }
    size
}

/// `grzip_compress`. The method/flag arguments are the archiver's, and the mode
/// word is assembled here exactly as `GRZipMTCompressor`'s constructor does --
/// note several of the named constants are 0, so the additions that look
/// redundant genuinely are.
#[allow(clippy::too_many_arguments)]
pub fn compress(
    io: &Io,
    method: c_int,
    block_size: c_int,
    enable_lzp: c_int,
    min_match_len: c_int,
    hash_size_log: c_int,
    alternative_bwt_sort: c_int,
    adaptive: c_int,
    delta_filter: c_int,
) -> c_int {
    let mut mode: i32 = match method {
        1 => 0,                          // BWT + WFC, both 0
        2 => GRZ_COMPRESSION_MTF,        // BWT + MTF
        3 => GRZ_COMPRESSION_ST4,        // ST4 + WFC
        4 => GRZ_COMPRESSION_ST4 | GRZ_COMPRESSION_MTF,
        _ => return FREEARC_ERRCODE_INVALID_COMPRESSOR,
    };
    mode += if enable_lzp != 0 {
        hash_size_log * 256 + min_match_len * 65536
    } else {
        0
    };
    // GRZ_BWTSorting_Strong is 0; only the fast flag adds anything.
    if alternative_bwt_sort == 0 {
        mode += 0x8;
    }
    // GRZ_Enable_DeltaFlt is 0; only disabling adds.
    if delta_filter == 0 {
        mode += 0x1;
    }

    let bs = (block_size as usize).min(GRZ_MAX_BLOCK_SIZE);
    let mut inbuf = vec![0u8; bs + 1024];
    let mut outbuf = vec![0u8; bs + 1024];
    let mut remainder = 0usize;

    loop {
        let got = io.read(&mut inbuf[remainder..bs]);
        if got < 0 {
            return got;
        }
        let mut in_size = got as usize + remainder;
        if in_size == 0 {
            return OK;
        }
        remainder = 0;
        let mut rem_pos = 0usize;
        if adaptive != 0 {
            let new_size = adaptive_block_size(&inbuf, in_size);
            rem_pos = new_size;
            remainder = in_size - new_size;
            in_size = new_size;
        }
        let n = match block::compress_block(&inbuf, in_size, &mut outbuf, mode) {
            Ok(n) => n,
            Err(e) => return e,
        };
        match io.write_all(&outbuf[..n]) {
            Err(e) => {
                return e;
            }
            Ok(_) => {}
        }
        if remainder > 0 {
            inbuf.copy_within(rem_pos..rem_pos + remainder, 0);
        }
    }
}
