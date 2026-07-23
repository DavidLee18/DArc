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
