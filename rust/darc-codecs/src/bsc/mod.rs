//! BSC (libbsc) decoder, ported from `Compression/BSC/`.
//!
//! Complete and wired: under `DARC_RUST`, `BSC_METHOD::decompress` routes every
//! block through [`dispatch::decompress`]. The encoder is still C.
//!
//! BSC is the largest codec at 17,368 lines, but its **decode surface is about
//! 4,800** -- most of the bulk is `libsais.c`'s forward suffix-array
//! construction, which only the compressor uses. The decode path needs, in the
//! order a block flows through it:
//!
//! | stage | from | approx |
//! |---|---|---|
//! | block header + dispatch | `libbsc.cpp` | 300 |
//! | entropy decode (QLFC) | `coder.cpp`, `qlfc.cpp` | 1,350 |
//! | inverse block sort | `libsais_unbwt`, or `st.cpp` for ST3..ST8 | 1,900 |
//! | inverse LZP | `lzp.cpp` | 400 |
//! | Adler-32 | `adler32.cpp` | 247 |
//!
//! ## The block format
//!
//! A 28-byte header, then the coded payload, then (for BWT with auxiliary
//! indexes) a trailing index array. The header is six little-endian 32-bit
//! fields:
//!
//! ```text
//!   0  blockSize      total bytes of this block, header included
//!   4  dataSize       decompressed size
//!   8  mode           packed stage selection, see below
//!  12  index          the block-sort primary index
//!  16  adler32_data   checksum of the DECOMPRESSED data
//!  20  adler32_body   checksum of everything after the header
//!  24  adler32_header checksum of bytes 0..23
//! ```
//!
//! `mode` packs the pipeline: bits 0-4 the block sorter (BWT or ST3..ST8),
//! bits 5-7 the coder, bits 8-15 the LZP minimum match length, bits 16-23 the
//! LZP hash size. `mode == 0` means the payload is stored verbatim, and
//! `mode == (mode & 0xff)` means LZP was not applied -- so the LZP stage is
//! keyed on the *upper* bits being set, not on a flag.
//!
//! Three separate Adler-32 checksums guard a block, which is why that comes
//! first: without it, nothing else can be validated the way the C validates it.

#![allow(dead_code)] // WIP: layers land before the decoder that uses them

pub mod adler32;
pub mod bwt;
pub mod bwt_enc;
pub mod dispatch;
pub mod header;
pub mod lzp;
pub mod lzp_enc;
pub mod model;
pub mod model_consts;
pub mod predictor;
pub mod qlfc;
pub mod qlfc_enc;
pub mod rangecoder;
pub mod st;
pub mod tables;

/// Error codes (`libbsc.h:41-47`).
pub const LIBBSC_NO_ERROR: i32 = 0;
pub const LIBBSC_BAD_PARAMETER: i32 = -1;
pub const LIBBSC_NOT_ENOUGH_MEMORY: i32 = -2;
/// Not an error the caller must propagate: the encoder returns it to mean
/// "this block did not shrink", and `bsc_compress` answers by storing it.
pub const LIBBSC_NOT_COMPRESSIBLE: i32 = -3;
pub const LIBBSC_UNEXPECTED_EOB: i32 = -5;
pub const LIBBSC_DATA_CORRUPT: i32 = -6;

/// `LIBBSC_HEADER_SIZE` (:84).
pub const HEADER_SIZE: usize = 28;

/// Block sorters (`libbsc.h:53-63`).
pub const BLOCKSORTER_NONE: u32 = 0;
pub const BLOCKSORTER_BWT: u32 = 1;
pub const BLOCKSORTER_ST3: u32 = 3;
pub const BLOCKSORTER_ST4: u32 = 4;
pub const BLOCKSORTER_ST5: u32 = 5;
pub const BLOCKSORTER_ST6: u32 = 6;
pub const BLOCKSORTER_ST7: u32 = 7;
pub const BLOCKSORTER_ST8: u32 = 8;

/// Entropy coders (`libbsc.h:67-70`).
pub const CODER_NONE: u32 = 0;
pub const CODER_QLFC_STATIC: u32 = 1;
pub const CODER_QLFC_ADAPTIVE: u32 = 2;
pub const CODER_QLFC_FAST: u32 = 3;

// ---------------------------------------------------------------------------
// The stream framing
// ---------------------------------------------------------------------------
//
// `bsc_stream_compress` / `bsc_stream_decompress` (`C_BSC.cpp:79`, `:131`),
// which lived in the C driver while the archiver was Haskell. `darc-arc` has no
// C to call, so the loop has to exist here for `-mbsc` archives to be written
// or read at all.
//
// The framing is an `int32` length before each block and a zero length as the
// end marker:
//
//     ([+n][n bytes])* [0]
//
// A block is emitted with a length of zero ONLY as that terminator, which is
// why a short read writes the terminator and stops rather than looping again.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_GENERAL};
use std::os::raw::c_int;

/// `full_read` — keep reading until the buffer is full or the input ends.
///
/// `Io::read` is one callback call and may return a short count; the C helper
/// loops, and so must this. Reading once and treating a short count as EOF
/// would silently truncate every block after the first.
fn full_read(io: &Io, buf: &mut [u8]) -> c_int {
    let mut done = 0usize;
    while done < buf.len() {
        let got = io.read(&mut buf[done..]);
        if got < 0 {
            return got;
        }
        if got == 0 {
            break;
        }
        done += got as usize;
    }
    done as c_int
}

/// `bsc_stream_compress` (`C_BSC.cpp:79`).
pub fn compress_stream(
    io: &Io,
    block_size: u32,
    lzp_hash_size: c_int,
    lzp_min_len: c_int,
    block_sorter: c_int,
    coder: c_int,
) -> c_int {
    let block_size = block_size.max(1) as usize;
    let mut inbuf = vec![0u8; block_size];
    let mut outbuf = vec![0u8; block_size + HEADER_SIZE + 1024];
    loop {
        let got = full_read(io, &mut inbuf);
        if got < 0 {
            return got;
        }
        if got == 0 {
            return match io.write_all(&0i32.to_le_bytes()) {
                Ok(()) => crate::ffi::OK,
                Err(e) => e,
            };
        }
        let n = got as usize;
        let mut produced = crate::bsc::qlfc_enc::compress(
            &inbuf[..n],
            &mut outbuf,
            lzp_hash_size.max(0) as u32,
            lzp_min_len.max(0) as u32,
            block_sorter.max(0) as u32,
            coder.max(0) as u32,
        );
        if produced < 0 {
            // "bsc_compress already stores internally when a block will not
            // compress, so this only catches a genuine refusal (ST7/ST8)."
            produced = crate::bsc::qlfc_enc::store(&inbuf[..n], &mut outbuf);
            if produced < 0 {
                return FREEARC_ERRCODE_GENERAL as c_int;
            }
        }
        let len = produced as usize;
        match io
            .write_all(&produced.to_le_bytes())
            .and_then(|()| io.write_all(&outbuf[..len]))
        {
            Ok(()) => {}
            Err(e) => return e,
        }
        if n < block_size {
            return match io.write_all(&0i32.to_le_bytes()) {
                Ok(()) => crate::ffi::OK,
                Err(e) => e,
            };
        }
    }
}

/// `bsc_stream_decompress` (`C_BSC.cpp:131`).
pub fn decompress_stream(io: &Io) -> c_int {
    let bad = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
    let mut inbuf: Vec<u8> = Vec::new();
    let mut outbuf: Vec<u8> = Vec::new();
    loop {
        let mut header = [0u8; 4];
        // `if (got != 4) result = BAD` -- note this is NOT an EOF check: a
        // well-formed stream always ends with the zero marker below, so running
        // out of input here IS corruption.
        if full_read(io, &mut header) != 4 {
            return bad;
        }
        let compressed = i32::from_le_bytes(header);
        if compressed == 0 {
            return crate::ffi::OK;
        }
        if compressed < HEADER_SIZE as i32 {
            return bad;
        }
        let want = compressed as usize;
        if inbuf.len() < want {
            inbuf.resize(want, 0);
        }
        if full_read(io, &mut inbuf[..want]) != compressed {
            return bad;
        }
        let parsed = match crate::bsc::header::parse(&inbuf[..HEADER_SIZE]) {
            Ok(p) => p,
            Err(_) => return bad,
        };
        // The framed length and the header's own must agree, or the stream has
        // been cut somewhere this loop cannot see.
        if parsed.block_size != compressed {
            return bad;
        }
        let data_size = match usize::try_from(parsed.data_size) {
            Ok(n) => n,
            Err(_) => return bad,
        };
        if outbuf.len() < data_size {
            outbuf.resize(data_size, 0);
        }
        if crate::bsc::dispatch::decompress(&inbuf[..want], &mut outbuf[..data_size]) != 0 {
            return bad;
        }
        match io.write_all(&outbuf[..data_size]) {
            Ok(()) => {}
            Err(e) => return e,
        }
    }
}
