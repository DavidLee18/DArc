//! LZ4, delegating to `lz4_flex` in place of the vendored C library.
//!
//! DArc uses the raw LZ4 **block** format -- `LZ4_compress_default` and
//! `LZ4_decompress_safe` -- not the frame format. The block format is a fixed
//! specification, so any conformant implementation reads any other's blocks,
//! which is what makes this substitution safe for existing archives.
//!
//! Everything around a block stays where it is. `C_LZ4.cpp` writes a version
//! byte, then per block an `int32` length whose sign flags a stored
//! (incompressible) block. That framing is DArc's own, is not LZ4, and is not
//! reimplemented here.
//!
//! The encoder does NOT produce byte-identical output to the C library. LZ4 is
//! a match-finder, and encoders legitimately choose different matches. Existing
//! archives still decode, which is the property that matters; `-mlz4` is not in
//! the fingerprint suite, so no recorded baseline moves either.

use std::os::raw::c_int;

/// Compress one block. Returns the compressed length, or `None` when the input
/// does not fit in `out`.
///
/// Mirrors `LZ4_compress_default`: a return of 0 or less means "did not fit",
/// which `C_LZ4.cpp` treats as "store this block raw" rather than as an error.
/// ## Why this cannot just call `compress_into(src, out)`
///
/// `lz4_flex::block::get_maximum_output_size` is `16 + 4 + len * 110 / 100` --
/// about **110%** of the input. `C_LZ4.cpp` sizes its output buffer with
/// `LZ4_compressBound`, which is `len + len/255 + 16` -- about **100.4%**. So a
/// C-sized buffer is always smaller than lz4_flex's precondition, and
/// `compress_into` rejects it with `OutputTooSmall` *before compressing
/// anything*. That returns "did not fit", which `C_LZ4.cpp` handles by storing
/// the block raw -- so the archive still round-trips perfectly while LZ4
/// compression silently does nothing at all. It cost ~7x on a real archive.
///
/// Compressing into scratch sized to lz4_flex's bound and copying back only
/// when the result genuinely fits keeps the C contract exactly: a return of 0
/// still means "store this block raw".
pub fn compress_block(src: &[u8], out: &mut [u8]) -> Option<usize> {
    let need = lz4_flex::block::get_maximum_output_size(src.len());
    if out.len() >= need {
        return lz4_flex::block::compress_into(src, out).ok().filter(|&n| n > 0);
    }
    let mut tmp = vec![0u8; need];
    let n = lz4_flex::block::compress_into(src, &mut tmp).ok().filter(|&n| n > 0)?;
    if n <= out.len() {
        out[..n].copy_from_slice(&tmp[..n]);
        Some(n)
    } else {
        None // genuinely did not fit; C_LZ4.cpp stores the block raw
    }
}

/// Decompress one block into a buffer of known capacity.
///
/// Mirrors `LZ4_decompress_safe`, which is the *safe* variant: it must not read
/// past `src` nor write past `out` however malformed the block is. Corrupt
/// archives reach this through an ordinary `arc t`, and the C decoders were
/// hardened for exactly that in v2.0.0.
pub fn decompress_block(src: &[u8], out: &mut [u8]) -> Result<usize, c_int> {
    lz4_flex::block::decompress_into(src, out).map_err(|_| crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int)
}


// ---------------------------------------------------------------------------
// The stream framing
// ---------------------------------------------------------------------------
//
// This is DArc's own framing, not LZ4's, and it lived in `C_LZ4.cpp` for as
// long as the archiver was Haskell -- the C driver owned the loop and Rust
// owned only the block primitives above. `darc-arc` has no C to call, so the
// loop has to exist here for `-mlz4` archives to be written or read at all.
//
// The layout is one version byte, then per block an `int32` length whose SIGN
// says whether the payload is compressed:
//
//     [01] ([+n][n compressed bytes] | [-n][n stored bytes])*
//
// ## What is NOT guaranteed
//
// Byte-identity with the C. LZ4 is a match finder and `lz4_flex` legitimately
// picks different matches, as the module header above already says. The
// framing here is exact, so archives cross-decode in both directions -- that
// is the property to test, and byte-comparing an `-mlz4` archive against the
// reference is not.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO};

/// `LZ4_VERSION_BYTE` (`C_LZ4.cpp:8`).
const VERSION_BYTE: u8 = 1;

/// `LZ4_compressBound` (`C_LZ4.cpp:28`) — `n + n/255 + 16`.
///
/// Deliberately the C's formula and not `lz4_flex`'s ~110% one; see
/// `compress_block` above for what happens when the two are confused.
fn c_compress_bound(n: usize) -> usize {
    n + n / 255 + 16
}

/// `LZ4_METHOD::compress` (`C_LZ4.cpp:75`).
pub fn compress_stream(
    io: &Io,
    compressor: c_int,
    block_size: u32,
    min_compression: c_int,
) -> c_int {
    let block_size = block_size.max(1) as usize;
    let dst_cap = c_compress_bound(block_size);
    let mut inbuf = vec![0u8; block_size];
    let mut out = vec![0u8; dst_cap];
    let mut first = true;
    loop {
        // READ_LEN_OR_EOF: one read, and anything <= 0 ends the stream.
        let got = io.read(&mut inbuf);
        if got <= 0 {
            return match got {
                0 => crate::ffi::OK,
                e => e,
            };
        }
        let in_size = got as usize;
        if first {
            match io.write_all(&[VERSION_BYTE]) {
                Ok(()) => {}
                Err(e) => return e,
            }
            first = false;
        }
        let produced = match compressor {
            0 => compress_block(&inbuf[..in_size], &mut out).unwrap_or(0),
            level => crate::lz4hc::compress_hc(&inbuf[..in_size], &mut out, level),
        };
        // `OutSize<=0 || (MinCompression>0 && OutSize >= (double(InSize)*MinCompression)/100)`
        // -- the ratio test is done in floating point, so it is done that way
        // here too rather than as integer arithmetic that rounds differently.
        let too_big = min_compression > 0
            && produced as f64 >= (in_size as f64 * f64::from(min_compression)) / 100.0;
        let stored = produced == 0 || too_big;
        let (len, payload): (i32, &[u8]) = match stored {
            true => (-(in_size as i32), &inbuf[..in_size]),
            false => (produced as i32, &out[..produced]),
        };
        match io
            .write_all(&len.to_le_bytes())
            .and_then(|()| io.write_all(payload))
        {
            Ok(()) => {}
            Err(e) => return e,
        }
    }
}

/// `LZ4_METHOD::decompress` (`C_LZ4.cpp:44`).
pub fn decompress_stream(io: &Io, block_size: u32) -> c_int {
    let block_size = block_size.max(1) as usize;
    // Sized to the COMPRESSED bound, not to block_size. A block is only stored
    // compressed when it got smaller, so block_size is enough for every archive
    // the encoder above can write -- but `-mlz4:0%` disables that test, and the
    // C then reads such a block into a BlockSize buffer and overruns it. Taking
    // the larger bound cannot reject anything valid and removes the overrun.
    let mut inbuf = vec![0u8; c_compress_bound(block_size)];
    let mut out = vec![0u8; block_size];

    let mut version = [0u8; 1];
    let got = io.read(&mut version);
    if got <= 0 {
        // READ_LEN_OR_EOF: an empty stream is an empty file, not an error.
        return match got {
            0 => crate::ffi::OK,
            e => e,
        };
    }
    if version[0] != VERSION_BYTE {
        return FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
    }
    loop {
        let mut header = [0u8; 4];
        let got = io.read(&mut header);
        if got <= 0 {
            return match got {
                0 => crate::ffi::OK,
                e => e,
            };
        }
        if got != 4 {
            return FREEARC_ERRCODE_IO as c_int;
        }
        let len = i32::from_le_bytes(header);
        let (want, stored) = match len < 0 {
            true => (len.unsigned_abs() as usize, true),
            false => (len as usize, false),
        };
        if want > inbuf.len() {
            return FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
        }
        // READ: a single call that must satisfy the whole request.
        let got = io.read(&mut inbuf[..want]);
        if got < 0 {
            return got;
        }
        if got as usize != want {
            return FREEARC_ERRCODE_IO as c_int;
        }
        let result = match stored {
            true => io.write_all(&inbuf[..want]),
            false => match decompress_block(&inbuf[..want], &mut out) {
                Ok(n) => io.write_all(&out[..n]),
                Err(e) => return e,
            },
        };
        match result {
            Ok(()) => {}
            Err(e) => return e,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    /// `C_LZ4.cpp` sizes its output buffer with `LZ4_compressBound`.
    fn compress_bound(n: usize) -> usize {
        n + n / 255 + 16
    }

    /// The point of the encoder is that it *compresses*. A version that always
    /// returned "did not fit" would still round-trip -- C_LZ4.cpp stores such a
    /// block raw -- so a round-trip test alone cannot catch it. Assert the size.
    #[test]
    fn compressible_input_actually_compresses() {
        let src: Vec<u8> = b"the quick brown fox jumps over the lazy dog. "
            .iter()
            .cycle()
            .take(100_000)
            .cloned()
            .collect();
        let mut out = vec![0u8; compress_bound(src.len())];
        let n = compress_block(&src, &mut out).expect("compress_block returned None");
        assert!(n < src.len() / 4, "expected real compression, got {n} from {}", src.len());
    }

    #[test]
    fn round_trips_through_both_directions() {
        for len in [1usize, 2, 17, 4096, 100_000] {
            let src: Vec<u8> = (0..len).map(|i| (i * 7 % 251) as u8).collect();
            let mut enc = vec![0u8; compress_bound(src.len())];
            let n = compress_block(&src, &mut enc).expect("compress");
            let mut dec = vec![0u8; src.len()];
            let m = decompress_block(&enc[..n], &mut dec).expect("decompress");
            assert_eq!(m, src.len());
            assert_eq!(dec, src, "round trip failed at len {len}");
        }
    }

    /// Corrupt blocks arrive through `arc t`; they must error, never panic.
    #[test]
    fn garbage_never_panics() {
        for seed in 0..64u32 {
            let junk: Vec<u8> = (0..200u32).map(|i| (i.wrapping_mul(seed).wrapping_add(11) % 256) as u8).collect();
            let mut out = vec![0u8; 4096];
            drop(decompress_block(&junk, &mut out));
        }
    }
}
