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
pub fn compress_block(src: &[u8], out: &mut [u8]) -> Option<usize> {
    lz4_flex::block::compress_into(src, out).ok().filter(|&n| n > 0)
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
