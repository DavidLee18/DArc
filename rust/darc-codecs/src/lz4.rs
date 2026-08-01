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
