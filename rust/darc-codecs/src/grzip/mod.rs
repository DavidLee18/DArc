//! GRZip decoder, ported from `Compression/GRZip/`.
//!
//! **Work in progress.** The LZP and record-de-interleave stages are ported;
//! the arithmetic decoders, the inverse BWT/ST4 and the block dispatcher are
//! not, so nothing is wired to `grzip_decompress` yet and the C decoder still
//! runs. As with every codec before it, the `#ifndef DARC_RUST` exclusion goes
//! in only once the whole path is verified byte-exact.
//!
//! ## Shape
//!
//! `GRZip_DecompressBlock` (C_GRZip.cpp:233) reads a 28-byte block header and
//! dispatches on `Mode` at offset 4:
//!
//! | `Mode` | path |
//! |---|---|
//! | `-1` | stored `memcpy`, or LZP alone |
//! | `-2` | **recursive**: 2 or 4 nested blocks, recombined by `GRZip_Rec_Decode` |
//! | else | MTF-arith or WFC-arith, then inverse BWT or ST4, then LZP |
//!
//! The recursive case is the one to be careful with. Nothing in the C bounds
//! how deeply `Mode == -2` may nest, and the depth comes straight from the
//! block header, so the port needs an explicit limit that the C does not have.
//!
//! Note the header sizes were already hardened C-side in an earlier pass --
//! bounds against `GRZ_MaxBlockSize`, the `AriOutSize` rounding fix, and an
//! input length passed to both arithmetic decoders. Read those comments before
//! concluding a check is missing.

#![allow(dead_code)] // WIP: stages land before the dispatcher that uses them

pub mod block;
pub mod bwt;
pub mod lzp;
pub mod mtf_ari;
pub mod rec;
pub mod st4;

use core::ffi::c_int;

/// Error codes (`libGRZip.h:46-52`). These are GRZip's own, not FreeArc's; the
/// dispatcher maps them at the boundary.
pub type GrzError = c_int;
pub const GRZ_NOT_ENOUGH_MEMORY: GrzError = -1;
pub const GRZ_CRC_ERROR: GrzError = -2;
pub const GRZ_UNEXPECTED_EOF: GrzError = -3;
pub const GRZ_NOT_COMPRESSIBLE: GrzError = -4;

/// `GRZ_MaxBlockSize` (:54). Every length in a block header is bounded by this,
/// which is what makes a corrupt header a rejection rather than a wild
/// allocation.
pub const GRZ_MAX_BLOCK_SIZE: usize = 8 * 1024 * 1024 - 512;

/// Mode bits (:61-67).
pub const GRZ_COMPRESSION_ST4: i32 = 0x2;
pub const GRZ_COMPRESSION_MTF: i32 = 0x4;

/// The block header is 28 bytes: seven little-endian 32-bit words, of which
/// two are reserved constants used as a weak integrity check.
pub const BLOCK_HEADER: usize = 28;

#[cfg(test)]
mod tests {
    use super::rec;

    /// Modes 1 and 2 are pure de-interleaves, so a round trip through the
    /// obvious inverse pins the byte order without needing the C.
    #[test]
    fn plain_deinterleave_restores_record_order() {
        // Mode 1: input is [all first bytes][all second bytes].
        let size = 8;
        let input = [1u8, 3, 5, 7, 2, 4, 6, 8];
        let mut out = vec![0u8; size];
        rec::decode(&input, size, &mut out, 1);
        assert_eq!(out, vec![1, 2, 3, 4, 5, 6, 7, 8]);

        // Mode 2: four planes.
        let input = [1u8, 5, 2, 6, 3, 7, 4, 8];
        let mut out = vec![0u8; size];
        rec::decode(&input, size, &mut out, 2);
        assert_eq!(out, vec![1, 2, 3, 4, 5, 6, 7, 8]);
    }

    /// The zigzag uses a bitwise complement, not a negation. An even delta is a
    /// plain right shift, so a run of zero deltas must reproduce the seed.
    #[test]
    fn delta_records_accumulate_from_zero() {
        // Mode 3, two 16-bit records, both deltas even (no sign bit).
        // Bytes are [high plane][low plane]: delta_i = (input[i]<<8)|input[i+n].
        let size = 4;
        let input = [0u8, 0, 4, 0]; // deltas 0x0004 -> 2, and 0x0000 -> 0
        let mut out = vec![0u8; size];
        rec::decode(&input, size, &mut out, 3);
        // first record 2, second 2+0
        assert_eq!(u16::from_le_bytes([out[0], out[1]]), 2);
        assert_eq!(u16::from_le_bytes([out[2], out[3]]), 2);
    }

    #[test]
    fn oversized_or_empty_input_is_ignored_not_panicking() {
        let mut out = vec![0u8; 4];
        rec::decode(&[], 0, &mut out, 3);
        rec::decode(&[1, 2], 99, &mut out, 4); // size beyond the buffers
        rec::decode(&[1, 2, 3, 4], 4, &mut out, 77); // unknown mode
    }
}
