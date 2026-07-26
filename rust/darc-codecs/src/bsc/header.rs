//! The 28-byte block header, ported from
//! `Compression/BSC/libbsc/libbsc/libbsc.cpp` (`bsc_block_info`, and the
//! header reads at the top of `bsc_decompress` :522).
//!
//! Six little-endian 32-bit fields plus the packed `mode` word. Everything the
//! decoder needs to know about which stages ran is in `mode`, and the three
//! Adler-32 checksums are what make a corrupt header a rejection rather than a
//! wild allocation.

use super::adler32::adler32;
use super::{
    BLOCKSORTER_BWT, BLOCKSORTER_ST3, BLOCKSORTER_ST8, CODER_QLFC_FAST, CODER_QLFC_STATIC,
    HEADER_SIZE, LIBBSC_DATA_CORRUPT, LIBBSC_UNEXPECTED_EOB,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BlockHeader {
    /// Total bytes of the block, header included.
    pub block_size: i32,
    /// Decompressed size.
    pub data_size: i32,
    pub mode: i32,
    /// Block-sort primary index.
    pub index: i32,
    /// Checksum of the decompressed data.
    pub adler32_data: u32,
    pub lzp_hash_size: u32,
    pub lzp_min_len: u32,
    pub coder: u32,
    pub block_sorter: u32,
}

impl BlockHeader {
    /// `mode == 0`: the payload is stored verbatim, no stages at all.
    pub fn is_stored(&self) -> bool {
        self.mode == 0
    }

    /// LZP ran iff any bit above the low byte is set. The C tests
    /// `mode != (mode & 0xff)` rather than a dedicated flag, because the LZP
    /// parameters themselves live in bits 8..23 -- so "LZP was applied" and
    /// "LZP has parameters" are the same condition.
    pub fn has_lzp(&self) -> bool {
        self.mode != (self.mode & 0xff)
    }
}

fn word(b: &[u8], at: usize) -> u32 {
    u32::from_le_bytes([b[at], b[at + 1], b[at + 2], b[at + 3]])
}

/// `bsc_block_info`: validate the header and unpack it.
///
/// Returns the libbsc error code on failure, matching the C's returns so the
/// caller's error mapping is unchanged.
pub fn parse(header: &[u8]) -> Result<BlockHeader, i32> {
    if header.len() < HEADER_SIZE {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    // The header checksums itself over its first 24 bytes.
    if word(header, 24) != adler32(&header[..24]) {
        return Err(LIBBSC_DATA_CORRUPT);
    }

    let block_size = word(header, 0) as i32;
    let data_size = word(header, 4) as i32;
    let mode = word(header, 8) as i32;
    let index = word(header, 12) as i32;
    let adler32_data = word(header, 16);

    let lzp_hash_size = ((mode >> 16) & 0xff) as u32;
    let lzp_min_len = ((mode >> 8) & 0xff) as u32;
    let coder = ((mode >> 5) & 0x7) as u32;
    let block_sorter = (mode & 0x1f) as u32;

    let h = BlockHeader {
        block_size,
        data_size,
        mode,
        index,
        adler32_data,
        lzp_hash_size,
        lzp_min_len,
        coder,
        block_sorter,
    };

    // mode == 0 is "stored"; it carries no stage selection to validate, and the
    // C's reconstruction of `mode` trivially matches. The size bounds below
    // still apply to it.
    if !h.is_stored() {
        // The C rebuilds `mode` from the unpacked fields and requires it to
        // match, which rejects any combination the encoder could not have
        // produced -- including reserved sorter and coder values.
        let sorter_ok = block_sorter == BLOCKSORTER_BWT
            || (BLOCKSORTER_ST3..=BLOCKSORTER_ST8).contains(&block_sorter);
        let coder_ok = (CODER_QLFC_STATIC..=CODER_QLFC_FAST).contains(&coder);
        if !sorter_ok || !coder_ok {
            return Err(LIBBSC_DATA_CORRUPT);
        }

        // The LZP parameters are only validated when present, matching the C's
        // `if (lzpMinLen != 0 || lzpHashSize != 0)` guard.
        if lzp_min_len != 0 || lzp_hash_size != 0 {
            if !(4..=255).contains(&lzp_min_len) {
                return Err(LIBBSC_DATA_CORRUPT);
            }
            if !(10..=28).contains(&lzp_hash_size) {
                return Err(LIBBSC_DATA_CORRUPT);
            }
        }
    }

    // Size bounds. These used to live apart from this function, so a caller
    // that used `parse` alone got weaker validation than the C -- it would size
    // a buffer from corrupt input instead of rejecting it. There is no such
    // split in the C: `bsc_decompress` (libbsc.cpp:534) validates by calling
    // `bsc_block_info`, which does all of this in one place. Keeping them here
    // makes `parse` equal to `bsc_block_info` and safe by default.
    if block_size < HEADER_SIZE as i32 || block_size > HEADER_SIZE as i32 + data_size {
        return Err(LIBBSC_DATA_CORRUPT);
    }
    if index < 0 || index > data_size {
        return Err(LIBBSC_DATA_CORRUPT);
    }

    Ok(h)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bsc::{BLOCKSORTER_ST4, CODER_QLFC_ADAPTIVE};

    /// Build a well-formed header with correct checksums.
    fn make(block_size: i32, data_size: i32, mode: i32, index: i32) -> Vec<u8> {
        let mut h = vec![0u8; HEADER_SIZE];
        h[0..4].copy_from_slice(&(block_size as u32).to_le_bytes());
        h[4..8].copy_from_slice(&(data_size as u32).to_le_bytes());
        h[8..12].copy_from_slice(&(mode as u32).to_le_bytes());
        h[12..16].copy_from_slice(&(index as u32).to_le_bytes());
        h[16..20].copy_from_slice(&0u32.to_le_bytes());
        h[20..24].copy_from_slice(&0u32.to_le_bytes());
        let ck = adler32(&h[..24]);
        h[24..28].copy_from_slice(&ck.to_le_bytes());
        h
    }

    #[test]
    fn mode_unpacks_into_the_four_stage_fields() {
        // hash 17, minlen 32, coder QLFC_ADAPTIVE, sorter ST4.
        let mode = (17 << 16) | (32 << 8) | ((CODER_QLFC_ADAPTIVE as i32) << 5) | BLOCKSORTER_ST4 as i32;
        let h = parse(&make(900, 900, mode, 1)).unwrap();
        assert_eq!(h.lzp_hash_size, 17);
        assert_eq!(h.lzp_min_len, 32);
        assert_eq!(h.coder, CODER_QLFC_ADAPTIVE);
        assert_eq!(h.block_sorter, BLOCKSORTER_ST4);
        assert!(h.has_lzp(), "upper mode bits set means LZP ran");
        assert!(!h.is_stored());
    }

    /// "LZP ran" is derived from the upper bits, not a flag -- a mode with only
    /// sorter and coder set must report no LZP.
    #[test]
    fn lzp_is_detected_from_the_upper_mode_bits() {
        let mode = ((CODER_QLFC_STATIC as i32) << 5) | BLOCKSORTER_BWT as i32;
        let h = parse(&make(900, 900, mode, 1)).unwrap();
        assert!(!h.has_lzp());
        assert_eq!(h.lzp_hash_size, 0);
        assert_eq!(h.lzp_min_len, 0);
    }

    #[test]
    fn stored_blocks_carry_no_stages() {
        let h = parse(&make(100, 72, 0, 0)).unwrap();
        assert!(h.is_stored());
    }

    #[test]
    fn a_corrupt_header_checksum_is_rejected() {
        let mut h = make(1000, 900, 1, 1);
        h[2] ^= 0xff; // change a field without fixing the checksum
        assert_eq!(parse(&h), Err(LIBBSC_DATA_CORRUPT));
    }

    /// The size bounds `bsc_block_info` applies. These lived outside `parse`
    /// until the checks were folded in, so a caller using `parse` alone got a
    /// header the C would have rejected -- and then sized a buffer from it.
    #[test]
    fn out_of_range_sizes_are_rejected() {
        let mode = ((CODER_QLFC_STATIC as i32) << 5) | BLOCKSORTER_BWT as i32;
        // blockSize may not exceed HEADER_SIZE + dataSize ...
        assert_eq!(parse(&make(1000, 900, mode, 1)), Err(LIBBSC_DATA_CORRUPT));
        // ... nor fall below the header itself ...
        assert_eq!(parse(&make(20, 900, mode, 1)), Err(LIBBSC_DATA_CORRUPT));
        // ... and the boundary itself is legal.
        assert!(parse(&make(900 + HEADER_SIZE as i32, 900, mode, 1)).is_ok());
        // The primary index must land inside the decoded block.
        assert_eq!(parse(&make(900, 900, mode, 901)), Err(LIBBSC_DATA_CORRUPT));
        assert_eq!(parse(&make(900, 900, mode, -1)), Err(LIBBSC_DATA_CORRUPT));
        assert!(parse(&make(900, 900, mode, 900)).is_ok());
    }

    /// A stored block skips the stage checks but NOT the size bounds -- the C
    /// reaches them for mode == 0 too.
    #[test]
    fn stored_blocks_are_still_size_checked() {
        assert_eq!(parse(&make(1000, 900, 0, 0)), Err(LIBBSC_DATA_CORRUPT));
        assert_eq!(parse(&make(100, 72, 0, 73)), Err(LIBBSC_DATA_CORRUPT));
    }

    /// The LZP parameters are validated only when present, matching the C's
    /// `if (lzpMinLen != 0 || lzpHashSize != 0)` guard.
    #[test]
    fn out_of_range_lzp_parameters_are_rejected() {
        let base = ((CODER_QLFC_STATIC as i32) << 5) | BLOCKSORTER_BWT as i32;
        let with = |hash: i32, minlen: i32| (hash << 16) | (minlen << 8) | base;
        // minlen below 4, hash below 10, hash above 28.
        assert_eq!(parse(&make(900, 900, with(15, 3), 1)), Err(LIBBSC_DATA_CORRUPT));
        assert_eq!(parse(&make(900, 900, with(9, 72), 1)), Err(LIBBSC_DATA_CORRUPT));
        assert_eq!(parse(&make(900, 900, with(29, 72), 1)), Err(LIBBSC_DATA_CORRUPT));
        // DArc's own defaults, and the range boundaries, are accepted.
        assert!(parse(&make(900, 900, with(15, 72), 1)).is_ok());
        assert!(parse(&make(900, 900, with(10, 4), 1)).is_ok());
        assert!(parse(&make(900, 900, with(28, 255), 1)).is_ok());
    }

    #[test]
    fn short_and_invalid_stage_selections_are_rejected() {
        assert_eq!(parse(&[0u8; 10]), Err(LIBBSC_UNEXPECTED_EOB));
        // Sorter 2 and 9..31 are unassigned; coder 4..7 likewise.
        for bad_sorter in [2i32, 9, 31] {
            let mode = ((CODER_QLFC_STATIC as i32) << 5) | bad_sorter;
            assert_eq!(parse(&make(100, 90, mode, 1)), Err(LIBBSC_DATA_CORRUPT), "sorter {bad_sorter}");
        }
        for bad_coder in [0i32, 4, 7] {
            let mode = (bad_coder << 5) | BLOCKSORTER_BWT as i32;
            assert_eq!(parse(&make(100, 90, mode, 1)), Err(LIBBSC_DATA_CORRUPT), "coder {bad_coder}");
        }
    }
}
