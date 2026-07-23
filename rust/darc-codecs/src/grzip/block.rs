//! The block dispatcher, ported from `Compression/GRZip/C_GRZip.cpp`
//! (`GRZip_DecompressBlock` :233).
//!
//! Every GRZip block carries a 28-byte header of seven little-endian 32-bit
//! words. Two of them are fixed constants used as a weak integrity check; the
//! rest are the decompressed size, the mode, an intermediate size, the inverse
//! transform's first-byte position, and the compressed length.
//!
//! `Mode` selects the pipeline:
//!
//! | `Mode` | meaning |
//! |---|---|
//! | `-1` | the payload is stored, or LZP-only |
//! | `-2` | **recursive**: 2 or 4 nested blocks, recombined by `rec::decode` |
//! | else | arithmetic decode, then inverse BWT or ST4, then optionally LZP |
//!
//! ## The recursion needs a bound the C does not have
//!
//! A `Mode == -2` block contains whole blocks, each decoded by calling straight
//! back into this function, and nothing in the C limits how deep that goes --
//! the nesting comes entirely from attacker-controlled header bytes. In C the
//! consequence is stack exhaustion; here it would be the same, and a stack
//! overflow is not something a `Result` can catch. `MAX_DEPTH` bounds it.
//!
//! Real blocks nest exactly once: the encoder emits `Mode == -2` only from
//! `GRZip_CompressBlock`'s record-filter path, which then emits plain blocks.

use super::{bwt, lzp, mtf_ari, rec, st4, wfc_ari, GrzError, GRZ_CRC_ERROR, GRZ_MAX_BLOCK_SIZE, GRZ_UNEXPECTED_EOF};

/// `RESERVED` -- the constant stored in two header words as an integrity check.
const RESERVED: i32 = 0;

/// Block header layout, in bytes.
const HDR: usize = 28;

/// Mode bits (`libGRZip.h:61-77`).
const COMPRESSION_ST4: i32 = 0x2;
const COMPRESSION_MTF: i32 = 0x4;

/// How deeply `Mode == -2` may nest. The encoder produces exactly one level;
/// anything past a handful is a malformed block rather than a deep one.
const MAX_DEPTH: u32 = 8;

fn word(b: &[u8], at: usize) -> i32 {
    i32::from_le_bytes([b[at], b[at + 1], b[at + 2], b[at + 3]])
}

/// `LZP_Enabled` / `Get_LZP_MinMatchLen` / `Get_LZP_HT_Size` (:75-77). Note
/// these are plain division rather than shifts, and `Mode` is signed -- so a
/// negative mode would divide toward zero. Only non-negative modes reach here.
fn lzp_enabled(mode: i32) -> bool {
    mode / 256 != 0
}
fn lzp_min_match_len(mode: i32) -> u32 {
    (mode / 65536 % 32767) as u32
}
fn lzp_ht_size(mode: i32) -> u32 {
    (1u32 << (mode / 256 % 256)).wrapping_sub(1)
}

/// `GRZip_DecompressBlock`. Returns the number of bytes written to `out`.
pub fn decompress_block(input: &[u8], out: &mut [u8]) -> Result<usize, GrzError> {
    decompress_block_at(input, out, 0)
}

fn decompress_block_at(input: &[u8], out: &mut [u8], depth: u32) -> Result<usize, GrzError> {
    if depth > MAX_DEPTH {
        return Err(GRZ_CRC_ERROR);
    }
    if input.len() < HDR {
        return Err(GRZ_UNEXPECTED_EOF);
    }
    if word(input, 24) != RESERVED || word(input, 20) != RESERVED {
        return Err(GRZ_CRC_ERROR);
    }
    let packed = word(input, 16);
    if packed < 0 || (packed as usize) + HDR > input.len() {
        return Err(GRZ_UNEXPECTED_EOF);
    }
    // Sizes feed allocations and bound the decode; unchecked they produced wild
    // allocations in the C until an earlier hardening pass.
    let raw_size = word(input, 0);
    let mid_size = word(input, 8);
    if raw_size < 0 || raw_size as usize > GRZ_MAX_BLOCK_SIZE {
        return Err(GRZ_CRC_ERROR);
    }
    if mid_size < 0 || mid_size as usize > GRZ_MAX_BLOCK_SIZE {
        return Err(GRZ_CRC_ERROR);
    }

    let mode = word(input, 4);
    let body = &input[HDR..HDR + packed as usize];

    if mode == -1 {
        // Stored, or LZP alone.
        let inner = mid_size;
        if inner == 0 {
            let n = packed as usize;
            if out.len() < n {
                return Err(GRZ_UNEXPECTED_EOF);
            }
            out[..n].copy_from_slice(body);
            return Ok(n);
        }
        let n = raw_size as usize;
        if out.len() < n {
            return Err(GRZ_UNEXPECTED_EOF);
        }
        return lzp::decode(body, &mut out[..n], lzp_min_match_len(inner), lzp_ht_size(inner));
    }

    if mode == -2 {
        // Recursive: `RecMode` decides 2 or 4 sub-blocks laid end to end, each
        // a complete block with its own header.
        let rec_mode = mid_size;
        let size = raw_size as usize;
        let parts = if rec_mode & 1 == 1 { 2 } else { 4 };
        let mut buf = vec![0u8; size + 1024];
        let mut at = 0usize; // offset within `body`
        let mut written = 0usize;
        for _ in 0..parts {
            if at + HDR > body.len() {
                return Err(GRZ_UNEXPECTED_EOF);
            }
            let sub_packed = word(body, at + 16);
            if sub_packed < 0 {
                return Err(GRZ_CRC_ERROR);
            }
            let sub_len = sub_packed as usize + HDR;
            if at + sub_len > body.len() {
                return Err(GRZ_UNEXPECTED_EOF);
            }
            if written > buf.len() {
                return Err(GRZ_CRC_ERROR);
            }
            let n = decompress_block_at(&body[at..at + sub_len], &mut buf[written..], depth + 1)?;
            written += n;
            at += sub_len;
        }
        if out.len() < size {
            return Err(GRZ_UNEXPECTED_EOF);
        }
        rec::decode(&buf, size, &mut out[..size], rec_mode);
        return Ok(size);
    }

    // The main pipeline. The compressor rounds the block up to a multiple of 8
    // before the transform and entropy stages but stores the *unrounded* length,
    // so the arithmetic decoder legitimately produces up to 7 bytes more.
    let ari_out_size = ((mid_size as usize) + 7) & !7;
    let mut work = vec![0u8; ari_out_size + 1024];

    let t_size = entropy_decode(body, &mut work, ari_out_size, mode)?;

    let fbp = word(input, 12);
    if mode & COMPRESSION_ST4 != 0 {
        st4::decode(&mut work, t_size, fbp)?;
    } else {
        bwt::decode(&mut work, t_size, fbp)?;
    }

    let n = mid_size as usize;
    if work.len() < n {
        return Err(GRZ_UNEXPECTED_EOF);
    }
    if lzp_enabled(mode) {
        let want = raw_size as usize;
        if out.len() < want {
            return Err(GRZ_UNEXPECTED_EOF);
        }
        lzp::decode(
            &work[..n],
            &mut out[..want],
            lzp_min_match_len(mode),
            lzp_ht_size(mode),
        )?;
    } else {
        if out.len() < n {
            return Err(GRZ_UNEXPECTED_EOF);
        }
        out[..n].copy_from_slice(&work[..n]);
    }
    Ok(raw_size as usize)
}

/// The entropy stage: MTF-arith or WFC-arith per `GRZ_Compression_MTF`.
fn entropy_decode(
    body: &[u8],
    work: &mut [u8],
    out_size: usize,
    mode: i32,
) -> Result<usize, GrzError> {
    if mode & COMPRESSION_MTF != 0 {
        mtf_ari::decode(body, work, out_size)
    } else {
        wfc_ari::decode(body, work, out_size)
    }
}
