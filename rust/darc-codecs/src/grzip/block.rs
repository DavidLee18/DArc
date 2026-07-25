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

// ---------------------------------------------------------------------------
// Encoder: GRZip_CompressBlock (C_GRZip.cpp:75).
//
// Chains the stages and decides, at three separate points, to give up and store
// the block verbatim instead. It is also RECURSIVE: when the record filter
// fires, the block is de-interleaved and then split into 2 or 4 parts, each of
// which re-enters this function with GRZ_Disable_DeltaFlt set so the split
// happens once.
//
// The 28-byte header is fixed:
//   [0]  original size          [4]  mode, or -1 stored, or -2 record-split
//   [8]  size after LZP         [12] the BWT's first-byte position
//   [16] coded size             [20] reserved   [24] reserved
// ---------------------------------------------------------------------------

use super::{BLOCK_HEADER, GRZ_COMPRESSION_MTF, GRZ_COMPRESSION_ST4, GRZ_NOT_ENOUGH_MEMORY};

/// `GRZ_Disable_DeltaFlt` (libGRZip.h:62).
const DISABLE_DELTA_FLT: i32 = 0x1;
/// `GRZ_BWTSorting_Fast` (:71).
const BWT_SORTING_FAST: i32 = 0x8;

/// `LZP_Enabled` / `Disable_LZP` / `DisableAllButLZP` (:77-79). The LZP
/// parameters live in the mode word above bit 8, so these are division rather
/// than masking.
fn lzp_on(mode: i32) -> bool {
    mode / 256 != 0
}
fn lzp_off(mode: i32) -> i32 {
    mode % 256
}
fn only_lzp(mode: i32) -> i32 {
    mode / 256 * 256
}

fn put_word(out: &mut [u8], at: usize, v: i32) {
    out[at..at + 4].copy_from_slice(&v.to_le_bytes());
}

/// `GRZip_StoreBlock` (:62): the block verbatim behind a header saying so.
fn store_block(input: &[u8], size: usize, out: &mut [u8], mode: i32) -> usize {
    put_word(out, 4, -1);
    put_word(out, 8, only_lzp(mode));
    put_word(out, 12, 0);
    put_word(out, 16, size as i32);
    out[BLOCK_HEADER..BLOCK_HEADER + size].copy_from_slice(&input[..size]);
    put_word(out, 20, 0);
    put_word(out, 24, 0);
    size + BLOCK_HEADER
}

/// `GRZip_CompressBlock`. Returns the number of bytes written to `out`.
pub fn compress_block(input: &[u8], size: usize, out: &mut [u8], mode: i32) -> Result<usize, GrzError> {
    if out.len() < size + BLOCK_HEADER + 1024 {
        return Err(GRZ_NOT_ENOUGH_MEMORY);
    }
    put_word(out, 0, size as i32);

    if size < 32 || size > GRZ_MAX_BLOCK_SIZE {
        return Ok(store_block(input, size, out, 0));
    }

    let mut mode = mode;
    // Below 1 KB the sort-transform is always cheaper than the BWT.
    if size < 1024 {
        mode |= GRZ_COMPRESSION_ST4;
    }

    // Record filter: de-interleave, then compress each part independently.
    if size > 1024 && (mode & DISABLE_DELTA_FLT) == 0 {
        let rec_mode = super::rec::test(input, size);
        if rec_mode != 0 {
            let mut buffer = vec![0u8; size + 1024];
            super::rec::encode(input, size, &mut buffer, rec_mode);
            let sub_mode = mode + DISABLE_DELTA_FLT;
            // Modes 1 and 3 split in two, 2 and 4 in four -- the low bit says
            // which, matching the record width the filter chose.
            let parts: Vec<(usize, usize)> = if rec_mode & 1 == 1 {
                let half = size >> 1;
                vec![(0, half), (half, size - half)]
            } else {
                let q = size >> 2;
                vec![(0, q), (q, q), (2 * q, q), (3 * q, size - 3 * q)]
            };
            let mut new_size = 0usize;
            let mut ok = true;
            for (off, len) in parts {
                let mut sub = vec![0u8; len + BLOCK_HEADER + 1024];
                match compress_block(&buffer[off..off + len], len, &mut sub, sub_mode) {
                    Ok(n) => {
                        if BLOCK_HEADER + new_size + n > out.len() {
                            ok = false;
                            break;
                        }
                        out[BLOCK_HEADER + new_size..BLOCK_HEADER + new_size + n]
                            .copy_from_slice(&sub[..n]);
                        new_size += n;
                    }
                    Err(_) => {
                        ok = false;
                        break;
                    }
                }
            }
            if ok && new_size < size {
                put_word(out, 4, -2);
                put_word(out, 8, rec_mode);
                put_word(out, 16, new_size as i32);
                put_word(out, 20, 0);
                put_word(out, 24, 0);
                return Ok(new_size + BLOCK_HEADER);
            }
            return Ok(store_block(input, size, out, 0));
        }
    }

    // LZP, or a straight copy when it is disabled or declines.
    let mut work = vec![0u8; size + 1024];
    let mut cur = size;
    if lzp_on(mode) {
        let mml = ((mode / 65536) % 32767) as u32;
        let ht = (1u32 << ((mode / 256) % 256)) - 1;
        match super::lzp::encode(&input[..size], &mut work, mml, ht) {
            Ok(n) => {
                put_word(out, 8, n as i32);
                cur = n;
            }
            Err(super::GRZ_NOT_ENOUGH_MEMORY) => {
                return Ok(store_block(input, size, out, 0));
            }
            Err(_) => {
                // Not compressible: carry on with the raw bytes, LZP disabled.
                mode = lzp_off(mode);
                work[..size].copy_from_slice(&input[..size]);
                put_word(out, 8, size as i32);
            }
        }
    } else {
        work[..size].copy_from_slice(&input[..size]);
        put_word(out, 8, size as i32);
    }

    // Pad to a multiple of 8 with zeros: the transforms read in 8-byte steps.
    for b in work[cur..cur + 8].iter_mut() {
        *b = 0;
    }
    let padded = (cur + 7) & !7usize;

    // Failing either transform or the entropy stage means storing -- and if LZP
    // ran, storing its OUTPUT rather than the original, which is why the mode
    // word keeps its LZP bits in that case.
    let store_fallback = |out: &mut [u8]| -> usize {
        if lzp_on(mode) {
            let mut lz = vec![0u8; size + 1024];
            let mml = ((mode / 65536) % 32767) as u32;
            let ht = (1u32 << ((mode / 256) % 256)) - 1;
            if let Ok(n) = super::lzp::encode(&input[..size], &mut lz, mml, ht) {
                return store_block(&lz, n, out, mode);
            }
        }
        store_block(input, size, out, 0)
    };

    let mut transformed = work.clone();
    let fbp = if mode & GRZ_COMPRESSION_ST4 != 0 {
        match super::st4::encode(&work, padded, &mut transformed) {
            Ok(v) => v,
            Err(_) => return Ok(store_fallback(out)),
        }
    } else {
        match super::bwt::encode(&work, padded, &mut transformed, mode & BWT_SORTING_FAST != 0) {
            Ok(v) => v,
            Err(_) => return Ok(store_fallback(out)),
        }
    };
    put_word(out, 12, fbp);

    let coded = if mode & GRZ_COMPRESSION_MTF != 0 {
        super::mtf_ari::encode(&transformed[..padded])
    } else {
        super::wfc_ari::encode(&transformed[..padded])
    };
    let coded = match coded {
        Ok(v) => v,
        Err(_) => return Ok(store_fallback(out)),
    };
    if BLOCK_HEADER + coded.len() > out.len() {
        return Ok(store_fallback(out));
    }
    out[BLOCK_HEADER..BLOCK_HEADER + coded.len()].copy_from_slice(&coded);

    put_word(out, 4, mode);
    put_word(out, 16, coded.len() as i32);
    put_word(out, 20, 0);
    put_word(out, 24, 0);
    Ok(coded.len() + BLOCK_HEADER)
}

#[cfg(test)]
mod block_tests {
    use super::*;

    /// Compress then decompress must be the identity, across every branch the
    /// driver can take: stored, LZP'd, ST4 vs BWT, MTF vs WFC, and the
    /// recursive record-filter split.
    #[test]
    fn block_round_trips() {
        fn lcg(seed: u32, n: usize) -> Vec<u8> {
            let mut st = seed;
            (0..n).map(|_| { st = st.wrapping_mul(1103515245).wrapping_add(12345); (st >> 16) as u8 }).collect()
        }
        let mut cases: Vec<(&str, Vec<u8>)> = vec![
            ("tiny", b"short".to_vec()),
            ("text", b"the quick brown fox jumps over the lazy dog. ".repeat(200)),
            ("noise", lcg(31, 50000)),
            ("runs", (0..40000u32).map(|i| ((i / 64) % 4) as u8 + b'p').collect()),
            ("zeros", vec![0u8; 30000]),
        ];
        // Fixed-width records, so the recursive split actually fires.
        let mut rec = Vec::new();
        for i in 0..20000u32 {
            rec.extend_from_slice(&((i * 3) as u16).to_le_bytes());
        }
        cases.push(("records", rec));

        // Mode words spanning both transforms and both entropy coders, with LZP
        // on and off.
        for &mode in &[0i32, GRZ_COMPRESSION_ST4, GRZ_COMPRESSION_MTF,
                       GRZ_COMPRESSION_ST4 | GRZ_COMPRESSION_MTF,
                       BWT_SORTING_FAST,
                       (15 << 8) + (32 << 16) + BWT_SORTING_FAST] {
            for (name, input) in cases.iter() {
                let size = input.len();
                let mut out = vec![0u8; size + BLOCK_HEADER + 4096];
                let n = compress_block(input, size, &mut out, mode).expect("compress");
                let mut back = vec![0u8; size + 64];
                let got = decompress_block(&out[..n], &mut back).expect("decompress");
                assert_eq!(got, size, "{name} mode={mode:#x}: length");
                assert_eq!(&back[..got], &input[..], "{name} mode={mode:#x}: bytes");
            }
        }
    }
}
