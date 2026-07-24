//! The block dispatcher, ported from `bsc_decompress`
//! (`Compression/BSC/libbsc/libbsc/libbsc.cpp` :522). This is the glue that
//! turns one framed block into its decompressed bytes by running the five
//! already-verified stages in order:
//!
//! ```text
//!   header validate  (bsc_block_info + the body Adler-32)
//!   entropy decode   (QLFC static/adaptive)          -> lzSize bytes
//!   inverse sort     (BWT, or ST3..ST8)              in place
//!   inverse LZP      (only when the mode's upper bits are set)
//!   Adler-32 check   (of the decompressed data)
//! ```
//!
//! The C's `bsc_decompress` re-runs `bsc_block_info` internally (the stream
//! wrapper also calls it once for framing), so this reproduces the full
//! `bsc_block_info` -- including the checks `header::parse` leaves out (the LZP
//! parameter ranges and the `blockSize`/`index` bounds) -- before touching the
//! payload.
//!
//! ## The one unported coder
//!
//! `CODER_QLFC_FAST` (3) is not ported yet, so a block coded with it is rejected
//! (`LIBBSC_DATA_CORRUPT`) rather than mis-decoded. That is a real gap for
//! wiring -- a fast-coder archive would fail to extract -- and must be closed,
//! or gated against, before the Rust dispatcher replaces the C one.

use super::header::{self, BlockHeader};
use super::{
    adler32::adler32, bwt, lzp, qlfc, st, BLOCKSORTER_BWT, BLOCKSORTER_ST3, BLOCKSORTER_ST8,
    HEADER_SIZE, LIBBSC_DATA_CORRUPT, LIBBSC_NO_ERROR, LIBBSC_UNEXPECTED_EOB,
};

fn word(b: &[u8], at: usize) -> u32 {
    u32::from_le_bytes([b[at], b[at + 1], b[at + 2], b[at + 3]])
}

/// The `bsc_block_info` checks that `header::parse` does not do: the LZP
/// parameter ranges (guarded on the params being present) and the `blockSize` /
/// `index` bounds. Returns the libbsc code the C would.
fn block_info_extra_checks(h: &BlockHeader) -> Result<(), i32> {
    if !h.is_stored() && (h.lzp_min_len != 0 || h.lzp_hash_size != 0) {
        if h.lzp_min_len < 4 || h.lzp_min_len > 255 {
            return Err(LIBBSC_DATA_CORRUPT);
        }
        if h.lzp_hash_size < 10 || h.lzp_hash_size > 28 {
            return Err(LIBBSC_DATA_CORRUPT);
        }
    }
    if h.block_size < HEADER_SIZE as i32 || h.block_size > HEADER_SIZE as i32 + h.data_size {
        return Err(LIBBSC_DATA_CORRUPT);
    }
    if h.index < 0 || h.index > h.data_size {
        return Err(LIBBSC_DATA_CORRUPT);
    }
    Ok(())
}

/// `bsc_decompress(input, inputSize, output, outputSize, features)`: decode one
/// framed block into `output`. Returns `LIBBSC_NO_ERROR` (0) or a negative
/// libbsc error code, exactly as the C does.
pub fn decompress(input: &[u8], output: &mut [u8]) -> i32 {
    let h = match header::parse(input) {
        Ok(h) => h,
        Err(e) => return e,
    };
    if let Err(e) = block_info_extra_checks(&h) {
        return e;
    }

    let block_size = h.block_size as usize;
    let data_size = h.data_size as usize;

    if input.len() < block_size || output.len() < data_size {
        return LIBBSC_UNEXPECTED_EOB;
    }

    // The body Adler-32 guards everything after the 28-byte header.
    if word(input, 20) != adler32(&input[HEADER_SIZE..block_size]) {
        return LIBBSC_DATA_CORRUPT;
    }

    if h.is_stored() {
        output[..data_size].copy_from_slice(&input[HEADER_SIZE..HEADER_SIZE + data_size]);
        return LIBBSC_NO_ERROR;
    }

    let index = h.index;
    let adler_data = h.adler32_data;

    // The auxiliary BWT indexes trail the payload: a count in the last byte,
    // then that many big-... little-endian i32s just before it.
    let num_indexes = input[block_size - 1];
    let mut indexes = [0i32; 256];
    if num_indexes > 0 {
        let base = match block_size.checked_sub(1 + 4 * num_indexes as usize) {
            Some(b) if b >= HEADER_SIZE => b,
            _ => return LIBBSC_DATA_CORRUPT,
        };
        for t in 0..num_indexes as usize {
            indexes[t] = word(input, base + 4 * t) as i32;
        }
    }

    // Entropy decode. bsc_coder_decompress splits large blocks into several
    // independently-coded sub-blocks, so this is the multi-block wrapper, not a
    // single decode. The coder self-delimits, so handing it the whole payload
    // (aux indexes included) is fine.
    let coded = &input[HEADER_SIZE..block_size];
    let lz_size = match qlfc::decompress(coded, output, h.coder) {
        Ok(n) => n,
        Err(e) => return e,
    };
    if lz_size > output.len() {
        return LIBBSC_DATA_CORRUPT;
    }

    // Inverse block sort, in place over the first lz_size bytes.
    let r = match h.block_sorter {
        BLOCKSORTER_BWT => bwt::bwt_decode(
            &mut output[..lz_size],
            lz_size,
            index,
            num_indexes,
            &indexes[..num_indexes as usize],
        ),
        s if (BLOCKSORTER_ST3..=BLOCKSORTER_ST8).contains(&s) => {
            st::st_decode(&mut output[..lz_size], lz_size, s, index)
        }
        _ => return LIBBSC_DATA_CORRUPT,
    };
    if r < 0 {
        return r;
    }

    // Inverse LZP, only when the mode carried LZP parameters.
    if h.has_lzp() {
        let tmp = output[..lz_size].to_vec();
        match lzp::decompress(&tmp, output, h.lzp_hash_size, h.lzp_min_len) {
            Ok(res) if res == data_size => {}
            Ok(_) => return LIBBSC_DATA_CORRUPT,
            Err(e) => return e,
        }
    } else if lz_size != data_size {
        return LIBBSC_DATA_CORRUPT;
    }

    if adler_data != adler32(&output[..data_size]) {
        return LIBBSC_DATA_CORRUPT;
    }
    LIBBSC_NO_ERROR
}
