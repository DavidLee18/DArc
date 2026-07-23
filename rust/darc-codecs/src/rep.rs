//! REP decoder, ported from `Compression/REP/rep.cpp` (`rep_decompress`).
//!
//! REP is a huge-dictionary LZ preprocessor. Its block format is explicit
//! rather than bit-packed: a block is a count `num`, three parallel tables
//! (`lens`, `offsets`, each `num` entries; `datalens`, `num+1`), then the
//! literal bytes those tables interleave with matches. Decoding is therefore a
//! straight walk, which is why the decoder ports cleanly ahead of the encoder
//! (the same decode-first order used for Dict).
//!
//! Output goes into a circular buffer of `BlockSize`; a match offset that would
//! reach before the buffer start wraps by subtracting `BlockSize`. The
//! wraparound only ever happens at a block boundary, so a block's decoded bytes
//! are always contiguous in the buffer.
//!
//! Every length and offset read from the stream is untrusted -- the decoder is
//! fed raw archive bytes and runs on `arc t` -- so each is validated against
//! the remaining input and output before use, mirroring the bounds the C added
//! during the v2.0.0 hardening. A single flipped byte in a `-mrep` archive
//! reaches these checks.

use crate::ffi::{Io, FREEARC_ERRCODE_BAD_COMPRESSED_DATA, FREEARC_ERRCODE_IO,
                 FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, OK};
use core::ffi::c_int;

const BAD: c_int = FREEARC_ERRCODE_BAD_COMPRESSED_DATA as c_int;
const IO: c_int = FREEARC_ERRCODE_IO as c_int;
const NOMEM: c_int = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY as c_int;

/// Read exactly `buf.len()` bytes, or return an error. The C `READ`/`READ4`
/// macros require the callback to fill the whole request; a short read is EOF
/// or an I/O error, never a partial success.
fn read_exact(io: &Io, buf: &mut [u8]) -> Result<(), c_int> {
    if buf.is_empty() {
        return Ok(());
    }
    match io.read(buf) {
        n if n as usize == buf.len() => Ok(()),
        n if n >= 0 => Err(IO), // short read where the format demands a full one
        n => Err(n),
    }
}

fn read_u32(io: &Io) -> Result<u32, c_int> {
    let mut b = [0u8; 4];
    read_exact(io, &mut b)?;
    Ok(u32::from_le_bytes(b))
}

fn i32_at(buf: &[u8], off: usize) -> i32 {
    i32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}

/// Decode a REP stream. Signature-compatible with `rep_decompress`; the tuning
/// parameters are not needed on the decode side (the block size that matters is
/// stored in the stream), so they are accepted and ignored, as the C does.
pub fn decompress(io: &Io) -> c_int {
    match run(io) {
        Ok(()) => OK,
        Err(e) => e,
    }
}

fn run(io: &Io) -> Result<(), c_int> {
    // The real dictionary size is the first word of the stream.
    let block_size = read_u32(io)? as usize;
    if block_size == 0 {
        return Err(BAD);
    }
    let mut data = vec![0u8; block_size];
    let mut pos: usize = 0; // current write index into `data` (the circular buffer)

    loop {
        let compr_size = read_u32(io)? as i32;
        if compr_size == 0 {
            break; // EOF marker
        }
        // Smallest legal block is `num` plus `datalens[0]`: two int32s.
        if compr_size < 2 * 4 {
            return Err(BAD);
        }
        let compr_size = compr_size as usize;

        let mut buf = vec![0u8; compr_size];
        read_exact(io, &mut buf)?;

        // Header: num, then lens[num], offsets[num], datalens[num+1]. num sizes
        // three tables plus itself: 4*(3*num+2) bytes, checked in 64-bit before
        // any table offset is derived so a corrupt num cannot overflow it.
        let num = i32_at(&buf, 0);
        if num < 0 || 4i64 * (3 * num as i64 + 2) > compr_size as i64 {
            return Err(BAD);
        }
        let num = num as usize;
        let lens_off = 4;
        let offsets_off = lens_off + 4 * num;
        let datalens_off = offsets_off + 4 * num;
        let mut bp = datalens_off + 4 * (num + 1); // literal data starts here

        let block_start = pos; // decoded bytes of this block are contiguous from here

        let lens = |i: usize| i32_at(&buf, lens_off + 4 * i);
        let offsets = |i: usize| i32_at(&buf, offsets_off + 4 * i);
        let datalens = |i: usize| i32_at(&buf, datalens_off + 4 * i);

        for i in 0..num {
            // literal run
            let dl = datalens(i);
            if dl < 0 || dl as usize > buf.len() - bp || dl as usize > block_size - pos {
                return Err(BAD);
            }
            let dl = dl as usize;
            data[pos..pos + dl].copy_from_slice(&buf[bp..bp + dl]);
            bp += dl;
            pos += dl;

            // match: offset relative to the current position, wrapping the buffer
            let raw_off = offsets(i);
            let offset = if raw_off as i64 <= pos as i64 {
                raw_off as i64
            } else {
                raw_off as i64 - block_size as i64
            };
            let ln = lens(i);
            if offset <= 0 || offset > pos as i64 || ln < 0 || ln as usize > block_size - pos {
                return Err(BAD);
            }
            let src = pos - offset as usize;
            // Overlapping LZ copy, byte at a time exactly as memcpy_lz_match.
            for k in 0..ln as usize {
                data[pos + k] = data[src + k];
            }
            pos += ln as usize;
        }

        // One trailing literal run (possibly empty).
        let dl = datalens(num);
        if dl < 0 || dl as usize > buf.len() - bp || dl as usize > block_size - pos {
            return Err(BAD);
        }
        let dl = dl as usize;
        data[pos..pos + dl].copy_from_slice(&buf[bp..bp + dl]);
        pos += dl;

        // Flush this block's decoded bytes (contiguous from block_start).
        let out = &data[block_start..pos];
        if !out.is_empty() {
            let n = io.write(out);
            if (n as usize) != out.len() {
                return Err(if n >= 0 { IO } else { n });
            }
        }

        // Wraparound happens only at a full buffer, and only at a block end.
        if pos == block_size {
            pos = 0;
        }
    }
    Ok(())
}

/// Signature-compatible entry with the unused tuning knobs, matching
/// `rep_decompress` so the C wrapper can forward straight through.
#[allow(clippy::too_many_arguments)]
pub fn decompress_full(
    io: &Io,
    _block_size: u32,
    _min_compression: c_int,
    _min_match_len: c_int,
    _barrier: c_int,
    _smallest_len: c_int,
    _hash_bits: c_int,
    _amplifier: c_int,
) -> c_int {
    let _ = NOMEM; // allocation-failure code, kept for parity with the C
    decompress(io)
}
