//! GRZip's LZP stage, ported from `Compression/GRZip/LZP.c` (`GRZip_LZP_Decode`
//! :131).
//!
//! A context-predicting pre-filter, not a compressor in its own right: an order-4
//! rolling context hashes to a table of previous output positions, and when the
//! context repeats, a flag byte plus a run length replaces the matched bytes. It
//! runs both as the only stage (`Mode == -1`) and as the last stage after the
//! BWT/arithmetic pipeline.
//!
//! Note this is a *different* LZP from `Compression/LZP/`, already ported as
//! `crate::lzp` -- different flags, different hash, different stream. They share
//! only the idea.
//!
//! Three flag bytes drive it: `0xF2` marks a match, `0xF3` continues a run
//! length, and the run terminator's contribution is `byte ^ 0xFF^0xF3`. A run
//! that never terminates and a match length that outruns the output are both
//! reachable from a corrupt block; the C was fixed to bound each, and the same
//! bounds are here.

use super::{GrzError, GRZ_NOT_COMPRESSIBLE, GRZ_UNEXPECTED_EOF};

const MATCH_FLAG: u8 = 0xF2;
const RUN_FLAG: u8 = 0xF3;
const XOR_FLAG: u32 = (0xFFu8 ^ RUN_FLAG) as u32;

/// `GRZip_LZP_Decode`. `out_size` is the block's decompressed length, taken
/// from the block header, and bounds every write.
pub fn decode(
    input: &[u8],
    out: &mut [u8],
    min_match_len: u32,
    ht_size: u32,
) -> Result<usize, GrzError> {
    // The header copy below touches the first four bytes of each buffer.
    if input.len() < 4 || out.len() < 4 {
        return Err(GRZ_UNEXPECTED_EOF);
    }
    // `ht_size` is a mask, so the table needs one more entry than its value.
    // The C sizes this by sizeof(uint8*) -- an earlier revision used a smaller
    // element and wrote past the end. Here the entries are output offsets:
    // 0 doubles as "unset" exactly as NULL does there, and no real entry can be
    // 0 because the first four bytes are copied before the loop starts.
    let mut contexts = vec![0usize; ht_size as usize + 1];

    out[..4].copy_from_slice(&input[..4]);
    let mut ctx: u32 = ((input[3] as u32))
        | ((input[2] as u32) << 8)
        | ((input[1] as u32) << 16)
        | ((input[0] as u32) << 24);

    let mut ip = 4usize;
    let mut op = 4usize;

    while ip < input.len() {
        let hash = (((ctx >> 15) ^ ctx ^ (ctx >> 3)) & ht_size) as usize;
        let pointer = contexts[hash];
        contexts[hash] = op;

        if pointer != 0 {
            let b = input[ip];
            ip += 1;
            if b != MATCH_FLAG {
                if op >= out.len() {
                    return Err(GRZ_UNEXPECTED_EOF);
                }
                out[op] = b;
                ctx = (ctx << 8) | b as u32;
                op += 1;
            } else {
                // Run length: each byte contributes `byte ^ XOR_FLAG`, and the
                // run continues while the byte is RUN_FLAG. Bounded against the
                // input end -- unbounded, this walked off a truncated block.
                let mut common: u32 = 0;
                while ip < input.len() {
                    let r = input[ip];
                    common = common.wrapping_add((r as u32) ^ XOR_FLAG);
                    ip += 1;
                    if r != RUN_FLAG {
                        break;
                    }
                }
                if common != 0 {
                    let len = common as usize + min_match_len as usize - 1;
                    // The copy source is an earlier output position, so bounding
                    // the destination bounds the source too.
                    if len > out.len() - op {
                        return Err(GRZ_UNEXPECTED_EOF);
                    }
                    // Byte at a time and overlapping-safe: `pointer` may be
                    // close enough behind `op` that the copy reads bytes it has
                    // just written, which is how a run is expressed.
                    let mut src = pointer;
                    for _ in 0..len {
                        out[op] = out[src];
                        op += 1;
                        src += 1;
                    }
                    if op < 4 {
                        return Err(GRZ_UNEXPECTED_EOF);
                    }
                    ctx = (out[op - 1] as u32)
                        | ((out[op - 2] as u32) << 8)
                        | ((out[op - 3] as u32) << 16)
                        | ((out[op - 4] as u32) << 24);
                } else {
                    // A bare match flag with a zero run is a literal 0xF2.
                    if op >= out.len() {
                        return Err(GRZ_UNEXPECTED_EOF);
                    }
                    out[op] = MATCH_FLAG;
                    ctx = (ctx << 8) | MATCH_FLAG as u32;
                    op += 1;
                }
            }
        } else {
            if op >= out.len() {
                return Err(GRZ_UNEXPECTED_EOF);
            }
            let b = input[ip];
            ip += 1;
            out[op] = b;
            ctx = (ctx << 8) | b as u32;
            op += 1;
        }
    }

    Ok(op)
}

/// `GRZip_LZP_Encode` (LZP.c:65).
///
/// Contexts hold INPUT positions here, where the decoder's hold output ones --
/// both are positions in the same reconstructed byte stream, so the two tables
/// agree step for step. `0` doubles as "unset" exactly as `NULL` does in C, and
/// no real entry can be 0 because the first four bytes are copied before the
/// loop begins.
///
/// # The overread this deliberately does NOT reproduce
///
/// C's early-match check is
///
/// ```c
/// if (*(uint32 *)(Ptr+LZP_MinMatchLen-4) == *(uint32 *)(Pointer+LZP_MinMatchLen-4))
/// ```
///
/// with no bound on `Ptr`. At the default `MinMatchLen` of 32 that reads four
/// bytes at `Ptr+28`, so with `Ptr` up to `InputEnd-1` it runs as far as 31
/// bytes past the end -- confirmed under AddressSanitizer as a
/// `heap-buffer-overflow READ of size 4 at LZP.c:89`. `Pointer` can overrun the
/// same way when it sits within 31 bytes of the end.
///
/// It is a read, it is in the encoder rather than the decoder, and the bytes it
/// reads cannot reach the output -- they only gate whether the match loop runs,
/// and that loop compares `Ptr < InputEnd` against a `Pointer` that trails it,
/// so every emitted literal and every length comes from in-bounds data. What it
/// can do is make the output depend on whatever follows the buffer: on Unix
/// `BigAlloc` is plain `malloc`, so that is uninitialised heap, and the same
/// input can then compress to different bytes.
///
/// So this reads a ZERO-PADDED view instead. That is the behaviour the C has
/// whenever its slack happens to be zero, which is what the stable `grzip`
/// fingerprint suggests it is in practice (multi-megabyte allocations come from
/// fresh `mmap` pages). The fingerprint is the check on that: if the real slack
/// is ever not zero, the archive drifts and the suite says so.
pub fn encode(
    input: &[u8],
    out: &mut [u8],
    min_match_len: u32,
    ht_size: u32,
) -> Result<usize, GrzError> {
    let size = input.len();
    // The caller (GRZip_CompressBlock) never passes fewer than 32 bytes; C
    // would read four regardless.
    if size < 4 || out.len() < size {
        return Err(GRZ_UNEXPECTED_EOF);
    }
    let mml = min_match_len as usize;
    let mut contexts = vec![0usize; ht_size as usize + 1];

    /// Four little-endian bytes at `at`, treating anything OUTSIDE the buffer
    /// as zero -- past the end, and also BEFORE the start.
    ///
    /// `at` is signed because `MinMatchLen` can be less than 4: the archiver
    /// reaches this with mml == 0 (mode words 0x100-0x106), where C's
    /// `Ptr + LZP_MinMatchLen - 4` points four bytes before the block. Guarding
    /// mml < 4 by declining to compress is NOT equivalent -- C runs the filter
    /// anyway, and a whole family of modes then diverges.
    fn word_padded(b: &[u8], at: isize) -> u32 {
        let mut v = 0u32;
        for i in 0..4 {
            let idx = at + i as isize;
            let byte = if idx < 0 { 0 } else { b.get(idx as usize).copied().unwrap_or(0) };
            v |= (byte as u32) << (8 * i);
        }
        v
    }

    out[..4].copy_from_slice(&input[..4]);
    let mut ctx: u32 = (input[3] as u32)
        | ((input[2] as u32) << 8)
        | ((input[1] as u32) << 16)
        | ((input[0] as u32) << 24);

    let mut ip = 4usize;
    let mut op = 4usize;
    // C compares against `OutputEnd = Output + Size - 1`, i.e. it stops once the
    // output has reached one byte short of the input's length.
    let out_end = size - 1;

    while ip < size && op < out_end {
        let hash = (((ctx >> 15) ^ ctx ^ (ctx >> 3)) & ht_size) as usize;
        let pointer = contexts[hash];
        contexts[hash] = ip;

        if pointer != 0 {
            let mut common = 0usize;
            let probe = mml as isize - 4;
            if word_padded(input, ip as isize + probe) == word_padded(input, pointer as isize + probe) {
                let mut p = ip;
                let mut q = pointer;
                while p < size {
                    if input[p] != input[q] {
                        break;
                    }
                    p += 1;
                    q += 1;
                    common += 1;
                }
            }
            // With mml == 0 this comparison is `common < 0`, which is false --
            // so ANY match length survives, including zero, and the length
            // written out is `common + 1`. Preserved rather than special-cased.
            if common < mml {
                common = 0;
            }
            if common != 0 {
                ip += common;
                ctx = (input[ip - 1] as u32)
                    | ((input[ip - 2] as u32) << 8)
                    | ((input[ip - 3] as u32) << 16)
                    | ((input[ip - 4] as u32) << 24);
                let mut len = common - mml + 1;
                out[op] = MATCH_FLAG;
                op += 1;
                while len > 254 {
                    out[op] = RUN_FLAG;
                    op += 1;
                    if op >= out_end {
                        return Err(GRZ_NOT_COMPRESSIBLE);
                    }
                    len -= 255;
                }
                out[op] = (len as u8) ^ (XOR_FLAG as u8);
                op += 1;
            } else {
                let ch = input[ip];
                out[op] = ch;
                op += 1;
                ip += 1;
                ctx = (ctx << 8) | ch as u32;
                // A literal that collides with the match flag is escaped.
                if ch == MATCH_FLAG {
                    out[op] = XOR_FLAG as u8;
                    op += 1;
                }
            }
        } else {
            let ch = input[ip];
            out[op] = ch;
            op += 1;
            ip += 1;
            ctx = (ctx << 8) | ch as u32;
        }
    }

    if op >= out_end {
        return Err(GRZ_NOT_COMPRESSIBLE);
    }
    Ok(op)
}
