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

use super::{GrzError, GRZ_UNEXPECTED_EOF};

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
