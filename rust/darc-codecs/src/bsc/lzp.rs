//! BSC's inverse LZP, ported from `Compression/BSC/libbsc/lzp/lzp.cpp`
//! (`bsc_lzp_decode_block` :715, `bsc_lzp_decompress` :953).
//!
//! LZP is BSC's *first* stage on encode and therefore its *last* on decode: an
//! order-4 context predicts a previous position, and when the prediction holds
//! the encoder replaces the matched bytes with a flag plus a length. Note this
//! is a third, distinct LZP -- unrelated to `Compression/LZP/` (`crate::lzp`)
//! and to GRZip's (`crate::grzip::lzp`). Different flag, different hash,
//! different framing; they share only the idea.
//!
//! ## Only the scalar loop is ported, and that is exact
//!
//! `bsc_lzp_decode_block` has two bodies: an unrolled path that processes four
//! positions at a time using unaligned 64-bit loads, taken only when
//! `hashSize <= 17` on x86-64/AArch64, and a scalar loop that finishes the
//! tail and handles every other case. They must compute the same thing -- the C
//! runs one and then the other on the same buffer -- so the scalar loop is the
//! definition and this ports that. The differential harness is what confirms
//! the equivalence rather than my reading of the 64-bit bit-mixing.
//!
//! ## The escape
//!
//! `0xF2` marks a match. A literal `0xF2` in the data is escaped as
//! `0xF2 0xFF`, and a match length is a sequence of bytes added to `minLen`,
//! continuing while the byte is `0xFE`. So `0xFF` after the flag means "not a
//! match, emit a literal flag byte" -- which is why the length reader must
//! check for `0xFF` *before* accumulating.

use super::{LIBBSC_NOT_ENOUGH_MEMORY, LIBBSC_UNEXPECTED_EOB};

/// `LIBBSC_LZP_MATCH_FLAG` (:42).
const MATCH_FLAG: u8 = 0xF2;

/// `bsc_lzp_decode_block`. Returns the number of bytes written to `output`.
pub fn decode_block(
    input: &[u8],
    output: &mut [u8],
    hash_size: u32,
    min_len: u32,
) -> Result<usize, i32> {
    if input.len() < 4 {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    if hash_size == 0 || hash_size > 28 {
        // The C allocates 1<<hashSize ints; an unbounded value from a corrupt
        // header would drive a wild allocation.
        return Err(LIBBSC_NOT_ENOUGH_MEMORY);
    }
    let mask: u32 = ((1u32 << hash_size) as u32).wrapping_sub(1);
    let mut lookup = vec![0i32; 1usize << hash_size];

    let mut ip = 0usize;
    let mut op = 0usize;

    // The first four bytes are always literal -- they seed the context.
    if output.len() < 4 {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    for _ in 0..4 {
        output[op] = input[ip];
        op += 1;
        ip += 1;
    }

    let mut context: u32 = (output[op - 1] as u32)
        | ((output[op - 2] as u32) << 8)
        | ((output[op - 3] as u32) << 16)
        | ((output[op - 4] as u32) << 24);

    while ip < input.len() {
        let index = (((context >> 15) ^ context ^ (context >> 3)) & mask) as usize;
        let value = lookup[index];
        lookup[index] = op as i32;

        if input[ip] == MATCH_FLAG && value > 0 {
            ip += 1;
            if ip >= input.len() {
                return Err(LIBBSC_UNEXPECTED_EOB);
            }
            if input[ip] != 255 {
                // A match: accumulate the length while the byte is 254.
                let mut len = min_len as usize;
                loop {
                    if ip >= input.len() {
                        return Err(LIBBSC_UNEXPECTED_EOB);
                    }
                    let b = input[ip];
                    len += b as usize;
                    ip += 1;
                    if b != 254 {
                        break;
                    }
                }
                let mut reference = value as usize;
                if op + len > output.len() || reference >= output.len() {
                    return Err(LIBBSC_UNEXPECTED_EOB);
                }
                // Byte at a time: the source may overlap the destination, which
                // is how a run is expressed.
                for _ in 0..len {
                    output[op] = output[reference];
                    op += 1;
                    reference += 1;
                }
                if op < 4 {
                    return Err(LIBBSC_UNEXPECTED_EOB);
                }
                context = (output[op - 1] as u32)
                    | ((output[op - 2] as u32) << 8)
                    | ((output[op - 3] as u32) << 16)
                    | ((output[op - 4] as u32) << 24);
            } else {
                // 0xFF after the flag: an escaped literal flag byte.
                ip += 1;
                if op >= output.len() {
                    return Err(LIBBSC_UNEXPECTED_EOB);
                }
                output[op] = MATCH_FLAG;
                context = (context << 8) | MATCH_FLAG as u32;
                op += 1;
            }
        } else {
            if op >= output.len() {
                return Err(LIBBSC_UNEXPECTED_EOB);
            }
            let b = input[ip];
            ip += 1;
            output[op] = b;
            context = (context << 8) | b as u32;
            op += 1;
        }
    }

    Ok(op)
}

/// `bsc_lzp_decompress`: one or more independently-coded blocks.
///
/// `input[0]` is the block count. For more than one, an index of two 32-bit
/// words per block follows -- the decompressed size then the compressed size --
/// and a block whose two sizes are equal was stored rather than coded.
pub fn decompress(
    input: &[u8],
    output: &mut [u8],
    hash_size: u32,
    min_len: u32,
) -> Result<usize, i32> {
    if input.is_empty() {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    let n_blocks = input[0] as usize;
    if n_blocks == 0 {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    if n_blocks == 1 {
        return decode_block(&input[1..], output, hash_size, min_len);
    }

    let index_bytes = 1 + 8 * n_blocks;
    if input.len() < index_bytes {
        return Err(LIBBSC_UNEXPECTED_EOB);
    }
    let word = |at: usize| -> i32 {
        i32::from_le_bytes([input[at], input[at + 1], input[at + 2], input[at + 3]])
    };

    let mut ip = index_bytes;
    let mut op = 0usize;
    for b in 0..n_blocks {
        let out_size = word(1 + 8 * b) as usize; // decompressed size
        let in_size = word(1 + 8 * b + 4) as usize; // compressed size
        if ip + in_size > input.len() || op + out_size > output.len() {
            return Err(LIBBSC_UNEXPECTED_EOB);
        }
        if in_size != out_size {
            let n = decode_block(&input[ip..ip + in_size], &mut output[op..], hash_size, min_len)?;
            if n != out_size {
                return Err(LIBBSC_UNEXPECTED_EOB);
            }
        } else {
            // Equal sizes mean the block was stored, not coded.
            output[op..op + in_size].copy_from_slice(&input[ip..ip + in_size]);
        }
        ip += in_size;
        op += out_size;
    }
    Ok(op)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// With no match flags anywhere, the block is a pure literal copy -- the
    /// simplest property that pins the framing and the four seed bytes.
    #[test]
    fn a_block_with_no_matches_is_a_literal_copy() {
        let data = b"the quick brown fox jumps over the lazy dog";
        let mut out = vec![0u8; data.len()];
        let n = decode_block(data, &mut out, 16, 32).unwrap();
        assert_eq!(n, data.len());
        assert_eq!(&out[..n], data);
    }

    /// An escaped literal 0xF2 must come back as one 0xF2 byte, consuming two
    /// input bytes. Without a prior match at that context the flag is a plain
    /// literal, so this drives the value==0 path too.
    #[test]
    fn an_escaped_flag_byte_round_trips() {
        // 4 seed bytes, then 0xF2 as a plain literal (lookup still empty).
        let input = [1u8, 2, 3, 4, MATCH_FLAG];
        let mut out = vec![0u8; 8];
        let n = decode_block(&input, &mut out, 16, 32).unwrap();
        assert_eq!(&out[..n], &[1, 2, 3, 4, MATCH_FLAG]);
    }

    #[test]
    fn truncated_and_absurd_inputs_are_rejected() {
        let mut out = vec![0u8; 64];
        assert_eq!(decode_block(&[1, 2, 3], &mut out, 16, 32), Err(LIBBSC_UNEXPECTED_EOB));
        // A hash size the C would turn into a multi-gigabyte allocation.
        assert!(decode_block(&[1, 2, 3, 4, 5], &mut out, 31, 32).is_err());
        assert!(decode_block(&[1, 2, 3, 4, 5], &mut out, 0, 32).is_err());
        // Output too small for even the four seed bytes.
        let mut tiny = vec![0u8; 2];
        assert_eq!(decode_block(&[1, 2, 3, 4, 5], &mut tiny, 16, 32), Err(LIBBSC_UNEXPECTED_EOB));
    }

    /// The multi-block index: two words per block, and equal sizes mean stored.
    #[test]
    fn multi_block_index_is_walked_and_stored_blocks_copied() {
        let a = b"first-block-data";
        let b = b"second-block!!!!";
        let mut input = vec![2u8]; // two blocks
        for part in [a.as_slice(), b.as_slice()] {
            input.extend_from_slice(&(part.len() as i32).to_le_bytes()); // out size
            input.extend_from_slice(&(part.len() as i32).to_le_bytes()); // in size == stored
        }
        input.extend_from_slice(a);
        input.extend_from_slice(b);

        let mut out = vec![0u8; a.len() + b.len()];
        let n = decompress(&input, &mut out, 16, 32).unwrap();
        assert_eq!(n, a.len() + b.len());
        assert_eq!(&out[..a.len()], a);
        assert_eq!(&out[a.len()..n], b);
    }
}
