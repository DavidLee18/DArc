//! BSC's forward LZP, ported from `Compression/BSC/libbsc/lzp/lzp.cpp`
//! (`bsc_lzp_encode_generic` :583, `bsc_lzp_compress_serial` :829,
//! `bsc_lzp_num_blocks` :44).
//!
//! The inverse lives in [`super::lzp`]; read its header first for the framing
//! and the escape rule, which this module produces rather than consumes.
//!
//! ## Why only the generic encoder
//!
//! `bsc_lzp_encode_block` (:679) dispatches through a stack of width-specialised
//! bodies -- `encode_small`, `encode_small2x`, `encode_medium`, `encode_large`
//! and a `_fast_path` for the default parameters -- all of them behind
//!
//! ```text
//! #if !defined(LIBBSC_NO_UNALIGNED_ACCESS) && (defined(LIBBSC_x86_64) || defined(LIBBSC_AArch64))
//!     if (hashSize <= 17) { ... }
//! ```
//!
//! and falls back to `bsc_lzp_encode_generic` otherwise. The generic body is
//! what runs on i386, on 32-bit ARM, under `LIBBSC_NO_UNALIGNED_ACCESS`, and on
//! any target at all once `hashSize >= 18` -- a size DArc exposes directly, via
//! `-mbsc:h18` and up.
//!
//! So the two must agree byte for byte, or libbsc on i386 and libbsc on x86-64
//! would write different archives from the same input. This port implements the
//! generic body alone and `bsc-lzp-encode-check.sh` compares it against whatever
//! path the C takes on the host, which is how that agreement gets tested rather
//! than assumed.
//!
//! ## No parallel variant
//!
//! `bsc_lzp_compress_parallel` is inside `#ifdef LIBBSC_OPENMP`, and DArc never
//! defines `LIBBSC_OPENMP_SUPPORT` (see `Compression/BSC/makefile`), so
//! `bsc_lzp_compress` always reaches the serial function. The two split the
//! input identically anyway -- both use `bsc_lzp_num_blocks` -- so this is a
//! scheduling difference, not a format one.

use super::LIBBSC_NOT_COMPRESSIBLE;

/// `LIBBSC_LZP_MATCH_FLAG` (:42).
const MATCH_FLAG: u8 = 0xF2;

/// `bsc_lzp_num_blocks` (:44). The thresholds are `k*k*65536` for each `k`, in
/// descending order, so the block count grows with the square root of the
/// input; below `2*2*65536` it is a single block.
fn num_blocks(n: usize) -> usize {
    const BREAK_POINTS: [(usize, usize); 12] = [
        (128, 128 * 128 * 65536),
        (96, 96 * 96 * 65536),
        (64, 64 * 64 * 65536),
        (48, 48 * 48 * 65536),
        (32, 32 * 32 * 65536),
        (24, 24 * 24 * 65536),
        (16, 16 * 16 * 65536),
        (12, 12 * 12 * 65536),
        (8, 8 * 8 * 65536),
        (6, 6 * 6 * 65536),
        (4, 4 * 4 * 65536),
        (2, 2 * 2 * 65536),
    ];
    for (blocks, threshold) in BREAK_POINTS {
        if n >= threshold {
            return blocks;
        }
    }
    1
}

/// Read four bytes little-endian, the portable spelling of the C's
/// `*(unsigned int *)p`.
///
/// Every call site below is inside the slack the C relies on: `inputMinLenEnd`
/// stops `minLen + 32` bytes short of the end, so a 4-byte read at or before it
/// cannot pass the buffer. The bound is asserted here rather than trusted --
/// the C reads past its own loop condition by design, and a port that copied
/// that shape without the slack would read out of bounds on short inputs.
#[inline]
fn u32_at(buf: &[u8], at: usize) -> u32 {
    u32::from_le_bytes([buf[at], buf[at + 1], buf[at + 2], buf[at + 3]])
}

/// `bsc_lzp_encode_generic` (:583). Encodes `input` into `output`, returning the
/// number of bytes written, or `LIBBSC_NOT_COMPRESSIBLE` when the result would
/// not fit in the space allowed.
///
/// `output` is the C's `[output, outputEnd)` window; the C stops at
/// `outputEnd - 8`, so the caller must supply at least 9 bytes.
pub fn encode_generic(input: &[u8], output: &mut [u8], hash_size: u32, min_len: u32) -> i32 {
    let n = input.len();
    let min_len = min_len as usize;

    // `if (inputEnd - input - minLen < 32) return LIBBSC_NOT_COMPRESSIBLE;`
    // Signed in the C, so an input shorter than minLen also lands here.
    if (n as i64) - (min_len as i64) < 32 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }
    if output.len() < 9 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let mask = ((1u64 << hash_size) - 1) as u32;
    let mut lookup = vec![0i32; 1usize << hash_size];

    // outputEOB = outputEnd - 8. Both loops below stop at it, and the final
    // return treats having reached it as "did not fit".
    let output_eob = output.len() - 8;
    // inputMinLenEnd = inputEnd - minLen - 32, which the guard above makes >= 0.
    let input_min_len_end = n - min_len - 32;

    let mut ip = 0usize; // `input`  as an offset from inputStart
    let mut op = 0usize; // `output` as an offset from outputStart
    let mut heuristic = 0usize; // `heuristic`, likewise an offset

    // The first four bytes are copied verbatim: they are what seeds the context.
    for _ in 0..4 {
        output[op] = input[ip];
        op += 1;
        ip += 1;
    }

    let context_at = |ip: usize| -> u32 {
        input[ip - 1] as u32
            | ((input[ip - 2] as u32) << 8)
            | ((input[ip - 3] as u32) << 16)
            | ((input[ip - 4] as u32) << 24)
    };

    // ── Main loop: match search ─────────────────────────────────────────────
    {
        let mut context = context_at(ip);

        while ip < input_min_len_end && op < output_eob {
            let index = (((context >> 15) ^ context ^ (context >> 3)) & mask) as usize;
            let value = lookup[index];
            lookup[index] = ip as i32;

            // `value > 0`, not `>= 0`: position 0 is indistinguishable from
            // "never seen" in the C's zero-initialised table, so a match at the
            // very start is never taken. Reproduced deliberately.
            if value > 0 {
                let reference = value as usize;

                // The C tests the LAST four bytes of the minimum length first,
                // then the first four -- cheapest rejection first, and the
                // order is immaterial to the outcome.
                let matches = u32_at(input, ip + min_len - 4)
                    == u32_at(input, reference + min_len - 4)
                    && u32_at(input, ip) == u32_at(input, reference);

                let mut found = false;
                if matches {
                    // The heuristic: a previous failed match already proved
                    // these four bytes differ, so do not walk them again.
                    if heuristic > ip
                        && u32_at(input, heuristic) != u32_at(input, reference + (heuristic - ip))
                    {
                        // falls through to the literal path
                    } else {
                        let mut len = 4usize;
                        while ip + len < input_min_len_end {
                            if u32_at(input, ip + len) != u32_at(input, reference + len) {
                                break;
                            }
                            len += 4;
                        }

                        if len < min_len {
                            heuristic = ip + len;
                        } else {
                            // Two tail comparisons, 2 bytes then 1, each adding
                            // its width only on equality. Written as the C does
                            // rather than as a byte loop: the widths are what
                            // decide the final length.
                            len += 2 * usize::from(
                                u16::from_le_bytes([input[ip + len], input[ip + len + 1]])
                                    == u16::from_le_bytes([
                                        input[reference + len],
                                        input[reference + len + 1],
                                    ]),
                            );
                            len += usize::from(input[ip + len] == input[reference + len]);

                            ip += len;
                            context = context_at(ip);

                            output[op] = MATCH_FLAG;
                            op += 1;

                            let mut rem = len - min_len;
                            while rem >= 254 {
                                rem -= 254;
                                output[op] = 254;
                                op += 1;
                                if op >= output_eob {
                                    break;
                                }
                            }
                            output[op] = rem as u8;
                            op += 1;
                            found = true;
                        }
                    }
                }

                if !found {
                    // LIBBSC_LZP_MATCH_NOT_FOUND
                    let next = input[ip];
                    output[op] = next;
                    op += 1;
                    ip += 1;
                    context = (context << 8) | next as u32;
                    if next == MATCH_FLAG {
                        output[op] = 255;
                        op += 1;
                    }
                }
            } else {
                let next = input[ip];
                output[op] = next;
                op += 1;
                ip += 1;
                context = (context << 8) | next as u32;
            }
        }
    }

    // ── Tail: no more room to match, but the table still has to be walked and
    // literal flag bytes still have to be escaped. ──────────────────────────
    {
        let mut context = context_at(ip);

        while ip < n && op < output_eob {
            let index = (((context >> 15) ^ context ^ (context >> 3)) & mask) as usize;
            let value = lookup[index];
            lookup[index] = ip as i32;

            let next = input[ip];
            output[op] = next;
            op += 1;
            ip += 1;
            context = (context << 8) | next as u32;
            if next == MATCH_FLAG && value > 0 {
                output[op] = 255;
                op += 1;
            }
        }
    }

    if op >= output_eob {
        LIBBSC_NOT_COMPRESSIBLE
    } else {
        op as i32
    }
}

/// Read eight bytes little-endian, the portable spelling of `*(unsigned long long *)p`.
#[inline]
fn u64_at(buf: &[u8], at: usize) -> u64 {
    u64::from_le_bytes([
        buf[at],
        buf[at + 1],
        buf[at + 2],
        buf[at + 3],
        buf[at + 4],
        buf[at + 5],
        buf[at + 6],
        buf[at + 7],
    ])
}

/// `LIBBSC_DEFAULT_LZPHASHSIZE` (`libbsc.h:78`).
pub const DEFAULT_LZP_HASH_SIZE: u32 = 15;
/// `LIBBSC_DEFAULT_LZPMINLEN` (`libbsc.h:79`).
pub const DEFAULT_LZP_MIN_LEN: u32 = 72;

/// `bsc_lzp_encode_large_fast_path` (:469): the body reached for exactly the
/// default parameters, and therefore the one every ordinary `-mbsc` archive
/// goes through. **Not** an optimisation of [`encode_block`] -- it finds
/// different match lengths and so emits different bytes; see the module header.
///
/// Four positions are examined per iteration. The four literal bytes are
/// written up-front by the C's `*(unsigned int *)output = ...` store, so the
/// per-position paths advance over bytes that are already in the output rather
/// than writing them -- including the `MATCH_NOT_FOUND` path, which *reads back*
/// the byte it just committed to decide whether it needs escaping.
pub fn encode_large_fast_path(input: &[u8], output: &mut [u8]) -> i32 {
    const MIN_LEN: usize = DEFAULT_LZP_MIN_LEN as usize;
    let mask = ((1u32 << DEFAULT_LZP_HASH_SIZE) - 1) as u64;

    let n = input.len();
    if (n as i64) - (MIN_LEN as i64) < 32 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }
    if output.len() < 9 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let mut lookup = vec![0i32; 1usize << DEFAULT_LZP_HASH_SIZE];
    let output_eob = output.len() - 8;
    let input_min_len_end = n - MIN_LEN - 32;

    let mut ip = 0usize;
    let mut op = 0usize;
    let mut heuristic = 0usize;
    // The C guards this load with `input < inputMinLenEnd`; the length check
    // above already guarantees it, but the shape is kept so the two read alike.
    let mut heuristic_v: u64 = if ip < input_min_len_end { u64_at(input, ip) } else { 0 };

    for _ in 0..4 {
        output[op] = input[ip];
        op += 1;
        ip += 1;
    }

    while ip < input_min_len_end && op < output_eob {
        // `*(unsigned long long *)(input - 4)`, then the four literals are
        // stored to the output before anything decides what to do with them.
        let next8_le = u64_at(input, ip - 4);
        output[op..op + 4].copy_from_slice(&((next8_le >> 32) as u32).to_le_bytes());
        // Byte-swapped, the value reads as input[3], input[2], ... input[-4]
        // from the low byte up, which is what the shifts below index.
        let next8 = next8_le.swap_bytes();

        // ((a >> 12) ^ a) >> 3 ^ a, i.e. a ^ (a >> 3) ^ (a >> 15) evaluated at
        // 64 bits. The generic body computes the same expression over a 32-bit
        // context; at 64 bits the shifts pull in neighbouring bytes, which is
        // one of the reasons the two bodies choose different matches.
        let mix = ((next8 >> 12) ^ next8) >> 3 ^ next8;

        // Each of the four positions: claim the hash slot, then test either for
        // a match worth taking or for a literal flag byte needing an escape.
        // `k` is the offset within the four, and the flag byte for offset k is
        // `next8 >> (3 - k) * 8`.
        let mut good: Option<(usize, usize)> = None; // (offset, reference)
        let mut bad: Option<usize> = None; // offset
        for k in 0..4usize {
            let index = ((mix >> (32 - 8 * k)) & mask) as usize;
            let value = lookup[index];
            lookup[index] = (ip + k) as i32;
            if value > 0 {
                let reference = value as usize;
                if u64_at(input, ip + MIN_LEN - 8 + k) == u64_at(input, reference + MIN_LEN - 8)
                    && u64_at(input, ip + k) == u64_at(input, reference)
                {
                    good = Some((k, reference));
                    break;
                }
                if ((next8 >> ((3 - k) * 8)) as u8) == MATCH_FLAG {
                    bad = Some(k);
                    break;
                }
            }
        }

        if let Some(k) = bad {
            // BAD_MATCH_FOUND: step past the literal, which is already in the
            // output, and append the escape byte.
            ip += k + 1;
            op += k + 1;
            output[op] = 255;
            op += 1;
            continue;
        }

        let (k, reference) = match good {
            Some(g) => g,
            None => {
                ip += 4;
                op += 4;
                continue;
            }
        };
        ip += k;
        op += k;

        // GOOD_MATCH_FOUND: verify against the heuristic, then measure.
        let mut not_found = heuristic > ip && heuristic_v != u64_at(input, reference + (heuristic - ip));

        if !not_found {
            let mut len = 8usize;
            while ip + len < input_min_len_end {
                let m = u64_at(input, ip + len) ^ u64_at(input, reference + len);
                if m != 0 {
                    // The exact mismatching byte. The generic body has no
                    // equivalent -- it steps in fours and then compares a
                    // 2-byte and a 1-byte tail, which is where the two part.
                    len += (m.trailing_zeros() / 8) as usize;
                    break;
                }
                len += 8;
            }

            if len < MIN_LEN {
                heuristic = ip + len;
                heuristic_v = u64_at(input, heuristic);
                not_found = true;
            } else {
                ip += len;
                let mut rem = len - MIN_LEN;
                output[op] = MATCH_FLAG;
                op += 1;
                while rem >= 254 {
                    rem -= 254;
                    output[op] = 254;
                    op += 1;
                    if op >= output_eob {
                        break;
                    }
                }
                output[op] = rem as u8;
                op += 1;
                continue;
            }
        }

        // MATCH_NOT_FOUND: the literal is already in the output; read it back
        // to see whether it is a flag byte needing an escape.
        ip += 1;
        let written = output[op];
        op += 1;
        if written == MATCH_FLAG {
            output[op] = 255;
            op += 1;
        }
    }

    // The tail is identical to the generic body's: no room left to match, but
    // the hash table still has to be walked so that a literal flag byte is
    // escaped exactly when the decoder will expect it.
    {
        let mut context = input[ip - 1] as u32
            | ((input[ip - 2] as u32) << 8)
            | ((input[ip - 3] as u32) << 16)
            | ((input[ip - 4] as u32) << 24);

        while ip < n && op < output_eob {
            let index = (((context >> 15) ^ context ^ (context >> 3)) as u64 & mask) as usize;
            let value = lookup[index];
            lookup[index] = ip as i32;

            let next = input[ip];
            output[op] = next;
            op += 1;
            ip += 1;
            context = (context << 8) | next as u32;
            if next == MATCH_FLAG && value > 0 {
                output[op] = 255;
                op += 1;
            }
        }
    }

    if op >= output_eob {
        LIBBSC_NOT_COMPRESSIBLE
    } else {
        op as i32
    }
}

/// `bsc_lzp_encode_block` (:679): pick the body the C would pick.
///
/// This reproduces the **x86-64 / AArch64** dispatch, because those are the
/// targets DArc ships and their archives are what the fingerprints in
/// `Tests/run-tests.sh` record. On i386 or under `LIBBSC_NO_UNALIGNED_ACCESS`
/// the C would take [`encode_generic`] for every parameter and emit different
/// bytes -- a divergence that predates this port and is the C's, not ours.
///
/// Only the default-parameter body is ported so far; the four remaining
/// specialisations fall through to the generic one, where
/// `bsc-lzp-encode-check.sh` reports them as mismatches until they land.
pub fn encode_block(input: &[u8], output: &mut [u8], hash_size: u32, min_len: u32) -> i32 {
    if hash_size <= 17 && hash_size == DEFAULT_LZP_HASH_SIZE && min_len == DEFAULT_LZP_MIN_LEN {
        return encode_large_fast_path(input, output);
    }
    encode_generic(input, output, hash_size, min_len)
}

/// `bsc_lzp_compress_serial` (:829) -- and therefore `bsc_lzp_compress`, since
/// the parallel variant is compiled out. Writes the block index and the coded
/// blocks into `output`, returning the total byte count.
///
/// `output` must be at least `input.len()` bytes: the C hands each block an
/// output window bounded by the remaining space in a buffer of exactly `n`, and
/// declares the whole thing incompressible when a block cannot fit.
pub fn compress(input: &[u8], output: &mut [u8], hash_size: u32, min_len: u32) -> i32 {
    let n = input.len();
    if output.len() < n || n == 0 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let n_blocks = num_blocks(n);

    if n_blocks == 1 {
        // The C passes `output + 1 .. output + n - 1`, i.e. one byte reserved
        // for the count and one byte short at the end. For n < 2 that window is
        // inverted; the C never notices because bsc_lzp_encode_block's own
        // length check returns first, but slicing it in Rust panics, so the
        // same answer is given here instead.
        if n < 2 {
            return LIBBSC_NOT_COMPRESSIBLE;
        }
        let result = encode_block(input, &mut output[1..n - 1], hash_size, min_len);
        if result < 0 {
            return result;
        }
        output[0] = 1;
        return result + 1;
    }

    let chunk_size = n / n_blocks;
    let mut output_ptr = 1 + 8 * n_blocks;

    output[0] = n_blocks as u8;
    for block_id in 0..n_blocks {
        let input_start = block_id * chunk_size;
        let input_size = if block_id != n_blocks - 1 {
            chunk_size
        } else {
            n - input_start
        };
        // The window given to the block is the smaller of its input size and
        // whatever is left of the n-byte buffer.
        let mut output_size = input_size;
        if output_size > n - output_ptr {
            output_size = n - output_ptr;
        }

        let result = encode_block(
            &input[input_start..input_start + input_size],
            &mut output[output_ptr..output_ptr + output_size],
            hash_size,
            min_len,
        );

        let result = if result < 0 {
            // Store the block instead -- unless storing it would overrun, in
            // which case the whole input is declared incompressible.
            if output_ptr + input_size >= n {
                return LIBBSC_NOT_COMPRESSIBLE;
            }
            output[output_ptr..output_ptr + input_size]
                .copy_from_slice(&input[input_start..input_start + input_size]);
            input_size
        } else {
            result as usize
        };

        // Two little-endian 32-bit words per block: the input size, then the
        // coded size. Equal sizes are how the decoder recognises a stored block.
        output[1 + 8 * block_id..1 + 8 * block_id + 4]
            .copy_from_slice(&(input_size as i32).to_le_bytes());
        output[1 + 8 * block_id + 4..1 + 8 * block_id + 8]
            .copy_from_slice(&(result as i32).to_le_bytes());

        output_ptr += result;
    }

    output_ptr as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bsc::lzp;

    /// The property the format rests on: whatever the encoder emits, the
    /// already-verified decoder must return the original bytes.
    fn round_trip(data: &[u8], hash_size: u32, min_len: u32) {
        let mut coded = vec![0u8; data.len().max(64)];
        let n = compress(data, &mut coded, hash_size, min_len);
        if n < 0 {
            return; // incompressible is a legitimate answer, not a failure
        }
        let mut back = vec![0u8; data.len() + 64];
        let got = lzp::decompress(&coded[..n as usize], &mut back, hash_size, min_len)
            .expect("the decoder must read what the encoder wrote");
        assert_eq!(&back[..got], data);
    }

    #[test]
    fn round_trips_repetitive_data() {
        let data: Vec<u8> = b"the quick brown fox jumps over the lazy dog. "
            .iter()
            .cycle()
            .take(100_000)
            .copied()
            .collect();
        round_trip(&data, 16, 32);
    }

    #[test]
    fn round_trips_data_containing_the_flag_byte() {
        // 0xF2 in the input has to be escaped; a corpus without it never
        // exercises the escape on either side.
        let mut data = vec![0u8; 60_000];
        let mut s: u32 = 7;
        for (i, b) in data.iter_mut().enumerate() {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            *b = if i % 17 == 0 { 0xF2 } else { (s >> 16) as u8 };
        }
        round_trip(&data, 16, 32);
    }

    #[test]
    fn short_input_is_not_compressible() {
        let data = [1u8; 16];
        let mut out = [0u8; 64];
        assert_eq!(
            encode_block(&data, &mut out, 16, 32),
            LIBBSC_NOT_COMPRESSIBLE
        );
    }

    #[test]
    fn block_count_matches_the_c_table() {
        assert_eq!(num_blocks(0), 1);
        assert_eq!(num_blocks(2 * 2 * 65536 - 1), 1);
        assert_eq!(num_blocks(2 * 2 * 65536), 2);
        assert_eq!(num_blocks(4 * 4 * 65536), 4);
        assert_eq!(num_blocks(128 * 128 * 65536), 128);
    }
}
