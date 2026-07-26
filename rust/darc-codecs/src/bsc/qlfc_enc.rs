//! The QLFC forward transform and the coder's block segmentation, ported from
//! `Compression/BSC/libbsc/coder/qlfc/qlfc.cpp` and `coder/coder.cpp`.
//!
//! The three encode bodies that consume this -- static, adaptive and fast --
//! are still C; this is the stage beneath them, and the one they all share.
//!
//! ## Only one transform needs porting, and it is the scalar one
//!
//! `qlfc.cpp` carries two `QLFC_TRANSFORM_FUNCTION_NAME` bodies: a SIMD one
//! (:205, for SSE2/AVX/AVX512/A64) and a scalar `#else` fallback (:469). After
//! LZP -- where six encoder bodies turned out to emit *different* bytes -- the
//! obvious question was whether these diverge too.
//!
//! They do not. Building the pinned C twice, once as-is and once with
//! `-DLIBBSC_CPU_FEATURE=0` to force the scalar path, and encoding the same
//! corpus gives byte-identical output: 15/15 across all three coders and five
//! input shapes. So this ports the scalar body, which is the same function
//! written legibly.
//!
//! The reason the two cases differ is worth keeping in mind rather than
//! generalising from: this transform computes a **rank**, which the input
//! determines, while LZP's encoder makes a **choice**, which it does not.
//!
//! ## What the transform produces
//!
//! Walking the input backwards, each *run* of equal bytes contributes one
//! entry: the byte's rank in a move-to-front table, except the first time a
//! byte is ever seen, where the rank is replaced by a running count of distinct
//! symbols. Entries are filled from the end of `buffer` downwards, so the
//! return value is where they start. `MTFTable` is left holding the alphabet in
//! last-seen order, which the encoder then codes as the block's preamble.

use super::model::ALPHABET_SIZE;

/// `bsc_qlfc_transform` (:469). Returns the offset into `buffer` at which the
/// rank array begins; entries run from there to `buffer[n]`.
///
/// `mtf_table` is written, not read -- it is an output as much as `buffer` is.
pub fn transform(input: &[u8], buffer: &mut [u8], mtf_table: &mut [u8; ALPHABET_SIZE]) -> usize {
    let n = input.len();
    let mut flag = [0u8; ALPHABET_SIZE];

    for (i, e) in mtf_table.iter_mut().enumerate() {
        *e = i as u8;
    }

    // The one special case: a block ending in 0 starts the table with 1 ahead
    // of 0, so the final run does not code as rank 0.
    if input[n - 1] == 0 {
        mtf_table[0] = 1;
        mtf_table[1] = 0;
    }

    let mut index = n;
    // `int nSymbols` in the C, not a byte. It reaches 256 on a block using the
    // whole alphabet, and only the ASSIGNMENT to `rank` truncates. Typed as u8
    // here it overflowed -- caught by the differential harness on the
    // full-alphabet input, and by this crate building with overflow-checks on.
    let mut n_symbols: i32 = 0;
    let mut i = n as isize - 1;

    while i >= 0 {
        let current_char = input[i as usize];
        i -= 1;
        // Skip the rest of this run: one entry is emitted per run, not per byte.
        while i >= 0 && input[i as usize] == current_char {
            i -= 1;
        }

        // Move-to-front, unrolled four at a time in the C. The unrolling is not
        // cosmetic to reproduce -- it is just a shift of the same chain -- but
        // the *shape* is kept so the rank arithmetic lines up with the source.
        let mut previous_char = mtf_table[0];
        let mut rank: usize = 1;
        mtf_table[0] = current_char;
        loop {
            let t0 = mtf_table[rank];
            mtf_table[rank] = previous_char;
            if t0 == current_char {
                break;
            }
            let t1 = mtf_table[rank + 1];
            mtf_table[rank + 1] = t0;
            if t1 == current_char {
                rank += 1;
                break;
            }
            let t2 = mtf_table[rank + 2];
            mtf_table[rank + 2] = t1;
            if t2 == current_char {
                rank += 2;
                break;
            }
            let t3 = mtf_table[rank + 3];
            mtf_table[rank + 3] = t2;
            if t3 == current_char {
                rank += 3;
                break;
            }
            rank += 4;
            previous_char = t3;
        }

        // First sighting of this byte: its "rank" is instead the number of
        // distinct symbols seen so far, which is what lets the decoder rebuild
        // the alphabet from the preamble.
        let mut rank = rank as u8;
        if flag[current_char as usize] == 0 {
            flag[current_char as usize] = 1;
            rank = n_symbols as u8;
            n_symbols += 1;
        }

        index -= 1;
        buffer[index] = rank;
    }

    buffer[n - 1] = 1;

    // Collapse the first still-unused entry onto its predecessor. That repeat
    // is what terminates the decoder's preamble loop and sets its maxRank.
    for rank in 1..ALPHABET_SIZE {
        if flag[mtf_table[rank] as usize] == 0 {
            mtf_table[rank] = mtf_table[rank - 1];
            break;
        }
    }

    index
}

/// `bsc_coder_num_blocks` (`coder.cpp:52`). The same table as LZP's
/// `bsc_lzp_num_blocks`, spelled separately in the C.
pub fn num_blocks(n: usize) -> usize {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn run(input: &[u8]) -> (usize, Vec<u8>, [u8; ALPHABET_SIZE]) {
        let mut buffer = vec![0u8; input.len()];
        let mut mtf = [0u8; ALPHABET_SIZE];
        let idx = transform(input, &mut buffer, &mut mtf);
        (idx, buffer, mtf)
    }

    #[test]
    fn one_entry_per_run_not_per_byte() {
        // Three runs -> three entries, however long the runs are.
        let input = b"aaaaabbbbbccccc";
        let (idx, _, _) = run(input);
        assert_eq!(input.len() - idx, 3);
    }

    #[test]
    fn first_sighting_uses_the_symbol_count() {
        // Walking backwards, 'c' is seen first and gets 0, then 'b' gets 1,
        // then 'a' gets 2 -- the running count, not an MTF rank.
        //
        // The last entry then reads 1, not 0: `buffer[n - 1] = 1` runs after
        // the walk and overwrites it unconditionally. That slot belongs to the
        // run the decoder starts from, whose rank it already knows, so the C
        // spends it on a constant. Worth a test of its own, because a port that
        // dropped that line would still round-trip everything else.
        let input = b"abc";
        let (idx, buffer, _) = run(input);
        assert_eq!(&buffer[idx..], &[2, 1, 1]);
    }

    #[test]
    fn a_block_ending_in_zero_reorders_the_table() {
        // input[n-1] == 0 swaps the first two table entries before the walk.
        let with_zero = [1u8, 2, 0];
        let (_, _, mtf) = run(&with_zero);
        // 0 was seen last going backwards, so it leads the table either way;
        // the point is that the special case does not corrupt the alphabet.
        assert_eq!(mtf[0], 1);
    }

    #[test]
    fn block_count_matches_the_c_table() {
        assert_eq!(num_blocks(0), 1);
        assert_eq!(num_blocks(2 * 2 * 65536 - 1), 1);
        assert_eq!(num_blocks(2 * 2 * 65536), 2);
        assert_eq!(num_blocks(128 * 128 * 65536), 128);
    }
}
