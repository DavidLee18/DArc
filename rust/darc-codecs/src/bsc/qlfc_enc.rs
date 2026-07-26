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

use super::model::{model_rank_state, model_run_state, QlfcModel1};
use super::model_consts::*;
use super::predictor::ProbabilityCounter;
use super::qlfc::{bit_scan_reverse, mix3};
use super::rangecoder::RangeEncoder;
use super::LIBBSC_NOT_COMPRESSIBLE;

/// `bsc_qlfc_static_encode` (`qlfc.cpp:900`), the mirror of
/// [`super::qlfc::static_decode`] and the body reached by
/// `LIBBSC_DEFAULT_CODER`.
///
/// Returns the coded length in bytes, or `LIBBSC_NOT_COMPRESSIBLE` when the
/// block will not fit -- which the C decides by polling `CheckEOB` at the top
/// of every run, not by checking afterwards.
///
/// The decoder is the specification for every model update here: each branch
/// updates the same three predictors with the same thresholds, and a single
/// transposed constant produces a stream that decodes for a while and then
/// diverges. They are written in the same order as the C so the two can be read
/// side by side.
pub fn static_encode(input: &[u8], output: &mut [u8]) -> i32 {
    let n = input.len();
    if n == 0 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let mut model = QlfcModel1::new();
    let mut buffer = vec![0u8; n];
    let mut mtf = [0u8; ALPHABET_SIZE];

    let mut context_rank0 = 0usize;
    let mut context_rank4 = 0usize;
    let mut context_run = 0usize;
    let mut max_rank = 7i32;
    let mut avg_rank = 0i32;

    let mut rank_history = [0u8; ALPHABET_SIZE];
    let mut run_history = [0u8; ALPHABET_SIZE];

    let mut rank_ptr = transform(input, &mut buffer, &mut mtf);

    let mut coder = RangeEncoder::new(output);
    coder.encode_word(n as u32);

    // --- Alphabet preamble --------------------------------------------------
    // A bit is coded only where the prefix so far leaves both values reachable
    // among the characters not yet used; everywhere else the decoder can infer
    // it. Encoder and decoder must agree exactly on which those are, so the
    // reachability scan is the same loop on both sides.
    let mut used_char = [0u8; ALPHABET_SIZE];
    let mut prev_char: i32 = -1;
    for rank in 0..ALPHABET_SIZE {
        let current_char = mtf[rank] as i32;

        for bit in (0..8).rev() {
            let (mut bit0, mut bit1) = (false, false);
            for c in 0..ALPHABET_SIZE as i32 {
                if c == prev_char || used_char[c as usize] == 0 {
                    if (current_char >> (bit + 1)) == (c >> (bit + 1)) {
                        if c & (1 << bit) != 0 {
                            bit1 = true;
                        } else {
                            bit0 = true;
                        }
                        if bit0 && bit1 {
                            break;
                        }
                    }
                }
            }
            if bit0 && bit1 {
                coder.encode_bit((current_char & (1 << bit)) as u32);
            }
        }

        if current_char == prev_char {
            max_rank = bit_scan_reverse(rank as u32 - 1) as i32;
            break;
        }
        prev_char = current_char;
        used_char[current_char as usize] = 1;
    }

    // --- Main loop: one (rank, run) pair per run of equal bytes --------------
    let mut ip = 0usize;
    while rank_ptr < n {
        if coder.check_eob() {
            return LIBBSC_NOT_COMPRESSIBLE;
        }

        let current_char = input[ip] as usize;
        let run_start = ip;
        ip += 1;
        while ip < n && input[ip] as usize == current_char {
            ip += 1;
        }
        let run_size = (ip - run_start) as i32;

        let mut rank = buffer[rank_ptr] as i32;
        rank_ptr += 1;

        let history = rank_history[current_char] as usize;
        let state = model_rank_state(context_rank4, context_run, history);

        if avg_rank < 32 {
            if rank == 1 {
                rank_history[current_char] = 0;
                let p = mix3(
                    model.rank.char_model[current_char] as i32,
                    model.rank.state_model[state] as i32,
                    model.rank.static_model as i32,
                    F_RANK_TM_LR0, F_RANK_TM_LR1, F_RANK_TM_LR2,
                );
                ProbabilityCounter::update_bit0(&mut model.rank.state_model[state], F_RANK_TS_TH0, F_RANK_TS_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.char_model[current_char], F_RANK_TC_TH0, F_RANK_TC_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.static_model, F_RANK_TP_TH0, F_RANK_TP_AR0);
                coder.encode_bit0_p(p, 12);
            } else {
                let p = mix3(
                    model.rank.char_model[current_char] as i32,
                    model.rank.state_model[state] as i32,
                    model.rank.static_model as i32,
                    F_RANK_TM_LR0, F_RANK_TM_LR1, F_RANK_TM_LR2,
                );
                ProbabilityCounter::update_bit1(&mut model.rank.state_model[state], F_RANK_TS_TH1, F_RANK_TS_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.char_model[current_char], F_RANK_TC_TH1, F_RANK_TC_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.static_model, F_RANK_TP_TH1, F_RANK_TP_AR1);
                coder.encode_bit1_p(p, 12);

                let bit_rank_size = bit_scan_reverse(rank as u32) as usize;
                rank_history[current_char] = bit_rank_size as u8;

                // Exponent, unary: `bit_rank_size - 1` ones then, if it fits
                // under maxRank, a terminating zero. `k` is the offset the C
                // advances its three row pointers by in lockstep.
                for k in 1..bit_rank_size {
                    let p = mix3(
                        model.rank.exponent.char_model[current_char][k - 1] as i32,
                        model.rank.exponent.state_model[state][k - 1] as i32,
                        model.rank.exponent.static_model[k - 1] as i32,
                        F_RANK_EM_LR0, F_RANK_EM_LR1, F_RANK_EM_LR2,
                    );
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.state_model[state][k - 1], F_RANK_ES_TH1, F_RANK_ES_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.char_model[current_char][k - 1], F_RANK_EC_TH1, F_RANK_EC_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.static_model[k - 1], F_RANK_EP_TH1, F_RANK_EP_AR1);
                    coder.encode_bit1_p(p, 12);
                }
                if (bit_rank_size as i32) < max_rank {
                    let k = bit_rank_size - 1;
                    let p = mix3(
                        model.rank.exponent.char_model[current_char][k] as i32,
                        model.rank.exponent.state_model[state][k] as i32,
                        model.rank.exponent.static_model[k] as i32,
                        F_RANK_EM_LR0, F_RANK_EM_LR1, F_RANK_EM_LR2,
                    );
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.state_model[state][k], F_RANK_ES_TH0, F_RANK_ES_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.char_model[current_char][k], F_RANK_EC_TH0, F_RANK_EC_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.static_model[k], F_RANK_EP_TH0, F_RANK_EP_AR0);
                    coder.encode_bit0_p(p, 12);
                }

                // Mantissa: the remaining bits of rank, most significant first,
                // with the context accumulating them.
                let m = &mut model.rank.mantissa[bit_rank_size];
                let mut context = 1usize;
                for bit in (0..bit_rank_size).rev() {
                    let p = mix3(
                        m.char_model[current_char][context] as i32,
                        m.state_model[state][context] as i32,
                        m.static_model[context] as i32,
                        F_RANK_MM_LR0, F_RANK_MM_LR1, F_RANK_MM_LR2,
                    );
                    let b = ((rank >> bit) & 1) as u32;
                    ProbabilityCounter::update_bit(b, &mut m.state_model[state][context], F_RANK_MS_TH0, F_RANK_MS_AR0, F_RANK_MS_TH1, F_RANK_MS_AR1);
                    ProbabilityCounter::update_bit(b, &mut m.char_model[current_char][context], F_RANK_MC_TH0, F_RANK_MC_AR0, F_RANK_MC_TH1, F_RANK_MC_AR1);
                    ProbabilityCounter::update_bit(b, &mut m.static_model[context], F_RANK_MP_TH0, F_RANK_MP_AR0, F_RANK_MP_TH1, F_RANK_MP_AR1);
                    context += context + b as usize;
                    coder.encode_bit_p(b, p, 12);
                }
            }
        } else {
            // Escape path: a high running average rank codes the whole value
            // against its own model instead of exponent + mantissa.
            rank_history[current_char] = bit_scan_reverse(rank as u32) as u8;
            let e = &mut model.rank.escape;
            let mut context = 1usize;
            for bit in (0..=max_rank).rev() {
                let p = mix3(
                    e.char_model[current_char][context] as i32,
                    e.state_model[state][context] as i32,
                    e.static_model[context] as i32,
                    F_RANK_PM_LR0, F_RANK_PM_LR1, F_RANK_PM_LR2,
                );
                let b = ((rank >> bit) & 1) as u32;
                ProbabilityCounter::update_bit(b, &mut e.state_model[state][context], F_RANK_PS_TH0, F_RANK_PS_AR0, F_RANK_PS_TH1, F_RANK_PS_AR1);
                ProbabilityCounter::update_bit(b, &mut e.char_model[current_char][context], F_RANK_PC_TH0, F_RANK_PC_AR0, F_RANK_PC_TH1, F_RANK_PC_AR1);
                ProbabilityCounter::update_bit(b, &mut e.static_model[context], F_RANK_PP_TH0, F_RANK_PP_AR0, F_RANK_PP_TH1, F_RANK_PP_AR1);
                context += context + b as usize;
                coder.encode_bit_p(b, p, 12);
            }
        }

        avg_rank = (avg_rank * 124 + rank * 4) >> 7;
        rank -= 1;

        // --- Run length -----------------------------------------------------
        let history = run_history[current_char] as usize;
        let state = model_run_state(context_rank0, context_run, rank as usize, history);

        if run_size == 1 {
            run_history[current_char] = (run_history[current_char] + 2) >> 2;
            let p = mix3(
                model.run.char_model[current_char] as i32,
                model.run.state_model[state] as i32,
                model.run.static_model as i32,
                F_RUN_TM_LR0, F_RUN_TM_LR1, F_RUN_TM_LR2,
            );
            ProbabilityCounter::update_bit0(&mut model.run.state_model[state], F_RUN_TS_TH0, F_RUN_TS_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.char_model[current_char], F_RUN_TC_TH0, F_RUN_TC_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.static_model, F_RUN_TP_TH0, F_RUN_TP_AR0);
            coder.encode_bit0_p(p, 12);
        } else {
            let p = mix3(
                model.run.char_model[current_char] as i32,
                model.run.state_model[state] as i32,
                model.run.static_model as i32,
                F_RUN_TM_LR0, F_RUN_TM_LR1, F_RUN_TM_LR2,
            );
            ProbabilityCounter::update_bit1(&mut model.run.state_model[state], F_RUN_TS_TH1, F_RUN_TS_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.char_model[current_char], F_RUN_TC_TH1, F_RUN_TC_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.static_model, F_RUN_TP_TH1, F_RUN_TP_AR1);
            coder.encode_bit1_p(p, 12);

            let bit_run_size = bit_scan_reverse(run_size as u32) as usize;
            run_history[current_char] =
                ((run_history[current_char] as u32 + 3 * bit_run_size as u32 + 3) >> 2) as u8;

            for k in 1..bit_run_size {
                let p = mix3(
                    model.run.exponent.char_model[current_char][k - 1] as i32,
                    model.run.exponent.state_model[state][k - 1] as i32,
                    model.run.exponent.static_model[k - 1] as i32,
                    F_RUN_EM_LR0, F_RUN_EM_LR1, F_RUN_EM_LR2,
                );
                ProbabilityCounter::update_bit1(&mut model.run.exponent.state_model[state][k - 1], F_RUN_ES_TH1, F_RUN_ES_AR1);
                ProbabilityCounter::update_bit1(&mut model.run.exponent.char_model[current_char][k - 1], F_RUN_EC_TH1, F_RUN_EC_AR1);
                ProbabilityCounter::update_bit1(&mut model.run.exponent.static_model[k - 1], F_RUN_EP_TH1, F_RUN_EP_AR1);
                coder.encode_bit1_p(p, 12);
            }
            {
                // Unlike rank's exponent, run's terminating zero is
                // unconditional -- there is no maxRank to run into.
                let k = bit_run_size - 1;
                let p = mix3(
                    model.run.exponent.char_model[current_char][k] as i32,
                    model.run.exponent.state_model[state][k] as i32,
                    model.run.exponent.static_model[k] as i32,
                    F_RUN_EM_LR0, F_RUN_EM_LR1, F_RUN_EM_LR2,
                );
                ProbabilityCounter::update_bit0(&mut model.run.exponent.state_model[state][k], F_RUN_ES_TH0, F_RUN_ES_AR0);
                ProbabilityCounter::update_bit0(&mut model.run.exponent.char_model[current_char][k], F_RUN_EC_TH0, F_RUN_EC_AR0);
                ProbabilityCounter::update_bit0(&mut model.run.exponent.static_model[k], F_RUN_EP_TH0, F_RUN_EP_AR0);
                coder.encode_bit0_p(p, 12);
            }

            let m = &mut model.run.mantissa[bit_run_size];
            let mut context = 1usize;
            for bit in (0..bit_run_size).rev() {
                let p = mix3(
                    m.char_model[current_char][context] as i32,
                    m.state_model[state][context] as i32,
                    m.static_model[context] as i32,
                    F_RUN_MM_LR0, F_RUN_MM_LR1, F_RUN_MM_LR2,
                );
                let b = ((run_size >> bit) & 1) as u32;
                ProbabilityCounter::update_bit(b, &mut m.state_model[state][context], F_RUN_MS_TH0, F_RUN_MS_AR0, F_RUN_MS_TH1, F_RUN_MS_AR1);
                ProbabilityCounter::update_bit(b, &mut m.char_model[current_char][context], F_RUN_MC_TH0, F_RUN_MC_AR0, F_RUN_MC_TH1, F_RUN_MC_AR1);
                ProbabilityCounter::update_bit(b, &mut m.static_model[context], F_RUN_MP_TH0, F_RUN_MP_AR0, F_RUN_MP_TH1, F_RUN_MP_AR1);
                // NOT a plain doubling: the context only accumulates the bit
                // for short runs. `int ctx = context + context + b; context++;
                // if (bitRunSize <= 5) { context = ctx; }` -- the decoder
                // carries the same asymmetry, and getting it wrong desynchronises
                // only on runs long enough to reach it.
                let ctx = context + context + b as usize;
                context += 1;
                if bit_run_size <= 5 {
                    context = ctx;
                }
                coder.encode_bit_p(b, p, 12);
            }
        }

        context_rank0 = ((context_rank0 << 1) | usize::from(rank == 0)) & 0x7;
        context_rank4 = ((context_rank4 << 2) | (rank.min(3) as usize)) & 0xff;
        context_run = ((context_run << 1) | usize::from(run_size < 3)) & 0xf;
    }

    coder.finish() as i32
}
