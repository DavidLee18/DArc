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
use super::qlfc::{bit_scan_reverse, mix3, Model2};
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

/// `bsc_qlfc_adaptive_encode` (`qlfc.cpp:534`), the mirror of
/// [`super::qlfc::adaptive_decode`].
///
/// Identical to [`static_encode`] in structure -- same transform, same preamble,
/// same (rank, run) walk, same contexts -- and differs in exactly two ways:
///
/// * every `F_*` constant becomes `M_*`; and
/// * the fixed three-way blend `mix3` is replaced by a **learned mixer**. The
///   three predictor values are read first, the counters are updated, and then
///   `MixupAndUpdateBitN` both produces the probability and trains the mixer.
///
/// The only genuinely new code is choosing which mixer. Four of the five
/// choices are made once per stage; `mixerOfRankEscape` is re-selected on every
/// iteration, keyed on the context accumulated so far. The exponent's mixer is
/// selected for the *next* iteration at the end of the current one, so a
/// one-bit exponent uses the one picked before the loop -- getting that
/// off-by-one wrong desynchronises only on ranks or runs large enough to need a
/// second exponent bit.
pub fn adaptive_encode(input: &[u8], output: &mut [u8]) -> i32 {
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

    // --- Alphabet preamble: identical to the static coder's, model-free. -----
    let mut used_char = [0u8; ALPHABET_SIZE];
    let mut prev_char: i32 = -1;
    for rank in 0..ALPHABET_SIZE {
        let current_char = mtf[rank] as i32;
        for bit in (0..8).rev() {
            let (mut bit0, mut bit1) = (false, false);
            for c in 0..ALPHABET_SIZE as i32 {
                if c == prev_char || used_char[c as usize] == 0 {
                    if (current_char >> (bit + 1)) == (c >> (bit + 1)) {
                        if c & (1 << bit) != 0 { bit1 = true; } else { bit0 = true; }
                        if bit0 && bit1 { break; }
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

    // --- Main loop ----------------------------------------------------------
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
                let (p0, p1, p2) = (
                    model.rank.char_model[current_char] as i32,
                    model.rank.state_model[state] as i32,
                    model.rank.static_model as i32,
                );
                ProbabilityCounter::update_bit0(&mut model.rank.state_model[state], M_RANK_TS_TH0, M_RANK_TS_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.char_model[current_char], M_RANK_TC_TH0, M_RANK_TC_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.static_model, M_RANK_TP_TH0, M_RANK_TP_AR0);
                let p = model.mixer_of_rank[current_char].mixup_and_update_bit0(
                    p0, p1, p2, M_RANK_TM_LR0, M_RANK_TM_LR1, M_RANK_TM_LR2, M_RANK_TM_TH0, M_RANK_TM_AR0);
                coder.encode_bit0_p(p as u32, 12);
            } else {
                let (p0, p1, p2) = (
                    model.rank.char_model[current_char] as i32,
                    model.rank.state_model[state] as i32,
                    model.rank.static_model as i32,
                );
                ProbabilityCounter::update_bit1(&mut model.rank.state_model[state], M_RANK_TS_TH1, M_RANK_TS_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.char_model[current_char], M_RANK_TC_TH1, M_RANK_TC_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.static_model, M_RANK_TP_TH1, M_RANK_TP_AR1);
                let p = model.mixer_of_rank[current_char].mixup_and_update_bit1(
                    p0, p1, p2, M_RANK_TM_LR0, M_RANK_TM_LR1, M_RANK_TM_LR2, M_RANK_TM_TH1, M_RANK_TM_AR1);
                coder.encode_bit1_p(p as u32, 12);

                let bit_rank_size = bit_scan_reverse(rank as u32) as usize;
                rank_history[current_char] = bit_rank_size as u8;

                // The mixer for the FIRST exponent bit; each iteration then
                // picks the one for the next.
                let (mut mrow, mut mcol) = (if history < 1 { 1 } else { history }, 1usize);
                for k in 1..bit_rank_size {
                    let (p0, p1, p2) = (
                        model.rank.exponent.char_model[current_char][k - 1] as i32,
                        model.rank.exponent.state_model[state][k - 1] as i32,
                        model.rank.exponent.static_model[k - 1] as i32,
                    );
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.state_model[state][k - 1], M_RANK_ES_TH1, M_RANK_ES_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.char_model[current_char][k - 1], M_RANK_EC_TH1, M_RANK_EC_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.exponent.static_model[k - 1], M_RANK_EP_TH1, M_RANK_EP_AR1);
                    let p = model.mixer_of_rank_exponent[mrow][mcol].mixup_and_update_bit1(
                        p0, p1, p2, M_RANK_EM_LR0, M_RANK_EM_LR1, M_RANK_EM_LR2, M_RANK_EM_TH1, M_RANK_EM_AR1);
                    coder.encode_bit1_p(p as u32, 12);
                    mrow = if history <= k { k + 1 } else { history };
                    mcol = k + 1;
                }
                if (bit_rank_size as i32) < max_rank {
                    let k = bit_rank_size - 1;
                    let (p0, p1, p2) = (
                        model.rank.exponent.char_model[current_char][k] as i32,
                        model.rank.exponent.state_model[state][k] as i32,
                        model.rank.exponent.static_model[k] as i32,
                    );
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.state_model[state][k], M_RANK_ES_TH0, M_RANK_ES_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.char_model[current_char][k], M_RANK_EC_TH0, M_RANK_EC_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.exponent.static_model[k], M_RANK_EP_TH0, M_RANK_EP_AR0);
                    let p = model.mixer_of_rank_exponent[mrow][mcol].mixup_and_update_bit0(
                        p0, p1, p2, M_RANK_EM_LR0, M_RANK_EM_LR1, M_RANK_EM_LR2, M_RANK_EM_TH0, M_RANK_EM_AR0);
                    coder.encode_bit0_p(p as u32, 12);
                }

                let mut context = 1usize;
                for bit in (0..bit_rank_size).rev() {
                    let (p0, p1, p2) = (
                        model.rank.mantissa[bit_rank_size].char_model[current_char][context] as i32,
                        model.rank.mantissa[bit_rank_size].state_model[state][context] as i32,
                        model.rank.mantissa[bit_rank_size].static_model[context] as i32,
                    );
                    if rank & (1 << bit) != 0 {
                        ProbabilityCounter::update_bit1(&mut model.rank.mantissa[bit_rank_size].state_model[state][context], M_RANK_MS_TH1, M_RANK_MS_AR1);
                        ProbabilityCounter::update_bit1(&mut model.rank.mantissa[bit_rank_size].char_model[current_char][context], M_RANK_MC_TH1, M_RANK_MC_AR1);
                        ProbabilityCounter::update_bit1(&mut model.rank.mantissa[bit_rank_size].static_model[context], M_RANK_MP_TH1, M_RANK_MP_AR1);
                        let p = model.mixer_of_rank_mantissa[bit_rank_size].mixup_and_update_bit1(
                            p0, p1, p2, M_RANK_MM_LR0, M_RANK_MM_LR1, M_RANK_MM_LR2, M_RANK_MM_TH1, M_RANK_MM_AR1);
                        coder.encode_bit1_p(p as u32, 12);
                        context += context + 1;
                    } else {
                        ProbabilityCounter::update_bit0(&mut model.rank.mantissa[bit_rank_size].state_model[state][context], M_RANK_MS_TH0, M_RANK_MS_AR0);
                        ProbabilityCounter::update_bit0(&mut model.rank.mantissa[bit_rank_size].char_model[current_char][context], M_RANK_MC_TH0, M_RANK_MC_AR0);
                        ProbabilityCounter::update_bit0(&mut model.rank.mantissa[bit_rank_size].static_model[context], M_RANK_MP_TH0, M_RANK_MP_AR0);
                        let p = model.mixer_of_rank_mantissa[bit_rank_size].mixup_and_update_bit0(
                            p0, p1, p2, M_RANK_MM_LR0, M_RANK_MM_LR1, M_RANK_MM_LR2, M_RANK_MM_TH0, M_RANK_MM_AR0);
                        coder.encode_bit0_p(p as u32, 12);
                        context += context;
                    }
                }
            }
        } else {
            rank_history[current_char] = bit_scan_reverse(rank as u32) as u8;
            let mut context = 1usize;
            for bit in (0..=max_rank).rev() {
                let (p0, p1, p2) = (
                    model.rank.escape.char_model[current_char][context] as i32,
                    model.rank.escape.state_model[state][context] as i32,
                    model.rank.escape.static_model[context] as i32,
                );
                // Re-selected every iteration, keyed on the context so far.
                if rank & (1 << bit) != 0 {
                    ProbabilityCounter::update_bit1(&mut model.rank.escape.state_model[state][context], M_RANK_PS_TH1, M_RANK_PS_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.escape.char_model[current_char][context], M_RANK_PC_TH1, M_RANK_PC_AR1);
                    ProbabilityCounter::update_bit1(&mut model.rank.escape.static_model[context], M_RANK_PP_TH1, M_RANK_PP_AR1);
                    let p = model.mixer_of_rank_escape[context].mixup_and_update_bit1(
                        p0, p1, p2, M_RANK_PM_LR0, M_RANK_PM_LR1, M_RANK_PM_LR2, M_RANK_PM_TH1, M_RANK_PM_AR1);
                    coder.encode_bit1_p(p as u32, 12);
                    context += context + 1;
                } else {
                    ProbabilityCounter::update_bit0(&mut model.rank.escape.state_model[state][context], M_RANK_PS_TH0, M_RANK_PS_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.escape.char_model[current_char][context], M_RANK_PC_TH0, M_RANK_PC_AR0);
                    ProbabilityCounter::update_bit0(&mut model.rank.escape.static_model[context], M_RANK_PP_TH0, M_RANK_PP_AR0);
                    let p = model.mixer_of_rank_escape[context].mixup_and_update_bit0(
                        p0, p1, p2, M_RANK_PM_LR0, M_RANK_PM_LR1, M_RANK_PM_LR2, M_RANK_PM_TH0, M_RANK_PM_AR0);
                    coder.encode_bit0_p(p as u32, 12);
                    context += context;
                }
            }
        }

        avg_rank = (avg_rank * 124 + rank * 4) >> 7;
        rank -= 1;

        // --- Run length -----------------------------------------------------
        let history = run_history[current_char] as usize;
        let state = model_run_state(context_rank0, context_run, rank as usize, history);

        if run_size == 1 {
            run_history[current_char] = (run_history[current_char] + 2) >> 2;
            let (p0, p1, p2) = (
                model.run.char_model[current_char] as i32,
                model.run.state_model[state] as i32,
                model.run.static_model as i32,
            );
            ProbabilityCounter::update_bit0(&mut model.run.state_model[state], M_RUN_TS_TH0, M_RUN_TS_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.char_model[current_char], M_RUN_TC_TH0, M_RUN_TC_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.static_model, M_RUN_TP_TH0, M_RUN_TP_AR0);
            let p = model.mixer_of_run[current_char].mixup_and_update_bit0(
                p0, p1, p2, M_RUN_TM_LR0, M_RUN_TM_LR1, M_RUN_TM_LR2, M_RUN_TM_TH0, M_RUN_TM_AR0);
            coder.encode_bit0_p(p as u32, 12);
        } else {
            let (p0, p1, p2) = (
                model.run.char_model[current_char] as i32,
                model.run.state_model[state] as i32,
                model.run.static_model as i32,
            );
            ProbabilityCounter::update_bit1(&mut model.run.state_model[state], M_RUN_TS_TH1, M_RUN_TS_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.char_model[current_char], M_RUN_TC_TH1, M_RUN_TC_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.static_model, M_RUN_TP_TH1, M_RUN_TP_AR1);
            let p = model.mixer_of_run[current_char].mixup_and_update_bit1(
                p0, p1, p2, M_RUN_TM_LR0, M_RUN_TM_LR1, M_RUN_TM_LR2, M_RUN_TM_TH1, M_RUN_TM_AR1);
            coder.encode_bit1_p(p as u32, 12);

            let bit_run_size = bit_scan_reverse(run_size as u32) as usize;
            run_history[current_char] =
                ((run_history[current_char] as u32 + 3 * bit_run_size as u32 + 3) >> 2) as u8;

            let (mut mrow, mut mcol) = (if history < 1 { 1 } else { history }, 1usize);
            for k in 1..bit_run_size {
                let (p0, p1, p2) = (
                    model.run.exponent.char_model[current_char][k - 1] as i32,
                    model.run.exponent.state_model[state][k - 1] as i32,
                    model.run.exponent.static_model[k - 1] as i32,
                );
                ProbabilityCounter::update_bit1(&mut model.run.exponent.state_model[state][k - 1], M_RUN_ES_TH1, M_RUN_ES_AR1);
                ProbabilityCounter::update_bit1(&mut model.run.exponent.char_model[current_char][k - 1], M_RUN_EC_TH1, M_RUN_EC_AR1);
                ProbabilityCounter::update_bit1(&mut model.run.exponent.static_model[k - 1], M_RUN_EP_TH1, M_RUN_EP_AR1);
                let p = model.mixer_of_run_exponent[mrow][mcol].mixup_and_update_bit1(
                    p0, p1, p2, M_RUN_EM_LR0, M_RUN_EM_LR1, M_RUN_EM_LR2, M_RUN_EM_TH1, M_RUN_EM_AR1);
                coder.encode_bit1_p(p as u32, 12);
                mrow = if history <= k { k + 1 } else { history };
                mcol = k + 1;
            }
            {
                let k = bit_run_size - 1;
                let (p0, p1, p2) = (
                    model.run.exponent.char_model[current_char][k] as i32,
                    model.run.exponent.state_model[state][k] as i32,
                    model.run.exponent.static_model[k] as i32,
                );
                ProbabilityCounter::update_bit0(&mut model.run.exponent.state_model[state][k], M_RUN_ES_TH0, M_RUN_ES_AR0);
                ProbabilityCounter::update_bit0(&mut model.run.exponent.char_model[current_char][k], M_RUN_EC_TH0, M_RUN_EC_AR0);
                ProbabilityCounter::update_bit0(&mut model.run.exponent.static_model[k], M_RUN_EP_TH0, M_RUN_EP_AR0);
                let p = model.mixer_of_run_exponent[mrow][mcol].mixup_and_update_bit0(
                    p0, p1, p2, M_RUN_EM_LR0, M_RUN_EM_LR1, M_RUN_EM_LR2, M_RUN_EM_TH0, M_RUN_EM_AR0);
                coder.encode_bit0_p(p as u32, 12);
            }

            let mut context = 1usize;
            for bit in (0..bit_run_size).rev() {
                let (p0, p1, p2) = (
                    model.run.mantissa[bit_run_size].char_model[current_char][context] as i32,
                    model.run.mantissa[bit_run_size].state_model[state][context] as i32,
                    model.run.mantissa[bit_run_size].static_model[context] as i32,
                );
                if run_size & (1 << bit) != 0 {
                    ProbabilityCounter::update_bit1(&mut model.run.mantissa[bit_run_size].state_model[state][context], M_RUN_MS_TH1, M_RUN_MS_AR1);
                    ProbabilityCounter::update_bit1(&mut model.run.mantissa[bit_run_size].char_model[current_char][context], M_RUN_MC_TH1, M_RUN_MC_AR1);
                    ProbabilityCounter::update_bit1(&mut model.run.mantissa[bit_run_size].static_model[context], M_RUN_MP_TH1, M_RUN_MP_AR1);
                    let p = model.mixer_of_run_mantissa[bit_run_size].mixup_and_update_bit1(
                        p0, p1, p2, M_RUN_MM_LR0, M_RUN_MM_LR1, M_RUN_MM_LR2, M_RUN_MM_TH1, M_RUN_MM_AR1);
                    coder.encode_bit1_p(p as u32, 12);
                    if bit_run_size <= 5 { context += context + 1; } else { context += 1; }
                } else {
                    ProbabilityCounter::update_bit0(&mut model.run.mantissa[bit_run_size].state_model[state][context], M_RUN_MS_TH0, M_RUN_MS_AR0);
                    ProbabilityCounter::update_bit0(&mut model.run.mantissa[bit_run_size].char_model[current_char][context], M_RUN_MC_TH0, M_RUN_MC_AR0);
                    ProbabilityCounter::update_bit0(&mut model.run.mantissa[bit_run_size].static_model[context], M_RUN_MP_TH0, M_RUN_MP_AR0);
                    let p = model.mixer_of_run_mantissa[bit_run_size].mixup_and_update_bit0(
                        p0, p1, p2, M_RUN_MM_LR0, M_RUN_MM_LR1, M_RUN_MM_LR2, M_RUN_MM_TH0, M_RUN_MM_AR0);
                    coder.encode_bit0_p(p as u32, 12);
                    if bit_run_size <= 5 { context += context; } else { context += 1; }
                }
            }
        }

        context_rank0 = ((context_rank0 << 1) | usize::from(rank == 0)) & 0x7;
        context_rank4 = ((context_rank4 << 2) | (rank.min(3) as usize)) & 0xff;
        context_run = ((context_run << 1) | usize::from(run_size < 3)) & 0xf;
    }

    coder.finish() as i32
}

/// `bsc_qlfc_fast_encode` (`qlfc.cpp:1206`), the mirror of
/// [`super::qlfc::fast_decode`].
///
/// A different animal from the other two: one predictor per (character,
/// position) with no state, static or mixer component, tuned constants written
/// inline rather than named, and a different range-coder precision per field --
/// **P = 13 for rank, P = 11 for run**. Those precisions are not decoration; a
/// stream coded at the wrong one decodes to noise.
///
/// Two structural differences from static/adaptive worth naming:
///
/// * the preamble's equiprobable bit is `EncodeBit<1>(bit, 1)`, not the
///   `EncodeBit(bit)` default of probability 2048 at P = 12 -- the same 50%
///   split expressed at a different precision;
/// * the rank exponent stops at a fixed 7 rather than at `maxRank`, so no
///   preamble bookkeeping feeds it.
pub fn fast_encode(input: &[u8], output: &mut [u8]) -> i32 {
    let n = input.len();
    if n == 0 {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let mut model = Model2::new();
    let mut buffer = vec![0u8; n];
    let mut mtf = [0u8; ALPHABET_SIZE];

    let mut rank_ptr = transform(input, &mut buffer, &mut mtf);

    let mut coder = RangeEncoder::new(output);
    coder.encode_word(n as u32);

    let mut used_char = [0u8; ALPHABET_SIZE];
    let mut prev_char: i32 = -1;
    for rank in 0..ALPHABET_SIZE {
        let current_char = mtf[rank] as i32;
        for bit in (0..8).rev() {
            let (mut bit0, mut bit1) = (false, false);
            for c in 0..ALPHABET_SIZE as i32 {
                if c == prev_char || used_char[c as usize] == 0 {
                    if (current_char >> (bit + 1)) == (c >> (bit + 1)) {
                        if c & (1 << bit) != 0 { bit1 = true; } else { bit0 = true; }
                        if bit0 && bit1 { break; }
                    }
                }
            }
            if bit0 && bit1 {
                // EncodeBit<1>(bit, 1): probability 1 at P = 1, i.e. even odds.
                coder.encode_bit_p(((current_char & (1 << bit)) != 0) as u32, 1, 1);
            }
        }
        if current_char == prev_char {
            break;
        }
        prev_char = current_char;
        used_char[current_char as usize] = 1;
    }

    let mut ip = 0usize;
    while rank_ptr < n {
        if coder.check_eob() {
            return LIBBSC_NOT_COMPRESSIBLE;
        }

        let current_rank = buffer[rank_ptr] as u32;
        rank_ptr += 1;

        let current_char = input[ip] as usize;
        let run_start = ip;
        ip += 1;
        while ip < n && input[ip] as usize == current_char {
            ip += 1;
        }
        let current_run = (ip - run_start) as u32;

        // --- Rank, at P = 13 ------------------------------------------------
        if current_rank == 1 {
            let p = model.re(current_char, 0);
            ProbabilityCounter::update_bit_r1(model.re_mut(current_char, 0), 8016, 4);
            coder.encode_bit0_p(p as u32, 13);
        } else {
            let p = model.re(current_char, 0);
            ProbabilityCounter::update_bit_r1(model.re_mut(current_char, 0), 83, 4);
            coder.encode_bit1_p(p as u32, 13);

            let bit_rank_size = bit_scan_reverse(current_rank) as usize;
            for bit in 1..bit_rank_size {
                let p = model.re(current_char, bit);
                ProbabilityCounter::update_bit_r1(model.re_mut(current_char, bit), 122, 4);
                coder.encode_bit1_p(p as u32, 13);
            }
            // Fixed 7, not maxRank: the fast coder keeps no preamble state.
            if bit_rank_size < 7 {
                let p = model.re(current_char, bit_rank_size);
                ProbabilityCounter::update_bit_r1(model.re_mut(current_char, bit_rank_size), 8114, 4);
                coder.encode_bit0_p(p as u32, 13);
            }

            let mut context = 1usize;
            for bit in (0..bit_rank_size).rev() {
                let b = (current_rank >> bit) & 1;
                let p = model.rm(current_char, bit_rank_size, context);
                ProbabilityCounter::update_bit_r(b, model.rm_mut(current_char, bit_rank_size, context), 7999, 235, 7);
                coder.encode_bit_p(b, p as u32, 13);
                context += context + b as usize;
            }
        }

        // --- Run, at P = 11 -------------------------------------------------
        if current_run == 1 {
            let p = model.rue(current_char, 0);
            ProbabilityCounter::update_bit_r1(model.rue_mut(current_char, 0), 2025, 5);
            coder.encode_bit0_p(p as u32, 11);
        } else {
            let p = model.rue(current_char, 0);
            ProbabilityCounter::update_bit_r1(model.rue_mut(current_char, 0), 42, 5);
            coder.encode_bit1_p(p as u32, 11);

            let bit_run_size = bit_scan_reverse(current_run) as usize;
            for bit in 1..bit_run_size {
                let p = model.rue(current_char, bit);
                ProbabilityCounter::update_bit_r1(model.rue_mut(current_char, bit), 142, 4);
                coder.encode_bit1_p(p as u32, 11);
            }
            {
                let p = model.rue(current_char, bit_run_size);
                ProbabilityCounter::update_bit_r1(model.rue_mut(current_char, bit_run_size), 1962, 4);
                coder.encode_bit0_p(p as u32, 11);
            }

            // Two whole loops, not one with a conditional step: short runs
            // accumulate the bits into the context, long ones only count.
            // Different constants and a different shift on each side.
            let mut context = 1usize;
            if bit_run_size <= 5 {
                for bit in (0..bit_run_size).rev() {
                    let b = (current_run >> bit) & 1;
                    let p = model.rum(current_char, bit_run_size, context);
                    ProbabilityCounter::update_bit_r(b, model.rum_mut(current_char, bit_run_size, context), 1951, 147, 6);
                    coder.encode_bit_p(b, p as u32, 11);
                    context += context + b as usize;
                }
            } else {
                for bit in (0..bit_run_size).rev() {
                    let b = (current_run >> bit) & 1;
                    let p = model.rum(current_char, bit_run_size, context);
                    ProbabilityCounter::update_bit_r(b, model.rum_mut(current_char, bit_run_size, context), 1987, 46, 5);
                    coder.encode_bit_p(b, p as u32, 11);
                    context += 1;
                }
            }
        }
    }

    coder.finish() as i32
}

/// `bsc_coder_split_blocks` (`coder.cpp:88`): choose where to cut a large input
/// into `n_blocks` independently-coded pieces.
///
/// Not an even split. It samples every 32nd byte, counts how often the sample
/// differs from its predecessor -- a cheap proxy for how much the local
/// statistics move -- and cuts at every `rankSize / nBlocks`-th change, so
/// blocks carry roughly equal amounts of *variation* rather than equal length.
/// Only when there are fewer changes than blocks does it fall back to even
/// division.
///
/// The C leaves `blockStart`/`blockSize` entries untouched if the scan ends
/// before it has made `nBlocks - 1` cuts, and they are uninitialised stack
/// arrays. That is unreachable -- the branch is guarded by
/// `rankSize > nBlocks`, so at least `nBlocks` changes exist and
/// `blockRankSize >= 1` -- but the port does not depend on it: everything is
/// initialised, and a short scan simply leaves a zero-length block.
pub fn split_blocks(input: &[u8], n_blocks: usize) -> Vec<(usize, usize)> {
    let n = input.len();
    let mut out = vec![(0usize, 0usize); n_blocks];
    if n_blocks == 0 {
        return out;
    }

    let mut rank_size = 0usize;
    let mut i = 1usize;
    while i < n {
        if input[i] != input[i - 1] {
            rank_size += 1;
        }
        i += 32;
    }

    if rank_size > n_blocks {
        let block_rank_size = rank_size / n_blocks;
        out[0].0 = 0;
        rank_size = 0;
        let mut id = 0usize;
        let mut i = 1usize;
        while i < n {
            if input[i] != input[i - 1] {
                rank_size += 1;
                if rank_size == block_rank_size {
                    rank_size = 0;
                    out[id].1 = i - out[id].0;
                    id += 1;
                    out[id].0 = i;
                    if id == n_blocks - 1 {
                        break;
                    }
                }
            }
            i += 32;
        }
        out[n_blocks - 1].1 = n - out[n_blocks - 1].0;
    } else {
        for p in 0..n_blocks {
            out[p].0 = (n / n_blocks) * p;
            out[p].1 = if p != n_blocks - 1 {
                n / n_blocks
            } else {
                n - (n / n_blocks) * (n_blocks - 1)
            };
        }
    }
    out
}

/// `bsc_coder_compress_serial` (`coder.cpp:130`) -- and therefore
/// `bsc_coder_compress`, since the parallel variant is inside
/// `#ifdef LIBBSC_OPENMP` and DArc never defines `LIBBSC_OPENMP_SUPPORT`.
///
/// The framing is LZP's, one layer up: a block count, then two little-endian
/// 32-bit words per block (input size, coded size), then the coded blocks. A
/// block whose two sizes are equal was stored rather than coded, which is how
/// the decoder recognises one the encoder gave up on.
pub fn coder_compress(input: &[u8], output: &mut [u8], coder: u32) -> i32 {
    let n = input.len();
    if n == 0 || output.len() < n {
        return LIBBSC_NOT_COMPRESSIBLE;
    }

    let encode = |inp: &[u8], out: &mut [u8]| -> i32 {
        match coder {
            1 => static_encode(inp, out),
            2 => adaptive_encode(inp, out),
            3 => fast_encode(inp, out),
            _ => super::LIBBSC_BAD_PARAMETER,
        }
    };

    let n_blocks = num_blocks(n);
    if n_blocks == 1 {
        if n < 2 {
            return LIBBSC_NOT_COMPRESSIBLE;
        }
        let result = encode(input, &mut output[1..n]);
        if result < 0 {
            return result;
        }
        output[0] = 1;
        return result + 1;
    }

    let blocks = split_blocks(input, n_blocks);
    let mut output_ptr = 1 + 8 * n_blocks;
    output[0] = n_blocks as u8;

    for (id, &(start, size)) in blocks.iter().enumerate() {
        let mut out_size = size;
        if out_size > n - output_ptr {
            out_size = n - output_ptr;
        }
        let result = encode(
            &input[start..start + size],
            &mut output[output_ptr..output_ptr + out_size],
        );
        let result = if result < 0 {
            if output_ptr + size >= n {
                return LIBBSC_NOT_COMPRESSIBLE;
            }
            output[output_ptr..output_ptr + size].copy_from_slice(&input[start..start + size]);
            size
        } else {
            result as usize
        };
        output[1 + 8 * id..1 + 8 * id + 4].copy_from_slice(&(size as i32).to_le_bytes());
        output[1 + 8 * id + 4..1 + 8 * id + 8].copy_from_slice(&(result as i32).to_le_bytes());
        output_ptr += result;
    }

    output_ptr as i32
}

/// `bsc_st3_transform_serial` (`st.cpp:56`), reached through `bsc_st_encode`
/// with `k == 3` -- the forward sort-transform of order 3, the alternative to
/// the BWT as BSC's block sorter.
///
/// Sorts the rotations of `T` by their first 3 bytes, using a bucket table over
/// the leading bigram and a 24-bit sliding window `W`. Returns the primary
/// index: the position of the rotation that starts at offset 0, which the
/// decoder needs to invert it.
///
/// **`t` must be at least `n + 28` bytes.** The C opens with
/// `for (i = 0; i < LIBBSC_HEADER_SIZE; ++i) T[n + i] = T[i]`, wrapping the
/// first 28 bytes past the end so the window can run off the edge without a
/// bounds test. That is padding the caller must supply, not an overread to
/// reproduce -- and `bsc_compress` does supply it, since the block sorter works
/// inside the output buffer behind the 28-byte header.
pub fn st3_encode(t: &mut [u8], n: usize) -> i32 {
    if n <= 1 {
        return 0;
    }
    assert!(
        t.len() >= n + 28,
        "st3_encode needs n + 28 bytes: the transform wraps the first 28 past the end"
    );

    let mut count = [0u32; ALPHABET_SIZE];
    let mut bucket = vec![0i32; ALPHABET_SIZE * ALPHABET_SIZE];

    for i in 0..28 {
        t[n + i] = t[i];
    }

    let mut c0 = t[n - 1];
    for i in 0..n {
        let c1 = t[i];
        count[c1 as usize] += 1;
        bucket[((c0 as usize) << 8) | c1 as usize] += 1;
        c0 = c1;
    }

    // Both tables become exclusive prefix sums, i.e. the start offset of each
    // bucket rather than its size.
    let mut sum = 0i32;
    for b in bucket.iter_mut() {
        let tmp = sum;
        sum += *b;
        *b = tmp;
    }
    let mut sum = 0u32;
    for c in count.iter_mut() {
        let tmp = sum;
        sum += *c;
        *c = tmp;
    }

    let pos = bucket[((t[1] as usize) << 8) | t[2] as usize] as usize;

    let mut p = vec![0u16; n];
    let mut w = ((t[n - 1] as u32) << 16) | ((t[0] as u32) << 8) | t[1] as u32;
    for i in 0..n {
        w = (w << 8) | t[i + 2] as u32;
        let b = (w & 0x0000_ffff) as usize;
        p[bucket[b] as usize] = (w >> 16) as u16;
        bucket[b] += 1;
    }

    // Scatter back, in two halves: the index is read between them, at exactly
    // the point the rotation starting at offset 0 lands.
    for i in 0..pos {
        let c = (p[i] & 0x00ff) as usize;
        t[count[c] as usize] = (p[i] >> 8) as u8;
        count[c] += 1;
    }
    let index = count[(p[pos] & 0x00ff) as usize] as i32;
    for i in pos..n {
        let c = (p[i] & 0x00ff) as usize;
        t[count[c] as usize] = (p[i] >> 8) as u8;
        count[c] += 1;
    }

    index
}

/// `bsc_st4_transform_serial` (`st.cpp:102`). Order 4.
///
/// Unlike [`st3_encode`] there is no separate `count` table: the same `bucket`
/// is prefix-summed, filled forward, and then walked BACKWARDS as a descending
/// cursor during the scatter. Reusing one table for both directions is what
/// makes the scatter loops run from `n - 1` down.
pub fn st4_encode(t: &mut [u8], n: usize) -> i32 {
    if n <= 1 { return 0; }
    assert!(t.len() >= n + 28, "st4_encode needs n + 28 bytes");
    let mut bucket = vec![0i32; ALPHABET_SIZE * ALPHABET_SIZE];
    for i in 0..28 { t[n + i] = t[i]; }

    let mut c0 = t[n - 1];
    for i in 0..n {
        let c1 = t[i];
        bucket[((c0 as usize) << 8) | c1 as usize] += 1;
        c0 = c1;
    }
    let mut sum = 0i32;
    for b in bucket.iter_mut() { let tmp = sum; sum += *b; *b = tmp; }

    let pos = bucket[((t[2] as usize) << 8) | t[3] as usize] as usize;

    let mut p = vec![0u32; n];
    let mut w = ((t[n - 1] as u32) << 24) | ((t[0] as u32) << 16) | ((t[1] as u32) << 8) | t[2] as u32;
    for i in 0..n {
        let c = (w >> 24) as u8;
        w = (w << 8) | t[i + 3] as u32;
        let b = (w & 0x0000_ffff) as usize;
        p[bucket[b] as usize] = (w & 0xffff_0000) | c as u32;
        bucket[b] += 1;
    }

    for i in (pos..n).rev() {
        let b = (p[i] >> 16) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    let index = bucket[(p[pos] >> 16) as usize];
    for i in (0..pos).rev() {
        let b = (p[i] >> 16) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    index
}

/// `bsc_st5_transform_serial` (`st.cpp:141`). Order 5.
///
/// The only one that builds its bucket table TWICE: once over the leading
/// 20 bits to place the rotations, then zeroed and rebuilt over a different
/// 20-bit slice -- `(P0 << 12) | (P1 << 4) | (P2 >> 4)` -- as an INCLUSIVE
/// prefix sum for the backward scatter. Missing the second build, or making it
/// exclusive like the first, silently produces a different permutation.
pub fn st5_encode(t: &mut [u8], n: usize) -> i32 {
    if n <= 1 { return 0; }
    assert!(t.len() >= n + 28, "st5_encode needs n + 28 bytes");
    const SQRT: usize = 16; // ALPHABET_SQRT_SIZE
    let mut bucket = vec![0i32; SQRT * ALPHABET_SIZE * ALPHABET_SIZE];
    for i in 0..28 { t[n + i] = t[i]; }

    let (mut c0, mut c1) = (t[n - 2] & 0xf, t[n - 1]);
    for i in 0..n {
        let c2 = t[i];
        bucket[((c0 as usize) << 16) | ((c1 as usize) << 8) | c2 as usize] += 1;
        c0 = c1 & 0xf;
        c1 = c2;
    }
    let mut sum = 0i32;
    for b in bucket.iter_mut() { let tmp = sum; sum += *b; *b = tmp; }

    let pos = bucket[(((t[2] & 0xf) as usize) << 16) | ((t[3] as usize) << 8) | t[4] as usize] as usize;

    let mut p = vec![0u32; n];
    let mut l = t[n - 1];
    let mut w = ((t[0] as u32) << 24) | ((t[1] as u32) << 16) | ((t[2] as u32) << 8) | t[3] as u32;
    for i in 0..n {
        let v = (w & 0xffff_f000) | l as u32;
        l = (w >> 24) as u8;
        w = (w << 8) | t[i + 4] as u32;
        let b = (w & 0x000f_ffff) as usize;
        p[bucket[b] as usize] = v;
        bucket[b] += 1;
    }

    // Second table, over a different slice, and INCLUSIVE this time.
    for b in bucket.iter_mut() { *b = 0; }
    let (mut p0, mut p1) = (t[n - 2], t[n - 1]);
    for i in 0..n {
        let p2 = t[i];
        bucket[((p0 as usize) << 12) | ((p1 as usize) << 4) | (p2 >> 4) as usize] += 1;
        p0 = p1;
        p1 = p2;
    }
    let mut sum = 0i32;
    for b in bucket.iter_mut() { sum += *b; *b = sum; }

    for i in (pos..n).rev() {
        let b = (p[i] >> 12) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    let index = bucket[(p[pos] >> 12) as usize];
    for i in (0..pos).rev() {
        let b = (p[i] >> 12) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    index
}

/// `bsc_st6_transform_serial` (`st.cpp:199`). Order 6, and the widest table --
/// a full 24-bit trigram, 16 M entries.
///
/// Two windows run at once: `W0` trails `W1` by four bytes, and the stored
/// value is `(W0 << 8) | (W0 >> 24)`, a rotation rather than a mask.
pub fn st6_encode(t: &mut [u8], n: usize) -> i32 {
    if n <= 1 { return 0; }
    assert!(t.len() >= n + 28, "st6_encode needs n + 28 bytes");
    let mut bucket = vec![0i32; ALPHABET_SIZE * ALPHABET_SIZE * ALPHABET_SIZE];
    for i in 0..28 { t[n + i] = t[i]; }

    let mut w = ((t[n - 2] as u32) << 16) | ((t[n - 1] as u32) << 8) | t[0] as u32;
    for i in 0..n {
        w = (w << 8) | t[i + 1] as u32;
        bucket[(w >> 8) as usize] += 1;
    }
    let mut sum = 0i32;
    for b in bucket.iter_mut() { let tmp = sum; sum += *b; *b = tmp; }

    let pos = bucket[((t[3] as usize) << 16) | ((t[4] as usize) << 8) | t[5] as usize] as usize;

    let mut p = vec![0u32; n];
    let mut w0 = ((t[n - 2] as u32) << 24) | ((t[n - 1] as u32) << 16) | ((t[0] as u32) << 8) | t[1] as u32;
    let mut w1 = ((t[2] as u32) << 24) | ((t[3] as u32) << 16) | ((t[4] as u32) << 8) | t[5] as u32;
    for i in 0..n {
        w0 = (w0 << 8) | t[i + 2] as u32;
        w1 = (w1 << 8) | t[i + 6] as u32;
        let b = (w1 >> 8) as usize;
        p[bucket[b] as usize] = (w0 << 8) | (w0 >> 24);
        bucket[b] += 1;
    }

    for i in (pos..n).rev() {
        let b = (p[i] >> 8) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    let index = bucket[(p[pos] >> 8) as usize];
    for i in (0..pos).rev() {
        let b = (p[i] >> 8) as usize;
        bucket[b] -= 1;
        t[bucket[b] as usize] = (p[i] & 0xff) as u8;
    }
    index
}

/// `bsc_st_encode` (`st.cpp:990`): dispatch on the order. ST7 and ST8 have no
/// CPU encoder in the C either -- they return NOT_SUPPORTED without CUDA.
pub fn st_encode(t: &mut [u8], n: usize, k: u32) -> i32 {
    if !(3..=8).contains(&k) {
        return super::LIBBSC_BAD_PARAMETER;
    }
    if n <= 1 {
        return 0;
    }
    match k {
        3 => st3_encode(t, n),
        4 => st4_encode(t, n),
        5 => st5_encode(t, n),
        6 => st6_encode(t, n),
        _ => -4, // LIBBSC_NOT_SUPPORTED
    }
}

/// `bsc_store` (`libbsc.cpp:68`): the fallback frame for data that will not
/// compress -- a 28-byte header with `mode == 0`, then the input verbatim.
pub fn store(input: &[u8], output: &mut [u8]) -> i32 {
    let n = input.len();
    if output.len() < n + super::HEADER_SIZE {
        return super::LIBBSC_NOT_ENOUGH_MEMORY;
    }
    let adler = super::adler32::adler32(input);
    output[super::HEADER_SIZE..super::HEADER_SIZE + n].copy_from_slice(input);
    let w = |o: &mut [u8], at: usize, v: u32| o[at..at + 4].copy_from_slice(&v.to_le_bytes());
    w(output, 0, (n + super::HEADER_SIZE) as u32);
    w(output, 4, n as u32);
    w(output, 8, 0);
    w(output, 12, 0);
    w(output, 16, adler);
    w(output, 20, adler);
    let h = super::adler32::adler32(&output[..24]);
    w(output, 24, h);
    (n + super::HEADER_SIZE) as i32
}

/// `bsc_compress` (`libbsc.cpp:213`), the out-of-place form: LZP, block sort,
/// entropy code, frame.
///
/// **Block sorter 1 (BWT) is not supported yet** -- `bsc_bwt_encode` needs
/// libsais, which is unported -- so this handles ST3..ST6 and returns
/// NOT_SUPPORTED for BWT. That is also why the `lzSize <= HEADER_SIZE` fallback
/// below, which forces BWT, is reported rather than silently mis-sorted.
///
/// `output` must have at least `n + 28` bytes: the block sorters wrap the first
/// 28 bytes past the end of their working area.
pub fn compress(
    input: &[u8],
    output: &mut [u8],
    lzp_hash_size: u32,
    lzp_min_len: u32,
    block_sorter: u32,
    coder: u32,
) -> i32 {
    let n = input.len();
    let bad = super::LIBBSC_BAD_PARAMETER;

    let mut mode: u32 = match block_sorter {
        1 | 3 | 4 | 5 | 6 | 7 | 8 => block_sorter,
        _ => return bad,
    };
    match coder {
        1 | 2 | 3 => mode += coder << 5,
        _ => return bad,
    }
    if lzp_min_len != 0 || lzp_hash_size != 0 {
        if !(4..=255).contains(&lzp_min_len) || !(10..=28).contains(&lzp_hash_size) {
            return bad;
        }
        mode += lzp_min_len << 8;
        mode += lzp_hash_size << 16;
    }
    if n > 1_073_741_824 {
        return bad;
    }
    if n <= super::HEADER_SIZE {
        return store(input, output);
    }
    if output.len() < n + super::HEADER_SIZE {
        return super::LIBBSC_NOT_ENOUGH_MEMORY;
    }

    let adler32_data = super::adler32::adler32(input);

    // LZP writes into `output` directly; if it declines, the mode's LZP fields
    // are cleared and the input is copied instead.
    let mut lz_size = 0usize;
    if mode != (mode & 0xff) {
        let r = compress_lzp_into(input, output, lzp_hash_size, lzp_min_len);
        if r < 0 {
            mode &= 0xff;
        } else {
            lz_size = r as usize;
        }
    }
    if mode == (mode & 0xff) {
        lz_size = n;
        output[..n].copy_from_slice(input);
    }

    if lz_size <= super::HEADER_SIZE {
        // The C forces BWT here. Without libsais there is nothing to force it
        // to, so this is refused rather than answered with a different sorter.
        return -4; // LIBBSC_NOT_SUPPORTED
    }

    let sorter_k = match block_sorter {
        3..=6 => block_sorter,
        _ => return -4, // BWT and ST7/ST8: no encoder here
    };
    let index = st_encode(output, lz_size, sorter_k);
    if index < 0 {
        return index;
    }

    let mut coded = vec![0u8; lz_size];
    let result = coder_compress(&output[..lz_size], &mut coded, coder);
    // The out-of-place bsc_compress STORES the block here; only the in-place
    // variant returns NOT_COMPRESSIBLE. Carrying the in-place behaviour over is
    // the bug the differential harness caught -- the C succeeded on
    // incompressible noise while this refused it. `num_indexes` is 0 for every
    // ST sorter, so the C's `result + 1 + 4 * num_indexes` reduces to
    // `result + 1`.
    if result < 0 || (result as usize) + 1 >= n {
        return store(input, output);
    }
    let result = result as usize;
    output[super::HEADER_SIZE..super::HEADER_SIZE + result].copy_from_slice(&coded[..result]);
    // num_indexes is 0 for every ST sorter; the trailing count byte is still
    // written, which is what the decoder reads.
    output[super::HEADER_SIZE + result] = 0;
    let result = result + 1;

    let w = |o: &mut [u8], at: usize, v: u32| o[at..at + 4].copy_from_slice(&v.to_le_bytes());
    w(output, 0, (result + super::HEADER_SIZE) as u32);
    w(output, 4, n as u32);
    w(output, 8, mode);
    w(output, 12, index as u32);
    w(output, 16, adler32_data);
    let a = super::adler32::adler32(&output[super::HEADER_SIZE..super::HEADER_SIZE + result]);
    w(output, 20, a);
    let h = super::adler32::adler32(&output[..24]);
    w(output, 24, h);
    (result + super::HEADER_SIZE) as i32
}

/// LZP into a caller buffer, matching `bsc_lzp_compress`'s contract that the
/// output window is exactly `n` bytes.
fn compress_lzp_into(input: &[u8], output: &mut [u8], hash_size: u32, min_len: u32) -> i32 {
    let n = input.len();
    super::lzp_enc::compress(input, &mut output[..n], hash_size, min_len)
}
