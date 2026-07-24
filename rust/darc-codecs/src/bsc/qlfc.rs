//! The QLFC static decoder, ported from
//! `Compression/BSC/libbsc/coder/qlfc/qlfc.cpp` (`QLFC_STATIC_DECODE_FUNCTION_NAME`
//! :1747 -- the body compiled once per CPU feature level).
//!
//! QLFC ("quantised local frequency coding") replaces each byte with its rank
//! in a move-to-front list plus a run length, and codes both through the range
//! coder against the mixed predictors in [`super::model`].
//!
//! A block is:
//!
//! 1. a 32-bit `n`, the decoded length;
//! 2. an **alphabet preamble** that reconstructs the initial MTF table by
//!    decoding each distinct character bit by bit, stopping when a character
//!    repeats -- that repeat also sets `maxRank`;
//! 3. `n` bytes' worth of (rank, run length) pairs.
//!
//! Rank and run are each coded as a unary exponent followed by a mantissa, with
//! a separate escape path for rank when the running average rank is high.
//!
//! ## Index expressions are taken from the source, never inferred
//!
//! The C walks flat model arrays by pointer -- `statePredictor++` steps along a
//! row, `statePredictor[rank]` indexes within one -- so each of those maps to a
//! (row, offset) pair here. Getting one wrong silently decodes a different
//! stream, and this port has already produced three bugs of exactly that kind
//! by inferring rather than reading. Two that are easy to get wrong:
//!
//! * The run mantissa's context does **not** simply double. The C writes
//!   `int ctx = context + context + b; context++; if (bitRunSize <= 5) { context = ctx; }`
//!   -- so for `bitRunSize > 5` the context merely increments, and only for
//!   short runs does it accumulate the decoded bits.
//! * `rankHistory` and `runHistory` are updated by different formulas on each
//!   branch, including `(runHistory + 3 * bitRunSize + 3) >> 2` on a match and
//!   `(runHistory + 2) >> 2` on a miss.
//!
//! ## Not yet verified
//!
//! This is transcribed and compiles; nothing has compared it to the C on a
//! single byte. The differential harness is the only thing that can, and on
//! this port's record it is where the errors surface.

use super::model::{model_rank_state, model_run_state, QlfcModel1, ALPHABET_SIZE};
use super::model_consts::*;
use super::predictor::ProbabilityCounter;
use super::rangecoder::RangeDecoder;
use super::LIBBSC_DATA_CORRUPT;

/// `bsc_bit_scan_reverse(x)` = `clz(x) ^ 31`, i.e. the index of the highest set
/// bit. Undefined for 0 in the C (`__builtin_clz(0)`); callers never pass 0.
#[inline]
fn bit_scan_reverse(x: u32) -> u32 {
    if x == 0 {
        0
    } else {
        31 - x.leading_zeros()
    }
}

/// How far past its input the range decoder may read before the block is
/// treated as truncated. The coder legitimately reads a little past the end
/// while flushing.
const MAX_OVERRUN: usize = 64;

/// `bsc_qlfc_static_decode`. Returns the number of bytes written.
pub fn static_decode(input: &[u8], output: &mut [u8]) -> Result<usize, i32> {
    let mut model = QlfcModel1::new();
    let mut coder = RangeDecoder::new(input);

    let mut mtf = [0u8; ALPHABET_SIZE];

    let mut context_rank0 = 0usize;
    let mut context_rank4 = 0usize;
    let mut context_run = 0usize;
    let mut max_rank = 7i32;
    let mut avg_rank = 0i32;

    let mut rank_history = [0u8; ALPHABET_SIZE];
    let mut run_history = [0u8; ALPHABET_SIZE];

    let n = coder.decode_word() as usize;
    if n > output.len() {
        return Err(LIBBSC_DATA_CORRUPT);
    }

    // --- Alphabet preamble: rebuild the initial MTF table. --------------------
    let mut used_char = [0u8; ALPHABET_SIZE];
    let mut prev_char: i32 = -1;
    for rank in 0..ALPHABET_SIZE {
        let mut current_char: i32 = 0;
        for bit in (0..8).rev() {
            // Decide whether this bit is forced: scan the characters still
            // reachable under the prefix decoded so far.
            let (mut bit0, mut bit1) = (false, false);
            for c in 0..ALPHABET_SIZE as i32 {
                if c == prev_char || used_char[c as usize] == 0 {
                    if current_char == (c >> (bit + 1)) {
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
                current_char += current_char + coder.decode_bit() as i32;
            } else if bit0 {
                current_char += current_char;
            } else if bit1 {
                current_char += current_char + 1;
            }
        }

        mtf[rank] = current_char as u8;

        if current_char == prev_char {
            // A repeat ends the alphabet; the rank it repeated at fixes maxRank.
            max_rank = bit_scan_reverse(rank as u32 - 1) as i32;
            break;
        }
        prev_char = current_char;
        used_char[current_char as usize] = 1;
    }
    // maxRank indexes Rank.Mantissa[8] and drives the escape loop; a corrupt
    // preamble must not push it out of range.
    if !(0..=7).contains(&max_rank) {
        return Err(LIBBSC_DATA_CORRUPT);
    }

    // --- Main loop: (rank, run length) pairs. ---------------------------------
    let mut i = 0usize;
    while i < n {
        if coder.overrun() > MAX_OVERRUN {
            return Err(LIBBSC_DATA_CORRUPT);
        }

        let current_char = mtf[0] as usize;
        let history = rank_history[current_char] as usize;
        let state = model_rank_state(context_rank4, context_run, history);

        let mut rank: i32 = 1;
        if avg_rank < 32 {
            let p = mix3(
                model.rank.char_model[current_char] as i32,
                model.rank.state_model[state] as i32,
                model.rank.static_model as i32,
                F_RANK_TM_LR0, F_RANK_TM_LR1, F_RANK_TM_LR2,
            );
            if coder.decode_bit_p(p) != 0 {
                ProbabilityCounter::update_bit1(&mut model.rank.state_model[state], F_RANK_TS_TH1, F_RANK_TS_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.char_model[current_char], F_RANK_TC_TH1, F_RANK_TC_AR1);
                ProbabilityCounter::update_bit1(&mut model.rank.static_model, F_RANK_TP_TH1, F_RANK_TP_AR1);

                // Exponent: unary, walking along the 8-wide rows.
                let mut bit_rank_size: usize = 1;
                let mut k: usize = 0; // the pointer offset the C advances
                loop {
                    if bit_rank_size as i32 == max_rank {
                        break;
                    }
                    let p = mix3(
                        model.rank.exponent.char_model[current_char][k] as i32,
                        model.rank.exponent.state_model[state][k] as i32,
                        model.rank.exponent.static_model[k] as i32,
                        F_RANK_EM_LR0, F_RANK_EM_LR1, F_RANK_EM_LR2,
                    );
                    if coder.decode_bit_p(p) != 0 {
                        ProbabilityCounter::update_bit1(&mut model.rank.exponent.state_model[state][k], F_RANK_ES_TH1, F_RANK_ES_AR1);
                        ProbabilityCounter::update_bit1(&mut model.rank.exponent.char_model[current_char][k], F_RANK_EC_TH1, F_RANK_EC_AR1);
                        ProbabilityCounter::update_bit1(&mut model.rank.exponent.static_model[k], F_RANK_EP_TH1, F_RANK_EP_AR1);
                        k += 1;
                        bit_rank_size += 1;
                        if k >= 8 || bit_rank_size > 7 {
                            return Err(LIBBSC_DATA_CORRUPT);
                        }
                    } else {
                        ProbabilityCounter::update_bit0(&mut model.rank.exponent.state_model[state][k], F_RANK_ES_TH0, F_RANK_ES_AR0);
                        ProbabilityCounter::update_bit0(&mut model.rank.exponent.char_model[current_char][k], F_RANK_EC_TH0, F_RANK_EC_AR0);
                        ProbabilityCounter::update_bit0(&mut model.rank.exponent.static_model[k], F_RANK_EP_TH0, F_RANK_EP_AR0);
                        break;
                    }
                }

                rank_history[current_char] = bit_rank_size as u8;

                // Mantissa: bit_rank_size bits, indexed by the accumulating rank.
                let m = &mut model.rank.mantissa[bit_rank_size];
                for _ in (0..bit_rank_size).rev() {
                    let idx = rank as usize;
                    if idx >= ALPHABET_SIZE {
                        return Err(LIBBSC_DATA_CORRUPT);
                    }
                    let p = mix3(
                        m.char_model[current_char][idx] as i32,
                        m.state_model[state][idx] as i32,
                        m.static_model[idx] as i32,
                        F_RANK_MM_LR0, F_RANK_MM_LR1, F_RANK_MM_LR2,
                    );
                    let b = coder.decode_bit_p(p);
                    ProbabilityCounter::update_bit(b, &mut m.state_model[state][idx], F_RANK_MS_TH0, F_RANK_MS_AR0, F_RANK_MS_TH1, F_RANK_MS_AR1);
                    ProbabilityCounter::update_bit(b, &mut m.char_model[current_char][idx], F_RANK_MC_TH0, F_RANK_MC_AR0, F_RANK_MC_TH1, F_RANK_MC_AR1);
                    ProbabilityCounter::update_bit(b, &mut m.static_model[idx], F_RANK_MP_TH0, F_RANK_MP_AR0, F_RANK_MP_TH1, F_RANK_MP_AR1);
                    rank += rank + b as i32;
                }
            } else {
                rank_history[current_char] = 0;
                ProbabilityCounter::update_bit0(&mut model.rank.state_model[state], F_RANK_TS_TH0, F_RANK_TS_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.char_model[current_char], F_RANK_TC_TH0, F_RANK_TC_AR0);
                ProbabilityCounter::update_bit0(&mut model.rank.static_model, F_RANK_TP_TH0, F_RANK_TP_AR0);
            }
        } else {
            // Escape: a flat binary rank over maxRank+1 bits.
            rank = 0;
            let mut context: usize = 1;
            for _ in (0..=max_rank).rev() {
                if context >= ALPHABET_SIZE {
                    return Err(LIBBSC_DATA_CORRUPT);
                }
                let p = mix3(
                    model.rank.escape.char_model[current_char][context] as i32,
                    model.rank.escape.state_model[state][context] as i32,
                    model.rank.escape.static_model[context] as i32,
                    F_RANK_PM_LR0, F_RANK_PM_LR1, F_RANK_PM_LR2,
                );
                let b = coder.decode_bit_p(p);
                ProbabilityCounter::update_bit(b, &mut model.rank.escape.state_model[state][context], F_RANK_PS_TH0, F_RANK_PS_AR0, F_RANK_PS_TH1, F_RANK_PS_AR1);
                ProbabilityCounter::update_bit(b, &mut model.rank.escape.char_model[current_char][context], F_RANK_PC_TH0, F_RANK_PC_AR0, F_RANK_PC_TH1, F_RANK_PC_AR1);
                ProbabilityCounter::update_bit(b, &mut model.rank.escape.static_model[context], F_RANK_PP_TH0, F_RANK_PP_AR0, F_RANK_PP_TH1, F_RANK_PP_AR1);
                context += context + b as usize;
                rank += rank + b as i32;
            }
            rank_history[current_char] = bit_scan_reverse(rank as u32) as u8;
        }

        // Move-to-front: slide [0..rank) down and place the character at `rank`.
        let r = rank as usize;
        if r >= ALPHABET_SIZE {
            return Err(LIBBSC_DATA_CORRUPT);
        }
        for j in 0..r {
            mtf[j] = mtf[j + 1];
        }
        mtf[r] = current_char as u8;

        avg_rank = (avg_rank * 124 + rank * 4) >> 7;
        rank -= 1;

        // --- Run length -------------------------------------------------------
        let history = run_history[current_char] as usize;
        let state = model_run_state(context_rank0, context_run, rank.max(0) as usize, history);

        let p = mix3(
            model.run.char_model[current_char] as i32,
            model.run.state_model[state] as i32,
            model.run.static_model as i32,
            F_RUN_TM_LR0, F_RUN_TM_LR1, F_RUN_TM_LR2,
        );
        if coder.decode_bit_p(p) != 0 {
            ProbabilityCounter::update_bit1(&mut model.run.state_model[state], F_RUN_TS_TH1, F_RUN_TS_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.char_model[current_char], F_RUN_TC_TH1, F_RUN_TC_AR1);
            ProbabilityCounter::update_bit1(&mut model.run.static_model, F_RUN_TP_TH1, F_RUN_TP_AR1);

            let mut run_size: i64 = 1;
            let mut bit_run_size: usize = 1;
            let mut k: usize = 0;
            loop {
                if k >= 32 {
                    return Err(LIBBSC_DATA_CORRUPT);
                }
                let p = mix3(
                    model.run.exponent.char_model[current_char][k] as i32,
                    model.run.exponent.state_model[state][k] as i32,
                    model.run.exponent.static_model[k] as i32,
                    F_RUN_EM_LR0, F_RUN_EM_LR1, F_RUN_EM_LR2,
                );
                if coder.decode_bit_p(p) != 0 {
                    ProbabilityCounter::update_bit1(&mut model.run.exponent.state_model[state][k], F_RUN_ES_TH1, F_RUN_ES_AR1);
                    ProbabilityCounter::update_bit1(&mut model.run.exponent.char_model[current_char][k], F_RUN_EC_TH1, F_RUN_EC_AR1);
                    ProbabilityCounter::update_bit1(&mut model.run.exponent.static_model[k], F_RUN_EP_TH1, F_RUN_EP_AR1);
                    k += 1;
                    bit_run_size += 1;
                    if bit_run_size >= 32 {
                        return Err(LIBBSC_DATA_CORRUPT);
                    }
                } else {
                    ProbabilityCounter::update_bit0(&mut model.run.exponent.state_model[state][k], F_RUN_ES_TH0, F_RUN_ES_AR0);
                    ProbabilityCounter::update_bit0(&mut model.run.exponent.char_model[current_char][k], F_RUN_EC_TH0, F_RUN_EC_AR0);
                    ProbabilityCounter::update_bit0(&mut model.run.exponent.static_model[k], F_RUN_EP_TH0, F_RUN_EP_AR0);
                    break;
                }
            }

            run_history[current_char] =
                ((run_history[current_char] as i32 + 3 * bit_run_size as i32 + 3) >> 2) as u8;

            let m = &mut model.run.mantissa[bit_run_size];
            let mut context: usize = 1;
            for _ in (0..bit_run_size).rev() {
                if context >= 32 {
                    return Err(LIBBSC_DATA_CORRUPT);
                }
                let p = mix3(
                    m.char_model[current_char][context] as i32,
                    m.state_model[state][context] as i32,
                    m.static_model[context] as i32,
                    F_RUN_MM_LR0, F_RUN_MM_LR1, F_RUN_MM_LR2,
                );
                let b = coder.decode_bit_p(p);
                ProbabilityCounter::update_bit(b, &mut m.state_model[state][context], F_RUN_MS_TH0, F_RUN_MS_AR0, F_RUN_MS_TH1, F_RUN_MS_AR1);
                ProbabilityCounter::update_bit(b, &mut m.char_model[current_char][context], F_RUN_MC_TH0, F_RUN_MC_AR0, F_RUN_MC_TH1, F_RUN_MC_AR1);
                ProbabilityCounter::update_bit(b, &mut m.static_model[context], F_RUN_MP_TH0, F_RUN_MP_AR0, F_RUN_MP_TH1, F_RUN_MP_AR1);

                run_size += run_size + b as i64;
                // NOT a plain doubling: the C computes both and picks by
                // bitRunSize. For runs longer than 5 bits the context only
                // increments; only short runs accumulate the decoded bits.
                let ctx = context + context + b as usize;
                context += 1;
                if bit_run_size <= 5 {
                    context = ctx;
                }
            }

            context_rank0 = ((context_rank0 << 1) | usize::from(rank == 0)) & 0x7;
            context_rank4 = ((context_rank4 << 2) | (rank.clamp(0, 3) as usize)) & 0xff;
            context_run = ((context_run << 1) | usize::from(run_size < 3)) & 0xf;

            if run_size < 0 || i as i64 + run_size > n as i64 {
                return Err(LIBBSC_DATA_CORRUPT);
            }
            for _ in 0..run_size {
                output[i] = current_char as u8;
                i += 1;
            }
        } else {
            run_history[current_char] = ((run_history[current_char] as i32 + 2) >> 2) as u8;
            ProbabilityCounter::update_bit0(&mut model.run.state_model[state], F_RUN_TS_TH0, F_RUN_TS_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.char_model[current_char], F_RUN_TC_TH0, F_RUN_TC_AR0);
            ProbabilityCounter::update_bit0(&mut model.run.static_model, F_RUN_TP_TH0, F_RUN_TP_AR0);

            context_rank0 = ((context_rank0 << 1) | usize::from(rank == 0)) & 0x7;
            context_rank4 = ((context_rank4 << 2) | (rank.clamp(0, 3) as usize)) & 0xff;
            context_run = ((context_run << 1) | 1) & 0xf;

            if i >= n {
                return Err(LIBBSC_DATA_CORRUPT);
            }
            output[i] = current_char as u8;
            i += 1;
        }
    }

    Ok(n)
}

/// The three-way predictor blend the static coder uses:
/// `(char * LR0 + state * LR1 + static * LR2) >> 5`.
///
/// This is NOT the `ProbabilityMixer`. The static coder blends with fixed
/// weights and a shift; the adaptive coder uses the learned mixer. That is the
/// whole difference between the two coders.
///
/// **Deliberately unclamped**, matching the C. Every weight triple sums to
/// exactly 32 and the shift is `>> 5`, so this is a weighted *average* of three
/// 12-bit probabilities and lands in 0..=4096 by construction -- see
/// `static_coder_weights_sum_to_32`. Clamping would look defensive but would
/// silently diverge from the C if a counter ever drifted out of range, turning
/// a detectable disagreement into wrong output. The arithmetic wraps like the
/// C's `int`.
#[inline]
fn mix3(char_p: i32, state_p: i32, static_p: i32, lr0: i32, lr1: i32, lr2: i32) -> u32 {
    (char_p
        .wrapping_mul(lr0)
        .wrapping_add(state_p.wrapping_mul(lr1))
        .wrapping_add(static_p.wrapping_mul(lr2))
        >> 5) as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The invariant that makes `mix3` safe without a clamp: each weight triple
    /// sums to 32, so `>> 5` averages rather than scales. If a future libbsc
    /// retunes these and breaks the sum, mix3 could leave the 12-bit range and
    /// this test is what says so.
    #[test]
    fn static_coder_weights_sum_to_32() {
        let triples: [(&str, [i32; 3]); 7] = [
            ("F_RANK_TM", [F_RANK_TM_LR0, F_RANK_TM_LR1, F_RANK_TM_LR2]),
            ("F_RANK_EM", [F_RANK_EM_LR0, F_RANK_EM_LR1, F_RANK_EM_LR2]),
            ("F_RANK_MM", [F_RANK_MM_LR0, F_RANK_MM_LR1, F_RANK_MM_LR2]),
            ("F_RANK_PM", [F_RANK_PM_LR0, F_RANK_PM_LR1, F_RANK_PM_LR2]),
            ("F_RUN_TM", [F_RUN_TM_LR0, F_RUN_TM_LR1, F_RUN_TM_LR2]),
            ("F_RUN_EM", [F_RUN_EM_LR0, F_RUN_EM_LR1, F_RUN_EM_LR2]),
            ("F_RUN_MM", [F_RUN_MM_LR0, F_RUN_MM_LR1, F_RUN_MM_LR2]),
        ];
        for (name, t) in triples {
            assert_eq!(t.iter().sum::<i32>(), 32, "{name} weights must average, not scale");
        }
    }

    /// With all three predictors at the 12-bit extremes, the blend stays inside
    /// the probability range -- which is what lets the clamp be absent.
    #[test]
    fn the_blend_stays_in_the_probability_range() {
        for p in [0i32, 1, 2048, 4095, 4096] {
            let v = mix3(p, p, p, F_RANK_TM_LR0, F_RANK_TM_LR1, F_RANK_TM_LR2);
            assert!(v <= 4096, "blend of {p} gave {v}");
        }
        // Mixed extremes too.
        let v = mix3(4096, 0, 4096, F_RANK_MM_LR0, F_RANK_MM_LR1, F_RANK_MM_LR2);
        assert!(v <= 4096);
    }

    #[test]
    fn bit_scan_reverse_matches_the_c_builtin() {
        // clz(x) ^ 31 == index of the highest set bit.
        for x in [1u32, 2, 3, 4, 7, 8, 255, 256, 1 << 30] {
            assert_eq!(bit_scan_reverse(x), 31 - x.leading_zeros());
        }
    }
}
