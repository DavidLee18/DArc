//! QLFC's statistical model, ported from
//! `Compression/BSC/libbsc/coder/qlfc/qlfc_model.h` and its initialiser in
//! `qlfc_model.cpp`.
//!
//! Every probability the coder uses lives here. Both sides build this
//! identically at the start of a block and adapt it in lockstep as they code,
//! so the *initial* values are as much a part of the format as the tables --
//! `bsc_qlfc_init_static_model` sets every Rank and Run probability to 2048
//! (one half in 12-bit fixed point) and calls `Init()` on every mixer.
//!
//! ## Shape
//!
//! A symbol is coded as a *rank* (its position in a move-to-front list) and
//! then a *run length*. Each of those is coded as a small exponent-mantissa
//! pair rather than a flat number, so the model has, for both Rank and Run:
//!
//! * a single `static_model` probability,
//! * `state_model[256]` keyed on a context state,
//! * `char_model[256]` keyed on the current character,
//! * and the same triple again for the exponent, the mantissa (one set per
//!   exponent value), and -- for Rank only -- an escape.
//!
//! Those three predictors are what the mixer blends. The C reaches them as
//! `&model->Rank.StateModel[state]` and indexes *past* that pointer, so the
//! layout is flat and contiguous; this uses explicit arrays and indices, which
//! is equivalent as long as every index matches.
//!
//! `QlfcStatisticalModel2` (the "fast" coder) is a smaller, separate model and
//! is not ported yet -- only the static and adaptive coders share Model1.

use super::predictor::ProbabilityMixer;

pub const ALPHABET_SIZE: usize = 256;

/// The half-probability every counter starts at (12-bit scale).
const INITIAL_PROBABILITY: i16 = 2048;

/// One predictor triple: static, per-state, per-character.
#[derive(Clone)]
pub struct RankExponent {
    pub static_model: [i16; 8],
    pub state_model: [[i16; 8]; ALPHABET_SIZE],
    pub char_model: [[i16; 8]; ALPHABET_SIZE],
}

#[derive(Clone)]
pub struct RankMantissa {
    pub static_model: [i16; ALPHABET_SIZE],
    pub state_model: Vec<[i16; ALPHABET_SIZE]>,
    pub char_model: Vec<[i16; ALPHABET_SIZE]>,
}

#[derive(Clone)]
pub struct RankEscape {
    pub static_model: [i16; ALPHABET_SIZE],
    pub state_model: Vec<[i16; ALPHABET_SIZE]>,
    pub char_model: Vec<[i16; ALPHABET_SIZE]>,
}

#[derive(Clone)]
pub struct RankModel {
    pub static_model: i16,
    pub state_model: [i16; ALPHABET_SIZE],
    pub char_model: [i16; ALPHABET_SIZE],
    pub exponent: RankExponent,
    /// One mantissa model per exponent value.
    pub mantissa: Vec<RankMantissa>,
    pub escape: RankEscape,
}

#[derive(Clone)]
pub struct RunExponent {
    pub static_model: [i16; 32],
    pub state_model: [[i16; 32]; ALPHABET_SIZE],
    pub char_model: [[i16; 32]; ALPHABET_SIZE],
}

#[derive(Clone)]
pub struct RunMantissa {
    pub static_model: [i16; 32],
    pub state_model: [[i16; 32]; ALPHABET_SIZE],
    pub char_model: [[i16; 32]; ALPHABET_SIZE],
}

#[derive(Clone)]
pub struct RunModel {
    pub static_model: i16,
    pub state_model: [i16; ALPHABET_SIZE],
    pub char_model: [i16; ALPHABET_SIZE],
    pub exponent: RunExponent,
    pub mantissa: Vec<RunMantissa>,
}

/// `QlfcStatisticalModel1` -- used by the static and adaptive coders.
pub struct QlfcModel1 {
    pub rank: RankModel,
    pub run: RunModel,
    pub mixer_of_rank: Vec<ProbabilityMixer>,
    pub mixer_of_rank_exponent: Vec<Vec<ProbabilityMixer>>,
    pub mixer_of_rank_mantissa: Vec<ProbabilityMixer>,
    pub mixer_of_rank_escape: Vec<ProbabilityMixer>,
    pub mixer_of_run: Vec<ProbabilityMixer>,
    pub mixer_of_run_exponent: Vec<Vec<ProbabilityMixer>>,
    pub mixer_of_run_mantissa: Vec<ProbabilityMixer>,
}

impl QlfcModel1 {
    /// `bsc_qlfc_init_model`: every counter to 2048, every mixer to its Init
    /// state. The C memcpys a prebuilt global; building it directly is the same
    /// thing, since that global is itself constructed this way once at startup.
    pub fn new() -> Self {
        let p = INITIAL_PROBABILITY;
        QlfcModel1 {
            rank: RankModel {
                static_model: p,
                state_model: [p; ALPHABET_SIZE],
                char_model: [p; ALPHABET_SIZE],
                exponent: RankExponent {
                    static_model: [p; 8],
                    state_model: [[p; 8]; ALPHABET_SIZE],
                    char_model: [[p; 8]; ALPHABET_SIZE],
                },
                mantissa: (0..8)
                    .map(|_| RankMantissa {
                        static_model: [p; ALPHABET_SIZE],
                        state_model: vec![[p; ALPHABET_SIZE]; ALPHABET_SIZE],
                        char_model: vec![[p; ALPHABET_SIZE]; ALPHABET_SIZE],
                    })
                    .collect(),
                escape: RankEscape {
                    static_model: [p; ALPHABET_SIZE],
                    state_model: vec![[p; ALPHABET_SIZE]; ALPHABET_SIZE],
                    char_model: vec![[p; ALPHABET_SIZE]; ALPHABET_SIZE],
                },
            },
            run: RunModel {
                static_model: p,
                state_model: [p; ALPHABET_SIZE],
                char_model: [p; ALPHABET_SIZE],
                exponent: RunExponent {
                    static_model: [p; 32],
                    state_model: [[p; 32]; ALPHABET_SIZE],
                    char_model: [[p; 32]; ALPHABET_SIZE],
                },
                mantissa: (0..32)
                    .map(|_| RunMantissa {
                        static_model: [p; 32],
                        state_model: [[p; 32]; ALPHABET_SIZE],
                        char_model: [[p; 32]; ALPHABET_SIZE],
                    })
                    .collect(),
            },
            mixer_of_rank: vec![ProbabilityMixer::default(); ALPHABET_SIZE],
            mixer_of_rank_exponent: vec![vec![ProbabilityMixer::default(); 8]; 8],
            mixer_of_rank_mantissa: vec![ProbabilityMixer::default(); 8],
            mixer_of_rank_escape: vec![ProbabilityMixer::default(); ALPHABET_SIZE],
            mixer_of_run: vec![ProbabilityMixer::default(); ALPHABET_SIZE],
            mixer_of_run_exponent: vec![vec![ProbabilityMixer::default(); 32]; 32],
            mixer_of_run_mantissa: vec![ProbabilityMixer::default(); 32],
        }
    }
}

impl Default for QlfcModel1 {
    fn default() -> Self {
        Self::new()
    }
}

/// `model_rank_state(contextRank4, contextRun, rankSizeHistory)` (tables.h).
#[inline]
pub fn model_rank_state(context_rank4: usize, context_run: usize, rank_size_history: usize) -> usize {
    super::tables::MODEL_RANK_STATE_TABLE[(context_run << 11) | (context_rank4 << 3) | rank_size_history]
        as usize
}

/// `model_run_state(contextRank0, contextRun, rank, runSizeHistory)`
/// (tables.h:1863).
///
/// Four inputs, not two, and the last two are CLAMPED TO 7 before packing:
///
/// ```c
/// model_run_state_table[(contextRank0 << 10) | (contextRun << 6)
///                       | ((rank < 7 ? rank : 7) << 3)
///                       | (runSizeHistory < 7 ? runSizeHistory : 7)]
/// ```
///
/// Without the clamps an ordinary rank above 7 would run the index past the
/// 8,192-entry table. An earlier version of this function took two arguments
/// and packed them differently -- it was wrong, and only reading the C fixed
/// it.
#[inline]
pub fn model_run_state(
    context_rank0: usize,
    context_run: usize,
    rank: usize,
    run_size_history: usize,
) -> usize {
    let r = rank.min(7);
    let h = run_size_history.min(7);
    super::tables::MODEL_RUN_STATE_TABLE[(context_rank0 << 10) | (context_run << 6) | (r << 3) | h]
        as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every counter starts at exactly half. Both sides depend on this, so a
    /// different initial value desynchronises from the very first symbol.
    #[test]
    fn every_counter_starts_at_half() {
        let m = QlfcModel1::new();
        assert_eq!(m.rank.static_model, 2048);
        assert!(m.rank.state_model.iter().all(|&x| x == 2048));
        assert!(m.rank.char_model.iter().all(|&x| x == 2048));
        assert!(m.rank.exponent.static_model.iter().all(|&x| x == 2048));
        assert!(m.rank.escape.static_model.iter().all(|&x| x == 2048));
        assert_eq!(m.run.static_model, 2048);
        assert!(m.run.exponent.static_model.iter().all(|&x| x == 2048));
        for mant in &m.run.mantissa {
            assert!(mant.static_model.iter().all(|&x| x == 2048));
        }
    }

    /// The submodel counts are format: 8 rank mantissa sets (one per exponent
    /// bit) and 32 run mantissa sets.
    #[test]
    fn submodel_counts_match_the_c_layout() {
        let m = QlfcModel1::new();
        assert_eq!(m.rank.mantissa.len(), 8);
        assert_eq!(m.run.mantissa.len(), 32);
        assert_eq!(m.mixer_of_rank.len(), ALPHABET_SIZE);
        assert_eq!(m.mixer_of_rank_exponent.len(), 8);
        assert_eq!(m.mixer_of_rank_exponent[0].len(), 8);
        assert_eq!(m.mixer_of_run_exponent.len(), 32);
        assert_eq!(m.mixer_of_run_exponent[0].len(), 32);
        assert_eq!(m.mixer_of_run_mantissa.len(), 32);
    }

    /// The state tables are indexed by a packed context; the shifts must not
    /// overflow the table. rank: 8192 entries per contextRun step, 32768 total.
    #[test]
    fn state_lookups_stay_inside_their_tables() {
        // Maximum legal indices per the C's packing.
        drop(model_rank_state(255, 15, 7));
        // run: contextRank0 fills bits 10..12, so it is 3 bits wide here.
        drop(model_run_state(7, 15, 7, 7));
        // The clamps must hold for out-of-range rank/history rather than
        // indexing past the table.
        drop(model_run_state(7, 15, 255, 255));
        // And the table sizes themselves.
        assert_eq!(super::super::tables::MODEL_RANK_STATE_TABLE.len(), 32768);
        assert_eq!(super::super::tables::MODEL_RUN_STATE_TABLE.len(), 8192);
    }
}
