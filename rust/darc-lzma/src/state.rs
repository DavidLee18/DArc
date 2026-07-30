//! LZMA constants, the 12-state machine, and the next-state transition tables.
//!
//! Constants are copied verbatim from `LzmaEnc.c` (see line citations). The
//! state-transition tables (`LzmaEnc.c:620-623`) select which probability arrays
//! the coder uses for the next symbol; they must be reproduced exactly.

// --- Range coder / probability model (LzmaEnc.c:44-52) ---

/// `kNumBitModelTotalBits`: probabilities are 11-bit.
pub const NUM_BIT_MODEL_TOTAL_BITS: u32 = 11;
/// `kBitModelTotal` = 1 << 11 = 2048.
pub const BIT_MODEL_TOTAL: u32 = 1 << NUM_BIT_MODEL_TOTAL_BITS;
/// `kNumMoveBits`: probability adaptation shift.
pub const NUM_MOVE_BITS: u32 = 5;
/// `kProbInitValue` = kBitModelTotal / 2: initial value of every probability.
pub const PROB_INIT_VALUE: u16 = (BIT_MODEL_TOTAL >> 1) as u16;
/// `kTopValue` = 1 << 24: range-coder renormalization threshold.
pub const TOP_VALUE: u32 = 1 << 24;

/// `kNumMoveReducingBits`: index reduction for the price table.
pub const NUM_MOVE_REDUCING_BITS: u32 = 4;
/// `kNumBitPriceShiftBits`: fixed-point fractional bits in prices.
pub const NUM_BIT_PRICE_SHIFT_BITS: u32 = 4;

// --- Match / length model (LzmaEnc.c:257-319) ---

/// `LZMA_MATCH_LEN_MIN`: shortest encodable match.
pub const MATCH_LEN_MIN: u32 = 2;

/// `kNumLenToPosStates`.
pub const NUM_LEN_TO_POS_STATES: u32 = 4;
/// `kNumPosSlotBits`.
pub const NUM_POS_SLOT_BITS: u32 = 6;
/// `kNumAlignBits`.
pub const NUM_ALIGN_BITS: u32 = 4;
/// `kStartPosModelIndex`.
pub const START_POS_MODEL_INDEX: u32 = 4;
/// `kEndPosModelIndex`.
pub const END_POS_MODEL_INDEX: u32 = 14;
/// `kNumFullDistances` = 1 << (kEndPosModelIndex >> 1).
pub const NUM_FULL_DISTANCES: u32 = 1 << (END_POS_MODEL_INDEX >> 1);

/// `kLenNumLowBits`.
pub const LEN_NUM_LOW_BITS: u32 = 3;
/// `kLenNumLowSymbols` = 1 << 3.
pub const LEN_NUM_LOW_SYMBOLS: u32 = 1 << LEN_NUM_LOW_BITS;
/// `kLenNumHighBits`.
pub const LEN_NUM_HIGH_BITS: u32 = 8;
/// `kLenNumHighSymbols` = 1 << 8.
pub const LEN_NUM_HIGH_SYMBOLS: u32 = 1 << LEN_NUM_HIGH_BITS;
/// `kLenNumSymbolsTotal` = low*2 + high.
pub const LEN_NUM_SYMBOLS_TOTAL: u32 = LEN_NUM_LOW_SYMBOLS * 2 + LEN_NUM_HIGH_SYMBOLS;
/// `LZMA_MATCH_LEN_MAX`.
pub const MATCH_LEN_MAX: u32 = MATCH_LEN_MIN + LEN_NUM_SYMBOLS_TOTAL - 1;

// --- 12-state machine (LzmaEnc.c:319, 614-627) ---

/// `kNumStates`: the LZMA state machine has 12 states.
pub const NUM_STATES: usize = 12;

/// Next state after encoding a literal (`kLiteralNextStates`).
pub const LITERAL_NEXT_STATES: [u8; NUM_STATES] = [0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5];
/// Next state after encoding a match (`kMatchNextStates`).
pub const MATCH_NEXT_STATES: [u8; NUM_STATES] = [7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10];
/// Next state after encoding a rep match (`kRepNextStates`).
pub const REP_NEXT_STATES: [u8; NUM_STATES] = [8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11];
/// Next state after encoding a short rep (`kShortRepNextStates`).
pub const SHORT_REP_NEXT_STATES: [u8; NUM_STATES] = [9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11];

// `IsLitState` (state < 7) and `GetLenToPosState` live with their callers
// (`encoder`/`optimum`), so they are not duplicated here.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derived_constants() {
        assert_eq!(BIT_MODEL_TOTAL, 2048);
        assert_eq!(PROB_INIT_VALUE, 1024);
        assert_eq!(NUM_FULL_DISTANCES, 128);
        assert_eq!(MATCH_LEN_MAX, 273); // 2 + (8*2 + 256) - 1
    }

    #[test]
    fn next_state_tables_have_12_entries() {
        for table in [
            &LITERAL_NEXT_STATES,
            &MATCH_NEXT_STATES,
            &REP_NEXT_STATES,
            &SHORT_REP_NEXT_STATES,
        ] {
            assert_eq!(table.len(), NUM_STATES);
            assert!(table.iter().all(|&s| (s as usize) < NUM_STATES));
        }
    }
}
