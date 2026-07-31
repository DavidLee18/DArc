//! Probability price table — a port of `LzmaEnc_InitPriceTables`
//! (`LzmaEnc.c:830`) and the `GET_PRICE_*` macros (`LzmaEnc.c:855-865`).
//!
//! Prices are fixed-point bit costs with [`NUM_BIT_PRICE_SHIFT_BITS`] fractional
//! bits. The optimal parser sums these to choose the cheapest encoding, so the
//! exact rounding here is part of the bit-exactness contract.

use crate::state::{
    BIT_MODEL_TOTAL, NUM_BIT_MODEL_TOTAL_BITS, NUM_BIT_PRICE_SHIFT_BITS, NUM_MOVE_REDUCING_BITS,
};

/// Number of entries in the price table: `kBitModelTotal >> kNumMoveReducingBits`
/// = 2048 >> 4 = 128.
pub const PROB_PRICE_TABLE_LEN: usize = (BIT_MODEL_TOTAL >> NUM_MOVE_REDUCING_BITS) as usize;

/// Precomputed bit-price table, indexed by `prob >> kNumMoveReducingBits`.
///
/// A direct port of `LzmaEnc_InitPriceTables` (`LzmaEnc.c:830`). In C this is
/// recomputed at every encoder init; here it is constant, so callers can build it
/// once with [`ProbPrices::new`].
pub struct ProbPrices {
    table: [u32; PROB_PRICE_TABLE_LEN],
}

impl ProbPrices {
    /// Build the table (`LzmaEnc_InitPriceTables`, `LzmaEnc.c:830-852`).
    pub fn new() -> Self {
        const K_CYCLES_BITS: u32 = NUM_BIT_PRICE_SHIFT_BITS;
        let mut table = [0u32; PROB_PRICE_TABLE_LEN];
        for (i, slot) in table.iter_mut().enumerate() {
            let mut w: u32 =
                ((i as u32) << NUM_MOVE_REDUCING_BITS) + (1 << (NUM_MOVE_REDUCING_BITS - 1));
            let mut bit_count: u32 = 0;
            for _ in 0..K_CYCLES_BITS {
                w = w.wrapping_mul(w);
                bit_count <<= 1;
                while w >= (1u32 << 16) {
                    w >>= 1;
                    bit_count += 1;
                }
            }
            *slot = (NUM_BIT_MODEL_TOTAL_BITS << K_CYCLES_BITS) - 15 - bit_count;
        }
        ProbPrices { table }
    }

    /// `GET_PRICE_0(prob)` — cost of coding a 0 bit at probability `prob`.
    #[inline]
    pub fn price_0(&self, prob: u16) -> u32 {
        self.table[(prob as u32 >> NUM_MOVE_REDUCING_BITS) as usize]
    }

    /// `GET_PRICE_1(prob)` — cost of coding a 1 bit at probability `prob`.
    #[inline]
    pub fn price_1(&self, prob: u16) -> u32 {
        self.table[((prob as u32 ^ (BIT_MODEL_TOTAL - 1)) >> NUM_MOVE_REDUCING_BITS) as usize]
    }

    /// `GET_PRICE(prob, bit)` — cost of coding `bit` at probability `prob`.
    #[inline]
    pub fn price(&self, prob: u16, bit: u32) -> u32 {
        let mask = (0u32.wrapping_sub(bit)) & (BIT_MODEL_TOTAL - 1);
        self.table[((prob as u32 ^ mask) >> NUM_MOVE_REDUCING_BITS) as usize]
    }
}

impl Default for ProbPrices {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_shape_and_anchors() {
        let p = ProbPrices::new();
        assert_eq!(p.table.len(), 128);
        // Canonical: ProbPrices[0] == 0x80 (price of a maximally-unlikely bit).
        assert_eq!(p.table[0], 0x80);
        // Prices strictly decrease as the modeled probability rises.
        assert!(p.table[127] < p.table[0]);
        assert!(p.table[127] >= 1);
    }

    #[test]
    fn price_helpers() {
        let p = ProbPrices::new();
        let max = (crate::state::BIT_MODEL_TOTAL - 1) as u16; // 2047
        for &prob in &[1u16, 32, 512, 1024, 1536, 2016, 2047] {
            // price(prob, bit) selects the same entry as the dedicated helpers.
            assert_eq!(p.price(prob, 0), p.price_0(prob));
            assert_eq!(p.price(prob, 1), p.price_1(prob));
            // The table is symmetric under bit complement: coding a 0 at `prob`
            // costs the same as coding a 1 at the complementary probability.
            assert_eq!(p.price_0(prob), p.price_1(max - prob));
        }
        // A higher modeled probability makes the matching bit cheaper to code.
        assert!(p.price_0(32) > p.price_0(2016));
    }
}
