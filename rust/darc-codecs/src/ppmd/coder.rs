//! Dmitry Subbotin's carryless range coder, ported from
//! `Compression/PPMD/Coder.hpp`.
//!
//! The file carries the author's original class in a comment block above the
//! macro implementation PPMd actually uses. **The two disagree**, and the live
//! code is what matters: the commented original has `BOT = 1<<16`, the macros
//! below it use `1<<15`. Values here are read from the macros, not from the
//! prose above them.
//!
//! `low`, `code` and `range` are 32-bit and rely on wrapping arithmetic
//! throughout -- `-low` in the normalisation condition is a wrapping negation
//! of an unsigned, which is why every operation here is explicitly `wrapping_`.

use super::stream::PrimeStream;

pub const TOP: u32 = 1 << 24;
pub const BOT: u32 = 1 << 15;

/// The C's file-scope `SubRange`, `low`, `code` and `range`. Grouped into one
/// struct rather than left as globals, since the encoder and decoder each need
/// their own -- `C_PPMD.cpp` includes `Model.cpp` twice, in separate
/// namespaces, so the C has two independent copies of exactly this state.
#[derive(Default)]
pub struct RangeCoder {
    pub low_count: u32,
    pub high_count: u32,
    pub scale: u32,

    pub low: u32,
    pub code: u32,
    pub range: u32,
}

impl RangeCoder {
    pub fn new() -> Self {
        RangeCoder::default()
    }

    /// `ariInitEncoder`.
    pub fn init_encoder(&mut self) {
        self.low = 0;
        self.range = u32::MAX;
    }

    /// `ARI_INIT_DECODER`: prime `code` with the first four bytes.
    pub fn init_decoder(&mut self, s: &mut PrimeStream) {
        self.low = 0;
        self.code = 0;
        self.range = u32::MAX;
        for _ in 0..4 {
            self.code = (self.code << 8) | (s.get() as u32 & 0xff);
        }
    }

    /// `ARI_ENC_NORMALIZE`.
    ///
    /// The condition is one expression in the C with a side effect in its
    /// second half: `range < BOT && ((range = -low & (BOT-1)), 1)`. The
    /// assignment happens only when the first test fails and `range < BOT`
    /// holds, and it must not happen otherwise -- writing this as two separate
    /// `if`s changes when `range` is clamped.
    pub fn encode_normalize(&mut self, s: &mut PrimeStream) {
        loop {
            if (self.low ^ self.low.wrapping_add(self.range)) < TOP {
                // fall through to the emit below
            } else if self.range < BOT {
                self.range = self.low.wrapping_neg() & (BOT - 1);
            } else {
                break;
            }
            s.put((self.low >> 24) as u8);
            self.range <<= 8;
            self.low <<= 8;
        }
    }

    /// `ARI_DEC_NORMALIZE`: the same condition, reading instead of writing.
    pub fn decode_normalize(&mut self, s: &mut PrimeStream) {
        loop {
            if (self.low ^ self.low.wrapping_add(self.range)) < TOP {
            } else if self.range < BOT {
                self.range = self.low.wrapping_neg() & (BOT - 1);
            } else {
                break;
            }
            self.code = (self.code << 8) | (s.get() as u32 & 0xff);
            self.range <<= 8;
            self.low <<= 8;
        }
    }

    /// `ariEncodeSymbol`.
    pub fn encode_symbol(&mut self) {
        self.range /= self.scale;
        self.low = self
            .low
            .wrapping_add(self.low_count.wrapping_mul(self.range));
        self.range = self.range.wrapping_mul(self.high_count - self.low_count);
    }

    /// `ariShiftEncodeSymbol`: the same, with a shift in place of the divide.
    pub fn shift_encode_symbol(&mut self, shift: u32) {
        self.range >>= shift;
        self.low = self
            .low
            .wrapping_add(self.low_count.wrapping_mul(self.range));
        self.range = self.range.wrapping_mul(self.high_count - self.low_count);
    }

    /// `ARI_FLUSH_ENCODER`: four bytes of `low`.
    pub fn flush_encoder(&mut self, s: &mut PrimeStream) {
        for _ in 0..4 {
            s.put((self.low >> 24) as u8);
            self.low <<= 8;
        }
    }

    /// `ariGetCurrentCount`. Note it MUTATES `range` (`range /= scale`), so it
    /// cannot be called twice for one symbol.
    pub fn get_current_count(&mut self) -> u32 {
        self.range /= self.scale;
        self.code.wrapping_sub(self.low) / self.range
    }

    /// `ariGetCurrentShiftCount`. Also mutates `range`.
    pub fn get_current_shift_count(&mut self, shift: u32) -> u32 {
        self.range >>= shift;
        self.code.wrapping_sub(self.low) / self.range
    }

    /// `ariRemoveSubrange`.
    pub fn remove_subrange(&mut self) {
        self.low = self
            .low
            .wrapping_add(self.range.wrapping_mul(self.low_count));
        self.range = self.range.wrapping_mul(self.high_count - self.low_count);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constants_are_read_from_the_macros_not_the_comment() {
        assert_eq!(TOP, 1 << 24);
        // The commented-out original above the macros says 1<<16. The macros
        // PPMd actually compiles say 1<<15, and that is what the format uses.
        assert_eq!(BOT, 1 << 15);
    }

    #[test]
    fn init_encoder_sets_full_range() {
        let mut rc = RangeCoder::new();
        rc.init_encoder();
        assert_eq!(rc.low, 0);
        assert_eq!(rc.range, u32::MAX);
    }

    /// `low.wrapping_neg()` is the C's `-low` on an unsigned, which is where a
    /// naive port using a signed negation would diverge.
    #[test]
    fn wrapping_negation_matches_c_unsigned_semantics() {
        assert_eq!(1u32.wrapping_neg(), 0xffff_ffff);
        assert_eq!(0u32.wrapping_neg(), 0);
        assert_eq!(0x0000_8000u32.wrapping_neg() & (BOT - 1), 0);
    }
}
