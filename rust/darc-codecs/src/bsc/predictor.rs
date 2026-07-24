//! The probability counter and mixer, ported from
//! `Compression/BSC/libbsc/coder/common/predictor.h`.
//!
//! QLFC predicts each binary decision from three sources -- a static model, a
//! per-state model and a per-character model -- and blends them. The blend is a
//! logistic mixer: probabilities are converted to the *stretched* domain
//! (log-odds, 12-bit fixed point), combined linearly with learned weights,
//! squashed back, then refined through a small adaptive probability map.
//!
//! Both sides run this identically as they code, so every rounding decision
//! here is part of the format. Three in particular:
//!
//! ## The weighted sum wraps at 32 bits, and that IS load-bearing
//!
//! ```c
//! short stretchedProbability = (s0 * weight0 + s1 * weight1 + s2 * weight2) >> 17;
//! if (stretchedProbability < -2047) ...
//! ```
//!
//! The narrowing to `short` looks like the trap and is not: `>> 17` leaves any
//! `int` sum within +/-16384, which always fits a `short`, so the assignment
//! never truncates. A test asserting otherwise is what established this --
//! it could not construct a case where wrapping and saturating differ.
//!
//! What *is* real is the sum itself. Those are `int` products that overflow
//! silently in C once the learned weights drift far enough. Rust panics on that
//! in a debug build, which would abort across the C ABI on input that merely
//! pushes the mixer hard -- reachable from `arc t` on a corrupt archive. So the
//! arithmetic here is `wrapping_*` throughout, matching C, and the `as i16` is
//! kept only because it is what the C writes.
//!
//! ## The weight update runs on the *previous* mix
//!
//! `UpdateBit0`/`UpdateBit1` (as opposed to the `MixupAndUpdate*` pair) use the
//! `index`, `mixedProbability` and stretched values cached by the last `Mixup`.
//! So the mixer is stateful across calls, and the update must see exactly what
//! that call left behind.
//!
//! ## `probabilityMap` has 17 entries and is indexed at `index + 1`
//!
//! `index` is `(stretched + 2048) >> 8`, which reaches 16 when stretched is
//! 2047 -- so the `index + 1` accesses require all 17 slots.

use super::tables::{SQUASH_TABLE, STRETCH_TABLE};

/// `bsc_stretch(p)` -- p is a 12-bit probability, 0..4096.
#[inline]
pub fn stretch(p: i32) -> i32 {
    STRETCH_TABLE[p.clamp(0, 4096) as usize] as i32
}

/// `bsc_squash(s)` -- s is a stretched value, -2048..2048.
#[inline]
pub fn squash(s: i32) -> i32 {
    SQUASH_TABLE[(2048 + s).clamp(0, 4096) as usize] as i32
}

/// `ProbabilityCounter`. Each is a `short` probability adapted toward a
/// threshold at a given rate.
pub struct ProbabilityCounter;

impl ProbabilityCounter {
    /// `UpdateBit(bit, probability, th0, ar0, th1, ar1)`.
    #[inline]
    pub fn update_bit(bit: u32, p: &mut i16, th0: i32, ar0: i32, th1: i32, ar1: i32) {
        let prob = *p as i32;
        let delta0 = prob.wrapping_mul(ar0).wrapping_sub((4096 - th0).wrapping_mul(ar0) - 4095);
        let delta1 = prob.wrapping_mul(ar1).wrapping_sub(th1.wrapping_mul(ar1));
        let delta = if bit != 0 { delta1 } else { delta0 };
        *p = (prob - (delta >> 12)) as i16;
    }

    /// `UpdateBit0(probability, threshold, adaptationRate)`.
    #[inline]
    pub fn update_bit0(p: &mut i16, threshold: i32, rate: i32) {
        let prob = *p as i32;
        *p = prob.wrapping_add((((4096 - threshold - prob).wrapping_mul(rate)) >> 12) ) as i16;
    }

    /// `UpdateBit1(probability, threshold, adaptationRate)`.
    #[inline]
    pub fn update_bit1(p: &mut i16, threshold: i32, rate: i32) {
        let prob = *p as i32;
        *p = prob.wrapping_sub((((prob - threshold).wrapping_mul(rate)) >> 12)) as i16;
    }

    /// `UpdateBit<R>(bit, probability, th0, th1)` -- the shift-rate form.
    #[inline]
    pub fn update_bit_r(bit: u32, p: &mut i16, th0: i32, th1: i32, r: u32) {
        let prob = *p as i32;
        let target = if bit != 0 { th1 } else { th0 };
        *p = (prob - ((prob - target) >> r)) as i16;
    }

    /// `UpdateBit<R>(probability, threshold)`.
    #[inline]
    pub fn update_bit_r1(p: &mut i16, threshold: i32, r: u32) {
        let prob = *p as i32;
        *p = (prob - ((prob - threshold) >> r)) as i16;
    }
}

/// `ProbabilityMixer`.
#[derive(Clone)]
pub struct ProbabilityMixer {
    stretched0: i16,
    stretched1: i16,
    stretched2: i16,
    mixed_probability: i32,
    index: usize,
    probability_map: [i16; 17],
    weight0: i32,
    weight1: i32,
    weight2: i32,
}

impl Default for ProbabilityMixer {
    fn default() -> Self {
        let mut m = ProbabilityMixer {
            stretched0: 0,
            stretched1: 0,
            stretched2: 0,
            mixed_probability: 0,
            index: 0,
            probability_map: [0; 17],
            weight0: 0,
            weight1: 0,
            weight2: 0,
        };
        m.init();
        m
    }
}

impl ProbabilityMixer {
    /// `Init`.
    pub fn init(&mut self) {
        self.weight0 = 2048 << 5;
        self.weight1 = 2048 << 5;
        self.weight2 = 0;
        for p in 0..17 {
            self.probability_map[p] = squash((p as i32 - 8) * 256) as i16;
        }
    }

    /// The shared front half: stretch, weight, narrow to i16, clamp, look up.
    /// Returns (stretched_clamped, index, mixed_probability).
    #[inline]
    fn mix(&self, s0: i32, s1: i32, s2: i32) -> (i32, usize, i32) {
        // NARROWING IS LOAD-BEARING: the C assigns the shifted int sum to a
        // `short`, which wraps, and clamps only afterwards.
        //
        // The sum itself is also wrapping. In C these are `int` products that
        // overflow silently once the learned weights drift far enough; Rust
        // panics on that in debug, which would abort across the C ABI on input
        // that merely pushes the mixer hard -- reachable from `arc t` on a
        // corrupt archive. Matching C's wraparound is both correct and safe.
        let sum = (s0.wrapping_mul(self.weight0))
            .wrapping_add(s1.wrapping_mul(self.weight1))
            .wrapping_add(s2.wrapping_mul(self.weight2));
        let mut stretched = (sum >> 17) as i16 as i32;
        if stretched < -2047 {
            stretched = -2047;
        }
        if stretched > 2047 {
            stretched = 2047;
        }
        let index = ((stretched + 2048) >> 8) as usize;
        let weight = stretched & 255;
        let probability = squash(stretched);
        let m0 = self.probability_map[index] as i32;
        let m1 = self.probability_map[index + 1] as i32;
        let mapped = m0 + (((m1 - m0) * weight) >> 8);
        let mixed = (3 * probability + mapped) >> 2;
        (stretched, index, mixed)
    }

    /// `Mixup`: mix and remember the state the later `UpdateBit*` will use.
    #[inline]
    pub fn mixup(&mut self, p0: i32, p1: i32, p2: i32) -> i32 {
        let (s0, s1, s2) = (stretch(p0), stretch(p1), stretch(p2));
        self.stretched0 = s0 as i16;
        self.stretched1 = s1 as i16;
        self.stretched2 = s2 as i16;
        let (_stretched, index, mixed) = self.mix(s0, s1, s2);
        self.index = index;
        self.mixed_probability = mixed;
        mixed
    }

    /// The weight update shared by both bit values. `eps` differs: the C uses
    /// `mixed - 4095` for a zero bit and `mixed - 1` for a one.
    #[inline]
    fn update_weights(&mut self, eps: i32, lr0: i32, lr1: i32, lr2: i32, s0: i32, s1: i32, s2: i32) {
        // Wrapping for the same reason as the mix sum: these are `int` in C.
        self.weight0 = self.weight0.wrapping_sub((lr0.wrapping_mul(eps).wrapping_mul(s0)) >> 16);
        self.weight1 = self.weight1.wrapping_sub((lr1.wrapping_mul(eps).wrapping_mul(s1)) >> 16);
        self.weight2 = self.weight2.wrapping_sub((lr2.wrapping_mul(eps).wrapping_mul(s2)) >> 16);
    }

    /// `UpdateBit0` -- uses the state cached by the last `mixup`.
    #[inline]
    pub fn update_bit0(&mut self, lr0: i32, lr1: i32, lr2: i32, threshold: i32, rate: i32) {
        let i = self.index;
        ProbabilityCounter::update_bit0(&mut self.probability_map[i], threshold, rate);
        ProbabilityCounter::update_bit0(&mut self.probability_map[i + 1], threshold, rate);
        let eps = self.mixed_probability - 4095;
        let (s0, s1, s2) = (self.stretched0 as i32, self.stretched1 as i32, self.stretched2 as i32);
        self.update_weights(eps, lr0, lr1, lr2, s0, s1, s2);
    }

    /// `UpdateBit1` -- uses the state cached by the last `mixup`.
    #[inline]
    pub fn update_bit1(&mut self, lr0: i32, lr1: i32, lr2: i32, threshold: i32, rate: i32) {
        let i = self.index;
        ProbabilityCounter::update_bit1(&mut self.probability_map[i], threshold, rate);
        ProbabilityCounter::update_bit1(&mut self.probability_map[i + 1], threshold, rate);
        let eps = self.mixed_probability - 1;
        let (s0, s1, s2) = (self.stretched0 as i32, self.stretched1 as i32, self.stretched2 as i32);
        self.update_weights(eps, lr0, lr1, lr2, s0, s1, s2);
    }

    /// `MixupAndUpdateBit0`: mix and update in one step, WITHOUT touching the
    /// cached state -- the C uses locals here, so a following bare `UpdateBit*`
    /// would still see whatever the last plain `Mixup` left.
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn mixup_and_update_bit0(
        &mut self, p0: i32, p1: i32, p2: i32,
        lr0: i32, lr1: i32, lr2: i32, threshold: i32, rate: i32,
    ) -> i32 {
        let (s0, s1, s2) = (stretch(p0), stretch(p1), stretch(p2));
        let (_st, index, mixed) = self.mix(s0, s1, s2);
        ProbabilityCounter::update_bit0(&mut self.probability_map[index], threshold, rate);
        ProbabilityCounter::update_bit0(&mut self.probability_map[index + 1], threshold, rate);
        self.update_weights(mixed - 4095, lr0, lr1, lr2, s0, s1, s2);
        mixed
    }

    /// `MixupAndUpdateBit1`.
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn mixup_and_update_bit1(
        &mut self, p0: i32, p1: i32, p2: i32,
        lr0: i32, lr1: i32, lr2: i32, threshold: i32, rate: i32,
    ) -> i32 {
        let (s0, s1, s2) = (stretch(p0), stretch(p1), stretch(p2));
        let (_st, index, mixed) = self.mix(s0, s1, s2);
        ProbabilityCounter::update_bit1(&mut self.probability_map[index], threshold, rate);
        ProbabilityCounter::update_bit1(&mut self.probability_map[index + 1], threshold, rate);
        self.update_weights(mixed - 1, lr0, lr1, lr2, s0, s1, s2);
        mixed
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// stretch and squash must be inverses over the usable range, which is the
    /// cheapest check that the generated tables are the right ones and are not
    /// swapped.
    #[test]
    fn stretch_and_squash_round_trip() {
        for p in [1i32, 100, 1000, 2048, 3000, 4000, 4095] {
            let s = stretch(p);
            let back = squash(s);
            assert!((back - p).abs() <= 32, "stretch/squash({p}) -> {s} -> {back}");
        }
        // Monotonic: a higher probability stretches to a higher log-odds.
        for p in 1..4095 {
            assert!(stretch(p + 1) >= stretch(p), "stretch not monotonic at {p}");
        }
    }

    /// `index + 1` reaches 16 at the top of the clamped range, so all 17 map
    /// slots are live. A 16-entry map would be an out-of-bounds read exactly
    /// when the mixer is most confident.
    #[test]
    fn the_probability_map_uses_all_seventeen_slots() {
        let stretched_max = 2047i32;
        let index = ((stretched_max + 2048) >> 8) as usize;
        assert_eq!(index, 15);
        assert!(index + 1 <= 16, "index+1 must stay inside the 17-entry map");
        // And the low end.
        let index_min = ((-2047 + 2048) >> 8) as usize;
        assert_eq!(index_min, 0);
    }

    /// The `short` assignment in the C mixer is a NO-OP, not a truncation:
    /// `>> 17` bounds any i32 sum to +/-16384, which always fits an i16. This
    /// test exists because the opposite was assumed at first; it pins the fact
    /// so nobody "fixes" the cast back into something that changes behaviour.
    #[test]
    fn the_short_narrowing_cannot_change_the_value() {
        for sum in [i32::MIN, i32::MIN / 2, -(1 << 20), 0, 1 << 20, i32::MAX / 2, i32::MAX] {
            let shifted = sum >> 17;
            assert_eq!(
                shifted as i16 as i32, shifted,
                "sum {sum} -> {shifted} must survive the i16 cast unchanged"
            );
            assert!((-16384..=16384).contains(&shifted));
        }
    }

    /// What IS load-bearing: the weighted sum overflows i32 in C and wraps.
    /// Rust must wrap too rather than panic, since `arc t` runs this on
    /// untrusted data that can drive the weights far.
    #[test]
    fn a_mixer_with_extreme_weights_wraps_instead_of_panicking() {
        let mut m = ProbabilityMixer::default();
        m.weight0 = i32::MAX;
        m.weight1 = i32::MAX;
        m.weight2 = i32::MAX;
        // Must not panic, and must land inside the clamped range.
        let (stretched, index, mixed) = m.mix(2047, -2047, 2047);
        assert!((-2047..=2047).contains(&stretched));
        assert!(index <= 15);
        assert!((0..=4096).contains(&mixed));

        // And the update path likewise, driven hard.
        for _ in 0..100 {
            m.mixup(4000, 50, 2048);
            m.update_bit1(80, 80, 80, -300, 1200);
            m.mixup(10, 4090, 2048);
            m.update_bit0(80, 80, 80, 300, 1200);
        }
    }

    /// A counter adapting toward a threshold must move toward it and stay in
    /// range across many updates.
    #[test]
    fn counters_converge_toward_their_threshold() {
        let mut p: i16 = 2048;
        for _ in 0..500 {
            ProbabilityCounter::update_bit1(&mut p, 100, 200);
        }
        assert!(p < 2048, "a stream of 1 bits must lower the probability");
        let mut q: i16 = 2048;
        for _ in 0..500 {
            ProbabilityCounter::update_bit0(&mut q, 100, 200);
        }
        assert!(q > 2048, "a stream of 0 bits must raise it");
    }
}
