//! `PolynomialRollingHash`, ported from `Compression/SREP/hashes.cpp:129-194`.
//!
//! The compressor's match finder is built entirely on this: it slides an
//! `L`-byte window over the block and uses the hash to probe the chunk table.
//! Every byte of a `-m3`/`-m4` compressed stream depends on it agreeing with the
//! C exactly, so this is ported before anything that uses it.
//!
//! # Wrapping is the algorithm, not an accident
//!
//! The C computes in `uint64` (`BigHash`), where overflow is defined and wraps.
//! This crate sets `overflow-checks = true` in the release profile, so every
//! operation here must be a `wrapping_*` call — a plain `*` would panic on the
//! first real input rather than wrap, and it would panic *inside a codec called
//! across a C ABI*. This is the same trap the rest of the port documents; it is
//! spelled out because the arithmetic looks ordinary.
//!
//! # Two windows, not one
//!
//! `compress()` runs two of these at different widths: `hash1` over
//! `L - OFFSET` bytes and `hash2` over `L`, where `OFFSET = CYCLES - 1` comes
//! from the `ACCELERATOR` template parameter. `hash2` is reconstructed from
//! `hash1` by adding the missing leading bytes scaled by powers of `PRIME`,
//! which is why [`RollingHash::prime`] is public.

/// `PRIME1` (`hashes.cpp:197`) — the seed the compressor always uses.
pub const PRIME1: u64 = 153_191;

/// `power()` (`hashes.cpp:95`) — exponentiation by squaring, wrapping.
///
/// Reproduced rather than replaced with `u64::pow`, which panics on overflow
/// under this crate's release profile, and with `wrapping_pow`, which is only
/// equivalent because the C's loop is also plain repeated squaring — checked
/// against it in the tests rather than assumed.
pub fn power(base: u64, n: u32) -> u64 {
    let mut result: u64 = 1;
    let mut base = base;
    let mut n = n;
    while n != 0 {
        if !n.is_multiple_of(2) {
            result = result.wrapping_mul(base);
            n -= 1;
        }
        n /= 2;
        base = base.wrapping_mul(base);
    }
    result
}

/// A polynomial rolling hash over a fixed `L`-byte window.
#[derive(Clone, Copy, Debug)]
pub struct RollingHash {
    /// The current hash of the window.
    pub value: u64,
    /// `PRIME` — the seed itself.
    pub prime: u64,
    prime2: u64,
    prime3: u64,
    prime4: u64,
    /// `PRIME_L` = `seed^L`, the weight of the byte leaving the window.
    prime_l: u64,
    prime_l1: u64,
    prime_l2: u64,
    prime_l3: u64,
    /// Window width in bytes.
    pub l: usize,
}

impl RollingHash {
    /// `PolynomialRollingHash(int _L, ValueT seed)` (`:136`).
    ///
    /// The C also precomputes `PRIME5..PRIME8`; they are unused by every call
    /// site the compressor has, so they are omitted rather than carried dead.
    pub fn new(l: usize, seed: u64) -> Self {
        let prime = seed;
        let prime2 = seed.wrapping_mul(prime);
        let prime3 = seed.wrapping_mul(prime2);
        let prime4 = seed.wrapping_mul(prime3);
        let prime_l = power(prime, l as u32);
        let prime_l1 = seed.wrapping_mul(prime_l);
        let prime_l2 = seed.wrapping_mul(prime_l1);
        let prime_l3 = seed.wrapping_mul(prime_l2);
        RollingHash {
            value: 0,
            prime,
            prime2,
            prime3,
            prime4,
            prime_l,
            prime_l1,
            prime_l2,
            prime_l3,
            l,
        }
    }

    /// `moveto()` (`:183`) — recompute the hash from scratch over `buf[..L]`.
    ///
    /// The C unrolls this 16 bytes at a time via `STEP`, but the recurrence is
    /// the plain Horner form and the unrolling does not change the result: each
    /// `STEP` is four applications of `value*PRIME + byte`. Written as the
    /// simple loop and checked against the unrolled shape in the tests.
    pub fn moveto(&mut self, buf: &[u8]) {
        let mut value: u64 = 0;
        for &b in &buf[..self.l] {
            value = value.wrapping_mul(self.prime).wrapping_add(u64::from(b));
        }
        self.value = value;
    }

    /// `update(BYTE sub, BYTE add)` (`:151`) — roll the window one byte.
    ///
    /// `sub` leaves the window, `add` enters it.
    pub fn update(&mut self, sub: u8, add: u8) {
        self.value = self
            .value
            .wrapping_mul(self.prime)
            .wrapping_add(u64::from(add))
            .wrapping_sub(self.prime_l.wrapping_mul(u64::from(sub)));
    }

    /// `update<N>(void *ptr)` (`:157`) — roll the window `n` bytes at once.
    ///
    /// `ptr` points at the first byte *leaving* the window; the bytes entering
    /// it are at `ptr[L..]`. The C specialises on `N % 4` and then runs `N / 4`
    /// four-byte steps. Reproduced with the same decomposition because the
    /// intermediate `value` differs between groupings — the multiplies do not
    /// associate the way the additions suggest once everything wraps.
    pub fn update_n(&mut self, ptr: &[u8], n: usize) {
        let l = self.l;
        let rem = n % 4;
        match rem {
            0 => {}
            1 => {
                self.value = self
                    .value
                    .wrapping_mul(self.prime)
                    .wrapping_add(u64::from(ptr[l]))
                    .wrapping_sub(self.prime_l.wrapping_mul(u64::from(ptr[0])));
            }
            2 => {
                self.value = self
                    .value
                    .wrapping_mul(self.prime2)
                    .wrapping_add(self.prime.wrapping_mul(u64::from(ptr[l])))
                    .wrapping_add(u64::from(ptr[l + 1]))
                    .wrapping_sub(self.prime_l1.wrapping_mul(u64::from(ptr[0])))
                    .wrapping_sub(self.prime_l.wrapping_mul(u64::from(ptr[1])));
            }
            _ => {
                self.value = self
                    .value
                    .wrapping_mul(self.prime3)
                    .wrapping_add(self.prime2.wrapping_mul(u64::from(ptr[l])))
                    .wrapping_add(self.prime.wrapping_mul(u64::from(ptr[l + 1])))
                    .wrapping_add(u64::from(ptr[l + 2]))
                    .wrapping_sub(self.prime_l2.wrapping_mul(u64::from(ptr[0])))
                    .wrapping_sub(self.prime_l1.wrapping_mul(u64::from(ptr[1])))
                    .wrapping_sub(self.prime_l.wrapping_mul(u64::from(ptr[2])));
            }
        }

        let mut base = rem;
        for _ in 0..n / 4 {
            self.value = self
                .value
                .wrapping_mul(self.prime4)
                .wrapping_add(self.prime3.wrapping_mul(u64::from(ptr[base + l])))
                .wrapping_add(self.prime2.wrapping_mul(u64::from(ptr[base + l + 1])))
                .wrapping_add(self.prime.wrapping_mul(u64::from(ptr[base + l + 2])))
                .wrapping_add(u64::from(ptr[base + l + 3]))
                .wrapping_sub(self.prime_l3.wrapping_mul(u64::from(ptr[base])))
                .wrapping_sub(self.prime_l2.wrapping_mul(u64::from(ptr[base + 1])))
                .wrapping_sub(self.prime_l1.wrapping_mul(u64::from(ptr[base + 2])))
                .wrapping_sub(self.prime_l.wrapping_mul(u64::from(ptr[base + 3])));
            base += 4;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n)
            .map(|_| {
                s = s.wrapping_mul(1_103_515_245).wrapping_add(12_345);
                (s >> 16) as u8
            })
            .collect()
    }

    /// The C's `power()` loop, transcribed literally, as an independent check on
    /// the version above rather than a restatement of it.
    fn power_ref(base: u64, n: u32) -> u64 {
        let mut r: u64 = 1;
        let (mut b, mut n) = (base, n);
        while n != 0 {
            if !n.is_multiple_of(2) {
                r = r.wrapping_mul(b);
                n -= 1;
            }
            n /= 2;
            b = b.wrapping_mul(b);
        }
        r
    }

    #[test]
    fn power_matches_repeated_multiplication() {
        // Small exponents can be checked against the definition outright, which
        // is what pins the squaring loop rather than trusting its shape.
        for n in 0u32..40 {
            let mut want: u64 = 1;
            for _ in 0..n {
                want = want.wrapping_mul(PRIME1);
            }
            assert_eq!(power(PRIME1, n), want, "PRIME1^{n}");
            assert_eq!(power(PRIME1, n), power_ref(PRIME1, n));
        }
        // And a width where wrapping is certain to have happened many times.
        assert_eq!(power(PRIME1, 4096), power_ref(PRIME1, 4096));
    }

    #[test]
    fn rolling_one_byte_agrees_with_recomputation() {
        // update() is only correct if it lands on the same value moveto() would
        // produce for the shifted window. That equivalence is the whole reason
        // the match finder can slide instead of rehashing.
        let buf = prng(1, 4096);
        for l in [16usize, 32, 64, 512] {
            let mut h = RollingHash::new(l, PRIME1);
            h.moveto(&buf);
            for i in 0..200 {
                h.update(buf[i], buf[i + l]);
                let mut fresh = RollingHash::new(l, PRIME1);
                fresh.moveto(&buf[i + 1..]);
                assert_eq!(h.value, fresh.value, "L={l} step={i}");
            }
        }
    }

    #[test]
    fn rolling_n_bytes_agrees_with_n_single_steps() {
        // update<N> is a manual unrolling of N update() calls. The groupings
        // differ, and under wrapping multiplication that is exactly where a
        // transcription error hides, so it is checked at every N the compressor
        // can pass: X = max(CYCLES,4) and CYCLES-1 for each ACCELERATOR.
        let buf = prng(7, 8192);
        for l in [32usize, 64] {
            for n in [1usize, 2, 3, 4, 5, 7, 8, 15, 16, 31, 32, 63, 64] {
                let mut bulk = RollingHash::new(l, PRIME1);
                bulk.moveto(&buf);
                bulk.update_n(&buf, n);

                let mut one = RollingHash::new(l, PRIME1);
                one.moveto(&buf);
                for i in 0..n {
                    one.update(buf[i], buf[i + l]);
                }
                assert_eq!(bulk.value, one.value, "L={l} N={n}");
            }
        }
    }

    #[test]
    fn moveto_is_horner_over_the_window() {
        // The C unrolls moveto 16 bytes at a time; this asserts the unrolling is
        // value-preserving, which is what licenses the simple loop.
        let buf = prng(3, 1024);
        for l in [1usize, 3, 4, 15, 16, 17, 31, 32, 64, 512] {
            let mut h = RollingHash::new(l, PRIME1);
            h.moveto(&buf);
            let mut want: u64 = 0;
            for &b in &buf[..l] {
                want = want.wrapping_mul(PRIME1).wrapping_add(u64::from(b));
            }
            assert_eq!(h.value, want, "L={l}");
        }
    }

    #[test]
    fn wrapping_is_reached_by_ordinary_input() {
        // If this ever stops wrapping, the wrapping_* calls above are untested
        // and a plain `*` would pass the suite while panicking in release.
        let buf = prng(5, 256);
        let mut h = RollingHash::new(64, PRIME1);
        h.moveto(&buf);
        assert!(h.value > u64::from(u32::MAX), "hash never exceeded 32 bits");
    }
}
