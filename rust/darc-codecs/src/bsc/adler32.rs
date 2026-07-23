//! Adler-32, ported from `Compression/BSC/libbsc/adler32/adler32.cpp`.
//!
//! Standard Adler-32 (`BASE` 65521, `NMAX` 5504). The C ships SSE2/SSSE3/AVX/
//! AVX2/NEON variants selected at runtime, but they all compute the same
//! function -- this is the scalar definition they optimise, so a single
//! implementation is exact rather than merely equivalent.
//!
//! BSC uses three of these per block: over the header's first 24 bytes, over
//! the block body, and over the *decompressed* data. All three must match or
//! the C rejects the block, so this has to be right before any stage can be
//! validated the way the C validates it.

/// `BASE` -- the largest prime below 65536.
const BASE: u32 = 65521;
/// `NMAX` -- the most bytes that can be accumulated before a reduction is
/// needed to keep `sum2` inside 32 bits.
const NMAX: usize = 5504;

/// `bsc_adler32`.
pub fn adler32(data: &[u8]) -> u32 {
    let mut sum1: u32 = 1;
    let mut sum2: u32 = 0;

    let mut rest = data;
    while !rest.is_empty() {
        let take = rest.len().min(NMAX);
        for &b in &rest[..take] {
            sum1 += b as u32;
            sum2 += sum1;
        }
        sum1 %= BASE;
        sum2 %= BASE;
        rest = &rest[take..];
    }

    (sum2 << 16) | sum1
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pinned against the algorithm's published definition rather than against
    /// this implementation: Adler-32 is a fixed standard, so known vectors are
    /// a real check and not a tautology.
    #[test]
    fn matches_known_vectors() {
        assert_eq!(adler32(b""), 1);
        assert_eq!(adler32(b"a"), 0x0062_0062);
        assert_eq!(adler32(b"abc"), 0x024d_0127);
        assert_eq!(adler32(b"Wikipedia"), 0x11E6_0398);
    }

    /// The NMAX reduction boundary is the one place a scalar/SIMD split could
    /// disagree, so inputs straddling it are checked against a direct
    /// (slow, obviously-correct) computation.
    #[test]
    fn agrees_with_the_direct_definition_across_the_nmax_boundary() {
        fn direct(data: &[u8]) -> u32 {
            let (mut a, mut b) = (1u64, 0u64);
            for &x in data {
                a = (a + x as u64) % BASE as u64;
                b = (b + a) % BASE as u64;
            }
            ((b << 16) | a) as u32
        }
        let mut s: u32 = 7;
        let big: Vec<u8> = (0..NMAX * 3 + 17)
            .map(|_| {
                s = s.wrapping_mul(1103515245).wrapping_add(12345);
                (s >> 16) as u8
            })
            .collect();
        for n in [0, 1, NMAX - 1, NMAX, NMAX + 1, 2 * NMAX, big.len()] {
            assert_eq!(adler32(&big[..n]), direct(&big[..n]), "length {n}");
        }
    }
}
