//! Move-to-front symbol list, the MTF half of `Compression/GRZip/MTF_Ari.c`
//! (`GRZip_MTF_Ari_Decode` :404).
//!
//! Everything except the rank-to-character step lives in [`super::ari`], which
//! the WFC coder shares; this is just the list discipline.

use super::ari::{self, SymbolList, MAX_BYTE};
use super::GrzError;

struct Mtf {
    list: [u32; MAX_BYTE],
}

impl SymbolList for Mtf {
    /// The classic move-to-front: take the character at `rank`, slide
    /// everything above it down one, and put it at the head.
    fn pick(&mut self, rank: usize) -> u8 {
        let ch = self.list[rank];
        if rank != 0 {
            let mut t = rank;
            while t > 0 {
                self.list[t] = self.list[t - 1];
                t -= 1;
            }
            self.list[0] = ch;
        }
        ch as u8
    }
}

/// `GRZip_MTF_Ari_Decode`. Note the C's parameter order here is
/// `(Input, Size = compressed length, Output, OutSize = capacity)` -- the WFC
/// sibling swaps the meanings of its second and fourth arguments, which is a
/// trap worth stating out loud rather than discovering at the call site.
pub fn decode(input: &[u8], out: &mut [u8], out_size: usize) -> Result<usize, GrzError> {
    let mut list = Mtf { list: core::array::from_fn(|i| i as u32) };
    ari::decode(input, out, out_size, &mut list)
}

impl ari::SymbolRank for Mtf {
    /// The inverse of `pick`: locate the character, then apply the identical
    /// list update. Both sides must move the list the same way or the models
    /// fall out of step after the first repeated symbol.
    fn find(&mut self, ch: u8) -> usize {
        let mut rank = 0usize;
        while self.list[rank] != ch as u32 {
            rank += 1;
        }
        if rank != 0 {
            let mut t = rank;
            while t > 0 {
                self.list[t] = self.list[t - 1];
                t -= 1;
            }
            self.list[0] = ch as u32;
        }
        rank
    }
}

/// `GRZip_MTF_Ari_Encode`. Returns the coded bytes, or `GRZ_NOT_COMPRESSIBLE`
/// once the output reaches within 24 bytes of the input's length -- the same
/// bail the C makes at the top of every symbol.
pub fn encode(input: &[u8]) -> Result<Vec<u8>, GrzError> {
    let mut list = Mtf { list: core::array::from_fn(|i| i as u32) };
    let limit = input.len().saturating_sub(24);
    ari::encode(input, &mut list, limit)
}

#[cfg(test)]
mod mtf_tests {
    use super::*;

    /// Encode then decode must be the identity. This catches drift between the
    /// two halves' model state, which the C differential cannot see -- there
    /// the encoder and decoder are separate copies of the same macros too.
    #[test]
    fn round_trips() {
        let cases: Vec<Vec<u8>> = vec![
            vec![b'A'; 64],
            (0..64u8).map(|i| i / 8).collect(),
            (0..600u32).map(|i| (i % 7) as u8).collect(),
            b"the quick brown fox jumps over the lazy dog. ".repeat(20),
            (0..1000).map(|i| ((i * 37) % 251) as u8).collect(),
        ];
        for (n, input) in cases.iter().enumerate() {
            let coded = encode(input).expect("encode");
            let mut out = vec![0u8; input.len() + 64];
            let got = match decode(&coded, &mut out, input.len()) {
                Ok(n) => n,
                Err(e) => panic!("case {n}: decode failed with {e} (input {} bytes, coded {} bytes)", input.len(), coded.len()),
            };
            assert_eq!(got, input.len(), "case {n}: length");
            assert_eq!(&out[..got], &input[..], "case {n}: bytes");
        }
    }
}
