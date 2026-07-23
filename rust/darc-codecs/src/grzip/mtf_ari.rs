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
