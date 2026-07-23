//! Weighted-frequency-count symbol list, the WFC half of
//! `Compression/GRZip/WFC_Ari.c` (`GRZip_WFC_Ari_Decode` :498).
//!
//! Everything except the rank-to-character step lives in [`super::ari`], which
//! the MTF coder shares.
//!
//! ## What WFC does differently
//!
//! Move-to-front promotes a character all the way to the head on every use, so
//! one appearance of a rare byte costs every other byte a rank. WFC instead
//! keeps a *weight* per character and sorts the list by it. A character used
//! now gains `WFC_VAL0`; characters used 1, 2, 4, ... 2048 symbols ago each
//! lose a decreasing amount. The twelve decrements sum to exactly `WFC_VAL0`,
//! so the total weight in the system is conserved.
//!
//! That is why the list needs a history buffer: to know which character was
//! `Pos` symbols back, the decoder keeps one byte per decoded *symbol* (not per
//! output byte, which run-lengths make far more numerous).
//!
//! `CharWeight[256] = -1` is a sentinel that terminates the insertion walk. It
//! only works while every real weight stays above -1; the walk is bounded at
//! the sentinel index here rather than trusting that, since a corrupt block
//! could in principle drive a weight below it and walk off the list.

use super::ari::{self, SymbolList, MAX_BYTE};
use super::GrzError;

/// Weight granted to the character just decoded (`WFC_Val0`, :42).
const VAL0: i32 = 131072;

/// Weights removed from the characters 1, 2, 4, ... 2048 symbols back
/// (`WFC_Val1`..`WFC_Val12`, :43-54). These sum to exactly `VAL0`.
const VALS: [i32; 12] = [114688, 7272, 4240, 2364, 1263, 649, 320, 153, 70, 31, 14, 8];

/// How far back each of those weights looks (`WFC_Pos1`..`WFC_Pos12`, :56-67).
const POSITIONS: [usize; 12] = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048];

struct Wfc {
    /// Characters ordered by weight, with a sentinel at index `MAX_BYTE`.
    list: [i32; MAX_BYTE + 1],
    /// Inverse of `list`.
    index: [i32; MAX_BYTE + 1],
    /// Current weight per character; `weight[MAX_BYTE]` is the -1 sentinel.
    weight: [i32; MAX_BYTE + 1],
    /// One byte per decoded symbol, for the look-back above.
    history: Vec<u8>,
    pos: usize,
}

impl Wfc {
    fn new(capacity: usize) -> Self {
        let mut w = Wfc {
            list: core::array::from_fn(|i| i as i32),
            index: core::array::from_fn(|i| i as i32),
            weight: [0; MAX_BYTE + 1],
            history: vec![0u8; capacity],
            pos: 0,
        };
        w.weight[MAX_BYTE] = -1;
        w
    }

    /// `Update_Weight0`: the character just decoded gains `VAL0` and moves to
    /// the head unconditionally.
    fn promote(&mut self, c: usize) {
        self.weight[c] += VAL0;
        let mut j = self.index[c] as usize;
        while j > 0 {
            let moved = self.list[j - 1];
            self.list[j] = moved;
            self.index[moved as usize] = j as i32;
            j -= 1;
        }
        self.list[0] = c as i32;
        self.index[c] = 0;
    }

    /// `Update_Weight`: `c` loses `amount` and sinks past every character that
    /// now outweighs it. Skipped for the character just decoded, which
    /// `promote` has already placed.
    fn demote(&mut self, c: usize, amount: i32, current: usize) {
        let w = self.weight[c] - amount;
        self.weight[c] = w;
        if c == current {
            return;
        }
        let mut j = self.index[c] as usize;
        // The sentinel at MAX_BYTE stops this in the C; bound it explicitly so
        // a weight driven below -1 by a corrupt block cannot walk off the end.
        while j + 1 <= MAX_BYTE && self.weight[self.list[j + 1] as usize] > w {
            let moved = self.list[j + 1];
            self.list[j] = moved;
            self.index[moved as usize] = j as i32;
            j += 1;
        }
        self.list[j] = c as i32;
        self.index[c] = j as i32;
    }
}

impl SymbolList for Wfc {
    fn pick(&mut self, rank: usize) -> u8 {
        let ch = self.list[rank] as usize;
        if self.pos < self.history.len() {
            self.history[self.pos] = ch as u8;
        }
        self.promote(ch);
        // `Update_Weight_Full` for each of the twelve look-back distances.
        for (amount, back) in VALS.iter().zip(POSITIONS.iter()) {
            if self.pos >= *back {
                let c = self.history[self.pos - *back] as usize;
                self.demote(c, *amount, ch);
            }
        }
        self.pos += 1;
        ch as u8
    }
}

/// `GRZip_WFC_Ari_Decode`.
///
/// **The C's parameter order is not the MTF one.** There it is
/// `(Input, Size = compressed length, Output, OutSize)`; here it is
/// `(Input, Size = *decoded* length, Output, InSize = compressed length)` --
/// the second and fourth arguments swap meaning between two functions that are
/// otherwise near-identical. `Size` is the decoded length because `WFC_Init`
/// uses it to size the history buffer.
pub fn decode(input: &[u8], out: &mut [u8], out_size: usize) -> Result<usize, GrzError> {
    let mut list = Wfc::new(out_size.min(out.len()).max(1));
    ari::decode(input, out, out_size, &mut list)
}
