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

impl ari::SymbolRank for Wfc {
    /// The exact mirror of `pick`: the rank is `Char2Index[Char]` read BEFORE
    /// the weight update, then the identical promote-and-twelve-demotes.
    ///
    /// Unlike MTF's `find`, no search is needed -- `index` is maintained as the
    /// inverse of `list`, so the rank is a direct lookup. That inverse is the
    /// whole reason WFC keeps two arrays.
    fn find(&mut self, ch: u8) -> usize {
        let c = ch as usize;
        // History is one byte per SYMBOL, not per output byte; runs collapse
        // before they reach here, exactly as on the decode side.
        if self.pos < self.history.len() {
            self.history[self.pos] = ch;
        }
        let rank = self.index[c] as usize;
        self.promote(c);
        for (amount, back) in VALS.iter().zip(POSITIONS.iter()) {
            if self.pos >= *back {
                let p = self.history[self.pos - *back] as usize;
                self.demote(p, *amount, c);
            }
        }
        self.pos += 1;
        rank
    }
}

/// `GRZip_WFC_Ari_Encode`.
///
/// The history buffer is sized from the INPUT length here. On the decode side
/// the C sizes it from the decoded length, which is the same quantity seen from
/// the other direction -- and in both cases it is an upper bound, since one
/// entry is appended per symbol and a symbol may cover a whole run.
pub fn encode(input: &[u8]) -> Result<Vec<u8>, GrzError> {
    let mut list = Wfc::new(input.len().max(1));
    let limit = input.len().saturating_sub(24);
    ari::encode(input, &mut list, limit)
}

#[cfg(test)]
mod wfc_tests {
    use super::*;

    /// Encode then decode must be the identity. This is what caught the MTF
    /// coder's shift-low truncation bug: the C differential says "these bytes
    /// differ", which is true but not diagnostic, while a failing round-trip
    /// says the fault is on my side of the boundary and localises it to one
    /// half. The C's encoder and decoder are separate copies of the same
    /// macros, so nothing in the differential setup compares the two Rust
    /// halves against each other.
    /// `demote` skips the list walk for the character just promoted, and
    /// defeating that guard changes NO output. That is not a gap in the corpus,
    /// it is structural, and this pins the premise the argument rests on.
    ///
    /// The twelve decrements sum to exactly `VAL0`, so a character can never
    /// lose more than the promotion just gave it. And look-back distance 1 can
    /// never match the current symbol, because runs are collapsed before they
    /// reach the coder -- consecutive SYMBOLS always differ. That takes `VAL1`
    /// (114688 of the 131072) permanently off the table, leaving the promoted
    /// character at least +16384 ahead of where it started: still the heaviest,
    /// still at index 0, so the walk has nowhere to move it.
    ///
    /// If run collapsing ever stopped guaranteeing that, the early return would
    /// become observable and the C would have to be re-read.
    #[test]
    fn consecutive_symbols_always_differ() {
        // Run-heavy input: without collapsing, history would be full of
        // adjacent duplicates.
        let input: Vec<u8> = (0..300u32)
            .flat_map(|i| core::iter::repeat((i % 5) as u8).take(1 + (i as usize % 7)))
            .collect();
        let mut list = Wfc::new(input.len().max(1));
        let _ = ari::encode(&input, &mut list, input.len().saturating_sub(24));
        assert!(list.pos > 1, "no symbols were coded");
        for i in 1..list.pos {
            assert_ne!(
                list.history[i - 1],
                list.history[i],
                "symbol {i} repeats its predecessor -- run collapsing broke, and \
                 demote's early return is no longer unobservable"
            );
        }
    }

    #[test]
    fn round_trips() {
        let cases: Vec<Vec<u8>> = vec![
            vec![b'A'; 64],
            (0..64u32).map(|i| (i / 8) as u8).collect(),
            (0..600u32).map(|i| (i % 7) as u8).collect(),
            b"the quick brown fox jumps over the lazy dog. ".repeat(20),
            (0..1000u32).map(|i| ((i * 37) % 251) as u8).collect(),
            // Enough symbols to reach past the furthest look-back (2048), which
            // is the only way the last four weight decrements ever fire.
            (0..5000u32).map(|i| ((i * 131) % 199) as u8).collect(),
        ];
        for (n, input) in cases.iter().enumerate() {
            let coded = encode(input).expect("encode");
            let mut out = vec![0u8; input.len() + 64];
            let got = match decode(&coded, &mut out, input.len()) {
                Ok(v) => v,
                Err(e) => panic!("case {n}: decode failed with {e}"),
            };
            assert_eq!(got, input.len(), "case {n}: length");
            assert_eq!(&out[..got], &input[..], "case {n}: bytes");
        }
    }
}
