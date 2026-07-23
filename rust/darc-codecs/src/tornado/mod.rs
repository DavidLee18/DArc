//! Tornado LZ77 decoder, ported from `Compression/Tornado/`.
//!
//! **Work in progress.** The stream and table layers are ported; the four
//! entropy back-ends, the data-table undiffer and the output loop are not, so
//! nothing here is wired to `tor_decompress` yet and the C decoder is still the
//! one that runs. Nothing is excluded C-side until the port is complete and
//! verified byte-exact, the same order used for REP, Dict, LZP, TTA and MM.
//!
//! Tornado is the highest-value target left: `tor` is a default method, so
//! every `-m4`-and-up archive contains Tornado streams.
//!
//! ## Shape of the thing
//!
//! `tor_decompress` (Tornado.cpp:487) reads a six-byte header --
//! `encoding_method`, `minlen`, then a 4-byte `bufsize` -- and dispatches to
//! `tor_decompress0<Decoder>` (:400) with one of four back-ends:
//!
//! | method | back-end | entropy layer |
//! |---|---|---|
//! | 1 BYTECODER | `LZ77_ByteDecoder` | none, byte-aligned with LZSS flag words |
//! | 2 BITCODER  | `LZ77_BitDecoder`  | none, bit-aligned with VLE codes |
//! | 3 HUFCODER  | `LZ77_Decoder<HuffmanDecoder>` | semi-adaptive Huffman |
//! | 4 ARICODER  | `LZ77_Decoder<ArithDecoder>`   | semi-adaptive range coder |
//!
//! 0 (STORING) never reaches the decoder loop. Only the entropy layer differs;
//! all four feed the same `is_literal` / `getchar` / `getlen` / `getdist`
//! interface to one output loop over a circular buffer.
//!
//! The Huffman and arithmetic back-ends are *semi-adaptive*: both sides update
//! symbol counters as they go and rebuild their tables on the same schedule, so
//! the decoder has to reproduce the tree builder and the rescale exactly, not
//! merely read the codes. That is the bulk of the remaining work.
//!
//! Encoder-only files are not in scope: `MatchFinder.cpp` (956 lines) and
//! `OptimalParsing.cpp` (44) are reached only from `tor_compress`.

#![allow(dead_code)] // WIP: layers land before the entry point that uses them

pub mod huffman;
pub mod lz77;
pub mod range;
pub mod stream;
pub mod vle;

/// Encoding methods (`enum {STORING..ARICODER}`, Tornado.cpp:34).
pub const STORING: u32 = 0;
pub const BYTECODER: u32 = 1;
pub const BITCODER: u32 = 2;
pub const HUFCODER: u32 = 3;
pub const ARICODER: u32 = 4;

/// Code-space layout for the Huffman/arith back-ends (LZ77_Coder.cpp:388-398).
pub const REPDIST_CODES: usize = 4;
pub const DIST_CODES: usize = vle::EXTRA_DBITS.len() + REPDIST_CODES; // 36
pub const LEN_CODES: usize = vle::EXTRA_LBITS2.len(); // 16
/// End of block -- also the signal to rebuild the Huffman tree.
pub const EOB_CODE: usize = 256 + LEN_CODES * DIST_CODES; // 832
/// Copy one char at the last distance.
pub const REPCHAR: usize = EOB_CODE + 1;
/// Repeat both the previous length and distance.
pub const REPBOTH: usize = EOB_CODE + 2;
/// Total alphabet, including seven spare codes.
pub const CODES: usize = EOB_CODE + 10; // 842

/// `PAD_FOR_TABLES` (DataTables.cpp:13): slack required both before and after
/// the output buffer so table undiffing can reach across its edges.
pub const MAX_TABLE_ROW_AT_DECOMPRESSION: usize = 256;
pub const PAD_FOR_TABLES: usize = MAX_TABLE_ROW_AT_DECOMPRESSION * 2;

#[cfg(test)]
mod tests {
    use super::vle::*;

    /// The distance base values are the one table where an off-by-one is
    /// invisible until archives decode to subtly wrong bytes, so they are
    /// pinned against the three-range construction in LZ77_Coder.cpp:233.
    #[test]
    fn distance_bases_follow_the_three_ranges() {
        let base = distance_bases();
        // First range counts distances one at a time from zero.
        assert_eq!(base[0], 0);
        assert_eq!(base[1], 16); // 1<<4
        assert_eq!(base[2], 32); // +1<<4
        assert_eq!(base[3], 64); // +1<<5
        // Ranges are monotonically increasing across all three regimes.
        for w in base.windows(2) {
            assert!(w[1] > w[0], "distance bases must increase: {w:?}");
        }
        // The last code reaches the 1 GB ceiling the coder documents.
        assert!(base[31] >= 1 << 20, "top distance base too small: {}", base[31]);
    }

    #[test]
    fn length_bases_are_running_sums_of_their_extra_bits() {
        let base = length_bases(&EXTRA_LBITS);
        assert_eq!(base[0], 0);
        assert_eq!(base[1], 1);
        assert_eq!(base[2], 2);
        assert_eq!(base[3], 3); // codes 0..2 carry no extra bits
        assert_eq!(base[4], 5); // +1<<1
        assert_eq!(base[5], 9); // +1<<2

        let base2 = length_bases(&EXTRA_LBITS2);
        // Seven zero-extra-bit codes, so the first seven bases are 0..6.
        for (i, b) in base2.iter().take(7).enumerate() {
            assert_eq!(*b, i as u32);
        }
        assert_eq!(base2[7], 7);
        assert_eq!(base2[8], 9); // +1<<1
    }
}
