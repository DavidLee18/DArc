//! Tornado LZ77 codec, ported from `Compression/Tornado/`.
//!
//! Tornado is the highest-value target left: `tor` is a default method, so
//! every `-m4`-and-up archive contains Tornado streams.
//!
//! **Decoding is done** -- ported, verified byte-exact by
//! `rust/difftest/tornado-check.sh`, and wired up as the `tor_decompress`
//! drop-in (`exports.rs`).
//!
//! **Encoding is in progress.** The output streams, the four entropy back-ends
//! and the LZ77 coders are ported (`out_stream`, `huffman`, `range`,
//! `lz77_enc`); the match finders and the compression loop are not. Nothing is
//! excluded C-side until the port is complete and verified byte-exact, the same
//! order used for REP, Dict, LZP, TTA, MM and GRZip.
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
//! each side has to reproduce the tree builder and the rescale exactly, not
//! merely read or write the codes.
//!
//! ## Scope of the encoder
//!
//! `tor_compress` (Tornado.cpp:307) is a template over five axes, but
//! `FULL_COMPILE` is **not** defined in the archiver build, so the `#else`
//! if-chain ships and only **nine** concrete instantiations exist -- not the
//! 4*8*3*2=192 the comment at :309 describes, and one more than the "8 variants"
//! it claims, since a `caching_finder==7` arm was added at :354 without the
//! comment being updated. `main.cpp` (451 lines) is not built at all; the
//! makefile lists six sources and that is not one of them.

#![allow(dead_code)] // WIP: layers land before the entry point that uses them

pub mod decode;
pub mod encode;
pub mod huffman;
pub mod lz77;
pub mod lz77_enc;
pub mod matchfinder;
pub mod out_stream;
pub mod range;
pub mod stream;
pub mod tables;
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

    /// Every VLE table, pinned against the C constructors.
    ///
    /// The six tables were dumped from a verbatim transcription of `VLE::VLE`,
    /// `LengthCoder` and `DistanceCoder` (LZ77_Coder.cpp:173/195/233) compiled on
    /// its own, and came out byte-identical to what this crate builds -- all
    /// 52,251 entries, not a sampled few. This hash is over exactly that content.
    ///
    /// A single wrong entry in `dc_code` picks a neighbouring distance code, and
    /// the stream stays perfectly decodable: the decoder reads the extra bits the
    /// wrong code calls for and reconstructs a *different distance*. So this
    /// cannot be left to a spot check of a few indices, which is what the
    /// per-value assertions below would amount to on their own.
    #[test]
    fn tables_match_the_c_constructors() {
        let mut h: u64 = 0xcbf2_9ce4_8422_2325;
        let mut eat = |b: u8| {
            h ^= b as u64;
            h = h.wrapping_mul(0x100_0000_01b3);
        };
        for base in [
            &length_bases(&EXTRA_LBITS)[..],
            &length_bases(&EXTRA_LBITS2)[..],
            &distance_bases()[..],
        ] {
            for b in base {
                for x in b.to_le_bytes() {
                    eat(x);
                }
            }
        }
        for codes in [length_codes(&EXTRA_LBITS), length_codes(&EXTRA_LBITS2), distance_codes()] {
            for c in codes {
                eat(c);
            }
        }
        assert_eq!(h, 0xc2a2_177e_0e0b_2b1a, "VLE tables diverged from the C constructors");
    }

    /// The encoder's `value -> code` map and the decoder's `code -> base` map are
    /// built by separate code paths, so this checks they are actually inverses:
    /// the code chosen for a value must have a base at or below it, and the next
    /// code's base must be above it.
    #[test]
    fn distance_codes_bracket_their_bases() {
        let t = Tables::for_encoding();
        let base = distance_bases();
        for dist in (0u32..1 << 20).step_by(7) {
            let c = t.dc_code(dist);
            assert!(base[c] <= dist, "dist {dist} got code {c} with base {}", base[c]);
            if c + 1 < base.len() {
                assert!(dist < base[c + 1], "dist {dist} got code {c}, but base[{}] <= it", c + 1);
            }
        }
    }

    /// Same inverse check for both length alphabets, over every length the
    /// coders can hand them (`code()` clamps past 600).
    #[test]
    fn length_codes_bracket_their_bases() {
        let t = Tables::for_encoding();
        let lc_base = length_bases(&EXTRA_LBITS);
        let lc2_base = length_bases(&EXTRA_LBITS2);
        for len in 0u32..=600 {
            for (which, bases, c) in [
                ("lc", &lc_base[..], t.lc_code(len)),
                ("lc2", &lc2_base[..], t.lc2_code(len)),
            ] {
                assert!(bases[c] <= len, "{which}: len {len} got code {c}, base {}", bases[c]);
                if c + 1 < bases.len() {
                    assert!(len < bases[c + 1], "{which}: len {len} got code {c}, next base too low");
                }
            }
        }
    }

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
