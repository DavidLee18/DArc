//! SREP decoder, ported from `Compression/SREP/` (SREP 3.93a beta, 2014).
//!
//! **Work in progress.** The format layer and the match decoder are ported; the
//! block loop and the four per-version decode strategies are not.
//!
//! ## SREP is not like the other codecs here
//!
//! Every other ported codec is an in-process function behind a C-ABI symbol,
//! swapped in under `DARC_RUST`. SREP is not reachable that way at all -- there
//! is no `C_SREP.cpp`, no registration, and no reference to it anywhere in the
//! archiver's sources. It is an **external compressor**: `arc.ini` carries an
//! `[External compressor:srep]` section whose `packcmd`/`unpackcmd` spawn a
//! `srep` binary against files on disk. So there is no symbol to exclude, no
//! drop-in, and nothing the archives-identical fingerprint gate can check. The
//! deliverable is an executable, verified by comparing its output files against
//! the C binary's.
//!
//! ## There is no specification
//!
//! SREP has no RFC, no format document, and no independent implementation --
//! the only thing published is the original C++ (the GitHub copy is an explicit
//! repackage of it, not a reimplementation) and the homepage the source cites
//! has been down for a decade. **The C source is the specification**, which
//! makes the differential harness the sole oracle. Careful reading counts for
//! even less here than usual; it has already been wrong twice in this port
//! (Tornado's `IMPOSSIBLE_LEN`, GRZip's strong BWT).
//!
//! ## What the format looks like
//!
//! A 16-byte archive header of four `u32`s -- two signatures, a packed
//! parameter word, and the base match length -- optionally followed by a hash
//! seed. The parameter word carries the format version in its low byte and the
//! hash selection above it.
//!
//! The version is not cosmetic: it selects one of four *decode strategies*.
//!
//! | version | strategy | what changes |
//! |---|---|---|
//! | 1 | I/O-LZ, rounded | match lengths and offsets are multiples of `L`, so a match costs 3 words instead of 4 |
//! | 2 | I/O-LZ | matches point backwards; sources before the current block are re-read from the output file |
//! | 3 | Future-LZ | matches are stored with their *source* block, pointing forward to where they will be needed |
//! | 4 | Index-LZ | the whole match list lives in a footer at the end of the file |
//!
//! A decoder that must read every existing `.srep` file has to implement all
//! four. Only v1 changes the match encoding itself; the rest change where
//! matches come from and when they can be applied.

#![allow(dead_code)] // WIP: the format layer lands before the block loop

pub mod matches;

/// `BULAT_ZIGANSHIN_SIGNATURE` -- note this is defined in
/// `srep/Compression/Compression.h` and nowhere else in the repo, which is part
/// of why the standalone build was broken.
pub const BULAT_ZIGANSHIN_SIGNATURE: u32 = 0x2635_1817;
/// `SREP_SIGNATURE` (srep.cpp:23), "SREP" little-endian.
pub const SREP_SIGNATURE: u32 = 0x5045_5253;

pub const FORMAT_VERSION1: u32 = 1;
pub const FORMAT_VERSION4: u32 = 4;
pub const FOOTER_VERSION1: u32 = 1;

/// Words in the archive header (`ARCHIVE_HEADER_SIZE`, :32).
pub const ARCHIVE_HEADER_WORDS: usize = 4;
/// Words in each block header, before the hash (`BLOCK_HEADER_SIZE`, :32).
pub const BLOCK_HEADER_WORDS: usize = 3;
/// `INDEX_LZ_FOOTER_SIZE` (:583) -- six words at the very end of a v4 file.
pub const INDEX_LZ_FOOTER_WORDS: usize = 6;

/// Smallest match length the format can express (`MINIMAL_MIN_MATCH`, :36).
pub const MINIMAL_MIN_MATCH: u32 = 16;

/// Which decode strategy a format version selects.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Strategy {
    /// v1: `ROUND_MATCHES` -- offsets and lengths are stored divided by `L`.
    IoLzRounded,
    /// v2: matches point backwards into already-written output.
    IoLz,
    /// v3: matches are stored with their source block, pointing forward.
    FutureLz,
    /// v4: the match list is a footer at the end of the file.
    IndexLz,
}

impl Strategy {
    pub fn from_version(v: u32) -> Option<Strategy> {
        match v {
            1 => Some(Strategy::IoLzRounded),
            2 => Some(Strategy::IoLz),
            3 => Some(Strategy::FutureLz),
            4 => Some(Strategy::IndexLz),
            _ => None,
        }
    }

    /// `ROUND_MATCHES`: only v1 divides offsets and lengths by `L`.
    pub fn round_matches(self) -> bool {
        self == Strategy::IoLzRounded
    }

    /// `FUTURE_LZ`: v3 stores a match's *source* position rather than its
    /// destination, which inverts how the two are recovered.
    pub fn future_lz(self) -> bool {
        self == Strategy::FutureLz
    }
}

/// The parsed archive header.
#[derive(Clone, Copy, Debug)]
pub struct ArchiveHeader {
    pub strategy: Strategy,
    /// `BASE_LEN` -- the minimum match length, `L` in the match encoding.
    pub base_len: u32,
    pub hash_num: u8,
    pub hash_seed_size: u8,
    /// Stored biased by 16 (`((header[2] >> 24) + 16) & 255`, :897).
    pub hash_size: u8,
}

/// Parse the four-word archive header. Returns `None` if it is not an SREP
/// file or the version is outside 1..=4, matching the C's two `error()` exits.
pub fn parse_header(words: &[u32]) -> Option<ArchiveHeader> {
    if words.len() < ARCHIVE_HEADER_WORDS {
        return None;
    }
    if words[0] != BULAT_ZIGANSHIN_SIGNATURE || words[1] != SREP_SIGNATURE {
        return None;
    }
    let strategy = Strategy::from_version(words[2] & 255)?;
    Some(ArchiveHeader {
        strategy,
        base_len: words[3],
        hash_num: ((words[2] >> 8) & 255) as u8,
        hash_seed_size: ((words[2] >> 16) & 255) as u8,
        // The +16 bias is the C's, and it wraps at 255 there too.
        hash_size: (((words[2] >> 24) + 16) & 255) as u8,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn header_words(version: u32) -> [u32; 4] {
        [BULAT_ZIGANSHIN_SIGNATURE, SREP_SIGNATURE, version, 512]
    }

    #[test]
    fn every_format_version_maps_to_a_strategy() {
        // All four must decode: a reader that only handles the newest cannot
        // open files written by older releases, and there is no converter.
        for (v, want) in [
            (1, Strategy::IoLzRounded),
            (2, Strategy::IoLz),
            (3, Strategy::FutureLz),
            (4, Strategy::IndexLz),
        ] {
            let h = parse_header(&header_words(v)).expect("version {v} must parse");
            assert_eq!(h.strategy, want);
            assert_eq!(h.base_len, 512);
        }
    }

    #[test]
    fn only_version_1_rounds_matches() {
        assert!(Strategy::IoLzRounded.round_matches());
        for s in [Strategy::IoLz, Strategy::FutureLz, Strategy::IndexLz] {
            assert!(!s.round_matches(), "{s:?} must not round");
        }
        assert!(Strategy::FutureLz.future_lz());
        assert!(!Strategy::IoLz.future_lz());
    }

    #[test]
    fn foreign_or_unsupported_files_are_rejected() {
        assert!(parse_header(&[]).is_none());
        assert!(parse_header(&[0, 0, 1, 512]).is_none()); // wrong signatures
        assert!(parse_header(&header_words(0)).is_none()); // version below v1
        assert!(parse_header(&header_words(5)).is_none()); // above v4
        assert!(parse_header(&header_words(255)).is_none());
    }

    #[test]
    fn hash_fields_unpack_from_the_parameter_word() {
        // version 3, hash_num 7, seed 16, hash_size stored as 8 -> 8+16 = 24.
        let w = [BULAT_ZIGANSHIN_SIGNATURE, SREP_SIGNATURE, 3 | (7 << 8) | (16 << 16) | (8 << 24), 64];
        let h = parse_header(&w).unwrap();
        assert_eq!(h.strategy, Strategy::FutureLz);
        assert_eq!(h.hash_num, 7);
        assert_eq!(h.hash_seed_size, 16);
        assert_eq!(h.hash_size, 24);
    }
}
