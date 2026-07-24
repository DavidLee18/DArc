//! BSC (libbsc) decoder, ported from `Compression/BSC/`.
//!
//! **Work in progress.** The block header and Adler-32 are ported; the four
//! decode stages are not, so nothing is wired to `BSC_METHOD::decompress` and
//! the C decoder still runs.
//!
//! BSC is the largest remaining codec at 17,368 lines, but its **decode
//! surface is about 4,800** -- most of the bulk is `libsais.c`'s forward
//! suffix-array construction, which only the compressor uses. The decode path
//! needs, in the order a block flows through it:
//!
//! | stage | from | approx |
//! |---|---|---|
//! | block header + dispatch | `libbsc.cpp` | 300 |
//! | entropy decode (QLFC) | `coder.cpp`, `qlfc.cpp` | 1,350 |
//! | inverse block sort | `libsais_unbwt`, or `st.cpp` for ST3..ST8 | 1,900 |
//! | inverse LZP | `lzp.cpp` | 400 |
//! | Adler-32 | `adler32.cpp` | 247 |
//!
//! ## The block format
//!
//! A 28-byte header, then the coded payload, then (for BWT with auxiliary
//! indexes) a trailing index array. The header is six little-endian 32-bit
//! fields:
//!
//! ```text
//!   0  blockSize      total bytes of this block, header included
//!   4  dataSize       decompressed size
//!   8  mode           packed stage selection, see below
//!  12  index          the block-sort primary index
//!  16  adler32_data   checksum of the DECOMPRESSED data
//!  20  adler32_body   checksum of everything after the header
//!  24  adler32_header checksum of bytes 0..23
//! ```
//!
//! `mode` packs the pipeline: bits 0-4 the block sorter (BWT or ST3..ST8),
//! bits 5-7 the coder, bits 8-15 the LZP minimum match length, bits 16-23 the
//! LZP hash size. `mode == 0` means the payload is stored verbatim, and
//! `mode == (mode & 0xff)` means LZP was not applied -- so the LZP stage is
//! keyed on the *upper* bits being set, not on a flag.
//!
//! Three separate Adler-32 checksums guard a block, which is why that comes
//! first: without it, nothing else can be validated the way the C validates it.

#![allow(dead_code)] // WIP: layers land before the decoder that uses them

pub mod adler32;
pub mod header;
pub mod lzp;
pub mod model;
pub mod model_consts;
pub mod predictor;
pub mod qlfc;
pub mod rangecoder;
pub mod tables;

/// Error codes (`libbsc.h:41-47`).
pub const LIBBSC_NO_ERROR: i32 = 0;
pub const LIBBSC_BAD_PARAMETER: i32 = -1;
pub const LIBBSC_NOT_ENOUGH_MEMORY: i32 = -2;
pub const LIBBSC_UNEXPECTED_EOB: i32 = -5;
pub const LIBBSC_DATA_CORRUPT: i32 = -6;

/// `LIBBSC_HEADER_SIZE` (:84).
pub const HEADER_SIZE: usize = 28;

/// Block sorters (`libbsc.h:53-63`).
pub const BLOCKSORTER_NONE: u32 = 0;
pub const BLOCKSORTER_BWT: u32 = 1;
pub const BLOCKSORTER_ST3: u32 = 3;
pub const BLOCKSORTER_ST4: u32 = 4;
pub const BLOCKSORTER_ST5: u32 = 5;
pub const BLOCKSORTER_ST6: u32 = 6;
pub const BLOCKSORTER_ST7: u32 = 7;
pub const BLOCKSORTER_ST8: u32 = 8;

/// Entropy coders (`libbsc.h:67-70`).
pub const CODER_NONE: u32 = 0;
pub const CODER_QLFC_STATIC: u32 = 1;
pub const CODER_QLFC_ADAPTIVE: u32 = 2;
pub const CODER_QLFC_FAST: u32 = 3;
