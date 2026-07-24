//! Rust ports of DArc's codecs, exposed under the original C ABI.
//!
//! Each codec is swapped in individually: the Rust symbol replaces the C one at
//! link time, everything else in the build stays put, and the archive
//! fingerprint suite proves the output is byte-identical before the C version
//! is removed. Bit-exactness is not optional -- a codec that compresses
//! "correctly" but differently produces archives older builds cannot read,
//! which is the highest-risk failure mode in this repository.

pub mod bsc;
pub mod delta;
pub mod dict;
pub mod dict_encode;
pub mod dispack;
pub mod grzip;
pub mod lz4;
pub mod lzp;
pub mod mm;
pub mod rep;
pub mod srep;
pub mod tornado;
pub mod tta;
// Behind a feature: zstd-sys compiles C, which would force a C cross-compiler
// for every shipped target. Nothing calls this yet -- see Cargo.toml.
#[cfg(feature = "zstd")]
pub mod zstd;
pub mod ffi;

mod exports;
