//! DisPack x86 branch/call/jump filter, ported from `Compression/DisPack/`.
//!
//! **Both directions are ported and wired.** `filter` inverts the transform
//! (`DisUnFilter`) and `encode` applies it (`DisFilter`); `decode` drives the
//! tagged-chunk stream. Each is byte-identical to the C, checked by
//! `rust/difftest/dispack-check.sh` (decode) and `dispack-filter-check.sh`
//! (encode).
//!
//! DisPack rewrites x86 code for better compression: it splits an instruction
//! stream into parallel byte streams and turns relative call/jump targets into
//! absolute ones, undone here. It is a self-contained transform -- unlike 4x4,
//! which delegates to the library dispatcher -- and reads the standard
//! callback protocol, so the usual decode-first / `#ifndef DARC_RUST` pattern
//! applies once the filter and driver land.
//!
//! Restored from the upstream FreeArc 0.67 release, not this repo's history:
//! DisPack arrived here already CP1251-corrupted (see the project notes), so
//! the reference for this port is the clean upstream source.

#![allow(dead_code)] // a few table entries are format documentation, unused by either direction

pub mod decode;
pub mod encode;
pub mod filter;
pub mod tables;
