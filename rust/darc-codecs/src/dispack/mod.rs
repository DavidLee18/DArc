//! DisPack x86 branch/call/jump filter, ported from `Compression/DisPack/`.
//!
//! **Work in progress.** The opcode tables and MTF helpers are ported; the
//! inverse filter (`DisUnFilter`) and the tagged-chunk stream driver
//! (`C_DisPack.cpp`) are not, so nothing is wired to `DISPACK_METHOD::decompress`
//! yet and the C decoder still runs.
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

#![allow(dead_code)] // WIP: tables land before the filter that uses them

pub mod decode;
pub mod filter;
pub mod tables;
