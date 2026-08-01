//! The archive format layer, in Rust.
//!
//! This is the port of `ByteStream.hs`, `ArhiveStructure.hs` and
//! `ArhiveDirectory.hs` — the part of the Haskell layer that decides what an
//! archive *is*. It subsumes `Unarc/` (5,389 lines of C++ that reimplement the
//! same reader for the standalone extractor and the SFX modules), which is why
//! it comes first.
//!
//! ## It is ported against the GHC semantics
//!
//! `Tests/arc` is built by MicroHs, and measurement (docs/testing.md) showed
//! that build diverges from the Haskell it appears to run:
//!
//! * `ArcvProcessCompress.hs:106` — under `__MHS__` a data block is one
//!   `darc_compress_solid_block_w` call. The Haskell compression pipeline is
//!   bypassed entirely, and no `FileStart` is ever emitted.
//! * `Arc.hs:75` — the uncaught-exception handler is compiled out with the
//!   capability setup, so errors print the MicroHs runtime's own message and
//!   exit 1 where the contract is 2.
//!
//! So `Tests/arc` is the reference for archive **bytes**, and `Tests/arc-ghc`
//! (`./compile-ghc-probe`) is the reference for **behaviour**.
//!
//! ## Concurrency
//!
//! Solid-block boundaries are decided by `splitToSolidBlocks`
//! (`ArhiveFileList.hs:291`), a **pure** function called at
//! `ArcvProcessRead.hs:104` before any concurrency exists. Measured: the
//! threaded GHC build is byte-identical to itself across `-N1/-N2/-N8` ×
//! `-mt1/-mt8`. Blocks are therefore independent, and this crate decodes and
//! decompresses them in parallel — the ordering that matters is re-imposed when
//! results are collected, never left to the scheduler.
//!
//! ## Lint gates
//!
//! Same rationale as `darc-codecs`: `wildcard_enum_match_arm` so a new block
//! type cannot be silently absorbed by a `_ =>`, and no `unwrap`/`expect`
//! outside tests because this decodes untrusted archive bytes and a panic here
//! crosses an FFI boundary.
#![deny(clippy::wildcard_enum_match_arm)]
#![deny(clippy::todo, clippy::unimplemented, clippy::mem_forget)]
#![deny(unused_must_use)]
#![allow(clippy::single_match)]
#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
#![allow(dropping_copy_types, dropping_references, clippy::drop_non_drop)] // see darc-codecs/src/lib.rs

pub mod archive;
pub mod block;
pub mod bytestream;
pub mod canonize;
pub mod codec_io;
pub mod crc;
pub mod decompress;
pub mod directory;
pub mod extract;
pub mod filetype;
pub mod fourx4;
pub mod memlimit;
pub mod method;
pub mod options;
pub mod sort;
pub mod writer;
