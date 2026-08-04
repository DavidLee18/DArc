//! Rust ports of DArc's codecs, exposed under the original C ABI.
//!
//! Each codec is swapped in individually: the Rust symbol replaces the C one at
//! link time, everything else in the build stays put, and the archive
//! fingerprint suite proves the output is byte-identical before the C version
//! is removed. Bit-exactness is not optional -- a codec that compresses
//! "correctly" but differently produces archives older builds cannot read,
//! which is the highest-risk failure mode in this repository.

//! ## Lint gates
//!
//! These are `deny`, not `warn`, and each one guards a way this crate has
//! actually been bitten or could be silently bitten:
//!
//! * `clippy::wildcard_enum_match_arm` -- a `_ =>` over an enum silently
//!   absorbs variants added later. This is the compile-time half of the lesson
//!   from the Tornado presets 7-11 bug, where a silent fallback (a trait default
//!   whose body was `debug_assert!`, dead in release) changed the encoder's
//!   output with nothing failing. As the codecs' bare mode integers become enums
//!   (RUST_PORT_PROGRESS.md section 10b, item 4), this lint is what converts each
//!   one into enforced exhaustiveness.
//! * `clippy::todo`, `clippy::unimplemented` -- placeholders that panic at run
//!   time in a library whose failures corrupt archives.
//! * `clippy::mem_forget` -- leaking a codec buffer is never intended here.
//! * `unused_must_use` -- these codecs return `c_int` status codes constantly;
//!   an ignored one is a swallowed error.
//!
//! Deliberately NOT denied: `unreachable_pub` (84 pre-existing hits) and the
//! ~169 style warnings the port carries. Those are noise to fix, not hazards,
//! and denying them would mean touching byte-exact code for cosmetics.
//! ## Why `single_match` is allowed
//!
//! Totality here is `wildcard_enum_match_arm`: a `_ =>` over an enum silently
//! absorbs variants added later, so every arm has to be named. That is the one
//! style rule CI enforces, and it is about the ARMS of a match, not about
//! choosing `match` over `if let`.
//!
//! `if let` and `let _` were both banned once, by CI grep, on the wider theory
//! that an exhaustive `match` says more than either. Those bans are gone. This
//! crate is still written mostly in `match` form and there is no need to churn
//! it, but neither spelling is a build failure now.
//!
//! `clippy::single_match` stays allowed all the same: it is warn-by-default and
//! would fire on a large amount of existing code that reads perfectly well,
//! which is noise rather than a finding.
#![deny(clippy::wildcard_enum_match_arm)]
#![deny(clippy::todo, clippy::unimplemented, clippy::mem_forget)]
#![deny(unused_must_use)]
#![allow(clippy::single_match)]
//! ## No `unwrap` / `expect` in production paths
//!
//! `cfg_attr(not(test), ...)`, so the deny applies to the normal library build
//! while unit-test modules -- which are compiled with `cfg(test)` -- keep both.
//! Integration tests under `tests/` are separate crates and never see this
//! attribute at all.
//!
//! The reason is specific to this crate: every entry point is reached across a C
//! ABI, and an unwind across `extern "C"` is undefined behaviour. A panic here is
//! not a crash report, it is a corrupted process. `src/bin/` is deliberately NOT
//! covered -- those are dev helpers, separate crate targets, where panicking on a
//! failed stdin read is the right behaviour.
#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
//! ## `drop_non_drop` is allowed, and it is a direct consequence of a CI rule
//!
//! This workspace bans `let _ = expr;` and `let _name = expr;` -- a discard that
//! reads as a binding, and that silences `unused_must_use`. The replacement is
//! `drop(expr)`. Rustc's `dropping_copy_types` (and clippy's `drop_non_drop`)
//! fire on exactly that when the value is `Copy` or has no destructor, and the
//! suggested fix is `let _ = expr` --
//! the form the CI grep rejects. One of the two has to give, and it is this
//! lint: the CI rule catches a class of bug that has actually occurred here (a
//! computed comparison thrown away), while `drop_non_drop` only objects to the
//! spelling of a deliberate discard.
#![allow(dropping_copy_types, dropping_references, clippy::drop_non_drop)]

pub mod bcj;
pub mod bsc;
pub mod delta;
pub mod dict;
pub mod dict_encode;
pub mod dispack;
pub mod grzip;
pub mod lz4;
pub mod lz4hc;
pub mod lzma;
pub mod lzp;
pub mod ppmd;
pub mod mm;
pub mod mmdet;
pub mod rep;
pub mod srep;
pub mod tornado;
pub mod tta;
pub mod zstd;
pub mod ffi;

mod exports;
