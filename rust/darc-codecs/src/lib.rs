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
#![deny(clippy::wildcard_enum_match_arm)]
#![deny(clippy::todo, clippy::unimplemented, clippy::mem_forget)]
#![deny(unused_must_use)]

pub mod bsc;
pub mod delta;
pub mod dict;
pub mod dict_encode;
pub mod dispack;
pub mod grzip;
pub mod lz4;
pub mod lz4hc;
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
