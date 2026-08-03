//! # lzma-sdk-rs
//!
//! A pure-Rust port of the **7-zip LZMA SDK 23.01** single-threaded LZMA encoder
//! whose output is **byte-identical** to the C `LzmaEnc_MemEncode(...)` for the
//! same input and properties.
//!
//! The emitted stream is a **raw LZMA stream**: no 13-byte `.lzma` file header
//! and no end-of-stream marker, exactly matching `LzmaEnc_MemEncode` called with
//! `writeEndMark = 0`. The 5 decoder property bytes are produced separately by
//! [`decoder_props`] (the equivalent of `LzmaEnc_WriteProperties`).
//!
//! This crate is consumed by `chd-rs` and others that need to *recreate* the exact
//! bytes 7-zip / MAME's CHD codec would produce. See `CLAUDE.md` for the porting
//! map, the bit-exactness hazards, and the differential-test rig.
//!
//! ## Status
//!
//! [`encode`] is **byte-exact** with the C `LzmaEnc_MemEncode` across a broad
//! corpus, verified out-of-tree against the C oracle (see
//! `docs/comparing-against-the-c-oracle.md`). The match finder, optimal parser,
//! decoder, and symbol layer are all complete. See `ROADMAP.md`.
//! ## `drop_non_drop` is allowed, and it is a direct consequence of a CI rule
//!
//! This workspace bans `let _ = expr;` and `let _name = expr;` -- a discard that
//! reads as a binding, and that silences `unused_must_use`. The replacement is
//! `drop(expr)`. Clippy's `drop_non_drop` fires on exactly that when the value
//! is `Copy` or has no destructor, and its suggested fix is `let _ = expr` --
//! the form the CI grep rejects. One of the two has to give, and it is this
//! lint: the CI rule catches a class of bug that has actually occurred here (a
//! computed comparison thrown away), while `drop_non_drop` only objects to the
//! spelling of a deliberate discard.
#![allow(dropping_copy_types, dropping_references, clippy::drop_non_drop)]

mod price;
mod props;
mod rangecoder;
mod state;

mod matchfinder;
mod optimum;
pub mod stream;

mod encoder;

pub mod lzma2_dec;
pub mod lzma2_enc;
pub mod lzma2_mt;

pub mod decode_stream;

#[cfg(test)]
mod roundtrip_tests;

pub use decode_stream::{Decoder, DecodeSummary, Finish, LzmaDecProps, LzmaDecodeError};
pub use lzma2_dec::{Lzma2Dec, Lzma2DecodeSummary};
pub use lzma2_enc::{DEFAULT_MT_MEMORY_BUDGET, Lzma2Enc, Lzma2EncProps, Lzma2Error, RawLzmaProps};
pub use props::{LzmaProps, MatchFinderKind};
pub use stream::{ERR_UNSUPPORTED, InStream, OutStream, SliceIn, StreamError, VecOut};

/// Encode `input` into a raw LZMA stream that is byte-identical to
/// `LzmaEnc_MemEncode` for the same `props`.
///
/// The returned bytes carry **no** `.lzma` header. Whether an end-of-payload
/// marker is emitted is [`LzmaProps::write_end_mark`]; obtain the 5 decoder
/// property bytes with [`decoder_props`].
///
/// This holds the whole input and the whole output in memory. For a solid block,
/// use [`encode_stream`], whose cost is O(dictionary).
pub fn encode(input: &[u8], props: &LzmaProps) -> Result<Vec<u8>, StreamError> {
    encoder::encode(input, props)
}

/// Encode from an [`InStream`] to an [`OutStream`], byte-identical to
/// `LzmaEnc_Encode` for the same `props`.
///
/// This is the entry point DArc uses. The match finder holds a sliding window of
/// `dict_size` plus slack rather than the whole input, and the range coder stages
/// its output 64 KiB at a time, so neither side is bounded by the block size.
pub fn encode_stream(
    source: &mut dyn InStream,
    sink: &mut dyn OutStream,
    props: &LzmaProps,
) -> Result<(), StreamError> {
    encoder::encode_stream(source, sink, props)
}

/// The 5 decoder property bytes for `props`, identical to
/// `LzmaEnc_WriteProperties`.
///
/// Byte 0 packs `(pb*5 + lp)*9 + lc`; bytes 1..5 are the little-endian *aligned*
/// dictionary size (see [`LzmaProps::decoder_props`] — the encoder rounds the
/// dictionary up before writing it, it does not emit the raw `dict_size`).
pub fn decoder_props(props: &LzmaProps) -> [u8; 5] {
    props.decoder_props()
}

/// Decode a raw LZMA stream (no header) of known output length, for round-trip
/// self-tests.
///
/// A thin wrapper over [`decode_stream`], which is the real decoder. It used to be a
/// second, separate `LzmaDec` port kept only for tests — that one required a known
/// output length, held all output rather than a bounded window, and had panics
/// reachable from archive input (an unvalidated props byte indexing a 16-entry table,
/// unchecked match distances, truncated input fed zeros). Keeping a defective decoder
/// compiled next to a hardened one is how the defective one ends up called, so it is
/// gone; this preserves only the test-facing shape.
///
/// `out_len` is retained for the callers' assertions and is not needed to decode: the
/// stream ends at its end-of-payload marker, or at the end of the input.
#[cfg(any(test, feature = "decode"))]
pub fn decode_raw(input: &[u8], props: &[u8; 5], _out_len: usize) -> Vec<u8> {
    let mut source = SliceIn::new(input);
    let mut sink = VecOut::default();
    match decode_stream::decode_stream(&mut source, &mut sink, props) {
        Ok(_) => sink.data,
        // The callers compare against the original input, so a failure surfaces as a
        // mismatch with a diff rather than as a silent truncation.
        Err(_) => sink.data,
    }
}
