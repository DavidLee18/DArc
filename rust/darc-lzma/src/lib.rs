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

mod price;
mod props;
mod rangecoder;
mod state;

mod matchfinder;
mod optimum;
pub mod stream;

mod encoder;

#[cfg(any(test, feature = "decode"))]
mod decoder;

#[cfg(test)]
mod roundtrip_tests;

pub use props::LzmaProps;
pub use stream::{InStream, OutStream, SliceIn, StreamError, VecOut};

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

/// Decode a raw LZMA stream (no header, no end marker) of known output length.
///
/// A port of `LzmaDec` provided for round-trip self-tests. Available to external
/// consumers only with the `decode` feature enabled.
#[cfg(any(test, feature = "decode"))]
pub fn decode_raw(input: &[u8], props: &[u8; 5], out_len: usize) -> Vec<u8> {
    decoder::decode_raw(input, props, out_len)
}
