//! Decompressing a service block in memory — `decompressInMemory`
//! (`ArhiveStructure.hs:365`).
//!
//! The chain is stored in **compression** order, so decompression walks it
//! backwards: `process compbuf compsize (reverse keyed_compressor)`. A two-link
//! chain `delta+lzma` was written by delta first and lzma second, so lzma comes
//! off first.
//!
//! Only the last step knows the true output size; the intermediate buffers are
//! sized the way the Haskell sizes them, `max compsize (origsize*2 + 100kb)`.
//! That bound is generous rather than exact because an intermediate stage's
//! output length is not recorded anywhere in the archive.

use crate::codec_io;
use crate::method::{LzmaParams, Method};

/// What stopped a block from decompressing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// The method string did not parse — `parse_LZMA` returning NULL, in effect.
    BadMethod(String),
    /// A method this port cannot decode yet, named so the caller can say which.
    Unsupported(String),
    /// The codec refused the data.
    Codec { method: String, detail: String },
    /// The block decompressed, but not to the size the descriptor promised.
    /// `archiveBlockReadAll` treats this exactly like a CRC failure, and so
    /// should any caller: a short block is a corrupt block.
    WrongSize { expected: usize, got: usize },
    /// The block's unpacked bytes do not match `blCRC`.
    BadCrc { expected: u32, got: u32 },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::BadMethod(m) => write!(f, "unparseable compression method {m:?}"),
            Error::Unsupported(m) => write!(f, "compression method {m:?} is not supported yet"),
            Error::Codec { method, detail } => write!(f, "{method} failed: {detail}"),
            Error::WrongSize { expected, got } => {
                write!(f, "decompressed to {got} bytes, expected {expected}")
            }
            Error::BadCrc { expected, got } => {
                write!(f, "CRC {got:08x}, expected {expected:08x}")
            }
        }
    }
}

impl std::error::Error for Error {}

/// Undo one compression step.
///
/// Every arm but LZMA goes through [`codec_io::run`], driving the same
/// `darc-codecs` entry point the C archiver drives — the only version that has
/// been differential-tested byte-for-byte against the original C.
fn undo(method: &Method, src: &[u8], hint: usize) -> Result<Vec<u8>, Error> {
    match method {
        // aNO_COMPRESSION: the bytes are already the data.
        Method::Storing => Ok(src.to_vec()),
        // LZMA is the exception: DArc writes no header, so it needs the property
        // bytes rebuilt from the method string rather than a callback.
        Method::Lzma(params) => undo_lzma(*params, src, hint),
        Method::Ppmd(p) => {
            drive("ppmd", src, hint, |io| {
                darc_codecs::ppmd::decompress(io, p.order, p.mem, p.mr_method)
            })
        }
        Method::Tornado(_) => {
            drive("tor", src, hint, darc_codecs::tornado::decode::decompress)
        }
        Method::Rep(_) => drive("rep", src, hint, darc_codecs::rep::decompress),
        Method::Grzip(_) => drive("grzip", src, hint, darc_codecs::grzip::stream::decompress),
        // The BCJ filter runs in the decode direction here.
        Method::Exe => drive("exe", src, hint, |io| {
            darc_codecs::bcj::de_compress(io, darc_codecs::bcj::Direction::Decode)
        }),
        Method::Dict(p) => {
            drive("dict", src, hint, |io| darc_codecs::dict::decompress(io, p.block_size))
        }
        Method::Lzp(p) => drive("lzp", src, hint, |io| {
            darc_codecs::lzp::decompress(
                io,
                p.block_size,
                p.min_match_len,
                p.hash_size_log,
                p.barrier,
                p.smallest_len,
            )
        }),
        Method::Delta(p) => drive("delta", src, hint, |io| {
            darc_codecs::delta::decompress(io, p.block_size, p.extended_tables)
        }),
        Method::Dispack(p) => drive("dispack", src, hint, |io| {
            darc_codecs::dispack::decode::decompress(io, p.block_size)
        }),
        // 4x4 recurses: its chunks are decoded by whatever inner method its
        // parameters named, through this same function.
        Method::FourX4(p) => crate::fourx4::decode(p, src, |inner, chunk, orig| {
            undo(inner, chunk, orig)
        })
        .map_err(|e| match e {
            crate::fourx4::Error::Inner(inner) => inner,
            crate::fourx4::Error::ChunkSize { expected, got } => {
                Error::WrongSize { expected, got }
            }
            crate::fourx4::Error::Framing(cause) => {
                Error::Codec { method: "4x4".to_string(), detail: cause.to_string() }
            }
        }),
        Method::Unsupported(name) => Err(Error::Unsupported(name.clone())),
    }
}

/// Run one `darc-codecs` decoder over a buffer, naming it if it fails.
fn drive<F>(name: &str, src: &[u8], hint: usize, f: F) -> Result<Vec<u8>, Error>
where
    F: FnOnce(&darc_codecs::ffi::Io) -> core::ffi::c_int,
{
    codec_io::run(src, hint, f)
        .map_err(|e| Error::Codec { method: name.to_string(), detail: e.to_string() })
}

fn undo_lzma(params: LzmaParams, src: &[u8], hint: usize) -> Result<Vec<u8>, Error> {
    use darc_lzma::{SliceIn, VecOut};
    let props = params.props();
    let mut source = SliceIn::new(src);
    let mut sink = VecOut { data: Vec::with_capacity(hint) };
    match darc_lzma::decode_stream::decode_stream(&mut source, &mut sink, &props) {
        Ok(_) => Ok(sink.data),
        Err(e) => Err(Error::Codec { method: "lzma".to_string(), detail: format!("{e:?}") }),
    }
}

/// `decompressInMemory` — run `compressor` backwards over `src`.
///
/// `orig_size` is what the descriptor claims the result will be; it sizes the
/// final buffer and is checked by [`read_block`], not here, so that a caller
/// wanting the bytes without the check can have them.
pub fn decompress_chain(
    compressor: &[String],
    src: &[u8],
    orig_size: usize,
) -> Result<Vec<u8>, Error> {
    let chain = Method::parse_chain(compressor)
        .ok_or_else(|| Error::BadMethod(compressor.join("+")))?;
    // Stored blocks skip the machinery entirely, as the Haskell's
    // `if compressor == aNO_COMPRESSION` does.
    if chain.iter().all(|m| *m == Method::Storing) {
        return Ok(src.to_vec());
    }
    let mut buf = src.to_vec();
    for (i, method) in chain.iter().enumerate().rev() {
        // `if null algorithms then (origsize, mainPool) else (max compsize
        // (origsize*2+100kb), tempPool)` -- only the final step knows the real
        // size; the intermediates get the Haskell's bound.
        let hint = if i == 0 {
            orig_size
        } else {
            src.len().max(orig_size.saturating_mul(2).saturating_add(100 * 1024))
        };
        buf = undo(method, &buf, hint)?;
    }
    Ok(buf)
}

/// `archiveBlockReadAll` — decompress a service block and check it.
///
/// Both checks are the Haskell's, and both matter: a block that decompresses to
/// the wrong length has been silently truncated, and a block with the wrong CRC
/// has been silently altered. Either one makes everything downstream
/// meaningless, so neither is a warning.
pub fn read_block(
    compressor: &[String],
    src: &[u8],
    orig_size: usize,
    expected_crc: u32,
) -> Result<Vec<u8>, Error> {
    let out = decompress_chain(compressor, src, orig_size)?;
    if out.len() != orig_size {
        return Err(Error::WrongSize { expected: orig_size, got: out.len() });
    }
    let got = crate::crc::calc(&out);
    if got != expected_crc {
        return Err(Error::BadCrc { expected: expected_crc, got });
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// The encode side
// ---------------------------------------------------------------------------

/// Compress a buffer with one method — the inverse of [`undo`].
///
/// Only the methods the writer needs so far. `aDEFAULT_DIR_COMPRESSION` is
/// `"lzma:bt4:1m"` (`Options.hs:376`), which canonicalises to the
/// `lzma:1mb:mf=BT4` every archive's directory and footer block carries, so
/// LZMA is required even to write an otherwise uncompressed `-m0` archive.
pub fn compress_one(method: &Method, src: &[u8]) -> Result<Vec<u8>, Error> {
    compress_one_with(method, src, false)
}

/// As [`compress_one`], but stating whether the caller is 4x4.
///
/// `compress_all_at_once` is a global the archiver sets, and `_4x4_METHOD::compress`
/// forces it to 1 for the whole inner call (`C_4x4.cpp:571`) because a chunk is
/// always a complete buffer. A TOP-LEVEL method sees the archiver's own value,
/// which is 0 for a solid block.
///
/// It is not a performance knob: it changes Tornado's output. `-mtor` on a
/// 438 KiB corpus differs by five bytes between the two settings, and the
/// archive is valid either way.
pub fn compress_one_with(method: &Method, src: &[u8], all_at_once: bool) -> Result<Vec<u8>, Error> {
    match method {
        Method::Storing => Ok(src.to_vec()),
        Method::Tornado(p) => {
            // PackMethod crosses the C ABI by value, so every field must be
            // present -- including caching_finder/hash3/shift, which no method
            // string can set and which come from the preset row alone.
            let m = darc_codecs::tornado::encode::PackMethod {
                number: p.number as core::ffi::c_int,
                encoding_method: p.encoding_method as core::ffi::c_int,
                find_tables: p.find_tables != 0,
                hash_row_width: p.hash_row_width as core::ffi::c_int,
                hashsize: p.hashsize,
                caching_finder: p.caching_finder as core::ffi::c_int,
                buffer: p.buffer,
                match_parser: p.match_parser as core::ffi::c_int,
                hash3: p.hash3 as core::ffi::c_int,
                shift: p.shift as core::ffi::c_int,
                update_step: p.update_step as core::ffi::c_int,
                auxhash_size: p.auxhash_size,
                auxhash_row_width: p.auxhash_row_width as core::ffi::c_int,
            };
            // `compress_all_at_once` is 1 inside 4x4 (C_4x4.cpp:571 sets it for
            // the whole call) and 0 otherwise. A chunk handed to the inner
            // method is always a whole buffer, so this is the 4x4 case.
            drive_enc("tor", src, |io| {
                darc_codecs::tornado::encode::compress(m, io, all_at_once)
            })
        }
        Method::Lzma(p) => {
            let props = darc_lzma::LzmaProps {
                lc: p.lit_context_bits as u8,
                lp: p.lit_pos_bits as u8,
                pb: p.pos_state_bits as u8,
                dict_size: p.dictionary_size,
                fb: p.num_fast_bytes,
                // 0 is DArc's "auto" sentinel and must be resolved from the
                // match finder and fb -- taking it literally underflows the
                // search's cut counter. A NON-zero value is the user's and must
                // be passed through.
                //
                // Always resolving looked right because auto_mc(32) on the
                // default HT4 finder is exactly 16, which is what -m4's `mc16`
                // asks for. -m3 asks for `mc8` and differed by 185 bytes.
                mc: if p.match_finder_cycles != 0 {
                    p.match_finder_cycles
                } else {
                    match_finder(p.match_finder).auto_mc(p.num_fast_bytes)
                },
                mf: match_finder(p.match_finder),
                num_threads: 1,
                // algorithm 1 is the optimal parser; 0 would be fast_mode.
                fast_mode: p.algorithm == 0,
                // parse_LZMA documents "eos" as ignored because the marker is
                // always written, and the decoder expects it.
                write_end_mark: true,
            };
            darc_lzma::encode(src, &props)
                .map_err(|e| Error::Codec { method: "lzma".to_string(), detail: format!("{e:?}") })
        }
        // Named rather than a wildcard, per the crate's exhaustiveness rule: a
        // method added later must show up as a compile error here, not be
        // silently reported as unsupported for writing.
        Method::FourX4(p) => {
            crate::fourx4::encode(p, src, |m, chunk| compress_one_with(m, chunk, true))
        }
        Method::Ppmd(p) => drive_enc("ppmd", src, |io| {
            darc_codecs::ppmd::compress(io, p.order, p.mem, p.mr_method)
        }),
        Method::Rep(p) => drive_enc("rep", src, |io| {
            darc_codecs::rep::compress(
                io,
                p.block_size,
                p.min_compression as core::ffi::c_int,
                p.min_match_len as core::ffi::c_int,
                p.barrier as core::ffi::c_int,
                p.smallest_len as core::ffi::c_int,
                p.hash_size_log as core::ffi::c_int,
                p.amplifier as core::ffi::c_int,
            )
        }),
        Method::Grzip(p) => drive_enc("grzip", src, |io| {
            darc_codecs::grzip::stream::compress(
                io,
                p.method as core::ffi::c_int,
                p.block_size as core::ffi::c_int,
                core::ffi::c_int::from(p.enable_lzp),
                p.min_match_len as core::ffi::c_int,
                p.hash_size_log as core::ffi::c_int,
                core::ffi::c_int::from(p.alternative_bwt_sort),
                core::ffi::c_int::from(p.adaptive_block_size),
                core::ffi::c_int::from(p.delta_filter),
            )
        }),
        Method::Exe => drive_enc("exe", src, |io| {
            darc_codecs::bcj::de_compress(io, darc_codecs::bcj::Direction::Encode)
        }),
        Method::Dict(p) => drive_enc("dict", src, |io| {
            darc_codecs::dict_encode::compress(
                io,
                p.block_size,
                p.min_compression as core::ffi::c_int,
                p.min_weak_chars as core::ffi::c_int,
                p.min_large_cnt as core::ffi::c_int,
                p.min_medium_cnt as core::ffi::c_int,
                p.min_small_cnt as core::ffi::c_int,
                p.min_ratio as core::ffi::c_int,
            )
        }),
        Method::Lzp(p) => drive_enc("lzp", src, |io| {
            darc_codecs::lzp::compress(
                io,
                p.block_size,
                p.min_compression as core::ffi::c_int,
                p.min_match_len,
                p.hash_size_log,
                p.barrier,
                p.smallest_len,
            )
        }),
        Method::Delta(p) => drive_enc("delta", src, |io| {
            darc_codecs::delta::compress(io, p.block_size, p.extended_tables)
        }),
        Method::Dispack(p) => drive_enc("dispack", src, |io| {
            darc_codecs::dispack::encode::compress(io, p.block_size)
        }),
        other @ Method::Unsupported(_) => Err(Error::Unsupported(format!("{other:?}"))),
    }
}

/// Run one `darc-codecs` ENCODER over a buffer, naming it if it fails.
fn drive_enc<F>(name: &str, src: &[u8], f: F) -> Result<Vec<u8>, Error>
where
    F: FnOnce(&darc_codecs::ffi::Io) -> core::ffi::c_int,
{
    crate::codec_io::run(src, src.len(), f)
        .map_err(|e| Error::Codec { method: name.to_string(), detail: e.to_string() })
}

/// The match-finder ids `parse_LZMA` stores, as the encoder's enum.
fn match_finder(id: u32) -> darc_lzma::MatchFinderKind {
    use darc_lzma::MatchFinderKind as M;
    match id {
        0 => M::Bt2,
        1 => M::Bt3,
        2 => M::Bt4,
        3 => M::Hc4,
        // kHT4 -- DArc's default, and Hc5 on the Rust side.
        _ => M::Hc5,
    }
}

/// Compress with a whole chain, in order.
pub fn compress_chain(compressor: &[String], src: &[u8]) -> Result<Vec<u8>, Error> {
    let chain = Method::parse_chain(compressor)
        .ok_or_else(|| Error::BadMethod(compressor.join("+")))?;
    let mut buf = src.to_vec();
    for method in &chain {
        buf = compress_one(method, &buf)?;
    }
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The encoder side of the same parameters, for round-trip tests only.
    /// Production never builds these -- decoding needs the five property bytes
    /// and nothing else.
    fn encoder_props(p: LzmaParams) -> darc_lzma::LzmaProps {
        darc_lzma::LzmaProps {
            lc: p.lit_context_bits as u8,
            lp: p.lit_pos_bits as u8,
            pb: p.pos_state_bits as u8,
            dict_size: p.dictionary_size,
            fb: p.num_fast_bytes,
            mc: 48,
            mf: darc_lzma::MatchFinderKind::Bt4,
            num_threads: 1,
            fast_mode: false,
            // DArc always writes the end-of-stream marker (`"eos"` is
            // documented as ignored in parse_LZMA because it is unconditional),
            // and decode_stream expects it.
            write_end_mark: true,
        }
    }

    #[test]
    fn a_stored_block_is_its_own_data() {
        let data = b"the bytes themselves".to_vec();
        let out = decompress_chain(&["storing".to_string()], &data, data.len()).expect("stored");
        assert_eq!(out, data);
    }

    /// Round-trip through the real encoder, so this checks the props path -- the
    /// part that has no header to fall back on.
    #[test]
    fn lzma_round_trips_with_props_rebuilt_from_the_method_string() {
        let params = crate::method::LzmaParams { dictionary_size: 1 << 20, ..Default::default() };
        let data: Vec<u8> = (0..40_000u32).map(|i| (i / 7 % 251) as u8).collect();
        let packed = darc_lzma::encode(&data, &encoder_props(params)).expect("encodes");
        let out = undo_lzma(params, &packed, data.len()).expect("decodes");
        assert_eq!(out, data);
    }

    /// The wrong dictionary size means the wrong property bytes, and LZMA has
    /// no header here to notice with. It must fail loudly rather than return
    /// plausible garbage.
    #[test]
    fn the_wrong_props_do_not_silently_produce_garbage() {
        let data: Vec<u8> = (0..20_000u32).map(|i| (i % 97) as u8).collect();
        let right = crate::method::LzmaParams { dictionary_size: 1 << 20, ..Default::default() };
        let packed = darc_lzma::encode(&data, &encoder_props(right)).expect("encodes");

        // Same stream, decoded with lc/lp/pb from a different method string.
        let wrong = crate::method::LzmaParams {
            dictionary_size: 1 << 20,
            lit_context_bits: 0,
            pos_state_bits: 0,
            ..Default::default()
        };
        match undo_lzma(wrong, &packed, data.len()) {
            Err(_) => {}
            Ok(out) => assert_ne!(out, data, "wrong props decoded to the right bytes"),
        }
    }

    #[test]
    fn an_unsupported_method_is_named() {
        let err = decompress_chain(&["tta".to_string()], b"x", 1).expect_err("refuses");
        assert_eq!(err, Error::Unsupported("tta".to_string()));
    }

    #[test]
    fn a_short_block_is_refused_not_returned() {
        let data = b"12345".to_vec();
        let err = read_block(&["storing".to_string()], &data, 99, 0).expect_err("refuses");
        assert_eq!(err, Error::WrongSize { expected: 99, got: 5 });
    }

    #[test]
    fn a_bad_crc_is_refused() {
        let data = b"12345".to_vec();
        let err = read_block(&["storing".to_string()], &data, 5, 0xDEAD).expect_err("refuses");
        match err {
            Error::BadCrc { expected, .. } => assert_eq!(expected, 0xDEAD),
            other @ (Error::BadMethod(_)
            | Error::Unsupported(_)
            | Error::Codec { .. }
            | Error::WrongSize { .. }) => panic!("{other:?}"),
        }
    }

    #[test]
    fn a_correct_stored_block_passes_both_checks() {
        let data = b"exactly these bytes".to_vec();
        let crc = crate::crc::calc(&data);
        let out = read_block(&["storing".to_string()], &data, data.len(), crc).expect("passes");
        assert_eq!(out, data);
    }
}


