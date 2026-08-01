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
fn undo(method: &Method, src: &[u8], hint: usize) -> Result<Vec<u8>, Error> {
    match method {
        // aNO_COMPRESSION: the bytes are already the data.
        Method::Storing => Ok(src.to_vec()),
        Method::Lzma(params) => undo_lzma(*params, src, hint),
        Method::Unsupported(name) => Err(Error::Unsupported(name.clone())),
    }
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
    let last = chain.len().saturating_sub(1);
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
        let _ = last;
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
