//! `4x4` — the multithreaded chunking meta-codec (`Compression/4x4/C_4x4.cpp`).
//!
//! Every DArc level from `-m1` upward wraps its real compressor in this:
//! `4x4:tor:3:434kb`, `4x4:b16mb:lzma:379kb:mc16`. It splits a solid block into
//! chunks, compresses each independently with an inner method, and writes them
//! with enough framing to be decompressed independently again. Without it the
//! port can read `-m0`, `-mtor` and `-mppmd` and nothing else.
//!
//! # Wire format
//!
//! From the comment at the top of `C_4x4.cpp`, confirmed against `do_decompress`
//! (`:436`):
//!
//! ```text
//!   version   : int32, currently 0
//!   per chunk:
//!     orig_size : int32   -- -1 means the payload is stored raw
//!     comp_size : int32   -- bytes of payload that follow
//!     payload   : comp_size bytes
//!   ... until EOF. There is no terminator: the reader stops when the
//!   8-byte header read returns 0.
//! ```
//!
//! # Where the parallelism goes
//!
//! The C runs a worker pool with a bounded queue and hands results to a writer
//! thread in submission order. The chunk boundaries are **in the stream**, so
//! this port instead walks the headers serially — which costs one pass over 8
//! bytes per chunk and nothing else — and then decodes every chunk at once,
//! collecting in index order.
//!
//! That is a stronger guarantee than the C's, not a weaker one: output order is
//! fixed by the collection, not by a queue discipline, so no scheduling decision
//! can reach the bytes. The C's `num_threads` is therefore *not* honoured, and
//! deliberately: it is a resource knob, and the archive is identical either way.

use crate::bytestream::{self, InStream, OutStream};
use crate::method::{parse_int, parse_mem, Method};
use rayon::prelude::*;

/// `_4x4_VERSION`.
const VERSION: u32 = 0;
/// `_4x4_HEADER_SIZE`.
const HEADER_SIZE: usize = 8;

/// `_4x4_METHOD`'s parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FourX4Params {
    /// 0 means "unset" — the C then derives a buffer size from the inner
    /// method's dictionary. It only ever sizes a buffer, never the format.
    pub block_size: u32,
    /// Also only a resource knob. Kept so a method string round-trips.
    pub num_threads: u32,
    /// The inner method, as a string, and parsed.
    pub inner_name: String,
    pub inner: Box<Method>,
}

/// `parse_4x4` (`C_4x4.cpp:623`).
///
/// The parameter that starts the inner method is found by a test worth
/// transcribing exactly: `!(isdigit(param[0]) || isdigit(param[1]))`. So `b8mb`
/// is a block size (digit at index 1), `3` is a thread count, and `tor` — no
/// digit in either position — begins the inner method, which swallows **all**
/// remaining parameters joined by ':'.
pub fn parse(params: &[&str]) -> Option<FourX4Params> {
    // `_4x4_METHOD::_4x4_METHOD()` (:540) defaults to "tor:3".
    let mut block_size = 0u32;
    let mut num_threads = 0u32;
    let mut inner_name = "tor:3".to_string();

    let mut i = 0usize;
    while i < params.len() {
        let param = params[i];
        let b = param.as_bytes();
        let digit0 = b.first().is_some_and(u8::is_ascii_digit);
        let digit1 = b.get(1).is_some_and(u8::is_ascii_digit);
        if !(digit0 || digit1) {
            // This parameter and every one after it are the inner method.
            inner_name = params[i..].join(":");
            break;
        }
        match b.first() {
            Some(b'b') => {
                block_size = parse_mem(&param[1..])?;
                i += 1;
                continue;
            }
            Some(b't') => {
                num_threads = parse_int(&param[1..])?;
                i += 1;
                continue;
            }
            Some(_) | None => {}
        }
        // A bare integer is the thread count; a bare memory size is the block
        // size. The C tries them in that order and clears the error between.
        match parse_int(param) {
            Some(n) => num_threads = n,
            None => block_size = parse_mem(param)?,
        }
        i += 1;
    }

    let inner = Method::parse(&inner_name)?;
    Some(FourX4Params { block_size, num_threads, inner_name, inner: Box::new(inner) })
}

/// One chunk as the stream describes it.
#[derive(Clone, Copy, Debug)]
struct Chunk {
    /// Byte range of the payload within the 4x4 stream.
    at: usize,
    comp_size: usize,
    /// `None` for a raw-stored chunk (`orig_size == -1`).
    orig_size: Option<usize>,
}

/// Walk the chunk headers. Cheap, serial, and the only part that must be.
fn chunks(body: &[u8]) -> Result<Vec<Chunk>, bytestream::Error> {
    let mut s = InStream::new(body);
    let version = s.u32()?;
    if version != VERSION {
        // A version this build does not know is corrupt data, not a truncation;
        // report it as a length error at offset 0 rather than inventing a
        // variant the bytestream layer has no other use for.
        return Err(bytestream::Error::ImplausibleLength {
            at: 0,
            len: u64::from(version),
            remaining: body.len(),
        });
    }
    let mut out = Vec::new();
    loop {
        if s.remaining() == 0 {
            // EOF is the terminator. There is no sentinel record.
            return Ok(out);
        }
        if s.remaining() < HEADER_SIZE {
            return Err(bytestream::Error::Truncated {
                at: s.pos(),
                want: HEADER_SIZE,
                have: s.remaining(),
            });
        }
        let orig = s.u32()? as i32;
        let comp = s.u32()? as i32;
        if comp < 0 {
            return Err(bytestream::Error::ImplausibleLength {
                at: s.pos(),
                len: comp as u32 as u64,
                remaining: s.remaining(),
            });
        }
        let comp_size = comp as usize;
        if comp_size > s.remaining() {
            return Err(bytestream::Error::Truncated {
                at: s.pos(),
                want: comp_size,
                have: s.remaining(),
            });
        }
        // -1 is the raw marker. Any OTHER negative value is corruption and is
        // refused here.
        //
        // This used to be `Some(orig.max(0) as usize)`, which silently turned
        // -5 into "a chunk that unpacks to nothing" and left a comment claiming
        // the decoder would catch it. It would not: a chunk whose inner method
        // happens to produce zero bytes passes that check, and the corruption
        // reaches the caller as a short solid block.
        let orig_size = match orig {
            -1 => None,
            n if n < 0 => {
                return Err(bytestream::Error::ImplausibleLength {
                    at: s.pos(),
                    len: n as u32 as u64,
                    remaining: s.remaining(),
                })
            }
            n => Some(n as usize),
        };
        out.push(Chunk { at: s.pos(), comp_size, orig_size });
        // Step over the payload; the decoder reads it from `body` directly.
        s.skip(comp_size)?;
    }
}

/// Decode a whole 4x4 stream.
///
/// `decode_one` undoes the inner method for one chunk. It is passed in rather
/// than called directly so this module does not depend on the dispatcher that
/// depends on it.
pub fn decode<F, E>(
    params: &FourX4Params,
    body: &[u8],
    decode_one: F,
) -> Result<Vec<u8>, Error<E>>
where
    F: Fn(&Method, &[u8], usize) -> Result<Vec<u8>, E> + Sync,
    E: Send,
{
    let list = chunks(body).map_err(Error::Framing)?;

    // Every chunk at once. `collect` into a Vec preserves index order, so the
    // concatenation below is the stream's order regardless of completion order.
    let decoded: Vec<Result<Vec<u8>, Error<E>>> = list
        .par_iter()
        .map(|c| {
            let payload = body.get(c.at..c.at + c.comp_size).ok_or(Error::Framing(
                bytestream::Error::Truncated { at: c.at, want: c.comp_size, have: 0 },
            ))?;
            match c.orig_size {
                None => Ok(payload.to_vec()),
                Some(orig) => {
                    let out = decode_one(&params.inner, payload, orig).map_err(Error::Inner)?;
                    // STRICTER THAN THE C, deliberately. `do_decompress` never
                    // looks at orig_size again after the `== -1` test
                    // (C_4x4.cpp:508) -- it decompresses the payload and writes
                    // whatever comes out. The encoder always writes the true
                    // size (:269), so checking it costs nothing on a valid
                    // archive and turns a corrupt chunk into an error one
                    // 8-byte header in, rather than a CRC failure over the whole
                    // solid block with no indication of where it went wrong.
                    if out.len() != orig {
                        return Err(Error::ChunkSize { expected: orig, got: out.len() });
                    }
                    Ok(out)
                }
            }
        })
        .collect();

    let mut out = Vec::new();
    for chunk in decoded {
        out.extend_from_slice(&chunk?);
    }
    Ok(out)
}

/// Encode a whole 4x4 stream — `do_compress` (`C_4x4.cpp:374`).
///
/// The chunk size is not free: it is `BlockSize`, or the INNER method's
/// dictionary when that is unset, or 8 MB, floored at 64 KB. Choosing it
/// differently changes every chunk boundary and so every byte of the output,
/// while still decoding correctly.
///
/// A chunk whose compressed form is not SMALLER than its input is stored raw
/// (`:206`) — `>=`, not `>`, so a chunk that compresses to exactly its own size
/// is stored. The C also stores raw when the codec reports
/// `FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL`; here that arrives as any encoder
/// failure on a chunk, which is the same observable outcome.
///
/// Chunks are compressed in PARALLEL and written in index order, for the same
/// reason decoding is: the boundaries are decided before any of them runs.
pub fn encode<F, E>(
    params: &FourX4Params,
    src: &[u8],
    compress_one: F,
) -> Result<Vec<u8>, E>
where
    F: Fn(&Method, &[u8]) -> Result<Vec<u8>, E> + Sync,
    E: Send,
{
    let chunk_size = chunk_size(params) as usize;
    let chunks: Vec<&[u8]> = if src.is_empty() {
        Vec::new()
    } else {
        src.chunks(chunk_size).collect()
    };

    let packed: Vec<(bool, Vec<u8>)> = chunks
        .par_iter()
        .map(|chunk| match compress_one(&params.inner, chunk) {
            // "If compression didn't help, store raw".
            Ok(out) if out.len() < chunk.len() => (false, out),
            Ok(_) => (true, chunk.to_vec()),
            // The C's OUTBLOCK_TOO_SMALL arm: store raw rather than fail.
            Err(_) => (true, chunk.to_vec()),
        })
        .collect();

    let mut out = OutStream::new();
    out.u32(VERSION);
    for (i, (raw, payload)) in packed.iter().enumerate() {
        let orig = chunks[i].len();
        // -1 marks a raw chunk; otherwise the unpacked size.
        out.u32(if *raw { (-1i32) as u32 } else { orig as u32 });
        out.u32(payload.len() as u32);
        for b in payload {
            out.u8(*b);
        }
    }
    Ok(out.into_bytes())
}

/// `do_compress`'s block-size choice (`C_4x4.cpp:376`).
fn chunk_size(params: &FourX4Params) -> u32 {
    let mut bs = params.block_size;
    if bs == 0 {
        let dict = crate::memlimit::get_dictionary(&params.inner);
        bs = if dict > 0 { dict } else { 8 * 1024 * 1024 };
    }
    bs.max(64 * 1024)
}

/// What can go wrong decoding a 4x4 stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error<E> {
    /// The chunk framing did not parse.
    Framing(bytestream::Error),
    /// A chunk decoded to the wrong length.
    ChunkSize { expected: usize, got: usize },
    /// The inner method failed.
    Inner(E),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytestream::OutStream;

    /// Build a 4x4 stream. `chunks` is (orig_size, payload); `None` is raw.
    fn encode(chunks: &[(Option<usize>, &[u8])]) -> Vec<u8> {
        let mut o = OutStream::new();
        o.u32(VERSION);
        for (orig, payload) in chunks {
            let orig_field: i32 = match orig {
                Some(n) => *n as i32,
                None => -1,
            };
            o.u32(orig_field as u32);
            o.u32(payload.len() as u32);
            for b in *payload {
                o.u8(*b);
            }
        }
        o.into_bytes()
    }

    fn params() -> FourX4Params {
        FourX4Params {
            block_size: 0,
            num_threads: 0,
            inner_name: "storing".to_string(),
            inner: Box::new(Method::Storing),
        }
    }

    /// The identity inner decoder, so the framing is tested on its own.
    fn identity(_m: &Method, src: &[u8], _orig: usize) -> Result<Vec<u8>, ()> {
        Ok(src.to_vec())
    }

    #[test]
    fn chunks_are_concatenated_in_stream_order() {
        let a = b"first chunk";
        let b = b"second chunk";
        let c = b"third";
        let stream = encode(&[
            (Some(a.len()), a),
            (Some(b.len()), b),
            (Some(c.len()), c),
        ]);
        let out = decode(&params(), &stream, identity).expect("decodes");
        assert_eq!(out, [&a[..], &b[..], &c[..]].concat());
    }

    /// Order must come from the chunk index, not from completion. With enough
    /// chunks that rayon really splits the work, a decoder that appended as
    /// results arrived would scramble this.
    #[test]
    fn many_chunks_still_come_back_in_order() {
        let payloads: Vec<Vec<u8>> =
            (0..500u32).map(|i| format!("chunk-{i:04}|").into_bytes()).collect();
        let refs: Vec<(Option<usize>, &[u8])> =
            payloads.iter().map(|p| (Some(p.len()), p.as_slice())).collect();
        let stream = encode(&refs);
        let out = decode(&params(), &stream, identity).expect("decodes");
        assert_eq!(out, payloads.concat());
    }

    /// orig_size == -1 means the payload is already the data.
    #[test]
    fn a_raw_chunk_is_passed_through_untouched() {
        let raw = b"not compressed at all";
        let stream = encode(&[(None, raw)]);
        // An inner decoder that would corrupt anything it touched.
        let out = decode(&params(), &stream, |_m, _s, _o| -> Result<Vec<u8>, ()> {
            Ok(b"WRONG".to_vec())
        })
        .expect("decodes");
        assert_eq!(out, raw, "the raw chunk went through the inner method");
    }

    #[test]
    fn raw_and_compressed_chunks_mix() {
        let stream = encode(&[(Some(3), b"abc"), (None, b"xy"), (Some(1), b"z")]);
        let out = decode(&params(), &stream, identity).expect("decodes");
        assert_eq!(out, b"abcxyz");
    }

    #[test]
    fn an_empty_stream_is_a_valid_empty_result() {
        let stream = encode(&[]);
        let out = decode(&params(), &stream, identity).expect("decodes");
        assert!(out.is_empty());
    }

    /// A chunk that decodes to the wrong length is caught at the chunk, not
    /// deferred to a CRC failure over the whole solid block.
    #[test]
    fn a_short_chunk_is_caught_where_it_happens() {
        let stream = encode(&[(Some(100), b"only ten..")]);
        let err = decode(&params(), &stream, identity).expect_err("refuses");
        assert_eq!(err, Error::ChunkSize { expected: 100, got: 10 });
    }

    /// A negative orig_size that is not the -1 raw marker is corruption, and
    /// must be refused rather than clamped to zero.
    #[test]
    fn a_negative_chunk_size_other_than_the_raw_marker_is_refused() {
        for bad in [-2i32, -5, i32::MIN] {
            let mut o = OutStream::new();
            o.u32(VERSION);
            o.u32(bad as u32);
            o.u32(3);
            o.u8(b'a');
            o.u8(b'b');
            o.u8(b'c');
            let stream = o.into_bytes();
            match decode(&params(), &stream, identity) {
                Err(Error::Framing(_)) => {}
                other => panic!("orig_size {bad} was accepted: {other:?}"),
            }
        }
        // ...while -1 remains the raw marker and is fine.
        let stream = encode(&[(None, b"abc")]);
        assert_eq!(decode(&params(), &stream, identity).expect("raw"), b"abc");
    }

    #[test]
    fn a_wrong_version_is_refused() {
        let mut stream = encode(&[(Some(1), b"x")]);
        stream[0] = 7;
        assert!(matches!(
            decode(&params(), &stream, identity),
            Err(Error::Framing(_))
        ));
    }

    /// Truncation anywhere is an error, never a panic or a silent short read.
    #[test]
    fn every_truncation_is_refused() {
        let stream = encode(&[(Some(5), b"hello"), (Some(5), b"world")]);
        for n in 0..stream.len() {
            match decode(&params(), &stream[..n], identity) {
                Err(_) => {}
                Ok(out) => assert!(
                    out.len() < 10,
                    "a {n}-byte prefix produced the whole {} byte result",
                    out.len()
                ),
            }
        }
    }

    // ---- the parser ------------------------------------------------------

    /// The method strings real archives contain, from `arc lt` on -m1..-m9.
    #[test]
    fn the_four_by_four_strings_real_archives_use_all_parse() {
        let cases: [(&str, u32, &str); 4] = [
            // (params after "4x4", expected block size, expected inner method)
            ("tor:3:434kb", 0, "tor:3:434kb"),
            ("tor:6:379kb:h2mb", 0, "tor:6:379kb:h2mb"),
            ("b8mb:lzma:379kb:a0:mc8", 8 * 1024 * 1024, "lzma:379kb:a0:mc8"),
            ("b254mb:lzma:379kb", 254 * 1024 * 1024, "lzma:379kb"),
        ];
        for (s, block, inner) in cases {
            let parts: Vec<&str> = s.split(':').collect();
            let p = parse(&parts).unwrap_or_else(|| panic!("{s} did not parse"));
            assert_eq!(p.block_size, block, "block size of {s}");
            assert_eq!(p.inner_name, inner, "inner method of {s}");
            assert!(
                !matches!(*p.inner, Method::Unsupported(_)),
                "{inner} is not supported"
            );
        }
    }

    /// The inner method swallows every remaining parameter. Stopping at the
    /// first one would give "lzma" with default 64 MB dictionary instead of
    /// "lzma:379kb" -- wrong property bytes, and LZMA has no header to notice.
    #[test]
    fn the_inner_method_takes_all_remaining_parameters() {
        let p = parse(&["b8mb", "lzma", "379kb", "a0", "mc8"]).expect("parses");
        assert_eq!(p.inner_name, "lzma:379kb:a0:mc8");
        assert!(
            matches!(*p.inner, Method::Lzma(l) if l.dictionary_size == 379 * 1024),
            "inner method was {:?}, not lzma with a 379kb dictionary",
            p.inner
        );
    }

    /// `!(isdigit(param[0]) || isdigit(param[1]))` -- so "b8mb" is a parameter
    /// (digit at index 1) and "tor" starts the inner method.
    #[test]
    fn a_digit_in_either_of_the_first_two_places_means_a_parameter() {
        let p = parse(&["t4", "b1mb", "tor"]).expect("parses");
        assert_eq!(p.num_threads, 4);
        assert_eq!(p.block_size, 1024 * 1024);
        assert_eq!(p.inner_name, "tor");
        // A bare number is the thread count.
        let p = parse(&["8", "tor"]).expect("parses");
        assert_eq!(p.num_threads, 8);
        assert_eq!(p.inner_name, "tor");
    }

    /// No parameters at all means the constructor's "tor:3".
    #[test]
    fn the_default_inner_method_is_tor_3() {
        let p = parse(&[]).expect("parses");
        assert_eq!(p.inner_name, "tor:3");
    }
}
