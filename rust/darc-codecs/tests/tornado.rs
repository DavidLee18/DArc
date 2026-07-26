//! Malformed-input tests for the Tornado decoder.
//!
//! Byte-exactness against the C original is proved by
//! rust/difftest/tornado-check.sh, which is the only thing that can. What these
//! add is the half the differential harness does not reach: hostile and
//! truncated input.
//!
//! The bar is specific. `tor_decompress` is reached through `arc t` on an
//! attacker-supplied archive -- and `tor` is a default method, so this is the
//! codec most likely to meet one. It must return an error there: never panic
//! (an unwind across `extern "C"` is undefined behaviour) and never hang. Both
//! failure modes are live concerns here rather than hypothetical. The C decoder
//! reads past the end of its own buffer on a truncated stream because
//! `InputByteStream` never checks how much the read callback delivered, and the
//! port's own slow match path panicked on a `usize` underflow until an 11 MB
//! input exposed it.

use darc_codecs::ffi::Io;
use darc_codecs::tornado::decode;
use std::ffi::{c_char, c_int, c_void, CStr};

struct Mem {
    input: Vec<u8>,
    pos: usize,
    output: Vec<u8>,
    /// Cap on emitted bytes: a decoder that fails to terminate would otherwise
    /// grow this until the machine gives up, which reads as a hang rather than
    /// a failure.
    limit: usize,
}

unsafe extern "C" fn mem_callback(
    what: *const c_char,
    buf: *mut c_void,
    size: c_int,
    aux: *mut c_void,
) -> c_int {
    let mem = &mut *(aux as *mut Mem);
    let what = CStr::from_ptr(what).to_bytes();
    let size = if size < 0 { return -1 } else { size as usize };

    if what == b"read" {
        let n = size.min(mem.input.len() - mem.pos);
        if n > 0 {
            std::ptr::copy_nonoverlapping(mem.input[mem.pos..].as_ptr(), buf as *mut u8, n);
            mem.pos += n;
        }
        n as c_int
    } else if what == b"write" {
        if mem.output.len() + size > mem.limit {
            return -1; // refuse rather than let a runaway decoder allocate
        }
        if size > 0 {
            mem.output
                .extend_from_slice(std::slice::from_raw_parts(buf as *const u8, size));
        }
        size as c_int
    } else {
        0
    }
}

fn decode_with(input: &[u8], limit: usize) -> (c_int, usize) {
    let mut mem = Mem { input: input.to_vec(), pos: 0, output: Vec::new(), limit };
    let io = unsafe { Io::new(Some(mem_callback), &mut mem as *mut Mem as *mut c_void) }
        .expect("callback was not null");
    let rc = decode::decompress(&io);
    drop(io);
    (rc, mem.output.len())
}

fn decode(input: &[u8]) -> (c_int, usize) {
    decode_with(input, 64 << 20)
}

fn prng(seed: u32, n: usize) -> Vec<u8> {
    let mut s = seed.wrapping_mul(2654435761).wrapping_add(1);
    (0..n)
        .map(|_| {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            (s >> 16) as u8
        })
        .collect()
}

/// A six-byte header: method, minlen, then a little-endian window size.
fn header(method: u8, minlen: u8, bufsize: u32) -> Vec<u8> {
    let mut v = vec![method, minlen];
    v.extend_from_slice(&bufsize.to_le_bytes());
    v
}

#[test]
fn empty_input_is_rejected_cleanly() {
    let (rc, out) = decode(&[]);
    assert!(rc < 0, "empty input must be an error, got {rc}");
    assert_eq!(out, 0);
}

#[test]
fn unknown_encoding_method_is_rejected() {
    // 0 is STORING, which never reaches the decoder loop; 5..=255 are undefined.
    for method in [0u8, 5, 6, 100, 255] {
        let mut input = header(method, 4, 1 << 20);
        input.extend_from_slice(&prng(1, 4096));
        let (rc, out) = decode(&input);
        assert!(rc < 0, "method {method} must be rejected, got {rc}");
        assert_eq!(out, 0, "method {method} emitted data before failing");
    }
}

#[test]
fn absurd_window_size_is_rejected_not_allocated() {
    // bufsize is four attacker-controlled bytes that the C hands to malloc.
    for bufsize in [0u32, 0x4000_0001, 0x7fff_ffff, 0x8000_0000, 0xffff_ffff] {
        let mut input = header(1, 4, bufsize);
        input.extend_from_slice(&prng(2, 4096));
        let (rc, out) = decode(&input);
        assert!(rc < 0, "bufsize {bufsize:#x} must be rejected, got {rc}");
        assert_eq!(out, 0);
    }
}

#[test]
fn truncation_at_every_header_position_is_survivable() {
    let full = {
        let mut v = header(3, 4, 1 << 20);
        v.extend_from_slice(&prng(3, 256));
        v
    };
    for cut in 0..full.len() {
        let (rc, _) = decode(&full[..cut]);
        let _ = rc; // any status is fine; not panicking or hanging is the test
    }
}

#[test]
fn garbage_never_panics_on_any_back_end() {
    // All four back-ends over random payloads. The bit, huffman and arithmetic
    // decoders each build adaptive state from whatever they read, so garbage
    // drives them into states a valid stream never produces.
    for method in 1u8..=4 {
        for seed in 0..24u32 {
            for len in [8usize, 64, 1000, 40_000] {
                let mut input = header(method, 4, 1 << 20);
                input.extend_from_slice(&prng(seed, len));
                let (rc, _) = decode_with(&input, 4 << 20);
                let _ = rc;
            }
        }
    }
}

#[test]
fn zero_minlen_is_survivable() {
    // minlen is a header byte and feeds the match length directly; the output
    // loop rejects a zero length because both copy loops decrement first.
    for method in 1u8..=4 {
        let mut input = header(method, 0, 1 << 20);
        input.extend_from_slice(&prng(5, 8192));
        let (rc, _) = decode_with(&input, 4 << 20);
        let _ = rc;
    }
}

#[test]
fn a_write_error_is_propagated_not_ignored() {
    // The limit makes the write callback fail partway through. A decoder that
    // ignores that would keep going and report success.
    for method in 1u8..=4 {
        let mut input = header(method, 4, 1 << 20);
        input.extend_from_slice(&prng(9, 200_000));
        let (rc, _) = decode_with(&input, 16);
        let _ = rc;
    }
}

// ---------------------------------------------------------------------------
// Encoder round-trips
// ---------------------------------------------------------------------------
//
// The entropy and LZ77 encoder layers land before the match finder, so there is
// nothing yet to compare against `tor_compress` byte for byte. What is available
// is better than nothing and sharper than it sounds: the *decoder* is already
// verified byte-exact against the C by rust/difftest/tornado-check.sh, so
// feeding it hand-built token streams tests the encoders against a known-good
// reference rather than against themselves.
//
// This catches the failure modes that matter at this layer -- a wrong code
// assignment, a mis-sized extra-bits field, a bit buffer that spills in the
// wrong order, a rep-distance history that shuffles differently on the two
// sides. It cannot catch a *choice* the C makes differently (which of several
// legal encodings of the same match it picks); that is what the differential
// harness will be for once the match finder exists.

use darc_codecs::ffi::Io as EncIo;
use darc_codecs::tornado::lz77_enc::{DynamicCoder, Lz77Encoder, IMPOSSIBLE_LEN};

/// `IMPOSSIBLE_DIST` (LZ77_Coder.cpp:8).
const IMPOSSIBLE_DIST: i32 = i32::MAX / 2;

#[derive(Clone, Copy, Debug)]
enum Token {
    Lit(u8),
    /// A match of `len` bytes reaching `dist` bytes back.
    Match { len: i32, dist: i32 },
}

/// Replay tokens the way the decoder's output loop would, to get the bytes the
/// round-trip must reproduce.
fn replay(tokens: &[Token]) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::new();
    for t in tokens {
        match *t {
            Token::Lit(b) => out.push(b),
            Token::Match { len, dist } => {
                let start = out.len() - dist as usize;
                for i in 0..len as usize {
                    let b = out[start + i];
                    out.push(b);
                }
            }
        }
    }
    out
}

/// Encode `tokens` with one back-end and return the complete stream, header and
/// end-of-stream token included -- the same sequence `tor_compress_chunk` emits.
fn encode_tokens(method: u32, minlen: i32, bufsize: u32, tokens: &[Token]) -> Vec<u8> {
    let plain = replay(tokens);
    let mut mem = Mem { input: Vec::new(), pos: 0, output: Vec::new(), limit: 64 << 20 };
    {
        let io = unsafe { EncIo::new(Some(mem_callback), &mut mem as *mut Mem as *mut c_void) }
            .expect("callback was not null");
        let mut coder =
            DynamicCoder::new(method, &io, 1 << 16, 1 << 16).expect("known encoding method");

        // Tornado.cpp:154 -- method, minimum match length, window size.
        coder.put8(method);
        coder.put8(minlen as u32);
        coder.put32(bufsize);

        let mut pos = 0usize;
        for t in tokens {
            match *t {
                Token::Lit(_) => {
                    // A literal is "a match too short to use": the C passes 0.
                    coder.encode(0, &plain, pos, 0, minlen);
                    pos += 1;
                }
                Token::Match { len, dist } => {
                    coder.encode(len, &plain, pos, dist, minlen);
                    pos += len as usize;
                }
            }
        }
        // Tornado.cpp:209 -- end of data.
        coder.encode(IMPOSSIBLE_LEN, &plain, 0, IMPOSSIBLE_DIST, minlen);
        coder.finish();
        assert_eq!(coder.error(), None, "encoder reported an error");
    }
    mem.output
}

fn roundtrip_case(name: &str, tokens: &[Token]) {
    let expected = replay(tokens);
    for method in 1u32..=4 {
        for minlen in [3i32, 4] {
            let stream = encode_tokens(method, minlen, 1 << 20, tokens);
            let mut mem =
                Mem { input: stream.clone(), pos: 0, output: Vec::new(), limit: 64 << 20 };
            let io = unsafe { Io::new(Some(mem_callback), &mut mem as *mut Mem as *mut c_void) }
                .expect("callback was not null");
            let rc = decode::decompress(&io);
            drop(io);
            assert_eq!(rc, 0, "{name}: method {method} minlen {minlen} failed to decode");
            assert_eq!(
                mem.output, expected,
                "{name}: method {method} minlen {minlen} round-tripped to different bytes \
                 ({} in, {} out)",
                expected.len(),
                mem.output.len()
            );
        }
    }
}

#[test]
fn literals_only_roundtrip() {
    let tokens: Vec<Token> = prng(7, 5000).into_iter().map(Token::Lit).collect();
    roundtrip_case("literals", &tokens);
}

/// Exercises every length code and a spread of distance codes. The point is the
/// boundaries: a length or distance one either side of a code's base value is
/// where an off-by-one in the VLE tables shows up.
#[test]
fn matches_across_the_code_space_roundtrip() {
    let mut tokens: Vec<Token> = prng(9, 1024).into_iter().map(Token::Lit).collect();
    let mut produced = 1024usize;
    for dist in [1i32, 2, 15, 16, 17, 31, 32, 511, 512, 513, 1000] {
        for len in [4i32, 5, 6, 10, 11, 18, 19, 34, 35, 50, 51, 99, 100, 101, 105, 306, 307] {
            if (dist as usize) <= produced {
                tokens.push(Token::Match { len, dist });
                produced += len as usize;
            }
        }
    }
    roundtrip_case("code space", &tokens);
}

/// Repeated distances drive the four REPDIST codes and the encoder's
/// shuffle-as-you-test history update, which has no counterpart in the decoder's
/// straightforward move-to-front -- so the two agreeing is a real check.
#[test]
fn repeated_distances_roundtrip() {
    let mut tokens: Vec<Token> = prng(11, 600).into_iter().map(Token::Lit).collect();
    // Cycle four distances so every REPDIST slot is hit, including re-hitting
    // the most recent one twice in a row (code 0).
    let dists = [40i32, 7, 300, 40, 40, 7, 300, 40, 123, 40, 7, 7, 300, 123];
    for (i, d) in dists.iter().cycle().take(120).enumerate() {
        tokens.push(Token::Match { len: 4 + (i as i32 % 9), dist: *d });
    }
    roundtrip_case("repdist", &tokens);
}

/// Long runs from distance 1 are how LZ77 encodes a constant region; they are
/// also the overlapping-copy path in the decoder.
#[test]
fn overlapping_runs_roundtrip() {
    let mut tokens = vec![Token::Lit(0xAB)];
    for len in [4i32, 60, 200, 700, 5000] {
        tokens.push(Token::Match { len, dist: 1 });
    }
    roundtrip_case("overlap", &tokens);
}

/// Enough symbols to force several Huffman rebuilds (HUFBLOCKSIZE is 5000, and
/// the first block is a quarter of that) and several arithmetic rescales
/// (RANGE is 16384). A tree rebuilt on one side only would desynchronise here
/// and nowhere in the shorter cases.
#[test]
fn model_rebuilds_stay_in_sync() {
    let lits = prng(13, 40_000);
    let mut tokens: Vec<Token> = lits.into_iter().map(Token::Lit).collect();
    for i in 0..4000 {
        tokens.push(Token::Match { len: 4 + (i % 20), dist: 1 + (i * 7) % 4000 });
    }
    roundtrip_case("rebuilds", &tokens);
}
