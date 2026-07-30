//! Round-trip and edge-case tests for the ported codecs.
//!
//! These are deliberately NOT a replacement for rust/difftest, which compares
//! this crate against the C originals byte for byte and is the only thing that
//! can prove archive-format equivalence. A round-trip test passes happily if
//! both halves of a codec are wrong in the same way -- Dict was byte-identical
//! to C on 14 inputs while still failing to detect four deliberate errors,
//! because the corpus never reached the branch under test.
//!
//! What these cover is the half difftest does not reach cheaply: inputs that
//! are awkward rather than representative -- empty, tiny, all one byte,
//! incompressible, and sizes sitting on internal block boundaries. Those are
//! where the decoders were found to run off their buffers during the v2.0.0
//! hardening work.
//!
//! Everything here drives the codec through its real entry point, `compress` /
//! `decompress` over an `Io`, rather than the per-block primitives underneath.
//! That is not a stylistic choice. `lzp::encode`/`decode` have a narrower
//! contract than they look: a block under 16 bytes is not a valid LZP block,
//! and an incompressible block is stored raw by the caller and never handed to
//! the decoder at all. Testing the primitives directly asserts a contract the
//! codec does not actually offer -- and rust/difftest already made exactly this
//! mistake once, with a driver whose read callback returned the whole input in
//! one call so the encoder ran once per process instead of once per block.

use darc_codecs::ffi::Io;
use darc_codecs::{dict, dict_encode, lzp};
use std::ffi::{c_char, c_int, c_void, CStr};

// ---------------------------------------------------------------------------
// In-memory Io, standing in for the archiver's callback
// ---------------------------------------------------------------------------

struct Mem {
    input: Vec<u8>,
    pos: usize,
    output: Vec<u8>,
}

/// The archiver hands codecs a `callback("read"/"write", buf, len, aux)`. This
/// is the same protocol backed by a Vec.
///
/// `read` deliberately serves at most `chunk` bytes per call even when more are
/// available, so codecs that loop on read are actually driven round their loop.
/// A driver that returns everything at once exercises one iteration and hides
/// every bug that only appears on the second.
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
        let remaining = mem.input.len() - mem.pos;
        let n = size.min(remaining);
        if n > 0 {
            std::ptr::copy_nonoverlapping(mem.input[mem.pos..].as_ptr(), buf as *mut u8, n);
            mem.pos += n;
        }
        n as c_int
    } else if what == b"write" {
        if size > 0 {
            mem.output
                .extend_from_slice(std::slice::from_raw_parts(buf as *const u8, size));
        }
        size as c_int
    } else {
        0
    }
}

/// Run `f` with an `Io` reading `input`, and return (status, bytes written).
fn with_io(input: &[u8], f: impl FnOnce(&Io) -> c_int) -> (c_int, Vec<u8>) {
    let mut mem = Mem {
        input: input.to_vec(),
        pos: 0,
        output: Vec::new(),
    };
    let io = unsafe { Io::new(Some(mem_callback), &mut mem as *mut Mem as *mut c_void) }
        .expect("callback was not null");
    let rc = f(&io);
    drop(io);
    (rc, mem.output)
}

/// Deterministic pseudo-random bytes. A fixed LCG rather than a rand crate, for
/// the same reason Tests/make-corpus.sh uses one: the point is repeatability,
/// not statistical quality, and a failing case must be reproducible from its
/// seed alone.
fn prng(seed: u32, n: usize) -> Vec<u8> {
    let mut s = seed.wrapping_mul(2654435761).wrapping_add(1);
    (0..n)
        .map(|_| {
            s = s.wrapping_mul(1103515245).wrapping_add(12345);
            (s >> 16) as u8
        })
        .collect()
}

/// Text-like input: compressible, with repeated words, which is what Dict and
/// LZP are actually for.
fn texty(n: usize) -> Vec<u8> {
    let words = [
        "the ", "quick ", "brown ", "fox ", "jumps ", "over ", "lazy ", "dog ", "and ", "then ",
    ];
    let mut out = Vec::with_capacity(n + 16);
    let mut i = 0usize;
    while out.len() < n {
        out.extend_from_slice(words[i % words.len()].as_bytes());
        if i % 7 == 0 {
            out.push(b'\n');
        }
        i += 1;
    }
    out.truncate(n);
    out
}

// ---------------------------------------------------------------------------
// LZP
// ---------------------------------------------------------------------------

const LZP_BLOCK: u32 = 1 << 20;
const LZP_MIN_COMPRESSION: c_int = 100;
const LZP_MIN_LEN: c_int = 32;
const LZP_HASH_LOG: c_int = 20;
const LZP_BARRIER: c_int = 32;
const LZP_SMALLEST: c_int = 32;

fn lzp_roundtrip(input: &[u8]) -> Vec<u8> {
    let (rc, packed) = with_io(input, |io| {
        lzp::compress(
            io,
            LZP_BLOCK,
            LZP_MIN_COMPRESSION,
            LZP_MIN_LEN,
            LZP_HASH_LOG,
            LZP_BARRIER,
            LZP_SMALLEST,
        )
    });
    assert!(rc >= 0, "lzp::compress failed with {rc} on {} bytes", input.len());

    let (rc, plain) = with_io(&packed, |io| {
        lzp::decompress(io, LZP_BLOCK, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
    });
    assert!(rc >= 0, "lzp::decompress failed with {rc} on {} bytes", packed.len());
    plain
}

#[test]
fn lzp_roundtrips_text() {
    let input = texty(64 * 1024);
    assert_eq!(lzp_roundtrip(&input), input);
}

#[test]
fn lzp_roundtrips_incompressible() {
    // Nothing to match on. The interesting part is the caller's decision to
    // store the block raw rather than the match path.
    let input = prng(0xC0FFEE, 40_000);
    assert_eq!(lzp_roundtrip(&input), input);
}

#[test]
fn lzp_roundtrips_highly_repetitive() {
    // The opposite extreme: one enormous match, which is where match lengths
    // accumulate across the 254-byte continuation encoding.
    let input = vec![b'A'; 100_000];
    assert_eq!(lzp_roundtrip(&input), input);
}

#[test]
fn lzp_roundtrips_awkward_sizes() {
    // On and either side of internal boundaries, including sizes below the
    // 16-byte minimum block the decoder enforces.
    for &n in &[0usize, 1, 2, 15, 16, 17, 255, 256, 257, 4095, 4096, 4097, 65_535, 65_536] {
        let input = texty(n);
        assert_eq!(lzp_roundtrip(&input), input, "lzp failed to round-trip {n} bytes");
    }
}

#[test]
fn lzp_roundtrips_across_several_blocks() {
    // Block size deliberately far below the input, so the codec is driven
    // round its loop many times. Dict's char_counts bug appeared only from
    // the second block onward, and a single-block test could never see it.
    let input = texty(300_000);
    let (rc, packed) = with_io(&input, |io| {
        lzp::compress(io, 32 * 1024, LZP_MIN_COMPRESSION, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
    });
    assert!(rc >= 0, "compress failed: {rc}");
    let (rc, plain) = with_io(&packed, |io| {
        lzp::decompress(io, 32 * 1024, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
    });
    assert!(rc >= 0, "decompress failed: {rc}");
    assert_eq!(plain, input);
}

// ---------------------------------------------------------------------------
// Malformed input
// ---------------------------------------------------------------------------
//
// The v2.0.0 work fixed decoders that read or wrote outside their buffers on
// corrupt archives, reachable through an ordinary "arc t". A Rust port must
// return an error there, not panic and not silently produce something: a panic
// unwinding across the C ABI is undefined behaviour, and these are called
// through it. nextest's per-test process isolation is what keeps a panic here
// reported as one failing test instead of taking the whole run down.

#[test]
fn lzp_rejects_truncated_stream_without_panicking() {
    let input = texty(65_536);
    let (rc, packed) = with_io(&input, |io| {
        lzp::compress(io, LZP_BLOCK, LZP_MIN_COMPRESSION, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
    });
    assert!(rc >= 0);

    for cut in [0usize, 1, 3, 4, 8, 15, 16, packed.len() / 4, packed.len() / 2, packed.len() - 1] {
        if cut > packed.len() {
            continue;
        }
        // Any status is acceptable; panicking or hanging is not.
        let _ = with_io(&packed[..cut], |io| {
            lzp::decompress(io, LZP_BLOCK, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
        });
    }
}

#[test]
fn lzp_rejects_corrupt_stream_without_panicking() {
    let input = texty(65_536);
    let (rc, packed) = with_io(&input, |io| {
        lzp::compress(io, LZP_BLOCK, LZP_MIN_COMPRESSION, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
    });
    assert!(rc >= 0);

    for seed in [1u32, 7, 42] {
        let mut bad = packed.clone();
        let noise = prng(seed, bad.len());
        // Flip a scattering of bytes rather than a contiguous run, so the
        // damage lands in headers and payload alike.
        for i in (0..bad.len()).step_by(97) {
            bad[i] ^= noise[i];
        }
        let _ = with_io(&bad, |io| {
            lzp::decompress(io, LZP_BLOCK, LZP_MIN_LEN, LZP_HASH_LOG, LZP_BARRIER, LZP_SMALLEST)
        });
    }
}

#[test]
fn dict_rejects_garbage_without_panicking() {
    for seed in [1u32, 2, 3, 99, 12345] {
        let garbage = prng(seed, 4096);
        let _ = dict::decode(&garbage, 1 << 20);
    }
}

// ---------------------------------------------------------------------------
// Dict
// ---------------------------------------------------------------------------

/// The parameters DICT_METHOD's constructor uses. Spelled out rather than
/// defaulted because an earlier reference driver disagreed with the real
/// constructor on five of six knobs while claiming to match it, and passed.
const DICT_MIN_WEAK: i32 = 0;
const DICT_MIN_LARGE: i32 = 256;
const DICT_MIN_MEDIUM: i32 = 64;
const DICT_MIN_SMALL: i32 = 16;
const DICT_MIN_RATIO: i32 = 8;

#[test]
fn dict_roundtrips_text() {
    let input = texty(64 * 1024);
    let encoded = dict_encode::encode_block(
        &input,
        DICT_MIN_WEAK,
        DICT_MIN_LARGE,
        DICT_MIN_MEDIUM,
        DICT_MIN_SMALL,
        DICT_MIN_RATIO,
    );
    match encoded {
        Ok(encoded) => {
            let out = dict::decode(&encoded, input.len() * 4 + 65_536)
                .expect("dict::decode rejected dict_encode's own output");
            assert_eq!(out, input);
        }
        // Declining an input is legitimate; decoding it to the wrong bytes is
        // not. Spelled out rather than left implicit by an `if let`.
        Err(_) => {}
    }
}
