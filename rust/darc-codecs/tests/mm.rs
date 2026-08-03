//! Malformed-input tests for the MM decoder.
//!
//! MM is ported decode-first, so there is no in-crate round trip to assert
//! against -- byte-exactness versus the C original is proved by
//! rust/difftest/mm-check.sh, which is the only thing that can. What these add
//! is the half the differential harness does not reach: hostile, truncated and
//! encoder-unreachable headers.
//!
//! The bar is specific. `mm_decompress` is reached through `arc t` on an
//! attacker-supplied archive, called across the C ABI. It must return an error
//! there -- never panic (an unwind across `extern "C"` is undefined behaviour)
//! and never hang. The hang is not hypothetical here: a header with zero
//! channels and a nonzero word size makes the C loops advance by `p += 0`
//! forever, which is the one place this port deliberately refuses to be
//! bug-compatible. nextest's per-test process isolation is what keeps a panic
//! reported as one failing test rather than taking the whole run down.
#![allow(dropping_copy_types, dropping_references, clippy::drop_non_drop)] // see darc-codecs/src/lib.rs

use darc_codecs::ffi::Io;
use darc_codecs::mm;
use std::ffi::{c_char, c_int, c_void, CStr};

struct Mem {
    input: Vec<u8>,
    pos: usize,
    output: Vec<u8>,
}

/// Serve at most `size` bytes per `read`, exactly as the archiver's callback
/// does, so a decoder that loops on read is actually driven round its loop.
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
        if size > 0 {
            mem.output
                .extend_from_slice(std::slice::from_raw_parts(buf as *const u8, size));
        }
        size as c_int
    } else {
        0
    }
}

/// Decode `input`, returning (status, bytes written). Any status is acceptable;
/// panicking or hanging is not.
fn decode(input: &[u8]) -> (c_int, Vec<u8>) {
    let mut mem = Mem { input: input.to_vec(), pos: 0, output: Vec::new() };
    let io = unsafe { Io::new(Some(mem_callback), &mut mem as *mut Mem as *mut c_void) }
        .expect("callback was not null");
    let rc = mm::decompress(&io);
    drop(io);
    (rc, mem.output)
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

/// A filtered-branch stream: flags=1, then num_chan/word_size, a zero offset,
/// no alignment padding needed (7 bytes of header is already a multiple of
/// `sample` only sometimes -- callers pass `pad` explicitly), then payload.
fn stream(num_chan: u8, word_size: u8, offset: &[u8], pad: usize, payload: &[u8]) -> Vec<u8> {
    let mut v = vec![1, num_chan, word_size];
    v.extend_from_slice(&(offset.len() as u32).to_le_bytes());
    v.extend_from_slice(offset);
    v.resize(v.len() + pad, 0);
    v.extend_from_slice(payload);
    v
}

#[test]
fn empty_input_is_rejected_cleanly() {
    // The flags byte is mandatory; mm_compress emits nothing at all for empty
    // input, so an empty stream is not decodable by C either.
    let (rc, out) = decode(&[]);
    assert!(rc < 0, "empty input must be an error, got {rc}");
    assert!(out.is_empty());
}

#[test]
fn reserved_flag_bits_are_rejected() {
    // Bits 1-2 are the reordering that mm.cpp never finished (unreorder_bytes
    // does not exist), and everything above is unallocated. Accepting either
    // would silently emit wrongly ordered data.
    for flags in 2u8..=255 {
        let (rc, out) = decode(&[flags, 2, 16, 0, 0, 0, 0]);
        assert!(rc < 0, "flags {flags:#04x} must be rejected, got {rc}");
        assert!(out.is_empty(), "flags {flags:#04x} produced output before failing");
    }
}

#[test]
fn stored_branch_copies_input_through() {
    // flags == 0: everything after the first byte is the payload, verbatim.
    let payload = prng(3, 200_000); // spans several 64 KB reads
    let mut input = vec![0u8];
    input.extend_from_slice(&payload);
    let (rc, out) = decode(&input);
    assert_eq!(rc, 0, "stored stream must decode");
    assert_eq!(out, payload);
}

#[test]
fn zero_channels_terminates_instead_of_spinning() {
    // C's `for (p=buf; p+N<=end; p+=N)` with N==0 never advances: mm_decompress
    // hangs. This test exists to pin that this port returns instead. If it ever
    // regresses the symptom is a test that never finishes, not one that fails.
    for word_size in [8u8, 16, 24, 32] {
        let (rc, _) = decode(&stream(0, word_size, &[], 0, &prng(1, 4096)));
        assert!(rc < 0, "num_chan=0 word_size={word_size} must be rejected, got {rc}");
    }
}

#[test]
fn zero_word_size_copies_through_as_c_does() {
    // byte_size==0 leaves N==0, but roundDown/roundUp guard on `b > 1` rather
    // than dividing, and the C `switch` has no case for it -- so the payload
    // passes through untouched. That is reachable data, not a hang, so it is
    // reproduced rather than rejected.
    let payload = prng(7, 5000);
    let (rc, out) = decode(&stream(2, 0, &[], 0, &payload));
    assert_eq!(rc, 0, "word_size=0 must decode");
    assert_eq!(out, payload);
}

#[test]
fn oversized_word_size_copies_through() {
    // word_size > 32 gives byte_size 5..=32, for which no filter exists. C
    // falls out of the switch and writes the block unchanged. byte_size is 25
    // here, so the 7 header bytes round up to 25 and 18 pad bytes precede the
    // payload -- an alignment this large is only reachable by hand, which is
    // the point of covering it.
    let payload = prng(11, 8192);
    let (rc, out) = decode(&stream(1, 200, &[], 18, &payload));
    assert_eq!(rc, 0, "word_size=200 must decode");
    assert_eq!(out, payload);
}

#[test]
fn absurd_offset_is_rejected_not_allocated() {
    // A 32-bit offset field lands in a C `int`, so a hostile stream can ask for
    // a near-4 GB copy (or a negative one). Nothing legitimate exceeds the 1 MB
    // first block.
    for off in [0x4000_0001u32, 0x7fff_ffff, 0x8000_0000, 0xffff_ffff] {
        let mut input = vec![1u8, 2, 16];
        input.extend_from_slice(&off.to_le_bytes());
        input.extend_from_slice(&prng(5, 4096));
        let (rc, out) = decode(&input);
        assert!(rc < 0, "offset {off:#x} must be rejected, got {rc}");
        assert!(out.is_empty(), "offset {off:#x} emitted data before failing");
    }
}

#[test]
fn truncation_at_every_header_position_is_survivable() {
    // Cut a well-formed stream at each of the first 64 bytes: every one lands
    // inside a length or count that the rest of the decode depends on.
    let full = stream(3, 24, &[0xaa; 20], 6, &prng(13, 4096));
    for cut in 0..64.min(full.len()) {
        let (rc, _) = decode(&full[..cut]);
        assert!(rc <= 0, "truncation at {cut} returned {rc}");
    }
}

#[test]
fn garbage_never_panics() {
    // Random bytes with a forced-valid flags byte, so the filtered branch is
    // reached rather than every case failing at byte 0.
    for seed in 0..64u32 {
        for len in [8usize, 64, 1000, 70_000] {
            let mut input = prng(seed, len);
            input[0] = 1;
            let (rc, _) = decode(&input);
            drop(rc); // any status is fine; not panicking or hanging is the test
        }
    }
}

#[test]
fn every_header_byte_combination_is_survivable() {
    // num_chan x word_size, with a payload long enough to drive the filter and
    // a length that leaves a partial sample at the end for most combinations.
    let payload = prng(17, 1021);
    for num_chan in [0u8, 1, 2, 3, 4, 5, 255] {
        for word_size in [0u8, 1, 7, 8, 9, 16, 17, 24, 32, 33, 64, 255] {
            let (rc, _) = decode(&stream(num_chan, word_size, &[], 0, &payload));
            drop(rc);
        }
    }
}

#[test]
fn nonzero_alignment_padding_seeds_the_accumulators() {
    // C reads the alignment bytes *into* `base`, so a stream that puts nonzero
    // bytes there starts the running sums somewhere other than zero. The
    // encoder always writes zeros; this pins that a stream which does not is
    // still followed exactly rather than being normalised away.
    //
    // num_chan=1, word_size=8: sample size 1, so `roundUp` asks for no padding
    // and the pad bytes must NOT be consumed -- the first payload byte is data.
    let (rc, out) = decode(&stream(1, 8, &[], 0, &[10, 20, 30]));
    assert_eq!(rc, 0);
    assert_eq!(out, vec![10, 30, 60], "8-bit running sum");

    // num_chan=2, word_size=8: 7 header bytes round up to 8, so exactly one pad
    // byte is consumed. Feeding it 5 seeds channel 0 at 5.
    let (rc, out) = decode(&stream(2, 8, &[], 0, &[5, 1, 2, 3, 4]));
    assert_eq!(rc, 0);
    // pad byte 5 -> base[0]=5; then samples (1,2) and (3,4):
    //   ch0: 5+1=6, 6+3=9    ch1: 0+2=2, 2+4=6
    assert_eq!(out, vec![6, 2, 9, 6], "padding seeded channel 0");
}
