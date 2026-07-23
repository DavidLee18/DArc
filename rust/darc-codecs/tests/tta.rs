//! Malformed-input tests for the TTA decoder.
//!
//! TTA is ported decode-first, so there is no in-crate round trip to assert
//! against -- byte-exactness versus the C original is proved by
//! rust/difftest/tta-check.sh, which is the only thing that can. What these add
//! is the half the differential harness does not reach: hostile and truncated
//! input.
//!
//! The bar is specific. `tta_decompress` is reached through `arc t` on an
//! attacker-supplied archive, called across the C ABI. It must return an error
//! there -- never panic (an unwind across `extern "C"` is undefined behaviour)
//! and never hang (a non-terminating Rice run must be bounded). nextest's
//! per-test process isolation is what keeps a panic here reported as one failing
//! test rather than taking the whole run down.

use darc_codecs::ffi::Io;
use darc_codecs::tta;
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
    let rc = tta::decompress(&io);
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

#[test]
fn empty_input_is_rejected_cleanly() {
    // No header at all: the first 4-byte read comes up short.
    let (_rc, out) = decode(&[]);
    assert!(out.is_empty());
}

#[test]
fn garbage_never_panics() {
    for seed in [1u32, 2, 3, 7, 42, 99, 1234, 65535] {
        for len in [1usize, 2, 3, 4, 5, 8, 16, 64, 256, 4096] {
            let _ = decode(&prng(seed, len));
        }
    }
}

#[test]
fn every_header_byte_combination_is_survivable() {
    // The header is (level, raw*2+is_float, num_chan, word_size). Sweep the
    // first two bytes over their whole range with a scattering of channel/word
    // values and some trailing bytes; each must return, not panic.
    for level in 0u8..=6 {
        for flags in 0u8..=5 {
            for &num_chan in &[0u8, 1, 2, 3, 255] {
                for &word_size in &[0u8, 8, 16, 24, 32, 255] {
                    let mut v = vec![level, flags, num_chan, word_size];
                    v.extend_from_slice(&prng(level as u32 * 7 + word_size as u32, 64));
                    let _ = decode(&v);
                }
            }
        }
    }
}

#[test]
fn valid_header_then_truncation_is_survivable() {
    // A well-formed 16-bit-stereo header (level 3), then a frame header claiming
    // a large block but with the body cut at every prefix length. The decoder
    // must treat each short read as an error, not walk off a buffer.
    let base = vec![3u8, 0, 2, 16, /*offset=*/ 0, 0, 0, 0];
    // frame header: bytes_read = 400000, then bit_array_size = 100000
    let mut frame = base.clone();
    frame.extend_from_slice(&400_000u32.to_le_bytes());
    frame.extend_from_slice(&100_000u32.to_le_bytes());
    frame.extend_from_slice(&prng(5, 5000)); // far short of 100000

    for cut in [8usize, 9, 12, 13, 16, 20, 100, frame.len()] {
        if cut <= frame.len() {
            let _ = decode(&frame[..cut]);
        }
    }
}

#[test]
fn absurd_lengths_are_rejected_not_allocated() {
    // A header followed by a frame claiming a 4 GB block. The `> 1<<30` guards
    // must reject it rather than attempt the allocation.
    let mut v = vec![3u8, 0, 2, 16, 0, 0, 0, 0];
    v.extend_from_slice(&0xFFFF_FFFFu32.to_le_bytes()); // bytes_read
    v.extend_from_slice(&0xFFFF_FFFFu32.to_le_bytes()); // bit_array_size
    v.extend_from_slice(&prng(6, 256));
    let (rc, _out) = decode(&v);
    assert!(rc < 0, "a 4 GB block claim should be rejected");
}

#[test]
fn stored_level0_copies_input_through() {
    // level == 0 means the payload is stored: everything after the 4-byte header
    // is copied verbatim to the output.
    let payload = prng(11, 5000);
    let mut v = vec![0u8, 0, 0, 0];
    v.extend_from_slice(&payload);
    let (rc, out) = decode(&v);
    assert!(rc >= 0);
    assert_eq!(out, payload, "level-0 stored stream must pass the payload through");
}

#[test]
fn stored_block_within_a_frame_copies_through() {
    // bit_array_size == 0 marks a stored block: bytes_read bytes are copied.
    let block = prng(12, 800);
    let mut v = vec![3u8, 0, 2, 16, 0, 0, 0, 0];
    v.extend_from_slice(&(block.len() as u32).to_le_bytes()); // bytes_read
    v.extend_from_slice(&0u32.to_le_bytes()); // bit_array_size == 0 -> stored
    v.extend_from_slice(&block);
    let (rc, out) = decode(&v);
    assert!(rc >= 0);
    assert_eq!(out, block, "a stored block must be copied verbatim");
}
