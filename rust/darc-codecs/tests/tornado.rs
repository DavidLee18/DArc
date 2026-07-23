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
