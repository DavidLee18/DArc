//! REP decoder edge cases that the differential test does not reach cheaply:
//! malformed streams must return an error, never panic, since the decoder runs
//! on raw archive bytes via `arc t`. (Byte-for-byte equivalence to the C
//! encoder's output is proven separately by rust/difftest/rep_ref.cpp.)
#![allow(dropping_copy_types, dropping_references, clippy::drop_non_drop)] // see darc-codecs/src/lib.rs

use darc_codecs::rep;
use darc_codecs::ffi::Io;
use std::ffi::{c_char, c_int, c_void, CStr};

struct Mem { input: Vec<u8>, pos: usize, out: Vec<u8> }
unsafe extern "C" fn cb(what: *const c_char, buf: *mut c_void, size: c_int, aux: *mut c_void) -> c_int {
    let m = &mut *(aux as *mut Mem);
    let what = CStr::from_ptr(what).to_bytes();
    let size = if size < 0 { return -1 } else { size as usize };
    if what == b"read" {
        let n = size.min(m.input.len() - m.pos);
        if n > 0 { std::ptr::copy_nonoverlapping(m.input[m.pos..].as_ptr(), buf as *mut u8, n); m.pos += n; }
        n as c_int
    } else if what == b"write" {
        if size > 0 { m.out.extend_from_slice(std::slice::from_raw_parts(buf as *const u8, size)); }
        size as c_int
    } else { 0 }
}
fn decompress(stream: &[u8]) -> c_int {
    let mut mem = Mem { input: stream.to_vec(), pos: 0, out: Vec::new() };
    let io = unsafe { Io::new(Some(cb), &mut mem as *mut Mem as *mut c_void) }.unwrap();
    rep::decompress(&io, 64 * 1024 * 1024)
}
fn le(v: u32) -> [u8; 4] { v.to_le_bytes() }

#[test]
fn empty_input_errors_without_panicking() {
    // No BlockSize word to read.
    assert!(decompress(&[]) < 0);
}

#[test]
fn zero_block_size_is_rejected() {
    assert!(decompress(&le(0)) < 0);
}

#[test]
fn lone_block_size_then_eof_marker_is_clean() {
    // BlockSize, then ComprSize==0 (EOF): a valid empty stream.
    let mut s = Vec::new();
    s.extend_from_slice(&le(1 << 20));
    s.extend_from_slice(&le(0));
    assert_eq!(decompress(&s), darc_codecs::ffi::OK);
}

#[test]
fn compr_size_below_minimum_is_rejected() {
    let mut s = Vec::new();
    s.extend_from_slice(&le(1 << 20));
    s.extend_from_slice(&le(4)); // < 2*sizeof(int32)
    assert!(decompress(&s) < 0);
}

#[test]
fn garbage_blocks_never_panic() {
    let mut seed = 0x1234u32;
    for _ in 0..200 {
        seed = seed.wrapping_mul(1103515245).wrapping_add(12345);
        let n = (seed >> 20) as usize % 400;
        let mut s = le(1 << 20).to_vec();
        s.extend((0..n).map(|i| ((seed >> (i % 24)) & 0xff) as u8));
        // Any outcome is acceptable except a panic / hang.
        drop(decompress(&s));
    }
}

#[test]
fn num_that_overflows_the_tables_is_rejected() {
    // A block whose `num` claims more table entries than the block can hold.
    let mut s = le(1 << 20).to_vec();
    let block = [le(0x7fff_ffff), le(0), le(0)].concat(); // huge num, tiny block
    s.extend_from_slice(&le(block.len() as u32));
    s.extend_from_slice(&block);
    assert!(decompress(&s) < 0);
}

// Encode -> decode round-trip through the ported functions. The byte-exact
// match to the C encoder is proven in rust/difftest/rep_ref.cpp; this is the
// cheap in-crate guard that the two halves agree.
fn compress(input: &[u8]) -> Vec<u8> {
    let mut mem = Mem { input: input.to_vec(), pos: 0, out: Vec::new() };
    let io = unsafe { Io::new(Some(cb), &mut mem as *mut Mem as *mut c_void) }.unwrap();
    // REP defaults from REP_METHOD::REP_METHOD().
    let rc = rep::compress(&io, 64 << 20, 100, 512, i32::MAX, 512, 0, 1);
    assert!(rc >= 0, "compress returned {rc}");
    mem.out
}

#[test]
fn encode_decode_round_trips() {
    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n).map(|_| { s = s.wrapping_mul(1103515245).wrapping_add(12345); (s >> 16) as u8 }).collect()
    }
    let blk = prng(2, 2000);
    let cases: Vec<Vec<u8>> = vec![
        Vec::new(),
        b"hello".to_vec(),
        prng(1, 20000),                                   // incompressible
        [blk.clone(), prng(3, 5000), blk.clone(), blk.clone()].concat(), // long repeats
        vec![0u8; 100000],                                // extreme repetition
        b"the quick brown fox ".repeat(5000),
    ];
    for (i, input) in cases.iter().enumerate() {
        let packed = compress(input);
        let mut mem = Mem { input: packed, pos: 0, out: Vec::new() };
        let io = unsafe { Io::new(Some(cb), &mut mem as *mut Mem as *mut c_void) }.unwrap();
        let rc = rep::decompress(&io, 64 * 1024 * 1024);
        assert!(rc >= 0, "case {i}: decompress returned {rc}");
        assert_eq!(mem.out, *input, "case {i}: round-trip mismatch ({} bytes)", input.len());
    }
}

/// The dictionary size is the first word of the stream, so it is
/// attacker-controlled, and `vec![0u8; n]` is infallible: a stream claiming a
/// 4 GiB dictionary used to get one attempted. That is harmless where the OS
/// hands back lazy zero pages -- measured at 1.85 MB max RSS on macOS, which is
/// why it never showed up as a crash -- and an abort through
/// `handle_alloc_error` under strict overcommit, a cgroup limit, or a 32-bit
/// target.
///
/// `dict` and `lzp` were already bounded this way; rep was missed when
/// `archive_sized_buffer` was introduced.
#[test]
fn an_absurd_dictionary_size_is_refused_rather_than_allocated() {
    let eof = le(0);
    for declared in [u32::MAX, 1 << 31, (64 * 1024 * 1024) + (1 << 20) + 2048] {
        let mut s = le(declared).to_vec();
        s.extend_from_slice(&eof);
        assert!(
            decompress(&s) < 0,
            "a stream declaring a {declared}-byte dictionary must be refused",
        );
    }
    // ...and the bound must not reject a legitimate one. 64 MiB is the method's
    // own default and what the difftest driver passes.
    let mut ok = le(64 * 1024 * 1024).to_vec();
    ok.extend_from_slice(&eof);
    assert_eq!(decompress(&ok), 0, "a 64 MiB dictionary is legitimate");
}

/// The per-block compressed size is bounded against the same figure.
#[test]
fn an_absurd_compressed_block_size_is_refused() {
    let mut s = le(1 << 16).to_vec();   // a small, legitimate dictionary
    s.extend_from_slice(&le(1 << 30));  // ...then a 1 GiB "compressed block"
    assert!(decompress(&s) < 0, "a compressed block far past the dictionary must be refused");
}

/// Issue #165. `data` is a CIRCULAR buffer of the rep block size, so once the
/// input is longer than that block, a match can reach back past the start of
/// the current cycle and its source lands *ahead* of the write position, in the
/// tail the previous cycle wrote. The stream spells such an offset cyclically
/// and the decoder unwraps it by subtracting the block size -- which makes the
/// offset negative, and made the v2.0.0 bounds check (`offset <= 0`) reject the
/// block as corrupt.
///
/// Every existing round-trip here passes a 64 MiB block and at most 100 KB of
/// input, so the buffer never wrapped and nothing noticed. The reported case
/// was two files, 113 MB, under the default `-m4` -- whose chain is `rep:96m`.
///
/// This test is deliberately expressed in the same terms: an input several
/// times the block size, with repeats long enough (>= 512 bytes) and near
/// enough (< block size) for rep to match across the wrap. Reverting the fix
/// turns it red with `-7` on the first wrapped block.
#[test]
fn a_match_across_a_buffer_wrap_round_trips() {
    fn prng(seed: u32, n: usize) -> Vec<u8> {
        let mut s = seed;
        (0..n).map(|_| { s = s.wrapping_mul(1103515245).wrapping_add(12345); (s >> 16) as u8 }).collect()
    }
    const BLOCK: u32 = 1 << 16;

    // ~5 cycles of the buffer, with a 4 KB chunk recurring every 8 KB: every
    // match is well under the block size, and most of them straddle a wrap.
    let chunk = prng(7, 4000);
    let mut input = Vec::new();
    for i in 0..40u32 {
        input.extend_from_slice(&chunk);
        input.extend_from_slice(&prng(1000 + i, 4000));
    }
    assert!(input.len() > 4 * BLOCK as usize, "the buffer has to wrap for this to test anything");

    let mut mem = Mem { input: input.clone(), pos: 0, out: Vec::new() };
    let io = unsafe { Io::new(Some(cb), &mut mem as *mut Mem as *mut c_void) }.unwrap();
    let rc = rep::compress(&io, BLOCK, 100, 512, i32::MAX, 512, 0, 1);
    assert!(rc >= 0, "compress returned {rc}");
    let packed = mem.out;
    // ...and it has to have found matches, or the wrapped offset never occurs
    // and the round-trip below proves nothing.
    assert!(packed.len() < input.len() * 3 / 4, "rep found no matches: {} => {}", input.len(), packed.len());

    let mut mem = Mem { input: packed, pos: 0, out: Vec::new() };
    let io = unsafe { Io::new(Some(cb), &mut mem as *mut Mem as *mut c_void) }.unwrap();
    let rc = rep::decompress(&io, BLOCK);
    assert!(rc >= 0, "decompress returned {rc} on a stream whose matches wrap the buffer");
    assert_eq!(mem.out, input, "round-trip mismatch across a buffer wrap");
}

/// The bound the wrap fix replaced the broken one with: an offset is spelled
/// cyclically, so it must land in `[1, block_size)`. Outside that the source is
/// not a position in the buffer at all, and the block is corrupt.
#[test]
fn an_offset_outside_the_cyclic_range_is_still_rejected() {
    // BlockSize=1<<16; one block with num=1, an empty leading literal run, a
    // match whose offset is >= the block size, then an empty trailing run.
    for offset in [0u32, 1 << 16, 1 << 30, 0xffff_ffff] {
        let block = [le(1), le(16), le(offset), le(0), le(0)].concat();
        let mut s = le(1 << 16).to_vec();
        s.extend_from_slice(&le(block.len() as u32));
        s.extend_from_slice(&block);
        s.extend_from_slice(&le(0));
        assert!(decompress(&s) < 0, "offset {offset} must be refused");
    }
}
