//! REP decoder edge cases that the differential test does not reach cheaply:
//! malformed streams must return an error, never panic, since the decoder runs
//! on raw archive bytes via `arc t`. (Byte-for-byte equivalence to the C
//! encoder's output is proven separately by rust/difftest/rep_ref.cpp.)

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
    rep::decompress(&io)
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
        let _ = decompress(&s);
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
