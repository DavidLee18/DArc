//! PPMd var.H, ported from `Compression/PPMD/`.
//!
//! Unlike the other codecs here, this one admits no algorithmic latitude: the
//! model branches on its own allocator's state, so the suballocator's layout is
//! part of the compressed format. See [`suballoc`] for the measurement that
//! establishes this.

pub mod coder;
pub mod model;
pub mod stream;
pub mod suballoc;

use crate::ffi::{Io, FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, OK};
use core::ffi::c_int;
use model::Model;
use stream::PrimeStream;

/// `ppmd_compress` (`C_PPMD.cpp:37`).
///
/// The C builds TWO `PRIME_STREAM`s over the same callback -- one that issues
/// "read" requests and one that issues "write" -- and that pairing is kept
/// here, since each carries its own 64 KB buffer.
pub fn compress(io: &Io, order: i32, mem: u32, mr_method: i32) -> c_int {
    let mut m = Model::new();
    if !m.sa.start(mem as usize) {
        return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY as c_int;
    }
    let mut fp_in = PrimeStream::new_reader(io);
    let mut fp_out = PrimeStream::new_writer(io);
    m.encode_file(&mut fp_out, &mut fp_in, order, mr_method);
    fp_out.flush();
    let mut err = OK;
    if fp_in.error() < 0 { err = fp_in.error(); }
    if fp_out.error() < 0 { err = fp_out.error(); }
    m.sa.stop();
    err
}

/// `ppmd_decompress` (`C_PPMD.cpp:75`).
pub fn decompress(io: &Io, order: i32, mem: u32, mr_method: i32) -> c_int {
    let mut m = Model::new();
    if !m.sa.start(mem as usize) {
        return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY as c_int;
    }
    let mut fp_in = PrimeStream::new_reader(io);
    let mut fp_out = PrimeStream::new_writer(io);
    m.decode_file(&mut fp_out, &mut fp_in, order, mr_method);
    fp_out.flush();
    let mut err = OK;
    if fp_in.error() < 0 { err = fp_in.error(); }
    if fp_out.error() < 0 { err = fp_out.error(); }
    m.sa.stop();
    err
}
