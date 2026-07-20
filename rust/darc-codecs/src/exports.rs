//! C entry points.
//!
//! These carry the same names and signatures as the C originals so a codec can
//! be swapped in at link time. Note the C declarations in Delta.cpp have C++
//! linkage -- there is no `extern "C"` anywhere in that file -- so C_Delta.cpp
//! currently links against a mangled symbol and will NOT pick these up. Adding
//! `extern "C"` to the declarations in C_Delta.h (a linkage-only change) is
//! what wires them together; until then these are exercised by the differential
//! harness rather than by the archiver.

use crate::delta;
use crate::ffi::{Io, CALLBACK_FUNC, FREEARC_ERRCODE_GENERAL};
use core::ffi::{c_int, c_void};

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_delta_decompress(
    block_size: u32,
    extended_tables: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    // A null callback is representable in the ABI; calling through it would be
    // undefined behaviour. bindgen types this as Option<fn> precisely so the
    // case has to be handled.
    match Io::new(callback, auxdata) {
        Some(io) => delta::decompress(&io, block_size, extended_tables),
        None => FREEARC_ERRCODE_GENERAL,
    }
}
