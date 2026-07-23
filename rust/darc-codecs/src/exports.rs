//! C entry points.
//!
//! These carry the same names and signatures as the C originals so a codec can
//! be swapped in at link time. Note the C declarations in Delta.cpp have C++
//! linkage -- there is no `extern "C"` anywhere in that file -- so C_Delta.cpp
//! currently links against a mangled symbol and will NOT pick these up. Adding
//! `extern "C"` to the declarations in C_Delta.h (a linkage-only change) is
//! what wires them together; until then these are exercised by the differential
//! harness rather than by the archiver.

use crate::{delta, dict, dict_encode, lzp, rep};
use crate::ffi::{Io, CALLBACK_FUNC, FREEARC_ERRCODE_GENERAL};
use core::ffi::{c_int, c_void};

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_delta_compress(
    block_size: u32,
    extended_tables: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => delta::compress(&io, block_size, extended_tables),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

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

// ---------------------------------------------------------------------------
// Drop-in replacements under the archiver's own symbol names.
//
// Delta.cpp defines delta_compress/delta_decompress with C++ linkage, so these
// C-linkage symbols do not collide with it: they are distinct names as far as
// the linker is concerned. Which one the archiver calls is decided entirely by
// how C_Delta.h declares them -- `extern "C"` picks these, the default picks
// the C++ originals. That is what makes the swap reversible with a build flag
// rather than a source deletion.
// ---------------------------------------------------------------------------

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn delta_compress(
    block_size: u32,
    extended_tables: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_delta_compress(block_size, extended_tables, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn delta_decompress(
    block_size: u32,
    extended_tables: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_delta_decompress(block_size, extended_tables, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_dict_decompress(
    block_size: u32,
    _min_compression: c_int,
    _min_weak_chars: c_int,
    _min_large_cnt: c_int,
    _min_medium_cnt: c_int,
    _min_small_cnt: c_int,
    _min_ratio: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => dict::decompress(&io, block_size),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name; see the note above on why the
/// switch is an exclusion in C_Dict.cpp rather than a redeclaration.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn dict_decompress(
    block_size: u32,
    a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_dict_decompress(block_size, a, b, c, d, e, f, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_lzp_decompress(
    block_size: u32,
    _min_compression: c_int,
    min_match_len: c_int,
    hash_size_log: c_int,
    barrier: c_int,
    smallest_len: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => lzp::decompress(&io, block_size, min_match_len, hash_size_log, barrier, smallest_len),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_lzp_compress(
    block_size: u32,
    min_compression: c_int,
    min_match_len: c_int,
    hash_size_log: c_int,
    barrier: c_int,
    smallest_len: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => lzp::compress(&io, block_size, min_compression, min_match_len, hash_size_log, barrier, smallest_len),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol names; see the note above on why the
/// switch is an exclusion in C_LZP.cpp rather than a redeclaration.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn lzp_compress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_lzp_compress(block_size, a, b, c, d, e, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn lzp_decompress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_lzp_decompress(block_size, a, b, c, d, e, callback, auxdata)
}

/// REP decoder. The seven tuning knobs match `rep_decompress`; only the block
/// size that matters is stored in the stream, so the rest are accepted and
/// ignored, exactly as the C decode path does.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_rep_decompress(
    block_size: u32, min_compression: c_int, min_match_len: c_int, barrier: c_int,
    smallest_len: c_int, hash_bits: c_int, amplifier: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => rep::decompress_full(
            &io, block_size, min_compression, min_match_len, barrier, smallest_len, hash_bits, amplifier),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn rep_decompress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_rep_decompress(block_size, a, b, c, d, e, f, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_dict_compress(
    block_size: u32, min_compression: c_int, min_weak_chars: c_int, min_large: c_int,
    min_medium: c_int, min_small: c_int, min_ratio: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => dict_encode::compress(&io, block_size, min_compression, min_weak_chars,
                                          min_large, min_medium, min_small, min_ratio),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn dict_compress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_dict_compress(block_size, a, b, c, d, e, f, callback, auxdata)
}
