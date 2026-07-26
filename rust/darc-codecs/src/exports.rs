//! C entry points.
//!
//! These carry the same names and signatures as the C originals so a codec can
//! be swapped in at link time. Note the C declarations in Delta.cpp have C++
//! linkage -- there is no `extern "C"` anywhere in that file -- so C_Delta.cpp
//! currently links against a mangled symbol and will NOT pick these up. Adding
//! `extern "C"` to the declarations in C_Delta.h (a linkage-only change) is
//! what wires them together; until then these are exercised by the differential
//! harness rather than by the archiver.

use crate::{bsc, delta, dict, dict_encode, dispack, grzip, lz4, lz4hc, lzp, mm, rep, tornado, tta, zstd};
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
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
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
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
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
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
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

/// Drop-in under the archiver's own symbol names.
///
/// Exported unconditionally: the C implementation this used to shadow has been
/// deleted (LZP is ported in BOTH directions), so there is nothing left to
/// collide with and the DARC_NO_RUST build needs these symbols to link.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
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
/// Exported unconditionally, for the same reason as `lzp_compress` above.
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

/// REP encoder.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_rep_compress(
    block_size: u32, min_compression: c_int, min_match_len: c_int, barrier: c_int,
    smallest_len: c_int, hash_bits: c_int, amplifier: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => rep::compress(
            &io, block_size, min_compression, min_match_len, barrier, smallest_len, hash_bits, amplifier),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn rep_compress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_rep_compress(block_size, a, b, c, d, e, f, callback, auxdata)
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn rep_decompress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_rep_decompress(block_size, a, b, c, d, e, f, callback, auxdata)
}

/// TTA decoder. `tta_decompress` takes no tuning parameters -- level, channel
/// count and word size all travel in the stream header.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_tta_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => tta::decompress(&io),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name; the switch is an exclusion in
/// C_TTA.cpp rather than a redeclaration, as with the other codecs.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn tta_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_tta_decompress(callback, auxdata)
}

/// TTA encoder. `level` 0 stores; 1-3 select the adaptive filter set. The
/// channel count and word size are autodetected when left at 0, exactly as for
/// MM -- TTA calls the same detector, with a looser entropy threshold.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_tta_compress(
    level: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    raw_data: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => tta::compress(
            &io, level, skip_header, is_float, num_chan, word_size, offset, raw_data,
        ),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn tta_compress(
    level: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    raw_data: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_tta_compress(
        level, skip_header, is_float, num_chan, word_size, offset, raw_data, callback, auxdata,
    )
}

/// MM decoder. Like TTA it takes no tuning parameters -- the channel count and
/// word size the encoder settled on travel in the stream header.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_mm_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => mm::decompress(&io),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name; the switch is an exclusion in
/// mm.cpp rather than a redeclaration, as with the other codecs.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn mm_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_mm_decompress(callback, auxdata)
}

/// MM encoder. Unlike the decoder this takes the full parameter set, because
/// the caller may pin the model (`-mmm:c2:w16`) instead of letting the
/// autodetector choose it -- and what it chooses lands in the stream header.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_mm_compress(
    mode: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    reorder: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => mm::compress(
            &io, mode, skip_header, is_float, num_chan, word_size, offset, reorder,
        ),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn mm_compress(
    mode: c_int,
    skip_header: c_int,
    is_float: c_int,
    num_chan: c_int,
    word_size: c_int,
    offset: c_int,
    reorder: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_mm_compress(
        mode,
        skip_header,
        is_float,
        num_chan,
        word_size,
        offset,
        reorder,
        callback,
        auxdata,
    )
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
/// Exported unconditionally: the C implementation this used to shadow has
/// been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn dict_compress(
    block_size: u32, a: c_int, b: c_int, c: c_int, d: c_int, e: c_int, f: c_int,
    callback: CALLBACK_FUNC, auxdata: *mut c_void,
) -> c_int {
    darc_rs_dict_compress(block_size, a, b, c, d, e, f, callback, auxdata)
}

/// Tornado decoder. Like TTA and MM it takes no tuning parameters -- the
/// encoding method, minimum match length and window size all travel in the
/// six-byte stream header.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_tor_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => tornado::decode::decompress(&io),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name; the switch is an exclusion in
/// Tornado.cpp rather than a redeclaration, as with the other codecs.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn tor_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_tor_decompress(callback, auxdata)
}

/// Tornado encoder, for the differential harness only.
///
/// `PackMethod` crosses the ABI by value, which is how `tor_compress` itself
/// takes it (Tornado.cpp:307). There is no `tor_compress` drop-in yet: the port
/// covers three of the nine live instantiations, and the rest return
/// INVALID_COMPRESSOR rather than a stream that would differ from the C's.
///
/// `all_at_once` is the C's `compress_all_at_once` global (Common.cpp:6), which
/// a drop-in could not read; the caller passes it explicitly.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_tor_compress(
    m: tornado::encode::PackMethod,
    all_at_once: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => tornado::encode::compress(m, &io, all_at_once != 0),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// GRZip block decoder, for the differential harness only.
///
/// GRZip is still being ported -- there is no `grzip_decompress` drop-in yet,
/// because the stream wrapper is not written. This exposes the block level,
/// which is where every stage that *is* ported actually runs, so the harness
/// can compare against `GRZip_DecompressBlock` before more code piles up on
/// top of unverified code.
///
/// Returns the number of bytes written, or a negative GRZip error code.
///
/// # Safety
/// `input`/`output` must be valid for `in_size`/`out_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_decompress_block(
    input: *const u8,
    in_size: c_int,
    output: *mut u8,
    out_cap: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || in_size < 0 || out_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, in_size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_cap as usize);
    match grzip::block::decompress_block(inp, out) {
        Ok(n) => n as c_int,
        Err(e) => e,
    }
}

/// GRZip decoder. Like TTA, MM and Tornado it takes no tuning parameters --
/// every block carries its own 28-byte header.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => grzip::stream::decompress(&io),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name; the switch is an exclusion in
/// C_GRZip.cpp rather than a redeclaration, as with the other codecs.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[cfg(feature = "dropin")]
#[no_mangle]
pub unsafe extern "C" fn grzip_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_grzip_decompress(callback, auxdata)
}

/// DisPack decoder. `block_size` bounds the untrusted chunk lengths.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_dispack_decompress(
    block_size: u32,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => dispack::decode::decompress(&io, block_size),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name.
///
/// Exported unconditionally: the C implementation this used to shadow has been
/// deleted (DisPack is ported in BOTH directions), so there is nothing left to
/// collide with and the DARC_NO_RUST build needs this symbol to link.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn dispack_decompress(
    block_size: u32,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_dispack_decompress(block_size, callback, auxdata)
}

/// BSC QLFC coder-level decode, for the differential harness.
///
/// BSC is still being ported; this exposes the entropy stage alone so the range
/// coder, mixer, model and decode bodies can be verified against the C before
/// the block dispatcher and the inverse transforms exist.
///
/// # Safety
/// `input`/`output` must be valid for `in_size`/`out_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bsc_qlfc_decode(
    input: *const u8,
    in_size: c_int,
    output: *mut u8,
    out_cap: c_int,
    coder: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || in_size < 0 || out_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, in_size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_cap as usize);
    let r = match coder {
        1 => bsc::qlfc::static_decode(inp, out),
        2 => bsc::qlfc::adaptive_decode(inp, out),
        3 => bsc::qlfc::fast_decode(inp, out),
        _ => return FREEARC_ERRCODE_GENERAL,
    };
    match r {
        Ok(n) => n as c_int,
        Err(e) => e,
    }
}

/// BSC inverse-BWT, for the differential harness. Mirrors `bsc_bwt_decode`:
/// inverts `data` in place, choosing the aux vs single-index path from
/// `num_indexes`. Returns `LIBBSC_NO_ERROR` (0) or a negative libbsc code.
///
/// # Safety
/// `data` must be valid for `n` bytes; `indexes` for `num_indexes` `i32`s.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bsc_bwt_decode(
    data: *mut u8,
    n: c_int,
    index: c_int,
    num_indexes: u8,
    indexes: *const i32,
) -> c_int {
    if data.is_null() || n < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let buf = core::slice::from_raw_parts_mut(data, n as usize);
    let idx: &[i32] = if indexes.is_null() || num_indexes == 0 {
        &[]
    } else {
        core::slice::from_raw_parts(indexes, num_indexes as usize)
    };
    bsc::bwt::bwt_decode(buf, n as usize, index, num_indexes, idx)
}

/// BSC inverse sort-transform (ST3..ST8), for the differential harness. Mirrors
/// `bsc_st_decode`: inverts `data` in place. Returns `LIBBSC_NO_ERROR` (0) or a
/// negative libbsc code.
///
/// # Safety
/// `data` must be valid for `n` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bsc_st_decode(data: *mut u8, n: c_int, k: c_int, index: c_int) -> c_int {
    if data.is_null() || n < 0 || k < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let buf = core::slice::from_raw_parts_mut(data, n as usize);
    bsc::st::st_decode(buf, n as usize, k as u32, index)
}

/// BSC block dispatcher, for the whole-codec differential harness. Mirrors
/// `bsc_decompress`: decode one framed block (28-byte header + payload) into
/// `output`. Returns `LIBBSC_NO_ERROR` (0) or a negative libbsc code.
///
/// # Safety
/// `input` must be valid for `in_size` bytes, `output` for `out_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bsc_decompress_block(
    input: *const u8,
    in_size: c_int,
    output: *mut u8,
    out_cap: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || in_size < 0 || out_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, in_size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_cap as usize);
    bsc::dispatch::decompress(inp, out)
}

/// `bsc_lzp_compress`: LZP-encode `input` into `output`. Returns the number of
/// bytes written, or a negative libbsc code -- `LIBBSC_NOT_COMPRESSIBLE` (-3)
/// when the input does not shrink, which the caller answers by skipping LZP.
///
/// Exported for the differential harness ahead of the rest of the BSC encoder;
/// nothing in the archiver calls it yet.
///
/// # Safety
/// `input` must be valid for `in_size` bytes, `output` for `out_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_bsc_lzp_compress(
    input: *const u8,
    in_size: c_int,
    output: *mut u8,
    out_cap: c_int,
    hash_size: c_int,
    min_len: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || in_size < 0 || out_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    if !(10..=28).contains(&hash_size) || !(4..=255).contains(&min_len) {
        return bsc::LIBBSC_BAD_PARAMETER;
    }
    let inp = core::slice::from_raw_parts(input, in_size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_cap as usize);
    bsc::lzp_enc::compress(inp, out, hash_size as u32, min_len as u32)
}

/// LZ4 raw-block decode, mirroring `LZ4_decompress_safe`: returns the number of
/// bytes written, or a negative code. This is the *safe* variant -- it must not
/// read past `src` nor write past `dst` however malformed the block is, since
/// corrupt archives reach it through an ordinary `arc t`.
///
/// The LZ4 block format is a fixed specification, so `lz4_flex` reads any block
/// the C library ever wrote; that is what makes the substitution safe for
/// existing archives.
///
/// # Safety
/// `src` must be valid for `src_size` bytes, `dst` for `dst_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_lz4_decompress_block(
    src: *const u8,
    src_size: c_int,
    dst: *mut u8,
    dst_cap: c_int,
) -> c_int {
    if src.is_null() || dst.is_null() || src_size < 0 || dst_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let s = core::slice::from_raw_parts(src, src_size as usize);
    let d = core::slice::from_raw_parts_mut(dst, dst_cap as usize);
    match lz4::decompress_block(s, d) {
        Ok(n) => n as c_int,
        Err(e) => e,
    }
}

/// LZ4 raw-block encode, mirroring `LZ4_compress_default`: returns the
/// compressed length, or 0 when the block does not fit -- which `C_LZ4.cpp`
/// treats as "store this block raw", not as an error.
///
/// Output is NOT byte-identical to the C library's. LZ4 is a match finder and
/// encoders legitimately choose different matches; that is acceptable under the
/// format-valid rule, and `-mlz4` has no fingerprint case.
///
/// # Safety
/// `src` must be valid for `src_size` bytes, `dst` for `dst_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_lz4_compress_block(
    src: *const u8,
    src_size: c_int,
    dst: *mut u8,
    dst_cap: c_int,
) -> c_int {
    if src.is_null() || dst.is_null() || src_size < 0 || dst_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let s = core::slice::from_raw_parts(src, src_size as usize);
    let d = core::slice::from_raw_parts_mut(dst, dst_cap as usize);
    lz4::compress_block(s, d).map_or(0, |n| n as c_int)
}

/// DisPack forward filter, mirroring `DisFilter` (`DisPack.cpp:600`).
///
/// Returns the filtered length, or a negative code. Unlike the archiver's
/// chunked driver this is the raw block transform, which is what the
/// differential harness compares byte for byte against the C.
///
/// `dst` must have room for the worst case: every input byte escaping to two
/// bytes, plus the `ST_MAX` header words.
///
/// # Safety
/// `src` must be valid for `src_size` bytes, `dst` for `dst_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_dispack_filter(
    src: *const u8,
    src_size: c_int,
    origin: u32,
    dst: *mut u8,
    dst_cap: c_int,
) -> c_int {
    if src.is_null() || dst.is_null() || src_size < 0 || dst_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let s = core::slice::from_raw_parts(src, src_size as usize);
    let out = dispack::encode::dis_filter(s, origin);
    if out.len() > dst_cap as usize {
        return FREEARC_ERRCODE_GENERAL;
    }
    core::ptr::copy_nonoverlapping(out.as_ptr(), dst, out.len());
    out.len() as c_int
}

/// DisPack compress driver, mirroring `DISPACK_METHOD::compress`
/// (`C_DisPack.cpp:170`) -- the chunked stream, not the raw block transform.
///
/// This subsumes `detect` and `DisFilter`: the wrapper hands over the whole
/// callback loop, so the C keeps only the method plumbing and the parser.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_dispack_compress(
    block_size: u32,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => dispack::encode::compress(&io, block_size),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// DisPack executable detection, mirroring `detect` (`C_DisPack.cpp:151`).
///
/// Returns 2 for `EXETYPE_EXE`, 1 for `EXETYPE_DATA`, matching the C enum so
/// the differential harness can compare the classification directly.
///
/// # Safety
/// `buf` must be valid for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_dispack_detect(buf: *const u8, len: c_int) -> c_int {
    if buf.is_null() || len < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let b = core::slice::from_raw_parts(buf, len as usize);
    match dispack::encode::detect(b) {
        dispack::encode::ExeType::Exe => 2,
        dispack::encode::ExeType::Data => 1,
    }
}

/// LZ4 high-compression encode, mirroring `LZ4_compress_HC`: returns the
/// compressed length, or 0 when the block does not fit -- which `C_LZ4.cpp`
/// treats as "store this block raw", not as an error.
///
/// HC is encoder-only and emits ordinary LZ4 blocks, so no existing archive
/// depends on this matching the C byte for byte; what it owes is the ratio.
/// See `lz4hc.rs` for which strategies are reproduced and which levels are
/// clamped.
///
/// # Safety
/// `src` must be valid for `src_size` bytes, `dst` for `dst_cap` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_lz4_compress_hc_block(
    src: *const u8,
    src_size: c_int,
    dst: *mut u8,
    dst_cap: c_int,
    level: c_int,
) -> c_int {
    if src.is_null() || dst.is_null() || src_size < 0 || dst_cap < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let s = core::slice::from_raw_parts(src, src_size as usize);
    let d = core::slice::from_raw_parts_mut(dst, dst_cap as usize);
    lz4hc::compress_hc(s, d, level) as c_int
}

/// zstd streaming decompress, replacing `zstd_stream_decompress` in
/// `C_Zstd.cpp`. `zstd-safe` bundles zstd 1.5.7 while the repository vendored
/// 1.5.6; the frame format is unchanged between them, which
/// `tests/zstd_vectors.rs` proves against frames the vendored build produced.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_zstd_stream_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => zstd::decompress_stream(&io),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// zstd streaming compress, replacing `zstd_stream_compress`.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_zstd_stream_compress(
    level: c_int,
    window_log: c_int,
    workers: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    let io = match Io::new(callback, auxdata) {
        Some(io) => io,
        None => return FREEARC_ERRCODE_GENERAL,
    };
    let params = zstd::Params {
        level,
        window_log: window_log.max(0) as u32,
        workers: workers.max(0) as u32,
    };
    zstd::compress_stream(&io, params)
}

/// `ZSTD_minCLevel` / `ZSTD_maxCLevel`, for `parse_ZSTD`'s level clamping.
#[no_mangle]
pub extern "C" fn darc_rs_zstd_min_clevel() -> c_int {
    zstd::min_c_level()
}

#[no_mangle]
pub extern "C" fn darc_rs_zstd_max_clevel() -> c_int {
    zstd::max_c_level()
}

/// `ZSTD_sizeof_CCtx` for a context configured as `ZSTD_METHOD` would, for
/// `GetCompressionMem`. Returns 0 when the parameters are rejected; the caller
/// falls back to its own default.
#[no_mangle]
pub extern "C" fn darc_rs_zstd_sizeof_cctx(level: c_int, window_log: c_int) -> usize {
    zstd::sizeof_cctx(level, window_log.max(0) as u32)
}

/// Exported unconditionally, unlike the other drop-ins: the vendored libzstd is
/// gone, so there is no C definition of this symbol to collide with, and the
/// DARC_NO_RUST build needs it to link at all.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn zstd_stream_decompress(
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_zstd_stream_decompress(callback, auxdata)
}

/// Exported unconditionally, unlike the other drop-ins: the vendored libzstd is
/// gone, so there is no C definition of this symbol to collide with, and the
/// DARC_NO_RUST build needs it to link at all.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
pub unsafe extern "C" fn zstd_stream_compress(
    level: c_int,
    window_log: c_int,
    workers: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_zstd_stream_compress(level, window_log, workers, callback, auxdata)
}

/// GRZip's LZP stage, forward direction. Exposed for the differential harness
/// only -- not a drop-in, because the C `GRZip_LZP_Encode` is still what the
/// block driver calls until that is ported too.
///
/// # Safety
/// `input`/`output` must be valid for `size`/`out_size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_lzp_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
    out_size: c_int,
    min_match_len: c_int,
    ht_size: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || size < 0 || out_size < 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_size as usize);
    match grzip::lzp::encode(inp, out, min_match_len as u32, ht_size as u32) {
        Ok(n) => n as c_int,
        Err(e) => e,
    }
}

/// GRZip's ST4 stage, forward direction. Harness-only, like the LZP one.
///
/// # Safety
/// `input`/`output` must be valid for `size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_st4_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let n = size as usize;
    let inp = core::slice::from_raw_parts(input, n);
    let out = core::slice::from_raw_parts_mut(output, n);
    match grzip::st4::encode(inp, n, out) {
        Ok(fbp) => fbp,
        Err(e) => e,
    }
}

/// GRZip's record filter: the mode decision and the forward transform.
/// Harness-only, like the other stages. Returns the mode; `output` is filled
/// only when the mode is nonzero.
///
/// # Safety
/// `input`/`output` must be valid for `size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_rec_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let n = size as usize;
    let inp = core::slice::from_raw_parts(input, n);
    let out = core::slice::from_raw_parts_mut(output, n);
    let mode = grzip::rec::test(inp, n);
    if mode != 0 {
        grzip::rec::encode(inp, n, out, mode);
    }
    mode
}

/// GRZip's MTF + arithmetic coder, forward direction. Harness-only.
///
/// Returns the coded length, or the GRZip error code. `out_size` must be at
/// least the input length.
///
/// # Safety
/// `input`/`output` must be valid for `size`/`out_size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_mtf_ari_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
    out_size: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 || out_size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, size as usize);
    match grzip::mtf_ari::encode(inp) {
        Ok(v) => {
            if v.len() > out_size as usize {
                return FREEARC_ERRCODE_GENERAL;
            }
            let out = core::slice::from_raw_parts_mut(output, v.len());
            out.copy_from_slice(&v);
            v.len() as c_int
        }
        Err(e) => e,
    }
}

/// GRZip's WFC + arithmetic coder, forward direction. Harness-only.
///
/// # Safety
/// `input`/`output` must be valid for `size`/`out_size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_wfc_ari_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
    out_size: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 || out_size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, size as usize);
    match grzip::wfc_ari::encode(inp) {
        Ok(v) => {
            if v.len() > out_size as usize {
                return FREEARC_ERRCODE_GENERAL;
            }
            let out = core::slice::from_raw_parts_mut(output, v.len());
            out.copy_from_slice(&v);
            v.len() as c_int
        }
        Err(e) => e,
    }
}

/// GRZip's strong BWT, forward direction. Harness-only. Returns the first-byte
/// position WITHOUT the strong flag -- the dispatcher ORs that in.
///
/// # Safety
/// `input`/`output` must be valid for `size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_strong_bwt_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let n = size as usize;
    let inp = core::slice::from_raw_parts(input, n);
    let out = core::slice::from_raw_parts_mut(output, n);
    match grzip::bwt::strong_encode(inp, n, out) {
        Ok(fbp) => fbp,
        Err(e) => e,
    }
}

/// GRZip's BWT, forward direction, with the fast/strong selection. Harness-only.
/// Returns the first-byte position, with `StrongBWT_Flag` set when the fast sort
/// gave up and the strong one ran.
///
/// # Safety
/// `input`/`output` must be valid for `size` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_bwt_encode(
    input: *const u8,
    size: c_int,
    output: *mut u8,
    fast: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let n = size as usize;
    let inp = core::slice::from_raw_parts(input, n);
    let out = core::slice::from_raw_parts_mut(output, n);
    match grzip::bwt::encode(inp, n, out, fast != 0) {
        Ok(fbp) => fbp,
        Err(e) => e,
    }
}

/// GRZip's block driver, forward direction. Returns bytes written to `output`.
///
/// # Safety
/// `input` valid for `size`; `output` must have room for `size + 28 + slack`.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_grzip_compress_block(
    input: *const u8,
    size: c_int,
    output: *mut u8,
    out_cap: c_int,
    mode: c_int,
) -> c_int {
    if input.is_null() || output.is_null() || size <= 0 || out_cap <= 0 {
        return FREEARC_ERRCODE_GENERAL;
    }
    let inp = core::slice::from_raw_parts(input, size as usize);
    let out = core::slice::from_raw_parts_mut(output, out_cap as usize);
    match grzip::block::compress_block(inp, size as usize, out, mode) {
        Ok(n) => n as c_int,
        Err(e) => e,
    }
}

/// GRZip's stream compressor -- the archiver's entry point.
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn darc_rs_grzip_compress(
    method: c_int,
    block_size: c_int,
    enable_lzp: c_int,
    min_match_len: c_int,
    hash_size_log: c_int,
    alternative_bwt_sort: c_int,
    adaptive_block_size: c_int,
    delta_filter: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    match Io::new(callback, auxdata) {
        Some(io) => grzip::stream::compress(
            &io, method, block_size, enable_lzp, min_match_len, hash_size_log,
            alternative_bwt_sort, adaptive_block_size, delta_filter,
        ),
        None => FREEARC_ERRCODE_GENERAL,
    }
}

/// Drop-in under the archiver's own symbol name.
///
/// Exported UNCONDITIONALLY, unlike the decoder below: the C encoder this used
/// to shadow has been deleted, so there is nothing left to collide with and the
/// DARC_NO_RUST build needs this symbol to link. (`grzip_decompress` stays
/// feature-gated, because its C still exists for Unarc.)
///
/// # Safety
/// `callback` and `auxdata` must be what the C caller supplied.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub unsafe extern "C" fn grzip_compress(
    method: c_int,
    block_size: c_int,
    enable_lzp: c_int,
    min_match_len: c_int,
    hash_size_log: c_int,
    alternative_bwt_sort: c_int,
    adaptive_block_size: c_int,
    delta_filter: c_int,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    darc_rs_grzip_compress(
        method, block_size, enable_lzp, min_match_len, hash_size_log,
        alternative_bwt_sort, adaptive_block_size, delta_filter, callback, auxdata,
    )
}
