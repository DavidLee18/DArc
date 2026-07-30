//! The FFI surface that lets `Compression/LZMA/C_LZMA.cpp` route to
//! [`darc_lzma`], mirroring what every other ported codec does.
//!
//! ## Why this is bounded rather than streaming, and why that is safe
//!
//! `darc_lzma`'s encoder is **in-memory by design** (upstream: "full input, no
//! streaming"), while DArc's `lzma_compress` is callback-driven and a solid block
//! can exceed RAM. Pretending otherwise would trade a correctness bug for a
//! memory one.
//!
//! So this entry point **refuses rather than degrades**: it buffers the callback
//! input up to [`MAX_BUFFERED`] and returns `FREEARC_ERRCODE_NOT_IMPLEMENTED`
//! beyond it, leaving the caller free to fall back to the C. A refusal is a
//! signal the caller can act on; an OOM at 3 GB into an archive is not.
//!
//! That is what makes this wireable *today*: `C_LZMA.cpp` can try Rust and fall
//! back, so the Rust path is exercised on real archives without being able to
//! break the large ones. When streaming lands, the bound and the fallback go.
//!
//! ## What is NOT covered
//!
//! * `matchFinder` other than BT4 (`kBT2`, `kBT3`, `kHC4`, `kHT4`) — refused.
//! * `algorithm` 0, the fast parser — refused.
//! * LZMA2 and BCJ have their own wrappers and are untouched.
//!
//! Every refusal is explicit: this returns an error code rather than silently
//! encoding something DArc did not ask for, which for a byte-exact codec would be
//! an archive that no other build reproduces.

use crate::ffi::{Io, FREEARC_ERRCODE_NOT_IMPLEMENTED, OK};
use core::ffi::{c_int, c_void};

/// The largest input this entry point will buffer: 256 MB.
///
/// Chosen to sit above DArc's default dictionary sizes while staying far below
/// the point where buffering competes with the encoder's own allocation. It is a
/// refusal threshold, not a tuning knob — raising it does not make the encoder
/// stream.
pub const MAX_BUFFERED: usize = 256 << 20;

/// `kBT4` in `C_LZMA.cpp`'s `enum { kBT2, kBT3, kBT4, kHC4, kHT4 }`.
const K_BT4: c_int = 2;

/// Encode via `darc_lzma`, or refuse.
///
/// Signature mirrors the C's `lzma_compress` argument order so `C_LZMA.cpp` can
/// forward its parameters unchanged.
///
/// # Safety
///
/// `callback` must be a valid `CALLBACK_FUNC` and `auxdata` whatever it expects,
/// exactly as for every other codec entry point here.
#[no_mangle]
pub unsafe extern "C" fn darc_lzma_compress(
    dictionary_size: c_int,
    _hash_size: c_int,
    algorithm: c_int,
    num_fast_bytes: c_int,
    match_finder: c_int,
    match_finder_cycles: c_int,
    pos_state_bits: c_int,
    lit_context_bits: c_int,
    lit_pos_bits: c_int,
    callback: crate::ffi::CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    // Refuse the configurations this encoder does not implement, before touching
    // the stream. `_hash_size` is ignored deliberately: the pinned C_LZMA.cpp does
    // not forward it into CLzmaEncProps either.
    if match_finder != K_BT4 || algorithm != 1 {
        return FREEARC_ERRCODE_NOT_IMPLEMENTED;
    }
    if dictionary_size <= 0 || num_fast_bytes <= 0 {
        return FREEARC_ERRCODE_NOT_IMPLEMENTED;
    }

    let io = match Io::new(callback, auxdata) {
        Some(io) => io,
        None => return FREEARC_ERRCODE_NOT_IMPLEMENTED,
    };

    // Drain the callback into memory, refusing past the bound rather than growing
    // without limit.
    let mut input: Vec<u8> = Vec::new();
    let mut chunk = vec![0u8; 1 << 16];
    loop {
        let n = io.read(&mut chunk);
        if n < 0 {
            return n;
        }
        if n == 0 {
            break;
        }
        if input.len() + n as usize > MAX_BUFFERED {
            return FREEARC_ERRCODE_NOT_IMPLEMENTED;
        }
        input.extend_from_slice(&chunk[..n as usize]);
    }

    // `mc == 0` is DArc's "auto" sentinel, resolved by the SDK at LzmaEnc.c:99 as
    // `(16 + (fb >> 1)) >> (btMode ? 0 : 1)`; btMode is 1 for BT4, so the shift is
    // 0. darc_lzma takes mc literally, and 0 would make the BT4 tree walk's
    // cut_value underflow.
    let fb = num_fast_bytes as u32;
    let mc = if match_finder_cycles <= 0 {
        16 + (fb >> 1)
    } else {
        match_finder_cycles as u32
    };

    let props = darc_lzma::LzmaProps {
        lc: lit_context_bits as u8,
        lp: lit_pos_bits as u8,
        pb: pos_state_bits as u8,
        dict_size: dictionary_size as u32,
        fb,
        mc,
        // DArc always sets it: "FreeArc streams with EOPM (unknown size)".
        // rust/difftest/lzma-gap-check.sh is what pins that this reproduces the
        // C's bytes exactly, 88/88 over the corpus.
        write_end_mark: true,
    };

    let out = darc_lzma::encode(&input, &props);
    match io.write_all(&out) {
        Ok(()) => OK,
        Err(e) => e,
    }
}
