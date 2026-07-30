//! The FFI surface that lets `Compression/LZMA/C_LZMA.cpp` route to
//! [`darc_lzma`], mirroring what every other ported codec does.
//!
//! ## Streaming, not buffering
//!
//! This used to drain the callback into a `Vec` and refuse past 256 MB, because
//! `darc_lzma`'s encoder was in-memory by design. It is not any more: the match
//! finder holds a sliding window of `dict_size` plus slack, and the range coder
//! stages 64 KiB of output at a time. So a solid block of any size goes straight
//! through, with memory proportional to the dictionary exactly as in the C.
//!
//! The `read` and `write` callbacks are adapted to `darc_lzma`'s `InStream` /
//! `OutStream` below — the same job `CbIn_Read` / `CbOut_Write` do on the C side.
//!
//! ## What is NOT covered
//!
//! * `matchFinder` other than BT4 (`kBT2`, `kBT3`, `kHC4`, `kHT4`) — refused.
//! * `algorithm` 0, the fast parser — refused.
//! * LZMA2 and BCJ have their own wrappers and are untouched.
//!
//! Every refusal is explicit: this returns `FREEARC_ERRCODE_NOT_IMPLEMENTED`
//! rather than silently encoding something DArc did not ask for, which for a
//! byte-exact codec would be an archive no other build reproduces.

use crate::ffi::{FREEARC_ERRCODE_NOT_IMPLEMENTED, Io, OK};
use core::ffi::{c_int, c_void};
use darc_lzma::{InStream, OutStream, StreamError};

/// `kBT4` in `C_LZMA.cpp`'s `enum { kBT2, kBT3, kBT4, kHC4, kHT4 }`.
const K_BT4: c_int = 2;

/// The callback's `read` side as an [`InStream`].
///
/// A negative return from the callback is an error code, not a byte count, and is
/// passed through unchanged — `Dict` and `LZP` were both broken once by treating a
/// negative return as failure when it was a stop signal, so the code the caller
/// chose is the code the caller gets back.
struct CallbackIn<'a> {
    io: &'a Io,
}

impl InStream for CallbackIn<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, StreamError> {
        let n = self.io.read(buf);
        if n < 0 {
            return Err(StreamError(n));
        }
        Ok(n as usize)
    }
}

/// The callback's `write` side as an [`OutStream`].
struct CallbackOut<'a> {
    io: &'a Io,
}

impl OutStream for CallbackOut<'_> {
    fn write(&mut self, data: &[u8]) -> Result<(), StreamError> {
        match self.io.write_all(data) {
            Ok(()) => Ok(()),
            Err(e) => Err(StreamError(e)),
        }
    }
}

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
        // C's bytes exactly, over a corpus that includes inputs many times the
        // dictionary so the sliding window is actually exercised.
        write_end_mark: true,
    };

    let mut source = CallbackIn { io: &io };
    let mut sink = CallbackOut { io: &io };
    match darc_lzma::encode_stream(&mut source, &mut sink, &props) {
        Ok(()) => OK,
        Err(StreamError(code)) => code,
    }
}
