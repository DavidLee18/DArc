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
use darc_lzma::{InStream, MatchFinderKind, OutStream, StreamError};

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
    //
    // `algorithm != 1` is the fast parser, which is not ported. An unrecognized
    // matchFinder id is refused rather than defaulted to BT4 the way the C does
    // (`C_LZMA.cpp:107`) -- silently encoding with a different finder than asked for
    // would produce an archive that no other build reproduces.
    // `algorithm` selects the parser: 0 is the fast one, 1 the optimal one
    // (`LzmaEnc.c:568`). Both are implemented. DArc reaches 0 through the method
    // words `fast`/`fastest` (`C_LZMA.cpp:361`), which its own `3binary` preset uses.
    let fast_mode = match algorithm {
        0 => true,
        1 => false,
        _ => return FREEARC_ERRCODE_NOT_IMPLEMENTED,
    };
    let mf = match MatchFinderKind::from_stream(match_finder) {
        Some(k) => k,
        None => return FREEARC_ERRCODE_NOT_IMPLEMENTED,
    };
    if dictionary_size <= 0 || num_fast_bytes <= 0 {
        return FREEARC_ERRCODE_NOT_IMPLEMENTED;
    }

    let io = match Io::new(callback, auxdata) {
        Some(io) => io,
        None => return FREEARC_ERRCODE_NOT_IMPLEMENTED,
    };

    // `mc == 0` is DArc's "auto" sentinel, resolved by the SDK at LzmaEnc.c:99 as
    // `(16 + (fb >> 1)) >> (btMode ? 0 : 1)`. darc_lzma takes mc literally, so 0
    // would make the search's cut counter underflow -- and the shift means the
    // answer differs per finder, which is why this asks `mf` instead of inlining
    // the BT form.
    let fb = num_fast_bytes as u32;
    let mc = if match_finder_cycles <= 0 {
        mf.auto_mc(fb)
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
        mf,
        fast_mode,
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
        // darc_lzma reports an unported configuration as the C SDK's
        // SZ_ERROR_UNSUPPORTED; translate that one into DArc's vocabulary. Every
        // other error came from the callback and is already a FreeArc code.
        Err(e) if e == darc_lzma::ERR_UNSUPPORTED => FREEARC_ERRCODE_NOT_IMPLEMENTED,
        Err(StreamError(code)) => code,
    }
}

/// Decode via [`darc_lzma`]'s hardened decoder.
///
/// Signature mirrors the C's `lzma_decompress` (`C_LZMA.cpp:145`) so the wrapper can
/// forward its parameters unchanged. Only four of the nine are read on this path --
/// `encode_props` (`C_LZMA.cpp:135-143`) builds the five decoder property bytes from
/// `dictionarySize`, `posStateBits`, `litContextBits` and `litPosBits`, and the rest
/// are encoder knobs the stream does not depend on. `algorithm` in particular has no
/// effect here: the parser choice is invisible to a decoder.
///
/// # Safety
///
/// `callback` must be a valid `CALLBACK_FUNC` and `auxdata` whatever it expects.
#[no_mangle]
pub unsafe extern "C" fn darc_lzma_decompress(
    dictionary_size: c_int,
    _hash_size: c_int,
    _algorithm: c_int,
    _num_fast_bytes: c_int,
    _match_finder: c_int,
    _match_finder_cycles: c_int,
    pos_state_bits: c_int,
    lit_context_bits: c_int,
    lit_pos_bits: c_int,
    callback: crate::ffi::CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    // A panic firewall, and this entry point is where it matters most. Everything
    // below parses bytes an attacker wrote, and `unarc` plus every SFX module link
    // this and are compiled `-D_NO_EXCEPTIONS` -- an unwind out of an `extern "C"`
    // frame there is undefined behaviour. The decoder is written not to panic
    // (rust/darc-lzma/src/decode_stream.rs bounds-checks every read), so reaching
    // this handler is a bug; it exists so that the bug is an error code rather than
    // a corrupted process.
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: the caller's contract, forwarded unchanged.
        unsafe {
            decompress_inner(
            dictionary_size,
            pos_state_bits,
            lit_context_bits,
            lit_pos_bits,
            callback,
            auxdata,
        )
        }
    }));
    match result {
        Ok(code) => code,
        Err(_) => crate::ffi::FREEARC_ERRCODE_GENERAL,
    }
}

/// # Safety
///
/// Same contract as the caller: `callback` valid, `auxdata` whatever it expects.
unsafe fn decompress_inner(
    dictionary_size: c_int,
    pos_state_bits: c_int,
    lit_context_bits: c_int,
    lit_pos_bits: c_int,
    callback: crate::ffi::CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    // `encode_props` (C_LZMA.cpp:135-143), including its `(Byte)` truncation: an
    // out-of-range pb/lc/lp from a method string wraps here exactly as it does in
    // the C, and is then REJECTED by the props validation rather than indexing a
    // table with it.
    let byte0 = ((pos_state_bits as u32)
        .wrapping_mul(5)
        .wrapping_add(lit_pos_bits as u32)
        .wrapping_mul(9)
        .wrapping_add(lit_context_bits as u32)) as u8;
    let d = dictionary_size as u32;
    let props = [
        byte0,
        d as u8,
        (d >> 8) as u8,
        (d >> 16) as u8,
        (d >> 24) as u8,
    ];

    let io = match crate::ffi::Io::new(callback, auxdata) {
        Some(io) => io,
        None => return FREEARC_ERRCODE_NOT_IMPLEMENTED,
    };
    let mut source = CallbackIn { io: &io };
    let mut sink = CallbackOut { io: &io };

    match darc_lzma::decode_stream::decode_stream(&mut source, &mut sink, &props) {
        Ok(_) => OK,
        // The mapping the C uses, one arm each: C_LZMA.cpp:163-166 turns a bad props
        // byte into INVALID_COMPRESSOR, :203/:219/:224 turn corrupt or truncated
        // data into BAD_COMPRESSED_DATA, and a callback error is returned verbatim.
        Err(darc_lzma::LzmaDecodeError::UnsupportedProps) => {
            crate::ffi::FREEARC_ERRCODE_INVALID_COMPRESSOR
        }
        Err(darc_lzma::LzmaDecodeError::DataError)
        | Err(darc_lzma::LzmaDecodeError::TruncatedInput) => {
            crate::ffi::FREEARC_ERRCODE_BAD_COMPRESSED_DATA
        }
        Err(darc_lzma::LzmaDecodeError::NotEnoughMemory) => {
            crate::ffi::FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
        }
        Err(darc_lzma::LzmaDecodeError::Stream(darc_lzma::StreamError(code))) => code,
        Err(darc_lzma::LzmaDecodeError::Internal) => crate::ffi::FREEARC_ERRCODE_GENERAL,
    }
}
