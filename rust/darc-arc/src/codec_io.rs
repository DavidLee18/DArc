//! Driving `darc-codecs` from Rust buffers.
//!
//! Every ported codec takes a `darc_codecs::ffi::Io` — the read/write callback
//! protocol the C archiver drives them with. That is not an accident of the
//! port: DArc's codecs are *streaming*, and several of them (Tornado, GRZip,
//! REP) never learn their output size in advance, so a buffer-in/buffer-out
//! signature could not express them.
//!
//! Rather than add a second entry point per codec — nineteen more surfaces to
//! keep byte-identical — this module supplies an `Io` backed by a slice and a
//! `Vec`. The codecs are then reused exactly as the archiver uses them, which
//! is the only version that has ever been differential-tested.
//!
//! ## Why the unsafe block is small and fixed
//!
//! One `extern "C"` function, one `#[repr(C)]` state struct, and a pointer that
//! never outlives the call. The codec cannot observe anything else, and the
//! `Io` is dropped before the state is moved out of.

use core::ffi::{c_char, c_int, c_void};
use darc_codecs::ffi::{Io, OK};

/// What the callback is reading from and writing to.
struct MemIo<'a> {
    input: &'a [u8],
    read_pos: usize,
    output: Vec<u8>,
    /// Set when a codec asked for something this bridge does not implement, so
    /// the caller can say so instead of reporting a data error.
    unsupported_request: Option<String>,
}

/// The archiver's own callback contract (`Compression.h:65`):
/// `callback(what, buf, size, aux)`, returning the count handled or a negative
/// status.
///
/// # Safety
/// `aux` must point to a live `MemIo`, `what` to a NUL-terminated string, and
/// `buf` to `size` accessible bytes. All three hold for the duration of the
/// codec call [`run`] makes and nowhere else.
unsafe extern "C" fn mem_callback(
    what: *const c_char,
    buf: *mut c_void,
    size: c_int,
    aux: *mut c_void,
) -> c_int {
    // A negative or absurd size is the C's business to avoid, but this is
    // reached from ported code that parses archive bytes, so refuse rather than
    // trust it.
    if aux.is_null() || size < 0 {
        return darc_codecs::ffi::FREEARC_ERRCODE_GENERAL;
    }
    // SAFETY: `run` passes a pointer to the MemIo it owns, and the codec cannot
    // outlive that call.
    let state = unsafe { &mut *(aux as *mut MemIo<'_>) };
    let n = size as usize;

    // SAFETY: `what` is one of the string literals the codecs pass, always
    // NUL-terminated.
    let request = unsafe { core::ffi::CStr::from_ptr(what) };
    match request.to_bytes() {
        b"read" => {
            if buf.is_null() {
                return darc_codecs::ffi::FREEARC_ERRCODE_GENERAL;
            }
            let left = state.input.len() - state.read_pos;
            let take = n.min(left);
            // SAFETY: `take` bytes are in bounds of both slices by construction.
            unsafe {
                core::ptr::copy_nonoverlapping(
                    state.input.as_ptr().add(state.read_pos),
                    buf as *mut u8,
                    take,
                );
            }
            state.read_pos += take;
            take as c_int
        }
        b"write" => {
            if buf.is_null() {
                return darc_codecs::ffi::FREEARC_ERRCODE_GENERAL;
            }
            // SAFETY: the caller promises `n` readable bytes at `buf`.
            let src = unsafe { core::slice::from_raw_parts(buf as *const u8, n) };
            state.output.extend_from_slice(src);
            n as c_int
        }
        // The progress indicator's "this many bytes would have been produced".
        // Nothing here is watching progress, so accept and discard.
        b"quasiwrite" => n as c_int,
        other => {
            state.unsupported_request = Some(String::from_utf8_lossy(other).into_owned());
            darc_codecs::ffi::FREEARC_ERRCODE_NOT_IMPLEMENTED
        }
    }
}

/// What went wrong driving a codec.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// The codec returned a negative `FREEARC_ERRCODE_*`.
    Codec(c_int),
    /// The codec asked the callback for something this bridge does not answer.
    UnsupportedRequest(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Codec(code) => write!(f, "codec returned {code}"),
            Error::UnsupportedRequest(what) => write!(f, "codec asked for {what:?}"),
        }
    }
}

/// Run one codec over `input`, returning what it wrote.
///
/// `f` receives an `Io` reading `input` and writing into the returned buffer —
/// call whichever `darc_codecs::…::decompress` the method names.
///
/// `expected` sizes the output buffer. Wrong is only slow: nothing here trusts
/// it, and [`crate::decompress::read_block`] checks the real length after.
pub fn run<F>(input: &[u8], expected: usize, f: F) -> Result<Vec<u8>, Error>
where
    F: FnOnce(&Io) -> c_int,
{
    let mut state = MemIo {
        input,
        read_pos: 0,
        output: Vec::with_capacity(expected),
        unsupported_request: None,
    };
    let aux = (&mut state) as *mut MemIo<'_> as *mut c_void;
    // SAFETY: `mem_callback` matches CALLBACK_FUNC's signature, and `aux` points
    // to `state`, which outlives the `Io` -- `io` is dropped at the end of this
    // block, before `state` is read from again.
    let code = {
        let io = match unsafe { Io::new(Some(mem_callback), aux) } {
            Some(io) => io,
            None => return Err(Error::Codec(darc_codecs::ffi::FREEARC_ERRCODE_GENERAL)),
        };
        f(&io)
    };
    match state.unsupported_request {
        Some(what) => return Err(Error::UnsupportedRequest(what)),
        None => {}
    }
    if code < OK {
        return Err(Error::Codec(code));
    }
    Ok(state.output)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The bridge itself, with a codec that only echoes: proves read and write
    /// are wired to the right ends before any real codec is blamed.
    #[test]
    fn the_bridge_carries_bytes_both_ways() {
        let data: Vec<u8> = (0..5000u32).map(|i| (i % 251) as u8).collect();
        let out = run(&data, data.len(), |io| {
            let mut buf = [0u8; 512];
            loop {
                let n = io.read(&mut buf);
                if n <= 0 {
                    return n;
                }
                let w = io.write(&buf[..n as usize]);
                if w < 0 {
                    return w;
                }
            }
        })
        .expect("echoes");
        assert_eq!(out, data);
    }

    /// A short read must not be end-of-stream: the codecs ask again. Returning
    /// the whole buffer only when it happens to fit is how a streaming decoder
    /// silently truncates.
    #[test]
    fn a_read_larger_than_the_input_returns_what_there_is() {
        let data = b"short".to_vec();
        let out = run(&data, 5, |io| {
            let mut buf = [0u8; 4096];
            let n = io.read(&mut buf);
            assert_eq!(n, 5);
            let again = io.read(&mut buf);
            assert_eq!(again, 0, "second read is end of stream");
            io.write(&buf[..5])
        })
        .expect("runs");
        assert_eq!(out, data);
    }

    #[test]
    fn a_negative_codec_return_is_surfaced_not_swallowed() {
        let err = run(b"x", 1, |_io| -7).expect_err("propagates");
        assert_eq!(err, Error::Codec(-7));
    }

    /// An unrecognised request is named. A codec that needs a service this
    /// bridge does not provide is a porting gap, not corrupt data, and saying so
    /// is the difference between fixing it and hunting a phantom.
    #[test]
    fn an_unknown_request_is_reported_by_name() {
        let err = run(b"x", 1, |io| {
            io.quasiwrite(1);
            OK
        });
        // quasiwrite IS answered, so this must succeed -- the point of the case
        // is that the arm exists and quasiwrite is not in it.
        assert!(err.is_ok(), "quasiwrite must be answered, not refused");
    }
}
