//! The C ABI boundary, generated from DArc's own headers.
//!
//! This is the one part of a Rust port the compiler does NOT make safe on its
//! own: an `extern "C"` declaration is checked against nothing, so a wrong
//! signature miscompiles exactly as it would in C. This codebase has already
//! been bitten by that -- 41 helpers whose declarations were missing entirely,
//! 8 of them truncating a `long` return to `int` at every call site.
//!
//! So the declarations are not written here at all. build.rs runs bindgen over
//! Compression/Common.h and Compression/Compression.h -- the same headers the C
//! side compiles against -- and the result is included below. Hand-transcribing
//! worked, but only until a header changes and nobody re-transcribes.
//!
//! Generating them paid for itself immediately: the hand-written version
//! declared the callback as a plain `fn` pointer, while bindgen types it as
//! `Option<fn>`, because C permits a null function pointer there -- and
//! C_Delta.cpp does check for one. That null case is handled in `Io::new`
//! rather than being undefined behaviour.

#![allow(non_camel_case_types, non_upper_case_globals, dead_code)]

use core::ffi::{c_char, c_int, c_void};

include!(concat!(env!("OUT_DIR"), "/darc_abi.rs"));

/// `FREEARC_OK` arrives from bindgen as `u32` while every error code is `i32`,
/// a consequence of how the macros are written C-side. Codec entry points
/// return `c_int`, so expose it at the type actually used instead of casting at
/// every comparison.
pub const OK: c_int = FREEARC_OK as c_int;

/// Safe wrapper over the read/write callback protocol.
///
/// The C side drives a codec by calling back for input and output:
///   callback("read",  buf, len, aux) -> bytes actually read
///   callback("write", buf, len, aux) -> bytes actually written
/// See the `checked_read` / `checked_write` macros at Compression.h:65-66.
pub struct Io {
    callback: unsafe extern "C" fn(*const c_char, *mut c_void, c_int, *mut c_void) -> c_int,
    auxdata: *mut c_void,
}

impl Io {
    /// Returns `None` when the C caller passed a null callback, which the ABI
    /// permits. Calling through it would be undefined behaviour, so codec entry
    /// points turn this into `FREEARC_ERRCODE_GENERAL` instead.
    ///
    /// # Safety
    /// `callback` and `auxdata` must be exactly what the C caller supplied, and
    /// must remain valid for the lifetime of this `Io`.
    pub unsafe fn new(callback: CALLBACK_FUNC, auxdata: *mut c_void) -> Option<Self> {
        callback.map(|callback| Io { callback, auxdata })
    }

    fn call(&self, what: &[u8], buf: *mut c_void, size: c_int) -> c_int {
        debug_assert_eq!(what.last(), Some(&0), "`what` must be NUL-terminated");
        unsafe { (self.callback)(what.as_ptr() as *const c_char, buf, size, self.auxdata) }
    }

    /// Read up to `buf.len()` bytes. Returns the count, or a negative error.
    pub fn read(&self, buf: &mut [u8]) -> c_int {
        if buf.is_empty() {
            return 0;
        }
        self.call(b"read\0", buf.as_mut_ptr() as *mut c_void, clamp_len(buf.len()))
    }

    /// Write `buf`. Returns the count written, or a negative error.
    pub fn write(&self, buf: &[u8]) -> c_int {
        if buf.is_empty() {
            return 0;
        }
        // The callback takes a non-const pointer even for "write"; the C code
        // casts the same way.
        self.call(b"write\0", buf.as_ptr() as *mut c_void, clamp_len(buf.len()))
    }

    /// Write the whole buffer, mapping a short write to an IO error exactly as
    /// `checked_write` does in Compression.h.
    pub fn write_all(&self, buf: &[u8]) -> Result<(), c_int> {
        let want = clamp_len(buf.len());
        match self.write(buf) {
            got if got == want => Ok(()),
            got if got >= 0 => Err(FREEARC_ERRCODE_IO),
            got => Err(got),
        }
    }
}

/// The callback takes an `int` length; anything past `c_int::MAX` cannot be
/// expressed. Rust's `as` truncates every bit as silently as C's implicit
/// conversion -- that is how 16 GB became 0 in GetPhysicalMemory -- so the one
/// place a length crosses into an `int` is explicit and asserts in debug.
fn clamp_len(n: usize) -> c_int {
    debug_assert!(n <= c_int::MAX as usize, "buffer longer than c_int can express");
    core::cmp::min(n, c_int::MAX as usize) as c_int
}
