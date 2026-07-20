//! The C ABI boundary.
//!
//! This is the one part of a Rust port that the compiler does NOT make safe.
//! Rust checks these declarations against nothing: if the signature here
//! disagrees with the C caller, it miscompiles exactly as C would. DArc has
//! already been bitten by this twice on the C side -- 41 helpers with no
//! `extern "C"` declaration, 8 of which returned `long` and were truncated to
//! `int` at every call site.
//!
//! So the types below are transcribed deliberately, not guessed:
//!
//!   Compression/Common.h:93       typedef unsigned MemSize;
//!   Compression/Compression.h:62  typedef int CALLBACK_FUNC
//!                                     (const char *what, void *data,
//!                                      int size, void *auxdata);
//!
//! `overflow-checks = true` is set even for release builds in Cargo.toml.
//! Release Rust wraps on overflow by default, which would silently reproduce
//! the class of bug found in `filenameHash` and in GRZip's hash table rather
//! than trapping on it. Codec work is not hot enough for the check to matter.

use core::ffi::{c_char, c_int, c_uint, c_void};

/// Matches `CALLBACK_FUNC` exactly.
pub type CallbackFunc =
    unsafe extern "C" fn(*const c_char, *mut c_void, c_int, *mut c_void) -> c_int;

/// Matches `MemSize`.
pub type MemSize = c_uint;

/// Error codes from Compression/Compression.h.
pub const FREEARC_OK: c_int = 0;
pub const FREEARC_ERRCODE_GENERAL: c_int = -1;
pub const FREEARC_ERRCODE_IO: c_int = -6;

/// Safe-ish wrapper over the read/write callback protocol.
///
/// The C side drives compression by calling back for input and output:
///   callback("read",  buf, len, aux) -> bytes actually read
///   callback("write", buf, len, aux) -> bytes actually written
pub struct Io {
    callback: CallbackFunc,
    auxdata: *mut c_void,
}

impl Io {
    /// # Safety
    /// `callback` and `auxdata` must be exactly what the C caller passed in,
    /// and must stay valid for the lifetime of this `Io`.
    pub unsafe fn new(callback: CallbackFunc, auxdata: *mut c_void) -> Self {
        Io { callback, auxdata }
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
        // does the same cast.
        self.call(b"write\0", buf.as_ptr() as *mut c_void, clamp_len(buf.len()))
    }

    /// Write the whole buffer, mapping a short write to an IO error the way
    /// `checked_write` does in Compression/Compression.h.
    pub fn write_all(&self, buf: &[u8]) -> Result<(), c_int> {
        let want = clamp_len(buf.len());
        let got = self.write(buf);
        if got == want {
            Ok(())
        } else if got >= 0 {
            Err(FREEARC_ERRCODE_IO)
        } else {
            Err(got)
        }
    }
}

/// The callback takes an `int` length. Anything above `c_int::MAX` cannot be
/// expressed, and silently truncating is how a 16 GB value became 0 in
/// `GetPhysicalMemory`. Callers never pass buffers that large, so clamp
/// explicitly and make the boundary visible rather than casting with `as`.
fn clamp_len(n: usize) -> c_int {
    debug_assert!(n <= c_int::MAX as usize, "buffer longer than c_int can express");
    core::cmp::min(n, c_int::MAX as usize) as c_int
}
