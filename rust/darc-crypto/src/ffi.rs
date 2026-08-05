//! The C ABI boundary, generated from DArc's own headers.
//!
//! The declarations are not written by hand. build.rs runs bindgen over
//! Compression/Common.h and Compression/Compression.h -- the same headers the C
//! side compiles against -- so `CALLBACK_FUNC` and the `FREEARC_ERRCODE_*`
//! constants cannot drift from their C definitions. This codebase was bitten by
//! hand-transcribed ABI before: 41 helpers whose declarations disagreed with
//! their definitions, 8 truncating a `long` return to `int`. Hand-writing the
//! callback signature here would be the same mistake in a new place.
//!
//! (This mirrors darc-codecs/src/ffi.rs. The two are candidates to share a
//! `darc-ffi` crate; kept separate for now to avoid a cross-crate refactor of
//! the codec linkage.)

#![allow(non_camel_case_types, non_upper_case_globals, dead_code)]
#![allow(unsafe_code)]

use core::ffi::{c_char, c_int, c_void};

include!(concat!(env!("OUT_DIR"), "/darc_abi.rs"));

pub const OK: c_int = FREEARC_OK as c_int;

/// Safe wrapper over the read/write callback protocol docrypt is driven by:
///   callback("read",  buf, len, aux) -> bytes read (or <0 error)
///   callback("write", buf, len, aux) -> bytes written (or <0 error)
pub struct Io {
    callback: unsafe extern "C" fn(*const c_char, *mut c_void, c_int, *mut c_void) -> c_int,
    auxdata: *mut c_void,
}

impl Io {
    /// `None` when the C caller passed a null callback (the ABI permits it);
    /// calling through it would be undefined behaviour.
    ///
    /// # Safety
    /// `callback`/`auxdata` must be what the C caller supplied and stay valid
    /// for this `Io`'s lifetime.
    pub unsafe fn new(callback: CALLBACK_FUNC, auxdata: *mut c_void) -> Option<Self> {
        callback.map(|callback| Io { callback, auxdata })
    }

    fn call(&self, what: &[u8], buf: *mut c_void, size: c_int) -> c_int {
        assert_eq!(what.last(), Some(&0), "`what` must be NUL-terminated");
        unsafe { (self.callback)(what.as_ptr() as *const c_char, buf, size, self.auxdata) }
    }

    /// Read up to `buf.len()` bytes; returns the count or a negative error.
    pub fn read(&self, buf: &mut [u8]) -> c_int {
        if buf.is_empty() {
            return 0;
        }
        self.call(b"read\0", buf.as_mut_ptr() as *mut c_void, clamp_len(buf.len()))
    }

    /// Write `buf`; returns the count written or a negative error.
    pub fn write(&self, buf: &[u8]) -> c_int {
        if buf.is_empty() {
            return 0;
        }
        self.call(b"write\0", buf.as_ptr() as *mut c_void, clamp_len(buf.len()))
    }
}

fn clamp_len(n: usize) -> c_int {
    assert!(n <= c_int::MAX as usize, "buffer longer than c_int can express");
    n as c_int
}
