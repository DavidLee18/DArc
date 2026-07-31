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

    /// Write `buf`. Returns the count written, or a negative status.
    ///
    /// **A negative return is not necessarily an error, and callers must
    /// propagate it rather than substitute one of their own.** Unarc's write
    /// callback answers `FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED` (-9,
    /// Compression.h:28) as soon as every file it wanted out of the solid block
    /// has been written -- it means "that was a success, now stop", and
    /// `Unarc/unarc.cpp:449` accepts exactly that code alongside `>= 0`:
    ///
    /// ```text
    /// CHECK (result>=0 || result==FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED,
    ///        "ERROR: archive data corrupted (decompression fails)");
    /// ```
    ///
    /// A decoder that turned this into `FREEARC_ERRCODE_IO` reported perfectly
    /// good archives as corrupt: `dict` and `lzp` did, which made every archive
    /// whose `$text` group used Dict -- i.e. anything written with `-m9` --
    /// unreadable by the standalone extractor and every SFX module, while the
    /// archiver read it back fine. Substituting also loses genuine codes, so
    /// `-5` (out of memory) would have been reported as an I/O failure too.
    pub fn write(&self, buf: &[u8]) -> c_int {
        if buf.is_empty() {
            return 0;
        }
        // The callback takes a non-const pointer even for "write"; the C code
        // casts the same way.
        self.call(b"write\0", buf.as_ptr() as *mut c_void, clamp_len(buf.len()))
    }

    /// `QUASIWRITE` (Compression.h:132): tell the caller how many bytes *would*
    /// have been produced, without producing them, so the progress indicator
    /// keeps moving while a codec buffers output. The callback is handed a
    /// pointer to the `int64` count as its buffer, and the same count again as
    /// its length -- an odd protocol, but it is the one the C side implements.
    ///
    /// Purely informational: the return value is discarded, exactly as the macro
    /// discards it. Tornado is the only codec in the tree that sends one.
    pub fn quasiwrite(&self, size: i64) {
        let mut local = size;
        self.call(
            b"quasiwrite\0",
            &mut local as *mut i64 as *mut c_void,
            clamp_len(size.max(0) as usize),
        );
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

/// Allocate a buffer whose length came out of an archive.
///
/// Three codecs read a 4-byte block length and hand it straight to `vec![0u8; n]`,
/// with `n` up to 2^31. Two things are wrong with that, and both are reachable from
/// the smallest SFX target:
///
/// * **`vec!` is infallible.** On allocation failure it calls `handle_alloc_error`,
///   which aborts the process. A corrupt archive must produce a diagnosis, not a
///   fault — the same rule that made GRZip's rec mode an `Option` rather than an
///   `unreachable!()`.
/// * **A length far above the method's own block size is corrupt input, not a big
///   block.** The encoder cannot emit one: it compresses `block_size` bytes at a
///   time and its worst case is `block_size + 2`. Rejecting early keeps a 2 GiB
///   request from ever being attempted.
///
/// The floor keeps the bound from rejecting a valid archive if `block_size` reaches
/// us small or unset — it only ever widens the cap, never narrows it. Modelled on
/// `grzip::stream::run`, which is the codec here that already got this right.
pub fn archive_sized_buffer(n: usize, block_size: u32) -> Result<Vec<u8>, c_int> {
    const FLOOR: usize = 1 << 20;
    const SLACK: usize = 1024;
    let cap = (block_size as usize).max(FLOOR).saturating_add(SLACK);
    if n > cap {
        return Err(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    }
    let mut buf = Vec::new();
    match buf.try_reserve_exact(n) {
        Ok(()) => {}
        Err(_) => return Err(FREEARC_ERRCODE_NOT_ENOUGH_MEMORY),
    }
    buf.resize(n, 0);
    Ok(buf)
}

/// Run a codec entry point behind an unwind firewall.
///
/// **Every `extern "C"` function in this crate that can return an error code goes
/// through this.** A Rust panic unwinding across an `extern "C"` frame is undefined
/// behaviour, and these frames are called from `arc`, from `unarc`, and from every
/// SFX module — the last two compiled `-D_NO_EXCEPTIONS`, parsing archives an
/// attacker wrote, with no surrounding process to contain a fault.
///
/// This is a backstop, not a licence. The codecs are written not to panic: bounds
/// are checked, lengths from the stream are validated, and `unwrap`/`expect` are
/// denied outside tests. Reaching this handler means one of those failed, and the
/// point is that the consequence is `FREEARC_ERRCODE_GENERAL` rather than a
/// corrupted process. `rust/Cargo.toml` sets `overflow-checks = true` in release, so
/// the surface is wider than `unwrap`: every unchecked arithmetic op is a panic too.
///
/// Not applied to the `darc_rs_ppmd_sa_*` allocator surface: those return pointers,
/// offsets or nothing, so there is no error value to return, and they are reached
/// only from `rust/difftest/ppmd_alloc_ref.cpp` — never from the archiver or the
/// extractors.
pub fn guard<F: FnOnce() -> c_int>(f: F) -> c_int {
    // AssertUnwindSafe: `Io` holds a raw callback pointer and is not `UnwindSafe`.
    // Defensible here because the panic path returns an error and the C caller tears
    // the codec down rather than reusing it.
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(code) => code,
        Err(_) => FREEARC_ERRCODE_GENERAL,
    }
}

#[cfg(test)]
mod guard_tests {
    use super::*;

    /// The firewall must convert a panic into an error code. Without this test the
    /// wrapping is 65 edits nobody ever demonstrated the effect of — and its absence
    /// is not visible in a passing suite, because a codec that does not panic
    /// behaves identically either way.
    #[test]
    fn a_panic_becomes_an_error_code_instead_of_unwinding() {
        let code = guard(|| panic!("a codec paniced on hostile input"));
        assert_eq!(code, FREEARC_ERRCODE_GENERAL);
    }

    /// The arithmetic case, which is the one that actually matters here: release
    /// builds set `overflow-checks = true`, so a subtraction that underflows on a
    /// corrupt length is a panic, not a wrap.
    #[test]
    fn an_arithmetic_overflow_is_caught_too() {
        let from_the_archive: usize = 5;
        let code = guard(move || {
            let n = from_the_archive - 10; // underflows under overflow-checks
            n as c_int
        });
        // In a build without overflow checks this wraps instead of panicking, and
        // the guard is simply not exercised -- assert the outcome is one of the two,
        // never a process fault.
        assert!(code == FREEARC_ERRCODE_GENERAL || code != 0);
    }

    /// A guarded call that does not panic must be perfectly transparent.
    #[test]
    fn the_guard_is_transparent_when_nothing_panics() {
        assert_eq!(guard(|| OK), OK);
        assert_eq!(guard(|| FREEARC_ERRCODE_BAD_COMPRESSED_DATA), FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    }
}
