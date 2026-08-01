//! The C ABI, unchanged from `Compression/7z/C_7z.c`.
//!
//! These three symbols are declared in `Compression.h:374-376` and imported by
//! `Arc7z.hs:26-31`. Signatures, argument order and return type are fixed by
//! those two files; only the implementation behind them moved.

// Reading a `*const c_char` the caller owns needs unsafe. The crate denies it
// globally so that the reader logic in lib.rs cannot acquire any.
#![allow(unsafe_code)]

use std::ffi::CStr;
use std::os::raw::{c_char, c_int};
use std::path::Path;

use crate::sres;

/// Turn an unwinding panic into an `SRes` rather than undefined behaviour.
///
/// These frames are called from Haskell across a C ABI, where unwinding past
/// the boundary is UB. `overflow-checks = true` in the workspace profile means
/// arithmetic traps too, so the surface is wider than the obvious `unwrap`.
/// Mirrors `darc_codecs::ffi::guard`, which cannot be reused without depending
/// on that whole crate.
fn guard<F: FnOnce() -> c_int>(f: F) -> c_int {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(code) => code,
        Err(_) => sres::FAIL,
    }
}

/// Borrow a caller-supplied C string as a path.
///
/// Returns `None` for NULL or for bytes that are not UTF-8. The C took the
/// pointer as-is; refusing non-UTF-8 is new, and it is the right refusal here
/// because the alternative is silently extracting to a different path than the
/// user named.
///
/// # Safety
/// `ptr` must be NULL or point to a NUL-terminated string that stays valid for
/// the duration of the call.
unsafe fn as_path<'a>(ptr: *const c_char) -> Option<&'a Path> {
    match ptr.is_null() {
        true => None,
        false => match CStr::from_ptr(ptr).to_str() {
            Ok(s) => Some(Path::new(s)),
            Err(_) => None,
        },
    }
}

/// List the archive at `path`. Returns an `SRes`.
///
/// # Safety
/// `path` must be a valid NUL-terminated string.
#[no_mangle]
pub unsafe extern "C" fn darc_7z_list(path: *const c_char) -> c_int {
    guard(move || match as_path(path) {
        Some(path) => crate::list(path),
        None => sres::PARAM,
    })
}

/// Extract the archive at `path` into `out_dir`. Returns an `SRes`.
///
/// A NULL `out_dir` means the current directory, matching `C_7z.c:221`.
///
/// # Safety
/// `path` must be a valid NUL-terminated string; `out_dir` must be NULL or one.
#[no_mangle]
pub unsafe extern "C" fn darc_7z_extract(path: *const c_char, out_dir: *const c_char) -> c_int {
    guard(move || {
        let archive = match as_path(path) {
            Some(path) => path,
            None => return sres::PARAM,
        };
        // C_7z.c:221 -- `out_dir ? out_dir : "."`.
        let dest = match out_dir.is_null() {
            true => Path::new("."),
            false => match as_path(out_dir) {
                Some(dir) => dir,
                None => return sres::PARAM,
            },
        };
        crate::extract_or_test(archive, Some(dest))
    })
}

/// Test the archive at `path` without writing anything. Returns an `SRes`.
///
/// # Safety
/// `path` must be a valid NUL-terminated string.
#[no_mangle]
pub unsafe extern "C" fn darc_7z_test(path: *const c_char) -> c_int {
    guard(move || match as_path(path) {
        Some(path) => crate::extract_or_test(path, None),
        None => sres::PARAM,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn null_path_is_a_param_error_not_a_crash() {
        assert_eq!(unsafe { darc_7z_list(std::ptr::null()) }, sres::PARAM);
        assert_eq!(unsafe { darc_7z_test(std::ptr::null()) }, sres::PARAM);
        assert_eq!(
            unsafe { darc_7z_extract(std::ptr::null(), std::ptr::null()) },
            sres::PARAM
        );
    }

    #[test]
    fn missing_archive_reports_fail_like_the_c() {
        // C_7z.c:99 returned SZ_ERROR_FAIL when InFile_Open failed.
        let path = match CString::new("/nonexistent/definitely-not-here.7z") {
            Ok(s) => s,
            Err(_) => return,
        };
        assert_eq!(unsafe { darc_7z_list(path.as_ptr()) }, sres::FAIL);
    }

    #[test]
    fn a_non_7z_file_is_refused_as_not_an_archive() {
        let dir = std::env::temp_dir().join("darc-sevenz-not-an-archive");
        drop(std::fs::create_dir_all(&dir));
        let file = dir.join("plain.7z");
        match std::fs::write(&file, b"this is not a 7z archive at all") {
            Ok(()) => {}
            Err(_) => return,
        }
        let c = match CString::new(match file.to_str() {
            Some(s) => s,
            None => return,
        }) {
            Ok(s) => s,
            Err(_) => return,
        };
        assert_eq!(unsafe { darc_7z_list(c.as_ptr()) }, sres::NO_ARCHIVE);
        drop(std::fs::remove_file(&file));
    }

    #[test]
    fn guard_converts_a_panic_into_an_sres() {
        assert_eq!(guard(|| panic!("boom")), sres::FAIL);
        assert_eq!(guard(|| sres::OK), sres::OK);
    }
}
