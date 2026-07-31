//! C entry points, matching the ABI of the vendored `_Encryption` so the shim
//! can forward to them one function at a time.
//!
//! Three symbols cover everything the archiver needs:
//!   darc_rs_docrypt              the streaming encrypt/decrypt loop
//!   darc_rs_pbkdf2_hmac_sha512   the KDF
//!   darc_rs_random_fill          secure random bytes (salts, IVs)
//!
//! Cipher and mode ids follow the vendored LibTomCrypt registration order,
//! which parse_ENCRYPTION already stored and the shim preserves:
//!   cipher: 0=aes 1=blowfish 2=serpent 3=twofish
//!   mode:   0=ctr 1=cfb
//! See darc-crypto/WIRING.md, all measured from the C.

#![allow(unsafe_code)]

use crate::ffi::{Io, CALLBACK_FUNC, OK};
use crate::{cfb, ctr, pbkdf2_hmac_sha512, random};
use crate::ffi::{FREEARC_ERRCODE_GENERAL, FREEARC_ERRCODE_INVALID_COMPRESSOR};
use cipher::{BlockCipherEncrypt, BlockSizeUser, KeyInit};
use core::ffi::{c_int, c_void};

/// docrypt reads and writes in 256 KB chunks (LARGE_BUFFER_SIZE). The size does
/// not affect output -- CTR and CFB are stream/feedback modes whose state
/// crosses chunk boundaries -- but matching it keeps behaviour identical.
const BUF: usize = 256 * 1024;

const ENCRYPT: c_int = 0;

/// # Safety
/// `callback`/`auxdata` must be what the C caller supplied; `key`/`iv` must
/// point to `key_len`/one-block bytes respectively.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_docrypt(
    do_encryption: c_int,
    cipher: c_int,
    mode: c_int,
    key: *const u8,
    key_len: c_int,
    _rounds: c_int,
    iv: *const u8,
    callback: CALLBACK_FUNC,
    auxdata: *mut c_void,
) -> c_int {
    guard(move || {
        let io = match Io::new(callback, auxdata) {
            Some(io) => io,
            None => return FREEARC_ERRCODE_GENERAL,
        };
        if key.is_null() || iv.is_null() || key_len < 0 {
            return FREEARC_ERRCODE_GENERAL;
        }
        let key = std::slice::from_raw_parts(key, key_len as usize);
        let encrypting = do_encryption == ENCRYPT;

        // Dispatch cipher x (aes key length) into a concrete type, then run the
        // chosen mode over the callback loop. `_rounds` is unused: DArc always
        // passes 0 (cipher default), the only case the fixed-round crates cover.
        match cipher {
            0 => match key.len() {
                16 => run::<aes::Aes128>(key, iv, mode, encrypting, &io),
                24 => run::<aes::Aes192>(key, iv, mode, encrypting, &io),
                32 => run::<aes::Aes256>(key, iv, mode, encrypting, &io),
                _ => FREEARC_ERRCODE_INVALID_COMPRESSOR,
            },
            1 => run::<blowfish::Blowfish>(key, iv, mode, encrypting, &io),
            2 => run::<serpent::Serpent>(key, iv, mode, encrypting, &io),
            3 => run::<twofish::Twofish>(key, iv, mode, encrypting, &io),
            _ => FREEARC_ERRCODE_INVALID_COMPRESSOR,
        }
    })
}

fn run<C>(key: &[u8], iv: *const u8, mode: c_int, encrypting: bool, io: &Io) -> c_int
where
    C: BlockCipherEncrypt + BlockSizeUser + KeyInit,
{
    let cipher = match C::new_from_slice(key) {
        Ok(c) => c,
        Err(_) => return FREEARC_ERRCODE_INVALID_COMPRESSOR,
    };
    let bs = C::block_size();
    // SAFETY: the C side sizes iv to the cipher's block length (ivSize =
    // cipher_descriptor[cipher].block_length), which is what `bs` is.
    let iv = unsafe { std::slice::from_raw_parts(iv, bs) };

    match mode {
        0 => {
            let mut st = ctr::Ctr::new(&cipher, iv);
            drive(io, |buf| st.apply(buf))
        }
        1 => {
            let mut st = cfb::Cfb::new(&cipher, iv);
            drive(io, |buf| {
                if encrypting {
                    st.encrypt(buf)
                } else {
                    st.decrypt(buf)
                }
            })
        }
        _ => FREEARC_ERRCODE_INVALID_COMPRESSOR,
    }
}

/// The read/encrypt/write loop, mirroring docrypt's structure. docrypt keeps a
/// RemainderSize but its own comment notes it is always 0 for these modes --
/// every byte is processed, so OutSize == InSize and nothing carries over.
fn drive(io: &Io, mut apply: impl FnMut(&mut [u8])) -> c_int {
    let mut buf = vec![0u8; BUF];
    loop {
        let got = io.read(&mut buf);
        if got < 0 {
            return got; // read error -> propagate the code, as docrypt does
        }
        if got == 0 {
            return OK; // clean end of input
        }
        let n = got as usize;
        apply(&mut buf[..n]);
        let w = io.write(&buf[..n]);
        if w < 0 {
            return w;
        }
    }
}

/// # Safety
/// pointers must reference the stated lengths.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_pbkdf2_hmac_sha512(
    pwd: *const u8,
    pwd_len: c_int,
    salt: *const u8,
    salt_len: c_int,
    iterations: c_int,
    out: *mut u8,
    out_len: c_int,
) -> c_int {
    guard(move || {
        if pwd.is_null() || salt.is_null() || out.is_null()
            || pwd_len < 0 || salt_len < 0 || out_len < 0 || iterations < 0
        {
            return FREEARC_ERRCODE_GENERAL;
        }
        let pwd = std::slice::from_raw_parts(pwd, pwd_len as usize);
        let salt = std::slice::from_raw_parts(salt, salt_len as usize);
        let out = std::slice::from_raw_parts_mut(out, out_len as usize);
        match pbkdf2_hmac_sha512(pwd, salt, iterations as u32, out) {
            Ok(()) => OK,
            // Only reachable for an out_len the KDF refuses; the C caller sees an
            // error code instead of a panic crossing the ABI.
            Err(()) => FREEARC_ERRCODE_GENERAL,
        }
    })
}

/// Fill `buf` with cryptographically secure random bytes. Backs the fortuna_*
/// ABI, which the shim reduces to this call; the stored salt means the PRNG
/// only has to be secure, not reproducible.
///
/// # Safety
/// `buf` must reference `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn darc_rs_random_fill(buf: *mut u8, len: c_int) -> c_int {
    guard(move || {
        if buf.is_null() || len < 0 {
            return FREEARC_ERRCODE_GENERAL;
        }
        let buf = std::slice::from_raw_parts_mut(buf, len as usize);
        match random::fill_secure(buf) {
            Ok(()) => OK,
            Err(_) => FREEARC_ERRCODE_GENERAL,
        }
    })
}

/// Run an entry point behind an unwind firewall — see the twin in
/// `darc-codecs`'s `ffi` module for the reasoning. A panic crossing an
/// `extern "C"` frame is undefined behaviour, and these are reached from `unarc`
/// and the SFX modules, compiled `-D_NO_EXCEPTIONS`.
fn guard<F: FnOnce() -> c_int>(f: F) -> c_int {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(code) => code,
        Err(_) => FREEARC_ERRCODE_GENERAL,
    }
}
