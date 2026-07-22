//! Cryptographic primitives for DArc, replacing the vendored LibTomCrypt.
//!
//! Encryption is not "format-valid is enough" territory. An archive written
//! with -p can only be opened again if the key is derived identically and the
//! cipher is driven identically, down to the counter's byte order. Every
//! function here is therefore pinned to what Compression/_Encryption does, and
//! verified against vectors from an independent implementation rather than
//! against itself.

#![forbid(unsafe_code)]

pub mod cfb;
pub mod random;

/// Serpent S-boxes, generated from the C. Not yet a usable cipher -- the key
/// schedule and round functions are still to come.
pub mod serpent_sboxes;
pub mod ctr;

use hmac::Hmac;
use sha2::Sha512;

/// Derive `out.len()` bytes from a password and salt, exactly as
/// `Pbkdf2Hmac` in C_Encryption.cpp:159 does: PKCS#5 algorithm 2 over
/// HMAC-SHA512.
///
/// `iterations` is whatever the archive recorded. It is not clamped here: a
/// stored archive may name any count, and refusing to reproduce it would make
/// that archive unreadable. Policy about acceptable counts belongs to the
/// caller creating archives, not to the function reading them.
pub fn pbkdf2_hmac_sha512(password: &[u8], salt: &[u8], iterations: u32, out: &mut [u8]) {
    pbkdf2::pbkdf2::<Hmac<Sha512>>(password, salt, iterations, out)
        .expect("HMAC-SHA512 accepts keys of any length, so this cannot fail");
}
