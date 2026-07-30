//! Cryptographic primitives for DArc, replacing the vendored LibTomCrypt.
//!
//! Encryption is not "format-valid is enough" territory. An archive written
//! with -p can only be opened again if the key is derived identically and the
//! cipher is driven identically, down to the counter's byte order. Every
//! function here is therefore pinned to what Compression/_Encryption does, and
//! verified against vectors from an independent implementation rather than
//! against itself.

// The crypto logic (ctr, cfb, pbkdf2, random) is unsafe-free and denied at the
// crate level; the FFI boundary (ffi, exports) needs unsafe and opts back in
// per-module, so the guarantee holds everywhere it can.
#![deny(unsafe_code)]
// Totality, matching darc-codecs. CI additionally rejects `if let` anywhere under
// rust/, so an unhandled case has to be written down rather than being the shape
// of the code. This crate had zero `if let` when that gate was widened.
//
// The three existing `_ =>` arms in exports.rs are unaffected: they match on
// integers from the C caller (`cipher`, `key.len()`), not enums, so this lint
// does not apply to them -- and they are correct as they stand, rejecting an
// unknown cipher id with FREEARC_ERRCODE_INVALID_COMPRESSOR. A c_int cannot be
// matched exhaustively; an enum can, which is what this guards.
#![deny(clippy::wildcard_enum_match_arm)]
#![deny(clippy::todo, clippy::unimplemented, clippy::mem_forget)]
#![deny(unused_must_use)]
#![allow(clippy::single_match)]
// Same policy as darc-codecs, and the same reason: these functions are reached
// over the C ABI, where an unwind is undefined behaviour. Three sites opt out
// individually with their justification attached; see them for why, and for the
// better fix that would remove the need.
#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

pub mod cfb;
pub mod ffi;
pub mod exports;
pub mod random;

// Serpent is NOT hand-ported: DArc's Serpent is standard Serpent (see
// rust/cryptref/serpent32.c for why it only looked otherwise), so the
// RustCrypto `serpent` crate substitutes directly, driven through the same
// `cipher` traits as aes/twofish/blowfish. The vectors it is checked against
// live in tests/serpent_vectors.rs.
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
/// `Err(())` only for an output length the KDF rejects (zero, or absurdly large).
/// Returned rather than asserted: the sole caller is an `extern "C"` export that
/// already reports failure as an error code, and unwinding across that boundary
/// is undefined behaviour.
pub fn pbkdf2_hmac_sha512(
    password: &[u8],
    salt: &[u8],
    iterations: u32,
    out: &mut [u8],
) -> Result<(), ()> {
    pbkdf2::pbkdf2::<Hmac<Sha512>>(password, salt, iterations, out).map_err(|_| ())
}
