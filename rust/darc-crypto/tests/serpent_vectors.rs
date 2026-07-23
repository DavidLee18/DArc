//! Serpent ECB vectors.
//!
//! These come from DArc's own algorithm compiled with `ulong32` at its
//! INTENDED 32 bits -- see `rust/cryptref/serpent32.c`. They agree with the
//! RustCrypto `serpent` crate, i.e. with standard Serpent.
//!
//! That agreement is the point. DArc's vendored Serpent is standard Serpent;
//! it only *appears* not to be on this machine, because
//! `Compression/_Encryption/headers/tomcrypt_macros.h:13` types `ulong32` as
//! `unsigned` for `__x86_64__` and sparc64 and as `unsigned long` -- 64 bits --
//! for everything else. serpent.c's key expansion rotates with a raw
//! `(lk << 11) | (lk >> 21)` instead of LTC's masked `ROL`, which is a rotate
//! at 32 bits and nonsense at 64.
//!
//! So an ARM64 build of DArc encrypts `-ae serpent` differently from an x86-64
//! build, and `serpent_test()` fails there -- invisibly, since
//! `C_Encryption.cpp` defines `LTC_NO_TEST`. This port implements the correct
//! 32-bit behaviour, which is what x86-64 DArc has always produced.

use cipher::{BlockCipherDecrypt, BlockCipherEncrypt, KeyInit};
use serpent::Serpent;

/// (name, key length in bytes, plaintext hex, ciphertext hex)
const VECTORS: &[(&str, usize, &str, &str)] = &[
    ("k256", 32, "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", "a15da3f63af78cb97bfdb61dfd02c6b0"),
    ("k192", 24, "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", "96020ee021c9d406c9c89c851617a0d4"),
    ("k128", 16, "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", "6ed3c45e6e8648a166f77757476ad337"),
    ("k160", 20, "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff", "2a814b3a4b44df1a4be9c2e6e846a446"),
    ("zeros", 32, "00000000000000000000000000000000", "49672ba898d98df95019180445491089"),
    ("ones", 32, "ffffffffffffffffffffffffffffffff", "6ac7579d9377845a816ca6d758f3feff"),
];

fn unhex(s: &str) -> Vec<u8> {
    (0..s.len() / 2).map(|i| u8::from_str_radix(&s[i * 2..i * 2 + 2], 16).unwrap()).collect()
}
fn hex(b: &[u8]) -> String { b.iter().map(|x| format!("{x:02x}")).collect() }

fn key_for(name: &str, len: usize) -> Vec<u8> {
    match name {
        "zeros" => vec![0u8; 32],
        "ones" => vec![0xffu8; 32],
        _ => (0u8..len as u8).collect(),
    }
}

#[test]
fn encrypt_matches_the_32_bit_reference() {
    for &(name, klen, pt, ct) in VECTORS {
        let cipher = Serpent::new_from_slice(&key_for(name, klen)).expect("key rejected");
        let mut block = *cipher::array::Array::<u8, _>::from_slice(&unhex(pt));
        cipher.encrypt_block(&mut block);
        assert_eq!(hex(&block[..]), ct, "{name}: ciphertext differs");
    }
}

#[test]
fn decrypt_inverts_encrypt() {
    for &(name, klen, pt, ct) in VECTORS {
        let cipher = Serpent::new_from_slice(&key_for(name, klen)).expect("key rejected");
        let mut block = *cipher::array::Array::<u8, _>::from_slice(&unhex(ct));
        cipher.decrypt_block(&mut block);
        assert_eq!(hex(&block[..]), pt, "{name}: decrypt did not recover the plaintext");
    }
}

#[test]
fn short_keys_take_the_padding_path() {
    // 160 bits is not a whole number of words, so Serpent's key padding runs.
    // Kept because the reference vector was generated for exactly this length;
    // it confirms the crate pads keys the way DArc's C does.
    let v = VECTORS.iter().find(|v| v.0 == "k160").expect("k160 vector missing");
    let cipher = Serpent::new_from_slice(&key_for("k160", 20)).unwrap();
    let mut block = *cipher::array::Array::<u8, _>::from_slice(&unhex(v.2));
    cipher.encrypt_block(&mut block);
    assert_eq!(hex(&block[..]), v.3);
}

#[test]
fn rejects_over_long_keys() {
    assert!(Serpent::new_from_slice(&vec![0u8; 33]).is_err(), "accepted a 264-bit key");
}
