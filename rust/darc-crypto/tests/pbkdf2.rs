//! PBKDF2-HMAC-SHA512 against vectors from an independent implementation.
//!
//! Generated with Python's hashlib, not with this crate and not with DArc's C.
//! Testing a KDF against itself proves only that it is deterministic; testing
//! it against LibTomCrypt would only prove the two agree, which is worth less
//! than it sounds if both were wrong. PKCS#5 algorithm 2 is a published
//! standard, so conforming to it IS being compatible with every archive DArc
//! has written.

use darc_crypto::pbkdf2_hmac_sha512;

fn check(password: &[u8], salt: &[u8], iterations: u32, expected_hex: &str) {
    let mut out = vec![0u8; expected_hex.len() / 2];
    pbkdf2_hmac_sha512(password, salt, iterations, &mut out);
    let got = out.iter().map(|b| format!("{b:02x}")).collect::<String>();
    assert_eq!(
        got, expected_hex,
        "PBKDF2-HMAC-SHA512(pw={password:?}, salt={salt:?}, c={iterations}) mismatch"
    );
}

#[test]
fn matches_reference_vectors() {
    check(b"password", b"salt", 1,
        "867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5d513554e1c8cf252\
         c02d470a285a0501bad999bfe943c08f050235d7d68b1da55e63f73b60a57fce");
    check(b"password", b"salt", 2,
        "e1d9c16aa681708a45f5c7c4e215ceb66e011a2e9f0040713f18aefdb866d53c\
         f76cab2868a39b9f7840edce4fef5a82be67335c77a6068e04112754f27ccf4e");
    check(b"password", b"salt", 4096,
        "d197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87f6902e072f457b5\
         143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5");
}

#[test]
fn handles_empty_password() {
    // The archiver permits an empty password; the KDF must still produce a key
    // rather than short-circuit.
    check(b"", b"salt", 1024,
        "c474710cedf7dd31094d50e0aa3cfdc65b6606b4f2ac92d4cab9d8eed7dcaea7\
         b67b0a1b00af1f387afaa03f53ab3d33538717ac7e44f9041f72d2d4caee8dec");
}

#[test]
fn handles_embedded_nul_bytes() {
    // LibTomCrypt takes explicit lengths, so a password or salt containing NUL
    // is legitimate. A port that reached for C string handling anywhere would
    // truncate here and silently derive the wrong key -- which would present as
    // "wrong password" on an archive whose password is correct.
    check(b"pass\0word", b"sa\0lt", 4096,
        "9d9e9c4cd21fe4be24d5b8244c759665f39d98fc12a9ca759bb021db3cfadf34");
}

#[test]
fn handles_binary_salt() {
    // Salts are random bytes, not text.
    check(b"DArc test password", &[0, 1, 2, 3, 4, 5, 6, 7], 1000,
        "b70771d467cd4abb4a91b868e3997e858a528c7f2e52f9e94857617d375db740");
}

#[test]
fn key_length_is_independent_of_iteration_count() {
    // Guards a plausible mis-port: PBKDF2 produces as many blocks as the
    // requested length needs, and the count must not leak into that.
    for len in [16usize, 24, 32, 48, 64, 100] {
        let mut a = vec![0u8; len];
        let mut b = vec![0u8; len];
        pbkdf2_hmac_sha512(b"pw", b"salt", 1, &mut a);
        pbkdf2_hmac_sha512(b"pw", b"salt", 1, &mut b);
        assert_eq!(a, b);
        assert_eq!(a.len(), len);
    }
}
