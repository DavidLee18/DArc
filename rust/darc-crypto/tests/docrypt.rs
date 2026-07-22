//! End-to-end test of `darc_rs_docrypt`: the whole path the archiver takes --
//! cipher-id dispatch, mode selection, and the read/encrypt/write loop --
//! against ciphertext DArc's own crypto produced.
//!
//! The reference bytes are the CTR keystream and CFB vectors already committed
//! in ctr_vectors.rs / cfb_vectors.rs (dumped from the vendored LibTomCrypt
//! with the archiver's parameters). docrypt calls the same ctr_start/ctr_encrypt
//! underneath, so reproducing them proves this docrypt is wire-compatible.

use darc_crypto::exports::darc_rs_docrypt;
use std::ffi::{c_char, c_int, c_void, CStr};

struct Mem {
    input: Vec<u8>,
    pos: usize,
    output: Vec<u8>,
    // Serve at most `chunk` bytes per read, so docrypt's loop is driven round
    // more than once and cross-chunk state is actually exercised.
    chunk: usize,
}

unsafe extern "C" fn cb(what: *const c_char, buf: *mut c_void, size: c_int, aux: *mut c_void) -> c_int {
    let m = &mut *(aux as *mut Mem);
    let what = CStr::from_ptr(what).to_bytes();
    let size = if size < 0 { return -1 } else { size as usize };
    if what == b"read" {
        let n = size.min(m.chunk).min(m.input.len() - m.pos);
        if n > 0 {
            std::ptr::copy_nonoverlapping(m.input[m.pos..].as_ptr(), buf as *mut u8, n);
            m.pos += n;
        }
        n as c_int
    } else if what == b"write" {
        if size > 0 {
            m.output.extend_from_slice(std::slice::from_raw_parts(buf as *const u8, size));
        }
        size as c_int
    } else {
        0
    }
}

/// Run docrypt over `input` and return (status, output).
fn docrypt(encrypt: bool, cipher: c_int, mode: c_int, key: &[u8], iv: &[u8], input: &[u8], chunk: usize) -> (c_int, Vec<u8>) {
    let mut mem = Mem { input: input.to_vec(), pos: 0, output: Vec::new(), chunk };
    let rc = unsafe {
        darc_rs_docrypt(
            if encrypt { 0 } else { 1 },
            cipher, mode,
            key.as_ptr(), key.len() as c_int,
            0,
            iv.as_ptr(),
            Some(cb),
            &mut mem as *mut Mem as *mut c_void,
        )
    };
    (rc, mem.output)
}

fn unhex(s: &str) -> Vec<u8> {
    (0..s.len() / 2).map(|i| u8::from_str_radix(&s[i * 2..i * 2 + 2], 16).unwrap()).collect()
}
fn hex(b: &[u8]) -> String { b.iter().map(|x| format!("{x:02x}")).collect() }
fn key32() -> Vec<u8> { (0u8..32).collect() }
fn iv16() -> Vec<u8> { (0..16).map(|i| 0xf0u8.wrapping_add(i as u8)).collect() }

// cipher ids: 0=aes 1=blowfish 2=serpent 3=twofish ; mode 0=ctr 1=cfb

#[test]
fn ctr_keystreams_match_darc() {
    // Encrypting 48 zero bytes yields the raw keystream, i.e. the ctr_vectors.
    let zeros = vec![0u8; 48];
    let cases: &[(&str, c_int, Vec<u8>, Vec<u8>, &str)] = &[
        ("aes256", 0, key32(), iv16(),
         "9200cd8d239680cb5a69e65440326314f043a8612107c7bb43c777759c642869e44100fb153133ad08dd0c0e97154009"),
        ("twofish", 3, key32(), iv16(),
         "7aab398567ef74f73945f0663768c404950ac57283f6ca017a74a6a9282341cfff112497d2f9fd8fad73ad37bbd6fd29"),
        // Correct (32-bit) Serpent CTR keystream from rust/cryptref/serpent32.c,
         // i.e. what x86-64 DArc produces. The committed CTR *ctr_vectors* omit
         // serpent because those were dumped on ARM64 where it is miscompiled.
         ("serpent", 2, key32(), iv16(),
         "a15da3f63af78cb97bfdb61dfd02c6b014c58cadc7c099399e5e848e73069b0a64769d87485d9ba7899b48880f1d7b60"),
    ];
    for (name, id, key, iv, want) in cases {
        for chunk in [1usize, 7, 48, 4096] {
            let (rc, out) = docrypt(true, *id, 0, key, iv, &zeros, chunk);
            assert_eq!(rc, 0, "{name}: docrypt returned {rc}");
            assert_eq!(hex(&out), *want, "{name}: CTR keystream (chunk {chunk})");
        }
    }
}

#[test]
fn ctr_blowfish_56_byte_key_matches_darc() {
    let key: Vec<u8> = (0u8..56).collect();
    let iv: Vec<u8> = (0..8).map(|i| 0xf0u8.wrapping_add(i as u8)).collect();
    let (rc, out) = docrypt(true, 1, 0, &key, &iv, &vec![0u8; 48], 5);
    assert_eq!(rc, 0);
    assert_eq!(hex(&out),
        "70e67c34dc7771f514530722811156b60247755ad8c1ad52cbb17e7b3ed825c7e1e504227f130c1b22bb0db9458a17ed");
}

#[test]
fn cfb_aes256_matches_darc_and_round_trips() {
    // CFB plaintext is the (i*7+1) pattern the cfb harness used.
    let plain: Vec<u8> = (0..48u32).map(|i| (i as u8).wrapping_mul(7).wrapping_add(1)).collect();
    let want_ct = "9308c29b3eb2abf96329a11a156e007eb67119a5dec319b7ae6b21149f1a2aff94cbec6396213f3f2c92cce0cd73ad8c";

    for chunk in [1usize, 13, 48] {
        let (rc, ct) = docrypt(true, 0, 1, &key32(), &iv16(), &plain, chunk);
        assert_eq!(rc, 0);
        assert_eq!(hex(&ct), want_ct, "CFB encrypt (chunk {chunk})");

        // And decrypt gets the plaintext back -- CFB is not self-inverse.
        let (rc, back) = docrypt(false, 0, 1, &key32(), &iv16(), &unhex(want_ct), chunk);
        assert_eq!(rc, 0);
        assert_eq!(back, plain, "CFB decrypt (chunk {chunk})");
    }
}

#[test]
fn rejects_unknown_cipher_and_mode() {
    let (rc, _) = docrypt(true, 9, 0, &key32(), &iv16(), b"x", 16);
    assert!(rc < 0, "unknown cipher id should error");
    let (rc, _) = docrypt(true, 0, 9, &key32(), &iv16(), b"x", 16);
    assert!(rc < 0, "unknown mode id should error");
}
