//! CFB against vectors produced by DArc's own encryption code.
//!
//! Same provenance as the CTR vectors: dumped by `rust/cryptref/ctr_vectors.cpp`,
//! which includes the same LibTomCrypt sources `C_Encryption.cpp` includes and
//! calls `cfb_start`/`cfb_encrypt` with the arguments the archiver passes.
//!
//! The plaintext here is deliberately not zeros. CFB feeds ciphertext back, so
//! a zero plaintext still drives the feedback path but makes an
//! encrypt/decrypt mix-up much harder to see -- the register would end up
//! holding the keystream either way.

use darc_crypto::cfb;

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn key32() -> Vec<u8> {
    (0u8..32).collect()
}
fn iv(len: usize) -> Vec<u8> {
    (0..len).map(|i| 0xf0u8.wrapping_add(i as u8)).collect()
}
/// The C harness's plaintext: `i * 7 + 1`, truncated to a byte.
fn plaintext(n: usize) -> Vec<u8> {
    (0..n).map(|i| (i as u8).wrapping_mul(7).wrapping_add(1)).collect()
}

fn encrypt<C>(key: &[u8], iv: &[u8], n: usize) -> Vec<u8>
where
    C: cipher::BlockCipherEncrypt + cipher::BlockSizeUser + cipher::KeyInit,
{
    let c = <C as cipher::KeyInit>::new_from_slice(key).expect("key length rejected");
    let mut data = plaintext(n);
    cfb::encrypt(&c, iv, &mut data);
    data
}

#[test]
fn aes256_matches_darc() {
    assert_eq!(
        hex(&encrypt::<aes::Aes256>(&key32(), &iv(16), 48)),
        "9308c29b3eb2abf96329a11a156e007e\
         b67119a5dec319b7ae6b21149f1a2aff\
         94cbec6396213f3f2c92cce0cd73ad8c"
    );
}

#[test]
fn blowfish_matches_darc() {
    // 64-bit block: the feedback register is half the size, so a hardcoded
    // 128-bit block would diverge from the ninth byte.
    assert_eq!(
        hex(&encrypt::<blowfish::Blowfish>(&key32()[..16], &iv(8), 48)),
        "bfc22370e264b31c864fe9ef8d8db73c\
         56eace05ec61920342adb9e06c0ff123\
         9b289fbe5b3d538c957d946898153139"
    );
}

#[test]
fn partial_final_block_matches_darc() {
    // 37 bytes: two whole blocks plus five. The C output for 37 bytes is the
    // 48-byte output truncated, which is only true if the trailing block
    // consumes keystream without padding.
    assert_eq!(
        hex(&encrypt::<aes::Aes256>(&key32(), &iv(16), 37)),
        "9308c29b3eb2abf96329a11a156e007e\
         b67119a5dec319b7ae6b21149f1a2aff\
         94cbec6396"
    );
}

#[test]
fn decrypt_inverts_encrypt() {
    // CFB is not self-inverse, unlike CTR -- the register takes ciphertext in
    // both directions, so a decrypt that mistakenly fed back plaintext would
    // still produce the right first block and then diverge.
    let key = key32();
    let iv = iv(16);
    let plain = plaintext(1000);

    let c = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();
    let mut buf = plain.clone();
    cfb::encrypt(&c, &iv, &mut buf);
    assert_ne!(buf, plain, "encryption did nothing");
    cfb::decrypt(&c, &iv, &mut buf);
    assert_eq!(buf, plain);
}

#[test]
fn decrypt_recovers_darcs_own_ciphertext() {
    // The direction that actually matters for compatibility: given bytes DArc
    // wrote, do we get the original back?
    let key = key32();
    let iv = iv(16);
    let ciphertext: Vec<u8> = (0..48)
        .map(|i| {
            let h = "9308c29b3eb2abf96329a11a156e007e\
                     b67119a5dec319b7ae6b21149f1a2aff\
                     94cbec6396213f3f2c92cce0cd73ad8c";
            u8::from_str_radix(&h[i * 2..i * 2 + 2], 16).unwrap()
        })
        .collect();

    let c = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();
    let mut buf = ciphertext;
    cfb::decrypt(&c, &iv, &mut buf);
    assert_eq!(buf, plaintext(48));
}

#[test]
fn streaming_in_chunks_matches_one_shot() {
    // Same hazard as CTR: docrypt loops over read buffers, so the feedback
    // register and the position within the keystream block must survive a
    // chunk boundary. Chunk sizes coprime with the 16-byte block, so every
    // call starts mid-block.
    let key = key32();
    let iv = iv(16);
    let plain = plaintext(1000);
    let c = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();

    let mut expected = plain.clone();
    cfb::encrypt(&c, &iv, &mut expected);

    for chunk in [1usize, 3, 7, 13, 16, 17, 64, 999] {
        let mut buf = plain.clone();
        let mut state = cfb::Cfb::new(&c, &iv);
        for part in buf.chunks_mut(chunk) {
            state.encrypt(part);
        }
        assert_eq!(buf, expected, "CFB encrypt streamed in {chunk}-byte chunks diverged");

        let mut back = buf.clone();
        let mut state = cfb::Cfb::new(&c, &iv);
        for part in back.chunks_mut(chunk) {
            state.decrypt(part);
        }
        assert_eq!(back, plain, "CFB decrypt streamed in {chunk}-byte chunks diverged");
    }
}
