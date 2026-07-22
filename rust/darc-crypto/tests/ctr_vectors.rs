//! CTR keystream against vectors produced by DArc's own encryption code.
//!
//! These are not from a specification and not from this crate. They were dumped
//! by a harness that `#include`s the same LibTomCrypt sources
//! `Compression/_Encryption/C_Encryption.cpp` includes, with the same `LTC_*`
//! defines, registering the same descriptors (`aes_enc_desc`, not `aes_desc`)
//! and calling `ctr_start`/`ctr_encrypt` with the same arguments the archiver
//! passes: `rounds = 0`, `CTR_COUNTER_LITTLE_ENDIAN`.
//!
//! That distinction matters. A standards-derived vector would prove this crate
//! implements textbook CTR; only DArc's own output proves it implements the CTR
//! that every existing `-p` archive was encrypted with. Those are different
//! claims, and the second is the one that decides whether old archives open.
//!
//! Each vector is the keystream itself: the C harness encrypts 48 zero bytes,
//! so the ciphertext *is* `E(IV) ‖ E(IV+1) ‖ E(IV+2)`. Three blocks, so the
//! counter increments twice and the little-endian carry is actually exercised
//! rather than merely present.

use darc_crypto::ctr::apply_keystream;

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

/// Key bytes 0x00..0x1f and IV bytes 0xf0.., matching the C harness.
fn key32() -> Vec<u8> {
    (0u8..32).collect()
}
fn iv(len: usize) -> Vec<u8> {
    (0..len).map(|i| 0xf0u8.wrapping_add(i as u8)).collect()
}

/// Encrypt 48 zero bytes, which yields the raw keystream.
fn keystream<C>(key: &[u8], iv: &[u8]) -> String
where
    C: cipher::BlockCipherEncrypt + cipher::BlockSizeUser + cipher::KeyInit,
{
    let cipher = <C as cipher::KeyInit>::new_from_slice(key).expect("key length rejected by cipher");
    let mut data = vec![0u8; 48];
    apply_keystream(&cipher, iv, &mut data);
    hex(&data)
}

#[test]
fn aes128_matches_darc() {
    assert_eq!(
        keystream::<aes::Aes128>(&key32()[..16], &iv(16)),
        "66a7c7e8345231489751de073316adad\
         5e279680aeeb4b90eace69f68766015e\
         046d5136ca2c0a413b8f83ca60233243"
    );
}

#[test]
fn aes256_matches_darc() {
    assert_eq!(
        keystream::<aes::Aes256>(&key32(), &iv(16)),
        "9200cd8d239680cb5a69e65440326314\
         f043a8612107c7bb43c777759c642869\
         e44100fb153133ad08dd0c0e97154009"
    );
}

#[test]
fn twofish_matches_darc() {
    assert_eq!(
        keystream::<twofish::Twofish>(&key32(), &iv(16)),
        "7aab398567ef74f73945f0663768c404\
         950ac57283f6ca017a74a6a9282341cf\
         ff112497d2f9fd8fad73ad37bbd6fd29"
    );
}

/// DArc's Serpent is NOT standard Serpent, and the `serpent` crate cannot
/// stand in for it. This asserts the divergence rather than hiding it, so that
/// a future crate update which happened to converge would fail here and force
/// someone to look, instead of silently making `-ae serpent` archives
/// unreadable.
///
/// The evidence, in order:
///
///   * The keystreams differ, and no byte-order convention reconciles them --
///     key and IV reversed by bytes, by words, and word-swapped, with the
///     output likewise transformed, were all tried and none matched. So this
///     is a different cipher, not a different calling convention.
///   * DArc's own bundled self-test agrees. `serpent_test()`, whose vectors
///     the file says come from Crypto++, FAILS, while `twofish_test()` and
///     `blowfish_test()` in the same build pass. That test never runs in
///     production because C_Encryption.cpp defines LTC_NO_TEST.
///
/// So the vendored implementation -- Gladman's, by way of libmcrypt -- does
/// not agree with published Serpent vectors, and `-ae serpent` is documented
/// and selectable at Options.hs:185. Archives written with it round-trip
/// through DArc, since the same code encrypts and decrypts, but they are not
/// Serpent as specified and no other tool would read them.
///
/// Substituting the crate is therefore off the table until the vendored
/// implementation is understood: it would render every existing Serpent
/// archive unopenable. Tracked separately from this port.
#[test]
fn serpent_cannot_be_substituted_by_the_rustcrypto_crate() {
    let darc = "1e0a349f3de9990b2a848e2d8a7f3c93\
                f5b7a1b58a4a771178ae03200aec0bd1\
                3c1796e652e1a02d9e0cc2a6f9f8b5a6";
    assert_ne!(
        keystream::<serpent::Serpent>(&key32(), &iv(16)),
        darc,
        "the serpent crate now agrees with DArc's vendored Serpent -- \
         re-check which of the two changed before adopting it"
    );
}

#[test]
fn blowfish_matches_darc() {
    // 64-bit block, so 48 bytes is six blocks and the counter increments five
    // times. Also the one cipher here whose block is not 16 bytes, which is
    // where a hardcoded 128-bit counter width would show up.
    assert_eq!(
        keystream::<blowfish::Blowfish>(&key32()[..16], &iv(8)),
        "beca2c66ff40982e6fe2812f53fe7526\
         d854d6f36b2d08b07b7b77418a404528\
         2a868d0bbec1c300653705e3778d3eba"
    );
}

#[test]
fn carry_propagates_across_the_whole_counter() {
    // IV = ff ff ... ff 00. Incrementing carries through fifteen bytes, so an
    // implementation that only ever touched the low byte, or that treated the
    // counter as big-endian, diverges from the second block onward while
    // producing an identical first block.
    let mut iv = vec![0xffu8; 16];
    iv[15] = 0x00;
    assert_eq!(
        keystream::<aes::Aes128>(&key32()[..16], &iv),
        "90dcf07a729615b8537aaafb90b8ec83\
         7346139595c0b41e497bbde365f42d0a\
         7fe6e7fa6b07ff190da174c7d7c9f362"
    );
}

#[test]
fn ctr_is_its_own_inverse() {
    let key = key32();
    let iv = iv(16);
    let plain: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();

    let cipher = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();
    let mut buf = plain.clone();
    apply_keystream(&cipher, &iv, &mut buf);
    assert_ne!(buf, plain, "keystream did nothing");
    apply_keystream(&cipher, &iv, &mut buf);
    assert_eq!(buf, plain);
}

#[test]
fn partial_final_block_is_not_padded() {
    // A length that is not a multiple of the block size must consume only as
    // many keystream bytes as it needs. If it padded, the prefix of a longer
    // encryption would stop matching a shorter one.
    let key = key32();
    let iv = iv(16);
    let cipher = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();

    let mut long = vec![0u8; 48];
    apply_keystream(&cipher, &iv, &mut long);

    for len in [1usize, 7, 15, 16, 17, 31, 33, 47] {
        let mut short = vec![0u8; len];
        apply_keystream(&cipher, &iv, &mut short);
        assert_eq!(
            short,
            long[..len],
            "keystream for {len} bytes is not a prefix of the 48-byte one"
        );
    }
}

#[test]
fn streaming_in_chunks_matches_one_shot() {
    // docrypt builds the cipher once and loops over read buffers, so a chunk
    // boundary is not a block boundary. State must carry across calls.
    //
    // The chunk sizes below are deliberately coprime with the 16-byte block --
    // 1, 3, 7, 13 -- so every call starts mid-block. An implementation that
    // restarted from the IV per call, or that only handled block-aligned
    // chunks, passes the one-shot tests above and fails here.
    let key = key32();
    let iv = iv(16);
    let plain: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();
    let cipher = <aes::Aes256 as cipher::KeyInit>::new_from_slice(&key).unwrap();

    let mut expected = plain.clone();
    darc_crypto::ctr::apply_keystream(&cipher, &iv, &mut expected);

    for chunk in [1usize, 3, 7, 13, 16, 17, 64, 999] {
        let mut buf = plain.clone();
        let mut state = darc_crypto::ctr::Ctr::new(&cipher, &iv);
        for part in buf.chunks_mut(chunk) {
            state.apply(part);
        }
        assert_eq!(buf, expected, "CTR streamed in {chunk}-byte chunks diverged");
    }
}
