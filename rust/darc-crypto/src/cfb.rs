//! CFB mode as LibTomCrypt drives it, the second of the two modes
//! `C_Encryption.cpp` offers (`mode == 1`).
//!
//! Full-block CFB with ciphertext feedback, written out for the same reason as
//! [`crate::ctr`]: the choices that distinguish one CFB from another are
//! invisible in the output until an archive fails to open.
//!
//! Read off the vendored sources:
//!
//!   * `cfb_start` finishes with `ecb_encrypt(cfb->IV, cfb->IV, ...)`, so the
//!     first keystream block is `E(IV)` and the user's IV is never used as
//!     keystream directly.
//!   * The register accumulates **ciphertext**, in both directions:
//!     `cfb_encrypt` stores the byte it just produced, `cfb_decrypt` stores the
//!     byte it just consumed. Feeding plaintext back instead would encrypt
//!     correctly and decrypt to garbage.
//!   * Re-encryption happens only when a whole block has accumulated, so this
//!     is CFB-128 (or CFB-64 for Blowfish) segmented by the block size, not
//!     CFB-8.
//!   * A trailing partial block consumes only the keystream bytes it needs.
//!
//! Unlike CTR, encryption and decryption are different operations here.

use cipher::array::Array;
use cipher::{BlockCipherEncrypt, BlockSizeUser};

/// Shared driver. `feedback_is_output` selects the direction: on encryption the
/// register takes the byte we produce, on decryption the byte we consume --
/// which is the ciphertext either way.
fn run<C>(cipher: &C, iv: &[u8], data: &mut [u8], encrypting: bool)
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    let bs = C::block_size();
    assert_eq!(
        iv.len(),
        bs,
        "CFB IV must be exactly one block ({bs} bytes for this cipher)"
    );

    // cfb_start encrypts the IV in place before any data is processed.
    let mut keystream = iv.to_vec();
    encrypt_in_place(cipher, &mut keystream);

    let mut feedback = vec![0u8; bs];
    let mut pos = 0usize;

    for byte in data.iter_mut() {
        if pos == bs {
            keystream.copy_from_slice(&feedback);
            encrypt_in_place(cipher, &mut keystream);
            pos = 0;
        }
        let cipher_byte = if encrypting {
            let c = *byte ^ keystream[pos];
            *byte = c;
            c
        } else {
            let c = *byte;
            *byte = c ^ keystream[pos];
            c
        };
        feedback[pos] = cipher_byte;
        pos += 1;
    }
}

fn encrypt_in_place<C>(cipher: &C, block: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    let b = <&mut Array<u8, C::BlockSize>>::try_from(block)
        .expect("buffer is allocated at exactly one block");
    cipher.encrypt_block(b);
}

/// Encrypt in place.
pub fn encrypt<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    run(cipher, iv, data, true)
}

/// Decrypt in place.
pub fn decrypt<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    run(cipher, iv, data, false)
}
