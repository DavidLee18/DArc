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

/// Streaming CFB state.
///
/// Like [`crate::ctr::Ctr`], the keystream block, the feedback register and the
/// position within them persist across calls, because `docrypt` builds the
/// cipher once and loops over read buffers. A chunk boundary is not a block
/// boundary.
pub struct Cfb<'a, C> {
    cipher: &'a C,
    keystream: Vec<u8>,
    feedback: Vec<u8>,
    pos: usize,
}

impl<'a, C> Cfb<'a, C>
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    /// `iv` must be exactly one cipher block.
    pub fn new(cipher: &'a C, iv: &[u8]) -> Self {
        let bs = C::block_size();
        assert_eq!(iv.len(), bs, "CFB IV must be exactly one block ({bs} bytes)");
        // cfb_start encrypts the IV in place before any data is processed.
        let mut keystream = iv.to_vec();
        encrypt_in_place(cipher, &mut keystream);
        Cfb {
            cipher,
            keystream,
            feedback: vec![0u8; bs],
            pos: 0,
        }
    }

    fn run(&mut self, data: &mut [u8], encrypting: bool) {
        let bs = C::block_size();
        for byte in data.iter_mut() {
            if self.pos == bs {
                self.keystream.copy_from_slice(&self.feedback);
                encrypt_in_place(self.cipher, &mut self.keystream);
                self.pos = 0;
            }
            // The register takes the CIPHERTEXT in both directions: the byte
            // produced when encrypting, the byte consumed when decrypting.
            let cipher_byte = if encrypting {
                let c = *byte ^ self.keystream[self.pos];
                *byte = c;
                c
            } else {
                let c = *byte;
                *byte = c ^ self.keystream[self.pos];
                c
            };
            self.feedback[self.pos] = cipher_byte;
            self.pos += 1;
        }
    }

    /// Encrypt in place, continuing from the previous call.
    pub fn encrypt(&mut self, data: &mut [u8]) {
        self.run(data, true)
    }

    /// Decrypt in place, continuing from the previous call.
    pub fn decrypt(&mut self, data: &mut [u8]) {
        self.run(data, false)
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

/// One-shot wrapper over [`Cfb`], for callers holding the whole message.
pub fn encrypt<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    Cfb::new(cipher, iv).encrypt(data)
}

/// One-shot wrapper over [`Cfb`].
pub fn decrypt<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    Cfb::new(cipher, iv).decrypt(data)
}
