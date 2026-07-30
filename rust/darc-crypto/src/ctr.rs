//! CTR mode as LibTomCrypt drives it, which is what DArc's archives contain.
//!
//! The mode is written out here rather than taken from the `ctr` crate. That is
//! not NIH: the details that matter are exactly the ones a generic CTR
//! implementation is free to choose differently, and getting any of them wrong
//! produces a keystream that is plausible, self-consistent, and unable to open
//! a single existing archive.
//!
//! What `ctr_start`/`ctr_encrypt` actually do, read off the vendored sources:
//!
//!   * The counter is the IV, used **as-is** for the first block.
//!     `ctr_start` finishes with `ecb_encrypt(ctr->ctr, ctr->pad, ...)`, so the
//!     first keystream block is `E(IV)`, not `E(IV+1)`. DArc passes no
//!     `LTC_CTR_RFC3686`, which is the flag that would have pre-incremented it.
//!   * The increment happens **before** generating each *subsequent* block, so
//!     the keystream is `E(IV) ‖ E(IV+1) ‖ E(IV+2) ‖ …`.
//!   * The counter is **little-endian**: the carry propagates from byte 0
//!     upward. `CTR_COUNTER_LITTLE_ENDIAN` is `0x0000`, so DArc passing plain
//!     `0` selects it. RustCrypto's idiomatic `Ctr128BE` would be wrong here.
//!   * The counter spans the **whole block**. `ctr_start` sets
//!     `ctrlen = ctr_mode & 255`, or the block length when that is zero --
//!     and DArc passes zero.
//!   * The final partial block just uses as many keystream bytes as it needs;
//!     there is no padding.
//!
//! Encryption and decryption are the same operation, as always for CTR.

use cipher::{BlockCipherEncrypt, BlockSizeUser, KeyInit};
use cipher::array::Array;

/// Streaming CTR state.
///
/// The counter and the position within the current keystream block persist
/// across calls, because `docrypt` builds the cipher once and then loops over
/// read buffers -- so a chunk boundary is not a block boundary and must not
/// restart anything. An implementation that took the IV afresh per call would
/// pass every whole-buffer test and corrupt every archive larger than one
/// read.
pub struct Ctr<'a, C> {
    cipher: &'a C,
    counter: Vec<u8>,
    pad: Vec<u8>,
    /// Bytes of `pad` already consumed. Starts at 0 with `pad` holding
    /// `E(IV)`, mirroring `ctr_start`, which pre-generates it.
    pos: usize,
    started: bool,
}

impl<'a, C> Ctr<'a, C>
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    /// `iv` must be exactly one cipher block.
    pub fn new(cipher: &'a C, iv: &[u8]) -> Self {
        let bs = C::block_size();
        assert_eq!(iv.len(), bs, "CTR IV must be exactly one block ({bs} bytes)");
        Ctr {
            cipher,
            counter: iv.to_vec(),
            pad: vec![0u8; bs],
            pos: bs, // forces the first block to be generated on first use
            started: false,
        }
    }

    /// Apply the keystream to `data` in place, continuing where the previous
    /// call left off. Encryption and decryption are the same operation.
    pub fn apply(&mut self, data: &mut [u8]) {
        let bs = C::block_size();
        for byte in data.iter_mut() {
            if self.pos == bs {
                // The first block uses the IV unchanged; every later block
                // increments first. ctr_start pre-generates E(IV) without
                // incrementing, and LTC_CTR_RFC3686 -- which would have
                // pre-incremented -- is not passed.
                if self.started {
                    increment_le(&mut self.counter);
                }
                self.started = true;
                self.pad.copy_from_slice(&self.counter);
                // Same opt-out and same reasoning as `cfb::encrypt_in_place`:
                // `pad` is `vec![0u8; C::block_size()]`, so the conversion cannot
                // fail, and the total fix is to type it as `Array<u8, C::BlockSize>`.
                #[allow(clippy::expect_used)]
                let block = <&mut Array<u8, C::BlockSize>>::try_from(&mut self.pad[..])
                    .expect("pad is allocated at exactly one block");
                self.cipher.encrypt_block(block);
                self.pos = 0;
            }
            *byte ^= self.pad[self.pos];
            self.pos += 1;
        }
    }
}

/// One-shot convenience wrapper over [`Ctr`], for callers that hold the whole
/// message at once.
pub fn apply_keystream<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    Ctr::new(cipher, iv).apply(data);
}

/// Add one to a little-endian counter spanning the whole buffer, carrying from
/// byte 0 upward and wrapping silently at the top, exactly as
/// `ctr_encrypt` does.
fn increment_le(counter: &mut [u8]) {
    for byte in counter.iter_mut() {
        *byte = byte.wrapping_add(1);
        if *byte != 0 {
            return;
        }
    }
}

/// Build a cipher from a key of a length the cipher accepts.
///
/// DArc passes `rounds = 0` to LibTomCrypt, which means "this cipher's default
/// number of rounds" -- the only reason the fixed-round RustCrypto ciphers can
/// stand in at all. A build that ever passed a non-zero round count would need
/// something other than these crates.
pub fn cipher_from_key<C: KeyInit>(key: &[u8]) -> Option<C> {
    C::new_from_slice(key).ok()
}

#[cfg(test)]
mod tests {
    use super::increment_le;

    #[test]
    fn increments_from_the_low_byte() {
        let mut c = [0u8; 4];
        increment_le(&mut c);
        assert_eq!(c, [1, 0, 0, 0], "carry must start at byte 0, not the top");
    }

    #[test]
    fn carries_upward() {
        let mut c = [0xff, 0x00, 0x00, 0x00];
        increment_le(&mut c);
        assert_eq!(c, [0x00, 0x01, 0x00, 0x00]);
    }

    #[test]
    fn wraps_silently_when_every_byte_is_ff() {
        // LibTomCrypt has no overflow handling here; it simply wraps. An
        // implementation that saturated or panicked would diverge on a stream
        // long enough to exhaust the counter.
        let mut c = [0xff, 0xff, 0xff, 0xff];
        increment_le(&mut c);
        assert_eq!(c, [0, 0, 0, 0]);
    }
}
