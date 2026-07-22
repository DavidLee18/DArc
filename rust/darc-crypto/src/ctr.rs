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

/// Apply DArc's CTR keystream to `data` in place.
///
/// `iv` must be one cipher block long; anything else is a caller bug rather
/// than a data-dependent condition, so it panics rather than returning an
/// error that would have to be threaded through every call site.
pub fn apply_keystream<C>(cipher: &C, iv: &[u8], data: &mut [u8])
where
    C: BlockCipherEncrypt + BlockSizeUser,
{
    let bs = C::block_size();
    assert_eq!(
        iv.len(),
        bs,
        "CTR IV must be exactly one block ({bs} bytes for this cipher)"
    );

    let mut counter = iv.to_vec();
    let mut pad = vec![0u8; bs];

    // First block uses the IV unchanged -- see the note above about ctr_start
    // pre-generating the pad.
    let mut first = true;
    for chunk in data.chunks_mut(bs) {
        if !first {
            increment_le(&mut counter);
        }
        first = false;

        pad.copy_from_slice(&counter);
        let block = <&mut Array<u8, C::BlockSize>>::try_from(&mut pad[..])
            .expect("pad is allocated at exactly one block");
        cipher.encrypt_block(block);

        for (b, k) in chunk.iter_mut().zip(pad.iter()) {
            *b ^= *k;
        }
    }
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
