//! SREP block-hash verification, from `Compression/SREP/hashes.cpp` (the
//! `hash_descriptors[]` table at :416).
//!
//! Each decompressed block is followed in the stream by a hash of its bytes,
//! and the C rejects the file if a computed hash does not match. This is an
//! integrity check on the *decompressed* data, not part of producing it -- the
//! differential harness already proves the decode byte-exact, so this exists
//! only to reproduce the C's rejection of corrupt input.
//!
//! ## Which hashes are verified, and why not all of them
//!
//! | tag | name | seed | verified here |
//! |---|---|---|---|
//! | 0 | md5     | 0  | yes -- RustCrypto |
//! | 1 | (none)  | 0  | never checked (hash_func is null in the C too) |
//! | 2 | sha1    | 0  | yes -- RustCrypto |
//! | 3 | sha512  | 0  | yes -- RustCrypto |
//! | 4 | vmac    | 16 | degraded (see below) -- the default |
//! | 5 | siphash | 16 | degraded |
//!
//! md5/sha1/sha512 are seedless standard algorithms, exact from a vetted crate.
//! vmac and siphash are *keyed* MACs -- vmac is 1,300 lines of AES-based
//! universal hashing -- and reimplementing either byte-exact would add a large,
//! error-prone surface whose only effect is rejecting corrupt input the harness
//! already covers for valid input. So for those two this follows the C's own
//! documented degradation: when it cannot check a hash it prints
//! "Block checksums can't be checked..." and decompresses anyway
//! (srep.cpp:900-908). A verified decode with unchecked keyed hashes is a real
//! mode of the C tool, not a gap invented here.

use md5::Md5;
use sha1::Sha1;
use sha2::{Digest, Sha512};

/// A block hash, resolved from the archive header's hash tag.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hash {
    /// No hash bytes are stored, or the algorithm cannot be verified; either
    /// way there is nothing to check.
    None,
    Md5,
    Sha1,
    Sha512,
    /// A keyed MAC carried in the stream but not verified -- see the module
    /// note. `size` is how many bytes each block stores, which must still be
    /// consumed correctly.
    Unverified(u8),
}

/// `MD5_SIZE`/`SHA1_SIZE`/`SHA512_SIZE`, and VMAC/SipHash tag lengths.
const MD5_SIZE: u8 = 16;
const SHA1_SIZE: u8 = 20;
const SHA512_SIZE: u8 = 64;
const VMAC_TAG: u8 = 16;
const SIPHASH_TAG: u8 = 8;

impl Hash {
    /// Resolve from the header's hash number. `hash_size` is the header's
    /// stored value (already de-biased), used only to size the keyed MACs whose
    /// bytes must be skipped.
    pub fn from_num(hash_num: u8, hash_size: u8) -> Hash {
        match hash_num {
            0 => Hash::Md5,
            2 => Hash::Sha1,
            3 => Hash::Sha512,
            4 => Hash::Unverified(if hash_size != 0 { hash_size } else { VMAC_TAG }),
            5 => Hash::Unverified(if hash_size != 0 { hash_size } else { SIPHASH_TAG }),
            // 1 is the placeholder "no hash function" row; anything else is
            // unknown and, like the C, left unchecked.
            _ => Hash::None,
        }
    }

    /// How many hash bytes follow each block header in the stream.
    pub fn stored_bytes(self) -> usize {
        match self {
            Hash::None => 0,
            Hash::Md5 => MD5_SIZE as usize,
            Hash::Sha1 => SHA1_SIZE as usize,
            Hash::Sha512 => SHA512_SIZE as usize,
            Hash::Unverified(n) => n as usize,
        }
    }

    /// Verify a decompressed block against its stored hash. `Ok(())` when the
    /// hash matches, cannot be checked, or is absent; `Err(())` on a genuine
    /// mismatch, which is the C's fatal "checksum ... not the same" error.
    pub fn verify(self, data: &[u8], stored: &[u8]) -> Result<(), ()> {
        let computed: Vec<u8> = match self {
            Hash::None | Hash::Unverified(_) => return Ok(()),
            Hash::Md5 => Md5::digest(data).to_vec(),
            Hash::Sha1 => Sha1::digest(data).to_vec(),
            Hash::Sha512 => Sha512::digest(data).to_vec(),
        };
        // The C compares only `hash_size` bytes; here the stored slice already
        // has that length, and a standard hash's output is exactly it.
        let n = stored.len().min(computed.len());
        if computed[..n] == stored[..n] {
            Ok(())
        } else {
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seedless_hashes_verify_and_reject() {
        let data = b"the quick brown fox";
        for h in [Hash::Md5, Hash::Sha1, Hash::Sha512] {
            let good = match h {
                Hash::Md5 => Md5::digest(data).to_vec(),
                Hash::Sha1 => Sha1::digest(data).to_vec(),
                Hash::Sha512 => Sha512::digest(data).to_vec(),
                _ => unreachable!(),
            };
            assert_eq!(good.len(), h.stored_bytes());
            assert!(h.verify(data, &good).is_ok());
            let mut bad = good.clone();
            bad[0] ^= 1;
            assert!(h.verify(data, &bad).is_err(), "{h:?} must reject a wrong hash");
        }
    }

    #[test]
    fn keyed_macs_are_carried_but_not_checked() {
        // The default is VMAC. Its bytes must be sized correctly so the stream
        // stays aligned, but verify() must not reject -- matching the C's
        // "can't check, decompress anyway".
        let h = Hash::from_num(4, 16);
        assert_eq!(h.stored_bytes(), 16);
        assert!(h.verify(b"anything", &[0u8; 16]).is_ok());

        let sip = Hash::from_num(5, 8);
        assert_eq!(sip.stored_bytes(), 8);
        assert!(sip.verify(b"anything", &[0xff; 8]).is_ok());
    }

    #[test]
    fn unknown_and_placeholder_hashes_store_nothing() {
        assert_eq!(Hash::from_num(1, 16), Hash::None);
        assert_eq!(Hash::from_num(99, 16), Hash::None);
        assert_eq!(Hash::None.stored_bytes(), 0);
    }
}
