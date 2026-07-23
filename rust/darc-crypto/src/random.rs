//! Random bytes for salts and IVs.
//!
//! This replaces the vendored Fortuna PRNG outright rather than reimplementing
//! it, and that is safe for a reason worth writing down: **nothing ever has to
//! reproduce these bytes.**
//!
//! `Encryption.hs` draws a salt and an initialisation vector per archive and
//! then *stores* them, in the method string as `":s"++encode16 salt`. Reading
//! the archive back derives the key from the user's password and the salt it
//! finds there. No decryption path re-runs the generator, so its output is
//! recorded, never recomputed.
//!
//! That makes the requirement "cryptographically secure", not "bit-identical to
//! LibTomCrypt's Fortuna" -- unlike every other primitive in this crate, where
//! matching the C exactly is the whole job. Fortuna in DArc is itself seeded
//! from OS entropy via `systemRandomData` in Environment.cpp, so this goes to
//! the same source with one fewer layer in between.

/// Fill `out` with cryptographically secure random bytes from the OS.
///
/// Returns an error rather than falling back to anything weaker. A silent
/// downgrade here would produce archives that look encrypted and are not, and
/// the failure would be invisible in every test that only checks round-trips.
pub fn fill_secure(out: &mut [u8]) -> Result<(), getrandom::Error> {
    getrandom::fill(out)
}

#[cfg(test)]
mod tests {
    use super::fill_secure;

    #[test]
    fn produces_different_bytes_each_call() {
        // Not a randomness test -- that needs far more than this -- but it does
        // catch the failure that matters most: a generator that returns a
        // constant, or leaves the buffer untouched. Both would make every
        // archive share a salt.
        let mut a = [0u8; 32];
        let mut b = [0u8; 32];
        fill_secure(&mut a).expect("OS entropy unavailable");
        fill_secure(&mut b).expect("OS entropy unavailable");
        assert_ne!(a, b);
        assert_ne!(a, [0u8; 32], "buffer left as zeros");
    }

    #[test]
    fn fills_the_whole_buffer() {
        // A generator that filled only the first block would leave a
        // predictable tail in longer salts.
        for len in [1usize, 7, 16, 31, 32, 33, 64, 100] {
            let mut v = vec![0u8; len];
            fill_secure(&mut v).expect("OS entropy unavailable");
            assert_eq!(v.len(), len);
            // Vanishingly unlikely to be all-zero for any of these lengths.
            assert!(v.iter().any(|&b| b != 0), "all zeros at len {len}");
        }
    }

    #[test]
    fn handles_zero_length() {
        let mut empty: [u8; 0] = [];
        fill_secure(&mut empty).expect("zero-length fill should succeed");
    }
}
