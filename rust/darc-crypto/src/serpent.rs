//! Serpent, ported from `Compression/_Encryption/ciphers/serpent.c`.
//!
//! This is standard Serpent: it agrees both with DArc's own algorithm compiled
//! at the intended word width and, independently, with the RustCrypto
//! `serpent` crate.
//!
//! ## The 64-bit `ulong32` bug, which this port does not reproduce
//!
//! `Compression/_Encryption/headers/tomcrypt_macros.h:13` reads:
//!
//! ```c
//! #if defined(__x86_64__) || (defined(__sparc__) && defined(__arch64__))
//!    typedef unsigned ulong32;        /* 32 bits */
//! #else
//!    typedef unsigned long ulong32;   /* 64 bits on any other LP64 target */
//! #endif
//! ```
//!
//! On ARM64 -- Apple Silicon, ARM64 Linux, the `linux-arm64` and `macos-arm64`
//! binaries DArc ships -- neither condition holds, so `ulong32` is 64 bits.
//! serpent.c's key expansion then does
//!
//! ```c
//! l_key[i + 8] = (lk << 11) | (lk >> 21);
//! ```
//!
//! which is a rotate at 32 bits and nonsense at 64, unlike the neighbouring
//! `ROL` macro, which masks. The S-boxes' `~` likewise sets the upper half.
//! The result is that an ARM64 build encrypts `-ae serpent` differently from
//! an x86-64 build, so those archives do not move between architectures.
//!
//! It goes unnoticed because `C_Encryption.cpp` defines `LTC_NO_TEST`, which
//! compiles out `serpent_test()` -- the check that fails on ARM64 and passes
//! on x86-64. This is the eleventh bug of the `long`-width family found in this
//! repository, after the ten in TTA.
//!
//! This port uses `u32` throughout, so it implements the 32-bit behaviour --
//! what x86-64 DArc has always produced. Whether to fix the C is a separate
//! decision.
//!
//! The S-boxes are generated; see [`crate::serpent_sboxes`].

use crate::serpent_sboxes::*;

/// Expanded key: 132 round-key words, plus the 8 leading words the expansion
/// consumes -- `l_key` in the C, indexed `4*r + 8` by `k_xor`.
pub struct Serpent {
    l_key: [u32; 140],
}

/// The golden-ratio constant the key expansion mixes in (`0x9e3779b9`).
const PHI: u32 = 0x9e37_79b9;

impl Serpent {
    /// Build the key schedule. Accepts keys up to 256 bits, as
    /// `serpent_setup` does; longer is rejected.
    pub fn new(key: &[u8]) -> Option<Self> {
        let key_len_bits = key.len() * 8;
        if key_len_bits > 256 {
            return None;
        }

        let mut l_key = [0u32; 140];

        // The C casts the key buffer to ulong32* and reads words directly, so
        // on a little-endian host this is a little-endian load. lk counts
        // whole and partial words alike: (bits + 31) / 32.
        let lk = (key_len_bits + 31) / 32;
        for i in 0..lk {
            let mut w = [0u8; 4];
            for (j, b) in w.iter_mut().enumerate() {
                *b = key.get(i * 4 + j).copied().unwrap_or(0);
            }
            l_key[i] = u32::from_le_bytes(w);
        }

        // Short keys are padded with a single 1 bit followed by zeros, which
        // is what the masking below does: keep the bits already present in the
        // partially-filled word, then set the next one.
        if key_len_bits < 256 {
            let i = key_len_bits / 32;
            let bit = 1u32 << (key_len_bits % 32);
            l_key[i] = (l_key[i] & (bit.wrapping_sub(1))) | bit;
        }

        for i in 0..132usize {
            let t = l_key[i] ^ l_key[i + 3] ^ l_key[i + 5] ^ l_key[i + 7] ^ PHI ^ (i as u32);
            l_key[i + 8] = t.rotate_left(11);
        }

        // The expanded words are then passed through the S-boxes in place, in
        // the order sb3, sb2, sb1, sb0, sb7, sb6, sb5, sb4, repeating.
        let mut k = Serpent { l_key };
        let order: [fn(u32, u32, u32, u32) -> (u32, u32, u32, u32); 8] =
            [sb3, sb2, sb1, sb0, sb7, sb6, sb5, sb4];
        for r in 0..33usize {
            let (a, b, c, d) = k.k_set(r);
            let (e, f, g, h) = order[r % 8](a, b, c, d);
            k.k_put(r, e, f, g, h);
        }
        Some(k)
    }

    #[inline(always)]
    fn k_set(&self, r: usize) -> (u32, u32, u32, u32) {
        let i = 4 * r + 8;
        (self.l_key[i], self.l_key[i + 1], self.l_key[i + 2], self.l_key[i + 3])
    }

    #[inline(always)]
    fn k_put(&mut self, r: usize, a: u32, b: u32, c: u32, d: u32) {
        let i = 4 * r + 8;
        self.l_key[i] = a;
        self.l_key[i + 1] = b;
        self.l_key[i + 2] = c;
        self.l_key[i + 3] = d;
    }

    #[inline(always)]
    fn k_xor(&self, r: usize, a: &mut u32, b: &mut u32, c: &mut u32, d: &mut u32) {
        let i = 4 * r + 8;
        *a ^= self.l_key[i];
        *b ^= self.l_key[i + 1];
        *c ^= self.l_key[i + 2];
        *d ^= self.l_key[i + 3];
    }

    /// Encrypt one 16-byte block in place.
    pub fn encrypt_block(&self, block: &mut [u8; 16]) {
        let (mut a, mut b, mut c, mut d) = load_le(block);

        let sboxes: [fn(u32, u32, u32, u32) -> (u32, u32, u32, u32); 8] =
            [sb0, sb1, sb2, sb3, sb4, sb5, sb6, sb7];

        // 31 rounds of "xor round key, S-box, linear transform", then a final
        // round whose linear transform is replaced by one more key xor.
        for r in 0..31usize {
            self.k_xor(r, &mut a, &mut b, &mut c, &mut d);
            let (e, f, g, h) = sboxes[r % 8](a, b, c, d);
            let (e, f, g, h) = rot(e, f, g, h);
            a = e;
            b = f;
            c = g;
            d = h;
        }
        self.k_xor(31, &mut a, &mut b, &mut c, &mut d);
        let (e, f, g, h) = sb7(a, b, c, d);
        a = e;
        b = f;
        c = g;
        d = h;
        self.k_xor(32, &mut a, &mut b, &mut c, &mut d);

        store_le(block, a, b, c, d);
    }

    /// Decrypt one 16-byte block in place.
    pub fn decrypt_block(&self, block: &mut [u8; 16]) {
        let (mut a, mut b, mut c, mut d) = load_le(block);

        let iboxes: [fn(u32, u32, u32, u32) -> (u32, u32, u32, u32); 8] =
            [ib0, ib1, ib2, ib3, ib4, ib5, ib6, ib7];

        // Mirror image: the last round first, and the inverse transform.
        self.k_xor(32, &mut a, &mut b, &mut c, &mut d);
        let (e, f, g, h) = ib7(a, b, c, d);
        a = e;
        b = f;
        c = g;
        d = h;
        for r in (0..31usize).rev() {
            self.k_xor(r + 1, &mut a, &mut b, &mut c, &mut d);
            let (e, f, g, h) = irot(a, b, c, d);
            let (e, f, g, h) = iboxes[r % 8](e, f, g, h);
            a = e;
            b = f;
            c = g;
            d = h;
        }
        self.k_xor(0, &mut a, &mut b, &mut c, &mut d);

        store_le(block, a, b, c, d);
    }
}

#[inline(always)]
fn load_le(b: &[u8; 16]) -> (u32, u32, u32, u32) {
    (
        u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
        u32::from_le_bytes([b[4], b[5], b[6], b[7]]),
        u32::from_le_bytes([b[8], b[9], b[10], b[11]]),
        u32::from_le_bytes([b[12], b[13], b[14], b[15]]),
    )
}

#[inline(always)]
fn store_le(out: &mut [u8; 16], a: u32, b: u32, c: u32, d: u32) {
    out[0..4].copy_from_slice(&a.to_le_bytes());
    out[4..8].copy_from_slice(&b.to_le_bytes());
    out[8..12].copy_from_slice(&c.to_le_bytes());
    out[12..16].copy_from_slice(&d.to_le_bytes());
}

/// The linear transform, `rot` in the C.
///
/// Note the `<<` are plain shifts, not rotations -- the bits shifted out are
/// discarded, which is what the specification calls for and what makes this
/// distinct from the `ROL` on the neighbouring lines.
#[inline(always)]
fn rot(a: u32, b: u32, c: u32, d: u32) -> (u32, u32, u32, u32) {
    let mut a = a.rotate_left(13);
    let mut b = b;
    let mut c = c.rotate_left(3);
    let mut d = d ^ c ^ (a << 3);
    b ^= a ^ c;
    d = d.rotate_left(7);
    b = b.rotate_left(1);
    a ^= b ^ d;
    c ^= d ^ (b << 7);
    a = a.rotate_left(5);
    c = c.rotate_left(22);
    (a, b, c, d)
}

/// The inverse linear transform, `irot` in the C.
#[inline(always)]
fn irot(a: u32, b: u32, c: u32, d: u32) -> (u32, u32, u32, u32) {
    let mut c = c.rotate_right(22);
    let mut a = a.rotate_right(5);
    let mut b = b;
    let mut d = d;
    c ^= d ^ (b << 7);
    a ^= b ^ d;
    d = d.rotate_right(7);
    b = b.rotate_right(1);
    d ^= c ^ (a << 3);
    b ^= a ^ c;
    c = c.rotate_right(3);
    a = a.rotate_right(13);
    (a, b, c, d)
}
