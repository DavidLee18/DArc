//! The cipher table, and the one place a cipher id becomes a concrete type.
//!
//! `parse_ENCRYPTION` (`C_Encryption.cpp:161`) stores the *index* of the cipher
//! in LibTomCrypt's registration order, and that index reaches the decoder
//! unchanged, so the order below is archive format rather than a local
//! convention. The block and key lengths were measured from the vendored
//! library, not recalled — `cipher_descriptor[]` in `C_Encryption.cpp:22`
//! carries the same four rows.
//!
//! Two callers need to turn an id into a type: the streaming `extern "C"`
//! entry point the C archiver drives, and the in-memory path `darc-arc` uses
//! to encrypt a whole block at once. [`with_cipher`] is the single dispatch
//! both go through, so the table cannot drift between them — including the
//! detail that only AES varies its type by key length.

use crate::{cfb, ctr};
use ::cipher::{BlockCipherEncrypt, BlockSizeUser, KeyInit};

/// One of the four ciphers DArc can name in a method string.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cipher {
    Aes,
    Blowfish,
    Serpent,
    Twofish,
}

impl Cipher {
    /// The registration-order index the archive stores.
    pub fn id(self) -> i32 {
        match self {
            Cipher::Aes => 0,
            Cipher::Blowfish => 1,
            Cipher::Serpent => 2,
            Cipher::Twofish => 3,
        }
    }

    pub fn from_id(id: i32) -> Option<Cipher> {
        match id {
            0 => Some(Cipher::Aes),
            1 => Some(Cipher::Blowfish),
            2 => Some(Cipher::Serpent),
            3 => Some(Cipher::Twofish),
            _ => None,
        }
    }

    /// `find_cipher` — an exact, case-sensitive match on the table's names.
    pub fn from_name(name: &str) -> Option<Cipher> {
        match name {
            "aes" => Some(Cipher::Aes),
            "blowfish" => Some(Cipher::Blowfish),
            "serpent" => Some(Cipher::Serpent),
            "twofish" => Some(Cipher::Twofish),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Cipher::Aes => "aes",
            Cipher::Blowfish => "blowfish",
            Cipher::Serpent => "serpent",
            Cipher::Twofish => "twofish",
        }
    }

    /// The block length in bytes, which is also `ivSize`: `parse_ENCRYPTION`
    /// sets `p->ivSize = cipher_descriptor[cipher].block_length`.
    pub fn block_length(self) -> usize {
        match self {
            Cipher::Blowfish => 8,
            Cipher::Aes | Cipher::Serpent | Cipher::Twofish => 16,
        }
    }

    /// The default key length in bytes, used when the method string names no
    /// `-NNN` size. Blowfish's 56 is the outlier and the reason a fixed
    /// 32-byte assumption would produce unreadable archives.
    pub fn max_key_length(self) -> usize {
        match self {
            Cipher::Blowfish => 56,
            Cipher::Aes | Cipher::Serpent | Cipher::Twofish => 32,
        }
    }
}

/// The chaining mode. `find_mode` (`C_Encryption.cpp:75`) knows these two and
/// nothing else, and an absent `/mode` in the method string means `ctr`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Ctr,
    Cfb,
}

impl Mode {
    pub fn id(self) -> i32 {
        match self {
            Mode::Ctr => 0,
            Mode::Cfb => 1,
        }
    }

    pub fn from_id(id: i32) -> Option<Mode> {
        match id {
            0 => Some(Mode::Ctr),
            1 => Some(Mode::Cfb),
            _ => None,
        }
    }

    pub fn from_name(name: &str) -> Option<Mode> {
        match name {
            "ctr" => Some(Mode::Ctr),
            "cfb" => Some(Mode::Cfb),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Mode::Ctr => "ctr",
            Mode::Cfb => "cfb",
        }
    }
}

/// Something to do with a cipher whose concrete type is not known yet.
///
/// This exists so [`with_cipher`] can own the id-to-type table alone. A caller
/// implements `call` generically and gets it instantiated for whichever of the
/// six concrete ciphers the id and key length select.
pub trait WithCipher {
    type Out;
    fn call<C>(self) -> Self::Out
    where
        C: BlockCipherEncrypt + BlockSizeUser + KeyInit;
}

/// Instantiate `w` for the cipher `c` uses at this key length.
///
/// `None` means AES was named with a key length it has no type for — 16, 24
/// and 32 bytes are the whole of it. The other three ciphers take a key range
/// inside one type and check the length themselves in `new_from_slice`, so
/// they always dispatch and may still fail after.
pub fn with_cipher<W: WithCipher>(c: Cipher, key_len: usize, w: W) -> Option<W::Out> {
    match c {
        Cipher::Aes => match key_len {
            16 => Some(w.call::<aes::Aes128>()),
            24 => Some(w.call::<aes::Aes192>()),
            32 => Some(w.call::<aes::Aes256>()),
            _ => None,
        },
        Cipher::Blowfish => Some(w.call::<blowfish::Blowfish>()),
        Cipher::Serpent => Some(w.call::<serpent::Serpent>()),
        Cipher::Twofish => Some(w.call::<twofish::Twofish>()),
    }
}

/// Why an in-memory encrypt or decrypt could not run.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {
    /// The key length is not one this cipher accepts.
    KeyLength(usize),
    /// The IV is not exactly one block. CTR and CFB both need a full block to
    /// start from, and a short one would otherwise panic inside the mode.
    IvLength { got: usize, want: usize },
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::KeyLength(n) => write!(f, "{n}-byte key is not valid for this cipher"),
            Error::IvLength { got, want } => {
                write!(f, "IV is {got} bytes, expected one {want}-byte block")
            }
        }
    }
}

impl std::error::Error for Error {}

/// Encrypt or decrypt `data` in place.
///
/// # Why one call over the whole buffer is the same as `docrypt`'s loop
///
/// `docrypt` reads in 256 KB chunks and carries the mode state across them.
/// 256 KB is a whole number of 8- and 16-byte blocks, so every chunk boundary
/// is also a block boundary and only the final chunk is ever partial — which
/// makes a single pass over the concatenation produce identical bytes. This is
/// only true because of that alignment; a chunk size of, say, 1000 bytes would
/// not have it, and neither would a caller that split the buffer arbitrarily
/// and called this twice.
pub fn apply_in_place(
    cipher: Cipher,
    mode: Mode,
    key: &[u8],
    iv: &[u8],
    encrypting: bool,
    data: &mut [u8],
) -> Result<(), Error> {
    let want = cipher.block_length();
    if iv.len() != want {
        return Err(Error::IvLength { got: iv.len(), want });
    }
    struct Op<'a> {
        key: &'a [u8],
        iv: &'a [u8],
        mode: Mode,
        encrypting: bool,
        data: &'a mut [u8],
    }
    impl WithCipher for Op<'_> {
        type Out = Result<(), Error>;
        fn call<C>(self) -> Self::Out
        where
            C: BlockCipherEncrypt + BlockSizeUser + KeyInit,
        {
            let key_len = self.key.len();
            let c = match C::new_from_slice(self.key) {
                Ok(c) => c,
                Err(_) => return Err(Error::KeyLength(key_len)),
            };
            match self.mode {
                Mode::Ctr => ctr::apply_keystream(&c, self.iv, self.data),
                Mode::Cfb => match self.encrypting {
                    true => cfb::encrypt(&c, self.iv, self.data),
                    false => cfb::decrypt(&c, self.iv, self.data),
                },
            }
            Ok(())
        }
    }
    let key_len = key.len();
    match with_cipher(cipher, key_len, Op { key, iv, mode, encrypting, data }) {
        Some(r) => r,
        None => Err(Error::KeyLength(key_len)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The four rows, in the order the archive stores them. A shuffled table
    /// would still encrypt and decrypt self-consistently and produce archives
    /// no other build could read, so the ids are pinned here rather than left
    /// to the enum's declaration order.
    #[test]
    fn the_cipher_ids_are_libtomcrypt_registration_order() {
        for (id, name, block, key) in [
            (0, "aes", 16, 32),
            (1, "blowfish", 8, 56),
            (2, "serpent", 16, 32),
            (3, "twofish", 16, 32),
        ] {
            let c = Cipher::from_id(id).expect("id is in the table");
            assert_eq!(c.name(), name);
            assert_eq!(c.id(), id);
            assert_eq!(Cipher::from_name(name), Some(c));
            assert_eq!(c.block_length(), block, "{name} block length");
            assert_eq!(c.max_key_length(), key, "{name} key length");
        }
        assert_eq!(Cipher::from_id(4), None);
        assert_eq!(Cipher::from_name("AES"), None, "find_cipher is case-sensitive");
    }

    #[test]
    fn the_mode_ids_match_find_mode() {
        assert_eq!(Mode::from_name("ctr"), Some(Mode::Ctr));
        assert_eq!(Mode::from_name("cfb"), Some(Mode::Cfb));
        assert_eq!(Mode::from_name("cbc"), None);
        assert_eq!(Mode::Ctr.id(), 0);
        assert_eq!(Mode::Cfb.id(), 1);
    }

    /// Every cipher and mode must round-trip, and — the part that matters —
    /// must not round-trip when the key is wrong. A mode wired to xor with a
    /// constant would pass the first half.
    #[test]
    fn every_cipher_and_mode_round_trips_and_only_with_the_right_key() {
        let plain: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();
        for c in [Cipher::Aes, Cipher::Blowfish, Cipher::Serpent, Cipher::Twofish] {
            for m in [Mode::Ctr, Mode::Cfb] {
                let key = vec![7u8; c.max_key_length()];
                let iv = vec![3u8; c.block_length()];
                let mut buf = plain.clone();
                apply_in_place(c, m, &key, &iv, true, &mut buf).expect("encrypts");
                assert_ne!(buf, plain, "{c:?}/{m:?} left the data alone");
                let mut wrong = buf.clone();
                let mut bad_key = key.clone();
                bad_key[0] ^= 1;
                apply_in_place(c, m, &bad_key, &iv, false, &mut wrong).expect("decrypts");
                assert_ne!(wrong, plain, "{c:?}/{m:?} decrypted with the wrong key");
                apply_in_place(c, m, &key, &iv, false, &mut buf).expect("decrypts");
                assert_eq!(buf, plain, "{c:?}/{m:?} did not round-trip");
            }
        }
    }

    /// A short IV must be an error rather than a panic: the IV comes from a
    /// method string in an untrusted archive, and `Ctr::new` asserts.
    #[test]
    fn a_short_iv_is_refused_rather_than_asserted() {
        let mut buf = [0u8; 32];
        let err = apply_in_place(Cipher::Aes, Mode::Ctr, &[0u8; 32], &[0u8; 8], true, &mut buf)
            .expect_err("refuses");
        assert_eq!(err, Error::IvLength { got: 8, want: 16 });
    }

    #[test]
    fn an_aes_key_length_with_no_type_is_refused() {
        let mut buf = [0u8; 16];
        let err = apply_in_place(Cipher::Aes, Mode::Ctr, &[0u8; 20], &[0u8; 16], true, &mut buf)
            .expect_err("refuses");
        assert_eq!(err, Error::KeyLength(20));
    }

    /// The claim that licenses the whole-buffer call: splitting at any 256 KB
    /// boundary and driving the streaming state gives the same bytes.
    #[test]
    fn a_whole_buffer_pass_equals_the_256kb_chunked_loop() {
        let plain: Vec<u8> = (0..600_000u32).map(|i| (i.wrapping_mul(2654435761) >> 13) as u8).collect();
        for c in [Cipher::Aes, Cipher::Blowfish] {
            let key = vec![9u8; c.max_key_length()];
            let iv = vec![5u8; c.block_length()];
            let mut whole = plain.clone();
            apply_in_place(c, Mode::Ctr, &key, &iv, true, &mut whole).expect("encrypts");

            let mut chunked = plain.clone();
            struct Chunked<'a> {
                key: &'a [u8],
                iv: &'a [u8],
                data: &'a mut [u8],
            }
            impl WithCipher for Chunked<'_> {
                type Out = ();
                fn call<C>(self)
                where
                    C: BlockCipherEncrypt + BlockSizeUser + KeyInit,
                {
                    let cc = C::new_from_slice(self.key).expect("key fits");
                    let mut st = ctr::Ctr::new(&cc, self.iv);
                    for chunk in self.data.chunks_mut(256 * 1024) {
                        st.apply(chunk);
                    }
                }
            }
            let n = key.len();
            with_cipher(c, n, Chunked { key: &key, iv: &iv, data: &mut chunked })
                .expect("dispatches");
            assert_eq!(whole, chunked, "{c:?} whole-buffer differs from the chunked loop");
        }
    }
}
