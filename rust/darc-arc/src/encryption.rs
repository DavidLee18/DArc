//! Encryption as a link in the compression chain — `Encryption.hs` and
//! `Compression/_Encryption/C_Encryption.cpp`.
//!
//! DArc does not have an "encrypted archive" flag. `-p` appends an encryption
//! *method* to whatever compression chain a block already had, so a block
//! packed with `lzma:1mb:mf=BT4` becomes
//! `lzma:1mb:mf=BT4+aes-256/ctr:n1000:r0:s…:c…:i…`, and everything that walks a
//! chain walks this one unchanged.
//!
//! ## Two strings, not one
//!
//! `generateEncryption` (`Encryption.hs:36`) returns a *pair* of chains for
//! each block:
//!
//! * the **real** chain, carrying `:k<key>:i<iv>` — used to drive the cipher
//!   and never written anywhere;
//! * the **stored** chain, carrying `:s<salt>:c<code>:i<iv>` — written into the
//!   archive, and deliberately missing the key.
//!
//! Confusing the two is the failure that matters here: storing the real chain
//! would put the key in the archive next to the ciphertext. The types below
//! keep them apart by construction — [`generate`] hands back both and the
//! caller has to choose, and [`Encryption::show_stored`] cannot emit a key.
//!
//! ## Why there is no byte-identity test for this
//!
//! The salt and the IV are freshly random per block, so two runs over the same
//! input produce different archives by design — the acceptance bar used
//! everywhere else in this port does not exist here. What replaces it is
//! *cross-decryption*: an archive this code writes must open with the reference
//! build, and one the reference writes must open with this code. See
//! `rust/difftest/arc-crypt-check.sh`.

use darc_crypto::cipher::{apply_in_place, Cipher, Mode};

/// The check code stored beside the salt: `checkCodeSize = 2`
/// (`Encryption.hs:43`). Two bytes is a 1-in-65536 chance of accepting a wrong
/// password at this stage — the CRC afterwards is what actually rejects it, and
/// this only exists to avoid decompressing a whole block to find out.
const CHECK_CODE_SIZE: usize = 2;

/// `MAXKEYSIZE*2+1` (`C_Encryption.h:4`) as `strncopy` bounds it: the field
/// holds 128 characters and a NUL, so a longer hex value in a method string is
/// truncated rather than rejected.
const MAX_HEX_LEN: usize = 128;

/// A parsed encryption method.
///
/// The four hex fields are kept as **strings, not bytes**, because the C keeps
/// them as `char[]` and `ShowCompressionMethod` prints them back verbatim. A
/// method string that carried odd-length or upper-case hex would round-trip
/// through the C unchanged, and so does this.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Encryption {
    pub cipher: Cipher,
    pub mode: Mode,
    /// Key length in bytes. `parse_ENCRYPTION` resolves an absent or zero
    /// `-NNN` to the cipher's maximum here, so this is never 0.
    pub key_size: usize,
    /// IV length in bytes = the cipher's block length.
    pub iv_size: usize,
    pub num_iterations: u32,
    pub rounds: u32,
    /// `:h1` — the key and IV are real hexadecimal.
    ///
    /// **False by default, and that default is load-bearing.** A method string
    /// read from an archive that says nothing about its hex decoding is an
    /// archive written before the parameter existed, and it has to keep being
    /// read the old way. New archives get `:h1` from the command line
    /// (`Cmdline.hs`'s `addHexFix`), never from a default here.
    pub hex_fix: bool,
    pub key: String,
    pub iv: String,
    pub salt: String,
    pub code: String,
}

/// What stopped an encrypted block being produced or read.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// The method string is not a valid encryption method.
    BadMethod(String),
    /// No password matched the stored check code. The archive is fine; the
    /// password is not — which is `BAD_PASSWORD`, not a corruption report.
    BadPassword,
    /// The cipher refused the key or IV the method string carried.
    Cipher(darc_crypto::cipher::Error),
    /// Secure random bytes were unavailable, so no salt or IV could be made.
    /// Falling back to anything weaker would silently produce an archive whose
    /// key is guessable, so this is fatal rather than degraded.
    NoEntropy,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::BadMethod(m) => write!(f, "bad name or parameters in encryption algorithm {m}"),
            Error::BadPassword => write!(f, "wrong password"),
            Error::Cipher(e) => write!(f, "{e}"),
            Error::NoEntropy => write!(f, "no secure random source available"),
        }
    }
}

impl std::error::Error for Error {}

/// `encode16` (`Utils.hs:577`) — lower-case, two digits per byte.
pub fn encode16(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push(char::from_digit(u32::from(b >> 4), 16).unwrap_or('0'));
        s.push(char::from_digit(u32::from(b & 15), 16).unwrap_or('0'));
    }
    s
}

/// `decode16` (`Utils.hs:582`) — pairs of digits, and a trailing odd character
/// is dropped rather than being an error.
///
/// A non-hex digit is `digitToInt`'s business in Haskell (it would throw) and
/// `char2int`'s in C (it produces nonsense quietly). Here it ends the decode,
/// which keeps a malformed archive from producing a key that looks plausible.
pub fn decode16(s: &str) -> Vec<u8> {
    let chars: Vec<char> = s.chars().collect();
    let mut out = Vec::with_capacity(chars.len() / 2);
    for pair in chars.chunks_exact(2) {
        let (hi, lo) = match (pair[0].to_digit(16), pair[1].to_digit(16)) {
            (Some(h), Some(l)) => (h, l),
            _ => return out,
        };
        out.push((hi * 16 + lo) as u8);
    }
    out
}

/// The **broken** decoder every archive written before the `:h1` parameter
/// existed needs — `char2int` (`Common.h:594`), now `char2int_broken`:
///
/// ```c
/// static inline int char2int(char c) {return isdigit(c)? c-'0' : tolower(c)-'a';}
/// ```
///
/// `'a'` comes out as **0**, not 10, and `'f'` as 5: the `+10` is missing. The
/// key and IV reaching the cipher were therefore not the bytes their hex named.
/// It stayed invisible because the same function ran on both the writing and
/// the reading side, so every build agreed with itself.
///
/// A nibble `v` maps to `v` below 10 and to `v - 10` above, folding 16 values
/// onto 10 — `'a'` and `'0'` produce the same byte. About 0.75 bits per nibble,
/// so roughly 208 bits of entropy in a 256-bit AES key, and the IV folded the
/// same way.
///
/// **This is kept only to read old archives.** New ones carry `:h1` and use
/// [`decode16`], which is real hexadecimal. See [`Encryption::hex_fix`].
///
/// Only the key and the IV ever went through this. The salt and the check code
/// are decoded on the *Haskell* side by `Utils.hs:582`, which was always
/// ordinary hex — which is why a build that decodes all four correctly still
/// verifies every password and then fails every CRC. The check code never
/// touches this function, so it cannot detect the mismatch.
pub fn decode16_broken(s: &str) -> Vec<u8> {
    fn char2int(c: char) -> u8 {
        match c.is_ascii_digit() {
            true => (c as u8) - b'0',
            // `tolower(c) - 'a'`, wrapping exactly as the C's int arithmetic
            // would for anything outside a-f.
            false => (c.to_ascii_lowercase() as u8).wrapping_sub(b'a'),
        }
    }
    // `for(; src[0] && src[1]; src+=2)` -- pairs, and a trailing odd character
    // is dropped.
    let chars: Vec<char> = s.chars().collect();
    chars
        .chunks_exact(2)
        .map(|p| char2int(p[0]).wrapping_mul(16).wrapping_add(char2int(p[1])))
        .collect()
}

/// `split(str, c, parts, 3)` (`Common.cpp:130`) with a three-slot result array:
/// it splits at the **first** separator only and leaves the rest whole. So
/// `aes/ctr/x` has mode `"ctr/x"`, which `find_mode` then rejects — the string
/// is refused rather than quietly read as `aes/ctr`.
fn split_once_c(s: &str, sep: char) -> (&str, Option<&str>) {
    match s.split_once(sep) {
        Some((head, tail)) => (head, Some(tail)),
        None => (s, None),
    }
}

impl Encryption {
    /// `parse_ENCRYPTION` (`C_Encryption.cpp:161`), given the name and the
    /// `':'`-separated parameters that follow it.
    ///
    /// `None` means "this is not an encryption method", which is exactly what
    /// the C returns and what makes the caller try the next parser.
    pub fn parse(name: &str, params: &[&str]) -> Option<Encryption> {
        // The C splits on '/' first, which writes a NUL over the separator, so
        // the '-' split that follows only ever sees the part before it.
        let (head, mode_part) = split_once_c(name, '/');
        let mode = match mode_part {
            None => Mode::Ctr,
            Some(m) => Mode::from_name(m)?,
        };
        let (cipher_name, key_bits) = split_once_c(head, '-');
        let cipher = Cipher::from_name(cipher_name)?;
        // Integer division, and before the zero test: `aes-4` is 4/8 = 0, which
        // then means "the default", not a half-byte key.
        let key_size = match key_bits {
            None => 0,
            Some(bits) => (crate::method::parse_int(bits)? / 8) as usize,
        };

        let mut p = Encryption {
            cipher,
            mode,
            key_size: match key_size {
                0 => cipher.max_key_length(),
                n => n,
            },
            iv_size: cipher.block_length(),
            // ENCRYPTION_METHOD's constructor, C_Encryption.cpp:93.
            num_iterations: 1000,
            rounds: 0,
            hex_fix: false,
            key: String::new(),
            iv: String::new(),
            salt: String::new(),
            code: String::new(),
        };

        for param in params {
            let mut chars = param.chars();
            let tag = chars.next()?; // an empty parameter hits `default: error=1`
            let value = chars.as_str();
            match tag {
                'k' => p.key = truncate_hex(value),
                'i' => p.iv = truncate_hex(value),
                's' => p.salt = truncate_hex(value),
                'c' => p.code = truncate_hex(value),
                'n' => p.num_iterations = crate::method::parse_int(value)?,
                'r' => p.rounds = crate::method::parse_int(value)?,
                // Any non-zero value means corrected, as `hexfix?…` in the C
                // tests it. A build without this case refuses the whole string,
                // which is the intended way for an old build to fail.
                'h' => p.hex_fix = crate::method::parse_int(value)? != 0,
                _ => return None,
            }
        }
        Some(p)
    }

    /// `ShowCompressionMethod` (`C_Encryption.cpp:143`) — the canonical form,
    /// which is what `-ae aes` is normalised to on the command line before any
    /// block is written.
    ///
    /// The optional fields print in the order `k i s c`, which is *not* the
    /// order `generateEncryption` appends them in. Both orders parse the same;
    /// only this one is what a canonicalising pass produces.
    pub fn show(&self) -> String {
        let mut s = self.algorithm_prefix();
        for (tag, value) in [("k", &self.key), ("i", &self.iv), ("s", &self.salt), ("c", &self.code)]
        {
            if !value.is_empty() {
                s.push(':');
                s.push_str(tag);
                s.push_str(value);
            }
        }
        s
    }

    /// The chain entry written **into the archive**: salt, check code and IV,
    /// and structurally no key.
    ///
    /// `generateEncryption` builds this by string concatenation onto the
    /// canonical algorithm, in the order `s c i`, and the result is stored
    /// verbatim — nothing re-canonicalises it — so the order is format.
    pub fn show_stored(&self) -> String {
        format!(
            "{}:s{}:c{}:i{}",
            self.algorithm_prefix(),
            self.salt,
            self.code,
            self.iv
        )
    }

    /// The chain entry that actually **drives the cipher**: key and IV.
    /// Never written to the archive.
    pub fn show_real(&self) -> String {
        format!("{}:k{}:i{}", self.algorithm_prefix(), self.key, self.iv)
    }

    /// The canonical algorithm with no key material — what `-ae` normalises to
    /// and what both of the above are built on.
    fn algorithm_prefix(&self) -> String {
        format!(
            "{}-{}/{}:n{}:r{}{}",
            self.cipher.name(),
            self.key_size * 8,
            self.mode.name(),
            self.num_iterations,
            self.rounds,
            match self.hex_fix {
                true => ":h1",
                false => "",
            }
        )
    }

    /// Encrypt or decrypt a block in place with the key this method carries.
    ///
    /// The key length used is `strlen(key)/2`, **not** `keySize`
    /// (`C_Encryption.cpp:129`). Those agree for anything this port writes, but
    /// an archive is free to disagree and the C would follow the hex string, so
    /// this does too.
    ///
    /// Which decoder the hex goes through is [`Encryption::hex_fix`]'s to say,
    /// and it is not a preference: an archive without `:h1` was written with
    /// the broken one and can only be read with the broken one. Read
    /// [`decode16_broken`] before touching this.
    pub fn apply(&self, data: &mut [u8], encrypting: bool) -> Result<(), Error> {
        let decode = match self.hex_fix {
            true => decode16,
            false => decode16_broken,
        };
        let key = decode(&self.key);
        let iv = decode(&self.iv);
        apply_in_place(self.cipher, self.mode, &key, &iv, encrypting, data).map_err(Error::Cipher)
    }

    /// `deriveKey` (`Encryption.hs:106`) — PBKDF2-HMAC-SHA512 over
    /// `password ++ keyfile`, split into the key and the check code.
    ///
    /// One call produces both, so the check code is not an independent hash of
    /// anything: it is simply the bytes past the key. That is why
    /// `check_code_size` is taken from the *stored* code's length on the
    /// decryption side — asking for more bytes than were stored would change
    /// nothing about the key but would compare against a longer string.
    pub fn derive_key(
        &self,
        password: &[u8],
        salt: &[u8],
        check_code_size: usize,
    ) -> (Vec<u8>, Vec<u8>) {
        let mut out = vec![0u8; self.key_size + check_code_size];
        match darc_crypto::pbkdf2_hmac_sha512(password, salt, self.num_iterations, &mut out) {
            Ok(()) => {}
            // Only for an output length the KDF refuses, which needs key_size
            // to be absurd. An all-zero key is not a silent fallback: the check
            // code derived alongside it will not match any stored one, so this
            // fails as a wrong password rather than encrypting with zeros.
            Err(()) => {}
        }
        let code = out.split_off(self.key_size);
        (out, code)
    }
}

/// `strncopy(dst, src, MAXKEYSIZE*2+1)` — a longer value is truncated, not an
/// error. Faithful because a truncated key still *decrypts something*, and a
/// port that rejected the string instead would refuse an archive the C reads.
fn truncate_hex(value: &str) -> String {
    match value.char_indices().nth(MAX_HEX_LEN) {
        Some((byte_index, _)) => value[..byte_index].to_string(),
        None => value.to_string(),
    }
}

/// `isEncryption` for a method string — see [`crate::block::is_encryption`].
/// Re-exported here so callers reasoning about encryption do not have to reach
/// into the block module for it.
pub use crate::block::is_encryption;

/// `canonizeCompressionMethod` for an encryption algorithm: `"aes"` becomes
/// `"aes-256/ctr:n1000:r0"`.
///
/// `Cmdline.hs:529` runs this over every `'+'`-joined part of `-ae` at parse
/// time, so what reaches a block is already canonical and every archive this
/// writes names the key size and mode explicitly.
pub fn canonize(algorithm: &str) -> Option<String> {
    let mut parts = algorithm.split(':');
    let name = parts.next().unwrap_or("");
    let params: Vec<&str> = parts.collect();
    Encryption::parse(name, &params).map(|e| e.show())
}

/// `addHexFix` (`Cmdline.hs`) — ask for real hexadecimal in the archive about
/// to be written, then canonicalise.
///
/// `:h1` goes immediately **after the name**, not at the end. Parameters are
/// applied left to right and the last wins, so this position leaves `-ae
/// aes:h0` — deliberately writing an old-format archive, for a build that
/// predates the parameter — in charge. Appending would override the user
/// silently, which is the whole failure mode this parameter exists to avoid.
pub fn canonize_for_writing(algorithm: &str) -> Option<String> {
    let mut parts = algorithm.split(':');
    let name = parts.next().unwrap_or("");
    let mut params: Vec<&str> = vec!["h1"];
    params.extend(parts);
    Encryption::parse(name, &params).map(|e| e.show())
}

/// `generateEncryption` (`Encryption.hs:36`) — for each algorithm in the chain,
/// draw a fresh salt and IV, derive the key, and return the real and stored
/// forms.
///
/// The salt is `keySize` bytes and the IV is `ivSize`; both come from the OS
/// entropy source per block, which is what makes two encrypted archives of the
/// same input differ.
pub fn generate(algorithms: &[String], password: &[u8]) -> Result<(Vec<String>, Vec<String>), Error> {
    let mut real = Vec::with_capacity(algorithms.len());
    let mut stored = Vec::with_capacity(algorithms.len());
    for algorithm in algorithms {
        let mut parts = algorithm.split(':');
        let name = parts.next().unwrap_or("");
        let params: Vec<&str> = parts.collect();
        let mut e = match Encryption::parse(name, &params) {
            Some(e) => e,
            None => return Err(Error::BadMethod(algorithm.clone())),
        };
        let mut iv = vec![0u8; e.iv_size];
        let mut salt = vec![0u8; e.key_size];
        darc_crypto::random::fill_secure(&mut iv).map_err(|_| Error::NoEntropy)?;
        darc_crypto::random::fill_secure(&mut salt).map_err(|_| Error::NoEntropy)?;
        let (key, code) = e.derive_key(password, &salt, CHECK_CODE_SIZE);
        e.key = encode16(&key);
        e.iv = encode16(&iv);
        e.salt = encode16(&salt);
        e.code = encode16(&code);
        real.push(e.show_real());
        stored.push(e.show_stored());
    }
    Ok((real, stored))
}

/// `generateDecryption` (`Encryption.hs:53`) — walk a chain read from an
/// archive and give every encryption link the key derived from a password that
/// verifies against its stored check code.
///
/// Non-encryption links pass through untouched. `Err(BadPassword)` means no
/// candidate matched, which is `BAD_PASSWORD` at the call site rather than a
/// corrupt-archive report — the distinction the user sees.
///
/// `passwords` is tried in order, each against every keyfile and then against
/// no keyfile at all (`keyfiles++[""]`, `Encryption.hs:73`), so a password that
/// needs no keyfile still works when keyfiles are configured.
pub fn generate_decryption(
    compressor: &[String],
    passwords: &[Vec<u8>],
    keyfiles: &[Vec<u8>],
) -> Result<Vec<String>, Error> {
    let mut out = Vec::with_capacity(compressor.len());
    for method in compressor {
        if !is_encryption(method) {
            out.push(method.clone());
            continue;
        }
        let mut parts = method.split(':');
        let name = parts.next().unwrap_or("");
        let params: Vec<&str> = parts.collect();
        let mut e = match Encryption::parse(name, &params) {
            Some(e) => e,
            None => return Err(Error::BadMethod(method.clone())),
        };
        let salt = decode16(&e.salt);
        let stored_code = decode16(&e.code);
        let key = find_key(&e, &salt, &stored_code, passwords, keyfiles)?;
        // The Haskell appends ":k<key>" to the WHOLE stored string, so the
        // resulting method still carries the salt and check code. Nothing reads
        // them again, but keeping them means a chain can be printed back and
        // still describe the block it came from.
        e.key = encode16(&key);
        out.push(format!("{}:k{}", method, e.key));
    }
    Ok(out)
}

/// `checkPwd` over every candidate: the first password whose derived check code
/// equals the stored one wins.
///
/// When the stored code is **empty** — an archive that recorded no `:c` — every
/// password "verifies" and the first is used. That is the C's behaviour and it
/// is not a hole worth closing here: the block's CRC still rejects a wrong key,
/// just later.
fn find_key(
    e: &Encryption,
    salt: &[u8],
    stored_code: &[u8],
    passwords: &[Vec<u8>],
    keyfiles: &[Vec<u8>],
) -> Result<Vec<u8>, Error> {
    for password in passwords {
        // keyfiles ++ [""] -- the empty keyfile is always tried last.
        for keyfile in keyfiles.iter().map(Vec::as_slice).chain(core::iter::once(&[][..])) {
            let mut candidate = password.clone();
            candidate.extend_from_slice(keyfile);
            let (key, code) = e.derive_key(&candidate, salt, stored_code.len());
            if code == stored_code {
                return Ok(key);
            }
        }
    }
    Err(Error::BadPassword)
}

/// The bytes a Haskell `String` password becomes on the way into the KDF.
///
/// `pbkdf2Hmac` marshals with `map (fromIntegral . ord)` into `Word8`
/// (`EncryptionLib.hs:33`), so each *character* contributes its low eight bits.
/// Under GHC the command line has already been decoded from UTF-8 into
/// characters, which means `-pé` sends **one** byte `0xe9`, not the two bytes
/// `0xc3 0xa9` the shell passed. Encoding the argument as UTF-8 here — the
/// obvious thing — would derive a different key and write archives the
/// reference build cannot open.
pub fn password_bytes(password: &str) -> Vec<u8> {
    password.chars().map(|c| (u32::from(c) & 0xff) as u8).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The canonical form the command line produces, measured from an archive
    /// the reference build wrote with `-hpSECRET`.
    #[test]
    fn the_default_algorithm_canonizes_the_way_the_reference_writes_it() {
        assert_eq!(canonize("aes").as_deref(), Some("aes-256/ctr:n1000:r0"));
    }

    #[test]
    fn every_cipher_canonizes_to_its_own_default_key_size() {
        assert_eq!(canonize("blowfish").as_deref(), Some("blowfish-448/ctr:n1000:r0"));
        assert_eq!(canonize("serpent").as_deref(), Some("serpent-256/ctr:n1000:r0"));
        assert_eq!(canonize("twofish").as_deref(), Some("twofish-256/ctr:n1000:r0"));
        assert_eq!(canonize("aes-128/cfb").as_deref(), Some("aes-128/cfb:n1000:r0"));
        assert_eq!(canonize("aes:n5000").as_deref(), Some("aes-256/ctr:n5000:r0"));
    }

    /// `keySize/8` is integer division and happens *before* the zero test, so a
    /// key size under 8 bits reads as "use the default" rather than as a tiny
    /// key. Rounding up or rejecting would both diverge.
    #[test]
    fn a_key_size_below_one_byte_falls_back_to_the_default() {
        assert_eq!(canonize("aes-4").as_deref(), Some("aes-256/ctr:n1000:r0"));
        assert_eq!(canonize("aes-0").as_deref(), Some("aes-256/ctr:n1000:r0"));
        // 12 bits is one byte after truncation, and one byte it stays.
        assert_eq!(canonize("aes-12").as_deref(), Some("aes-8/ctr:n1000:r0"));
    }

    #[test]
    fn a_second_separator_is_not_split_off() {
        // split(..., 3) leaves "ctr/x" whole, and find_mode refuses it.
        assert_eq!(canonize("aes/ctr/x"), None);
        assert_eq!(canonize("aes/cbc"), None);
        assert_eq!(canonize("rijndael"), None);
    }

    #[test]
    fn an_unknown_or_empty_parameter_refuses_the_whole_string() {
        assert_eq!(canonize("aes:z5"), None);
        assert_eq!(canonize("aes:"), None);
        assert_eq!(canonize("aes:nx"), None);
    }

    /// The exact string an archive from the reference build carries, parsed
    /// back into its parts. Lifted from `-hpSECRET` output, not constructed.
    #[test]
    fn a_stored_method_from_the_reference_parses_into_its_parts() {
        let m = "aes-256/ctr:n1000:r0:s9b22fbc808790c607d44dcfe9d97995b8f152c1451a3840b6c7e0170eae6a59e:c0925:i5f389af10cae78aebbcc21f4541dc3f5";
        let mut parts = m.split(':');
        let name = parts.next().expect("has a name");
        let params: Vec<&str> = parts.collect();
        let e = Encryption::parse(name, &params).expect("parses");
        assert_eq!(e.cipher, Cipher::Aes);
        assert_eq!(e.mode, Mode::Ctr);
        assert_eq!(e.key_size, 32);
        assert_eq!(e.iv_size, 16);
        assert_eq!(e.num_iterations, 1000);
        assert_eq!(decode16(&e.salt).len(), 32, "the salt is keySize bytes");
        assert_eq!(decode16(&e.code).len(), 2, "checkCodeSize is 2");
        assert_eq!(decode16(&e.iv).len(), 16, "the IV is one block");
        assert!(e.key.is_empty(), "an archive never stores the key");
        // And it prints back to exactly the same string, in the s/c/i order
        // generateEncryption appends them in.
        assert_eq!(e.show_stored(), m);
    }

    /// The one mistake with real consequences: the stored chain must not carry
    /// the key. Asserted against the generator's output, not against a
    /// hand-written string.
    #[test]
    fn the_stored_chain_never_contains_the_key() {
        let (real, stored) =
            generate(&["aes-256/ctr:n1000:r0".to_string()], b"hunter2").expect("generates");
        let key_hex = real[0]
            .split(":k")
            .nth(1)
            .and_then(|s| s.split(':').next())
            .expect("the real chain has a key");
        assert_eq!(key_hex.len(), 64, "a 256-bit key");
        assert!(!stored[0].contains(":k"), "stored chain has a k parameter: {}", stored[0]);
        assert!(!stored[0].contains(key_hex), "stored chain leaks the key bytes");
        assert!(stored[0].contains(":s"), "stored chain has no salt: {}", stored[0]);
        assert!(stored[0].contains(":c"), "stored chain has no check code: {}", stored[0]);
    }

    /// Two blocks encrypted with the same password must not share a salt or an
    /// IV — reusing either across blocks would be the classic CTR failure.
    #[test]
    fn each_call_draws_a_fresh_salt_and_iv() {
        let alg = vec!["aes-256/ctr:n1000:r0".to_string()];
        let (real_a, stored_a) = generate(&alg, b"pw").expect("generates");
        let (real_b, stored_b) = generate(&alg, b"pw").expect("generates");
        assert_ne!(stored_a, stored_b, "salt and IV repeated across blocks");
        assert_ne!(real_a, real_b, "key and IV repeated across blocks");
    }

    /// The round trip the whole feature rests on: what `generate` stores must
    /// let `generate_decryption` rebuild the same key, and a wrong password
    /// must be rejected rather than producing a different key silently.
    #[test]
    fn the_stored_chain_recovers_the_key_only_with_the_right_password() {
        let alg = vec!["aes-256/ctr:n1000:r0".to_string()];
        let (real, stored) = generate(&alg, b"correct horse").expect("generates");
        let chain = vec!["lzma:1mb".to_string(), stored[0].clone()];

        let keyed = generate_decryption(&chain, &[b"correct horse".to_vec()], &[])
            .expect("the right password verifies");
        assert_eq!(keyed[0], "lzma:1mb", "a non-encryption link was rewritten");
        let recovered = keyed[1].split(":k").nth(1).expect("has a key");
        let original = real[0].split(":k").nth(1).and_then(|s| s.split(':').next());
        assert_eq!(Some(recovered), original);

        let err = generate_decryption(&chain, &[b"wrong".to_vec()], &[]).expect_err("refuses");
        assert_eq!(err, Error::BadPassword);
    }

    /// Keyfiles are appended to the password, and the empty keyfile is tried
    /// last — so a password that needs no keyfile still opens the block.
    #[test]
    fn a_keyfile_is_appended_to_the_password_and_the_empty_one_is_still_tried() {
        let alg = vec!["aes-256/ctr:n1000:r0".to_string()];
        let (_, with_file) = generate(&alg, b"pwKEYFILE").expect("generates");
        let (_, without) = generate(&alg, b"pw").expect("generates");

        generate_decryption(&with_file, &[b"pw".to_vec()], &[b"KEYFILE".to_vec()])
            .expect("password + keyfile verifies");
        generate_decryption(&without, &[b"pw".to_vec()], &[b"KEYFILE".to_vec()])
            .expect("the empty keyfile is tried too");
        let err = generate_decryption(&with_file, &[b"pw".to_vec()], &[])
            .expect_err("without the keyfile it cannot verify");
        assert_eq!(err, Error::BadPassword);
    }

    /// End to end through the cipher, with the real chain on both sides.
    #[test]
    fn a_block_encrypted_with_the_real_chain_decrypts_with_the_recovered_one() {
        for algorithm in ["aes", "blowfish", "serpent", "twofish", "aes-128/cfb", "twofish/cfb"] {
            let alg = vec![canonize(algorithm).expect("canonizes")];
            let (real, stored) = generate(&alg, b"s3cret").expect("generates");
            let plain: Vec<u8> = (0..40_000u32).map(|i| (i % 253) as u8).collect();

            let mut buf = plain.clone();
            method_of(&real[0]).apply(&mut buf, true).expect("encrypts");
            assert_ne!(buf, plain, "{algorithm} left the block in the clear");

            let keyed = generate_decryption(&stored, &[b"s3cret".to_vec()], &[])
                .expect("the password verifies");
            method_of(&keyed[0]).apply(&mut buf, false).expect("decrypts");
            assert_eq!(buf, plain, "{algorithm} did not round-trip");
        }
    }

    fn method_of(s: &str) -> Encryption {
        let mut parts = s.split(':');
        let name = parts.next().unwrap_or("");
        let params: Vec<&str> = parts.collect();
        Encryption::parse(name, &params).expect("parses")
    }

    /// A password character above U+00FF contributes its low byte, because the
    /// Haskell marshals with `fromIntegral . ord` into `Word8`. UTF-8 encoding
    /// it instead would send different bytes to the KDF.
    #[test]
    fn a_password_character_contributes_its_low_byte_not_its_utf8() {
        assert_eq!(password_bytes("abc"), b"abc");
        assert_eq!(password_bytes("é"), vec![0xe9]);
        assert_eq!(password_bytes("\u{101}"), vec![0x01]);
    }

    /// The broken decoder, pinned against a ciphertext an old build produced.
    ///
    /// The vector is a real archive: `arc a -m0 -pSECRET` over 64 `'A'`s,
    /// written before `:h1` existed, whose stored method named this salt and
    /// IV. Decoding its key as hex gives a key whose check code STILL MATCHES —
    /// which is why catching this needed a ciphertext and could not come from a
    /// round trip. Without this vector there is nothing keeping the legacy read
    /// path honest, since no current build writes archives it can be tested on.
    #[test]
    fn the_legacy_decoder_reproduces_an_old_builds_ciphertext() {
        assert_eq!(decode16_broken("0a"), vec![0x00], "'a' is 0, not 10");
        assert_eq!(decode16_broken("ff"), vec![0x55], "'f' is 5, not 15");
        assert_eq!(decode16_broken("09"), vec![0x09], "digits are unchanged");
        assert_eq!(decode16_broken("FF"), vec![0x55], "tolower first");
        // …and the correct decoder disagrees, so a test exercising one of them
        // cannot silently be exercising the other.
        assert_eq!(decode16("ff"), vec![0xff]);

        // No ":h1" -- an archive from before the parameter existed.
        let e = method_of(
            "aes-256/ctr:n1000:r0\
             :kf012ca272b5efb2bbe496b21da1ee037004ff64d3a2ee911c842316cf886e145\
             :i6090b4cacecf5fb120ba94b9125db455",
        );
        assert!(!e.hex_fix, "an archive with no :h parameter is a legacy one");
        let mut buf = vec![b'A'; 64];
        e.apply(&mut buf, true).expect("encrypts");
        assert_eq!(
            encode16(&buf),
            "99743a501df4b37ccd871a8b9e55d30ee5490ef3ed946743e7ed5a375bce657d\
             2d05749e280c7b834d047942a79712fcc37867e056951ea35548159ccce3453c"
                .replace(char::is_whitespace, ""),
            "the legacy path no longer reproduces what an old build wrote"
        );

        // The same method WITH :h1 must produce different bytes -- otherwise
        // the parameter is being ignored and old archives are being read with
        // the new decoder, or the reverse.
        let fixed = method_of(
            "aes-256/ctr:n1000:r0:h1\
             :kf012ca272b5efb2bbe496b21da1ee037004ff64d3a2ee911c842316cf886e145\
             :i6090b4cacecf5fb120ba94b9125db455",
        );
        assert!(fixed.hex_fix);
        let mut fixed_buf = vec![b'A'; 64];
        fixed.apply(&mut fixed_buf, true).expect("encrypts");
        assert_ne!(fixed_buf, buf, ":h1 changed nothing, so it is being ignored");
    }

    /// The salt and the check code come off the Haskell side, which always used
    /// real hexadecimal. Decoding them with the broken function would derive a
    /// different key from the same archive — in EITHER format.
    #[test]
    fn the_salt_was_always_ordinary_hex() {
        let salt_hex = "12c17eb14283b3d7b60d28c204b304cc79a06a290c36186d49aaf58f901f1c08";
        assert_eq!(decode16(salt_hex)[0], 0x12);
        assert_eq!(decode16(salt_hex)[1], 0xc1);
        assert_ne!(decode16_broken(salt_hex)[1], 0xc1, "the two decoders must differ here");
    }

    /// `:h1` goes after the NAME, so a user's own `h` overrides it. Appending
    /// it instead would make `-ae aes:h0` silently write a corrected archive.
    #[test]
    fn the_hex_fix_is_requested_for_writing_and_can_be_overridden() {
        assert_eq!(canonize_for_writing("aes").as_deref(), Some("aes-256/ctr:n1000:r0:h1"));
        assert_eq!(
            canonize_for_writing("aes:h0").as_deref(),
            Some("aes-256/ctr:n1000:r0"),
            "an explicit h0 must win, for writing an archive an old build can read"
        );
        assert_eq!(
            canonize_for_writing("blowfish/cfb:n5000").as_deref(),
            Some("blowfish-448/cfb:n5000:r0:h1")
        );
        // Reading is unaffected: canonize does not add anything.
        assert_eq!(canonize("aes").as_deref(), Some("aes-256/ctr:n1000:r0"));
    }

    /// The generator must carry `:h1` into the archive, or every archive it
    /// writes claims the legacy decoding while using the new one.
    #[test]
    fn a_generated_chain_records_which_decoding_it_used() {
        let alg = vec![canonize_for_writing("aes").expect("canonizes")];
        let (real, stored) = generate(&alg, b"pw").expect("generates");
        assert!(stored[0].contains(":h1"), "the archive does not record :h1: {}", stored[0]);
        assert!(real[0].contains(":h1"));
        assert!(method_of(&stored[0]).hex_fix);
    }

    #[test]
    fn hex_round_trips_and_a_trailing_odd_digit_is_dropped() {
        assert_eq!(encode16(&[0x00, 0x0f, 0xa5, 0xff]), "000fa5ff");
        assert_eq!(decode16("000fa5ff"), vec![0x00, 0x0f, 0xa5, 0xff]);
        assert_eq!(decode16("abc"), vec![0xab]);
        assert_eq!(decode16("zz"), Vec::<u8>::new());
    }

    /// `strncopy` truncates at the field width instead of refusing, so a method
    /// string with an over-long key is still readable.
    #[test]
    fn an_over_long_hex_field_is_truncated_not_rejected() {
        let long = "a".repeat(200);
        let e = method_of(&format!("aes:k{long}"));
        assert_eq!(e.key.len(), MAX_HEX_LEN);
    }
}
