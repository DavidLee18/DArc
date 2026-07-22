# Wiring darc-crypto into the archiver

Design notes for replacing `Compression/_Encryption` (vendored LibTomCrypt) with
this crate. Every fact here was read off the C or measured against it, not
recalled -- a key size wrong by one byte changes the stored salt and orphans
every encrypted archive.

## The C-ABI surface encryption must keep

Two consumers, so the shim has to satisfy both.

**Haskell side** (`Compression/EncryptionLib.hs` foreign imports):

| symbol | header | role |
|---|---|---|
| `Pbkdf2Hmac(pwd,pwdlen,salt,saltlen,iters,key,keylen)` | Compression.h | KDF -> `darc_crypto::pbkdf2_hmac_sha512` |
| `fortuna_size()` | Compression.h | size of the opaque PRNG state blob |
| `fortuna_start/add_entropy/ready/read(...,prng)` | EncryptionFFI.h | PRNG |

The salt and IV are **stored in the archive** (`Encryption.hs`: `":s"++encode16
salt`) and never recomputed, so the PRNG only has to be *secure*, not
reproducible. `fortuna_read` -> `getrandom`; `start`/`add_entropy` become no-ops;
`ready` -> true; `size` -> any small constant (the blob is unused). This keeps
the Haskell untouched.

**C compression-method path** (`C_Encryption.cpp`, internal):

| symbol | role |
|---|---|
| `docrypt(enc, cipher, mode, key, keylen, rounds, iv, cb, aux)` | the streaming loop -> `darc_crypto::{ctr,cfb}` |
| `find_cipher(name)` | name -> index (table below) |
| `find_mode(name)` | ctr=0, cfb=1 |
| `cipher_descriptor[id].block_length` / `.max_key_length` | used by `parse_ENCRYPTION` for ivSize / default keySize |

## Cipher table (measured from the vendored LibTomCrypt)

Index = `register_all` order. block/maxkey drive ivSize and the default key size,
both stored, both compatibility-critical.

| name | id | block | max key | notes |
|---|---|---|---|---|
| aes | 0 | 16 | 32 | `aes_enc_desc`, encrypt-only |
| blowfish | 1 | 8 | 56 | 56-byte default verified against C; `Blowfish<BE>` |
| serpent | 2 | 16 | 32 | crate; correct Serpent (see serpent32.c re: ARM64) |
| twofish | 3 | 16 | 32 | |

## docrypt behaviour, already reproduced in this crate

- CTR: little-endian counter, whole-block width, first block = `E(IV)`,
  increment before each later block, partial final block unpadded. `ctr.rs`.
- CFB: full-block, ciphertext feedback both directions, first keystream =
  `E(IV)`, partial final block unpadded. `cfb.rs`.
- Both are STATEFUL: `docrypt` builds the cipher once and loops over
  LARGE_BUFFER_SIZE reads, so state crosses chunk boundaries. `Ctr`/`Cfb`.
- `rounds` is passed as 0 = cipher default, which is why the fixed-round crates
  fit. A non-zero rounds would need something else.

## Remaining before this links

- A CALLBACK_FUNC/Io bridge in darc-crypto (darc-codecs has `ffi::Io`; either
  share it via a small crate or duplicate the ~40 lines).
- `#[no_mangle]` exports: `darc_rs_docrypt`, `darc_rs_pbkdf2_hmac_sha512`,
  `darc_rs_random_fill`.
- A C shim compiled under `DARC_RUST` that excludes the LTC includes +
  `register_all` + the C `docrypt`, keeps `ENCRYPTION_METHOD`/`parse_ENCRYPTION`
  (with the two `cipher_descriptor[]` accesses redirected to the table above),
  and forwards to the exports.
- Follow the `#ifndef DARC_RUST` exclusion pattern from `C_Delta.cpp`.
