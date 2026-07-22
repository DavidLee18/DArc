# Reference vectors from DArc's own encryption code

`ctr_vectors.cpp` regenerates the vectors asserted in
`darc-crypto/tests/ctr_vectors.rs`. It `#include`s the same LibTomCrypt
sources `Compression/_Encryption/C_Encryption.cpp` includes, with the same
`LTC_*` defines, registers the same descriptors (`aes_enc_desc`, not
`aes_desc`), and calls `ctr_start`/`ctr_encrypt` with the same arguments the
archiver passes: `rounds = 0`, `CTR_COUNTER_LITTLE_ENDIAN`.

It exists so those hex strings are reproducible rather than magic numbers. A
vector nobody can regenerate is a vector nobody can check.

```sh
clang++ -std=c++17 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
  -I. -ICompression -ICompression/_Encryption -ICompression/_Encryption/headers \
  rust/cryptref/ctr_vectors.cpp -o /tmp/ctr_vectors && /tmp/ctr_vectors
```

Run from the repository root.

It also enables the LibTomCrypt self-tests, which `C_Encryption.cpp` compiles
out via `LTC_NO_TEST`. That is how the Serpent problem surfaced:

```
serpent_test()  = FAIL
twofish_test()  = PASS
blowfish_test() = PASS
```

DArc's vendored Serpent does not agree with the Crypto++-derived vectors
shipped in the same file, and `-ae serpent` is a documented option
(`Options.hs:185`). See the comment on
`serpent_cannot_be_substituted_by_the_rustcrypto_crate`.
