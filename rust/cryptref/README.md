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

That failure is NOT a non-standard cipher. It is `ulong32` being 64 bits:
`headers/tomcrypt_macros.h:13` selects `unsigned` only for `__x86_64__` and
sparc64, and `unsigned long` otherwise, so on ARM64 serpent.c's key expansion
rotates a 64-bit value with `(lk << 11) | (lk >> 21)` and produces garbage.
`serpent_test()` passes on x86-64.

Consequence: an ARM64 build encrypts `-ae serpent` differently from an x86-64
build, so those archives do not move between architectures. `serpent32.c` in
this directory rebuilds the algorithm at the intended width and is what
`darc-crypto/tests/serpent_vectors.rs` is checked against.
