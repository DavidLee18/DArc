// Declarations for the Fortuna PRNG entry points bound by EncryptionLib.hs.
//
// These exist only so that MicroHs-generated C has a prototype to call
// through; without one the calls are implicit declarations, which C89 resolves
// by assuming a return type of int and thereby truncates fortuna_read's
// unsigned long result.
//
// They deliberately live in their own header rather than in Compression.h.
// LibTomCrypt declares the same functions in headers/tomcrypt_prng.h taking
// prng_state*, so any translation unit seeing both sets gets a conflicting-type
// error. Nothing in the C sources includes this file; it is named only by the
// foreign import strings in EncryptionLib.hs, so the generated C picks it up
// and the library's own sources never do.
//
// prng_state is opaque here on purpose: the pointer is passed straight back to
// LibTomCrypt, so only its size matters, and keeping it void* avoids dragging
// the tomcrypt headers into the generated C.

#ifndef DARC_ENCRYPTION_FFI_H
#define DARC_ENCRYPTION_FFI_H

#ifdef __cplusplus
extern "C" {
#endif

int  fortuna_start       (void *prng);
int  fortuna_add_entropy (const unsigned char *in, unsigned long inlen, void *prng);
int  fortuna_ready       (void *prng);
unsigned long fortuna_read (unsigned char *out, unsigned long outlen, void *prng);

#ifdef __cplusplus
}
#endif

#endif  // DARC_ENCRYPTION_FFI_H
