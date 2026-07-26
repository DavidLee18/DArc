/* The encryption layer for the differential harness, compiled once per side.
 *
 * Both sides include the SAME source file, C_Encryption.cpp -- the C side from
 * the pinned tree without -DDARC_RUST (so the vendored LibTomCrypt is what
 * runs), the Rust side from the working tree, where only the DARC_RUST branch
 * survives and forwards to libdarc_crypto.a. That is deliberate: it makes the
 * harness test the production forwarding shim rather than a copy of it, so a
 * mistake in the shim's argument order shows up here rather than in an archive.
 *
 * C_Encryption.cpp registers itself as a compression method at static-init
 * time, which is the only thing it needs from outside its own translation
 * unit. Rather than link CompressionLibrary.cpp -- which would drag in every
 * codec in the tree -- the two symbols involved are stubbed below. Neither is
 * reached: the harness calls docrypt/Pbkdf2Hmac directly and never constructs
 * an ENCRYPTION_METHOD.
 */
#include "../../Compression/_Encryption/C_Encryption.cpp"

// Cipher ids are NOT a constant of the format -- they are whatever position a
// cipher ended up at in LibTomCrypt's registration table, and the Rust side
// hard-codes 0=aes 1=blowfish 2=serpent 3=twofish to match. That agreement is
// exactly the kind of thing that can silently diverge, so the driver resolves
// names through each side's own find_cipher and the harness compares the ids.
//
// `cipher_descriptor` is LibTomCrypt's ltc_cipher_descriptor[] on the C side
// and the four-entry darc_cipher_desc[] on the Rust side; the two field names
// used here are common to both, which is why this compiles unchanged for each.
extern "C" int ref_find_cipher_id (const char *name)
{
    return find_cipher (name);
}

extern "C" int ref_cipher_block_length (int id)
{
    return cipher_descriptor[id].block_length;
}

extern "C" int ref_cipher_max_key_length (int id)
{
    return cipher_descriptor[id].max_key_length;
}

// ── Stubs standing in for CompressionLibrary.cpp ────────────────────────────
int AddCompressionMethod (CM_PARSER parser)
{
    (void) parser;
    return 0;
}

int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
    (void) what; (void) param; (void) data; (void) callback;
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}
