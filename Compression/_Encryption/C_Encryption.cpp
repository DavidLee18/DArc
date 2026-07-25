extern "C" {
#include "C_Encryption.h"

#ifdef DARC_RUST
// ============================================================================
// Rust-backed encryption. Replaces the vendored LibTomCrypt below with calls
// into rust/darc-crypto (see rust/darc-crypto/WIRING.md). The whole file is
// already inside extern "C" (line 1), so these definitions take C linkage and
// resolve the same FFI symbols the LibTomCrypt versions did.
// ============================================================================

// Exports from libdarc_crypto.a; signatures mirror the Rust #[no_mangle] fns.
int darc_rs_docrypt (int do_encryption, int cipher, int mode,
                     const unsigned char *key, int key_len, int rounds,
                     const unsigned char *iv, CALLBACK_FUNC *callback, void *auxdata);
int darc_rs_pbkdf2_hmac_sha512 (const unsigned char *pwd, int pwd_len,
                                const unsigned char *salt, int salt_len,
                                int iterations, unsigned char *out, int out_len);
int darc_rs_random_fill (unsigned char *buf, int len);

// Cipher table; index = the LibTomCrypt registration order parse_ENCRYPTION
// still stores. Values measured from the vendored library, not recalled.
struct darc_cipher_desc { const char *name; int block_length; int max_key_length; };
static const darc_cipher_desc cipher_descriptor[4] = {
    {"aes",      16, 32},
    {"blowfish",  8, 56},
    {"serpent",  16, 32},
    {"twofish",  16, 32},
};
static int find_cipher (const char *name)
{
    for (int i = 0; i < 4; i++)
        if (strequ ((char*)name, (char*)cipher_descriptor[i].name))  return i;
    return -1;
}

// Only name() survives: ShowCompressionMethod is the sole remaining caller.
struct EncryptionMode
{
    int mode;
    EncryptionMode (int _mode) {mode = _mode;}
    char *name()
    { switch (mode) { case 0: return (char*)"ctr"; case 1: return (char*)"cfb"; default: return (char*)""; } }
};

// Fortuna's ABI, kept for the Haskell FFI but reduced to OS entropy: the salt
// is stored in the archive, so the generator must be secure, not reproducible.
// The opaque state blob is unused. 0 is CRYPT_OK.
int fortuna_size (void)                { return 64; }
int fortuna_start (void *prng)         { (void)prng; return 0; }
int fortuna_add_entropy (const unsigned char *in, unsigned long n, void *prng)
                                       { (void)in; (void)n; (void)prng; return 0; }
int fortuna_ready (void *prng)         { (void)prng; return 0; }
unsigned long fortuna_read (unsigned char *out, unsigned long outlen, void *prng)
{ (void)prng; return darc_rs_random_fill (out, (int)outlen) == 0 ? outlen : 0; }

void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize)
{ darc_rs_pbkdf2_hmac_sha512 (pwd, pwdSize, salt, saltSize, numIterations, key, keySize); }

int docrypt (enum TEncrypt DoEncryption, int cipher, int mode, BYTE *key, int keysize,
             int rounds, BYTE *iv, CALLBACK_FUNC *callback, void *auxdata)
{ return darc_rs_docrypt ((int)DoEncryption, cipher, mode, key, keysize, rounds, iv, callback, auxdata); }
#endif  // DARC_RUST

#ifndef DARC_RUST
#define LTC_NO_CIPHERS
#define   LTC_BLOWFISH
#define   LTC_RIJNDAEL
#define     ENCRYPT_ONLY
#define   LTC_TWOFISH
#define   LTC_SERPENT
#define LTC_NO_HASHES
#define   LTC_SHA1
#define   LTC_SHA512
#define LTC_NO_MATH
// LTC_NO_TEST is deliberately NOT defined. LibTomCrypt ships self-tests with
// authoritative vectors, and they would have caught the 64-bit ulong32 on the
// first ARM64 build -- serpent_test fails outright with it. They run once from
// register_all()'s static initialiser and cost microseconds. Note this whole
// block is #ifndef DARC_RUST, so only the C-crypto comparison build pays even
// that.
#include "ciphers/aes/aes.c"
#include "ciphers/blowfish.c"
#include "ciphers/twofish/twofish.c"
#include "ciphers/serpent.c"
#include "crypt/crypt_argchk.c"
#include "crypt/crypt_cipher_descriptor.c"
#include "crypt/crypt_cipher_is_valid.c"
#include "crypt/crypt_find_cipher.c"
#include "crypt/crypt_find_hash.c"
#include "crypt/crypt_find_prng.c"
#include "crypt/crypt_hash_descriptor.c"
#include "crypt/crypt_hash_is_valid.c"
#include "crypt/crypt_prng_descriptor.c"
#include "crypt/crypt_prng_is_valid.c"
#include "crypt/crypt_register_cipher.c"
#include "crypt/crypt_register_hash.c"
#include "crypt/crypt_register_prng.c"
#include "hashes/helper/hash_memory.c"
#include "hashes/sha1.c"
#include "hashes/sha2/sha512.c"
#include "mac/hmac/hmac_done.c"
#include "mac/hmac/hmac_init.c"
#include "mac/hmac/hmac_memory.c"
#include "mac/hmac/hmac_process.c"
#include "misc/error_to_string.c"
#include "misc/pkcs5/pkcs_5_2.c"
#include "misc/zeromem.c"
#include "modes/ctr/ctr_decrypt.c"
#include "modes/ctr/ctr_done.c"
#include "modes/ctr/ctr_encrypt.c"
#include "modes/ctr/ctr_start.c"
#include "modes/cfb/cfb_decrypt.c"
#include "modes/cfb/cfb_done.c"
#include "modes/cfb/cfb_encrypt.c"
#include "modes/cfb/cfb_start.c"
#include "prngs/fortuna.c"
#endif  // !DARC_RUST (vendored LibTomCrypt includes)
}


/*-------------------------------------------------*/
/* LibTomCrypt encryption library initialization   */
/*-------------------------------------------------*/

#ifndef DARC_RUST
// Register all algorithms included in the program
int register_all()
{
    register_cipher (&aes_enc_desc);
    register_cipher (&blowfish_desc);
    register_cipher (&serpent_desc);
    register_cipher (&twofish_desc);
    register_hash (&sha1_desc);
    register_hash (&sha512_desc);
#ifndef LTC_NO_TEST
    CHECK (blowfish_test()==CRYPT_OK, (s,"blowfish_test failed!"));
//    CHECK (rijndael_test()==CRYPT_OK, (s,"rijndael_test failed!"));
    CHECK (serpent_test ()==CRYPT_OK, (s,"serpent_test failed!"));
    CHECK (twofish_test ()==CRYPT_OK, (s,"twofish_test failed!"));
    CHECK (sha1_test    ()==CRYPT_OK, (s,"sha1_test failed!"));
    CHECK (sha512_test  ()==CRYPT_OK, (s,"sha512_test failed!"));
//    CHECK (hmac_test    ()==CRYPT_OK, (s,"hmac_test failed!"));
//    CHECK (ctr_test     ()==CRYPT_OK, (s,"ctr_test failed!"));
//    CHECK (cfb_test     ()==CRYPT_OK, (s,"cfb_test failed!"));
#endif
    return 0;
}
int call_register_all = register_all();

// Size of the Fortuna PRNG buffer
int fortuna_size (void)
{
    return sizeof(prng_state);
}


/*------------------------------------------------------*/
/* Generic interface to encryption modes (CFB, CTR)     */
/*------------------------------------------------------*/

struct EncryptionMode
{
    int mode;
    symmetric_CTR ctr;
    symmetric_CFB cfb;

    EncryptionMode (int _mode) {mode = _mode;}

    char *name()
    {
        switch (mode) {
        case 0: return "ctr";
        case 1: return "cfb";
        default: return "";
        }
    }

    int start (int cipher, BYTE *iv, BYTE *key, int keysize, int rounds)
    {
        switch (mode) {
        case 0: return ctr_start (cipher, iv, key, keysize, rounds, CTR_COUNTER_LITTLE_ENDIAN, &ctr);
        case 1: return cfb_start (cipher, iv, key, keysize, rounds, &cfb);
        default: return CRYPT_ERROR;
        }
    }

    int encrypt (BYTE *pt, BYTE *ct, int len)
    {
        switch (mode) {
        case 0: return ctr_encrypt(pt, ct, len, &ctr);
        case 1: return cfb_encrypt(pt, ct, len, &cfb);
        default: return CRYPT_ERROR;
        }
    }

    int decrypt (BYTE *pt, BYTE *ct, int len)
    {
        switch (mode) {
        case 0: return ctr_decrypt(pt, ct, len, &ctr);
        case 1: return cfb_decrypt(pt, ct, len, &cfb);
        default: return CRYPT_ERROR;
        }
    }

    int done()
    {
        switch (mode) {
        case 0: return ctr_done (&ctr);
        case 1: return cfb_done (&cfb);
        default: return CRYPT_ERROR;
        }
    }
};

#endif  // !DARC_RUST (register_all, fortuna_size, EncryptionMode)

// Find the encryption mode number by its name
int find_mode (char *name)
{
    if (strequ(name,"ctr"))  return  0;
    if (strequ(name,"cfb"))  return  1;
    else                     return -1;
}


/*-------------------------------------------------*/
/* User-facing functions                           */
/*-------------------------------------------------*/

#ifndef DARC_RUST
// Generate a key from the password and salt using numIterations hashing iterations (PKCS5#2)
void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize)
{
    int hash = find_hash("sha512");
    unsigned long ulKeySize = keySize;
    pkcs_5_alg2 (pwd, pwdSize, salt, saltSize, numIterations, hash, key, &ulKeySize);
}

// Encrypts or decrypts the data stream, depending on the value of DoEncryption
int docrypt (enum TEncrypt DoEncryption, int cipher, int mode, BYTE *key, int keysize, int rounds, BYTE *iv,
             CALLBACK_FUNC *callback, void *auxdata)
{
    EncryptionMode encryptor(mode);
    encryptor.start (cipher, iv, key, keysize, rounds);

    int InSize = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;  // number of bytes read or an error code
    int RemainderSize = 0;                           // unprocessed remainder of the previous block (always 0 in the current implementation)
    BYTE* Buf = (BYTE*)malloc(LARGE_BUFFER_SIZE);    // storage for the data
    if (!Buf)   goto Exit;                           // exit when out of memory

    while ( (InSize = callback ("read", Buf+RemainderSize, LARGE_BUFFER_SIZE-RemainderSize, auxdata)) >= 0 )  // exit on a read error
    {
        if ((InSize+=RemainderSize)==0)     break;  // exit if there is no more data

        DoEncryption==ENCRYPT
          ? encryptor.encrypt(Buf, Buf, InSize)
          : encryptor.decrypt(Buf, Buf, InSize);

        int OutSize = InSize, x;
        if( (x=callback("write",Buf,OutSize,auxdata))<0 )   {InSize=x; break;}  // exit on a write error
        RemainderSize = InSize-OutSize;
        // Move the unprocessed remainder of the data to the beginning of the buffer
        if (RemainderSize>0)                memmove (Buf, Buf+OutSize, RemainderSize);
    }
Exit:
    encryptor.done();
    free (Buf);
    return InSize;  // return the error code, or 0 if everything is fine
}


#endif  // !DARC_RUST (Pbkdf2Hmac, docrypt)

/*-------------------------------------------------*/
/* ENCRYPTION_METHOD class implementation          */
/*-------------------------------------------------*/

// Constructor assigning default values to the compression method parameters
ENCRYPTION_METHOD::ENCRYPTION_METHOD()
{
    cipher        = -1;
    mode          = -1;
    numIterations = 1000;
    rounds        = 0;
    keySize       = -1;
    strcpy(key,  "");
    strcpy(iv,   "");
    strcpy(salt, "");
    strcpy(code, "");
}

// Universal method, answers the "encryption?", "KeySize" and "IVSize" queries
int ENCRYPTION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
         if (strequ (what, "encryption?"))    return 1;               // Yes, this is an encryption algorithm
    else if (strequ (what, "keySize"))        return keySize;         // Returns the size of the key used by this compression method
    else if (strequ (what, "ivSize"))         return ivSize;          // Returns the size of the InitVector used by this compression method
    else if (strequ (what, "numIterations"))  return numIterations;   // Returns the number of iterations used when generating the key from password+salt
    else                                      return COMPRESSION_METHOD::doit (what, param, data, callback);  // Pass the remaining calls to the parent procedure
}

// Decode a hexadecimal string into a sequence of bytes
void decode16 (char *src, BYTE *dst)
{
    for( ; src[0] && src[1]; src+=2)
        *dst++ = char2int(src[0]) * 16 + char2int(src[1]);
}


// Decompression function
int ENCRYPTION_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  decode16 (key, key_bytes);
    BYTE iv_bytes [MAXKEYSIZE];  decode16 (iv,  iv_bytes);
    return docrypt (DECRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int ENCRYPTION_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  decode16 (key, key_bytes);
    BYTE iv_bytes [MAXKEYSIZE];  decode16 (iv,  iv_bytes);
    return docrypt (ENCRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (inverse of parse_ENCRYPTION)
void ENCRYPTION_METHOD::ShowCompressionMethod (char *buf)
{
    sprintf (buf, "%s-%d/%s:n%d:r%d%s%s%s%s%s%s%s%s"
                                        , cipher_descriptor[cipher].name, keySize*8
                                        , EncryptionMode(mode).name()
                                        , numIterations
                                        , rounds
                                        , *key ?":k":"", key
                                        , *iv  ?":i":"", iv
                                        , *salt?":s":"", salt
                                        , *code?":c":"", code
                                        );
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an ENCRYPTION_METHOD object with the given compression parameters
// or returns NULL if this is a different compression method or there is an error in the parameters
COMPRESSION_METHOD* parse_ENCRYPTION (char** parameters)
{
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Make a local copy, since split destroys the string
    char local_method[MAX_METHOD_STRLEN];
    strncopy (local_method, parameters[0], MAX_METHOD_STRLEN);

    // Split the method string into at most 2 parts separated by '/'
    // These are the cipher and the encryption mode (for example, "aes/cfb")
    char *parts[3];
    split (local_method, '/', parts, 3);
    int mode    = parts[1]? find_mode(parts[1]) : 0;

    // Split the method string into at most 2 parts separated by '-'
    // After the '-' the key size in bits may be specified (for example, "aes-128")
    split (local_method, '-', parts, 3);

    int cipher  = find_cipher(parts[0]);
    int keySize = parts[1]? parseInt (parts[1], &error)/8 : 0;
    if (mode<0 || cipher<0 || error)   return NULL;   // This is not an ENCRYPTION method

    ENCRYPTION_METHOD *p = new ENCRYPTION_METHOD;
    p->cipher  = cipher;
    p->mode    = mode;
    p->keySize = keySize? keySize : cipher_descriptor[cipher].max_key_length;
    p->ivSize  = cipher_descriptor[cipher].block_length;

    // Iterate over all method parameters (or exit early if an error occurs while parsing one of them)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Parameters carrying values
        case 'k':  strncopy (p->key,  param+1, sizeof (p->key));    continue;
        case 'i':  strncopy (p->iv,   param+1, sizeof (p->iv));     continue;
        case 's':  strncopy (p->salt, param+1, sizeof (p->salt));   continue;
        case 'c':  strncopy (p->code, param+1, sizeof (p->code));   continue;
        case 'n':  p->numIterations  = parseInt (param+1, &error);  continue;
        case 'r':  p->rounds         = parseInt (param+1, &error);  continue;
        default :  error=1;                                         continue;
      }
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
}

static int ENCRYPTION_x = AddCompressionMethod (parse_ENCRYPTION);   // Register the ENCRYPTION method parser

