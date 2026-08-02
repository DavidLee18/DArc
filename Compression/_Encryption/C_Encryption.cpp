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

}


/*-------------------------------------------------*/
/* LibTomCrypt encryption library initialization   */
/*-------------------------------------------------*/


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
    // 0, so a method string read from an ARCHIVE that says nothing about the
    // hex decoding gets the old one. Every archive written before ":h1" existed
    // is in exactly that position, and defaulting the other way would make all
    // of them unreadable. New archives get ":h1" from Cmdline.hs, not from here.
    hexfix        = 0;
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

// Decode a hexadecimal string into a sequence of bytes.
//
// `corrected` selects WHICH decoding, and it is not a preference:
//
//   1  real hexadecimal, what every archive written from now on uses
//   0  char2int_broken (Common.h), where 'a' is 0 and 'f' is 5 -- what every
//      archive written before the ":h1" parameter existed used
//
// Only the key and the IV pass through here. The salt and the check code are
// decoded on the Haskell side by Utils.hs's decode16, which was always real
// hex -- which is why a build that decodes all four correctly still VERIFIES
// every password and then fails every CRC. The check code never touched the
// broken decoder, so it cannot detect the mismatch.
void decode16 (char *src, BYTE *dst, int corrected)
{
    for( ; src[0] && src[1]; src+=2)
        *dst++ = corrected? hex2int(src[0]) * 16 + hex2int(src[1])
                          : char2int_broken(src[0]) * 16 + char2int_broken(src[1]);
}


// Decompression function
int ENCRYPTION_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  decode16 (key, key_bytes, hexfix);
    BYTE iv_bytes [MAXKEYSIZE];  decode16 (iv,  iv_bytes,  hexfix);
    return docrypt (DECRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int ENCRYPTION_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE key_bytes[MAXKEYSIZE];  decode16 (key, key_bytes, hexfix);
    BYTE iv_bytes [MAXKEYSIZE];  decode16 (iv,  iv_bytes,  hexfix);
    return docrypt (ENCRYPT, cipher, mode, key_bytes, strlen(key)/2, rounds, iv_bytes, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (inverse of parse_ENCRYPTION)
void ENCRYPTION_METHOD::ShowCompressionMethod (char *buf)
{
    sprintf (buf, "%s-%d/%s:n%d:r%d%s%s%s%s%s%s%s%s%s"
                                        , cipher_descriptor[cipher].name, keySize*8
                                        , EncryptionMode(mode).name()
                                        , numIterations
                                        , rounds
                                        , hexfix?":h1":""
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
        // ":h1" says the key and IV are real hexadecimal. An OLD build has no
        // case for 'h', so it hits `default: error=1` and refuses the whole
        // method string -- which is the point: it fails loudly rather than
        // decoding with the wrong table and reporting a corrupt archive.
        case 'h':  p->hexfix         = parseInt (param+1, &error);  continue;
        default :  error=1;                                         continue;
      }
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
}

static int ENCRYPTION_x = AddCompressionMethod (parse_ENCRYPTION);   // Register the ENCRYPTION method parser

