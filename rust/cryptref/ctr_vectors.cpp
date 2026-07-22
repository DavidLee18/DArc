// Reference vectors straight out of DArc's own encryption code.
//
// Includes exactly the LibTomCrypt sources C_Encryption.cpp includes, with the
// same LTC_* defines, then calls ctr_start/ctr_encrypt with exactly the
// arguments it passes (rounds=0, CTR_COUNTER_LITTLE_ENDIAN), and registers the
// same descriptors -- aes_enc_desc, not aes_desc. Whatever this prints is what
// every -p archive is built from.
#include <stdio.h>
#include <string.h>
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

static void dump(const char* name, const char* cipher, const unsigned char* key, int keylen,
                 const unsigned char* iv, int nbytes)
{
    unsigned char pt[256], ct[256];
    memset(pt, 0, sizeof pt);            // encrypting zeros yields the raw keystream
    symmetric_CTR ctr;
    int idx = find_cipher(cipher);
    if (idx < 0) { printf("%s ERROR cipher-not-registered\n", name); return; }
    int err = ctr_start(idx, iv, key, keylen, 0, CTR_COUNTER_LITTLE_ENDIAN, &ctr);
    if (err != CRYPT_OK) { printf("%s ERROR ctr_start=%d\n", name, err); return; }
    err = ctr_encrypt(pt, ct, nbytes, &ctr);
    if (err != CRYPT_OK) { printf("%s ERROR ctr_encrypt=%d\n", name, err); return; }
    printf("%s ", name);
    for (int i = 0; i < nbytes; i++) printf("%02x", ct[i]);
    printf("\n");
}

int main()
{
    register_cipher(&aes_enc_desc);
    register_cipher(&blowfish_desc);
    register_cipher(&serpent_desc);
    register_cipher(&twofish_desc);

    printf("serpent_test()  = %s\n", serpent_test()  == CRYPT_OK ? "PASS" : "FAIL");
    printf("twofish_test()  = %s\n", twofish_test()  == CRYPT_OK ? "PASS" : "FAIL");
    printf("blowfish_test() = %s\n", blowfish_test() == CRYPT_OK ? "PASS" : "FAIL");

    unsigned char key32[32], iv16[16], iv8[8];
    for (int i = 0; i < 32; i++) key32[i] = (unsigned char)i;
    for (int i = 0; i < 16; i++) iv16[i] = (unsigned char)(0xf0 + i);
    for (int i = 0;  i < 8; i++) iv8[i]  = (unsigned char)(0xf0 + i);

    dump("aes128",   "aes",      key32, 16, iv16, 48);
    dump("aes256",   "aes",      key32, 32, iv16, 48);
    dump("twofish",  "twofish",  key32, 32, iv16, 48);
    dump("serpent",  "serpent",  key32, 32, iv16, 48);
    dump("blowfish", "blowfish", key32, 16, iv8,  48);

    unsigned char ivc[16];
    memset(ivc, 0xff, 16); ivc[15] = 0x00;   // low bytes all 0xff -> carry propagates
    dump("aes128carry", "aes", key32, 16, ivc, 48);
    return 0;
}
