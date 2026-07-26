/* Reference driver for differential-testing the encryption port.
 *
 *     crypto_ref info CIPHER                              -> id/block/maxkey
 *     crypto_ref e|d  CIPHER MODE KEYHEX IVHEX  <in >out   -> docrypt
 *     crypto_ref kdf  PWD SALTHEX ITER OUTLEN      >out    -> Pbkdf2Hmac
 *
 * Built twice from the same source: once against the pinned C (vendored
 * LibTomCrypt) and once against the working tree's DARC_RUST shim, which
 * forwards to rust/darc-crypto. Both builds call docrypt/Pbkdf2Hmac by the
 * same names, so the driver itself needs no conditionals at all -- the two
 * binaries differ only in what those names resolve to.
 *
 * Encryption is the one place where a "close enough" port is worthless: an
 * archive written with -p is readable only if the key derivation and the
 * cipher stream agree to the byte, so every comparison here is byte-for-byte.
 *
 * stdout is the RESULT CODE as a 4-byte little-endian int followed by the
 * payload, and the process exits 0 either way. That makes a single `cmp` of
 * the two stdouts cover both the bytes and the outcome: a port that refuses a
 * key size the C accepts differs here just as loudly as one that encrypts it
 * differently, which a plain "exit nonzero on error" driver would have hidden
 * behind two equally empty outputs.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// C_Encryption.h declares docrypt at file scope, and C_Encryption.cpp includes
// it from inside an `extern "C" {` that spans the whole file -- so docrypt and
// Pbkdf2Hmac are defined with C linkage. The wrapper here matches that; without
// it the driver names the C++-mangled symbols and nothing links.
extern "C" {
#include "../../Compression/_Encryption/C_Encryption.h"
}

extern "C" {
int ref_find_cipher_id       (const char *name);
int ref_cipher_block_length  (int id);
int ref_cipher_max_key_length(int id);
}

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
};
static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what,"read")==0) {
    size_t avail=b->in_len-b->in_pos, n=(size_t)size<avail?(size_t)size:avail;
    memcpy(data,b->in+b->in_pos,n); b->in_pos+=n; return (int)n;
  }
  if (strcmp(what,"write")==0) {
    if (b->out_len+(size_t)size>b->out_cap) {
      size_t cap=b->out_cap?b->out_cap:65536;
      while (cap<b->out_len+(size_t)size) cap*=2;
      unsigned char *g=(unsigned char*)realloc(b->out,cap);
      if(!g) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out=g; b->out_cap=cap;
    }
    memcpy(b->out+b->out_len,data,(size_t)size); b->out_len+=(size_t)size; return size;
  }
  if (strcmp(what,"quasiwrite")==0)  return 0;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Hex is how the archive itself stores keys, IVs and salts (see decode16 in
// C_Encryption.cpp), so the harness passes them the same way rather than
// inventing an encoding the real caller never uses.
static int unhex (const char *src, unsigned char *dst, int cap) {
  int n = 0;
  for ( ; src[0] && src[1]; src += 2) {
    if (n >= cap) return -1;
    int hi = 0, lo = 0;
    if (sscanf(src, "%1x%1x", &hi, &lo) != 2) return -1;
    dst[n++] = (unsigned char)(hi*16 + lo);
  }
  return src[0] ? -1 : n;   // an odd number of digits is a harness bug, not input
}

static void emit (int rc, const unsigned char *payload, size_t len) {
  unsigned char hdr[4] = { (unsigned char)( (unsigned)rc        & 0xff),
                           (unsigned char)(((unsigned)rc >>  8) & 0xff),
                           (unsigned char)(((unsigned)rc >> 16) & 0xff),
                           (unsigned char)(((unsigned)rc >> 24) & 0xff) };
  fwrite (hdr, 1, 4, stdout);
  if (len)  fwrite (payload, 1, len, stdout);
}

int main (int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr,"usage: %s info CIPHER | e|d CIPHER MODE KEYHEX IVHEX | kdf PWD SALTHEX ITER OUTLEN\n",argv[0]);
    return 2;
  }

  if (strcmp(argv[1],"info")==0) {
    if (argc<3) return 2;
    int id = ref_find_cipher_id (argv[2]);
    if (id < 0) { printf("-1 0 0\n"); return 0; }
    printf ("%d %d %d\n", id, ref_cipher_block_length(id), ref_cipher_max_key_length(id));
    return 0;
  }

  if (strcmp(argv[1],"kdf")==0) {
    if (argc<6) return 2;
    unsigned char salt[MAXKEYSIZE]; int saltSize = unhex (argv[3], salt, sizeof salt);
    if (saltSize < 0) { fprintf(stderr,"bad salt hex\n"); return 2; }
    int iterations = atoi (argv[4]);
    int outLen     = atoi (argv[5]);
    if (outLen < 0 || outLen > MAXKEYSIZE) { fprintf(stderr,"bad out length\n"); return 2; }
    unsigned char key[MAXKEYSIZE];
    memset (key, 0, sizeof key);
    // Pbkdf2Hmac returns void on both sides, so 0 stands in for the code.
    Pbkdf2Hmac ((const BYTE*)argv[2], (int)strlen(argv[2]), salt, saltSize, iterations, key, outLen);
    emit (0, key, (size_t)outLen);
    return 0;
  }

  if ((argv[1][0]!='e' && argv[1][0]!='d') || argc < 6) {
    fprintf(stderr,"usage: %s e|d CIPHER MODE KEYHEX IVHEX\n",argv[0]); return 2; }

  int cipher = ref_find_cipher_id (argv[2]);
  if (cipher < 0) { fprintf(stderr,"unknown cipher %s\n",argv[2]); return 2; }
  int mode = strcmp(argv[3],"ctr")==0 ? 0 : strcmp(argv[3],"cfb")==0 ? 1 : -1;
  if (mode < 0) { fprintf(stderr,"unknown mode %s\n",argv[3]); return 2; }

  unsigned char key[MAXKEYSIZE]; int keysize = unhex (argv[4], key, sizeof key);
  unsigned char iv [MAXKEYSIZE]; int ivsize  = unhex (argv[5], iv,  sizeof iv);
  if (keysize < 0 || ivsize < 0) { fprintf(stderr,"bad key/iv hex\n"); return 2; }
  if (ivsize != ref_cipher_block_length(cipher)) {
    fprintf(stderr,"iv is %d bytes, cipher block is %d\n",ivsize,ref_cipher_block_length(cipher));
    return 2;
  }

  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }

  Buffers b={in,len,0,NULL,0,0};
  // rounds is always 0 in DArc (parse_ENCRYPTION's default, never overridden by
  // the archiver), which means "the cipher's own default"; the Rust side only
  // implements that case, so passing anything else would compare nothing real.
  int rc = docrypt (argv[1][0]=='e' ? ENCRYPT : DECRYPT, cipher, mode,
                    key, keysize, 0, iv, io_callback, &b);
  emit (rc, b.out, b.out_len);
  free(in); free(b.out);
  return 0;
}
