/* Reference driver for differential-testing the BSC LZP ENCODER.
 *
 *     bsc_lzp_enc_ref HASH MINLEN  <in >out
 *
 * Built twice from the same source: plain it drives the C `bsc_lzp_compress`,
 * and under -DUSE_RUST the Rust `darc_rs_bsc_lzp_compress`. Byte-for-byte
 * equality of the two outputs is the bar -- LZP produces the bytes the block
 * sorter and the entropy coder then consume, so a "valid but different"
 * encoding would silently change every -mbsc archive.
 *
 * stdout is the result code as a 4-byte little-endian int followed by the
 * payload, and the process exits 0 either way. A single `cmp` of the two
 * stdouts therefore covers the outcome as well as the bytes: one side
 * declaring an input incompressible while the other encodes it differs here
 * just as loudly as a wrong byte, which a driver that wrote nothing on failure
 * would have hidden behind two empty files.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_init (int features);
int darc_bsc_lzp_compress (const unsigned char *in, unsigned char *out, int n, int hashSize, int minLen, int features);
#ifdef USE_RUST
int darc_rs_bsc_lzp_compress (const unsigned char *in, int inSize, unsigned char *out, int outCap, int hashSize, int minLen);
#endif
}

static unsigned char *slurp (size_t *outLen) {
  size_t cap = 1 << 20, len = 0;
  unsigned char *b = (unsigned char *)malloc(cap);
  if (!b) exit(3);
  for (;;) {
    if (len == cap) { cap *= 2; b = (unsigned char *)realloc(b, cap); if (!b) exit(3); }
    size_t n = fread(b + len, 1, cap - len, stdin);
    if (n == 0) break;
    len += n;
  }
  *outLen = len;
  return b;
}

static void emit (int rc, const unsigned char *payload, size_t len) {
  unsigned char hdr[4] = { (unsigned char)( (unsigned)rc        & 0xff),
                           (unsigned char)(((unsigned)rc >>  8) & 0xff),
                           (unsigned char)(((unsigned)rc >> 16) & 0xff),
                           (unsigned char)(((unsigned)rc >> 24) & 0xff) };
  fwrite (hdr, 1, 4, stdout);
  if (len) fwrite (payload, 1, len, stdout);
}

int main (int argc, char **argv) {
  if (argc < 3) { fprintf(stderr, "usage: %s HASH MINLEN\n", argv[0]); return 2; }
  int hashSize = atoi(argv[1]);
  int minLen   = atoi(argv[2]);

  darc_bsc_init(0);

  size_t len = 0;
  unsigned char *in = slurp(&len);
  int n = (int)len;

  /* bsc_lzp_compress_serial writes into a buffer of exactly n bytes and treats
   * running out as "not compressible", so the sizes must match on both sides or
   * the comparison is between two different problems. */
  unsigned char *out = (unsigned char *)malloc(n > 0 ? n : 1);
  if (!out) { free(in); return 3; }

#ifdef USE_RUST
  int rc = darc_rs_bsc_lzp_compress (in, n, out, n, hashSize, minLen);
#else
  int rc = darc_bsc_lzp_compress (in, out, n, hashSize, minLen, 0);
#endif

  emit (rc, out, rc > 0 ? (size_t)rc : 0);
  free(in); free(out);
  return 0;
}
