/* Reference driver for differential-testing the whole BSC codec (block level).
 *
 *     bsc_full_ref e SORTER CODER HASH MINLEN  <in  >enc
 *     bsc_full_ref d SIZE                      <enc >out   (C, or Rust under -DUSE_RUST)
 *
 * `e` builds one real framed block with bsc_compress and writes:
 *
 *     int32  n            (original size, == the decompress outputSize)
 *     uint8  block[...]   (28-byte libbsc header + payload)
 *
 * SORTER: 1 = BWT, 3..8 = ST3..ST8.  CODER: 1 = static, 2 = adaptive.
 * HASH/MINLEN: LZP parameters, or 0 0 to disable LZP.
 *
 * bsc_compress may fall back to a stored block (mode = 0) when the input does
 * not compress; the dispatcher handles that, so it is not a skip.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_init (int features);
int darc_bsc_compress (const unsigned char *in, unsigned char *out, int n, int lzpHashSize, int lzpMinLen, int blockSorter, int coder, int features);
int darc_bsc_decompress (const unsigned char *in, int inSize, unsigned char *out, int outSize, int features);
#ifdef USE_RUST
int darc_rs_bsc_decompress_block (const unsigned char *in, int inSize, unsigned char *out, int outCap);
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

int main (int argc, char **argv) {
  if (argc < 2 || (argv[1][0] != 'e' && argv[1][0] != 'd')) {
    fprintf(stderr, "usage: %s e SORTER CODER HASH MINLEN | d SIZE\n", argv[0]); return 2;
  }
  darc_bsc_init(0);

  size_t len = 0;
  unsigned char *in = slurp(&len);

  if (argv[1][0] == 'e') {
    int n      = (int)len;
    int sorter = argc > 2 ? atoi(argv[2]) : 1;
    int coder  = argc > 3 ? atoi(argv[3]) : 1;
    int hash   = argc > 4 ? atoi(argv[4]) : 0;
    int minlen = argc > 5 ? atoi(argv[5]) : 0;
    if (n <= 0) { free(in); return 6; }

    size_t cap = (size_t)n + (1 << 16);
    unsigned char *out = (unsigned char *)malloc(cap);
    if (!out) { free(in); return 3; }
    int csize = darc_bsc_compress(in, out, n, hash, minlen, sorter, coder, 0);
    if (csize < 0) { free(in); free(out); return 6; }   /* declined -- e.g. ST7/8 without CUDA */

    fwrite(&n, sizeof(int), 1, stdout);
    fwrite(out, 1, (size_t)csize, stdout);
    free(in); free(out);
    return 0;
  }

  /* decode */
  int n; memcpy(&n, in, sizeof(int));
  unsigned char *block = in + sizeof(int);
  int blockLen = (int)(len - sizeof(int));

  unsigned char *out = (unsigned char *)malloc((size_t)n + 16);
  if (!out) { free(in); return 3; }

  int rc;
#ifdef USE_RUST
  rc = darc_rs_bsc_decompress_block(block, blockLen, out, n);
#else
  rc = darc_bsc_decompress(block, blockLen, out, n, 0);
#endif
  if (rc < 0) { fprintf(stderr, "decode returned %d\n", rc); free(in); free(out); return 4; }

  if (fwrite(out, 1, (size_t)n, stdout) != (size_t)n) { free(in); free(out); return 5; }
  free(in); free(out);
  return 0;
}
