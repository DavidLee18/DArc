/* Reference driver for differential-testing the BSC inverse sort-transform.
 *
 *     bsc_st_ref e K      <in  >enc     (C forward ST-K: index + transformed bytes)
 *     bsc_st_ref d SIZE   <enc >out     (invert -- C, or Rust under -DUSE_RUST)
 *
 * K is the ST order, 3..8. The encoded stream is self-describing:
 *
 *     int32  n
 *     int32  k
 *     int32  index
 *     uint8  T[n]
 *
 * ST has no auxiliary indexes (unlike BWT), so the stream is just the primary
 * index and the transformed block.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_init (int features);
int darc_bsc_st_encode (unsigned char *T, int n, int k, int features);
int darc_bsc_st_decode (unsigned char *T, int n, int k, int index, int features);
#ifdef USE_RUST
int darc_rs_bsc_st_decode (unsigned char *data, int n, int k, int index);
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
    fprintf(stderr, "usage: %s e K | d SIZE\n", argv[0]); return 2;
  }
  darc_bsc_init(0);

  size_t len = 0;
  unsigned char *in = slurp(&len);

  if (argv[1][0] == 'e') {
    int n = (int)len;
    int k = argc > 2 ? atoi(argv[2]) : 5;
    if (n <= 1) { free(in); return 6; }
    int index = darc_bsc_st_encode(in, n, k, 0);
    if (index < 0) { free(in); return 6; }   /* encoder declined (e.g. ST8 without SSE) */

    fwrite(&n, sizeof(int), 1, stdout);
    fwrite(&k, sizeof(int), 1, stdout);
    fwrite(&index, sizeof(int), 1, stdout);
    fwrite(in, 1, (size_t)n, stdout);
    free(in);
    return 0;
  }

  /* decode */
  size_t off = 0;
  auto rd_int = [&](void) -> int { int v; memcpy(&v, in + off, sizeof(int)); off += sizeof(int); return v; };
  int n     = rd_int();
  int k     = rd_int();
  int index = rd_int();
  unsigned char *T = in + off;

  int rc;
#ifdef USE_RUST
  rc = darc_rs_bsc_st_decode(T, n, k, index);
#else
  rc = darc_bsc_st_decode(T, n, k, index, 0);
#endif
  if (rc < 0) { fprintf(stderr, "decode returned %d\n", rc); free(in); return 4; }

  if (fwrite(T, 1, (size_t)n, stdout) != (size_t)n) { free(in); return 5; }
  free(in);
  return 0;
}
