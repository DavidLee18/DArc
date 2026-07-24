/* Reference driver for differential-testing the BSC inverse BWT.
 *
 *     bsc_bwt_ref e      <in  >enc     (C forward BWT: index + aux indexes + transformed bytes)
 *     bsc_bwt_ref d SIZE <enc >out     (invert -- C, or Rust under -DUSE_RUST)
 *
 * The encoded stream is self-describing so the two decoders see identical input:
 *
 *     int32  n
 *     int32  index                 (primary block-sort index)
 *     int32  num_indexes           (auxiliary checkpoint count)
 *     int32  indexes[num_indexes]
 *     uint8  T[n]                   (the transformed block)
 *
 * The encoder replicates libbsc's `if (n < 64*1024) num_indexes = 0`
 * (libbsc.cpp:303): below 64 KiB the format drops the aux indexes and the
 * decoder inverts from the primary index alone (r == n); at/above 64 KiB it
 * keeps them and walks r = mod+1 strided regions. Both paths must reproduce the
 * original, so the corpus straddles 64 KiB.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_init (int features);
int darc_bsc_bwt_encode (unsigned char *T, int n, unsigned char *num_indexes, int *indexes, int features);
int darc_bsc_bwt_decode (unsigned char *T, int n, int index, unsigned char num_indexes, int *indexes, int features);
#ifdef USE_RUST
int darc_rs_bsc_bwt_decode (unsigned char *data, int n, int index, unsigned char num_indexes, const int *indexes);
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
    fprintf(stderr, "usage: %s e | d SIZE\n", argv[0]); return 2;
  }
  darc_bsc_init(0);

  size_t len = 0;
  unsigned char *in = slurp(&len);

  if (argv[1][0] == 'e') {
    int n = (int)len;
    if (n <= 1) { free(in); return 6; }              /* nothing to transform */
    unsigned char num_indexes = 0;
    int *indexes = (int *)malloc(256 * sizeof(int));
    int index = darc_bsc_bwt_encode(in, n, &num_indexes, indexes, 0);
    if (index < 0) { free(in); free(indexes); return 6; }
    if (n < 64 * 1024) num_indexes = 0;              /* the format's own rule */

    int ni = num_indexes;
    fwrite(&n, sizeof(int), 1, stdout);
    fwrite(&index, sizeof(int), 1, stdout);
    fwrite(&ni, sizeof(int), 1, stdout);
    if (ni > 0) fwrite(indexes, sizeof(int), ni, stdout);
    fwrite(in, 1, (size_t)n, stdout);
    free(in); free(indexes);
    return 0;
  }

  /* decode */
  size_t off = 0;
  auto rd_int = [&](void) -> int { int v; memcpy(&v, in + off, sizeof(int)); off += sizeof(int); return v; };
  int n           = rd_int();
  int index       = rd_int();
  int num_indexes = rd_int();
  int *indexes = num_indexes > 0 ? (int *)(in + off) : NULL;
  off += (size_t)num_indexes * sizeof(int);
  unsigned char *T = in + off;

  int rc;
#ifdef USE_RUST
  rc = darc_rs_bsc_bwt_decode(T, n, index, (unsigned char)num_indexes, indexes);
#else
  rc = darc_bsc_bwt_decode(T, n, index, (unsigned char)num_indexes, indexes, 0);
#endif
  if (rc < 0) { fprintf(stderr, "decode returned %d\n", rc); free(in); return 4; }

  if (fwrite(T, 1, (size_t)n, stdout) != (size_t)n) { free(in); return 5; }
  free(in);
  return 0;
}
