/* Reference driver for differential-testing the forward BWT.
 *
 *     bsc_bwt_enc_ref <mode>  <in >out
 *
 * Built twice: plain drives the C (libsais), `-DUSE_RUST` the Rust port.
 *
 *   b  libsais_bwt      -- 4-byte LE index, then the packed transform
 *   a  libsais_bwt_aux  -- 4-byte LE rc, 4-byte LE count, the I[] array,
 *                          then the packed transform
 *   f  bsc_bwt_encode   -- 4-byte LE index, 1-byte num_indexes, the published
 *                          indexes, then the transform
 *
 * The index is emitted alongside the bytes so a single `cmp` covers both. A
 * port that produced the right transform under a wrong primary index would
 * still make every archive undecodable.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

extern "C" {
int32_t libsais_bwt (const uint8_t *T, uint8_t *U, int32_t *A, int32_t n, int32_t fs, int32_t *freq);
int32_t libsais_bwt_aux (const uint8_t *T, uint8_t *U, int32_t *A, int32_t n, int32_t fs, int32_t *freq, int32_t r, int32_t *I);
int bsc_bwt_encode (unsigned char *T, int n, unsigned char *num_indexes, int *indexes, int features);
int darc_bsc_init (int features);
#ifdef USE_RUST
int darc_rs_bsc_bwt_encode (const uint8_t *input, uint8_t *output, int n);
int darc_rs_bsc_bwt_aux_encode (const uint8_t *input, uint8_t *output, int n, int r, int *I);
int darc_rs_bsc_bwt_encode_full (uint8_t *data, int n, unsigned char *num_indexes, int *indexes);
#endif
}

static void put4 (int32_t v) {
  unsigned char b[4] = { (unsigned char)( (uint32_t)v        & 0xff),
                         (unsigned char)(((uint32_t)v >>  8) & 0xff),
                         (unsigned char)(((uint32_t)v >> 16) & 0xff),
                         (unsigned char)(((uint32_t)v >> 24) & 0xff) };
  fwrite (b, 1, 4, stdout);
}

int main (int argc, char **argv) {
  if (argc < 2) return 2;
  char mode = argv[1][0];
  darc_bsc_init (0);

  size_t cap = 1 << 20, len = 0;
  unsigned char *in = (unsigned char *) malloc (cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; in = (unsigned char *) realloc (in, cap); if (!in) return 3; }
    size_t r = fread (in + len, 1, cap - len, stdin);
    if (r == 0) break;
    len += r;
  }
  int n = (int) len;
  if (n <= 0) { free (in); return 0; }

  unsigned char *U = (unsigned char *) malloc ((size_t) n);
  int32_t *A = (int32_t *) malloc ((size_t) n * sizeof (int32_t));
  if (!U || !A) return 3;

  if (mode == 'b') {
#ifdef USE_RUST
    int index = darc_rs_bsc_bwt_encode (in, U, n);
#else
    int index = libsais_bwt (in, U, A, n, 0, NULL);
#endif
    put4 (index);
    if (index >= 0) fwrite (U, 1, (size_t) n, stdout);

  } else if (mode == 'a') {
    /* the same mod/r bwt.cpp derives from n */
    int mod = n / 8;
    mod |= mod >> 1;  mod |= mod >> 2;  mod |= mod >> 4;
    mod |= mod >> 8;  mod |= mod >> 16; mod >>= 1;
    int r = mod + 1;
    if (r < 2) { free (in); return 4; }          /* n too small for the aux path */
    int32_t I[256];
    memset (I, 0, sizeof I);
#ifdef USE_RUST
    int rc = darc_rs_bsc_bwt_aux_encode (in, U, n, r, I);
#else
    int rc = libsais_bwt_aux (in, U, A, n, 0, NULL, r, I);
#endif
    int cnt = (n - 1) / r + 1;
    put4 (rc);
    put4 (cnt);
    for (int j = 0; j < cnt && j < 256; j++) put4 (I[j]);
    if (rc == 0) fwrite (U, 1, (size_t) n, stdout);

  } else if (mode == 'f') {
    unsigned char num_indexes = 0;
    int indexes[256];
    memset (indexes, 0, sizeof indexes);
    memcpy (U, in, (size_t) n);
#ifdef USE_RUST
    int index = darc_rs_bsc_bwt_encode_full (U, n, &num_indexes, indexes);
#else
    int index = bsc_bwt_encode (U, n, &num_indexes, indexes, 0);
#endif
    put4 (index);
    fwrite (&num_indexes, 1, 1, stdout);
    for (int t = 0; t < num_indexes; t++) put4 (indexes[t]);
    if (index >= 0) fwrite (U, 1, (size_t) n, stdout);

  } else {
    return 2;
  }

  free (in); free (U); free (A);
  return 0;
}
