/* Reference driver for differential-testing the QLFC forward transform.
 *
 *     bsc_qlfc_transform_ref  <in >out
 *
 * Built twice: plain drives the C `bsc_qlfc_transform`, `-DUSE_RUST` the Rust
 * port. The transform is the stage all three QLFC encode bodies share, so
 * cutting here means a mismatch points at the move-to-front walk rather than at
 * the range coder wrapped around it -- the same reason the QLFC *decoders* were
 * tested at the coder level rather than through a whole block.
 *
 * stdout is:
 *
 *     int32  index          where the rank array starts within the buffer
 *     uint8  mtf[256]       the alphabet the encoder codes as its preamble
 *     uint8  ranks[n-index] the rank array itself
 *
 * Both outputs are compared, not just the ranks: `MTFTable` is an output of
 * this function, and a port that produced the right ranks from a wrong table
 * would go on to write a preamble no decoder could follow.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_init (int features);
int darc_bsc_qlfc_transform (const unsigned char *in, int n, unsigned char *buffer, unsigned char *mtf);
#ifdef USE_RUST
int darc_rs_bsc_qlfc_transform (const unsigned char *in, int n, unsigned char *buffer, unsigned char *mtf);
#endif
}

int main (void) {
  darc_bsc_init(0);

  size_t cap = 1 << 20, len = 0;
  unsigned char *in = (unsigned char *)malloc(cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; in = (unsigned char *)realloc(in, cap); if (!in) return 3; }
    size_t n = fread(in + len, 1, cap - len, stdin);
    if (n == 0) break;
    len += n;
  }
  int n = (int)len;
  if (n <= 0) { free(in); return 0; }

  unsigned char *buffer = (unsigned char *)malloc(n);
  unsigned char mtf[256];
  if (!buffer) { free(in); return 3; }
  memset(buffer, 0, n);
  memset(mtf, 0, sizeof mtf);

#ifdef USE_RUST
  int index = darc_rs_bsc_qlfc_transform (in, n, buffer, mtf);
#else
  int index = darc_bsc_qlfc_transform (in, n, buffer, mtf);
#endif

  unsigned char hdr[4] = { (unsigned char)( (unsigned)index        & 0xff),
                           (unsigned char)(((unsigned)index >>  8) & 0xff),
                           (unsigned char)(((unsigned)index >> 16) & 0xff),
                           (unsigned char)(((unsigned)index >> 24) & 0xff) };
  fwrite (hdr, 1, 4, stdout);
  fwrite (mtf, 1, sizeof mtf, stdout);
  if (index >= 0 && index < n) fwrite (buffer + index, 1, (size_t)(n - index), stdout);

  free(in); free(buffer);
  return 0;
}
