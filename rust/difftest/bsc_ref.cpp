/* Reference driver for differential-testing the BSC QLFC decoders.
 *
 *     bsc_ref c CODER  <in >coded     (C encodes one QLFC block)
 *     bsc_ref d CODER SIZE <coded >out
 *
 * Built a second time with -DUSE_RUST, where `d` drives the Rust decoder and
 * `c` the Rust ENCODERS -- all three coders.
 *
 * CODER: 1 = QLFC static (libbsc's default), 2 = adaptive, 3 = fast.
 *
 * Deliberately at the coder level: no BWT, ST, LZP or block header. QLFC is the
 * largest and most error-prone part of the BSC port, and testing it alone means
 * a failure points at the range coder / mixer / model rather than at four
 * interacting stages.
 *
 * Note the C encoder can refuse a block ("not compressible"); the harness treats
 * that as a skip, not a failure -- it is the encoder's decision, not a decode
 * disagreement.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_bsc_coder_encode_block (const unsigned char *in, unsigned char *out, int inSize, int outSize, int coder);
int darc_bsc_coder_decode_block (const unsigned char *in, unsigned char *out, int coder);
int darc_bsc_init (int features);
#ifdef USE_RUST
int darc_rs_bsc_qlfc_decode (const unsigned char *in, int inSize, unsigned char *out, int outCap, int coder);
int darc_rs_bsc_qlfc_static_encode (const unsigned char *in, int inSize, unsigned char *out, int outSize);
int darc_rs_bsc_qlfc_adaptive_encode (const unsigned char *in, int inSize, unsigned char *out, int outSize);
int darc_rs_bsc_qlfc_fast_encode (const unsigned char *in, int inSize, unsigned char *out, int outSize);
#endif
}

int main (int argc, char **argv) {
  if (argc < 2 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf(stderr, "usage: %s c CODER | d CODER SIZE\n", argv[0]); return 2; }
  darc_bsc_init(0);
  int coder = argc > 2 ? atoi(argv[2]) : 1;

  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*)malloc(cap); if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g = (unsigned char*)realloc(in, cap); if (!g) { free(in); return 3; } in = g; }
    size_t n = fread(in + len, 1, cap - len, stdin); if (n == 0) break; len += n;
  }

  /* Encode buffer is sized from the input; decode buffer from the stated
   * output SIZE, since compressible input expands far past len*2 and
   * bsc_coder_decode_block writes dataSize bytes without bounding to a cap. */
  int size = argc > 3 ? atoi(argv[3]) : (int)(len * 2 + (1<<16));
  size_t outCap = argv[1][0] == 'c' ? len * 2 + (1<<16) : (size_t)size + 4096;
  unsigned char *out = (unsigned char*)malloc(outCap); if (!out) { free(in); return 3; }
  int rc;

  if (argv[1][0] == 'c') {
    if (len == 0) { free(in); free(out); return 6; }   /* nothing to code */
#ifdef USE_RUST
    /* Only the static coder has a Rust encoder so far; the harness asks for the
     * others from the C on both sides, which compares nothing and says so. */
    if (coder == 1)      rc = darc_rs_bsc_qlfc_static_encode  (in, (int)len, out, (int)outCap);
    else if (coder == 2) rc = darc_rs_bsc_qlfc_adaptive_encode(in, (int)len, out, (int)outCap);
    else if (coder == 3) rc = darc_rs_bsc_qlfc_fast_encode    (in, (int)len, out, (int)outCap);
    else { free(in); free(out); return 7; }
#else
    rc = darc_bsc_coder_encode_block(in, out, (int)len, (int)outCap, coder);
#endif
    if (rc < 0) { free(in); free(out); return 6; }     /* encoder declined: skip */
  } else {
#ifdef USE_RUST
    rc = darc_rs_bsc_qlfc_decode(in, (int)len, out, size, coder);
#else
    rc = darc_bsc_coder_decode_block(in, out, coder);
#endif
    if (rc < 0) { fprintf(stderr, "decode returned %d\n", rc); free(in); free(out); return 4; }
  }
  if (rc > 0 && fwrite(out, 1, (size_t)rc, stdout) != (size_t)rc) { free(in); free(out); return 5; }
  free(in); free(out); return 0;
}
