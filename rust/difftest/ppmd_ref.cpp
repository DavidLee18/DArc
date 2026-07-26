/* Reference driver for differential-testing PPMd var.H.
 *
 *     ppmd_ref c ORDER MEM_MB MRMETHOD  <in  >enc
 *     ppmd_ref d ORDER MEM_MB MRMETHOD  <enc >out
 *
 * Built twice: plain drives the C, `-DUSE_RUST` the Rust port.
 *
 * The codec is callback-driven -- C drives the loop and asks for "read"/"write"
 * -- so this supplies a callback backed by two in-memory buffers.
 *
 * The return code is emitted in the first four bytes ahead of the payload, so
 * that one side refusing where the other codes shows up as a difference rather
 * than as two empty files comparing equal.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

extern "C" {
int darc_ppmd_stream_compress   (int order, MemSize mem, int mrmethod, CALLBACK_FUNC *cb, void *aux);
int darc_ppmd_stream_decompress (int order, MemSize mem, int mrmethod, CALLBACK_FUNC *cb, void *aux);
#ifdef USE_RUST
int darc_rs_ppmd_compress   (int order, unsigned mem, int mrmethod, CALLBACK_FUNC *cb, void *aux);
int darc_rs_ppmd_decompress (int order, unsigned mem, int mrmethod, CALLBACK_FUNC *cb, void *aux);
#endif
}

struct Buffers {
  const unsigned char *in;
  size_t in_len, in_pos;
  unsigned char *out;
  size_t out_cap, out_len;
};

/* The archiver's callback protocol: "read" fills the buffer and returns the
 * count, "write" consumes it. Anything else is unsupported here. */
static int cb (const char *what, void *buf, int size, void *aux)
{
  Buffers *b = (Buffers *) aux;
  if (strcmp (what, "read") == 0) {
    size_t left = b->in_len - b->in_pos;
    size_t n = (size_t) size < left ? (size_t) size : left;
    memcpy (buf, b->in + b->in_pos, n);
    b->in_pos += n;
    return (int) n;
  }
  if (strcmp (what, "write") == 0) {
    if (b->out_len + (size_t) size > b->out_cap) {
      b->out_cap = (b->out_len + (size_t) size) * 2;
      b->out = (unsigned char *) realloc (b->out, b->out_cap);
      if (!b->out) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    }
    memcpy (b->out + b->out_len, buf, (size_t) size);
    b->out_len += (size_t) size;
    return size;
  }
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

static unsigned char *slurp (size_t *len)
{
  size_t cap = 1 << 20, n = 0;
  unsigned char *b = (unsigned char *) malloc (cap);
  if (!b) exit (3);
  for (;;) {
    if (n == cap) { cap *= 2; b = (unsigned char *) realloc (b, cap); if (!b) exit (3); }
    size_t r = fread (b + n, 1, cap - n, stdin);
    if (r == 0) break;
    n += r;
  }
  *len = n;
  return b;
}

int main (int argc, char **argv)
{
  if (argc < 5 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf (stderr, "usage: %s c|d ORDER MEM_MB MRMETHOD\n", argv[0]);
    return 2;
  }
  char mode   = argv[1][0];
  int order   = atoi (argv[2]);
  MemSize mem = (MemSize) atoi (argv[3]) * 1024 * 1024;
  int mrm     = atoi (argv[4]);

  size_t len = 0;
  unsigned char *in = slurp (&len);

  Buffers b;
  b.in = in; b.in_len = len; b.in_pos = 0;
  b.out_cap = len + (1 << 16); b.out_len = 0;
  b.out = (unsigned char *) malloc (b.out_cap);
  if (!b.out) return 3;

  int rc;
  if (mode == 'c') {
#ifdef USE_RUST
    rc = darc_rs_ppmd_compress (order, (unsigned) mem, mrm, cb, &b);
#else
    rc = darc_ppmd_stream_compress (order, mem, mrm, cb, &b);
#endif
  } else {
#ifdef USE_RUST
    rc = darc_rs_ppmd_decompress (order, (unsigned) mem, mrm, cb, &b);
#else
    rc = darc_ppmd_stream_decompress (order, mem, mrm, cb, &b);
#endif
  }

  unsigned char hdr[4] = { (unsigned char)( (unsigned) rc        & 0xff),
                           (unsigned char)(((unsigned) rc >>  8) & 0xff),
                           (unsigned char)(((unsigned) rc >> 16) & 0xff),
                           (unsigned char)(((unsigned) rc >> 24) & 0xff) };
  fwrite (hdr, 1, 4, stdout);
  fwrite (b.out, 1, b.out_len, stdout);

  free (in); free (b.out);
  return 0;
}
