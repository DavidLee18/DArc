/* Reference driver for differential-testing the Delta codec port.
 *
 * Links the original C Delta.cpp and drives it over an in-memory buffer using
 * the same callback protocol the archiver uses, so a ported implementation can
 * be compared against it byte for byte:
 *
 *     delta_ref  c   <in >out      compress with the C original
 *     delta_ref  d   <in >out      decompress with the C original
 *
 * Why this exists: these codecs define the archive format. A port that
 * compresses "correctly" but differently produces archives older builds cannot
 * read, which is the highest-risk failure mode in this repository. Comparing
 * whole archives catches that, but only one bit at a time and only for inputs
 * the corpus happens to contain. Driving the codec directly makes it cheap to
 * throw thousands of inputs at both implementations and diff the results.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

// Declared with C++ linkage, deliberately: Delta.cpp defines these without
// extern "C", so C_Delta.cpp links against the mangled names. Declaring them
// extern "C" here compiles fine and then fails at link time with "undefined
// symbol _delta_compress".
//
// That matters beyond this driver. A Rust replacement exporting
// #[no_mangle] extern "C" fn delta_compress would not link either, for exactly
// the same reason -- so swapping a codec needs extern "C" added to the C
// declarations (a linkage-only change) or a small C++ shim forwarding to the
// Rust symbol.
int delta_compress   (MemSize BlockSize, int ExtendedTables, CALLBACK_FUNC *callback, void *auxdata);
int delta_decompress (MemSize BlockSize, int ExtendedTables, CALLBACK_FUNC *callback, void *auxdata);

/* Built a second time with -DUSE_RUST to produce the same driver over the Rust
 * port, so the two can be diffed on identical input. The Rust symbol IS
 * extern "C" -- it is the C original that is not. */
#ifdef USE_RUST
extern "C" int darc_rs_delta_compress   (MemSize BlockSize, int ExtendedTables,
                                         CALLBACK_FUNC *callback, void *auxdata);
extern "C" int darc_rs_delta_decompress (MemSize BlockSize, int ExtendedTables,
                                         CALLBACK_FUNC *callback, void *auxdata);
#endif

// Buffers the codec reads from and writes to.
struct Buffers {
  const unsigned char *in;
  size_t in_len, in_pos;
  unsigned char *out;
  size_t out_len, out_cap;
};

static int io_callback (const char *what, void *data, int size, void *auxdata)
{
  Buffers *b = (Buffers*) auxdata;
  if (size < 0)  return FREEARC_ERRCODE_GENERAL;

  if (strcmp(what, "read") == 0) {
    size_t avail = b->in_len - b->in_pos;
    size_t n = (size_t)size < avail ? (size_t)size : avail;
    memcpy (data, b->in + b->in_pos, n);
    b->in_pos += n;
    return (int) n;
  }

  if (strcmp(what, "write") == 0) {
    if (b->out_len + (size_t)size > b->out_cap) {
      size_t cap = (b->out_cap ? b->out_cap : 65536);
      while (cap < b->out_len + (size_t)size)  cap *= 2;
      unsigned char *grown = (unsigned char*) realloc (b->out, cap);
      if (!grown)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out = grown;                 // realloc may move the block; see the
      b->out_cap = cap;               // dict.cpp phase2 bug for why this is
    }                                 // assigned rather than assumed stable
    memcpy (b->out + b->out_len, data, (size_t)size);
    b->out_len += (size_t)size;
    return size;
  }

  // Anything else the codec might ask about is not supported here.
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv)
{
  if (argc < 2 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf (stderr, "usage: %s c|d [blocksize] [extended] <in >out\n", argv[0]);
    return 2;
  }
  MemSize blocksize = argc > 2 ? (MemSize) strtoul (argv[2], NULL, 0) : 8*1024*1024;
  int extended      = argc > 3 ? atoi (argv[3]) : 0;

  // Slurp stdin.
  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*) malloc (cap);
  if (!in)  { fprintf (stderr, "oom\n"); return 3; }
  for (;;) {
    if (len == cap) {
      cap *= 2;
      unsigned char *grown = (unsigned char*) realloc (in, cap);
      if (!grown)  { free(in); fprintf (stderr, "oom\n"); return 3; }
      in = grown;
    }
    size_t n = fread (in + len, 1, cap - len, stdin);
    if (n == 0)  break;
    len += n;
  }

  Buffers b;
  b.in = in;  b.in_len = len;  b.in_pos = 0;
  b.out = NULL;  b.out_len = 0;  b.out_cap = 0;

#ifdef USE_RUST
  int rc = argv[1][0] == 'c'
         ? darc_rs_delta_compress    (blocksize, extended, io_callback, &b)
         : darc_rs_delta_decompress  (blocksize, extended, io_callback, &b);
#else
  int rc = argv[1][0] == 'c'
         ? delta_compress   (blocksize, extended, io_callback, &b)
         : delta_decompress (blocksize, extended, io_callback, &b);
#endif

  if (rc < 0) {
    fprintf (stderr, "codec returned %d\n", rc);
    free (in);  free (b.out);
    return 4;
  }
  if (b.out_len && fwrite (b.out, 1, b.out_len, stdout) != b.out_len) {
    fprintf (stderr, "short write\n");
    free (in);  free (b.out);
    return 5;
  }
  free (in);  free (b.out);
  return 0;
}
