/* Reference driver for differential-testing the LZP codec port.
 *
 *     lzp_ref c <in >out     compress with the C original
 *     lzp_ref d <in >out     decompress
 *
 * Built a second time with -DUSE_RUST to drive the Rust port instead, so the
 * two can be diffed on identical input. Same rationale as delta_ref.cpp: these
 * codecs define the archive format, so a port has to be bit-exact.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

// extern "C" because C_LZP.cpp wraps its whole body in extern "C" and
// #includes dict.cpp, so these are C symbols -- the same arrangement as
// C_Delta.cpp. That is also why swapping in the Rust port has to exclude the C
// implementation rather than redeclare it: with both present the linker
// resolves from the object file and never touches the Rust archive.
extern "C" {
int lzp_compress   (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog,
                     int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);
int lzp_decompress (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog,
                     int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);
}

#ifdef USE_RUST
extern "C" int darc_rs_lzp_decompress (MemSize BlockSize, int, int, int, int, int,
                                        CALLBACK_FUNC *callback, void *auxdata);
#endif

// C_LZP.cpp registers itself with the compression-method table and inherits
// from COMPRESSION_METHOD, so CompressionLibrary.cpp has to be linked in --
// stubbing AddCompressionMethod/LoadFromDLL is not enough, the base class
// vtable is needed too.

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
};

static int io_callback (const char *what, void *data, int size, void *auxdata)
{
  Buffers *b = (Buffers*) auxdata;
  if (size < 0)  return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what, "read") == 0) {
    size_t avail = b->in_len - b->in_pos;
    size_t n = (size_t)size < avail ? (size_t)size : avail;
    memcpy (data, b->in + b->in_pos, n);  b->in_pos += n;  return (int) n;
  }
  if (strcmp(what, "write") == 0) {
    if (b->out_len + (size_t)size > b->out_cap) {
      size_t cap = (b->out_cap ? b->out_cap : 65536);
      while (cap < b->out_len + (size_t)size)  cap *= 2;
      unsigned char *grown = (unsigned char*) realloc (b->out, cap);
      if (!grown)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out = grown;  b->out_cap = cap;
    }
    memcpy (b->out + b->out_len, data, (size_t)size);  b->out_len += (size_t)size;  return size;
  }
  // "quasiwrite" and "time" are progress signals the archiver consumes; the
  // codec does not depend on their result, so reporting them unimplemented is
  // what the real callback chain does for unknown requests too.
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv)
{
  if (argc < 2 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf (stderr, "usage: %s c|d [blocksize] <in >out\n", argv[0]);  return 2;
  }
  MemSize blocksize = argc > 2 ? (MemSize) strtoul (argv[2], NULL, 0) : 8*1024*1024;

  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*) malloc (cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g = (unsigned char*) realloc (in, cap); if (!g) { free(in); return 3; } in = g; }
    size_t n = fread (in + len, 1, cap - len, stdin);
    if (n == 0) break;
    len += n;
  }

  Buffers b; b.in = in; b.in_len = len; b.in_pos = 0; b.out = NULL; b.out_len = 0; b.out_cap = 0;

  // Defaults taken from C_LZP.cpp's parse_DICT.
  // Defaults from parse_LZP in C_LZP.cpp.
  const int MinCompression = 100, MinMatchLen = 32, HashSizeLog = 16, Barrier = 0x7fffffff,
            SmallestLen = 32;
  int rc;
  if (argv[1][0] == 'c')
    rc = lzp_compress (blocksize, MinCompression, MinMatchLen, HashSizeLog, Barrier, SmallestLen, io_callback, &b);
  else
#ifdef USE_RUST
    rc = darc_rs_lzp_decompress (blocksize, MinCompression, MinMatchLen, HashSizeLog, Barrier, SmallestLen, io_callback, &b);
#else
    rc = lzp_decompress (blocksize, MinCompression, MinMatchLen, HashSizeLog, Barrier, SmallestLen, io_callback, &b);
#endif

  if (rc < 0) { fprintf (stderr, "codec returned %d\n", rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite (b.out, 1, b.out_len, stdout) != b.out_len) { free(in); free(b.out); return 5; }
  free (in); free (b.out); return 0;
}
