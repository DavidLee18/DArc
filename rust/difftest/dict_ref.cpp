/* Reference driver for differential-testing the Dict codec port.
 *
 *     dict_ref c <in >out     compress with the C original
 *     dict_ref d <in >out     decompress
 *
 * Built a second time with -DUSE_RUST to drive the Rust port instead, so the
 * two can be diffed on identical input. Same rationale as delta_ref.cpp: these
 * codecs define the archive format, so a port has to be bit-exact.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

// extern "C" because C_Dict.cpp wraps its whole body in extern "C" and
// #includes dict.cpp, so these are C symbols -- the same arrangement as
// C_Delta.cpp. That is also why swapping in the Rust port has to exclude the C
// implementation rather than redeclare it: with both present the linker
// resolves from the object file and never touches the Rust archive.
extern "C" {
int dict_compress   (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt,
                     int MinMediumCnt, int MinSmallCnt, int MinRatio,
                     CALLBACK_FUNC *callback, void *auxdata);
int dict_decompress (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt,
                     int MinMediumCnt, int MinSmallCnt, int MinRatio,
                     CALLBACK_FUNC *callback, void *auxdata);
}

#ifdef USE_RUST
extern "C" int darc_rs_dict_compress   (MemSize BlockSize, int, int, int, int, int, int,
                                        CALLBACK_FUNC *callback, void *auxdata);
extern "C" int darc_rs_dict_decompress (MemSize BlockSize, int, int, int, int, int, int,
                                        CALLBACK_FUNC *callback, void *auxdata);
#endif

// C_Dict.cpp registers itself with the compression-method table and inherits
// from COMPRESSION_METHOD, so CompressionLibrary.cpp has to be linked in --
// stubbing AddCompressionMethod/LoadFromDLL is not enough, the base class
// vtable is needed too.

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
  size_t chunk;   // max bytes one "read" may return; 0 = no limit
};

static int io_callback (const char *what, void *data, int size, void *auxdata)
{
  Buffers *b = (Buffers*) auxdata;
  if (size < 0)  return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what, "read") == 0) {
    size_t avail = b->in_len - b->in_pos;
    size_t n = (size_t)size < avail ? (size_t)size : avail;
    // A short read is not an error here, and getting this wrong hid a real
    // divergence for a long time. dict_compress loops on "read", so in the
    // archiver the codec sees a *sequence* of pipeline-sized buffers, and the
    // block boundaries decide which words each DictEncode call ever sees.
    // Returning the whole input in one read -- what this driver used to do --
    // exercises exactly one block and can never reproduce that.
    if (b->chunk && n > b->chunk)  n = b->chunk;
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

/* Mode 'v': mimic dict_compress's read loop but call DictEncode directly and
 * report what each block decided. dict_compress only reports a total, so a
 * per-block disagreement between implementations is invisible in it -- which
 * is how a block that compresses standalone but declines mid-stream went
 * unexplained. C++ linkage, matching dict.cpp (see the note at the top). */
#ifndef DARC_RUST
int DictEncode (byte *buf, unsigned bufsize, byte **outbuf, unsigned *outsize,
                int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio);

static void verbose_stream (const unsigned char *in, size_t len, size_t chunk,
                            int MinCompression, int MinWeakChars, int MinLargeCnt,
                            int MinMediumCnt, int MinSmallCnt, int MinRatio)
{
  size_t pos = 0;  int blk = 0;
  while (pos < len) {
    unsigned InSize = (unsigned) ((len - pos < chunk) ? len - pos : chunk);
    byte *In = (byte*) malloc (InSize);
    memcpy (In, in + pos, InSize);
    byte *Out = NULL;  unsigned OutSize = 0;
    int x = DictEncode (In, InSize, &Out, &OutSize, MinWeakChars, MinLargeCnt,
                        MinMediumCnt, MinSmallCnt, MinRatio);
    int declined = (x || OutSize/MinCompression >= InSize/100);
    fprintf (stderr, "block %2d  in=%-7u rc=%-3d out=%-7u %s\n",
             blk++, InSize, x, OutSize, declined ? "DECLINED" : "engaged");
    FreeAndNil (Out);  FreeAndNil (In);
    pos += InSize;
  }
}
#endif

int main (int argc, char **argv)
{
  if (argc < 2 || (argv[1][0] != 'c' && argv[1][0] != 'd' && argv[1][0] != 'v')) {
    fprintf (stderr, "usage: %s c|d|v [blocksize] <in >out\n", argv[0]);  return 2;
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
  const char *chunk_env = getenv ("DICT_CHUNK");
  b.chunk = chunk_env ? (size_t) strtoul (chunk_env, NULL, 0) : 0;

  // These MUST match DICT_METHOD's constructor (C_Dict.cpp:111-116), which is
  // what the archiver actually runs. An earlier version of this driver used
  // MinWeakChars=0, MinLargeCnt=200, MinMediumCnt=200, MinSmallCnt=200,
  // MinRatio=0 under a comment claiming they came from parse_DICT. They did
  // not: five of the six were wrong. The port was declared byte-identical on a
  // parameter set the archiver never uses, and the first whole-archive
  // comparison found a 9200-byte divergence the phase harness could not see.
  // If these drift from C_Dict.cpp again the suite goes quietly green on
  // nothing, so they are overridable below only to *widen* coverage.
  int MinCompression = 100, MinWeakChars = 20, MinLargeCnt = 2048, MinMediumCnt = 100,
      MinSmallCnt = 50, MinRatio = 4;
  if (argc > 3)  MinCompression = atoi (argv[3]);
  if (argc > 4)  MinWeakChars   = atoi (argv[4]);
  if (argc > 5)  MinLargeCnt    = atoi (argv[5]);
  if (argc > 6)  MinMediumCnt   = atoi (argv[6]);
  if (argc > 7)  MinSmallCnt    = atoi (argv[7]);
  if (argc > 8)  MinRatio       = atoi (argv[8]);

#ifndef DARC_RUST
  if (argv[1][0] == 'v') {
    verbose_stream (in, len, b.chunk ? b.chunk : len, MinCompression, MinWeakChars,
                    MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio);
    free (in);  return 0;
  }
#endif

  int rc;
  if (argv[1][0] == 'c')
#ifdef USE_RUST
    rc = darc_rs_dict_compress (blocksize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, io_callback, &b);
#else
    rc = dict_compress (blocksize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, io_callback, &b);
#endif
  else
#ifdef USE_RUST
    rc = darc_rs_dict_decompress (blocksize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, io_callback, &b);
#else
    rc = dict_decompress (blocksize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, io_callback, &b);
#endif

  if (rc < 0) { fprintf (stderr, "codec returned %d\n", rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite (b.out, 1, b.out_len, stdout) != b.out_len) { free(in); free(b.out); return 5; }
  free (in); free (b.out); return 0;
}
