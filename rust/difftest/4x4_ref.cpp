/* Reference driver for differential-testing the 4x4 threading meta-codec.
 *
 *     4x4_ref c "<method>" <in >out     compress
 *     4x4_ref d "<method>" <in >out     decompress
 *
 * # Why this driver is shaped differently from the others
 *
 * Every other *_ref.cpp compares a C codec against a RUST port of that codec.
 * 4x4 has no Rust port and is not getting one, by decision: it compresses
 * nothing itself. It splits the input into blocks and calls
 * the library dispatcher, `Decompress(method, ...)`, with an INNER method named
 * in its own parameter string -- so a Rust 4x4 would be Rust calling C calling
 * Rust, for no gain, and a decode-first port would drop the parallelism that is
 * the codec's entire purpose.
 *
 * What is worth testing is the thing that decision leaves exposed. 4x4's output
 * is its own framing wrapped around WHATEVER the dispatcher resolves the inner
 * method to -- and under DARC_RUST those inner codecs are now Rust drop-ins. So
 * the same 4x4 stream is produced by C framing over C codecs at the pinned
 * revision, and by C framing over RUST codecs today. Those must agree byte for
 * byte, because 4x4 is on the default path: Compression.hs:474-481 defines
 * 3binary..9binary as `4x4:bNm:lzma:...`, which is what -m3 through -m9 use for
 * the $binary group.
 *
 * Hence: this driver is built twice from the SAME pinned C source, once linking
 * the pinned C codecs and once with -DDARC_RUST plus the Rust staticlib. The
 * comparison is "did substituting Rust codecs underneath 4x4 change the stream".
 *
 * The method string is a parameter rather than hardcoded so the harness can
 * sweep block sizes and inner methods; `tor` and `lzma` are the two the presets
 * actually use.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

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
    memcpy (b->out + b->out_len, data, (size_t)size);  b->out_len += (size_t)size;
    return size;
  }

  // "quasiwrite" and friends are progress signals; reporting them unimplemented
  // is what the real callback chain does for unknown requests.
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv)
{
  if (argc < 3 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf (stderr, "usage: %s c|d \"<method>\" <in >out\n", argv[0]);  return 2;
  }
  char method[MAX_METHOD_STRLEN];
  strncopy (method, argv[2], sizeof(method));

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
  const char *chunk_env = getenv ("X4_CHUNK");
  b.chunk = chunk_env ? (size_t) strtoul (chunk_env, NULL, 0) : 0;

  int rc = (argv[1][0] == 'c') ? Compress   (method, io_callback, &b)
                               : Decompress (method, io_callback, &b);

  if (rc < 0) { fprintf (stderr, "codec returned %d\n", rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite (b.out, 1, b.out_len, stdout) != b.out_len) { free(in); free(b.out); return 5; }
  free (in); free (b.out); return 0;
}
