/* Reference driver for differential-testing the BCJ x86 (`exe`) filter port.
 *
 *     bcj_ref c CHUNK  <in >filtered      encode (relative -> absolute)
 *     bcj_ref d CHUNK  <in >unfiltered    decode (absolute -> relative)
 *
 * CHUNK caps how many bytes each `read` callback returns; 0 means "give the
 * codec as much as it asks for". That knob is the point of this driver. BCJ
 * carries two pieces of state across calls -- `_bufferPos`, which feeds the
 * absolute position added to every displacement, and `_prevMask`, which
 * remembers whether the last three positions held a branch byte -- so a port
 * that resets either one round-trips perfectly at one chunk size and corrupts
 * at another. Nothing else about this codec is hard to get right.
 *
 * Built twice: once as-is, driving the C through `BCJ_X86_METHOD`, and once
 * with -DUSE_RUST, driving `rust/darc-codecs/src/bcj.rs`. The C reference comes
 * from a pinned revision, not the working tree -- see c-reference.sh.
 *
 * The C build textually includes C_BCJ.cpp itself rather than reimplementing its
 * streaming loop, because that loop IS part of what is being ported: the
 * `InSize <= 5` bypass, the remainder memmove and the "return the callback's
 * value unchanged" error path all affect the bytes emitted. Reproducing it here
 * would mean the oracle and the port could share a mistake. C_BCJ.cpp needs
 * exactly two symbols from the rest of the compression library --
 * AddCompressionMethod (called by its static registration) and
 * COMPRESSION_METHOD::doit (a non-pure virtual, so the vtable references it) --
 * and both are stubbed below rather than linking CompressionLibrary.cpp.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "../../Compression/Compression.h"
}

#ifdef USE_RUST
extern "C" int darc_rs_bcj_x86_compress   (CALLBACK_FUNC*, void*);
extern "C" int darc_rs_bcj_x86_decompress (CALLBACK_FUNC*, void*);
#else
/* Stubs for the two symbols C_BCJ.cpp pulls in from the compression library.
 * Neither is reachable from compress()/decompress(); AddCompressionMethod runs
 * once at static-init time and its result is discarded. */
extern "C" int AddCompressionMethod (CM_PARSER parser) { (void)parser; return 0; }
int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  { (void)what; (void)param; (void)data; (void)callback; return FREEARC_ERRCODE_NOT_IMPLEMENTED; }
#include "../../Compression/LZMA/C_BCJ.cpp"
#endif

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
  size_t chunk;          /* 0 = unlimited */
  long   reads, writes;  /* reported on stderr, so a run that never called back is visible */
};

static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what,"read")==0) {
    size_t want = (size_t)size;
    if (b->chunk && want > b->chunk) want = b->chunk;
    size_t avail = b->in_len - b->in_pos, n = want < avail ? want : avail;
    memcpy(data, b->in + b->in_pos, n); b->in_pos += n; b->reads++;
    return (int)n;
  }
  if (strcmp(what,"write")==0) {
    if (b->out_len + (size_t)size > b->out_cap) {
      size_t cap = b->out_cap ? b->out_cap : 65536;
      while (cap < b->out_len + (size_t)size) cap *= 2;
      unsigned char *g = (unsigned char*) realloc(b->out, cap);
      if (!g) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out = g; b->out_cap = cap;
    }
    memcpy(b->out + b->out_len, data, (size_t)size); b->out_len += (size_t)size; b->writes++;
    return size;
  }
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv) {
  if (argc < 2 || (argv[1][0] != 'c' && argv[1][0] != 'd')) {
    fprintf(stderr, "usage: %s c|d [CHUNK]\n", argv[0]); return 2; }
  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*) malloc(cap); if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g = (unsigned char*) realloc(in, cap);
                      if (!g) { free(in); return 3; } in = g; }
    size_t n = fread(in+len, 1, cap-len, stdin); if (n == 0) break; len += n; }

  Buffers b; memset(&b, 0, sizeof b);
  b.in = in; b.in_len = len;
  b.chunk = argc > 2 ? (size_t) strtoul(argv[2], NULL, 10) : 0;

  int rc;
#ifdef USE_RUST
  rc = (argv[1][0]=='c') ? darc_rs_bcj_x86_compress   (io_callback, &b)
                         : darc_rs_bcj_x86_decompress (io_callback, &b);
#else
  BCJ_X86_METHOD m;
  rc = (argv[1][0]=='c') ? m.compress   (io_callback, &b)
                         : m.decompress (io_callback, &b);
#endif

  fprintf(stderr, "reads=%ld writes=%ld in=%zu out=%zu\n", b.reads, b.writes, len, b.out_len);
  if (rc < 0) { fprintf(stderr, "codec returned %d\n", rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out, 1, b.out_len, stdout) != b.out_len) {
    free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
