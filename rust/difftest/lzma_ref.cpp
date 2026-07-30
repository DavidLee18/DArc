// Drive the PINNED C LZMA encoder over stdin -> stdout, so its bytes can be
// compared against rust/darc-lzma's stock-SDK encoder.
//
// This is a MEASUREMENT harness, not a port check. Nothing in DArc uses
// darc-lzma yet; the question it answers is how far DArc's forked LZMA is from
// stock LZMA, which decides whether that fork can be re-derived incrementally.
//
// Two differences are known up front and are the reason a naive diff would be
// uninformative:
//
//   * DArc sets `props.writeEndMark = 1` ("FreeArc streams with EOPM"), while the
//     Rust side emits none. So DArc's stream should be the Rust stream plus an
//     end-of-payload marker -- the comparison must allow for that, not trip over
//     it.
//   * DArc sets `props.numThreads = GetCompressionThreads() > 1 ? 2 : 1`. The
//     SDK's multi-threaded match finder can emit different bytes than the
//     single-threaded one, which is a divergence axis independent of DArc's own
//     changes. `-t1` below forces one thread so that axis can be isolated.
//
// argv: dictSize lc lp pb fb mc matchFinder algorithm
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "../../Compression/Compression.h"

// C_LZMA.cpp is compiled INTO this driver, the same way crypto_ccodec.cpp
// includes C_Encryption.cpp: calling DArc's own `lzma_compress` is the point --
// transcribing its 20-line CLzmaEncProps block into this file would measure my
// transcription, not DArc's configuration.
//
// Including it drags in LZMA_METHOD, which needs three things from
// CompressionLibrary.cpp. They are stubbed below, exactly as the crypto harness
// stubs the same symbols.
#include "../../Compression/LZMA/C_LZMA.cpp"

// ── Stubs standing in for CompressionLibrary.cpp ────────────────────────────
int AddCompressionMethod (CM_PARSER parser) { (void) parser; return 0; }

// One thread, deliberately. DArc uses `GetCompressionThreads() > 1 ? 2 : 1`, and
// the SDK's multi-threaded match finder can emit different bytes than the
// single-threaded one. Returning 1 isolates DArc's OWN divergences from that
// axis; measuring the mt path is a separate question.
int GetCompressionThreads (void) { return 1; }

// Only LZMA_METHOD::compress/decompress reach this, and this driver calls
// lzma_compress directly.
// Signature per Compression.h:192 -- `FARPROC LoadFromDLL (char *funcname)`.
FARPROC LoadFromDLL (char *funcname) { (void) funcname; return NULL; }

int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  (void) what; (void) param; (void) data; (void) callback;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
};

static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what,"read")==0) {
    size_t avail=b->in_len-b->in_pos, n=(size_t)size<avail?(size_t)size:avail;
    memcpy(data,b->in+b->in_pos,n); b->in_pos+=n; return (int)n;
  }
  if (strcmp(what,"write")==0) {
    if (b->out_len+(size_t)size>b->out_cap) {
      size_t cap=b->out_cap?b->out_cap:65536;
      while (cap<b->out_len+(size_t)size) cap*=2;
      unsigned char *g=(unsigned char*)realloc(b->out,cap);
      if(!g) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out=g; b->out_cap=cap;
    }
    memcpy(b->out+b->out_len,data,(size_t)size); b->out_len+=(size_t)size; return size;
  }
  if (strcmp(what,"quasiwrite")==0)  return 0;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv) {
  if (argc < 9) {
    fprintf(stderr, "usage: %s dictSize lc lp pb fb mc matchFinder algorithm\n", argv[0]);
    return 2;
  }
  int dictSize   = atoi(argv[1]);
  int lc         = atoi(argv[2]);
  int lp         = atoi(argv[3]);
  int pb         = atoi(argv[4]);
  int fb         = atoi(argv[5]);
  int mc         = atoi(argv[6]);
  int mf         = atoi(argv[7]);
  int algorithm  = atoi(argv[8]);

  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*)malloc(cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g=(unsigned char*)realloc(in,cap); if(!g) return 3; in=g; }
    size_t n = fread(in+len, 1, cap-len, stdin);
    if (n == 0) break;
    len += n;
  }

  Buffers b = { in, len, 0, NULL, 0, 0 };
  // hashSize is passed as 0 (the SDK default): this pinned C_LZMA.cpp does not
  // forward it into CLzmaEncProps at all, which is worth knowing given the
  // readme claims a hashSize change.
  int rc = lzma_compress(dictSize, 0, algorithm, fb, mf, mc, pb, lc, lp,
                         io_callback, &b);
  if (rc != FREEARC_OK) { fprintf(stderr, "lzma_compress failed: %d\n", rc); return 1; }
  if (b.out_len) fwrite(b.out, 1, b.out_len, stdout);
  free(in); free(b.out);
  return 0;
}
