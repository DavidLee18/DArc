// Drive DArc's C LZMA DECODER (`lzma_decompress`, Compression/LZMA/C_LZMA.cpp:145)
// over stdin -> stdout, and report what it did in a machine-readable form.
//
// The sibling of lzma_ref.cpp, deliberately sharing its argv shape so a stream
// can be produced and consumed by two drivers that differ only in direction.
//
//     argv: dictSize lc lp pb fb mc matchFinder algorithm [readChunk]
//
// The first eight are lzma_ref.cpp's, unchanged, and are passed to
// `lzma_decompress` through the same permutation `LZMA_METHOD::decompress` uses
// (C_LZMA.cpp:264): (dictSize, hashSize=0, algorithm, fb, mf, mc, pb, lc, lp).
// ONLY FOUR OF THE NINE ARE READ on the decode path -- dictionarySize,
// posStateBits, litContextBits, litPosBits, which C_LZMA.cpp:158 folds into the
// 5-byte properties blob. The other five are accepted and ignored, exactly as
// DArc accepts and ignores them; keeping them in argv is what lets a harness
// hand the same eight fields to the encoder and the decoder.
//
// `readChunk` is the one addition, and it is not cosmetic. `lzma_decompress`
// reads through a 64 KiB buffer (C_LZMA.cpp:169), so the bytes it takes FROM THE
// CALLBACK are not the bytes the LZMA decoder CONSUMES: at EOPM it stops mid-
// buffer and the surplus is already gone from the stream. With readChunk=1 the
// callback hands over one byte at a time, so `consumed` below is the decoder's
// true consumption and the end-of-payload-marker position becomes observable
// from outside. readChunk=0 (the default) reproduces DArc's shipped buffering.
//
// ── The report ───────────────────────────────────────────────────────────────
//
// One line on stderr, always, success or failure:
//
//     DARC_DEC rc=<int> consumed=<u64> produced=<u64> capped=<0|1> maxrss=<i64>
//
//   rc        the FreeArc error code lzma_decompress returned (0 = FREEARC_OK,
//             -2 = INVALID_COMPRESSOR, -5 = NOT_ENOUGH_MEMORY,
//             -7 = BAD_COMPRESSED_DATA -- Compression.h:19-26).
//   consumed  INPUT bytes handed to the decoder through the "read" callback.
//   produced  OUTPUT bytes handed back through the "write" callback.
//   capped    the output cap tripped, so `rc` reflects the harness aborting the
//             stream and not the decoder's own verdict. A corrupt stream can
//             decode unboundedly; without a cap a fuzz corpus fills the disk.
//   maxrss    getrusage(RUSAGE_SELF).ru_maxrss, RAW -- BYTES on Darwin, KiB on
//             Linux. Normalising is the caller's job, because only the caller
//             knows what it is running on. This exists so `dictSize=0xFFFFFFFF`
//             can be checked for what it must not do (allocate 4 GiB) rather
//             than merely for what it returns.
//
// Exit code, which is what a harness should gate on:
//   0  the decoder ACCEPTED the stream (rc == FREEARC_OK)
//   1  the decoder REJECTED it (rc != FREEARC_OK)
//   2  bad usage
//   3  the driver itself ran out of memory
// Anything else -- a signal, in particular -- is the driver crashing, which is a
// finding in its own right and must never be confused with a clean rejection.
//
// Decoded bytes go to stdout, and only there: the report is on stderr so stdout
// stays byte-exact for `cmp`.
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/resource.h>
#include "../../Compression/Compression.h"

// C_LZMA.cpp is compiled INTO this driver, the same way lzma_ref.cpp does it and
// for the same reason: calling DArc's own `lzma_decompress` is the point.
// Re-implementing its properties-blob construction and its 64 KiB double-buffer
// loop here would test my transcription of the decode path, not the decode path.
#include "../../Compression/LZMA/C_LZMA.cpp"

// ── Stubs standing in for CompressionLibrary.cpp ────────────────────────────
// Identical to lzma_ref.cpp's. Including C_LZMA.cpp drags in LZMA_METHOD, which
// needs these three; the driver reaches none of them.
int AddCompressionMethod (CM_PARSER parser) { (void) parser; return 0; }
int GetCompressionThreads (void) { return 1; }
FARPROC LoadFromDLL (char *funcname) { (void) funcname; return NULL; }

int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  (void) what; (void) param; (void) data; (void) callback;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Default 128 MiB. Every legitimate corpus stream here decodes to far less, so
// tripping the cap always means the input was corrupt in a way that made the
// decoder productive rather than unhappy -- worth reporting distinctly.
static size_t out_cap_limit (void)
{
  const char *e = getenv("LZMA_DEC_OUT_CAP");
  if (!e || !*e) return (size_t)128 << 20;
  long long v = atoll(e);
  return v > 0 ? (size_t)v : (size_t)128 << 20;
}

struct Buffers {
  const unsigned char *in;
  size_t in_len, in_pos;
  size_t read_chunk;            // 0 = give the decoder whatever it asks for
  unsigned char *out;
  size_t out_len, out_cap;
  size_t out_limit;
  int    capped;
};

static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return FREEARC_ERRCODE_GENERAL;

  if (strcmp(what,"read")==0) {
    size_t want = (size_t) size;
    if (b->read_chunk && want > b->read_chunk) want = b->read_chunk;
    size_t avail = b->in_len - b->in_pos;
    size_t n = want < avail ? want : avail;
    memcpy(data, b->in + b->in_pos, n);
    b->in_pos += n;
    return (int) n;
  }

  if (strcmp(what,"write")==0) {
    if (b->out_len + (size_t)size > b->out_limit) { b->capped = 1; return FREEARC_ERRCODE_GENERAL; }
    if (b->out_len + (size_t)size > b->out_cap) {
      size_t cap = b->out_cap ? b->out_cap : 65536;
      while (cap < b->out_len + (size_t)size) cap *= 2;
      unsigned char *g = (unsigned char*) realloc(b->out, cap);
      if (!g) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out = g; b->out_cap = cap;
    }
    memcpy(b->out + b->out_len, data, (size_t)size);
    b->out_len += (size_t)size;
    return size;
  }

  // lzma_decompress uses "read" and "write" and nothing else -- no "quasiwrite"
  // on the decode path. Returning NOT_IMPLEMENTED for anything else would turn a
  // silent extra verb into a visible failure rather than a wrong answer.
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv) {
  if (argc < 9) {
    fprintf(stderr, "usage: %s dictSize lc lp pb fb mc matchFinder algorithm [readChunk]\n", argv[0]);
    return 2;
  }
  // strtoll, not atoi: the malformed corpus passes dictSize=4294967295, which
  // atoi() would clamp to INT_MAX and quietly test a different number. The cast
  // to int reproduces exactly what DArc's own `int dictionarySize` parameter
  // does with such a value, and encode_props() then casts it back to UInt32.
  int dictSize  = (int) strtoll(argv[1], NULL, 10);
  int lc        = (int) strtoll(argv[2], NULL, 10);
  int lp        = (int) strtoll(argv[3], NULL, 10);
  int pb        = (int) strtoll(argv[4], NULL, 10);
  int fb        = (int) strtoll(argv[5], NULL, 10);
  int mc        = (int) strtoll(argv[6], NULL, 10);
  int mf        = (int) strtoll(argv[7], NULL, 10);
  int algorithm = (int) strtoll(argv[8], NULL, 10);
  size_t chunk  = argc > 9 ? (size_t) strtoull(argv[9], NULL, 10) : 0;

  size_t cap = 1<<16, len = 0;
  unsigned char *in = (unsigned char*) malloc(cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g=(unsigned char*)realloc(in,cap); if(!g) return 3; in=g; }
    size_t n = fread(in+len, 1, cap-len, stdin);
    if (n == 0) break;
    len += n;
  }

  Buffers b;
  b.in = in; b.in_len = len; b.in_pos = 0; b.read_chunk = chunk;
  b.out = NULL; b.out_len = 0; b.out_cap = 0;
  b.out_limit = out_cap_limit(); b.capped = 0;

  // Same permutation as LZMA_METHOD::decompress (C_LZMA.cpp:264). hashSize is 0
  // because that is what LZMA_METHOD's constructor leaves it at (C_LZMA.cpp:250)
  // and the decode path never looks at it anyway.
  int rc = lzma_decompress(dictSize, 0, algorithm, fb, mf, mc, pb, lc, lp,
                           io_callback, &b);

  struct rusage ru;
  long long maxrss = getrusage(RUSAGE_SELF, &ru) == 0 ? (long long) ru.ru_maxrss : -1;

  fprintf(stderr, "DARC_DEC rc=%d consumed=%llu produced=%llu capped=%d maxrss=%lld\n",
          rc,
          (unsigned long long) b.in_pos,
          (unsigned long long) b.out_len,
          b.capped,
          maxrss);
  fflush(stderr);

  if (b.out_len) fwrite(b.out, 1, b.out_len, stdout);
  fflush(stdout);
  free(in); free(b.out);
  return rc == FREEARC_OK ? 0 : 1;
}
