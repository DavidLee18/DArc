// Drive DArc's C LZMA2 codec -- `lzma2_compress` (Compression/LZMA/C_LZMA2.cpp:49)
// and `lzma2_decompress` (:112) -- over stdin -> stdout, in either direction, and
// report what happened in a machine-readable form.
//
// The sibling of lzma_ref.cpp / lzma_dec_ref.cpp. Those are two binaries because
// LZMA's two entry points take different argument lists; LZMA2's decoder takes no
// parameters at all, so both directions fit behind one argv shape and a selector.
//
//     argv: dictSize lc lp pb fb mc matchFinder algorithm <direction> [readChunk]
//
//     direction = enc    stdin is plaintext, stdout is the LZMA2 stream
//                        (prop byte first -- see below)
//                 dec    stdin is an LZMA2 stream, stdout is the plaintext
//                 trace  stdin is an LZMA2 stream, stdout is its CHUNK STRUCTURE
//                        in text; nothing is decoded
//
// The first eight fields are lzma_ref.cpp's, unchanged and in the same order, so
// one case description can feed the LZMA driver, the LZMA2 encoder and the LZMA2
// decoder without re-ordering anything. `lzma2_compress` takes them in a
// different order than `lzma_compress` does (dictionarySize, algorithm, fb, mf,
// mc, pb, lc, lp -- no hashSize); the permutation happens in main(), once.
//
// ON THE DECODE PATH ALL EIGHT ARE IGNORED, and that is not sloppiness on this
// driver's part: `lzma2_decompress(callback, auxdata)` has no parameters. An
// LZMA2 stream carries its own dictionary size in the leading prop byte and its
// own lc/lp/pb in each chunk's prop byte, so the decoder is configured entirely
// by the stream. They stay in argv so the harness can hand the same eight fields
// to every direction.
//
// `readChunk` (decode only) caps how many bytes the "read" callback hands over at
// a time. `lzma2_decompress` reads through a 64 KiB buffer (C_LZMA2.cpp:127), so
// the bytes it takes FROM THE CALLBACK are not the bytes the decoder CONSUMES.
// readChunk=1 makes the end-of-stream position observable from outside;
// readChunk=0 (the default) reproduces DArc's shipped buffering.
//
// ── The leading prop byte ────────────────────────────────────────────────────
//
// C_LZMA2.cpp:96-98 writes ONE byte before handing the stream to the SDK:
//
//     Byte prop = Lzma2Enc_WriteProperties(enc);
//     callback("write", &prop, 1, auxdata);
//
// It encodes the dictionary size (Lzma2Enc.c:485-494) and is DArc's own framing,
// not the SDK's. Because it goes through the same "write" callback, it lands in
// this driver's stdout ahead of everything else and is covered by a plain byte
// comparison -- which is the point. A port that got the whole SDK right and this
// byte wrong would produce archives nothing can open.
//
// ── One thread, deliberately ─────────────────────────────────────────────────
//
// C_LZMA2.cpp:86-87 sets `numTotalThreads` and `numBlockThreads_Max` from
// GetCompressionThreads(), and unlike LZMA that is not a no-op: with more than
// one block thread Lzma2EncProps_Normalize (Lzma2Enc.c:305-324) stops using a
// SOLID block and splits the input into blocks of `max(dictSize*4, 1 MiB)`, each
// starting with a dictionary reset. The STREAM ITSELF changes -- different chunk
// boundaries, different control bytes, a different compression ratio.
//
// GetCompressionThreads() is stubbed to 1 here so that axis is isolated rather
// than mixed into every comparison, exactly as lzma_ref.cpp isolates the same
// axis for LZMA. What that costs is stated plainly: this driver measures DArc's
// LZMA2 as it behaves on a single-core machine. DARC_LZMA2_THREADS overrides it
// so the multi-block path can be measured on purpose later, without editing this
// file -- but note that at >1 the encoder is no longer deterministic across
// machines, so it is not a mode a byte-identity gate can use.
//
// Z7_ST is NOT defined for this driver, and that is the second deliberate choice.
// DArc's own Compression/LZMA/makefile builds MtCoder.c, MtDec.c, Threads.c and
// LzFindMt.c and passes no Z7_ST, so the shipped object code is the
// multi-threading-capable one. lzma-gap-check.sh and lzma-decode-check.sh do
// define it, which for LZMA is harmless (C_LZMA.cpp pins numThreads itself), but
// for LZMA2 it would change `LzmaEncProps_Normalize`'s default numThreads from 2
// to 1 (LzmaEnc.c:101-107) and therefore the whole thread arithmetic in
// Lzma2EncProps_Normalize. Building the code DArc ships and pinning the thread
// COUNT is the honest way round.
//
// ── The report ───────────────────────────────────────────────────────────────
//
// One line on stderr, always, success or failure:
//
//     DARC_LZMA2 dir=<enc|dec|trace> rc=<int> consumed=<u64> produced=<u64> capped=<0|1> maxrss=<i64>
//
//   rc        the FreeArc error code (0 = FREEARC_OK, -2 = INVALID_COMPRESSOR,
//             -5 = NOT_ENOUGH_MEMORY, -7 = BAD_COMPRESSED_DATA -- Compression.h).
//             In trace mode, 0 or -7 from this file's own parser.
//   consumed  bytes taken from stdin through the "read" callback.
//   produced  bytes handed back through the "write" callback.
//   capped    the output cap tripped, so `rc` is the harness aborting the stream
//             rather than the decoder's verdict. A corrupt LZMA2 stream can
//             decode unboundedly; without a cap a fuzz corpus fills the disk.
//   maxrss    getrusage(RUSAGE_SELF).ru_maxrss, RAW -- BYTES on Darwin, KiB on
//             Linux. Normalising is the caller's job; only the caller knows what
//             it is running on.
//
// Trace mode adds a second stderr line, which is where the structural assertions
// are counted:
//
//     DARC_LZMA2_TRACE chunks=<n> lzma=<n> copy=<n> mode0=<n> mode1=<n> mode2=<n>
//       mode3=<n> init=<n> initlate=<n> dicresets=<n> badfirst=<n> copymax=<n>
//       copyruns=<n> copyruns_multi=<n> copy_tail_exact=<n>
//       unpack=<u64> pack=<u64> trailing=<n> parsed=<0|1>
//
//   mode1            UNREACHABLE, and gated as such. Lzma2Enc.c:201 computes
//                    `mode = (srcPos == 0) ? 3 : (needInitState ? (needInitProp
//                    ? 2 : 1) : 0)`, and mode 1 needs needInitState true while
//                    needInitProp is false. Both are set true together by
//                    Lzma2EncInt_InitBlock (:106-111) and cleared together at
//                    :214-215, so that combination never occurs. A nonzero count
//                    means this reasoning is wrong and the harness says so.
//
//                    Modes 2 AND 3 do both occur, which corrects the obvious
//                    guess that DArc only ever emits 3 and 0. mode 3 is the
//                    first chunk of the block when that chunk is an LZMA chunk;
//                    mode 2 is the first LZMA chunk when COPY chunks came first,
//                    because a copy chunk leaves needInitState alone (the
//                    `/* needInitState = True; */` at :190 is commented out).
//                    Both were observed: 3 on text, 2 on incompressible input
//                    whose leading blocks were copied.
//   init/initlate    LZMA chunks in mode 2 or 3 (`init`), and how many of those
//                    were NOT the first LZMA chunk (`initlate`). With one SOLID
//                    block there is exactly one block init, so init is 1 for any
//                    stream containing an LZMA chunk and initlate is always 0.
//   dicresets        chunks that reset the dictionary: copy control 1, or LZMA
//                    mode 3. Exactly one per non-empty stream, at chunk 0 --
//                    again a consequence of the single SOLID block. A second one
//                    means the encoder started splitting into blocks, which is
//                    what DARC_LZMA2_THREADS>1 does.
//   badfirst         LZMA chunk payloads whose first byte is not 0x00. Every
//                    chunk gets a fresh RangeEnc_Init, whose first emitted byte
//                    is the range coder's zero cache -- Lzma2Dec.c:414-419
//                    rejects a stream where it is anything else.
//   copymax          the largest copy chunk seen. Bounded ABOVE by roughly 49159
//                    for anything `lzma2_compress` produces, which is why
//                    copy_tail_exact below is expected to be 0 -- see the note
//                    on the 64 KiB split.
//   copy_tail_exact  maximal runs of consecutive copy chunks whose LAST chunk is
//                    exactly LZMA2_COPY_CHUNK_SIZE (64 KiB). Within one copy
//                    block every chunk is 64 KiB except possibly the last
//                    (Lzma2Enc.c:168-191), so a run ending on a full chunk means
//                    the block's unpackSize was an exact multiple -- the case
//                    where `while (unpackSize > 0)` must NOT emit a zero-length
//                    tail chunk.
//
//                    MEASURED RESULT: this is unreachable from lzma2_compress,
//                    and the bound is arithmetic rather than luck.
//                    LzmaEnc_CodeOneBlock stops as soon as
//                    `RangeEnc_GetProcessed + kPackReserve >= maxPackSize`
//                    (LzmaEnc.c:2666-2667) with kPackReserve = 16384 and
//                    maxPackSize = LZMA2_PACK_SIZE_MAX = 65536, so a subblock's
//                    packSize never exceeds ~49157. `useCopyBlock` then needs
//                    either `packSize > (1 << 16)` -- impossible at that bound --
//                    or `packSize + 2 >= unpackSize`, which forces
//                    unpackSize <= ~49159 < 64 KiB. So the `while` loop always
//                    runs exactly once and a copy block is always one chunk.
//                    The harness gates `copymax < 65536` rather than pretending
//                    to cover the split, and reaches the decoder's 64 KiB copy
//                    path with HAND-BUILT streams instead, where it is reachable.
//
// Exit code, which is what a harness should gate on:
//   0  ACCEPTED  (enc: compressed; dec: decoded; trace: parsed to the terminator)
//   1  REJECTED  (a nonzero FreeArc code, or an unparseable stream)
//   2  bad usage
//   3  the driver itself ran out of memory
// Anything else -- a signal, in particular -- is the driver crashing, which is a
// finding in its own right and must never be confused with a clean rejection.
//
// Payload bytes go to stdout and only there; the report is on stderr so stdout
// stays byte-exact for `cmp`.
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/resource.h>
#include "../../Compression/Compression.h"

// C_LZMA2.cpp is compiled INTO this driver, the same way lzma_ref.cpp includes
// C_LZMA.cpp, and for the same reason: calling DArc's own `lzma2_compress` is the
// point. Transcribing its CLzma2EncProps block -- including the leading prop
// byte, the match-finder mapping and writeEndMark=0 -- into this file would
// measure my transcription rather than DArc's configuration.
#include "../../Compression/LZMA/C_LZMA2.cpp"

// ── Stubs standing in for CompressionLibrary.cpp ────────────────────────────
// Including C_LZMA2.cpp drags in LZMA2_METHOD, which needs these; the driver
// calls lzma2_compress / lzma2_decompress directly and reaches none of them.
int AddCompressionMethod (CM_PARSER parser) { (void) parser; return 0; }

FARPROC LoadFromDLL (char *funcname) { (void) funcname; return NULL; }

int COMPRESSION_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  (void) what; (void) param; (void) data; (void) callback;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// One by default -- see the header comment for why this is the load-bearing
// stub in this file rather than a formality.
int GetCompressionThreads (void)
{
  const char *e = getenv("DARC_LZMA2_THREADS");
  if (!e || !*e) return 1;
  int v = atoi(e);
  return v > 0 ? v : 1;
}

// Default 128 MiB. Every legitimate corpus stream decodes to far less, so
// tripping the cap always means the input was corrupt in a way that made the
// decoder productive rather than unhappy -- worth reporting distinctly.
static size_t out_cap_limit (void)
{
  const char *e = getenv("LZMA2_OUT_CAP");
  if (!e || !*e) return (size_t)128 << 20;
  long long v = atoll(e);
  return v > 0 ? (size_t)v : (size_t)128 << 20;
}

struct Buffers {
  const unsigned char *in;
  size_t in_len, in_pos;
  size_t read_chunk;            // 0 = give the codec whatever it asks for
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

  // Neither lzma2_compress nor lzma2_decompress uses any other verb. Returning
  // NOT_IMPLEMENTED turns a silently-added verb into a visible failure rather
  // than a wrong answer.
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// ── The chunk tracer ─────────────────────────────────────────────────────────
//
// An LZMA2 stream's chunking is a pure function of its bytes, so the trace is
// derived by PARSING rather than by instrumenting the SDK. Three things follow
// from that, and all three are why it is done this way:
//
//   * no edit to Compression/ is needed to get the trace out of the C;
//   * the same parser can be pointed at a Rust-produced stream, so a harness
//     gets a like-for-like structural comparison even before a Rust tracer
//     exists, and can cross-check a Rust tracer against this one once it does;
//   * the parser is itself a structural validator. A stream that does not parse
//     to its terminator with nothing left over is malformed whatever its bytes
//     compare equal to.
//
// The framing (Lzma2Dec.c:16-28):
//     00000000                     end of stream
//     00000001 U U                 uncompressed, reset dic
//     00000010 U U                 uncompressed, no reset
//     1MMuuuuu U U P P [S]         LZMA; S present iff control & 0x40
//   sizes are stored biased by one.
#define LZMA2_TRACE_COPY_CHUNK 65536

struct Trace {
  unsigned long long chunks, lzma, copy;
  unsigned long long mode[4];
  unsigned long long init, initlate, dicresets;
  unsigned long long badfirst, copymax;
  unsigned long long copyruns, copyruns_multi, copy_tail_exact;
  unsigned long long unpack_total, pack_total;
  unsigned long long trailing;
  int parsed;
};

static int trace_stream (const unsigned char *in, size_t len, FILE *out, Trace *t)
{
  memset(t, 0, sizeof(*t));
  size_t pos = 0;
  unsigned long long run_len = 0, run_last = 0;

  // Close a maximal run of consecutive copy chunks and score it.
  #define END_COPY_RUN() do { \
      if (run_len) { \
        t->copyruns++; \
        if (run_len > 1) t->copyruns_multi++; \
        if (run_last == LZMA2_TRACE_COPY_CHUNK) t->copy_tail_exact++; \
        run_len = 0; run_last = 0; \
      } \
    } while (0)

  #define NEED(n) do { \
      if (pos + (size_t)(n) > len) { \
        fprintf(out, "TRUNCATED at=%llu need=%d have=%llu\n", \
                (unsigned long long)pos, (int)(n), (unsigned long long)(len - pos)); \
        END_COPY_RUN(); \
        return 0; \
      } \
    } while (0)

  NEED(1);
  fprintf(out, "props %02X\n", (unsigned)in[0]);
  pos = 1;

  for (;;) {
    NEED(1);
    const unsigned ctl = in[pos];
    const unsigned long long ctl_off = pos;
    pos++;

    if (ctl == 0) {
      END_COPY_RUN();
      fprintf(out, "end off=%llu chunks=%llu unpack=%llu pack=%llu\n",
              ctl_off, t->chunks, t->unpack_total, t->pack_total);
      t->trailing = (unsigned long long)(len - pos);
      t->parsed = 1;
      return 1;
    }

    if ((ctl & 0x80) == 0) {
      // Uncompressed (copy) chunk.
      if (ctl > 2) {
        END_COPY_RUN();
        fprintf(out, "BADCONTROL off=%llu ctl=%02X\n", ctl_off, ctl);
        return 0;
      }
      NEED(2);
      unsigned long long u = ((unsigned long long)in[pos] << 8 | in[pos+1]) + 1;
      pos += 2;
      NEED(u);
      fprintf(out, "%llu ctl=%02X kind=copy resetdic=%d unpack=%llu pack=%llu\n",
              t->chunks, ctl, ctl == 1 ? 1 : 0, u, u);
      pos += (size_t)u;
      t->chunks++; t->copy++;
      if (ctl == 1) t->dicresets++;
      if (u > t->copymax) t->copymax = u;
      t->unpack_total += u; t->pack_total += u;
      run_len++; run_last = u;
      continue;
    }

    // LZMA chunk.
    END_COPY_RUN();
    const unsigned mode = (ctl >> 5) & 3;
    NEED(4);
    unsigned long long u = (((unsigned long long)(ctl & 0x1F) << 16)
                            | ((unsigned long long)in[pos] << 8)
                            | in[pos+1]) + 1;
    unsigned long long p = (((unsigned long long)in[pos+2] << 8) | in[pos+3]) + 1;
    pos += 4;
    int prop = -1;
    if (ctl & 0x40) { NEED(1); prop = in[pos]; pos++; }
    NEED(p);
    const unsigned first = in[pos];
    if (first != 0) t->badfirst++;
    if (prop >= 0)
      fprintf(out, "%llu ctl=%02X kind=lzma mode=%u unpack=%llu pack=%llu prop=%02X first=%02X\n",
              t->chunks, ctl, mode, u, p, (unsigned)prop, first);
    else
      fprintf(out, "%llu ctl=%02X kind=lzma mode=%u unpack=%llu pack=%llu prop=- first=%02X\n",
              t->chunks, ctl, mode, u, p, first);
    pos += (size_t)p;
    if (mode >= 2) { if (t->lzma != 0) t->initlate++; t->init++; }
    if (mode == 3) t->dicresets++;
    t->chunks++; t->lzma++; t->mode[mode]++;
    t->unpack_total += u; t->pack_total += p;
  }
  #undef NEED
  #undef END_COPY_RUN
}

int main (int argc, char **argv) {
  if (argc < 10) {
    fprintf(stderr, "usage: %s dictSize lc lp pb fb mc matchFinder algorithm "
                    "<enc|dec|trace> [readChunk]\n", argv[0]);
    return 2;
  }
  // strtoll, not atoi: the malformed corpus passes dictSize=4294967295, which
  // atoi() would clamp to INT_MAX and quietly test a different number. The cast
  // to int reproduces what DArc's own `int dictionarySize` parameter does.
  int dictSize  = (int) strtoll(argv[1], NULL, 10);
  int lc        = (int) strtoll(argv[2], NULL, 10);
  int lp        = (int) strtoll(argv[3], NULL, 10);
  int pb        = (int) strtoll(argv[4], NULL, 10);
  int fb        = (int) strtoll(argv[5], NULL, 10);
  int mc        = (int) strtoll(argv[6], NULL, 10);
  int mf        = (int) strtoll(argv[7], NULL, 10);
  int algorithm = (int) strtoll(argv[8], NULL, 10);
  const char *dir = argv[9];
  size_t chunk  = argc > 10 ? (size_t) strtoull(argv[10], NULL, 10) : 0;

  const int do_enc   = strcmp(dir, "enc")   == 0;
  const int do_dec   = strcmp(dir, "dec")   == 0;
  const int do_trace = strcmp(dir, "trace") == 0;
  if (!do_enc && !do_dec && !do_trace) {
    fprintf(stderr, "unknown direction '%s' (want enc, dec or trace)\n", dir);
    return 2;
  }

  size_t cap = 1<<16, len = 0;
  unsigned char *in = (unsigned char*) malloc(cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g=(unsigned char*)realloc(in,cap); if(!g) return 3; in=g; }
    size_t n = fread(in+len, 1, cap-len, stdin);
    if (n == 0) break;
    len += n;
  }

  int rc = FREEARC_OK;
  Buffers b;
  b.in = in; b.in_len = len; b.in_pos = 0; b.read_chunk = chunk;
  b.out = NULL; b.out_len = 0; b.out_cap = 0;
  b.out_limit = out_cap_limit(); b.capped = 0;

  if (do_trace) {
    Trace t;
    // The trace goes to stdout as text; nothing is decoded, so `produced` below
    // is the trace's own byte count and is not comparable with a decode run's.
    int ok = trace_stream(in, len, stdout, &t);
    fflush(stdout);
    rc = ok ? FREEARC_OK : FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    b.in_pos = len;
    fprintf(stderr,
      "DARC_LZMA2_TRACE chunks=%llu lzma=%llu copy=%llu mode0=%llu mode1=%llu "
      "mode2=%llu mode3=%llu init=%llu initlate=%llu dicresets=%llu "
      "badfirst=%llu copymax=%llu copyruns=%llu copyruns_multi=%llu "
      "copy_tail_exact=%llu unpack=%llu pack=%llu trailing=%llu parsed=%d\n",
      t.chunks, t.lzma, t.copy, t.mode[0], t.mode[1], t.mode[2], t.mode[3],
      t.init, t.initlate, t.dicresets,
      t.badfirst, t.copymax, t.copyruns, t.copyruns_multi, t.copy_tail_exact,
      t.unpack_total, t.pack_total, t.trailing, t.parsed);
  } else if (do_enc) {
    // Same argument order as LZMA2_METHOD::compress (C_LZMA2.cpp:222-224).
    // Note it differs from lzma_compress's: no hashSize, and algorithm second.
    rc = lzma2_compress(dictSize, algorithm, fb, mf, mc, pb, lc, lp,
                        io_callback, &b);
  } else {
    // No parameters at all -- everything the decoder needs is in the stream.
    rc = lzma2_decompress(io_callback, &b);
  }

  struct rusage ru;
  long long maxrss = getrusage(RUSAGE_SELF, &ru) == 0 ? (long long) ru.ru_maxrss : -1;

  fprintf(stderr, "DARC_LZMA2 dir=%s rc=%d consumed=%llu produced=%llu capped=%d maxrss=%lld\n",
          dir, rc,
          (unsigned long long) b.in_pos,
          (unsigned long long) b.out_len,
          b.capped,
          maxrss);
  fflush(stderr);

  if (!do_trace && b.out_len) fwrite(b.out, 1, b.out_len, stdout);
  fflush(stdout);
  free(in); free(b.out);
  return rc == FREEARC_OK ? 0 : 1;
}
