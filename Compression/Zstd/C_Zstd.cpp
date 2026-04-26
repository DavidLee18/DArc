/*-------------------------------------------------*/
/* DArc streaming wrapper around zstd 1.5.6.        */
/*                                                 */
/* zstd: (c) Meta Platforms, Inc. BSD-3-Clause.    */
/*-------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "C_Zstd.h"
}

#include "libzstd/zstd.h"

static const size_t ZSTD_IN_BUFSZ  = 1 << 17;  // 128 KiB
static const size_t ZSTD_OUT_BUFSZ = 1 << 17;

#ifndef FREEARC_DECOMPRESS_ONLY

int zstd_stream_compress (int Level, int WindowLog, int Workers,
                          CALLBACK_FUNC *callback, void *auxdata)
{
  ZSTD_CCtx *cctx = ZSTD_createCCtx();
  if (!cctx) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  if (ZSTD_isError(ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, Level))) {
    ZSTD_freeCCtx(cctx); return FREEARC_ERRCODE_GENERAL;
  }
  if (WindowLog > 0) {
    ZSTD_CCtx_setParameter(cctx, ZSTD_c_windowLog, WindowLog);
    ZSTD_CCtx_setParameter(cctx, ZSTD_c_enableLongDistanceMatching, 1);
  }
  if (Workers > 0) {
    ZSTD_CCtx_setParameter(cctx, ZSTD_c_nbWorkers, Workers);
  }

  void *inBuf  = malloc(ZSTD_IN_BUFSZ);
  void *outBuf = malloc(ZSTD_OUT_BUFSZ);
  if (!inBuf || !outBuf) { free(inBuf); free(outBuf); ZSTD_freeCCtx(cctx); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; }

  int result = FREEARC_OK;
  int finished = 0;
  while (!finished) {
    int got = callback("read", inBuf, (int)ZSTD_IN_BUFSZ, auxdata);
    if (got < 0) { result = got; break; }

    ZSTD_inBuffer  in  = { inBuf,  (size_t)got, 0 };
    ZSTD_EndDirective mode = (got == 0) ? ZSTD_e_end : ZSTD_e_continue;

    int drained = 0;
    while (!drained) {
      ZSTD_outBuffer out = { outBuf, ZSTD_OUT_BUFSZ, 0 };
      size_t remaining = ZSTD_compressStream2(cctx, &out, &in, mode);
      if (ZSTD_isError(remaining)) { result = FREEARC_ERRCODE_GENERAL; goto done; }

      if (out.pos > 0) {
        int w = callback("write", outBuf, (int)out.pos, auxdata);
        if (w < 0) { result = w; goto done; }
      }

      if (mode == ZSTD_e_end) {
        drained = (remaining == 0);
      } else {
        drained = (in.pos == in.size);
      }
    }

    if (got == 0) finished = 1;
  }

done:
  free(inBuf); free(outBuf); ZSTD_freeCCtx(cctx);
  return result;
}

#endif  // !FREEARC_DECOMPRESS_ONLY

int zstd_stream_decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  ZSTD_DCtx *dctx = ZSTD_createDCtx();
  if (!dctx) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  void *inBuf  = malloc(ZSTD_IN_BUFSZ);
  void *outBuf = malloc(ZSTD_OUT_BUFSZ);
  if (!inBuf || !outBuf) { free(inBuf); free(outBuf); ZSTD_freeDCtx(dctx); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; }

  int result = FREEARC_OK;
  size_t last_ret = 0;
  for (;;) {
    int got = callback("read", inBuf, (int)ZSTD_IN_BUFSZ, auxdata);
    if (got < 0) { result = got; break; }
    if (got == 0) {
      if (last_ret != 0) result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }

    ZSTD_inBuffer in = { inBuf, (size_t)got, 0 };
    while (in.pos < in.size) {
      ZSTD_outBuffer out = { outBuf, ZSTD_OUT_BUFSZ, 0 };
      last_ret = ZSTD_decompressStream(dctx, &out, &in);
      if (ZSTD_isError(last_ret)) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; goto done; }
      if (out.pos > 0) {
        int w = callback("write", outBuf, (int)out.pos, auxdata);
        if (w < 0) { result = w; goto done; }
      }
    }
  }

done:
  free(inBuf); free(outBuf); ZSTD_freeDCtx(dctx);
  return result;
}

/*-------------------------------------------------*/
/* ZSTD_METHOD                                     */
/*-------------------------------------------------*/

ZSTD_METHOD::ZSTD_METHOD()
{
  Level     = 3;
  WindowLog = 0;
  Workers   = 0;
}

int ZSTD_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return zstd_stream_decompress(callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int ZSTD_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return zstd_stream_compress(Level, WindowLog, Workers, callback, auxdata);
}

MemSize ZSTD_METHOD::GetCompressionMem (void)
{
  // Rough upper bound: zstd level-22 with default window can use ~256 MiB per thread.
  // Use the library's own estimate when possible via a transient context.
  ZSTD_CCtx *c = ZSTD_createCCtx();
  if (!c) return 64*mb;
  ZSTD_CCtx_setParameter(c, ZSTD_c_compressionLevel, Level);
  if (WindowLog > 0) ZSTD_CCtx_setParameter(c, ZSTD_c_windowLog, WindowLog);
  size_t est = ZSTD_sizeof_CCtx(c);
  ZSTD_freeCCtx(c);
  if (Workers > 0) est = est * (Workers+1);
  return (MemSize)(est ? est : 64*mb);
}

void ZSTD_METHOD::SetCompressionMem (MemSize mem)
{
  // Map available memory heuristically onto windowLog. zstd's own default
  // window grows with level so we only override when the user explicitly
  // constrains memory.
  if (mem == 0) return;
  int wl = 10;
  while (wl < 27 && (MemSize)((size_t)1 << wl) * 4 < mem) wl++;
  WindowLog = wl;
}

void ZSTD_METHOD::ShowCompressionMethod (char *buf)
{
  char extras[64] = "";
  char *p = extras;
  if (WindowLog > 0) { p += sprintf(p, ":long%d", WindowLog); }
  if (Workers   > 0) { p += sprintf(p, ":w%d",    Workers);   }
  sprintf (buf, "zstd:%d%s", Level, extras);
}

MemSize ZSTD_METHOD::GetDecompressionMem (void)
{
  // Decompression memory is dominated by the window size. When LDM is off,
  // zstd picks a window based on level; when on, we know WindowLog.
  int wl = WindowLog > 0 ? WindowLog : 23;  // zstd's max default window ~8 MiB .. 128 MiB
  size_t est = ((size_t)1 << wl) + (128 << 10);
  return (MemSize)est;
}

#endif  // !FREEARC_DECOMPRESS_ONLY

COMPRESSION_METHOD* parse_ZSTD (char** parameters)
{
  if (strcmp (parameters[0], "zstd") != 0) return NULL;

  ZSTD_METHOD *p = new ZSTD_METHOD;
  int error = 0;

  while (!error && *++parameters) {
    char *param = *parameters;
    if (strncmp(param, "long", 4) == 0) {
      p->WindowLog = parseInt(param+4, &error);
      if (p->WindowLog == 0) p->WindowLog = 27;   // "long" alone enables LDM with w=27
      continue;
    }
    if (param[0] == 'w') {
      p->Workers = parseInt(param+1, &error);
      continue;
    }
    // Bare number = level.
    int lvl = parseInt(param, &error);
    if (!error && lvl != 0) p->Level = lvl;
  }
  if (error) { delete p; return NULL; }

  // Clamp to zstd's advertised range.
  if (p->Level < ZSTD_minCLevel()) p->Level = ZSTD_minCLevel();
  if (p->Level > ZSTD_maxCLevel()) p->Level = ZSTD_maxCLevel();
  return p;
}

static int ZSTD_x = AddCompressionMethod (parse_ZSTD);
