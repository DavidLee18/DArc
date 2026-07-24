/*-------------------------------------------------*/
/* DArc wrapper around Zstandard.                   */
/*                                                 */
/* The codec itself is the `zstd-safe` crate, fetched by cargo -- the vendored
   libzstd tree that used to live beside this file is gone. This wrapper is now
   only DArc's COMPRESSION_METHOD plumbing: parameter parsing, memory
   estimation and the method registration, all forwarding to rust/darc-codecs.

   Unlike the other codecs there is no DARC_NO_RUST fallback here, because
   there is no longer any C to fall back to. */
/*-------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "C_Zstd.h"
}

// zstd_stream_compress / zstd_stream_decompress are defined in
// rust/darc-codecs and declared in C_Zstd.h, which is included inside this
// file's extern "C" block -- so they are the same C-linkage symbols and this
// file simply calls them.
//
// zstd-safe bundles zstd 1.5.7 where this repository used to vendor 1.5.6. The
// frame format is unchanged between them, which rust/darc-codecs/tests/
// zstd_vectors.rs proves against frames the vendored build actually produced,
// rather than taking it from the changelog.
extern "C" {
int    darc_rs_zstd_min_clevel  (void);
int    darc_rs_zstd_max_clevel  (void);
size_t darc_rs_zstd_sizeof_cctx (int level, int windowLog);
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
  size_t est = darc_rs_zstd_sizeof_cctx(Level, WindowLog);
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
  if (p->Level < darc_rs_zstd_min_clevel()) p->Level = darc_rs_zstd_min_clevel();
  if (p->Level > darc_rs_zstd_max_clevel()) p->Level = darc_rs_zstd_max_clevel();
  return p;
}

static int ZSTD_x = AddCompressionMethod (parse_ZSTD);
