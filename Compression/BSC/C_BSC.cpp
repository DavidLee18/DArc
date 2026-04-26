/*-------------------------------------------------*/
/* DArc streaming wrapper around libbsc.           */
/*                                                 */
/* libbsc: (c) 2009-2025 Ilya Grebnov, Apache 2.0  */
/* Wrapper: part of DArc, LGPL (same as GRZip).    */
/*                                                 */
/* Wire layout: a sequence of compressed blocks.    */
/* Each block: 4-byte LE header size (always 28)   */
/* + 4-byte LE payload size + libbsc 28-byte header*/
/* + payload.                                      */
/*                                                 */
/* Actually we store a single 4-byte LE size       */
/* followed by (28-byte libbsc header + payload).  */
/* A size of 0 marks end of stream (EOF).          */
/*                                                 */
/* End-of-stream: an empty block (size = 0).       */
/*-------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "C_BSC.h"
}

#include "libbsc/libbsc.h"
#include "libbsc/platform/platform.h"

// Vendored libbsc TUs (compile as part of this object for header-only build).
#include "libbsc/libbsc/libbsc.cpp"
#include "libbsc/bwt/bwt.cpp"
#include "libbsc/bwt/libsais/libsais.c"
// libsais.c does #undef INLINE — restore it for the rest of the TUs.
#if defined(__GNUC__)
  #define INLINE __inline__
#elif defined(_MSC_VER)
  #define INLINE __forceinline
#elif defined(__cplusplus)
  #define INLINE inline
#else
  #define INLINE
#endif
#include "libbsc/coder/coder.cpp"
#include "libbsc/coder/qlfc/qlfc.cpp"
#include "libbsc/coder/qlfc/qlfc_model.cpp"
#include "libbsc/filters/detectors.cpp"
#include "libbsc/filters/preprocessing.cpp"
#include "libbsc/lzp/lzp.cpp"
#include "libbsc/platform/platform.cpp"
#include "libbsc/st/st.cpp"
#include "libbsc/adler32/adler32.cpp"

static int bsc_initialized = 0;
static int ensure_bsc_init(int features)
{
  if (bsc_initialized) return LIBBSC_NO_ERROR;
  int r = bsc_init(features);
  if (r == LIBBSC_NO_ERROR) bsc_initialized = 1;
  return r;
}

// Helpers to read/write a full buffer through the streaming callback.
static int full_read(CALLBACK_FUNC *cb, void *buf, int size, void *aux)
{
  char *p = (char*)buf;  int remaining = size;
  while (remaining > 0) {
    int n = cb("read", p, remaining, aux);
    if (n <= 0) return size - remaining;
    p += n; remaining -= n;
  }
  return size;
}

static int full_write(CALLBACK_FUNC *cb, void *buf, int size, void *aux)
{
  return cb("write", buf, size, aux);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int bsc_stream_compress (int BlockSize,
                         int LzpHashSize,
                         int LzpMinLen,
                         int BlockSorter,
                         int Coder,
                         CALLBACK_FUNC *callback,
                         void *auxdata)
{
  int features = LIBBSC_DEFAULT_FEATURES;
  int err = ensure_bsc_init(features);
  if (err != LIBBSC_NO_ERROR) return FREEARC_ERRCODE_GENERAL;

  unsigned char *inBuf  = (unsigned char*) malloc(BlockSize);
  unsigned char *outBuf = (unsigned char*) malloc(BlockSize + LIBBSC_HEADER_SIZE + 1024);
  if (!inBuf || !outBuf) { free(inBuf); free(outBuf); return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; }

  int result = FREEARC_OK;
  for (;;) {
    int got = full_read(callback, inBuf, BlockSize, auxdata);
    if (got <= 0) {
      // Emit EOF marker (size = 0).
      int zero = 0;
      full_write(callback, &zero, 4, auxdata);
      break;
    }

    int compressed = bsc_compress(inBuf, outBuf, got,
                                  LzpHashSize, LzpMinLen,
                                  BlockSorter, Coder, features);
    if (compressed < LIBBSC_NO_ERROR) {
      // Fallback: store block with libbsc's own store path.
      compressed = bsc_store(inBuf, outBuf, got, features);
      if (compressed < LIBBSC_NO_ERROR) { result = FREEARC_ERRCODE_GENERAL; break; }
    }

    full_write(callback, &compressed, 4, auxdata);
    full_write(callback, outBuf, compressed, auxdata);

    if (got < BlockSize) {
      // Last block — write EOF marker and stop.
      int zero = 0;
      full_write(callback, &zero, 4, auxdata);
      break;
    }
  }

  free(inBuf); free(outBuf);
  return result;
}

#endif  // !FREEARC_DECOMPRESS_ONLY

int bsc_stream_decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  int features = LIBBSC_DEFAULT_FEATURES;
  int err = ensure_bsc_init(features);
  if (err != LIBBSC_NO_ERROR) return FREEARC_ERRCODE_GENERAL;

  unsigned char *inBuf = NULL, *outBuf = NULL;
  int inCap = 0, outCap = 0;
  int result = FREEARC_OK;

  for (;;) {
    int compressed = 0;
    int got = full_read(callback, &compressed, 4, auxdata);
    if (got != 4) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }
    if (compressed == 0) break;  // EOF marker
    if (compressed < LIBBSC_HEADER_SIZE) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    if (compressed > inCap) {
      free(inBuf);
      inCap = compressed;
      inBuf = (unsigned char*) malloc(inCap);
      if (!inBuf) { result = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; break; }
    }

    got = full_read(callback, inBuf, compressed, auxdata);
    if (got != compressed) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    int blockSize = 0, dataSize = 0;
    err = bsc_block_info(inBuf, LIBBSC_HEADER_SIZE, &blockSize, &dataSize, features);
    if (err != LIBBSC_NO_ERROR || blockSize != compressed) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    if (dataSize > outCap) {
      free(outBuf);
      outCap = dataSize;
      outBuf = (unsigned char*) malloc(outCap);
      if (!outBuf) { result = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; break; }
    }

    err = bsc_decompress(inBuf, compressed, outBuf, dataSize, features);
    if (err != LIBBSC_NO_ERROR) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    full_write(callback, outBuf, dataSize, auxdata);
  }

  free(inBuf); free(outBuf);
  return result;
}

/*-------------------------------------------------*/
/* BSC_METHOD                                       */
/*-------------------------------------------------*/

BSC_METHOD::BSC_METHOD()
{
  BlockSize    = 25*mb;
  LzpHashSize  = LIBBSC_DEFAULT_LZPHASHSIZE;
  LzpMinLen    = LIBBSC_DEFAULT_LZPMINLEN;
  BlockSorter  = LIBBSC_DEFAULT_BLOCKSORTER;
  Coder        = LIBBSC_DEFAULT_CODER;
}

int BSC_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bsc_stream_decompress(callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int BSC_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bsc_stream_compress(BlockSize, LzpHashSize, LzpMinLen, BlockSorter, Coder, callback, auxdata);
}

void BSC_METHOD::SetBlockSize (MemSize bs)
{
  if (bs > 0) BlockSize = bs;
}

void BSC_METHOD::ShowCompressionMethod (char *buf)
{
  char bsStr[64];
  showMem (BlockSize, bsStr);
  sprintf (buf, "bsc:%s:b%d:l%d:h%d:c%d", bsStr, BlockSorter, LzpMinLen, LzpHashSize, Coder);
}

#endif  // !FREEARC_DECOMPRESS_ONLY

COMPRESSION_METHOD* parse_BSC (char** parameters)
{
  if (strcmp (parameters[0], "bsc") != 0) return NULL;

  BSC_METHOD *p = new BSC_METHOD;
  int error = 0;

  while (!error && *++parameters) {
    char *param = *parameters;
    switch (*param) {
      case 'b': p->BlockSorter = parseInt (param+1, &error); continue;
      case 'l': p->LzpMinLen   = parseInt (param+1, &error); continue;
      case 'h': p->LzpHashSize = parseInt (param+1, &error); continue;
      case 'c': p->Coder       = parseInt (param+1, &error); continue;
    }
    // Bare number = block size in bytes/KB/MB.
    int tmp = 0;
    p->BlockSize = parseMem (param, &error);
    (void)tmp;
  }
  if (error) { delete p; return NULL; }
  return p;
}

static int BSC_x = AddCompressionMethod (parse_BSC);
