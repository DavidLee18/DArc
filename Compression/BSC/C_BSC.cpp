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

// The vendored libbsc is gone: BSC is ported to Rust in BOTH directions and
// every stage is byte-identical to the C it replaces, proven per stage by the
// harnesses in rust/difftest (LZP, QLFC transform, all three QLFC coders, the
// coder layer, ST3..ST6, the forward BWT, and bsc_compress end to end).
//
// Only these constants were ever used from libbsc.h, so they are inlined here
// rather than keeping a header for six defines. Values read from the original
// libbsc.h, not inferred:
//
//   LIBBSC_NO_ERROR 0, LIBBSC_HEADER_SIZE 28, LZPHASHSIZE 15, LZPMINLEN 72,
//   BLOCKSORTER_BWT 1, CODER_QLFC_STATIC 1.
//
// LIBBSC_DEFAULT_FEATURES was FASTMODE|MULTITHREADING (3). It is dropped, not
// inlined: the only paths that consulted it were OpenMP-gated, and this tree
// never defines LIBBSC_OPENMP_SUPPORT. Verified rather than assumed --
// bsc_compress at features=0 and features=3 produces byte-identical output over
// 240 sorter/coder/LZP combinations, so the Rust port taking no features
// argument loses nothing.
#define LIBBSC_NO_ERROR                0
#define LIBBSC_HEADER_SIZE             28
#define LIBBSC_DEFAULT_LZPHASHSIZE     15
#define LIBBSC_DEFAULT_LZPMINLEN       72
#define LIBBSC_DEFAULT_BLOCKSORTER     1   /* BWT */
#define LIBBSC_DEFAULT_CODER           1   /* QLFC static */

extern "C" {
int darc_rs_bsc_decompress_block (const unsigned char *input, int inSize, unsigned char *output, int outCap);
int darc_rs_bsc_compress (const unsigned char *input, int inSize, unsigned char *output, int outSize,
                          int lzpHashSize, int lzpMinLen, int blockSorter, int coder);
int darc_rs_bsc_store (const unsigned char *input, unsigned char *output, int n);
int darc_rs_bsc_block_info (const unsigned char *header, int headerSize, int *blockSize, int *dataSize);
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

    int compressed = darc_rs_bsc_compress(inBuf, got, outBuf,
                                          BlockSize + LIBBSC_HEADER_SIZE + 1024,
                                          LzpHashSize, LzpMinLen,
                                          BlockSorter, Coder);
    if (compressed < LIBBSC_NO_ERROR) {
      // Fallback: frame the block uncompressed. bsc_compress already stores
      // internally when a block will not compress, so this only catches a
      // genuine refusal (ST7/ST8, which have no CPU encoder in the C either).
      compressed = darc_rs_bsc_store(inBuf, outBuf, got);
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
  // No init step: the Rust port has no global tables to build, so what
  // bsc_init(features) used to set up (platform, coder, bwt, st) is gone.
  int err = LIBBSC_NO_ERROR;

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
    err = darc_rs_bsc_block_info(inBuf, LIBBSC_HEADER_SIZE, &blockSize, &dataSize);
    if (err != LIBBSC_NO_ERROR || blockSize != compressed) { result = FREEARC_ERRCODE_BAD_COMPRESSED_DATA; break; }

    if (dataSize > outCap) {
      free(outBuf);
      outCap = dataSize;
      outBuf = (unsigned char*) malloc(outCap);
      if (!outBuf) { result = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; break; }
    }

    err = darc_rs_bsc_decompress_block(inBuf, compressed, outBuf, dataSize);
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
