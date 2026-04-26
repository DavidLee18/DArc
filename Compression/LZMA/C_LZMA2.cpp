// C_LZMA2.cpp - DArc interface to LZMA2 (7-Zip 24.09 C API)
//
// Wire format: 1 byte LZMA2 props + standard LZMA2 stream (self-terminating).
// The stream embeds its own end marker (0x00 control byte), so no length prefix.

extern "C" {
#include "C_LZMA2.h"
#include "7z24/Lzma2Enc.h"
#include "7z24/Lzma2Dec.h"
}

#include <string.h>

extern "C" {

// ---------- Allocator ----------
static void *SzAlloc2 (ISzAllocPtr, size_t size) { return size == 0 ? NULL : malloc(size); }
static void  SzFree2  (ISzAllocPtr, void *addr)  { free(addr); }
static const ISzAlloc g_Alloc2 = { SzAlloc2, SzFree2 };

// ---------- Stream wrappers over CALLBACK_FUNC ----------
struct Cb2InStream  { ISeqInStream  vt; CALLBACK_FUNC *callback; void *auxdata; int errcode; };
struct Cb2OutStream { ISeqOutStream vt; CALLBACK_FUNC *callback; void *auxdata; int errcode; };

static SRes Cb2In_Read(ISeqInStreamPtr pp, void *buf, size_t *size)
{
  Cb2InStream *p = (Cb2InStream*)pp;
  size_t want = *size;
  if (want == 0) return SZ_OK;
  SSIZE_T res = p->callback("read", buf, want, p->auxdata);
  if (res < 0) { p->errcode = (int)res; *size = 0; return SZ_ERROR_READ; }
  *size = (size_t)res;
  return SZ_OK;
}

static size_t Cb2Out_Write(ISeqOutStreamPtr pp, const void *buf, size_t size)
{
  Cb2OutStream *p = (Cb2OutStream*)pp;
  if (size == 0) return 0;
  SSIZE_T res = p->callback("write", (void*)buf, size, p->auxdata);
  if (res < 0) { p->errcode = (int)res; return 0; }
  return (size_t)res;
}

} // extern "C"

#ifndef FREEARC_DECOMPRESS_ONLY

int lzma2_compress (int dictionarySize,
                    int algorithm,
                    int numFastBytes,
                    int matchFinder,
                    int matchFinderCycles,
                    int posStateBits,
                    int litContextBits,
                    int litPosBits,
                    CALLBACK_FUNC *callback,
                    void *auxdata)
{
  Cb2InStream  inS;  inS.vt.Read = Cb2In_Read;    inS.callback = callback;  inS.auxdata = auxdata;  inS.errcode = 0;
  Cb2OutStream outS; outS.vt.Write = Cb2Out_Write; outS.callback = callback; outS.auxdata = auxdata; outS.errcode = 0;

  CLzma2EncHandle enc = Lzma2Enc_Create(&g_Alloc2, &g_Alloc2);
  if (!enc) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  CLzma2EncProps props;
  Lzma2EncProps_Init(&props);
  props.lzmaProps.dictSize = dictionarySize;
  props.lzmaProps.lc       = litContextBits;
  props.lzmaProps.lp       = litPosBits;
  props.lzmaProps.pb       = posStateBits;
  props.lzmaProps.algo     = algorithm;
  props.lzmaProps.fb       = numFastBytes;
  // Match-finder mapping same as LZMA (kBT2..kHT4)
  enum { kBT2, kBT3, kBT4, kHC4, kHT4 };
  switch (matchFinder) {
    case kBT2: props.lzmaProps.btMode = 1; props.lzmaProps.numHashBytes = 2; break;
    case kBT3: props.lzmaProps.btMode = 1; props.lzmaProps.numHashBytes = 3; break;
    case kBT4: props.lzmaProps.btMode = 1; props.lzmaProps.numHashBytes = 4; break;
    case kHC4: props.lzmaProps.btMode = 0; props.lzmaProps.numHashBytes = 4; break;
    case kHT4: props.lzmaProps.btMode = 0; props.lzmaProps.numHashBytes = 5; break;
    default:   props.lzmaProps.btMode = 1; props.lzmaProps.numHashBytes = 4; break;
  }
  props.lzmaProps.mc        = matchFinderCycles;
  props.lzmaProps.writeEndMark = 0;
  props.numTotalThreads     = GetCompressionThreads();
  props.numBlockThreads_Max = GetCompressionThreads();

  SRes r = Lzma2Enc_SetProps(enc, &props);
  if (r != SZ_OK) {
    Lzma2Enc_Destroy(enc);
    return r == SZ_ERROR_MEM ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                             : FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }

  Byte prop = Lzma2Enc_WriteProperties(enc);
  SSIZE_T wrote = callback("write", &prop, 1, auxdata);
  if (wrote < 0) { Lzma2Enc_Destroy(enc); return (int)wrote; }

  r = Lzma2Enc_Encode2(enc, &outS.vt, NULL, NULL, &inS.vt, NULL, 0, NULL);
  Lzma2Enc_Destroy(enc);

  if (inS.errcode)  return inS.errcode;
  if (outS.errcode) return outS.errcode;
  if (r == SZ_ERROR_MEM) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  if (r != SZ_OK) return FREEARC_ERRCODE_GENERAL;
  return FREEARC_OK;
}

#endif

int lzma2_decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  Byte prop;
  SSIZE_T got = callback("read", &prop, 1, auxdata);
  if (got < 0) return (int)got;
  if (got != 1) return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;

  CLzma2Dec dec;
  Lzma2Dec_Construct(&dec);
  SRes r = Lzma2Dec_Allocate(&dec, prop, &g_Alloc2);
  if (r != SZ_OK)
    return r == SZ_ERROR_MEM ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                             : FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
  Lzma2Dec_Init(&dec);

  const size_t IN_BUF  = 1 << 16;
  const size_t OUT_BUF = 1 << 16;
  Byte *inBuf  = (Byte*)malloc(IN_BUF);
  Byte *outBuf = (Byte*)malloc(OUT_BUF);
  if (!inBuf || !outBuf) {
    free(inBuf); free(outBuf);
    Lzma2Dec_Free(&dec, &g_Alloc2);
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }

  size_t inAvail = 0, inPos = 0;
  int rc = FREEARC_OK;

  for (;;) {
    if (inPos == inAvail) {
      SSIZE_T n = callback("read", inBuf, IN_BUF, auxdata);
      if (n < 0) { rc = (int)n; break; }
      inAvail = (size_t)n;
      inPos   = 0;
    }

    SizeT outLen = OUT_BUF;
    SizeT srcLen = inAvail - inPos;
    ELzmaStatus status;
    r = Lzma2Dec_DecodeToBuf(&dec, outBuf, &outLen,
                             inBuf + inPos, &srcLen,
                             LZMA_FINISH_ANY, &status);
    inPos += srcLen;

    if (r != SZ_OK) {
      rc = (r == SZ_ERROR_MEM) ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                               : FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }

    if (outLen > 0) {
      SSIZE_T w = callback("write", outBuf, outLen, auxdata);
      if (w < 0) { rc = (int)w; break; }
    }

    if (status == LZMA_STATUS_FINISHED_WITH_MARK) break;
    if (status == LZMA_STATUS_NEEDS_MORE_INPUT && inAvail == 0) {
      rc = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }
    if (outLen == 0 && srcLen == 0) {
      rc = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }
  }

  free(inBuf);
  free(outBuf);
  Lzma2Dec_Free(&dec, &g_Alloc2);
  return rc;
}


/*-------------------------------------------------*/
/*  LZMA2_METHOD class implementation              */
/*-------------------------------------------------*/

static char* start_from2 (char* str, const char* start)
{
  while (*start && *str==*start)  str++, start++;
  return *start? NULL : str;
}

static const char *kMF2[] = { "BT2","BT3","BT4","HC4","HT4" };
static int FindMF2 (const char *s) {
  for (int i=0; i<5; i++) if (!strcasecmp(kMF2[i], s)) return i;
  return -1;
}

LZMA2_METHOD::LZMA2_METHOD()
{
  dictionarySize    = 64*mb;
  algorithm         = 1;
  numFastBytes      = 32;
  matchFinder       = 4;   // kHT4
  matchFinderCycles = 0;
  posStateBits      = 2;
  litContextBits    = 3;
  litPosBits        = 0;
}

int LZMA2_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return lzma2_decompress (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int LZMA2_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return lzma2_compress (dictionarySize, algorithm, numFastBytes, matchFinder,
                         matchFinderCycles, posStateBits, litContextBits, litPosBits,
                         callback, auxdata);
}

MemSize LZMA2_METHOD::GetCompressionMem (void)
{
  MemSize dict = dictionarySize;
  MemSize divisor = (matchFinder <= 2) ? 11 : (matchFinder == 3 ? 7 : 6);
  return dict * divisor + 8*mb;
}

MemSize LZMA2_METHOD::GetDecompressionMem (void) { return dictionarySize + 2*mb; }

void LZMA2_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem < 2*mb) mem = 2*mb;
  MemSize base = 8*mb;
  MemSize avail = mem > base ? mem - base : mem;
  MemSize divisor = (matchFinder <= 2) ? 11 : (matchFinder == 3 ? 7 : 6);
  dictionarySize = avail / divisor;
  if (dictionarySize < 4*kb) dictionarySize = 4*kb;
}

void LZMA2_METHOD::SetDecompressionMem (MemSize mem)
{
  if (mem > 2*mb) dictionarySize = mem - 2*mb;
}

void LZMA2_METHOD::SetDictionary (MemSize dict) { if (dict) dictionarySize = dict; }

void LZMA2_METHOD::ShowCompressionMethod (char *buf)
{
  LZMA2_METHOD d; char dstr[100]; showMem(dictionarySize, dstr);
  char p[400]; p[0]='\0';
  if (algorithm       != d.algorithm)       sprintf(p+strlen(p), ":a%d", algorithm);
  if (numFastBytes    != d.numFastBytes)    sprintf(p+strlen(p), ":fb%d", numFastBytes);
  if (matchFinder     != d.matchFinder)     sprintf(p+strlen(p), ":mf=%s", kMF2[matchFinder]);
  if (matchFinderCycles!=d.matchFinderCycles) sprintf(p+strlen(p), ":mc%d", matchFinderCycles);
  if (posStateBits    != d.posStateBits)    sprintf(p+strlen(p), ":pb%d", posStateBits);
  if (litContextBits  != d.litContextBits)  sprintf(p+strlen(p), ":lc%d", litContextBits);
  if (litPosBits      != d.litPosBits)      sprintf(p+strlen(p), ":lp%d", litPosBits);
  sprintf(buf, "lzma2:%s%s", dstr, p);
}

#endif

COMPRESSION_METHOD* parse_LZMA2 (char** parameters)
{
  if (strcmp(parameters[0], "lzma2") != 0) return NULL;
  LZMA2_METHOD *p = new LZMA2_METHOD;
  int error = 0;
  char *rest;
  while (*++parameters && !error) {
    char* param = *parameters;
    if (strequ(param,"max"))     { p->algorithm = 1; continue; }
    if (strequ(param,"normal"))  { p->algorithm = 1; continue; }
    if (strequ(param,"fast"))    { p->algorithm = 0; continue; }
    if (strequ(param,"fastest")) { p->algorithm = 0; continue; }
    if (strequ(param,"eos"))     { continue; }
    { int mf = FindMF2(param); if (mf >= 0) { p->matchFinder = mf; continue; } }
    if ((rest = start_from2(param, "mf=")) != NULL) {
      int mf = FindMF2(rest);
      if (mf < 0) { error=1; break; }
      p->matchFinder = mf; continue;
    }
    switch (*param) {
      case 'd': p->dictionarySize = parseMem(param+1, &error); continue;
      case 'a': p->algorithm      = parseInt(param+1, &error); continue;
      case 'p':
        if (param[1]=='b') { p->posStateBits = parseInt(param+2, &error); continue; }
        break;
      case 'l':
        if (param[1]=='c') { p->litContextBits = parseInt(param+2, &error); continue; }
        if (param[1]=='p') { p->litPosBits     = parseInt(param+2, &error); continue; }
        break;
      case 'f':
        if (param[1]=='b') { p->numFastBytes = parseInt(param+2, &error); continue; }
        break;
      case 'm':
        if (param[1]=='c') { p->matchFinderCycles = parseInt(param+2, &error); continue; }
        break;
    }
    if (*param >= '0' && *param <= '9') {
      const char *s = param;
      while (*s >= '0' && *s <= '9') s++;
      if (*s == 'b' || *s == 'k' || *s == 'm' || *s == 'g') {
        MemSize m = parseMem(param, &error);
        if (!error) { p->dictionarySize = m; continue; }
        error = 0;
      }
      int n = parseInt(param, &error);
      if (!error) { p->numFastBytes = n; continue; }
    }
    error = 1;
  }
  if (error) { delete p; return NULL; }
  return p;
}

static int LZMA2_x = AddCompressionMethod (parse_LZMA2);
