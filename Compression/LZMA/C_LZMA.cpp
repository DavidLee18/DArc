// C_LZMA.cpp - FreeArc/DArc interface to LZMA (7-Zip 24.09 C API)
//
// Wire format: raw LZMA1 stream WITHOUT 5-byte properties header.
// Properties (dictSize, lc, lp, pb) are carried out-of-band in the method string,
// preserving byte-level compatibility with FreeArc 0.51-era archives.

extern "C" {
#include "C_LZMA.h"
#include "7z24/LzmaEnc.h"
#include "7z24/LzmaDec.h"
}

#include <string.h>

// Old FreeArc match-finder IDs (kept for CLI compatibility)
enum { kBT2, kBT3, kBT4, kHC4, kHT4 };

static const char *kMatchFinderIDs[] = { "BT2", "BT3", "BT4", "HC4", "HT4" };

static int FindMatchFinder(const char *s)
{
  for (int m = 0; m < (int)(sizeof(kMatchFinderIDs) / sizeof(kMatchFinderIDs[0])); m++)
    if (!strcasecmp(kMatchFinderIDs[m], s))
      return m;
  return -1;
}

extern "C" {

// ---------- Allocator (malloc/free) ----------
static void *SzAlloc (ISzAllocPtr, size_t size) { return size == 0 ? NULL : malloc(size); }
static void  SzFree  (ISzAllocPtr, void *addr)  { free(addr); }
static const ISzAlloc g_Alloc = { SzAlloc, SzFree };

// ---------- Stream wrappers over CALLBACK_FUNC ----------
struct CbInStream {
  ISeqInStream vt;
  CALLBACK_FUNC *callback;
  void *auxdata;
  int errcode;
};

struct CbOutStream {
  ISeqOutStream vt;
  CALLBACK_FUNC *callback;
  void *auxdata;
  int errcode;
};

static SRes CbIn_Read(ISeqInStreamPtr pp, void *buf, size_t *size)
{
  CbInStream *p = (CbInStream*)pp;
  size_t want = *size;
  if (want == 0) return SZ_OK;
  SSIZE_T res = p->callback("read", buf, want, p->auxdata);
  if (res < 0) { p->errcode = (int)res; *size = 0; return SZ_ERROR_READ; }
  *size = (size_t)res;
  return SZ_OK;
}

static size_t CbOut_Write(ISeqOutStreamPtr pp, const void *buf, size_t size)
{
  CbOutStream *p = (CbOutStream*)pp;
  if (size == 0) return 0;
  SSIZE_T res = p->callback("write", (void*)buf, size, p->auxdata);
  if (res < 0) { p->errcode = (int)res; return 0; }
  return (size_t)res;
}

} // extern "C"

#ifndef FREEARC_DECOMPRESS_ONLY

int lzma_compress (int dictionarySize,
                   int hashSize,
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
  CbInStream  inS;  inS.vt.Read  = CbIn_Read;  inS.callback = callback; inS.auxdata = auxdata; inS.errcode = 0;
  CbOutStream outS; outS.vt.Write = CbOut_Write; outS.callback = callback; outS.auxdata = auxdata; outS.errcode = 0;

  CLzmaEncHandle enc = LzmaEnc_Create(&g_Alloc);
  if (!enc) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

  CLzmaEncProps props;
  LzmaEncProps_Init(&props);
  props.dictSize     = dictionarySize;
  props.lc           = litContextBits;
  props.lp           = litPosBits;
  props.pb           = posStateBits;
  props.algo         = algorithm;      // 0 = fast, 1 = normal
  props.fb           = numFastBytes;
  // Old FreeArc match-finder id -> 7zip (btMode, numHashBytes)
  switch (matchFinder) {
    case kBT2: props.btMode = 1; props.numHashBytes = 2; break;
    case kBT3: props.btMode = 1; props.numHashBytes = 3; break;
    case kBT4: props.btMode = 1; props.numHashBytes = 4; break;
    case kHC4: props.btMode = 0; props.numHashBytes = 4; break;
    case kHT4: props.btMode = 0; props.numHashBytes = 5; break;  // map to 5-byte hash
    default:   props.btMode = 1; props.numHashBytes = 4; break;
  }
  props.mc           = matchFinderCycles;  // 0 = auto
  props.writeEndMark = 1;                  // FreeArc streams with EOPM (unknown size)
  props.numThreads   = GetCompressionThreads() > 1 ? 2 : 1;

  SRes r = LzmaEnc_SetProps(enc, &props);
  if (r != SZ_OK) {
    LzmaEnc_Destroy(enc, &g_Alloc, &g_Alloc);
    return r == SZ_ERROR_MEM ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                             : FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }

  r = LzmaEnc_Encode(enc, &outS.vt, &inS.vt, NULL, &g_Alloc, &g_Alloc);
  LzmaEnc_Destroy(enc, &g_Alloc, &g_Alloc);

  if (inS.errcode)  return inS.errcode;
  if (outS.errcode) return outS.errcode;
  if (r == SZ_ERROR_MEM) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  if (r != SZ_OK) return FREEARC_ERRCODE_GENERAL;
  return FREEARC_OK;
}

#endif // !FREEARC_DECOMPRESS_ONLY

// Encode 5-byte LZMA properties blob from FreeArc params.
//   byte 0 = (pb*5 + lp)*9 + lc
//   bytes 1..4 = dictSize (little-endian UInt32)
static void encode_props(Byte *props, int dictSize, int pb, int lc, int lp)
{
  props[0] = (Byte)((pb * 5 + lp) * 9 + lc);
  UInt32 d = (UInt32)dictSize;
  props[1] = (Byte)(d);
  props[2] = (Byte)(d >> 8);
  props[3] = (Byte)(d >> 16);
  props[4] = (Byte)(d >> 24);
}

int lzma_decompress (int dictionarySize,
                     int hashSize,
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
  Byte propsBuf[LZMA_PROPS_SIZE];
  encode_props(propsBuf, dictionarySize, posStateBits, litContextBits, litPosBits);

  CLzmaDec dec;
  LzmaDec_Construct(&dec);
  SRes r = LzmaDec_Allocate(&dec, propsBuf, LZMA_PROPS_SIZE, &g_Alloc);
  if (r != SZ_OK) {
    return r == SZ_ERROR_MEM ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                             : FREEARC_ERRCODE_INVALID_COMPRESSOR;
  }
  LzmaDec_Init(&dec);

  const size_t IN_BUF  = 1 << 16;
  const size_t OUT_BUF = 1 << 16;
  Byte *inBuf  = (Byte*)malloc(IN_BUF);
  Byte *outBuf = (Byte*)malloc(OUT_BUF);
  if (!inBuf || !outBuf) {
    free(inBuf); free(outBuf);
    LzmaDec_Free(&dec, &g_Alloc);
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }

  size_t inAvail = 0;
  size_t inPos   = 0;
  int rc = FREEARC_OK;
  int finished = 0;

  for (;;)
  {
    if (inPos == inAvail) {
      SSIZE_T got = callback("read", inBuf, IN_BUF, auxdata);
      if (got < 0) { rc = (int)got; break; }
      inAvail = (size_t)got;
      inPos   = 0;
    }

    SizeT outLen = OUT_BUF;
    SizeT srcLen = inAvail - inPos;
    ELzmaStatus status;
    r = LzmaDec_DecodeToBuf(&dec, outBuf, &outLen,
                            inBuf + inPos, &srcLen,
                            LZMA_FINISH_ANY, &status);
    inPos += srcLen;

    if (r != SZ_OK) {
      rc = (r == SZ_ERROR_MEM) ? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY
                               : FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }

    if (outLen > 0) {
      SSIZE_T wrote = callback("write", outBuf, outLen, auxdata);
      if (wrote < 0) { rc = (int)wrote; break; }
    }

    if (status == LZMA_STATUS_FINISHED_WITH_MARK ||
        status == LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK) {
      finished = 1;
      break;
    }
    if (status == LZMA_STATUS_NEEDS_MORE_INPUT && inAvail == 0) {
      // EOF without end marker and without indicating finished — error.
      rc = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }
    if (outLen == 0 && srcLen == 0) {
      // No progress: avoid infinite loop.
      rc = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
      break;
    }
  }

  (void)finished;
  free(inBuf);
  free(outBuf);
  LzmaDec_Free(&dec, &g_Alloc);
  return rc;
}


/*-------------------------------------------------*/
/*  LZMA_METHOD class implementation               */
/*-------------------------------------------------*/

static char* start_from (char* str, const char* start)
{
  while (*start && *str==*start)  str++, start++;
  return *start? NULL : str;
}

LZMA_METHOD::LZMA_METHOD()
{
  dictionarySize    = 64*mb;
  hashSize          = 0;
  algorithm         = 1;
  numFastBytes      = 32;
  matchFinder       = kHT4;
  matchFinderCycles = 0;
  posStateBits      = 2;
  litContextBits    = 3;
  litPosBits        = 0;
}

int LZMA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  static FARPROC f = LoadFromDLL ("lzma_decompress");
  if (!f) f = (FARPROC) lzma_decompress;
  return ((int (*)(int,int,int,int,int,int,int,int,int, CALLBACK_FUNC*, void*)) f)
           (dictionarySize, hashSize, algorithm, numFastBytes, matchFinder,
            matchFinderCycles, posStateBits, litContextBits, litPosBits,
            callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int LZMA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  static FARPROC f = LoadFromDLL ("lzma_compress");
  if (!f) f = (FARPROC) lzma_compress;
  return ((int (*)(int,int,int,int,int,int,int,int,int, CALLBACK_FUNC*, void*)) f)
           (dictionarySize, hashSize, algorithm, numFastBytes, matchFinder,
            matchFinderCycles, posStateBits, litContextBits, litPosBits,
            callback, auxdata);
}

// Memory usage formulas (approximate, mirror of 7-Zip docs):
//   dictSize + (numHashBytes < 4 ? 0 : 2^20) + approx
// We use simple safe upper bounds.
MemSize LZMA_METHOD::GetCompressionMem (void)
{
  MemSize dict = dictionarySize;
  MemSize mfMem;
  switch (matchFinder) {
    case kBT2: mfMem = dict * 10; break;
    case kBT3: mfMem = dict * 11; break;
    case kBT4: mfMem = dict * 11; break;
    case kHC4: mfMem = dict * 7;  break;
    case kHT4: mfMem = dict * 6;  break;
    default:   mfMem = dict * 11; break;
  }
  return mfMem + 6*mb;
}

MemSize LZMA_METHOD::GetDecompressionMem (void)
{
  return dictionarySize + 2*mb;
}

void LZMA_METHOD::SetCompressionMem (MemSize mem)
{
  if (mem < 2*mb) mem = 2*mb;
  MemSize base = 6*mb;
  MemSize avail = mem > base ? mem - base : mem;
  MemSize divisor;
  switch (matchFinder) {
    case kBT2: divisor = 10; break;
    case kBT3: divisor = 11; break;
    case kBT4: divisor = 11; break;
    case kHC4: divisor = 7;  break;
    case kHT4: divisor = 6;  break;
    default:   divisor = 11; break;
  }
  dictionarySize = avail / divisor;
  if (dictionarySize < 4*kb) dictionarySize = 4*kb;
}

void LZMA_METHOD::SetDecompressionMem (MemSize mem)
{
  if (mem > 2*mb) dictionarySize = mem - 2*mb;
}

void LZMA_METHOD::SetDictionary (MemSize dict)
{
  if (dict) dictionarySize = dict;
}

void LZMA_METHOD::ShowCompressionMethod (char *buf)
{
  LZMA_METHOD defaults;
  char DictionaryStr[100];
  showMem (dictionarySize, DictionaryStr);
  char params[400]; params[0]='\0';
  if (algorithm       != defaults.algorithm)       sprintf(params+strlen(params), ":a%d", algorithm);
  if (numFastBytes    != defaults.numFastBytes)    sprintf(params+strlen(params), ":fb%d", numFastBytes);
  if (matchFinder     != defaults.matchFinder)     sprintf(params+strlen(params), ":mf=%s", kMatchFinderIDs[matchFinder]);
  if (matchFinderCycles!=defaults.matchFinderCycles) sprintf(params+strlen(params), ":mc%d", matchFinderCycles);
  if (posStateBits    != defaults.posStateBits)    sprintf(params+strlen(params), ":pb%d", posStateBits);
  if (litContextBits  != defaults.litContextBits)  sprintf(params+strlen(params), ":lc%d", litContextBits);
  if (litPosBits      != defaults.litPosBits)      sprintf(params+strlen(params), ":lp%d", litPosBits);
  sprintf (buf, "lzma:%s%s", DictionaryStr, params);
}

#endif // !FREEARC_DECOMPRESS_ONLY

COMPRESSION_METHOD* parse_LZMA (char** parameters)
{
  if (strcmp (parameters[0], "lzma") == 0) {
    LZMA_METHOD *p = new LZMA_METHOD;
    int error = 0;
    char *rest;
    while (*++parameters && !error) {
      char* param = *parameters;
      if (strequ(param,"max"))      { p->algorithm = 1; continue; }
      if (strequ(param,"normal"))   { p->algorithm = 1; continue; }
      if (strequ(param,"fast"))     { p->algorithm = 0; continue; }
      if (strequ(param,"fastest"))  { p->algorithm = 0; continue; }
      if (strequ(param,"eos"))      { continue; }        // ignored: always write EOS
      { int mf = FindMatchFinder(param);
        if (mf >= 0) { p->matchFinder = mf; continue; } }
      if ((rest = start_from(param, "mf=")) != NULL) {
        int mf = FindMatchFinder(rest);
        if (mf < 0) { error=1; break; }
        p->matchFinder = mf; continue;
      }
      if ((rest = start_from(param, "mf")) != NULL) {  // e.g. mfbt4
        int mf = FindMatchFinder(rest);
        if (mf < 0) { error=1; break; }
        p->matchFinder = mf; continue;
      }
      switch (*param) {
        case 'd': p->dictionarySize = parseMem(param+1, &error); continue;
        case 'h': p->hashSize       = parseMem(param+1, &error); continue;
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
      // Arg starts with digit: treat as dictionary size if has mem suffix, else fb.
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
  } else
    return NULL;
}

static int LZMA_x = AddCompressionMethod (parse_LZMA);
