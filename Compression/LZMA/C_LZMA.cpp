// C_LZMA.cpp - FreeArc/DArc interface to LZMA (7-Zip 24.09 C API)
//
// Wire format: raw LZMA1 stream WITHOUT 5-byte properties header.
// Properties (dictSize, lc, lp, pb) are carried out-of-band in the method string,
// preserving byte-level compatibility with FreeArc 0.51-era archives.

extern "C" {
#include "C_LZMA.h"
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
} // extern "C"




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

// The Rust codec in rust/darc-lzma. Same 11-argument ABI the C entry points had, so
// the LoadFromDLL overrides below still work unchanged.
extern "C" int darc_lzma_compress   (int, int, int, int, int, int, int, int, int,
                                     CALLBACK_FUNC*, void*);
extern "C" int darc_lzma_decompress (int, int, int, int, int, int, int, int, int,
                                     CALLBACK_FUNC*, void*);

int LZMA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  static FARPROC f = LoadFromDLL ("lzma_decompress");
  if (!f) f = (FARPROC) darc_lzma_decompress;
  return ((int (*)(int,int,int,int,int,int,int,int,int, CALLBACK_FUNC*, void*)) f)
           (dictionarySize, hashSize, algorithm, numFastBytes, matchFinder,
            matchFinderCycles, posStateBits, litContextBits, litPosBits,
            callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int LZMA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  static FARPROC f = LoadFromDLL ("lzma_compress");
  // The Rust encoder in rust/darc-lzma. Byte-identical to the C across every
  // configuration this wrapper can ask for -- all five match finders and both
  // parsers -- gated by rust/difftest/lzma-gap-check.sh at 222/222, which is why
  // switching it on changed no archive bytes.
  if (!f) f = (FARPROC) darc_lzma_compress;
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
