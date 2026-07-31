// C_LZMA2.cpp - DArc interface to LZMA2 (7-Zip 24.09 C API)
//
// Wire format: 1 byte LZMA2 props + standard LZMA2 stream (self-terminating).
// The stream embeds its own end marker (0x00 control byte), so no length prefix.

extern "C" {
#include "C_LZMA2.h"
}

#include <string.h>

extern "C" {

// ---------- Allocator ----------
} // extern "C"

#ifndef FREEARC_DECOMPRESS_ONLY


#endif



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

// The Rust LZMA2 in rust/darc-lzma. Same ABI as the C entry points below.
// rust/difftest/lzma2-check.sh gates it at 157/157 byte-identical streams and
// 72/72 decode cases, with cross-decode clean in both directions.
extern "C" int darc_lzma2_compress   (int, int, int, int, int, int, int, int, int,
                                      CALLBACK_FUNC*, void*);
extern "C" int darc_lzma2_decompress (CALLBACK_FUNC*, void*);

int LZMA2_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return darc_lzma2_decompress (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

int LZMA2_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // GetCompressionThreads() is passed rather than read on the Rust side, so the
  // codec crate keeps no link dependency on CompressionLibrary.cpp. Above one thread
  // this selects the multi-block stream, exactly as the C SDK did.
  return darc_lzma2_compress (dictionarySize, algorithm, numFastBytes, matchFinder,
                              matchFinderCycles, posStateBits, litContextBits, litPosBits,
                              GetCompressionThreads(), callback, auxdata);
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
