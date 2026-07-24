extern "C" {
#include "C_REP.h"
}


#define REP_LIBRARY

/*-------------------------------------------------*/
/* Implementation of the REP_METHOD class          */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
REP_METHOD::REP_METHOD()
{
  BlockSize      = 64*mb;
  MinCompression = 100;
  MinMatchLen    = 512;
  HashSizeLog    = 0;
  Barrier        = INT_MAX;
  SmallestLen    = 512;
  Amplifier      = 1;
}

// Decompression function
int REP_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("rep_decompress");
  if (!f) f = (FARPROC) rep_decompress;

  return ((int (__cdecl *)(MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                          (BlockSize, MinCompression, MinMatchLen, Barrier, SmallestLen, HashSizeLog, Amplifier, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int REP_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("rep_compress");
  if (!f) f = (FARPROC) rep_compress;

  return ((int (__cdecl *)(MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                          (BlockSize, MinCompression, MinMatchLen, Barrier, SmallestLen, HashSizeLog, Amplifier, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_REP)
void REP_METHOD::ShowCompressionMethod (char *buf)
{
    REP_METHOD defaults; char BlockSizeStr[100], MinCompressionStr[100], BarrierTempStr[100], BarrierStr[100], SmallestLenStr[100], HashSizeLogStr[100], AmplifierStr[100], MinMatchLenStr[100];
    showMem (BlockSize, BlockSizeStr);
    showMem (Barrier,   BarrierTempStr);
    sprintf (MinCompressionStr, MinCompression!=defaults.MinCompression? ":%d%%" : "", MinCompression);
    sprintf (BarrierStr,     Barrier    !=defaults.Barrier    ? ":d%s" : "", BarrierTempStr);
    sprintf (SmallestLenStr, SmallestLen!=defaults.SmallestLen? ":s%d" : "", SmallestLen);
    sprintf (AmplifierStr,   Amplifier  !=defaults.Amplifier  ? ":a%d" : "", Amplifier);
    sprintf (HashSizeLogStr, HashSizeLog!=defaults.HashSizeLog? ":h%d" : "", HashSizeLog);
    sprintf (MinMatchLenStr, MinMatchLen!=defaults.MinMatchLen? ":%d"  : "", MinMatchLen);
    sprintf (buf, "rep:%s%s%s%s%s%s%s", BlockSizeStr, MinCompressionStr, MinMatchLenStr, BarrierStr, SmallestLenStr, HashSizeLogStr, AmplifierStr);
}

// Moved here verbatim from rep.cpp when that file was deleted: the Rust port
// replaced rep_compress/rep_decompress, but GetCompressionMem below still needs
// these two pure helpers to reproduce the encoder's hash sizing. They affect
// only the reported memory estimate, never archive bytes.
//
// sqrtb(36,2) == 4
inline static unsigned sqrtb (unsigned n, unsigned base = 2)
{
    int result;
    for (result=1; (n/=base*base) != 0; result *= base);
    return result;
}

// The hash size should match the number of values we want to store in it, but
// not exceed a quarter of the buffer size.
static MemSize CalcHashSize (MemSize HashBits, MemSize BlockSize, MemSize k)
{
    return HashBits>0? (1<<HashBits) : roundup_to_power_of(BlockSize/3*2,2) / mymax(k,16);
}

// Compute how much memory is required to compress with the given method
MemSize REP_METHOD::GetCompressionMem (void)
{
    // Copied from rep_compress
    int L = roundup_to_power_of (mymin(SmallestLen,MinMatchLen)/2, 2);  // Size of the blocks whose checksums are put into the hash
    int k = sqrtb(L*2);
    int HashSize = CalcHashSize (HashSizeLog, BlockSize, k);

    return BlockSize + HashSize*sizeof(int);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs a REP_METHOD object with the given compression parameters
// or returns NULL if this is a different compression method or the parameters are invalid
COMPRESSION_METHOD* parse_REP (char** parameters)
{
  if (strcmp (parameters[0], "rep") == 0) {
    // If the method name (parameter zero) is "rep", parse the remaining parameters

    REP_METHOD *p = new REP_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Walk through all method parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Parameters that carry a value
        case 'b':  p->BlockSize   = parseMem (param+1, &error); continue;
        case 'l':  p->MinMatchLen = parseInt (param+1, &error); continue;
        case 'd':  p->Barrier     = parseMem (param+1, &error); continue;
        case 's':  p->SmallestLen = parseInt (param+1, &error); continue;
        case 'h':  p->HashSizeLog = parseInt (param+1, &error); continue;
        case 'a':  p->Amplifier   = parseInt (param+1, &error); continue;
      }
      // If the parameter ends with a percent sign, try to parse it as "N%"
      if (last_char(param) == '%') {
        char str[100]; strcpy(str,param); last_char(str) = '\0';
        int n = parseInt (str, &error);
        if (!error) { p->MinCompression = n; continue; }
        error=0;
      }
      // We end up here when the parameter has no name given
      // If this parameter can be parsed as an integer (i.e. it contains only digits),
      // then assign its value to the MinMatchLen field, otherwise try to parse it as BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinMatchLen = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the REP method
}

static int REP_x = AddCompressionMethod (parse_REP);   // Register the REP method parser
