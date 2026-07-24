extern "C" {
#include "C_Dict.h"
}

#define DICT_LIBRARY
// DARC_RUST=1 selects the Rust port. Both halves are ported now -- the
// encoder (phases 1-7) and the decoder -- so the whole C implementation is
// excluded, not just one entry point.

#ifndef FREEARC_DECOMPRESS_ONLY
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// This file wraps its body in extern "C", so this definition and the Rust
// export are the same symbol. With both present the linker resolves from this
// object and never pulls the Rust one, producing a binary that looks correctly
// linked while running the C code -- so the switch has to remove this
// definition, not merely add a declaration elsewhere.
//
// The port is verified byte-identical to DictDecode over 11 inputs including
// prose, records, source and binary -- see rust/difftest.


/*-------------------------------------------------*/
/* DICT_METHOD class implementation                */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
DICT_METHOD::DICT_METHOD()
{
  BlockSize      = 64*mb;
  MinCompression = 100;
  MinWeakChars   = 20;
  MinLargeCnt    = 2048;
  MinMediumCnt   = 100;
  MinSmallCnt    = 50;
  MinRatio       = 4;
}

// Decompression function
int DICT_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("dict_decompress");
  if (!f) f = (FARPROC) dict_decompress;

  return ((int (*)(MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                  (BlockSize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int DICT_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("dict_compress");
  if (!f) f = (FARPROC) dict_compress;

  return ((int (*)(MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                  (BlockSize, MinCompression, MinWeakChars, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_DICT)
void DICT_METHOD::ShowCompressionMethod (char *buf)
{
    DICT_METHOD defaults; char BlockSizeStr[100], MinCompressionStr[100], MinWeakCharsStr[100];
    char MinLargeCntStr[100], MinMediumCntStr[100], MinSmallCntStr[100], MinRatioStr[100];
    showMem (BlockSize, BlockSizeStr);
    sprintf (MinCompressionStr, MinCompression!=defaults.MinCompression? ":%d%%" : "", MinCompression);
    sprintf (MinWeakCharsStr,   MinWeakChars  !=defaults.MinWeakChars  ? ":c%d"  : "", MinWeakChars);
    sprintf (MinLargeCntStr,    MinLargeCnt   !=defaults.MinLargeCnt   ? ":l%d"  : "", MinLargeCnt );
    sprintf (MinMediumCntStr,   MinMediumCnt  !=defaults.MinMediumCnt  ? ":m%d"  : "", MinMediumCnt);
    sprintf (MinSmallCntStr,    MinSmallCnt   !=defaults.MinSmallCnt   ? ":s%d"  : "", MinSmallCnt );
    sprintf (MinRatioStr,       MinRatio      !=defaults.MinRatio      ? ":r%d"  : "", MinRatio    );
    sprintf (buf, "dict:%s%s%s%s%s%s%s", BlockSizeStr, MinCompressionStr, MinWeakCharsStr,
                                         MinLargeCntStr, MinMediumCntStr, MinSmallCntStr, MinRatioStr);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs a DICT_METHOD object with the given compression parameters
// or returns NULL if this is a different compression method or the parameters contain an error
COMPRESSION_METHOD* parse_DICT (char** parameters)
{
  if (strcmp (parameters[0], "dict") == 0) {
    // If the method name (parameter zero) is "dict", parse the remaining parameters

    DICT_METHOD *p = new DICT_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all the method parameters (or exit earlier if an error occurs while parsing the current parameter)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 'p':  p->MinLargeCnt=8192; p->MinMediumCnt=400; p->MinSmallCnt=100; p->MinRatio=4; continue;
        case 'f':  p->MinLargeCnt=2048; p->MinMediumCnt=100; p->MinSmallCnt= 50; p->MinRatio=0; continue;
      }
      else switch (*param) {                    // Parameters carrying a value
        case 'b':  p->BlockSize    = parseMem (param+1, &error); continue;
        case 'c':  p->MinWeakChars = parseInt (param+1, &error); continue;
        case 'l':  p->MinLargeCnt  = parseInt (param+1, &error); continue;
        case 'm':  p->MinMediumCnt = parseInt (param+1, &error); continue;
        case 's':  p->MinSmallCnt  = parseInt (param+1, &error); continue;
        case 'r':  p->MinRatio     = parseInt (param+1, &error); continue;
      }
      // If the parameter ends with a percent sign, try to parse it as "N%"
      if (last_char(param) == '%') {
        char str[100]; strcpy(str,param); last_char(str) = '\0';
        int n = parseInt (str, &error);
        if (!error) { p->MinCompression = n; continue; }
        error=0;
      }
      // We get here if the parameter name was not specified
      // If this parameter can be parsed as an integer (i.e. it contains only digits),
      // assign its value to the MinMatchLen field, otherwise try to parse it as BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinWeakChars = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not a DICT method
}

static int DICT_x = AddCompressionMethod (parse_DICT);   // Register the DICT method parser
