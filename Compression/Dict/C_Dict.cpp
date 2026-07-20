extern "C" {
#include "C_Dict.h"
}

#define DICT_LIBRARY
#include "dict.cpp"

#ifndef FREEARC_DECOMPRESS_ONLY
int dict_compress (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio, CALLBACK_FUNC *callback, void *auxdata)
{
    BYTE* In = NULL;  // pointer to the input data
    BYTE* Out= NULL;  // pointer to the output data
    int x;            // code of the error that occurred
    while ( (x = callback ("read", (In = (BYTE*) malloc(BlockSize)), BlockSize, auxdata)) > 0 )
    {
        unsigned InSize, OutSize;     // number of bytes in the input and output buffers, respectively
        In = (BYTE*) realloc(In,InSize=x);
        x = DictEncode(In,InSize,&Out,&OutSize,MinWeakChars,MinLargeCnt,MinMediumCnt,MinSmallCnt,MinRatio);
        if (x || OutSize/MinCompression>=InSize/100) {
            // compressing the data [well enough] failed, so write the original data instead
            int WrSize=-InSize;
            FreeAndNil(Out);
            // Write the original block and exit if a write error occurred / no more data is needed
            checked_write (&WrSize, sizeof(WrSize));
            checked_write (In, InSize);
            FreeAndNil(In);
        } else {
            // the data was compressed successfully, we can free the input buffer before writing it out
            // (in order to free more memory for the next algorithm in the compression chain)
            FreeAndNil(In);
            // Write the compressed block and exit if a write error occurred / no more data is needed
            checked_write (&OutSize, sizeof(OutSize));
            checked_write (Out, OutSize);
            FreeAndNil(Out);
        }
    }
finished:
    FreeAndNil(In); FreeAndNil(Out); return x;  // 0 if everything is fine, otherwise the error code
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// Only the decoder is excluded here, not the whole dict.cpp include: the
// encoder (DictEncode and its seven phases) has not been ported yet and is
// still needed by dict_compress below.
//
// This file wraps its body in extern "C", so this definition and the Rust
// export are the same symbol. With both present the linker resolves from this
// object and never pulls the Rust one, producing a binary that looks correctly
// linked while running the C code -- so the switch has to remove this
// definition, not merely add a declaration elsewhere.
//
// The port is verified byte-identical to DictDecode over 11 inputs including
// prose, records, source and binary -- see rust/difftest.
#ifndef DARC_RUST
int dict_decompress (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio, CALLBACK_FUNC *callback, void *auxdata)
{
  BYTE* In = NULL;  // pointer to the input data
  BYTE* Out= NULL;  // pointer to the output data
  int x;            // code of the error that occurred
  for(;;) {
    int InSize; unsigned OutSize;   // number of bytes in the input and output buffers, respectively
    // Read block header; 0 bytes = clean EOF (end of compressed stream)
    x = callback ("read", &InSize, sizeof(InSize), auxdata);
    if (x == 0)  { x=0; goto finished; }
    if (x != sizeof(InSize))  { if (x>=0) x=FREEARC_ERRCODE_IO; goto finished; }
    if (InSize<0) {
        // copy the uncompressed data as is
        In = (BYTE*) malloc(-InSize);
        checked_read  (In, -InSize);
        checked_write (In, -InSize);
        FreeAndNil(In);
    } else {
        // Perform the decoding and obtain the size of the output data
        In  = (BYTE*) malloc(InSize);
        Out = (BYTE*) malloc(BlockSize);
        checked_read  (In, InSize);
        x = DictDecode (In, InSize, Out, &OutSize);
        //x = DictDecode (InSize, callback, auxdata);   // for operating within a fixed amount of memory
        if (x) break;
        FreeAndNil(In);
        Out = (BYTE*) realloc (Out, OutSize);
        checked_write (Out, OutSize);
        FreeAndNil(Out);
    }
  }
finished:
  FreeAndNil(In); FreeAndNil(Out);
  return x<=0? x : FREEARC_ERRCODE_IO;  // 0 if everything is fine, otherwise the error code
}
#endif  // !DARC_RUST


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
