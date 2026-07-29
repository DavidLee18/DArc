extern "C" {
#include "C_TTA.h"
}

// tta.cpp, entropy.cpp and filters.cpp are gone: tta_compress and
// tta_decompress come from rust/darc-codecs/src/tta.rs, and those three files
// were their bodies -- the TTA entropy coder, the adaptive predictors, and the
// WAV reader/writer around them.
//
// mmdet.h went with them. This file detects nothing; the detector is still
// compiled, into C_MM.o, where the Haskell FFI reaches it.


/*-------------------------------------------------*/
/* Implementation of the TTA_METHOD class          */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
TTA_METHOD::TTA_METHOD()
{
  level       = 3;
  skip_header = 0;
  is_float    = 0;
  num_chan    = 0;
  word_size   = 0;
  offset      = 0;
  raw_data    = 0;
}

// Decompression function
int TTA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("tta_decompress");
  if (!f) f = (FARPROC) tta_decompress;

  return ((int (__cdecl *)(CALLBACK_FUNC*, void*)) f) (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int TTA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("tta_compress");
  if (!f) f = (FARPROC) tta_compress;

  return ((int (__cdecl *)(int, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                          (level, skip_header, is_float, num_chan, word_size, offset, raw_data, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_TTA)
void TTA_METHOD::ShowCompressionMethod (char *buf)
{
    TTA_METHOD defaults;  char eStr[100], cStr[100], rStr[100];
    if (num_chan || word_size) {
        sprintf (cStr, ":%d*%d%s", num_chan, word_size, is_float? "f":"");
        if (offset)  sprintf (str_end(cStr), ":o%d", offset);
    } else {
        sprintf (cStr, skip_header? ":s" : "");
    }
    sprintf (eStr, level      !=defaults.level?       ":m%d" : "", level);
    sprintf (rStr, raw_data   !=defaults.raw_data?    ":r%d" : "", raw_data);
    sprintf (buf, "tta%s%s%s", eStr, cStr, rStr);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an object of type TTA_METHOD with the given compression parameters
// or returns NULL if this is a different compression method or there is an error in the parameters
COMPRESSION_METHOD* parse_TTA (char** parameters)
{
  if (strcmp (parameters[0], "tta") == 0) {
    // If the method name (parameter zero) is "tta", then parse the remaining parameters

    TTA_METHOD *p = new TTA_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all method parameters (or exit early if an error occurs while parsing one of them)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Parameters that carry values
        case 'm':  p->level       = parseInt (param+1, &error); continue;
        case 's':  p->skip_header = 1;                          continue;
        case 'f':  p->is_float    = 1;                          continue;
        case 'c':  p->num_chan    = parseInt (param+1, &error); continue;
        case 'w':  p->word_size   = parseInt (param+1, &error); continue;
        case 'o':  p->offset      = parseInt (param+1, &error); continue;
        case 'r':  p->raw_data    = parseInt (param+1, &error); continue;
      }
      // We get here if the parameter does not specify its name
      // If this parameter can be parsed as c*w,
      // then we use these values for the num_chan and word_size fields.
      // An additional character 'f' means that this is data in FP format
      int a, b;  char s[MAX_METHOD_STRLEN];
      if (sscanf (param, "%d*%d%s", &a, &b, s)==3  &&  strequ(s,"f"))
          p->is_float = 1, p->num_chan=a, p->word_size=b;
      else if (sscanf (param, "%d*%d", &a, &b)==2)
          p->is_float = 0, p->num_chan=a, p->word_size=b;
      else error=1;
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the TTA method
}

static int TTA_x = AddCompressionMethod (parse_TTA);   // Register the TTA method parser
