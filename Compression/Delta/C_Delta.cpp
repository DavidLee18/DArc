extern "C" {
#include "C_Delta.h"
}


#define DELTA_LIBRARY
// DARC_RUST=1 selects the Rust port of this codec (rust/darc-codecs).
//
// Note this whole file is wrapped in extern "C" (line 1), so Delta.cpp's
// definitions already have C linkage here -- the same names the Rust crate
// exports. That is why the switch has to be an exclusion rather than a
// declaration change: with both present the linker resolves delta_compress from
// this object and never pulls the archive member, producing a binary
// byte-identical to the C build while appearing to have "linked Rust in".
//
// The port is verified byte-identical to Delta.cpp over 23 inputs and ~280
// detected tables, in both directions -- see rust/difftest.

/*-------------------------------------------------*/
/* DELTA_METHOD class implementation                 */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
DELTA_METHOD::DELTA_METHOD()
{
  BlockSize      = 8*mb;
  ExtendedTables = 0;
}

// Decompression function
int DELTA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("delta_decompress");
  if (!f) f = (FARPROC) delta_decompress;

  return ((int (__cdecl *)(MemSize, int, CALLBACK_FUNC*, void*)) f)
                          (BlockSize, ExtendedTables, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int DELTA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("delta_compress");
  if (!f) f = (FARPROC) delta_compress;

  return ((int (__cdecl *)(MemSize, int, CALLBACK_FUNC*, void*)) f)
                          (BlockSize, ExtendedTables, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_DELTA)
void DELTA_METHOD::ShowCompressionMethod (char *buf)
{
    DELTA_METHOD defaults; char BlockSizeStr[100]=":";
    showMem (BlockSize, BlockSizeStr+1);
    sprintf (buf, "delta%s%s", BlockSize!=defaults.BlockSize? BlockSizeStr:"", ExtendedTables? ":x":"");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs a DELTA_METHOD object with the given compression parameters,
// or returns NULL if this is a different compression method or the parameters are invalid
COMPRESSION_METHOD* parse_DELTA (char** parameters)
{
  if (strcmp (parameters[0], "delta") == 0) {
    // If the method name (parameter zero) is "delta", parse the remaining parameters

    DELTA_METHOD *p = new DELTA_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all method parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 'x':  p->ExtendedTables = 1; continue;
      }
      switch (*param) {                    // Parameters that carry a value
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
      }
      // We get here if the parameter does not include a name
      // If this parameter can be parsed as an amount of memory,
      // its value is assigned to the BlockSize field
      p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the DELTA method
}

static int DELTA_x = AddCompressionMethod (parse_DELTA);   // Register the DELTA method parser

