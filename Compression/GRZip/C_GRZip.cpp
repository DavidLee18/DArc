/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor          libGRZip.c */
/* Archiver-side glue for the GRZip method         */
/*-------------------------------------------------*/

/*--
  This file is a part of GRZipII and/or libGRZip, a program
  and library for lossless, block-sorting data compression.

  Copyright (C) 2002-2004 Grebnov Ilya. All rights reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  Grebnov Ilya, Ivanovo, Russian Federation.
  Ilya.Grebnov@magicssoft.ru, http://magicssoft.ru/

  This program is based on (at least) the work of:
  Juergen Abel, Jon L. Bentley, Edgar Binder,
  Charles Bloom, Mike Burrows, Andrey Cadach,
  Damien Debin, Sebastian Deorowicz, Peter Fenwick,
  George Plechanov, Michael Schindler, Robert Sedgewick,
  Julian Seward, David Wheeler, Vadim Yoockin.

  For more information on these sources, see the manual.
--*/

// None of GRZip's algorithm is left in C. Both entry points come from
// rust/darc-codecs/src/grzip/, which exports grzip_compress and
// grzip_decompress under those exact names; what remains here is the
// archiver's glue -- the GRZIP_METHOD parameter object and the parser for the
// method string.
//
// The encoder went in an earlier pass, taking GRZip_StoreBlock,
// GRZip_CompressBlock, GRZip_GetAdaptiveBlockSize and the GRZipMTCompressor
// pool with it. This pass removes the decoder that outlived it --
// GRZip_DecompressBlock, GRZip_CheckBlockSign, GRZipDecompressionThread,
// GRZipMTDecompressor -- and with them the six vendored transform files they
// were the only callers of: LZP.c, BWT.c, ST4.c, MTF_Ari.c, WFC_Ari.c and
// Rec_Flt.c, plus WFC_MTF.h and libGRZip.h.
//
// Two comments kept that code alive and both had stopped being true. The
// decoder's said Unarc "does not link the Rust crate", so the standalone
// extractor still needed a C one. It did not: Unarc/makefile built
// libdarc_codecs.a with the `dropin` feature and linked it, and `dropin` is
// precisely what exports grzip_decompress. (Unarc/ has since been deleted
// outright -- the extractor is rust/darc-unarc and calls the crate directly.) The worker pool's said it stayed "for the encoder, which
// still uses them", written in the same edit that deleted that encoder.
//
// Byte-identity in both directions is established by
// rust/difftest/grzip-check.sh, which builds its oracle from a pinned revision
// of the C rather than from this tree -- so removing the working-tree copy
// does not weaken the test that justifies removing it.

extern "C" {
#include "C_GRZip.h"
}


/*-------------------------------------------------*/
/* Implementation of the GRZIP_METHOD class        */
/*-------------------------------------------------*/
// Constructor assigning default values to the parameters of the compression method
GRZIP_METHOD::GRZIP_METHOD()
{
  Method              = 1;
  BlockSize           = 8*mb;
  EnableLZP           = 1;
  MinMatchLen         = 32;
  HashSizeLog         = 15;
  AlternativeBWTSort  = 0;
  AdaptiveBlockSize   = 0;
  DeltaFilter         = 0;
}

// Decompression function
int GRZIP_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("grzip_decompress");
  if (!f) f = (FARPROC) grzip_decompress;

  return ((int (__cdecl *)(CALLBACK_FUNC*, void*)) f) (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int GRZIP_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("grzip_compress");
  if (!f) f = (FARPROC) grzip_compress;

  return ((int (__cdecl *)(int, int, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                        (Method,
                         BlockSize,
                         EnableLZP,
                         MinMatchLen,
                         HashSizeLog,
                         AlternativeBWTSort,
                         AdaptiveBlockSize,
                         DeltaFilter,
                         callback,
                         auxdata);
}

// Set the block size and shrink the hash size if it is too large for such a small block
void GRZIP_METHOD::SetBlockSize (MemSize bs)
{
  if (bs>0) {
    BlockSize   = mymin (bs, GRZ_MaxBlockSize);
    HashSizeLog = mymin (HashSizeLog, 1+lb(BlockSize-1));
  }
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_GRZIP)
void GRZIP_METHOD::ShowCompressionMethod (char *buf)
{
  char LZP_Str[100], BlockSizeStr[100];
  sprintf (LZP_Str, "l%d:h%d", MinMatchLen, HashSizeLog);
  showMem (BlockSize, BlockSizeStr);
  sprintf (buf, "grzip:%s:m%d:%s%s%s%s", BlockSizeStr,
                                         Method,
                                         EnableLZP?          LZP_Str : "l",
                                         AlternativeBWTSort? ":s" : "",
                                         AdaptiveBlockSize?  ":a" : "",
                                         DeltaFilter?        ":d" : "");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an object of type GRZIP_METHOD with the given compression parameters
// or returns NULL if this is a different compression method or an error was made when specifying the parameters
COMPRESSION_METHOD* parse_GRZIP (char** parameters)
{
  if (strcmp (parameters[0], "grzip") == 0) {
    // If the method name (parameter zero) is "grzip", then parse the remaining parameters

    GRZIP_METHOD *p = new GRZIP_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    while (!error && *++parameters)  // Iterate over all parameters of the method
    {
      char *param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 's':  p->AlternativeBWTSort  = 1; continue;
        case 'a':  p->AdaptiveBlockSize   = 1; continue;
        case 'l':  p->EnableLZP           = 0; continue;
        case 'd':  p->DeltaFilter         = 1; continue;
        case 'p':  p->AdaptiveBlockSize=0; p->EnableLZP=0; p->DeltaFilter=1; continue;
      }
      else switch (*param) {                    // Parameters carrying a value
        case 'm':  p->Method      = parseInt (param+1, &error); continue;
        case 'b':  p->BlockSize   = parseMem (param+1, &error); continue;
        case 'l':  p->MinMatchLen = parseInt (param+1, &error); continue;
        case 'h':  p->HashSizeLog = parseInt (param+1, &error); continue;
      }
      // We get here if the parameter does not specify its name
      // If this parameter can be parsed as an integer (i.e. it contains only digits),
      // then assign its value to the MinMatchLen field, otherwise try to parse it as BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinMatchLen = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the grzip method
}

static int GRZIP_x = AddCompressionMethod (parse_GRZIP);   // Register the parser for the GRZIP method

/*-------------------------------------------------*/
/* End                                  libGRZip.c */
/*-------------------------------------------------*/
