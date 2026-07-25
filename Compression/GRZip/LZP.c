/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor               LZP.c */
/* LZP Preprocessing Functions                     */
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
  grib@crazy.ru

  This program is based on (at least) the work of:
  Juergen Abel, Jon L. Bentley, Edgar Binder,
  Charles Bloom, Mike Burrows, Andrey Cadach,
  Damien Debin, Sebastian Deorowicz, Peter Fenwick,
  Michael Schindler, Robert Sedgewick, Julian Seward,
  David Wheeler, Vadim Yoockin.

  LZP memory use         : 2*BlockLen + [16-1024] Kb
  Reverse LZP memory use : 2*BlockLen + [16-1024] Kb

  For more information on these sources, see the manual.
--*/

#include <stdio.h>
#include "libGRZip.h"

#define LZP_MatchFlag    0xF2
#define LZP_RunFlag      0xF3
#define LZP_XorFlag      (uint8)(0xFF^LZP_RunFlag)

// Contexts holds pointers, so it must be sized by sizeof(uint8*), not
// sizeof(uint32). The two are the same width only on 32-bit; on 64-bit this
// allocated exactly half the table it then indexed to LZP_HT_Size, so
// "Contexts[HashIndex]=Input" wrote past the end of the block and the matching
// read returned a wild pointer that was immediately dereferenced.
//
// It surfaced as an intermittent SIGSEGV in a GRZip worker thread, and -- when
// the overflow landed on heap metadata instead -- as glibc "double free or
// corruption" during an unrelated later operation. Which one you got depended
// on heap layout, so the failure appeared to wander between codecs and
// platforms rather than pointing here.
#define LZP_AllocHashTable()                                              \
  uint8 ** Contexts=(uint8 **)BigAlloc((LZP_HT_Size+1)*sizeof(uint8*));   \
  if (Contexts==NULL) return (GRZ_NOT_ENOUGH_MEMORY);                     \
  memset(Contexts,0,(LZP_HT_Size+1)*sizeof(uint8*));

#define LZP_FreeHashTable() BigFree(Contexts);

// The encoder (the LZP pre-filter) lived here and is now Rust: see
// rust/darc-codecs/src/grzip/. Verified byte-identical to this C across the
// stage matrix and the multi-block stream -- rust/difftest/grzip-check.sh and
// grzip-stage-check.sh compare the produced streams, and the archiver's grzip
// fingerprint is unchanged.
//
// The DECODER below stays. Unarc builds these files with
// -DFREEARC_DECOMPRESS_ONLY and does NOT link the Rust crate, so the standalone
// extractor and the SFX modules still need it. It goes when Unarc does.


sint32 GRZip_LZP_Decode(uint8 * Input,uint32 Size,uint8 * Output,uint32 LZP_MinMatchLen,uint32 LZP_HT_Size,uint32 OutSize)
{
  LZP_AllocHashTable();
  uint8  * InputEnd=Input+Size;
  uint8  * OutputBeg=Output;
  uint8  * OutputEnd=Output+OutSize;

  // Input/Output sizes come from a possibly corrupt block header. The header
  // read/write below touch [0..3] of each buffer, and everything in the loop
  // is bounded against InputEnd/OutputEnd, so a bad block is rejected instead
  // of over-reading the input or over-running the output. Valid blocks satisfy
  // every bound by construction -- OutSize is the block's decompressed size,
  // which the caller allocated Output to hold -- so this is transparent.
  if (Size < 4 || OutSize < 4)  { LZP_FreeHashTable(); return (GRZ_UNEXPECTED_EOF); }

  *((uint32 *)Output)=*((uint32 *)Input);
  uint32 Ctx=(Input[3]+(Input[2]<<8)+(Input[1]<<16)+(Input[0]<<24));

  Input+=4;
  Output+=4;

  while (Input<InputEnd)
  {
    uint32  HashIndex=((Ctx>>15)^Ctx^(Ctx>>3))&LZP_HT_Size;
    uint8 * Pointer=Contexts[HashIndex];

    Contexts[HashIndex]=Output;

    if (Pointer)
     {
       if ((*Input++)!=LZP_MatchFlag) {
         if (Output>=OutputEnd)  { LZP_FreeHashTable(); return (GRZ_UNEXPECTED_EOF); }
         Ctx=(Ctx<<8)|(*Output++=*(Input-1));
       }
       else
       {
         uint32 CommonLength=0;
         // Bound the run-length reader against InputEnd -- it was unbounded and
         // walked off the input on a truncated/hostile block.
         while (Input<InputEnd && (CommonLength+=((*Input)^LZP_XorFlag),(*Input++)==LZP_RunFlag));
         if (CommonLength)
          {
            CommonLength=CommonLength+LZP_MinMatchLen-1;
            // The copy writes CommonLength bytes and reads them from Pointer,
            // an earlier output position, so bounding the destination bounds
            // the source too. Reject a length that would run past OutputEnd.
            if (CommonLength > (uint32)(OutputEnd-Output))  { LZP_FreeHashTable(); return (GRZ_UNEXPECTED_EOF); }
            while (CommonLength--) *Output++=*Pointer++;
            Ctx=(Output[-1]+(Output[-2]<<8)+(Output[-3]<<16)+(Output[-4]<<24));
          }
         else {
          if (Output>=OutputEnd)  { LZP_FreeHashTable(); return (GRZ_UNEXPECTED_EOF); }
          Ctx=(Ctx<<8)|(*Output++=LZP_MatchFlag);
         }
       }
     }
    else {
      if (Output>=OutputEnd)  { LZP_FreeHashTable(); return (GRZ_UNEXPECTED_EOF); }
      Ctx=(Ctx<<8)|((*Output++)=(*Input++));
    }
  }
  LZP_FreeHashTable();
  return (Output-OutputBeg);
}

#undef LZP_MatchFlag
#undef LZP_RunFlag
#undef LZP_XorFlag
#undef LZP_InitHashTables
#undef LZP_FreeHashTables

/*-------------------------------------------------*/
/* End                                       LZP.c */
/*-------------------------------------------------*/
