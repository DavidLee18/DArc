/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor           Rec_Flt.c */
/* Data reordering and Delta Filter                */
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

#include <stdlib.h>
#include <math.h>

#include "libGRZip.h"

// The encoder (the record-structure detector and de-interleaver) lived here and is now Rust: see
// rust/darc-codecs/src/grzip/. Verified byte-identical to this C across the
// stage matrix and the multi-block stream -- rust/difftest/grzip-check.sh and
// grzip-stage-check.sh compare the produced streams, and the archiver's grzip
// fingerprint is unchanged.
//
// The DECODER below stays. Unarc builds these files with
// -DFREEARC_DECOMPRESS_ONLY and does NOT link the Rust crate, so the standalone
// extractor and the SFX modules still need it. It goes when Unarc does.


void GRZip_Rec_Decode(uint8 * Input, sint32 Size,
                      uint8 * Output,sint32 Mode)
{
  if (Mode==3)
  {
    sint32 NumRecords=(Size>>1);
    uint16 Code,PredCode=0;
    uint16 * Outp=(uint16 *)Output;
    uint16 * OutpEnd=Outp+NumRecords;
    while (Outp<OutpEnd)
    {
      uint16 Delta=*Input;
      Delta=(Delta<<8)|(*(Input+NumRecords));Input++;
      if (Delta&1) Delta=~(Delta>>1); else Delta>>=1;
      Code=Delta+PredCode; PredCode=Code;
      *Outp++=Code;
    }

    sint32 i=2*NumRecords;
    while (i<Size)
    {
      Output[i]=*(Input+NumRecords);
      i++;Input++;
    }
  }

  if (Mode==4)
  {
    sint32 NumRecords=(Size>>2);
    sint32 P1=NumRecords;
    sint32 P2=2*NumRecords;
    sint32 P3=3*NumRecords;
    uint32 Code,PredCode=0;
    uint32 * Outp=(uint32 *)Output;
    uint32 * OutpEnd=Outp+NumRecords;
    while (Outp<OutpEnd)
    {
      uint32 Delta=*Input;
      Delta=(Delta<<8)|(*(Input+P3));
      Delta=(Delta<<8)|(*(Input+P2));
      Delta=(Delta<<8)|(*(Input+P1)); Input++;
      if (Delta&1) Delta=~(Delta>>1); else Delta>>=1;
      Code=Delta+PredCode; PredCode=Code;
      *Outp++=Code;
    }

    sint32 i=4*NumRecords;
    while (i<Size)
    {
      Output[i]=*(Input+P3);
      i++;Input++;
    }
  }

  if (Mode==1)
  {
    sint32 i;
    for(i=0;i<Size;i+=2) Output[i]=*(Input++);
    for(i=1;i<Size;i+=2) Output[i]=*(Input++);
  }

  if (Mode==2)
  {
    sint32 i;
    for(i=0;i<Size;i+=4) Output[i]=*(Input++);
    for(i=1;i<Size;i+=4) Output[i]=*(Input++);
    for(i=2;i<Size;i+=4) Output[i]=*(Input++);
    for(i=3;i<Size;i+=4) Output[i]=*(Input++);
  }

}

/*-------------------------------------------------*/
/* End                                 Rec_Flt.c.c */
/*-------------------------------------------------*/
