/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor               BWT.c */
/* BWT Sorting Functions                           */
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

  "Fast" BWT memory use   : 6*BlockLen + 256Kb
  "Strong" BWT memory use : 9*BlockLen + 1Mb
  Reverse BWT memory use  : 5*BlockLen

  For more information on these sources, see the manual.
--*/

#include <stdio.h>
#include "libGRZip.h"

#define BWT_MaxByte                 256
#define BWT_MaxWord                 65536
#define BWT_MinQSort                20
#define BWT_MedTrh                  96

#define BWT_SPush(L,H,D)            {Stack[sp]=L;Stack[sp+1]=H;Stack[sp+2]=D;sp+=3;}
#define BWT_SPop(L,H,D)             {sp-=3;L=Stack[sp];H=Stack[sp+1];D=Stack[sp+2];}
#define BWT_Swap(SC1,SC2)           {sint32 Stmp=SC1;SC1=SC2;SC2=Stmp;}
#define BWT_Min(A,B)                ((A) < (B)) ? (A): (B)

#define FastBWT_RepTreshStep2       0.35
#define FastBWT_RepTreshStep4       1.15

#define FastBWT_MaxQSortDepth       32
#define FastBWT_QSortStackSize      1024
#define FastBWT_NumOverShoot        80
#define FastBWT_NumGroups           8
#define FastBWT_NSize(Ns)           (NextHi[Ns]-NextLo[Ns])

#define FastBWT_NSwap(N1,N2)                                        \
{                                                                   \
  sint32 Ntmp;                                                      \
  Ntmp=NextLo[N1];NextLo[N1]=NextLo[N2];NextLo[N2]=Ntmp;            \
  Ntmp=NextHi[N1];NextHi[N1]=NextHi[N2];NextHi[N2]=Ntmp;            \
  Ntmp=NextDepth[N1];NextDepth[N1]=NextDepth[N2];NextDepth[N2]=Ntmp;\
}


#define StrongBWT_Flag              0x40000000

#define StrongBWT_TSortStackSize    3*65536
#define StrongBWT_BFreq(B)          (Buckets[(B+1) << 8] - Buckets[B << 8])
#define StrongBWT_CMask             0x3FFFFFFF
#define StrongBWT_SMask             0x40000000

#define BWT_VSwap(S1,S2,Num)               \
{                                          \
  sint32 TS1=(S1);                         \
  sint32 TS2=(S2);                         \
  sint32 TNum=(Num);                       \
  while (TNum)                             \
  {                                        \
    BWT_Swap(Index[TS1],Index[TS2]);       \
    TS1++;TS2++;TNum--;                    \
  }                                        \
}

// The encoder (the BWT (both the fast tournament sort and the strong induced sort)) lived here and is now Rust: see
// rust/darc-codecs/src/grzip/. Verified byte-identical to this C across the
// stage matrix and the multi-block stream -- rust/difftest/grzip-check.sh and
// grzip-stage-check.sh compare the produced streams, and the archiver's grzip
// fingerprint is unchanged.
//
// The DECODER below stays. Unarc builds these files with
// -DFREEARC_DECOMPRESS_ONLY and does NOT link the Rust crate, so the standalone
// extractor and the SFX modules still need it. It goes when Unarc does.


sint32 GRZip_StrongBWT_Decode(uint8 * Input,sint32 Size,sint32 FBP)
{
  uint32 Count[BWT_MaxByte];
  sint32 i;
  uint32 Sum;

  uint32 * T=(uint32 *)BigAlloc((Size+1)*sizeof(uint32));
  if (T==NULL) return (GRZ_NOT_ENOUGH_MEMORY);

  memset(Count,0,BWT_MaxByte*sizeof(uint32));

  for (i=0;i<FBP;i++)
  {
    uint8 c=Input[i];
    T[i]=((Count[c]++)<<8)|c;
  }

  for (i=FBP;i<Size;i++)
  {
    uint8 c=Input[i];
    T[i+1]=((Count[c]++)<<8)|c;
  }

  for (Sum=1,i=0;i<BWT_MaxByte;i++){Sum+=Count[i];Count[i]=Sum-Count[i];}

  for (FBP=0,i=Size-1;i>=0;i--)
  {
    uint32 u=T[FBP];
    uint8  c=u&0xFF;
    FBP=(u>>8)+Count[c];
    Input[i]=c;
  }

  BigFree(T);
  return GRZ_NO_ERROR;
}

// The encoder (the BWT (both the fast tournament sort and the strong induced sort)) lived here and is now Rust: see
// rust/darc-codecs/src/grzip/. Verified byte-identical to this C across the
// stage matrix and the multi-block stream -- rust/difftest/grzip-check.sh and
// grzip-stage-check.sh compare the produced streams, and the archiver's grzip
// fingerprint is unchanged.
//
// The DECODER below stays. Unarc builds these files with
// -DFREEARC_DECOMPRESS_ONLY and does NOT link the Rust crate, so the standalone
// extractor and the SFX modules still need it. It goes when Unarc does.


sint32 GRZip_FastBWT_Decode(uint8 * Input,sint32 Size,sint32 FBP)
{
  uint32 Count[BWT_MaxByte];
  sint32 i;
  uint32 Sum;

  uint32 * T=(uint32 *)BigAlloc(Size*sizeof(uint32));
  if (T==NULL) return (GRZ_NOT_ENOUGH_MEMORY);

  memset(Count,0,BWT_MaxByte*sizeof(uint32));

  for (i=0;i<Size;i++)
  {
    uint8 c=Input[i];
    T[i]=((Count[c]++)<<8)|c;
  }

  for (Sum=0,i=0;i<BWT_MaxByte;i++){Sum+=Count[i];Count[i]=Sum-Count[i];}

  for (i=Size-1;i>=0;i--)
  {
    uint32 u=T[FBP];
    uint8  c=u&0xFF;
    FBP=(u>>8)+Count[c];
    Input[i]=c;
  }

  BigFree(T);
  return GRZ_NO_ERROR;
}

// The encoder (the BWT (both the fast tournament sort and the strong induced sort)) lived here and is now Rust: see
// rust/darc-codecs/src/grzip/. Verified byte-identical to this C across the
// stage matrix and the multi-block stream -- rust/difftest/grzip-check.sh and
// grzip-stage-check.sh compare the produced streams, and the archiver's grzip
// fingerprint is unchanged.
//
// The DECODER below stays. Unarc builds these files with
// -DFREEARC_DECOMPRESS_ONLY and does NOT link the Rust crate, so the standalone
// extractor and the SFX modules still need it. It goes when Unarc does.


sint32 GRZip_BWT_Decode(uint8 * Input,sint32 Size,sint32 FBP)
{
  // FBP is the position of the original first byte, taken straight from the
  // block header and never checked, so an out-of-range value indexed off the
  // end of the inverse transform's table. The two variants differ in what is
  // legal: FastBWT reads T[FBP] from a table of exactly Size entries, while
  // StrongBWT only uses FBP to split its fill loops over a Size+1 entry table,
  // so FBP==Size is valid there. Reject anything outside those ranges.
  if (Size<=0)  return (GRZ_CRC_ERROR);

  if ((FBP&StrongBWT_Flag)==0)
  {
    if (FBP<0 || FBP>=Size)  return (GRZ_CRC_ERROR);
    return (GRZip_FastBWT_Decode(Input,Size,FBP));
  }
  else
  {
    sint32 RealFBP = FBP&(~StrongBWT_Flag);
    if (RealFBP<0 || RealFBP>Size)  return (GRZ_CRC_ERROR);
    return (GRZip_StrongBWT_Decode(Input,Size,RealFBP));
  }
}

#undef BWT_MaxByte
#undef BWT_MaxWord
#undef BWT_MinQSort
#undef BWT_MedTrh
#undef BWT_SPush
#undef BWT_SPop
#undef BWT_Swap
#undef BWT_Min
#undef BWT_VSwap

#undef FastBWT_RepTreshStep2
#undef FastBWT_RepTreshStep4
#undef FastBWT_MaxQSortDepth
#undef FastBWT_QSortStackSize
#undef FastBWT_NumOverShoot
#undef FastBWT_NumGroups

#undef StrongBWT_Flag
#undef StrongBWT_TSortStackSize
#undef StrongBWT_BFreq
#undef StrongBWT_CMask
#undef StrongBWT_SMask

/*-------------------------------------------------*/
/* End                                       BWT.c */
/*-------------------------------------------------*/
