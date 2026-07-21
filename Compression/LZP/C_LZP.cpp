/* Quick&dirty LZP compresion algorithm, developed by Dmitry Shkarin.
   Original code: http://www.compression.ru/ds/lzp.rar
   In turn, this code is based on LZP preprocessor in GRZipII compression
     algorithm, developed by Ilya Grebnov, Ilya.Grebnov@magicssoft.ru.
   Original code: http://magicssoft.ru/content/download/GRZipII/GRZipIISRC.zip
*/

extern "C" {
#include "C_LZP.h"
}


/* 32-bit Rotates */
#if defined(FREEARC_WIN)

/* intrinsic rotate */
#include <stdlib.h>
#pragma intrinsic(_lrotr,_lrotl)
#define ROR(x,n) _lrotr(x,n)

#elif !defined(__STRICT_ANSI__) && defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__)) && !defined(INTEL_CC) && !defined(LTC_NO_ASM)

static inline unsigned ROR(unsigned word, int i)
{
   asm ("rorl %%cl,%0"
      :"=r" (word)
      :"0" (word),"c" (i));
   return word;
}

#else

/* rotates the hard way */
#define ROR(x, y) ( ((((unsigned long)(x)&0xFFFFFFFFUL)>>(unsigned long)((y)&31)) | ((unsigned long)(x)<<(unsigned long)(32-((y)&31)))) & 0xFFFFFFFFUL)

#endif


/*------------------------------------------------------------------------------------*/
/* Compression/decompression methods that take and return data via memory buffers     */
/*------------------------------------------------------------------------------------*/

/*                          tuned for PPMd
static const BYTE MO2MML[4] = {5,11,19,44};
static inline UINT GetMinMatchLen(UINT MaxOrder) {
    return (MaxOrder < 6)?(MO2MML[MaxOrder-2]):(CLAMP(10*MaxOrder-15,51,475));
}
*/
enum { LZP_MATCH_FLAG=0xB5 };

static inline UINT& lzpC(BYTE* p) { return *(UINT*)(p-4); }
static inline UINT  lzpH(UINT c,BYTE* p,int HashMask) {
//    return (c+11*(c >> 15)+13*lzpC(p-1)) & HashMask;
    return (c+5*ROR(c,17)+3*lzpC(p-1)) & HashMask;
}
#define LZP_INIT(HashSize,Pattern)                                               \
    UINT i, k, n1=1, n=1, HashMask=HashSize-1;                                   \
    BYTE *p, *InEnd=In+Size, *OutStart=Out;                                      \
    BYTE **HTable = (BYTE**) malloc (HashSize * sizeof(BYTE*));                  \
    if (HTable==NULL)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                 \
    for (i=0;i < HashSize;i++)              HTable[i]=Pattern+5;                 \
    lzpC(Out+4)=lzpC(In+4);                 lzpC(Out+8)=lzpC(In+8);              \
    i=lzpC(Out += 12)=lzpC(In += 12);       k=lzpH(i,Out,HashMask);

#ifndef FREEARC_DECOMPRESS_ONLY
int LZPEncode(BYTE* In,UINT Size,BYTE* Out,int MinLen,int HashSize,int Barrier,int SmallestLen)
{
    BYTE* OutEnd=Out+Size;   if (Size<32)  return 0;
    LZP_INIT(HashSize,In);
    do {
        p=HTable[k];                        int ml;
        if ( !--n )  { HTable[k]=In;        n=n1; }
        if (i != lzpC(p))                   *Out++ = *In++;
        else if ((ml = In-p>Barrier? SmallestLen:MinLen), (In+ml <= InEnd && lzpC(p+ml) == lzpC(In+ml))) {
            for (i=4;In+i <= InEnd && lzpC(p+i) == lzpC(In+i);i += 4)
                    ;
            for (i -= 4;In+i < InEnd && In[i] == p[i];i++)
                    ;
            if (i < ml)                     goto MATCH_NOT_FOUND;
            HTable[k]=In;                   n1 += (In-p > (n1+1)*HashSize && n1 < 7);
            *Out++ = LZP_MATCH_FLAG;        In += (k=i);
            for (i -= ml;i>=254 && Out<OutEnd;i -= 254)
                    *--OutEnd = 0;
            *--OutEnd = i+1;
            while(int(k -= 2*n1+1) > 0)     HTable[lzpH(lzpC(In-k),In-k,HashMask)]=In-k;
        } else {
MATCH_NOT_FOUND:
            if ((*Out++ = *In++) == LZP_MATCH_FLAG)
                    *--OutEnd = 255;
        }
        k=lzpH(i=lzpC(In),In,HashMask);
    } while (In<InEnd && Out<OutEnd);
    free(HTable);
    if (Out >= OutEnd)       return 0;
    memmove(Out,OutEnd,OutStart+Size-OutEnd);
    return Size-(OutEnd-Out);
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

int LZPDecode(BYTE* In,UINT Size,BYTE* Out,int MinLen,int HashSize,int Barrier,int SmallestLen,UINT OutSize)
{
    // LZP_INIT reads In[0..11] unconditionally -- the 12-byte header copied to
    // the output plus the first context word. A block shorter than that (a
    // truncated archive is the common way to get one) made it read past the
    // input buffer. Reject it; valid compressed blocks always carry the full
    // header, so this is transparent to real data.
    if (Size < 16)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    LZP_INIT(HashSize,Out);
    // OutSize is the capacity of the Out buffer. Everything below is bounded
    // against it and against InEnd so a corrupt or hostile block is rejected
    // rather than over-reading the input or over-writing the output. Valid
    // blocks satisfy every bound by construction, so this is transparent.
    //
    // The input forward pointer is already safe: In advances by at most one per
    // iteration (the leftmost "*In++" always runs) and the do/while re-checks
    // In<InEnd, so it can never pass InEnd. Two things were unbounded -- the
    // backward length walk (InEnd stepping down with no floor) and the output
    // writes -- and each gets a bound below. Bounding the output length also
    // removes a denial-of-service: a match length of 0 made "while(--i)" wrap
    // to 4G and copy a byte at a time for minutes before finally faulting.
    BYTE *OutEnd = OutStart + OutSize;
    do {
        p=HTable[k];
        if ( !--n )  { HTable[k]=Out;       n=n1; }
        if (*In++ != LZP_MATCH_FLAG || i != lzpC(p) || *--InEnd == 255) {
                if (Out >= OutEnd)  {free(HTable); return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;}
                *Out++ = In[-1];
        } else {
            HTable[k]=Out;                  n1 += (Out-p > (n1+1)*HashSize && n1 < 7);
            // "InEnd > In" keeps the backward length walk from stepping below
            // the forward pointer (and thus below the buffer). It never fires
            // on valid data, whose length bytes always sit above In.
            for (i=(Out-p>Barrier? SmallestLen:MinLen)-1;InEnd > In && *InEnd == 0;InEnd--)
                    i += 254;
            i += *InEnd;                    k=2*n1+2;
            // The copy writes i bytes to Out and reads i bytes from p (an
            // earlier output position, so p+i stays below OutEnd once Out+i
            // does). Reject i==0 (would wrap --i) and any length that would run
            // past OutEnd. UINT compare avoids pointer overflow on a huge i.
            if (i == 0 || i > (UINT)(OutEnd - Out))  {free(HTable); return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;}
            do {
                if ( !--k ) { k=2*n1+1;     HTable[lzpH(lzpC(Out),Out,HashMask)]=Out; }
                *Out++ = *p++;
            } while ( --i );
        }
        k=lzpH(i=lzpC(Out),Out,HashMask);
    } while (In < InEnd);
    free(HTable);                           return (Out-OutStart);
}



/*-------------------------------------------------------------------------*/
/* Compression/decompression methods that use callbacks for I/O            */
/*-------------------------------------------------------------------------*/

#ifndef FREEARC_DECOMPRESS_ONLY
// DARC_RUST=1 selects the Rust port of this codec (rust/darc-codecs).
//
// Excluded rather than redeclared: with both definitions present the linker
// resolves from this object and never pulls the Rust one, producing a binary
// that looks correctly linked while running the C code. Same arrangement as
// C_Delta.cpp and C_Dict.cpp.
//
// LZPEncode/LZPDecode above stay compiled -- they are only reachable through
// these two entry points. Verified byte-identical over 8 inputs in both
// directions; see rust/difftest.
#ifndef DARC_RUST
int lzp_compress (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;   // Error code returned by last operation or FREEARC_OK
    BYTE* In = NULL;  // pointer to the input data
    BYTE* Out= NULL;  // pointer to the output data
    while (1)
    {
        int InSize, OutSize;     // number of bytes in the input and output buffers, respectively
        MALLOC (BYTE, In, BlockSize+2);
    	READ_LEN_OR_EOF (InSize, In, BlockSize);
        In = (BYTE*) realloc(In,InSize);
        MALLOC (BYTE, Out, InSize+2);
        OutSize = LZPEncode (In, InSize, Out, MinMatchLen, 1<<HashSizeLog, Barrier, SmallestLen);
        if (OutSize<0)  {errcode=OutSize; goto finished;}
        if (OutSize==0 || (MinCompression>0 && OutSize/MinCompression>=InSize/100)) {
            // Failed to compress the data [well enough], so store the original data instead
            FreeAndNil(Out);
            WRITE4 (-InSize);      // A negative number as the block length marks a Stored block
            WRITE  (In, InSize);
            FreeAndNil(In);
        } else {
            // The data was compressed successfully; we can free the input buffer before writing it out
            // (to free up more memory for the next algorithm in the compression chain)
            FreeAndNil(In);
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
            FreeAndNil(Out);
        }
    }
finished:
    FreeAndNil(In); FreeAndNil(Out);
    return errcode;
}
#endif  // !DARC_RUST
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


#ifndef DARC_RUST
int lzp_decompress (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;   // Error code returned by last operation or FREEARC_OK
    BYTE* In = NULL;  // pointer to the input data
    BYTE* Out= NULL;  // pointer to the output data
    for(;;) {
        int InSize, OutSize;     // number of bytes in the input and output buffers, respectively
        READ4_OR_EOF (InSize);
        if (InSize<0) {
            // copy the uncompressed data
            InSize = -InSize;
            MALLOC (BYTE, In, InSize);
            READ  (In, InSize);
            WRITE (In, InSize);
            FreeAndNil(In);
        } else {
            // Decode and obtain the size of the output data
            MALLOC (BYTE, In,  InSize);
            MALLOC (BYTE, Out, BlockSize);
            READ  (In, InSize);
            OutSize = LZPDecode (In, InSize, Out, MinMatchLen, 1<<HashSizeLog, Barrier, SmallestLen, BlockSize);
            if (OutSize < 0)  {errcode = OutSize; goto finished;}   // reject a block LZPDecode refused
            FreeAndNil(In);
            Out = (BYTE*) realloc (Out, OutSize);
            WRITE (Out, OutSize);
            FreeAndNil(Out);
        }
    }
finished:
    FreeAndNil(In); FreeAndNil(Out);
    return errcode;
}
#endif  // !DARC_RUST


/*-------------------------------------------------*/
/* LZP_METHOD class implementation                 */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method's parameters
LZP_METHOD::LZP_METHOD()
{
  BlockSize      = 8*mb;
  MinCompression = 100;
  MinMatchLen    = 64;
  HashSizeLog    = 18;
  Barrier        = INT_MAX;
  SmallestLen    = 32;
}

// Decompression function
int LZP_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("lzp_decompress");
  if (!f) f = (FARPROC) lzp_decompress;

  return ((int (*)(MemSize, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
            (BlockSize, MinCompression, MinMatchLen, HashSizeLog, Barrier, SmallestLen, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int LZP_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("lzp_compress");
  if (!f) f = (FARPROC) lzp_compress;

  return ((int (*)(MemSize, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
            (BlockSize, MinCompression, MinMatchLen, HashSizeLog, Barrier, SmallestLen, callback, auxdata);
}

// Set the block size and reduce the hash size if it's too large for such a small block
void LZP_METHOD::SetBlockSize (MemSize bs)
{
  if (bs>0) {
    BlockSize   = bs;
    HashSizeLog = mymin (HashSizeLog, 1+lb(BlockSize-1));
  }
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_LZP)
void LZP_METHOD::ShowCompressionMethod (char *buf)
{
    LZP_METHOD defaults; char BlockSizeStr[100], MinCompressionStr[100], BarrierTempStr[100], BarrierStr[100], SmallestLenStr[100];
    showMem (BlockSize, BlockSizeStr);
    showMem (Barrier,   BarrierTempStr);
    sprintf (MinCompressionStr, MinCompression!=defaults.MinCompression? ":%d%%" : "", MinCompression);
    sprintf (BarrierStr, Barrier!=defaults.Barrier? ":d%s" : "", BarrierTempStr);
    sprintf (SmallestLenStr, SmallestLen!=defaults.SmallestLen? ":s%d" : "", SmallestLen);
    sprintf (buf, "lzp:%s%s:%d:h%d%s%s", BlockSizeStr, MinCompressionStr, MinMatchLen, HashSizeLog, BarrierStr, SmallestLenStr);
}

// Sets the amount of memory that should be used for compression and decompression
void LZP_METHOD::SetCompressionMem (MemSize mem)
{
  MemSize hashsize = (1<<HashSizeLog) * sizeof(BYTE*);
  // If the hash takes up too much space, shrink it first. That may turn out to be enough
  if (hashsize > mem/4) {
    HashSizeLog = lb(mem/16);
    if (GetCompressionMem() <= mem)  return;
    hashsize = (1<<HashSizeLog) * sizeof(BYTE*);
  }
  SetBlockSize ((mem-hashsize)/2);
}


#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an LZP_METHOD object with the given compression parameters,
// or returns NULL if this is a different compression method or the parameters are invalid
COMPRESSION_METHOD* parse_LZP (char** parameters)
{
  if (strcmp (parameters[0], "lzp") == 0) {
    // If the method name (parameter zero) is "lzp", parse the remaining parameters

    LZP_METHOD *p = new LZP_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all the method's parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Parameters that carry values
        case 'b':  p->BlockSize   = parseMem (param+1, &error); continue;
        case 'l':  p->MinMatchLen = parseInt (param+1, &error); continue;
        case 'h':  p->HashSizeLog = parseInt (param+1, &error); continue;
        case 'd':  p->Barrier     = parseMem (param+1, &error); continue;
        case 's':  p->SmallestLen = parseInt (param+1, &error); continue;
      }
      // If the parameter ends with a percent sign, try to parse it as "N%"
      if (last_char(param) == '%') {
        char str[100]; strcpy(str,param); last_char(str) = '\0';
        int n = parseInt (str, &error);
        if (!error) { p->MinCompression = n; continue; }
        error=0;
      }
      // We get here if the parameter doesn't specify its name.
      // If this parameter can be parsed as an integer (i.e. it contains only digits),
      // assign its value to the MinMatchLen field, otherwise try to parse it as BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinMatchLen = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method's parameters
    return p;
  } else
    return NULL;   // This is not the lzp method
}

static int LZP_x = AddCompressionMethod (parse_LZP);   // Register the LZP method parser
