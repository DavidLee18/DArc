// C_LZ4.cpp - FreeArc/DArc interface to LZ4 / LZ4HC (lz4 v1.10.0)

extern "C" {
#include "C_LZ4.h"
#include "lz4.c"
#ifndef FREEARC_DECOMPRESS_ONLY
#include "lz4hc.c"
#endif
}

// DArc LZ4 wire format version byte
#define LZ4_VERSION_BYTE 1

int LZ4_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;
    BYTE* In = NULL;
    BYTE* Out= NULL;
    MALLOC (BYTE, In,  BlockSize);
    MALLOC (BYTE, Out, BlockSize);
    int len; READ_LEN_OR_EOF (len, In, 1);
    if (len!=1 || *In!=LZ4_VERSION_BYTE)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    for(;;) {
        int InSize, OutSize;
        READ4_OR_EOF (InSize);
        if (InSize<0) {
            InSize = -InSize;
            READ  (In, InSize);
            WRITE (In, InSize);
        } else {
            READ  (In, InSize);
            OutSize = LZ4_decompress_safe ((const char*)In, (char*)Out, InSize, BlockSize);
            if (OutSize<0)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            WRITE (Out, OutSize);
        }
    }
finished:
    FreeAndNil(In); FreeAndNil(Out);
    return errcode;
}

#ifndef FREEARC_DECOMPRESS_ONLY

int LZ4_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;
    BYTE* In = NULL;
    BYTE* Out= NULL;
    int dstCap = LZ4_compressBound(BlockSize);
    MALLOC (BYTE, In,  BlockSize);
    MALLOC (BYTE, Out, dstCap);
    for (bool FirstTime=true;;FirstTime=false)
    {
        int InSize, OutSize;
        READ_LEN_OR_EOF (InSize, In, BlockSize);
        if (FirstTime) {BYTE v = LZ4_VERSION_BYTE;  WRITE (&v, 1);}
        OutSize = Compressor
                ? LZ4_compress_HC      ((const char*)In, (char*)Out, InSize, dstCap, Compressor)
                : LZ4_compress_default ((const char*)In, (char*)Out, InSize, dstCap);
        if (OutSize<=0  ||  (MinCompression>0 && OutSize >= (double(InSize)*MinCompression)/100)) {
            // Stored (uncompressible) block: signal with negative length
            WRITE4 (-InSize);
            WRITE  (In, InSize);
        } else {
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
        }
    }
finished:
    FreeAndNil(In); FreeAndNil(Out);
    return errcode;
}

MemSize LZ4_METHOD::GetCompressionMem()
{
  return BlockSize*2 + (Compressor? LZ4_sizeofStateHC() : LZ4_sizeofState());
}

void LZ4_METHOD::SetCompressionMem (MemSize mem)
{
  // Reserve ~256 KB for LZ4 state; rest split between in/out buffers
  MemSize state = Compressor? LZ4_sizeofStateHC() : LZ4_sizeofState();
  MemSize avail = (mem > state + 2*kb) ? (mem - state) / 2 : 64*kb;
  if (avail < 64*kb) avail = 64*kb;           // sanity floor
  if (avail > 256*mb) avail = 256*mb;         // sanity ceiling
  BlockSize = avail;
}

void LZ4_METHOD::ShowCompressionMethod (char *buf)
{
  LZ4_METHOD defaults; char BlockSizeStr[100], CompressorStr[100], MinCompressionStr[100];
  showMem (BlockSize, BlockSizeStr);
  sprintf (CompressorStr,     Compressor    !=defaults.Compressor?     ":c%d"  : "", Compressor);
  sprintf (MinCompressionStr, MinCompression!=defaults.MinCompression? ":%d%%" : "", MinCompression);
  sprintf (buf, "lz4%s%s%s%s",
                    CompressorStr,
                    BlockSize!=defaults.BlockSize? ":b"         : "",
                    BlockSize!=defaults.BlockSize? BlockSizeStr : "",
                    MinCompressionStr);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


LZ4_METHOD::LZ4_METHOD()
{
  Compressor     = 0;
  BlockSize      = 1*mb;
  HashSize       = 0;
  MinCompression = 100;
}

COMPRESSION_METHOD* parse_LZ4 (char** parameters)
{
  if (strcmp (parameters[0], "lz4") == 0) {
    LZ4_METHOD *p = new LZ4_METHOD;
    int error = 0;

    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strequ(param,"hc"))  {p->Compressor = 9; continue;}
      else switch (*param) {
        case 'c':  p->Compressor= parseInt (param+1, &error); continue;
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
        case 'h':  p->HashSize  = parseMem (param+1, &error); continue;
      }
      if (last_char(param) == '%') {
        char str[100]; strcpy(str,param); last_char(str) = '\0';
        int n = parseInt (str, &error);
        if (!error) { p->MinCompression = n; continue; }
        error=0;
      }
      error=1;
    }
    if (error)  {delete p; return NULL;}
    return p;
  } else
    return NULL;
}

static int LZ4_x = AddCompressionMethod (parse_LZ4);
