// C_LZ4.cpp - FreeArc/DArc interface to LZ4 / LZ4HC (lz4 v1.10.0)

extern "C" {
#include "C_LZ4.h"
}

// DArc LZ4 wire format version byte
#define LZ4_VERSION_BYTE 1

// The LZ4 codec is Rust-only: the vendored lz4.c/lz4hc.c (292 KB) are gone, so
// unlike most codecs here there is no DARC_NO_RUST fallback to guard against.
//
// The fast encoder and the decoder come from lz4_flex; the high-compression
// encoder is DArc's own port in rust/darc-codecs/src/lz4hc.rs. Deleting the C
// waited on that port precisely because lz4hc.c does `#include "lz4.c"` for
// shared internals, which made the two files a unit that had to go together.
//
// The block format is a fixed specification, so the Rust decoder reads every
// block the C library ever wrote; and for every level DArc can select, the HC
// port is byte-identical to the C encoder (rust/difftest/lz4hc-check.sh).
extern "C" int darc_rs_lz4_decompress_block  (const unsigned char *src, int srcSize, unsigned char *dst, int dstCap);
extern "C" int darc_rs_lz4_compress_block    (const unsigned char *src, int srcSize, unsigned char *dst, int dstCap);
extern "C" int darc_rs_lz4_compress_hc_block (const unsigned char *src, int srcSize, unsigned char *dst, int dstCap, int level);

// `LZ4_compressBound` (lz4.h). Kept as an inline formula rather than a call so
// the header does not have to survive: it is a fixed part of the format's
// worst case, not a tunable.
static inline int LZ4_compressBound (int isize)
{
  return isize + isize/255 + 16;
}

// sizeof(LZ4_stream_t) and sizeof(LZ4_streamHC_t) as the vendored v1.10.0
// reported them, measured rather than assumed.
//
// These are NOT free to update to whatever the Rust side happens to allocate.
// SetCompressionMem() subtracts the state size before splitting what is left
// into buffers, so the value decides BlockSize -- and BlockSize decides where
// block boundaries fall in the archive. Changing either number silently
// changes the bytes DArc writes.
#define LZ4_SIZEOF_STATE     16416
#define LZ4_SIZEOF_STATE_HC  262200

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
            OutSize = darc_rs_lz4_decompress_block (In, InSize, Out, BlockSize);
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
                ? darc_rs_lz4_compress_hc_block (In, InSize, Out, dstCap, Compressor)
                : darc_rs_lz4_compress_block    (In, InSize, Out, dstCap);
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
  return BlockSize*2 + (Compressor? LZ4_SIZEOF_STATE_HC : LZ4_SIZEOF_STATE);
}

void LZ4_METHOD::SetCompressionMem (MemSize mem)
{
  // Reserve ~256 KB for LZ4 state; rest split between in/out buffers
  MemSize state = Compressor? LZ4_SIZEOF_STATE_HC : LZ4_SIZEOF_STATE;
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
