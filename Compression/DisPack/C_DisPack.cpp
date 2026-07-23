// DisPack codec. Originally by Bulat Ziganshin, from FreeArc 0.67
// (March 2014, formerly http://freearc.org); ported into DArc.
// The comments below arrived corrupted and were restored from the
// upstream 0.67 sources and translated.

extern "C" {
#include "C_DisPack.h"
}

// Compatibility shims for macros that exist in FreeArc 0.67 but not in DArc.
#ifndef BIGALLOC
#define BIGALLOC(type, ptr, size)                                          \
{                                                                          \
    (ptr) = (type*) BigAlloc ((size) * sizeof(type));                      \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}
#endif

#ifndef READ_LEN
#define READ_LEN(len, buf, size)                                           \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) < 0) {      \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}
#endif

#ifndef BigFreeAndNil
#define BigFreeAndNil(p)         ((p) && (BigFree(p), (p)=NULL))
#endif

// Big-endian load/store helpers used by DisPack.cpp. Present in FreeArc 0.67
// Common.h but absent in DArc.
static inline uint16 value16b (void *p) {
  uint8 *m = (uint8 *)p; return (m[0] << 8) + m[1];
}
static inline uint32 value32b (void *p) {
  uint8 *m = (uint8 *)p;
  return (m[0] << 24) + (m[1] << 16) + (m[2] << 8) + m[3];
}
static inline void setvalue16b (void *p, uint32 x) {
  uint8 *m = (uint8 *)p; m[0] = x >> 8; m[1] = x;
}
static inline void setvalue32b (void *p, uint32 x) {
  uint8 *m = (uint8 *)p;
  m[0] = x >> 24; m[1] = x >> 16; m[2] = x >> 8; m[3] = x;
}

#define DISPACK_LIBRARY
#include "DisPack.cpp"

/*-------------------------------------------------*/
/* Implementation of the DISPACK_METHOD class      */
/*-------------------------------------------------*/

// Constructor assigning default values to the compression method's parameters
DISPACK_METHOD::DISPACK_METHOD()
{
    BlockSize      = 8*mb;
    ExtendedTables = 0;
}

enum {TAG_DATA = 0xC71B3AE1, TAG_EXE};
bool is_tag (unsigned x)  {return (x^TAG_DATA) < 0x10;}

// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// The decode logic is split out of DISPACK_METHOD::decompress into this free
// function so the switch is a link-time symbol replacement, matching the other
// codecs. It is declared in C_DisPack.h, which this file includes inside its
// extern "C" block, so it and the Rust export are the same C-linkage symbol.
// With both present the linker resolves from this object and never pulls the
// Rust one -- so the switch must remove this definition, not merely add a
// declaration elsewhere. DisUnFilter and the rest of the codec stay compiled.
//
// Verified byte-identical over real i386 code across three block sizes; see
// rust/difftest/dispack-check.sh.
#ifndef DARC_RUST
int dispack_decompress (MemSize BlockSize, CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // Pointers to the input and output data, respectively
    uint  BaseAddress = 1u<<30;
    int   CHUNK_SIZE, InBufferSize = BlockSize+BlockSize/4+1024;
    READ4_OR_EOF (CHUNK_SIZE);
    if (CHUNK_SIZE > BlockSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    BIGALLOC (BYTE, In,  InBufferSize+2);
    BIGALLOC (BYTE, Out, BlockSize+2);
    for(;;) {
        int tag;
        READ4_OR_EOF (tag);
        if (!is_tag(tag) || tag==TAG_DATA) {
            // copy the uncompressed data; we may already have read 4 bytes of it ;)
            int done = 0, len;
            if (tag==TAG_DATA) {
              READ4 (len);
            } else {
              done = 4;
              len = CHUNK_SIZE;
              setvalue32 (In, tag);
            }
            READ  (In+done, len-done);
            WRITE (In, len);
            BaseAddress += len;
        } else if (tag==TAG_EXE) {
            int InSize, OutSize;     // number of bytes in the input and output buffers, respectively
            // Perform the decoding and obtain the size of the output data
            READ4 (OutSize);
            READ4 (InSize);
            if (OutSize > BlockSize  ||  InSize > InBufferSize)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            READ (In, InSize);
            bool success = DisUnFilter (In, InSize, Out, OutSize, BaseAddress);
            if (!success)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            WRITE (Out, OutSize);
            BaseAddress += OutSize;
        } else {
            ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        }
        if (BaseAddress >= 3u<<30)  BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); BigFreeAndNil(Out);
    return errcode;
}
#endif  // !DARC_RUST

// Decompression function
int DISPACK_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    return dispack_decompress (BlockSize, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

enum EXETYPE {EXETYPE_UNKNOWN, EXETYPE_DATA, EXETYPE_EXE};

EXETYPE detect (BYTE *buf, int len)
{
  int e8=0, exe=0, obj=0;
  for (BYTE *p=buf; p+5<buf+len; p++)
  {
    if (*p == 0xE8)
    {
      e8++;
      if (p[4]==0xFF && p[5]!=0xFF)
        exe++;
      if (p[4]==0    && p[5]!=0)
        obj++;
    }
  }
  // printf("  e8 %d, exe %d, obj %d, len %d\n", e8, exe, obj, len);
  return double(e8)/len >= 0.002   &&   double(exe+obj)/e8 >= 0.20  &&   double(exe)/e8 >= 0.01?  EXETYPE_EXE : EXETYPE_DATA;
}

// Compression function
int DISPACK_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    int   errcode = FREEARC_OK;     // Error code returned by last operation or FREEARC_OK
    BYTE *In = NULL,  *Out = NULL;  // Pointers to the input and output data, respectively
    int   InSize;  uint32 OutSize;  // Number of bytes in the input and output buffers, respectively
    uint  BaseAddress = 1u<<30;
    const int CHUNK_SIZE = 16*kb;
    bool  first_time = TRUE;
    BIGALLOC (BYTE, In, BlockSize+2);
    for(;;)
    {
        // Read the file in 16 kb blocks until the executable code runs out
        BYTE *p = In;  int len;
        do {
            READ_LEN (len, p, CHUNK_SIZE);
            if (len==0) break;
            EXETYPE exe_type = detect (p, len);
            if (exe_type!=EXETYPE_EXE) break;
            p += len, len = 0;
        } while (p-In <= BlockSize-CHUNK_SIZE);

        InSize = p-In;
        if (InSize+len == 0)  break;
        if (first_time)   WRITE4 (CHUNK_SIZE);  first_time = FALSE;

        if (InSize)
        {
            // Encode the executable code
            Out = DisFilter(In, InSize, BaseAddress, OutSize);
            if (Out==NULL)  ReturnErrorCode(FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);
            WRITE4 (TAG_EXE);
            WRITE4 (InSize);
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
            free (Out);
        }
        if (len)
        {
            // Encode the remaining data
            if (len!=CHUNK_SIZE  ||  is_tag(value32(p))) {
                WRITE4 (TAG_DATA);
                WRITE4 (len);
            }
            WRITE (p, len);
        }
        if ((BaseAddress += InSize+len)  >=  3u<<30)   BaseAddress -= 2u<<30;
    }
finished:
    BigFreeAndNil(In); //BigFreeAndNil(Out);
    return errcode;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_DISPACK)
void DISPACK_METHOD::ShowCompressionMethod (char *buf)
{
    DISPACK_METHOD defaults; char BlockSizeStr[100]=":";
    showMem (BlockSize, BlockSizeStr+1);
    sprintf (buf, "dispack070%s%s", BlockSize!=defaults.BlockSize? BlockSizeStr:"", ExtendedTables? ":x":"");
}

// Constructs a DISPACK_METHOD object with the given compression parameters
// or returns NULL if this is a different compression method, or a parameter is malformed
COMPRESSION_METHOD* parse_DISPACK (char** parameters)
{
  if (strcmp (parameters[0], "dispack") == 0
   || strcmp (parameters[0], "dispack070") == 0) {
    // If the method name (parameter zero) is "dispack", parse the remaining parameters

    DISPACK_METHOD *p = new DISPACK_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all the method's parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 'x':  p->ExtendedTables = 1; continue;
      }
      switch (*param) {                    // Parameters carrying values
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
      }
      // We get here when the parameter does not state its name
      // If this parameter can be parsed as an amount of memory,
      // then assign its value to the BlockSize field
      p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method's parameters
    return p;
  } else
    return NULL;   // This is not the DISPACK method
}

static int DISPACK_x = AddCompressionMethod (parse_DISPACK);   // Register the DISPACK method parser

