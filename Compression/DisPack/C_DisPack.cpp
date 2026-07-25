// DisPack codec. Originally by Bulat Ziganshin, from FreeArc 0.67
// (March 2014, formerly http://freearc.org); ported into DArc.
// The comments below arrived corrupted and were restored from the
// upstream 0.67 sources and translated.

extern "C" {
#include "C_DisPack.h"
}

// DisPack is Rust-only: the whole compress loop -- chunked reads, detect(),
// the filter and the tagged-chunk framing -- lives in
// rust/darc-codecs/src/dispack/encode.rs, and the decoder in dispack/filter.rs.
// Unlike most codecs here there is no DARC_NO_RUST fallback left to guard.
extern "C" int darc_rs_dispack_compress (MemSize BlockSize, CALLBACK_FUNC *callback, void *auxdata);

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

// DisPack.cpp is gone: both directions are Rust now (dispack/filter.rs and
// dispack/encode.rs), verified byte-identical to the C over 108 filter
// comparisons, 27 detect classifications and three whole-archive method
// variants. Only the COMPRESSION_METHOD plumbing and the parser stay here.

/*-------------------------------------------------*/
/* Implementation of the DISPACK_METHOD class      */
/*-------------------------------------------------*/

// Constructor assigning default values to the compression method's parameters
DISPACK_METHOD::DISPACK_METHOD()
{
    BlockSize      = 8*mb;
    ExtendedTables = 0;
}


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
    // The Rust driver owns the entire loop: chunked reads, detect(), the
    // filter, and the tagged-chunk framing. Verified byte-identical to the C
    // over 108 filter comparisons and 27 detect classifications
    // (rust/difftest/dispack-filter-check.sh), and the `dispack` fingerprint
    // is unchanged.
    return darc_rs_dispack_compress (BlockSize, callback, auxdata);
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

