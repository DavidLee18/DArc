#ifndef FREEARC_COMPRESSION_H
#define FREEARC_COMPRESSION_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <time.h>

#include "Common.h"


#ifdef __cplusplus
extern "C" {
#endif

// Signature of files made by my utilities
#define BULAT_ZIGANSHIN_SIGNATURE 0x26351817

// Constants for conveniently writing memory amounts
#define b_ (1u)
#define kb (1024*b_)
#define mb (1024*kb)
#define gb (1024*mb)
#define terabyte (1024*uint64(gb))

// Number of bytes that should be read/written at a time in all compressors
#define BUFFER_SIZE (256*kb)

// Number of bytes that should be read/written at a time in the fast methods and when decompressing asymmetric algorithms
#define LARGE_BUFFER_SIZE (256*kb)

// Block size that BigAlloc can usually allocate using Large Pages
#define BIGALLOC_BUFFER_SIZE (2*mb)

// Number of bytes that should be read/written at a time in the very fast methods (storing, tornado and the like)
// This amount minimizes the losses on disk seek operations - provided that no I/O is happening in another thread at the same time ;)
#define HUGE_BUFFER_SIZE (8*mb)

// How often progress in compression/decompression should be reported
#define PROGRESS_CHUNK_SIZE (64*kb)

// Additional definitions that make it convenient to write parsers for compression method strings
#define COMPRESSION_METHODS_DELIMITER            '+'   /* Separator between compression algorithms in the string description of a compressor */
#define COMPRESSION_METHOD_PARAMETERS_DELIMITER  ':'   /* Separator between parameters in the string description of a compression method */
#define MAX_COMPRESSION_METHODS    1000        /* Must be no less than the number of compression methods registered via AddCompressionMethod */
#define MAX_PARAMETERS             200         /* Must be no less than the maximum number of parameters (separated by colons) that a compression method may have */
#define MAX_COMPRESSOR_STRLEN      2048        /* Maximum length of a string describing a compressor */
#define MAX_METHOD_STRLEN          512         /* Maximum length of a string describing a compression method */
#define MAX_METHODS_IN_COMPRESSOR  100         /* Maximum number of methods in a single compressor */
#define MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH 2048  /* Maximum length of the [External compressor] section */


// ****************************************************************************************************************************
// DATA READ/WRITE HELPERS FOR COMPRESSION METHODS ****************************************************************************
// ****************************************************************************************************************************

// Callback function type
typedef int CALLBACK_FUNC (const char *what, void *data, int size, void *auxdata);

// Macros for reading/writing the in(out)put streams, checking that exactly as much data was transferred as was requested
#define checked_read(ptr,size)         {if ((x = callback("read" ,ptr,size,auxdata)) != size) {x>=0 && (x=FREEARC_ERRCODE_READ);  goto finished;}}
#define checked_write(ptr,size)        {if ((x = callback("write",ptr,size,auxdata)) != size) {x>=0 && (x=FREEARC_ERRCODE_WRITE); goto finished;}}
// Macro for reading input streams with checks for errors and end of input data
#define checked_eof_read(ptr,size)     {if ((x = callback("read", ptr,size,auxdata)) != size) {x>0  && (x=FREEARC_ERRCODE_READ);  goto finished;}}

// Auxiliary code to read/write data blocks and 4-byte headers
#define INIT() callback ("init", NULL, 0, auxdata)
#define DONE() callback ("done", NULL, 0, auxdata)

#define MALLOC(type, ptr, size)                                            \
{                                                                          \
    (ptr) = (type*) malloc ((size) * sizeof(type));                        \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}

#define BIGALLOC(type, ptr, size)                                          \
{                                                                          \
    (ptr) = (type*) BigAlloc ((size) * sizeof(type));                      \
    if ((ptr) == NULL) {                                                   \
        errcode = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;                       \
        goto finished;                                                     \
    }                                                                      \
}

#define READ(buf, size)                                                    \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    int localErrCode;                                                                                \
    if (localSize  &&  (localErrCode=callback("read",localBuf,localSize,auxdata)) != localSize) {    \
        errcode = localErrCode<0? localErrCode : FREEARC_ERRCODE_READ;                               \
        goto finished;                                                     \
    }                                                                      \
}

#define READ_LEN(len, buf, size)                                           \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) < 0) {      \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}

#define READ_LEN_OR_EOF(len, buf, size)                                    \
{                                                                          \
    int localErrCode;                                                      \
    if ((localErrCode=(len)=callback("read",buf,size,auxdata)) <= 0) {     \
        errcode = localErrCode;                                            \
        goto finished;                                                     \
    }                                                                      \
}

#define WRITE(buf, size)                                                   \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    int localErrCode;                                                                   \
    /* "write" callback on success guarantees to write all the data and may return 0 */ \
    if (localSize && (localErrCode=callback("write",localBuf,localSize,auxdata))<0) {   \
        errcode = localErrCode;                                                         \
        goto finished;                                                     \
    }                                                                      \
}

#define READ4(var)                                                         \
{                                                                          \
    unsigned char localHeader[4];                                          \
    READ (localHeader, 4);                                                 \
    (var) = value32 (localHeader);                                         \
}

#define READ4_OR_EOF(var)                                                  \
{                                                                          \
    int localHeaderSize;                                                   \
    unsigned char localHeader[4];                                          \
    READ_LEN_OR_EOF (localHeaderSize, localHeader, 4);                     \
    if (localHeaderSize!=4)  {errcode=FREEARC_ERRCODE_READ; goto finished;}\
    (var) = value32 (localHeader);                                         \
}

#define WRITE4(value)                                                      \
{                                                                          \
    unsigned char localHeader[4];                                          \
    setvalue32 (localHeader, value);                                       \
    WRITE (localHeader, 4);                                                \
}

#define QUASIWRITE(size)                                                   \
{                                                                          \
    int64 localSize = (size);                                              \
    callback ("quasiwrite", &localSize, (size), auxdata);                  \
}

#define PROGRESS(insize,outsize)                                           \
{                                                                          \
    int64 localSize[2] = {int64(insize), int64(outsize)};                  \
    callback ("progress", localSize, 0, auxdata);                          \
}

#define ReturnErrorCode(err)                                               \
{                                                                          \
    errcode = (err);                                                       \
    goto finished;                                                         \
}                                                                          \


// Buffered data output
#ifndef FREEARC_STANDALONE_TORNADO
#define FOPEN()   Buffer fbuffer(BUFFER_SIZE)
#define FWRITE(buf, size)                                                  \
{                                                                          \
    void *flocalBuf = (buf);                                               \
    int flocalSize = (size);                                               \
    int rem = fbuffer.remainingSpace();                                    \
    if (flocalSize>=4096) {                                                \
        FFLUSH();                                                          \
        WRITE(flocalBuf, flocalSize);                                      \
    } else if (flocalSize < rem) {                                         \
        fbuffer.put (flocalBuf, flocalSize);                               \
    } else {                                                               \
        fbuffer.put (flocalBuf, rem);                                      \
        FFLUSH();                                                          \
        fbuffer.put ((byte*)flocalBuf+rem, flocalSize-rem);                \
    }                                                                      \
}
#define FWRITESZ(value)                                                    \
{                                                                          \
    const char *flocalValue = (value);                                     \
    int flocalBytes = strlen(flocalValue) + 1;                             \
    FWRITE ((void*)flocalValue, flocalBytes);                              \
}
#define FWRITE4(value)                                                     \
{                                                                          \
    unsigned char flocalHeader[4];                                         \
    setvalue32 (flocalHeader, value);                                      \
    FWRITE (flocalHeader, 4);                                              \
}
#define FWRITE1(value)                                                     \
{                                                                          \
    unsigned char flocalHeader = (value);                                  \
    FWRITE (&flocalHeader, 1);                                             \
}
#define FFLUSH()  { WRITE (fbuffer.buf, fbuffer.len());  fbuffer.empty(); }
#define FCLOSE()  { FFLUSH(); }


// A buffer used to organize several independent write streams
// within the program. The buffer can write 8/16/32-bit numbers into itself and grow
// as needed. Later the buffer contents are flushed into the output stream.
// In addition, the buffer supports reading data previously written into it.
// The end of the written part of the buffer is max(p,end), where p is the current pointer,
// and end is the highest position of previously written data.
struct Buffer
{
    byte*  buf;              // address of the allocated buffer
    byte*  p;                // current read/write pointer inside this buffer
    byte*  end;              // address just past the end of the read/written data
    byte*  bufend;           // end of the buffer itself

    Buffer (uint size=64*kb) { buf=p=end= (byte*) malloc(size);  bufend=buf+size; }
    ~Buffer()                { freebuf(); }
    void   freebuf()         { free(buf);  buf=p=end=NULL; }
    void   empty()           { p=end=buf; }
    int    len()             { return mymax(p,end)-buf; }

    void   put8 (uint x)     { reserve(sizeof(uint8 ));  *(uint8 *)p=x;    p+=sizeof(uint8 ); }
    void   put16(uint x)     { reserve(sizeof(uint16));  setvalue16(p,x);  p+=sizeof(uint16); }
    void   put32(uint x)     { reserve(sizeof(uint32));  setvalue32(p,x);  p+=sizeof(uint32); }

    void   put(void *b, int n)  { reserve(n);  memcpy(p,b,n);  p+=n; }
    void   puts (char *s)    { put (s, strlen(s)); }
    void   putsz(char *s)    { put (s, strlen(s)+1); }

    int    remainingSpace()  { return bufend-p; }
    void   reserve(uint n)   {
                               if (remainingSpace() < n)
                               {
                                 uint newsize = mymax(p+n-buf, (bufend-buf)*2);
                                 byte* newbuf = (byte*) realloc (buf, newsize);
                                 bufend = newbuf + newsize;
                                 p   += newbuf-buf;
                                 end += newbuf-buf;
                                 buf  = newbuf;
                               }
                             }

    void reverseBytes()      {
                               byte *lo = buf,  *hi = buf + len() - 1,  swap;
                               while (lo < hi)  { swap = *lo;  *lo++ = *hi;  *hi-- = swap; }
                             }
// For reading data
    void   rewind()          { end=mymax(p,end);  p=buf; }
    uint   get8 ()           { uint x = *(uint8 *)p;  p+=sizeof(uint8 );  return x; }
    uint   get16()           { uint x = value16(p);   p+=sizeof(uint16);  return x; }
    uint   get32()           { uint x = value32(p);   p+=sizeof(uint32);  return x; }
    int    get(void *b, int n)  { n = mymin(remainingData(), n);  memcpy(b,p,n);  p+=n;  return n;}
    int    remainingData()   { return p<end? end-p : 0; }
    bool   eof()             { return remainingData()==0; }
};

#endif // !FREEARC_STANDALONE_TORNADO


// ****************************************************************************************************************************
// CRC-32 COMPUTATION                                                                                                         *
// ****************************************************************************************************************************

#define INIT_CRC 0xffffffff

uint32 UpdateCRC (const void *Addr, size_t Size, uint32 StartCRC);     // Update CRC with the contents of a data block
uint32 CalcCRC   (const void *Addr, size_t Size);                      // Compute the CRC of a data block


// ****************************************************************************************************************************
// UTILITIES ******************************************************************************************************************
// ****************************************************************************************************************************

// A parameter of a compression/encryption algorithm
typedef char *CPARAM;

// A compression/encryption algorithm represented as a string
typedef char *CMETHOD;

// A sequence of compression/encryption algorithms represented as "exe+rep+lzma+aes"
typedef char *COMPRESSOR;

// Request service `what` of compression method `method`
LongMemSize CompressionService (char *method, char *what, DEFAULT(int param,0), DEFAULT(void *data,NULL), DEFAULT(CALLBACK_FUNC *callback,NULL));

// Check whether the given compressor includes an encryption algorithm
int compressorIsEncrypted (COMPRESSOR c);
// Compute how much memory is needed to compress with this compressor
LongMemSize compressorGetCompressionMem (COMPRESSOR c);
// Compute how much memory is needed to decompress data compressed by this compressor
LongMemSize compressorGetDecompressionMem (COMPRESSOR c);

// Compute compression ratio for order-0 byte-granular arithmetic coder
double order0_compression_ratio (void *buf, size_t bufsize);

// Get/set number of threads used for (de)compression
int  __cdecl GetCompressionThreads (void);
void __cdecl SetCompressionThreads (int threads);

// Used in 4x4 only: read entire input buffer before compression begins, allocate output buffer large enough to hold entire compressed output
extern int compress_all_at_once;
void __cdecl Set_compress_all_at_once (int n);
struct Set_compress_all_at_once_Until_end_of_block
{
  int save;
  Set_compress_all_at_once_Until_end_of_block (int n)  {save = compress_all_at_once;  Set_compress_all_at_once(n);}
  ~Set_compress_all_at_once_Until_end_of_block()       {Set_compress_all_at_once(save);}
};

// Enable debugging output
extern int debug_mode;
void __cdecl Set_debug_mode (int n);

// Load accelerated function either from facompress.dll or facompress_mt.dll
FARPROC LoadFromDLL (char *funcname, DEFAULT(int only_facompress_mt, FALSE));

// Other compression methods may chain-redefine this callback in order to perform their own cleanup procedures
extern void (*BeforeUnloadDLL)();

// This function unloads DLLs containing accelerated compression functions
void UnloadDLL (void);

#ifdef FREEARC_WIN
extern HINSTANCE hinstUnarcDll;   // unarc.dll instance
#endif

// This function should cleanup Compression Library
void compressionLib_cleanup (void);


// ****************************************************************************************************************************
// DATA COMPRESSION AND DECOMPRESSION SERVICES ********************************************************************************
// ****************************************************************************************************************************

enum COMPRESSION {COMPRESS, DECOMPRESS};  // Direction of operation

// Decompress data compressed with the given method or chain of methods
int Decompress (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Read the compression method designation from the input stream and decompress the data with that method
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata);
// Decompress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int DecompressMem (char *method, void *input, int inputSize, void *output, int outputSize);
int DecompressMemWithHeader     (void *input, int inputSize, void *output, int outputSize);

#ifndef FREEARC_DECOMPRESS_ONLY
// Compress data with the given method or chain of methods
int Compress   (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Write the compression method designation into the output stream and compress the data with that method
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Compress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int CompressMem           (char *method, void *input, int inputSize, void *output, int outputSize);
int CompressMemWithHeader (char *method, void *input, int inputSize, void *output, int outputSize);
// Information about the memory needed for compression/decompression, the dictionary size and the block size.
MemSize GetCompressionMem      (char *method);
MemSize GetMinCompressionMem   (char *method);
MemSize GetMinDecompressionMem (char *method);
// Return in out_method a new compression method configured to use
// the corresponding amount of memory / dictionary / block size
int SetCompressionMem          (char *in_method, MemSize mem,  char *out_method);
int SetMinDecompressionMem     (char *in_method, MemSize mem,  char *out_method);
int SetDictionary              (char *in_method, MemSize dict, char *out_method);
int SetBlockSize               (char *in_method, MemSize bs,   char *out_method);
// Return in out_method a new compression method, reducing, if necessary,
// the memory used by the algorithm / its dictionary / the block size
int LimitCompressionMem        (char *in_method, MemSize mem,  char *out_method);
int LimitMinDecompressionMem   (char *in_method, MemSize mem,  char *out_method);
int LimitDictionary            (char *in_method, MemSize dict, char *out_method);
int LimitBlockSize             (char *in_method, MemSize bs,   char *out_method);
#endif
MemSize GetDictionary          (char *method);
MemSize GetBlockSize           (char *method);
MemSize GetDecompressionMem    (char *method);
int     SetDecompressionMem    (char *in_method, MemSize mem,  char *out_method);
int     LimitDecompressionMem  (char *in_method, MemSize mem,  char *out_method);

// Write into out_method the canonical representation of compression method in_method (performs ParseCompressionMethod + ShowCompressionMethod)
//   purify!=0: prepare the method representation for writing into the archive (for example, strip :t:i for 4x4)
int CanonizeCompressionMethod (char *in_method, char *out_method, int purify);

// A "(de)compression" function that copies the data verbatim
int copy_data   (CALLBACK_FUNC *callback, void *auxdata);


// ****************************************************************************************************************************
// CLASS IMPLEMENTING THE INTERFACE TO A COMPRESSION METHOD *******************************************************************
// ****************************************************************************************************************************

#ifdef __cplusplus

// Abstract interface to an arbitrary compression method
class COMPRESSION_METHOD
{
public:
  // Decompression and compression functions
  //   DeCompressMem can either compress or decompress, either from memory block `input` to `output` or calling `callback` for I/O.
  //   CodecState, unless NULL, points to the place for storing pointer to persistent codec state, such as allocated buffers
  //     and precomputed tables, that should be finally freed by the empty DeCompressMem() call.
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) = 0;
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata) = 0;

  // Information about the memory needed for compression/decompression (Min - with :t1:i0, i.e. the minimum number of threads/buffers - for ArcInfo and the like),
  // the dictionary size (that is, how far the algorithm looks back when searching for similar data - for lz/bs schemes),
  // and the block size (that is, the maximum amount of data it makes sense to put into one solid block - for bs schemes and lzp)
  virtual MemSize GetCompressionMem        (void)         = 0;
  virtual MemSize GetMinCompressionMem     (void)               {return GetCompressionMem();}
  virtual MemSize GetMinDecompressionMem   (void)               {return GetDecompressionMem();}
  // Configure the compression method to use the given amount of memory, dictionary or block size
  virtual void    SetDictionary            (MemSize dict)       {}
  virtual void    SetBlockSize             (MemSize bs)         {}
  virtual void    SetCompressionMem        (MemSize mem)  = 0;
  virtual void    SetMinDecompressionMem   (MemSize mem)  = 0;  // for -ld during compression (i.e. with :t1:i0): set the minimum amount of memory required for decompression
  // Limit the memory used during compression/decompression, or the dictionary / block size
  virtual void    LimitDictionary          (MemSize dict)       {if (dict>0 && GetDictionary()          > dict)  SetDictionary(dict);}
  virtual void    LimitBlockSize           (MemSize bs)         {if (bs>0   && GetBlockSize()           > bs)    SetBlockSize(bs);}
  virtual void    LimitCompressionMem      (MemSize mem)        {if (mem>0  && GetCompressionMem()      > mem)   SetCompressionMem(mem);}
  virtual void    LimitMinDecompressionMem (MemSize mem)        {if (mem>0  && GetMinDecompressionMem() > mem)   SetMinDecompressionMem(mem);}
#endif
  virtual MemSize GetDictionary            (void)               {return 0;}
  virtual MemSize GetBlockSize             (void)               {return 0;}
  virtual MemSize GetAlgoMem               (void);                            // Amount of memory that characterizes the algorithm
  virtual MemSize GetDecompressionMem      (void)         = 0;
  virtual void    SetDecompressionMem      (MemSize mem)        {}    // for -ld during decompression (i.e. we change only parameters like :t:i, keeping compatibility with the compressed data)
  virtual void    LimitDecompressionMem    (MemSize mem)        {if (mem>0  && GetDecompressionMem() > mem)   SetDecompressionMem(mem);}

  // Maximum possible inflation of incompressible input data
  virtual LongMemSize GetMaxCompressedSize (LongMemSize insize) {return insize + (insize/4) + 16*kb;}

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of ParseCompressionMethod)
  virtual void ShowCompressionMethod (char *buf, bool purify) = 0;

  // Universal method. Parameters:
  //   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
  //   data: data for the operation, in a format that depends on the particular operation being performed
  //   param&result: a simple numeric parameter, which is enough for many informational operations
  // Unused parameters are set to NULL/0. result<0 is an error code
  virtual LongMemSize doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  // Check boolean method property
  bool is (char *request)   {return doit (request, 0, NULL, NULL) > 0;}

  double addtime;  // Extra time spent on compression (in external programs, additional threads and so on)
  COMPRESSION_METHOD() {addtime=0;}
  virtual ~COMPRESSION_METHOD() {}
//  Debugging code:  char buf[100]; ShowCompressionMethod(buf,FALSE); printf("%s : %u => %u\n", buf, GetCompressionMem(), mem);
};


// ****************************************************************************************************************************
// COMPRESSION_METHOD FACTORY *************************************************************************************************
// ****************************************************************************************************************************

// Construct an object of a class derived from COMPRESSION_METHOD
// that implements the compression method given by the string `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method);

// Class for enumerating all possible parsing variants and running TryCompressor on them until it returns true
struct CompressionMethodParser
{
  bool EnumerateCompressors (char* method);
  virtual bool TryCompressor (COMPRESSION_METHOD *_compressor) = 0;
};

// Reference to a function that parses the string of one of the compression methods and on success returns a reference to the constructed object
typedef COMPRESSION_METHOD* (*CM_PARSER) (char** parameters, void *data);

// Add the parser of a new compression method to the list of supported compression methods,
// together with the extra parameter that must be passed to that parser
int AddCompressionMethod (CM_PARSER parser, void *data = NULL);

// Clear the table of external compressors
void ClearExternalCompressorsTable (void);


// ****************************************************************************************************************************
// THE "COMPRESSION" METHOD STORING *******************************************************************************************
// ****************************************************************************************************************************

// Implementation of the "compression" method STORING
class STORING_METHOD : public COMPRESSION_METHOD
{
public:
  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Get/set the amount of memory used during compression/decompression
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_STORING)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "storing");}
};

// Parser for the STORING compression method string
COMPRESSION_METHOD* parse_STORING (char** parameters);


// ****************************************************************************************************************************
// THE "COMPRESSION" METHOD CRC: read the data and write nothing **************************************************************
// ****************************************************************************************************************************

// Implementation of the "compression" method crc
class CRC_METHOD : public COMPRESSION_METHOD
{
public:
  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Get/set the amount of memory used during compression/decompression
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_CRC)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "crc");}
};

// Parser for the "compression" method crc string
COMPRESSION_METHOD* parse_CRC (char** parameters);


// ****************************************************************************************************************************
// THE "COMPRESSION" METHOD FAKE: read no data and write nothing **************************************************************
// ****************************************************************************************************************************

// Implementation of the "compression" method fake
class FAKE_METHOD : public COMPRESSION_METHOD
{
public:
  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata) {return FREEARC_ERRCODE_INTERNAL;}

  // Get/set the amount of memory used during compression/decompression
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_FAKE)
  virtual void ShowCompressionMethod (char *buf, bool purify)   {sprintf (buf, "fake");}
};

// Parser for the STORING compression method string
COMPRESSION_METHOD* parse_FAKE (char** parameters);

#endif  // __cplusplus


// ****************************************************************************************************************************
// (De)compress data from memory buffer (input) to another memory buffer (output)                                             *
// ****************************************************************************************************************************

// Structure storing the position in the read/write buffers during in-memory compression/decompression
struct MemBuf
{
  MemBuf (void *input, int inputSize, void *output, int outputSize, CALLBACK_FUNC *_callback=0, void *_auxdata=0)
  {
    readPtr=(BYTE*)input, readLeft=inputSize, writePtr=(BYTE*)output, writeLeft=writeBufferSize=outputSize, callback=_callback, auxdata=_auxdata;
  }

  // How much data was written into the buffer
  int written()  {return writeBufferSize-writeLeft;}

  BYTE *readPtr;          // current position of the data being read (NULL if the data must be read via callback)
  int   readLeft;         // how many bytes are still left in the input buffer
  BYTE *writePtr;         // current position of the data being written (NULL if the data must be written via callback)
  int   writeLeft;        // how many bytes are still left in the output buffer
  int   writeBufferSize;  // full size of the output buffer
  CALLBACK_FUNC *callback;
  void *auxdata;
};

// Read/write callback function for in-memory (de)compression
int ReadWriteMem (const char *what, void *buf, int size, void *_membuf);


// ****************************************************************************************************************************
// ENCRYPTION ROUTINES *****************************************************************************************************
// ****************************************************************************************************************************

// Generates key based on password and salt using given number of hashing iterations
void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize);

int fortuna_size (void);


#ifdef __cplusplus
}       // extern "C"
#endif

#endif  // FREEARC_COMPRESSION_H
