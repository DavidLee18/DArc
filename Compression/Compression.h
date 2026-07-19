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

//Error codes
#define FREEARC_OK                               0     /* ALL RIGHT */
#define FREEARC_ERRCODE_GENERAL                  (-1)  /* Some error when (de)compressing */
#define FREEARC_ERRCODE_INVALID_COMPRESSOR       (-2)  /* Invalid compression method or parameters */
#define FREEARC_ERRCODE_ONLY_DECOMPRESS          (-3)  /* Program builded with FREEARC_DECOMPRESS_ONLY, so don't try to use compress */
#define FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL       (-4)  /* Output block size in (de)compressMem is not enough for all output data */
#define FREEARC_ERRCODE_NOT_ENOUGH_MEMORY        (-5)  /* Can't allocate memory needed for (de)compression */
#define FREEARC_ERRCODE_IO                       (-6)  /* Error when reading or writing data */
#define FREEARC_ERRCODE_BAD_COMPRESSED_DATA      (-7)  /* Data can't be decompressed */
#define FREEARC_ERRCODE_NOT_IMPLEMENTED          (-8)  /* Requested feature isn't supported */
#define FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED    (-9)  /* Required part of data was already decompressed */


// Constants for conveniently writing memory amounts
#define b_ (1u)
#define kb (1024*b_)
#define mb (1024*kb)
#define gb (1024*mb)

// Number of bytes that should be read/written at a time in all compressors
#define BUFFER_SIZE (64*kb)

// Number of bytes that should be read/written at a time in the fast methods and when decompressing asymmetric algorithms
#define LARGE_BUFFER_SIZE (256*kb)

// Number of bytes that should be read/written at a time in the very fast methods (storing, tornado and the like)
// This amount minimizes the losses on disk seek operations - provided that no I/O is happening in another thread at the same time ;)
#define HUGE_BUFFER_SIZE (8*mb)

// Additional definitions that make it convenient to write parsers for compression method strings
#define COMPRESSION_METHODS_DELIMITER            '+'   /* Separator between compression algorithms in the string description of a compressor */
#define COMPRESSION_METHOD_PARAMETERS_DELIMITER  ':'   /* Separator between parameters in the string description of a compression method */
#define MAX_COMPRESSION_METHODS    1000        /* Must be no less than the number of compression methods registered via AddCompressionMethod */
#define MAX_PARAMETERS             200         /* Must be no less than the maximum number of parameters (separated by colons) that a compression method may have */
#define MAX_METHOD_STRLEN          2048        /* Maximum length of a string describing a compression method */
#define MAX_METHODS_IN_COMPRESSOR  100         /* Maximum number of methods in a single compressor */
#define MAX_EXTERNAL_COMPRESSOR_SECTION_LENGTH 2048  /* Maximum length of the [External compressor] section */


// ****************************************************************************************************************************
// DATA READ/WRITE HELPERS FOR COMPRESSION METHODS ****************************************************************************
// ****************************************************************************************************************************

// Type of the callback functions
typedef int CALLBACK_FUNC (const char *what, void *data, int size, void *auxdata);

// Macros for reading/writing the in/out streams that check that exactly as much data was transferred as was requested
#define checked_read(ptr,size)         do { x = callback("read" ,ptr,size,auxdata); if (x != size) { if (x>=0) x=FREEARC_ERRCODE_IO; goto finished; } } while(0)
#define checked_write(ptr,size)        do { x = callback("write",ptr,size,auxdata); if (x != size) { if (x>=0) x=FREEARC_ERRCODE_IO; goto finished; } } while(0)
// Macro for reading input streams that checks for errors and for the end of the input data
#define checked_eof_read(ptr,size)     do { x = callback("write",ptr,size,auxdata); if (x != size) { if (x>0)  x=FREEARC_ERRCODE_IO; goto finished; } } while(0)

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

#define READ(buf, size)                                                    \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    if (localSize  &&  (errcode=callback("read",localBuf,localSize,auxdata)) != localSize) { \
        if (errcode>=0) errcode=FREEARC_ERRCODE_IO;                        \
        goto finished;                                                     \
    }                                                                      \
}

#define READ_LEN_OR_EOF(len, buf, size)                                    \
{                                                                          \
    if ((errcode=len=callback("read",buf,size,auxdata)) <= 0) {            \
        goto finished;                                                     \
    }                                                                      \
}

#define WRITE(buf, size)                                                   \
{                                                                          \
    void *localBuf = (buf);                                                \
    int localSize  = (size);                                               \
    /* "write" callback on success guarantees to write all the data and may return 0 */ \
    if (localSize && (errcode=callback("write",localBuf,localSize,auxdata))<0)  \
        goto finished;                                                     \
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
    if (localHeaderSize!=4)  {errcode=FREEARC_ERRCODE_IO; goto finished;}  \
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
    callback ("quasiwrite", &localSize, size, auxdata);                    \
}

#define ReturnErrorCode(x)                                                 \
{                                                                          \
    errcode = (x);                                                         \
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
#define FFLUSH()  { WRITE (fbuffer.buf, fbuffer.len());  fbuffer.empty(); }
#define FCLOSE()  { FFLUSH();  fbuffer.free(); }
#endif // !FREEARC_STANDALONE_TORNADO


// ****************************************************************************************************************************
// UTILITIES ******************************************************************************************************************
// ****************************************************************************************************************************

// A compression/encryption algorithm represented as a string
typedef char *CMETHOD;

// A sequence of compression/encryption algorithms represented as "exe+rep+lzma+aes"
typedef char *COMPRESSOR;

// Request service `what` of compression method `method`
int CompressionService (char *method, char *what, DEFAULT(int param,0), DEFAULT(void *data,NULL), DEFAULT(CALLBACK_FUNC *callback,NULL));

// Check whether the given compressor includes an encryption algorithm
int compressorIsEncrypted (COMPRESSOR c);
// Compute how much memory is needed to decompress data compressed by this compressor
MemSize compressorGetDecompressionMem (COMPRESSOR c);

// Get/set number of threads used for (de)compression
int  __cdecl GetCompressionThreads (void);
void __cdecl SetCompressionThreads (int threads);

// Load (accelerated) function from facompress.dll
FARPROC LoadFromDLL (char *funcname);

// Used in 4x4 only: read entire input buffer before compression begins, allocate output buffer large enough to hold entire compressed output
extern int compress_all_at_once;

// Register/unregister temporary files that should be deleted on ^Break
void registerTemporaryFile   (char *name, DEFAULT(FILE* file, NULL));
void unregisterTemporaryFile (char *name);

// This function should cleanup Compression Library
void compressionLib_cleanup (void);


// ****************************************************************************************************************************
// DATA COMPRESSION AND DECOMPRESSION SERVICES ********************************************************************************
// ****************************************************************************************************************************

// Decompress data compressed with the given method
int Decompress (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Decompress data compressed with a chain of methods
int MultiDecompress (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Read the compression method designation from the input stream and decompress the data with that method
int DecompressWithHeader (CALLBACK_FUNC *callback, void *auxdata);
// Decompress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int DecompressMem (char *method, void *input, int inputSize, void *output, int outputSize);
int DecompressMemWithHeader     (void *input, int inputSize, void *output, int outputSize);

#ifndef FREEARC_DECOMPRESS_ONLY
// Compress data with the given method
int Compress   (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Write the compression method designation into the output stream and compress the data with that method
int CompressWithHeader (char *method, CALLBACK_FUNC *callback, void *auxdata);
// Compress data in memory, writing no more than outputSize bytes into the output buffer.
// Returns an error code or the number of bytes written into the output buffer
int CompressMem           (char *method, void *input, int inputSize, void *output, int outputSize);
int CompressMemWithHeader (char *method, void *input, int inputSize, void *output, int outputSize);
// Write into out_method the canonical representation of compression method in_method (performs ParseCompressionMethod + ShowCompressionMethod)
int CanonizeCompressionMethod (char *in_method, char *out_method);
// Information about the memory needed for compression/decompression, the dictionary size and the block size.
MemSize GetCompressionMem   (char *method);
MemSize GetDecompressionMem (char *method);
MemSize GetDictionary       (char *method);
MemSize GetBlockSize        (char *method);
// Return in out_method a new compression method configured to use
// the corresponding amount of memory / dictionary / block size
int SetCompressionMem   (char *in_method, MemSize mem,  char *out_method);
int SetDecompressionMem (char *in_method, MemSize mem,  char *out_method);
int SetDictionary       (char *in_method, MemSize dict, char *out_method);
int SetBlockSize        (char *in_method, MemSize bs,   char *out_method);
// Return in out_method a new compression method, reducing, if necessary,
// the memory used by the algorithm / its dictionary / the block size
int LimitCompressionMem   (char *in_method, MemSize mem,  char *out_method);
int LimitDecompressionMem (char *in_method, MemSize mem,  char *out_method);
int LimitDictionary       (char *in_method, MemSize dict, char *out_method);
int LimitBlockSize        (char *in_method, MemSize bs,   char *out_method);
#endif

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
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata) = 0;
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata) = 0;

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of ParseCompressionMethod)
  virtual void ShowCompressionMethod (char *buf) = 0;

  // Information about the memory needed for compression/decompression,
  // the dictionary size (that is, how far the algorithm looks back when searching for similar data - for lz/bs schemes),
  // and the block size (that is, the maximum amount of data it makes sense to put into one solid block - for bs schemes and lzp)
  virtual MemSize GetCompressionMem   (void)         = 0;
  virtual MemSize GetDecompressionMem (void)         = 0;
  virtual MemSize GetDictionary       (void)         = 0;
  virtual MemSize GetBlockSize        (void)         = 0;
  // Configure the compression method to use the given amount of memory, dictionary or block size
  virtual void    SetCompressionMem   (MemSize mem)  = 0;
  virtual void    SetDecompressionMem (MemSize mem)  = 0;
  virtual void    SetDictionary       (MemSize dict) = 0;
  virtual void    SetBlockSize        (MemSize bs)   = 0;
  // Limit the memory used during compression/decompression, or the dictionary / block size
  void LimitCompressionMem   (MemSize mem)  {if (GetCompressionMem()   > mem)   SetCompressionMem(mem);}
  void LimitDecompressionMem (MemSize mem)  {if (GetDecompressionMem() > mem)   SetDecompressionMem(mem);}
  void LimitDictionary       (MemSize dict) {if (GetDictionary()       > dict)  SetDictionary(dict);}
  void LimitBlockSize        (MemSize bs)   {if (GetBlockSize()        > bs)    SetBlockSize(bs);}
#endif
  // Universal method. Parameters:
  //   what: "compress", "decompress", "setCompressionMem", "limitDictionary"...
  //   data: data for the operation, in a format that depends on the particular operation being performed
  //   param&result: a simple numeric parameter, which is enough for many informational operations
  // Unused parameters are set to NULL/0. result<0 is an error code
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  double addtime;  // Extra time spent on compression (in external programs, additional threads and so on)
  COMPRESSION_METHOD() {addtime=0;}
  virtual ~COMPRESSION_METHOD() {}
//  Debugging code:  char buf[100]; ShowCompressionMethod(buf); printf("%s : %u => %u\n", buf, GetCompressionMem(), mem);
};


// ****************************************************************************************************************************
// COMPRESSION_METHOD FACTORY *************************************************************************************************
// ****************************************************************************************************************************

// Construct an object of a class derived from COMPRESSION_METHOD
// that implements the compression method given by the string `method`
COMPRESSION_METHOD *ParseCompressionMethod (char* method);

typedef COMPRESSION_METHOD* (*CM_PARSER) (char** parameters);
typedef COMPRESSION_METHOD* (*CM_PARSER2) (char** parameters, void *data);
int AddCompressionMethod         (CM_PARSER parser);  // Add the parser of a new method to the list of supported compression methods
int AddExternalCompressionMethod (CM_PARSER2 parser2, void *data);  // Add the parser of an external compression method together with the extra parameter that must be passed to that parser
#endif  // __cplusplus
void ClearExternalCompressorsTable (void);                          // Clear the table of external compressors
#ifdef __cplusplus


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

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_STORING)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used during compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem   (void)    {return BUFFER_SIZE;}
  virtual MemSize GetDecompressionMem (void)    {return BUFFER_SIZE;}
  virtual MemSize GetDictionary       (void)    {return 0;}
  virtual MemSize GetBlockSize        (void)    {return 0;}
  virtual void    SetCompressionMem   (MemSize) {}
  virtual void    SetDecompressionMem (MemSize) {}
  virtual void    SetDictionary       (MemSize) {}
  virtual void    SetBlockSize        (MemSize) {}
#endif
};

// Parser for the STORING compression method string
COMPRESSION_METHOD* parse_STORING (char** parameters);

#endif  // __cplusplus


// ****************************************************************************************************************************
// ENCRYPTION ROUTINES *****************************************************************************************************
// ****************************************************************************************************************************

// Generates key based on password and salt using given number of hashing iterations
void Pbkdf2Hmac (const BYTE *pwd, int pwdSize, const BYTE *salt, int saltSize,
                 int numIterations, BYTE *key, int keySize);

int fortuna_size (void);

// The rest of the Fortuna PRNG interface used by EncryptionLib.hs. Only
// fortuna_size was declared here, so the other four were reaching their call
// sites in MicroHs-generated C as implicit declarations -- assumed to return
// int, which silently truncated fortuna_read's unsigned long result. The
// prng_state pointer is opaque here to avoid dragging LibTomCrypt's headers
// into every translation unit that includes this file; it is passed straight
// back to the library, so only its size matters, not its shape.
int  fortuna_start       (void *prng);
int  fortuna_add_entropy (const unsigned char *in, unsigned long inlen, void *prng);
int  fortuna_ready       (void *prng);
unsigned long fortuna_read (unsigned char *out, unsigned long outlen, void *prng);

// .7z support, implemented in Compression/7z/C_7z.c. Declared here rather than
// in a 7z-specific header so that the FFI imports in Arc7z.hs can name a
// header the generated C already includes.
int darc_7z_list    (const char *path);
int darc_7z_extract (const char *path, const char *out_dir);
int darc_7z_test    (const char *path);


#ifdef __cplusplus
}       // extern "C"
#endif

#endif  // FREEARC_COMPRESSION_H
