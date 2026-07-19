#include "../Compression.h"
#include "libGRZip.h"

int __cdecl grzip_compress   (int Method,
                      int BlockSize,
                      int EnableLZP,
                      int MinMatchLen,
                      int HashSizeLog,
                      int AlternativeBWTSort,
                      int AdaptiveBlockSize,
                      int DeltaFilter,
                      CALLBACK_FUNC *callback,
                      void *auxdata);

int __cdecl grzip_decompress (CALLBACK_FUNC *callback,
                      void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class GRZIP_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Size of the data block processed as a unit
  int     Method;
  int     EnableLZP;
  int     MinMatchLen;
  int     HashSizeLog;
  int     AlternativeBWTSort;
  int     AdaptiveBlockSize;
  int     DeltaFilter;

  // Constructor that assigns default values to the compression method parameters
  GRZIP_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_GRZIP)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return BlockSize*9*GetCompressionThreads();}
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize*5*GetCompressionThreads();}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return BlockSize;}
  virtual void    SetCompressionMem     (MemSize mem)  {SetBlockSize (mem/9/GetCompressionThreads());}
  virtual void    SetDecompressionMem   (MemSize mem)  {SetBlockSize (mem/5/GetCompressionThreads());}
  virtual void    SetDictionary         (MemSize dict) {SetBlockSize (dict);}
  virtual void    SetBlockSize          (MemSize bs);
#endif
};

// Parser for the GRZIP compression method string
COMPRESSION_METHOD* parse_GRZIP (char** parameters);

#endif  // __cplusplus
