#include "../Compression.h"

#ifdef __cplusplus

// LZMA2 compression method (DArc interface, using 7-Zip 24.09 C API)
class LZMA2_METHOD : public COMPRESSION_METHOD
{
public:
  MemSize dictionarySize;
  int     algorithm;
  int     numFastBytes;
  int     matchFinder;      // kBT2..kHT4 (0..4), same mapping as LZMA_METHOD
  int     matchFinderCycles;
  int     posStateBits;
  int     litContextBits;
  int     litPosBits;

  LZMA2_METHOD();

  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  virtual void    ShowCompressionMethod (char *buf);
  virtual MemSize GetCompressionMem     (void);
  virtual MemSize GetDecompressionMem   (void);
  virtual MemSize GetDictionary         (void)         {return dictionarySize;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem);
  virtual void    SetDecompressionMem   (MemSize mem);
  virtual void    SetDictionary         (MemSize dict);
  virtual void    SetBlockSize          (MemSize)      {}
#endif
};

COMPRESSION_METHOD* parse_LZMA2 (char** parameters);

#endif
