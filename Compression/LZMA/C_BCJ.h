#include "../Compression.h"

#ifndef FREEARC_DECOMPRESS_ONLY
int bcj_x86_compress   (CALLBACK_FUNC *callback, void *auxdata);
#endif
int bcj_x86_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression-method interface
class BCJ_X86_METHOD : public COMPRESSION_METHOD
{
public:
  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_BCJ_X86)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return LARGE_BUFFER_SIZE;}
  virtual MemSize GetDecompressionMem   (void)         {return LARGE_BUFFER_SIZE;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {}
  virtual void    SetDecompressionMem   (MemSize mem)  {}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the BCJ_X86 compression method string
COMPRESSION_METHOD* parse_BCJ_X86 (char** parameters);

#endif  // __cplusplus
