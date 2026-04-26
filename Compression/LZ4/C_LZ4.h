#include "../Compression.h"

#ifdef __cplusplus

// LZ4 compression method (DArc interface)
class LZ4_METHOD : public COMPRESSION_METHOD
{
public:
  int     Compressor;       // 0 = fast LZ4; 1..12 = HC level
  MemSize BlockSize;        // Block size
  MemSize HashSize;         // reserved (unused by modern LZ4)
  int     MinCompression;   // Minimum compression ratio (%); else stored raw

  LZ4_METHOD();

  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  virtual void    ShowCompressionMethod (char *buf);

  virtual MemSize GetCompressionMem     (void);
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize*2;}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return BlockSize;}
  virtual void    SetCompressionMem     (MemSize mem);
  virtual void    SetDecompressionMem   (MemSize mem)  {SetCompressionMem(mem);}
  virtual void    SetDictionary         (MemSize dict) {if (dict) BlockSize = dict;}
  virtual void    SetBlockSize          (MemSize bs)   {if (bs)   BlockSize = bs;}
#endif
};

COMPRESSION_METHOD* parse_LZ4 (char** parameters);

#endif  // __cplusplus
