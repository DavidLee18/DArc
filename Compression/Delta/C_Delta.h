#include "../Compression.h"

int delta_compress   (MemSize BlockSize, int ExtendedTables, CALLBACK_FUNC *callback, void *auxdata);
int delta_decompress (MemSize BlockSize, int ExtendedTables, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression-method interface
class DELTA_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Size of the data block processed in one pass
  int     ExtendedTables;   // Look for tables whose element size differs from 2/4

  // Constructor that assigns default values to the compression method parameters
  DELTA_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_DELTA)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return BlockSize;}
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {if (mem>0)   BlockSize = mem;}
  virtual void    SetDecompressionMem   (MemSize mem)  {if (mem>0)   BlockSize = mem;}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the DELTA compression method string
COMPRESSION_METHOD* parse_DELTA (char** parameters);

#endif  // __cplusplus
