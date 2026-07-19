#include "../Compression.h"

int rep_compress   (MemSize BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashSizeLog, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);
int rep_decompress (MemSize BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashSizeLog, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression-method interface
class REP_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Buffer size. Matches are searched only within this distance. Memory usage is BlockSize+BlockSize/4
  int     MinCompression;   // Minimum compression percentage. If the output is larger than that, the original (uncompressed) data is stored instead
  int     MinMatchLen;      // Minimum string length at which it is replaced by a reference to a previous occurrence
  int     Barrier;          // Boundary past which smaller matches may be used (since lzma/ppmd would skip them anyway)
  int     SmallestLen;      // That smaller size
  int     HashSizeLog;      // Logarithm of the hash size (in 4-byte words). Larger values improve compression but slow it down. When zero, the optimal size is computed automatically
  int     Amplifier;        // Search "amplification" factor

  // Constructor assigning default values to the compression method parameters
  REP_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_REP)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void);
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize;}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {if (mem>0)   BlockSize = 1<<lb(mem/7*6);}
  virtual void    SetDecompressionMem   (MemSize mem)  {if (mem>0)   BlockSize = mem;}
  virtual void    SetDictionary         (MemSize dict) {if (dict>0)  BlockSize = dict;}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the REP compression method string
COMPRESSION_METHOD* parse_REP (char** parameters);

#endif  // __cplusplus
