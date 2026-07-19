#include "../Compression.h"

int lzp_compress   (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);
int lzp_decompress (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class LZP_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Size of the block processed at a time. Matches are only searched for within this block
  int     MinCompression;   // Minimum compression percentage. If the output data is larger, the original (uncompressed) data is written instead
  int     MinMatchLen;      // Minimum length of a matching string that will be compressed
  int     HashSizeLog;      // Logarithm of the hash size (in 4-byte words). Larger values improve compression but slow it down considerably
  int     Barrier;          // Threshold beyond which shorter matches are allowed (since lzma/ppmd would miss them anyway)
  int     SmallestLen;      // What string length is allowed when the distance > Barrier

  // Constructor assigning default values to the parameters of the compression method
  LZP_METHOD();
  // Generic method: answer "VeryFast?" queries positively for a hash <= 128 Kb
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if (strequ (what,"VeryFast?"))  return HashSizeLog<=15;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_LZP)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return BlockSize*2 + (1<<HashSizeLog)*sizeof(BYTE*);}
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize*2 + (1<<HashSizeLog)*sizeof(BYTE*);}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return BlockSize;}
  virtual void    SetCompressionMem     (MemSize mem);
  virtual void    SetDecompressionMem   (MemSize mem)  {SetCompressionMem(mem);}
  virtual void    SetDictionary         (MemSize dict) {SetBlockSize (dict);}
  virtual void    SetBlockSize          (MemSize bs);
#endif
};

// Parser for the LZP compression method string
COMPRESSION_METHOD* parse_LZP (char** parameters);

#endif  // __cplusplus
