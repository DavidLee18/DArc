#include "../Compression.h"

int dict_compress   (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio, CALLBACK_FUNC *callback, void *auxdata);
int dict_decompress (MemSize BlockSize, int MinCompression, int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class DICT_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Size of the data block processed at a time. Each block gets its own dictionary
  int     MinCompression;   // Minimum compression percentage. If the output data is larger, the original (uncompressed) data is stored instead
  int     MinWeakChars;     // Minimum acceptable number of weak chars. If it turns out to be lower, compression is refused, since small weak-char values usually indicate a binary file that this algorithm will not manage to compress
  int     MinLargeCnt;      // Minimum "large" counter
  int     MinMediumCnt;     // Minimum "medium" counter
  int     MinSmallCnt;      // Minimum "small" counter
  int     MinRatio;         // Minimum "ratio"

  // Constructor assigning default values to the compression method parameters
  DICT_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_DICT)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return BlockSize*2;}
  virtual MemSize GetDecompressionMem   (void)         {return 1*mb /*BlockSize*2*/;}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return BlockSize;}
  virtual void    SetCompressionMem     (MemSize mem)  {if (mem>0)   BlockSize = mem/2;}
  virtual void    SetDecompressionMem   (MemSize mem)  {if (mem>0)   BlockSize = mem/2;}
  virtual void    SetDictionary         (MemSize dict) {if (dict>0)  BlockSize = dict;}
  virtual void    SetBlockSize          (MemSize bs)   {if (bs>0)    BlockSize = bs;}
#endif
};

// Parser for the DICT preprocessor string
COMPRESSION_METHOD* parse_DICT (char** parameters);

#endif  // __cplusplus
