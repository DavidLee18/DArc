#include "../Compression.h"

int ppmd_compress   (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata);
int ppmd_decompress (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class PPMD_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  int     order;     // Model order (how many preceding symbols are used to predict the next one)
  MemSize mem;       // Amount of memory used to store the model
  int     MRMethod;  // What to do when the memory allocated for the model is exhausted

  // Constructor assigning default values to the parameters of the compression method
  PPMD_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_PPMD)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)          {return mem;}
  virtual MemSize GetDecompressionMem   (void)          {return mem;}
  virtual MemSize GetDictionary         (void)          {return 0;}
  virtual MemSize GetBlockSize          (void)          {return 0;}
  virtual void    SetCompressionMem     (MemSize _mem);
  virtual void    SetDecompressionMem   (MemSize _mem)  {SetCompressionMem(_mem);}
  virtual void    SetDictionary         (MemSize dict)  {}
  virtual void    SetBlockSize          (MemSize bs)    {}
#endif
};

// Parser for the PPMD compression method string
COMPRESSION_METHOD* parse_PPMD (char** parameters);

#endif  // __cplusplus
