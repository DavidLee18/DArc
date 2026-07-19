#include "../Compression.h"

int tor_compress   (PackMethod m, CALLBACK_FUNC *callback, void *auxdata);
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class TORNADO_METHOD : public COMPRESSION_METHOD
{
public:
  struct PackMethod m;      // Parameters of this compression method

  // Constructor that assigns default values to the compression method parameters
  TORNADO_METHOD();
  // Universal method: we answer "VeryFast?" queries positively for compression levels 1-4
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if (strequ (what,"VeryFast?"))  return m.hash_row_width<=2;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_TORNADO)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used during compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return m.hashsize + m.buffer + tornado_compressor_outbuf_size(m.buffer);}
  virtual MemSize GetDecompressionMem   (void)         {return m.buffer;}
  virtual MemSize GetDictionary         (void)         {return m.buffer;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {if (mem>0)   m.hashsize = 1<<lb(mem/3), m.buffer=mem-m.hashsize;}
  virtual void    SetDecompressionMem   (MemSize mem)  {SetDictionary (mem);}
  virtual void    SetDictionary         (MemSize dict);
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the TORNADO compression method string
COMPRESSION_METHOD* parse_TORNADO (char** parameters);

#endif  // __cplusplus
