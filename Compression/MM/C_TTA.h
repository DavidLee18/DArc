// Wave sound comression algorithm
#include "../Compression.h"
#include "ttaenc.h"


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression-method interface
class TTA_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  int level;        // Compression level (1..3, higher means tighter and slower compression)
  int skip_header;  // Skip WAV header detection
  int is_float;     // Floating-point data format
  int num_chan;     // Channels count
  int word_size;    // Size of each encoded value, in bits
  int offset;       // File offset where MM data start (header is copied intact)
  int raw_data ;    // Write raw predictor's output without using entropy encoder

  // Constructor that assigns default values to the compression method's parameters
  TTA_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] the string describing the compression method and its parameters (inverse of parse_TTA)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return 2*mb;}
  virtual MemSize GetDecompressionMem   (void)         {return 1*mb;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {}
  virtual void    SetDecompressionMem   (MemSize mem)  {}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the TTA compression method string
COMPRESSION_METHOD* parse_TTA (char** parameters);

#endif  // __cplusplus
