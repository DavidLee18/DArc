#include "../Compression.h"

int lzma_compress   (int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     void *auxdata);

int lzma_decompress (int dictionarySize,
                     int hashSize,
                     int algorithm,
                     int numFastBytes,
                     int matchFinder,
                     int matchFinderCycles,
                     int posStateBits,
                     int litContextBits,
                     int litPosBits,
                     CALLBACK_FUNC *callback,
                     void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD interface for compression methods
class LZMA_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize dictionarySize;
  MemSize hashSize;
  int     algorithm;
  int     numFastBytes;
  int     matchFinder;
  int     matchFinderCycles;
  int     posStateBits;
  int     litContextBits;
  int     litPosBits;

  // Constructor that assigns the default values to the compression method parameters
  LZMA_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_LZMA)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used during packing/unpacking, the dictionary size or the block size
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

// Parser for the LZMA compression method string
COMPRESSION_METHOD* parse_LZMA (char** parameters);

#endif  // __cplusplus
