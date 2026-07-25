// Multimedia preprocessing filter
#include "../Compression.h"
#include "mmdet.h"

// `mode` was missing here, and the parameter after num_chan is word_size, not
// byte_size. The declaration therefore never matched the definition in mm.cpp:
// C_MM.cpp pulls this header in inside `extern "C"`, so the intent is that the
// definition inherit C linkage -- but with a different arity it simply declared
// a second, never-defined overload, and the real mm_compress stayed C++-mangled
// (__Z11mm_compressiiiiiiiPFiPKcPviS1_ES1_). Harmless while both sides were C,
// since the call site resolved to the definition either way. It stops being
// harmless the moment a Rust drop-in wants the symbol: an extern "C"
// mm_compress would not have replaced anything, and the C would have kept
// running with no sign that the port was inert.
int mm_compress   (int mode, int skip_header, int is_float, int num_chan, int word_size, int offset, int reorder, CALLBACK_FUNC *callback, void *auxdata);
int mm_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class MM_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  int mode;         // Detection speed mode (1 - fastest, 9 - most accurate)
  int skip_header;  // Skip file header detection
  int is_float ;    // Floating-point data format
  int num_chan ;    // Channels count
  int word_size;    // Size of each encoded value, in bits
  int offset;       // File offset where MM data start (header is copied intact)
  int reorder;      // Reorder buffer contents so that each channel data are placed continuosly
                    //   (1 - reorder words, 2 - reorder bytes)

  // Constructor assigning default values to the compression method parameters
  MM_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (inverse of parse_TTA)
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

// Parser for the MM compression method string
COMPRESSION_METHOD* parse_MM (char** parameters);

#endif  // __cplusplus
