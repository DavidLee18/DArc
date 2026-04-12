// DArc 4x4: multithreaded block compression
// Adapted from FreeArc 0.67's 4x4 module

#ifndef DARC_C_4X4_H
#define DARC_C_4X4_H

#include "../Compression.h"

#ifdef __cplusplus

// _4x4_METHOD is a compression method that wraps another method
// and applies it independently to fixed-size blocks on N threads.
class _4x4_METHOD : public COMPRESSION_METHOD
{
public:
  char    Method [MAX_METHOD_STRLEN];   // Inner compression method (applied per block)
  int     NumThreads;                   // Worker threads; 0 = auto (GetCompressionThreads())
  MemSize BlockSize;                    // Bytes per block; 0 = inferred from inner method dictionary

  _4x4_METHOD();

  virtual int  decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int  compress   (CALLBACK_FUNC *callback, void *auxdata);

  virtual void ShowCompressionMethod (char *buf);
  virtual MemSize GetCompressionMem   (void);
  virtual MemSize GetDecompressionMem (void);
  virtual MemSize GetDictionary       (void)         {return BlockSize;}
  virtual MemSize GetBlockSize        (void)         {return BlockSize;}
  virtual void    SetCompressionMem   (MemSize mem);
  virtual void    SetDecompressionMem (MemSize)      {}
  virtual void    SetDictionary       (MemSize dict) {BlockSize = dict;}
  virtual void    SetBlockSize        (MemSize bs)   {BlockSize = bs;}
#endif
  virtual int     doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  int  get_num_threads();
  void get_inner_method (char *buf);
};

COMPRESSION_METHOD* parse_4x4 (char** parameters);

#endif  // __cplusplus

#endif  // DARC_C_4X4_H
