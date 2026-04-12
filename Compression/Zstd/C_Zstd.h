/*-------------------------------------------------*/
/* DArc streaming wrapper around zstd (Yann Collet, */
/* Meta). Exposes zstd as a FreeArc                 */
/* COMPRESSION_METHOD.                              */
/*-------------------------------------------------*/
#ifndef _C_ZSTD_H
#define _C_ZSTD_H

#include "../Compression.h"

#ifdef __cplusplus
extern "C" {
#endif

int zstd_stream_compress   (int Level, int WindowLog, int Workers,
                            CALLBACK_FUNC *callback, void *auxdata);
int zstd_stream_decompress (CALLBACK_FUNC *callback, void *auxdata);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

class ZSTD_METHOD : public COMPRESSION_METHOD
{
public:
  int Level;       // compression level 1..22 (default 3)
  int WindowLog;   // 0 = default; >0 = long-range mode with given log2 window
  int Workers;     // 0 = single-threaded; >0 = N worker threads

  ZSTD_METHOD();

  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);
  virtual void ShowCompressionMethod (char *buf);

  virtual MemSize GetCompressionMem   (void);
  virtual void    SetCompressionMem   (MemSize mem);
#endif
  virtual MemSize GetDecompressionMem (void);
  virtual MemSize GetDictionary       (void)         {return 0;}
  virtual MemSize GetBlockSize        (void)         {return 0;}
  virtual void    SetDecompressionMem (MemSize)      {}
  virtual void    SetDictionary       (MemSize)      {}
  virtual void    SetBlockSize        (MemSize)      {}
};

COMPRESSION_METHOD* parse_ZSTD (char** parameters);

#endif  // __cplusplus

#endif
