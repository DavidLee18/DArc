/*-------------------------------------------------*/
/* DArc streaming wrapper around libbsc (Ilya      */
/* Grebnov). Exposes block-based libbsc API as a    */
/* FreeArc COMPRESSION_METHOD.                     */
/*-------------------------------------------------*/
#ifndef _C_BSC_H
#define _C_BSC_H

#include "../Compression.h"

#ifdef __cplusplus
extern "C" {
#endif

int bsc_stream_compress (int BlockSize,
                         int LzpHashSize,
                         int LzpMinLen,
                         int BlockSorter,
                         int Coder,
                         CALLBACK_FUNC *callback,
                         void *auxdata);

int bsc_stream_decompress (CALLBACK_FUNC *callback,
                           void *auxdata);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

class BSC_METHOD : public COMPRESSION_METHOD
{
public:
  MemSize BlockSize;      // bytes per block (default 25 MB)
  int     LzpHashSize;    // LZP hash size, 0 disables LZP
  int     LzpMinLen;      // LZP minimum match length
  int     BlockSorter;    // BWT / ST3..ST8
  int     Coder;          // QLFC variant

  BSC_METHOD();

  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);
  virtual void ShowCompressionMethod (char *buf);

  virtual MemSize GetCompressionMem     (void)         {return BlockSize * 5;}
  virtual MemSize GetDecompressionMem   (void)         {return BlockSize * 3;}
  virtual MemSize GetDictionary         (void)         {return BlockSize;}
  virtual MemSize GetBlockSize          (void)         {return BlockSize;}
  virtual void    SetCompressionMem     (MemSize mem)  {SetBlockSize (mem / 5);}
  virtual void    SetDecompressionMem   (MemSize mem)  {SetBlockSize (mem / 3);}
  virtual void    SetDictionary         (MemSize dict) {SetBlockSize (dict);}
  virtual void    SetBlockSize          (MemSize bs);
#endif
};

COMPRESSION_METHOD* parse_BSC (char** parameters);

#endif  // __cplusplus

#endif
