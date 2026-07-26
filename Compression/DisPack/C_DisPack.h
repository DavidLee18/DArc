#include "../Compression.h"

// Free decompression function, split out from DISPACK_METHOD::decompress so the
// DARC_RUST switch can replace it at link time. Declared here (and included
// inside C_DisPack.cpp's extern "C" block) so it has C linkage, matching the
// Rust export -- see the DARC_RUST note in C_DisPack.cpp.
int dispack_decompress (MemSize BlockSize, CALLBACK_FUNC *callback, void *auxdata);

#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression-method interface
class DISPACK_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  MemSize BlockSize;        // Size of the data block processed at one time
  int     ExtendedTables;   // Look for tables whose element size differs from 2/4

  // Constructor assigning default values to the compression method's parameters
  DISPACK_METHOD();

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);

  // Outside the guard: SetDecompressionMem below forwards to it, and that one
  // is part of the COMPRESSION_METHOD interface in every build. Leaving it in
  // meant a FREEARC_DECOMPRESS_ONLY build -- which is what Unarc and the SFX
  // modules are -- did not compile at all.
  virtual void    SetMinDecompressionMem   (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/ 9*4,64*kb);}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem        (void)               {return 3*BlockSize+BlockSize/4+1024;}
  virtual void    SetCompressionMem        (MemSize mem)        {if (mem>0)   BlockSize = mymax(mem/13*4,64*kb);}
  virtual void    ShowCompressionMethod    (char *buf);
#endif
  virtual MemSize GetDecompressionMem      (void)               {return 2*BlockSize+BlockSize/4+1024;}

  // DArc COMPRESSION_METHOD API fill-ins (pure in DArc, absent in 0.67).
  virtual MemSize GetDictionary            (void)               {return 0;}
  virtual MemSize GetBlockSize             (void)               {return BlockSize;}
  virtual void    SetDecompressionMem      (MemSize mem)        {SetMinDecompressionMem(mem);}
  virtual void    SetDictionary            (MemSize)            {}
  virtual void    SetBlockSize             (MemSize bs)         {if (bs>0) BlockSize = bs;}
};

// Parser for the DISPACK compression method string
COMPRESSION_METHOD* parse_DISPACK (char** parameters);

#endif  // __cplusplus
