#include "../Compression.h"
#include "../_CLS/cls.h"


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class CLS_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  char     name[100];            // Method name (pmm, ccm...)       ////
  char     params[100];          // Additional method parameters    ////
  CLS_MAIN *ClsMain;
  CALLBACK_FUNC *callback;
  void *auxdata;

  CLS_METHOD(char *_name, CLS_MAIN *_ClsMain)
    { strcpy(name, _name); ClsMain = _ClsMain; strcpy(params, ""); }

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_CLS)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)          {return 0;}
  virtual MemSize GetDecompressionMem   (void)          {return 0;}
  virtual MemSize GetDictionary         (void)          {return 0;}
  virtual MemSize GetBlockSize          (void)          {return 0;}
  virtual void    SetCompressionMem     (MemSize)  {}
  virtual void    SetDecompressionMem   (MemSize)  {}
  virtual void    SetDictionary         (MemSize)  {}
  virtual void    SetBlockSize          (MemSize)  {}
#endif
};

// Parser for the CLS preprocessor string
COMPRESSION_METHOD* parse_CLS (char** parameters);

#endif  // __cplusplus
