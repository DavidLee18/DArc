#include "../Compression.h"

int external_compress   (char *packcmd, char *unpackcmd, char *datafile, char *packedfile, CALLBACK_FUNC *callback, void *auxdata);
int external_decompress (char *packcmd, char *unpackcmd, char *datafile, char *packedfile, CALLBACK_FUNC *callback, void *auxdata);

// Add to the compression method table an external compressor described by the user in arc.ini.
// params contains the compressor description from arc.ini. Returns 1 if the description is valid.
int AddExternalCompressor (char *params);

#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD compression method interface
class EXTERNAL_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this compression method
  char    *name;            // Method name (pmm, ccm...)
  bool    can_set_mem;      // Can the memory requirements be changed?
  MemSize cmem;             // Amount of memory used for compression
  MemSize dmem;             // Amount of memory used for decompression
  char    *datafile;        // Name of the file holding the unpacked data
  char    *packedfile;      // Name of the file holding the packed data
  char    *packcmd;         // Command that compresses the data (datafile -> packedfile)
  char    *unpackcmd;       // Command that decompresses the data (packedfile -> datafile)
  char    *options[MAX_PARAMETERS];             // Additional method parameters
  char     option_strings[MAX_METHOD_STRLEN];   // Text buffer holding the parameter text
  char    *defaultopt;      // Default parameter values

  // Parameters specific to PPMonstr
  int     order;            // Model order (how many preceding symbols the next one is predicted from)
  int     MRMethod;         // What to do when the memory allocated for storing the model is exhausted
  int     MinCompression;   // Minimum compression percentage. If the output data is larger, the original (uncompressed) data is stored instead

  EXTERNAL_METHOD() {};
  // Universal method: answer "external?" queries in the affirmative
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if (strequ (what,"external?"))  return 1;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_EXTERNAL)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used for compression/decompression, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)          {return cmem;}
  virtual MemSize GetDecompressionMem   (void)          {return dmem;}
  virtual MemSize GetDictionary         (void)          {return 0;}
  virtual MemSize GetBlockSize          (void)          {return 0;}
  virtual void    SetCompressionMem     (MemSize _mem);
  virtual void    SetDecompressionMem   (MemSize _mem)  {SetCompressionMem(_mem);}
  virtual void    SetDictionary         (MemSize dict)  {}
  virtual void    SetBlockSize          (MemSize bs)    {}
#endif
};

// Parser for the EXTERNAL preprocessor string
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters);

#endif  // __cplusplus
