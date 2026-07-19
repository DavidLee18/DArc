#include "../Compression.h"

// Maximum size of the key/iv/hash in bytes
#define MAXKEYSIZE 64

enum TEncrypt {ENCRYPT, DECRYPT};
int docrypt (enum TEncrypt DoEncryption, int cipher, int mode, BYTE *key, int keysize, int rounds, BYTE *iv,
             CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// Implementation of the standard COMPRESSION_METHOD interface for compression/encryption methods
class ENCRYPTION_METHOD : public COMPRESSION_METHOD
{
public:
  // Parameters of this encryption method
  int  cipher;                   // ID of the encryption algorithm
  int  mode;                     // ID of the encryption mode (ctr, cfb...)
  char key [MAXKEYSIZE*2+1];     // Key in base16 notation
  char iv  [MAXKEYSIZE*2+1];     // Initialization vector in base16 notation
  char salt[MAXKEYSIZE*2+1];     // "Salt" in base16 notation
  char code[MAXKEYSIZE*2+1];     // Self-check code in base16 notation
  int  numIterations;            // Number of iterations when generating the key from password+salt
  int  rounds;                   // Number of rounds during encryption; the more, the slower but the more secure. 0 - use the default value recommended by the algorithm's authors
  int  keySize;                  // Key length in bytes (0 - use the maximum available)
  int  ivSize;                   // IV length in bytes (= the block size of the encryption algorithm)

  // Constructor that assigns the default values to the encryption method parameters
  ENCRYPTION_METHOD();

  // Universal method, answers the "encryption?", "KeySize" and "IVSize" queries
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // Decompression and compression functions
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_ENCRYPTION)
  virtual void ShowCompressionMethod (char *buf);

  // Get/set the amount of memory used during packing/unpacking, the dictionary size or the block size
  virtual MemSize GetCompressionMem     (void)         {return 0;}
  virtual MemSize GetDecompressionMem   (void)         {return 0;}
  virtual MemSize GetDictionary         (void)         {return 0;}
  virtual MemSize GetBlockSize          (void)         {return 0;}
  virtual void    SetCompressionMem     (MemSize mem)  {}
  virtual void    SetDecompressionMem   (MemSize mem)  {}
  virtual void    SetDictionary         (MemSize dict) {}
  virtual void    SetBlockSize          (MemSize bs)   {}
#endif
};

// Parser for the encryption method string
COMPRESSION_METHOD* parse_ENCRYPTION (char** parameters);

#endif  // __cplusplus
