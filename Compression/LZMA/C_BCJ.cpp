extern "C" {
#include "C_BCJ.h"
}

#include "7zip/Compress/Branch/BranchX86.c"
#include "7zip/Compress/Branch/BranchCoder.cpp"
#include "7zip/Compress/Branch/x86.cpp"

template <class CBCJ_x86_Coder>
int bcj_x86_de_compress( CALLBACK_FUNC *callback, void *auxdata)
{
  CBCJ_x86_Coder c; c.Init();                    // encoder/decoder for the BCJ-X86 preprocessor
  BYTE* Buf = (BYTE*)malloc(LARGE_BUFFER_SIZE);  // room for the data
  if (Buf==NULL)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  int RemainderSize=0;                           // data left over from the previous round
  int x, InSize;                                 // number of bytes read
  while ( (InSize = x = callback ("read", Buf+RemainderSize, LARGE_BUFFER_SIZE-RemainderSize, auxdata)) >= 0 )
  {
    if ((InSize+=RemainderSize)==0)            goto Ok;  // No more data
    int OutSize = InSize<=5? InSize : c.Filter(Buf, InSize);  // this filter doesn't handle less than 5 bytes :)
    if( (x=callback("write",Buf,OutSize,auxdata)) != OutSize )      goto Error;
    RemainderSize = InSize-OutSize;
    // Move the unprocessed remainder of the data to the start of the buffer
    if (RemainderSize>0)                memmove(Buf,Buf+OutSize,RemainderSize);
  }
Error: free(Buf); return x;            // an error occurred while reading/writing
Ok:    free(Buf); return FREEARC_OK;   // everything is fine
}


/*-------------------------------------------------*/
/* BCJ_X86_METHOD class implementation             */
/*-------------------------------------------------*/
// Decompression function
int BCJ_X86_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_x86_de_compress<CBCJ_x86_Decoder> (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int BCJ_X86_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_x86_de_compress<CBCJ_x86_Encoder> (callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method (the inverse of parse_BCJ_X86)
void BCJ_X86_METHOD::ShowCompressionMethod (char *buf)
{
  sprintf (buf, "exe");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs a BCJ_X86_METHOD object, or returns NULL if this is a different compression method
COMPRESSION_METHOD* parse_BCJ_X86 (char** parameters)
{
  if (strcmp (parameters[0], "exe") == 0
      &&  parameters[1]==NULL )
    // If the method name is "exe" and it has no parameters, then this is our method
    return new BCJ_X86_METHOD;
  else
    return NULL;   // This is not the bcj_x86 method
}

static int BCJ_X86_x = AddCompressionMethod (parse_BCJ_X86);   // Register the BCJ_X86 method parser
