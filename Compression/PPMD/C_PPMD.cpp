#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

extern "C" {
#include "C_PPMD.h"
}
#include "PPMdType.h"

/*-------------------------------------------------*/
/* ppmd_compress implementation                    */
/*-------------------------------------------------*/
#ifndef FREEARC_DECOMPRESS_ONLY

namespace PPMD_compression {

#include "Model.cpp"

extern "C" {
int ppmd_compress (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata)
{
  if ( !StartSubAllocator(mem) ) {
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }
  _PPMD_FILE* fpIn  = new _PPMD_FILE (callback, auxdata);
  _PPMD_FILE* fpOut = new _PPMD_FILE (callback, auxdata);
  EncodeFile (fpOut, fpIn, order, MR_METHOD(MRMethod));
  fpOut->flush();
  int ErrCode = FREEARC_OK;
  if (_PPMD_ERROR_CODE(fpIn) <0)  ErrCode = _PPMD_ERROR_CODE (fpIn);
  if (_PPMD_ERROR_CODE(fpOut)<0)  ErrCode = _PPMD_ERROR_CODE (fpOut);
  delete fpOut;
  delete fpIn;
  StopSubAllocator();
  return ErrCode;
}
} // extern "C"

void _STDCALL PrintInfo (_PPMD_FILE* DecodedFile, _PPMD_FILE* EncodedFile)
{
}

} // namespace PPMD_compression
#undef _PPMD_H_

#endif // FREEARC_DECOMPRESS_ONLY


/*-------------------------------------------------*/
/* ppmd_decompress implementation                  */
/*-------------------------------------------------*/

namespace PPMD_decompression {

#include "Model.cpp"

extern "C" {
int ppmd_decompress (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata)
{
  if ( !StartSubAllocator(mem) ) {
    return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
  }
  _PPMD_FILE* fpIn  = new _PPMD_FILE (callback, auxdata);
  _PPMD_FILE* fpOut = new _PPMD_FILE (callback, auxdata);
  DecodeFile (fpOut, fpIn, order, MR_METHOD(MRMethod));
  fpOut->flush();
  int ErrCode = FREEARC_OK;
  if (_PPMD_ERROR_CODE(fpIn) <0)  ErrCode = _PPMD_ERROR_CODE (fpIn);
  if (_PPMD_ERROR_CODE(fpOut)<0)  ErrCode = _PPMD_ERROR_CODE (fpOut);
  delete fpOut;
  delete fpIn;
  StopSubAllocator();
  return ErrCode;
}
} // extern "C"

void _STDCALL PrintInfo(_PPMD_FILE* DecodedFile,_PPMD_FILE* EncodedFile)
{
}

} // namespace PPMD_decompression


/*-------------------------------------------------*/
/* PPMD_METHOD class implementation               */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
PPMD_METHOD::PPMD_METHOD()
{
  order    = 10;
  mem      = 48*mb;
  MRMethod = 0;
}

// Decompression function
int PPMD_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("ppmd_decompress");
  if (!f) f = (FARPROC) ppmd_decompress;

  return ((int (*)(int, MemSize, int, CALLBACK_FUNC*, void*)) f) (order, mem, MRMethod, callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int PPMD_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("ppmd_compress");
  if (!f) f = (FARPROC) ppmd_compress;

  return ((int (*)(int, MemSize, int, CALLBACK_FUNC*, void*)) f) (order, mem, MRMethod, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_PPMD)
void PPMD_METHOD::ShowCompressionMethod (char *buf)
{
  char MemStr[100];
  showMem (mem, MemStr);
  sprintf (buf, "ppmd:%d:%s%s", order, MemStr, MRMethod==2? ":r2": (MRMethod==1? ":r":""));
}

// Change the memory requirement, tuning order along the way
void PPMD_METHOD::SetCompressionMem (MemSize _mem)
{
  if (_mem==0)  return;
  order  +=  int (log(double(_mem)/mem) / log(double(2)) * 4);
  mem = _mem;
}


#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs a PPMD_METHOD object with the given compression parameters,
// or returns NULL if this is a different compression method or the parameters are malformed
COMPRESSION_METHOD* parse_PPMD (char** parameters)
{
  if (strcmp (parameters[0], "ppmd") == 0) {
    // If the method name (parameter zero) is "ppmd", parse the remaining parameters

    PPMD_METHOD *p = new PPMD_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Iterate over all method parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char *param = *parameters;
      if (start_with (param, "mem")) {
        param+=2;  // Handle "mem..." the same as "m..."
      }
      if (strlen(param)==1) switch (*param) {    // Single-letter parameters
        case 'r':  p->MRMethod = 1; continue;
      }
      else switch (*param) {                    // Parameters carrying values
        case 'm':  p->mem      = parseMem (param+1, &error); continue;
        case 'o':  p->order    = parseInt (param+1, &error); continue;
        case 'r':  p->MRMethod = parseInt (param+1, &error); continue;
      }
      // We get here if the parameter has no name given
      // If this parameter parses as an integer (i.e. it contains only digits),
      // assign its value to the order field, otherwise try to parse it as mem
      int n = parseInt (param, &error);
      if (!error) p->order = n;
      else        error=0, p->mem = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the ppmd method
}

static int PPMD_x = AddCompressionMethod (parse_PPMD);   // Register the PPMD method parser
