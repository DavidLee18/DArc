#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

extern "C" {
#include "C_PPMD.h"
}

// PPMd var.H lives in rust/darc-codecs/src/ppmd/ -- the model, the suballocator,
// the Subbotin range coder and the PRIME_STREAM buffering. This file is the
// COMPRESSION_METHOD wrapper around it and nothing else.
//
// The port is byte-identical to Shkarin's C as DArc built it, proven per push by
// rust/difftest/ppmd-check.sh over order x memory x MRMethod x a 19-entry
// corpus, and by ppmd-alloc-check.sh over the suballocator alone. Byte-identity
// is the bar because the model branches on allocator state -- GetUsedMemory(),
// pText/UnitsStart crossings -- so the heap layout is part of the format and a
// merely-correct implementation still writes different archives.
extern "C" {
int darc_rs_ppmd_compress   (int order, unsigned mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata);
int darc_rs_ppmd_decompress (int order, unsigned mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata);
}

// Valid range for the model order.
//
// The lower bound is not cosmetic. PPMd reserves order<2 as an internal signal
// meaning "do not restart the model, continue the existing one" -- see the
// comment block that was in PPMd.h. StartModelRare's solid-mode branch walks
// MaxContext without initialising it, and MaxContext is BSS. ppmd_compress
// starts the suballocator and encodes afresh on every call, so it can never
// legitimately be handed that signal: "arc a -mppmd:o0" and "-mppmd:o1"
// dereferenced a NULL MaxContext and died with SIGSEGV.
//
// 128 is the model's own limit: `const int MAX_O=128` in the deleted
// PPMdType.h, mirrored as MAX_O in rust/darc-codecs/src/ppmd/model.rs.
static const int PPMD_MIN_ORDER = 2;
static const int PPMD_MAX_ORDER = 128;

/*-------------------------------------------------*/
/* ppmd_compress / ppmd_decompress                 */
/*-------------------------------------------------*/

// Straight pass-through. The C used to instantiate Model.cpp twice, in two
// namespaces, because its model state was file-scope statics and the two
// directions would otherwise have shared them; the Rust port carries its state
// in a struct, so one entry point per direction is enough.

#ifndef FREEARC_DECOMPRESS_ONLY
extern "C" {
int ppmd_compress (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata)
{
  return darc_rs_ppmd_compress (order, mem, MRMethod, callback, auxdata);
}
} // extern "C"
#endif // FREEARC_DECOMPRESS_ONLY

extern "C" {
int ppmd_decompress (int order, MemSize mem, int MRMethod, CALLBACK_FUNC *callback, void *auxdata)
{
  return darc_rs_ppmd_decompress (order, mem, MRMethod, callback, auxdata);
}
} // extern "C"


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
  // Clamp: the adjustment is unbounded, so shrinking memory by more than 4x
  // drove order below the model's minimum. "-mppmd:o10:m64m -lc4m" produced
  // order -6 and a method string "ppmd:-6:4mb" that parse_PPMD then rejected,
  // so limiting memory made a valid method unusable with a confusing
  // "Unsupported compression method" error.
  order = mymax (order, PPMD_MIN_ORDER);
  order = mymin (order, PPMD_MAX_ORDER);
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
    // Reject an out-of-range order rather than letting it reach the model.
    // Rejecting here makes it an "unsupported method or error in parameters"
    // message, which is what every other malformed parameter produces; without
    // it "-mppmd:o0" and "-mppmd:o1" reached StartModelRare's solid-mode branch
    // and crashed. See PPMD_MIN_ORDER above.
    if (p->order < PPMD_MIN_ORDER || p->order > PPMD_MAX_ORDER)  {delete p; return NULL;}
    return p;
  } else
    return NULL;   // This is not the ppmd method
}

static int PPMD_x = AddCompressionMethod (parse_PPMD);   // Register the PPMD method parser
