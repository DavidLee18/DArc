extern "C" {
#include "C_BCJ.h"
}

// The x86 branch filter and this driver loop now live in rust/darc-codecs/src/bcj.rs,
// byte-identical to the C they replace (rust/difftest/bcj-check.sh). The three
// #includes that used to pull in 7zip/Compress/Branch/ are gone with them -- those
// files were the last live users of that directory.
//
// bcj_x86_compress / bcj_x86_decompress are declared in C_BCJ.h, which is included
// inside `extern "C"` above, so the Rust symbols already have the right linkage.


/*-------------------------------------------------*/
/* BCJ_X86_METHOD class implementation             */
/*-------------------------------------------------*/
// Decompression function
int BCJ_X86_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_x86_decompress (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int BCJ_X86_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  return bcj_x86_compress (callback, auxdata);
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
