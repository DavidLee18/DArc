extern "C" {
#include "C_MM.h"
}

// mm.cpp is gone. Both mm_compress and mm_decompress come from
// rust/darc-codecs/src/mm.rs, and what remained of that file was the diff/undiff
// helpers they used plus a standalone driver.
//
// What it also did, and what still has to happen here, is pull in the detector.
// mmdet.cpp is NOT dead code, though it looks it: its own comment defends it by
// a tta.cpp call to autodetect_wav_header/autodetect_by_entropy that no longer
// exists, so searching for those two names finds nothing. But mmdet.h exports
// four more -- detect_datatype, detect_mm, detect_mm_header, detect_mm_bytes --
// and all four are bound by the Haskell FFI at ArhiveFileList.hs:588-598, where
// they decide $text/$exe/$compressed grouping and MM autodetection. That is
// solid-block layout, so this include is load-bearing for what archives look
// like, not just for what compresses.
//
// Both directives are carried over from mm.cpp verbatim:
//   * MMD_LIBRARY suppresses mmdet.cpp's own standalone main (mmdet.cpp:913).
//   * the FREEARC_DECOMPRESS_ONLY guard keeps the detector out of Unarc, which
//     never decides how to compress anything and so never calls it.
#define MMD_LIBRARY
#ifndef FREEARC_DECOMPRESS_ONLY
#include "mmdet.cpp"
#endif


/*-------------------------------------------------*/
/* Implementation of the MM_METHOD class           */
/*-------------------------------------------------*/

// Constructor that assigns default values to the compression method parameters
MM_METHOD::MM_METHOD()
{
    mode        = 9;
    skip_header = 0;
    is_float    = 0;
    num_chan    = 0;
    word_size   = 0;
    offset      = 0;
    reorder     = 0;
}

// Decompression function
int MM_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    return mm_decompress (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Compression function
int MM_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    return mm_compress (mode, skip_header, is_float, num_chan, word_size, offset, reorder, callback, auxdata);
}

// Write into buf[MAX_METHOD_STRLEN] a string describing the compression method and its parameters (the inverse of parse_MM)
void MM_METHOD::ShowCompressionMethod (char *buf)
{
    MM_METHOD defaults;
    char dStr[100], cStr[100], rStr[100];
    if (num_chan || word_size) {
        sprintf (cStr, ":%d*%d%s", num_chan, word_size, is_float? "f":"");
        if (offset)  sprintf (str_end(cStr), ":o%d", offset);
    } else {
        sprintf (cStr, skip_header? ":s" : "");
    }
    sprintf (rStr, reorder? ":r%d" : "", reorder);
    sprintf (dStr, mode!=defaults.mode? ":d%d" : "", mode);
    sprintf (buf, "mm%s%s%s", dStr, cStr, rStr);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Constructs an MM_METHOD object with the given compression parameters
// or returns NULL if this is a different compression method or the parameters are invalid
COMPRESSION_METHOD* parse_MM (char** parameters)
{
  if (strcmp (parameters[0], "mm") == 0) {
    // If the method name (parameter zero) is "mm", parse the remaining parameters

    MM_METHOD *p = new MM_METHOD;
    int error = 0;  // Flag indicating that an error occurred while parsing the parameters

    // Walk through all method parameters (or bail out early if parsing one of them fails)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // Parameters that carry a value
        case 's':  p->skip_header = 1;                          continue;
        case 'f':  p->is_float    = 1;                          continue;
        case 'd':  p->mode        = parseInt (param+1, &error); continue;
        case 'c':  p->num_chan    = parseInt (param+1, &error); continue;
        case 'w':  p->word_size   = parseInt (param+1, &error); continue;
        case 'o':  p->offset      = parseInt (param+1, &error); continue;
        // ':r1' is byte reordering, now implemented in both directions. ':r2'
        // was reorder_words, whose C implementation is `return buf;` -- it
        // transformed nothing and only set a flag no decoder accepted, so it is
        // rejected rather than resurrected as a no-op.
        case 'r':  p->reorder = parseInt (param+1, &error);
                   if (!error && p->reorder != 0 && p->reorder != 1)  error = 1;
                   continue;
      }
      // We end up here when the parameter has no name given
      // If this parameter can be parsed as c*w,
      // then use those values for the num_chan and word_size fields.
      // An additional 'f' character means the data is in FP format
      int a, b;  char s[MAX_METHOD_STRLEN];
      if (sscanf (param, "%d*%d%s", &a, &b, s)==3  &&  strequ(s,"f"))
          p->is_float = 1, p->num_chan=a, p->word_size=b;
      else if (sscanf (param, "%d*%d", &a, &b)==2)
          p->is_float = 0, p->num_chan=a, p->word_size=b;
      else error=1;
    }
    if (error)  {delete p; return NULL;}  // Error while parsing the method parameters
    return p;
  } else
    return NULL;   // This is not the MM method
}

static int MM_x = AddCompressionMethod (parse_MM);   // Register the MM method parser

