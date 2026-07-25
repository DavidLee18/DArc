extern "C" {
#include "C_MM.h"
}
#define MM_LIBRARY
#include "mm.cpp"


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
        // ':r' is REJECTED rather than parsed. The encoder writes
        // header[0] = 1 + reorder*2, and both decoders reject any flag bit
        // above bit 0 (mm.cpp:287) -- so -mmm:r1 and -mmm:r2 produced archives
        // that could be created successfully and then never extracted.
        // Verified: both round-trip as "CREATED BUT CANNOT BE EXTRACTED".
        //
        // The feature was never finished: reorder_words() is a stub that
        // returns its argument, so :r2 did not even transform the data, it just
        // set an unreadable flag. Failing the method string is strictly better
        // than accepting it and emitting an archive nobody can open.
        case 'r':  error = 1;                                   continue;
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

