/* Reference driver for differential-testing the MM decoder port.
 *
 *     mm_ref c MODE SKIPHDR ISFLOAT NUMCHAN WORDSIZE OFFSET  <in >stream
 *     mm_ref d                                               <stream >out
 *
 * Built a second time with -DUSE_RUST so BOTH directions drive the Rust port
 * instead. Decode was ported first (like REP, Dict, LZP and TTA); the encoder
 * followed, so `c` now has two implementations to compare and the bar for it is
 * byte-exact stream equality, not merely round-trippability. MM defines an
 * archive format: a stream that differs from the C's would still decode through
 * our own decoder while making archives other builds read differently.
 *
 * Both explicit parameters and autodetection are worth exercising, so the
 * encoder arguments are passed straight through: leave NUMCHAN/WORDSIZE at 0
 * and mm_compress runs its WAV-header and entropy detectors, which is how real
 * archives get their header -- and, on input it cannot classify, is how the
 * "stored" branch of the decoder gets reached. Autodetection is the substance
 * of the encoder port (~600 of its ~730 lines), and it is *only* reached with
 * those two left at 0.
 *
 * `reorder` is deliberately not exposed here. It is the one part of MM that
 * cannot be pinned against the reference: the C reorder path is being changed
 * in the same work that ports it -- it wrote a flag no decoder accepted, and
 * gains a block-length prefix to become decodable -- so the pinned C is not an
 * oracle for it. Tests/mm-reorder-check.sh covers that path by round-trip
 * instead, across every tail remainder.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Compression/Compression.h"

// mm.cpp, compiled standalone here, has C++ linkage (C_MM.cpp gives it C
// linkage only by declaring it inside `extern "C"` first). The Rust drop-in is
// extern "C".
int mm_compress   (int, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
int mm_decompress (CALLBACK_FUNC*, void*);
#ifdef USE_RUST
extern "C" int darc_rs_mm_decompress (CALLBACK_FUNC*, void*);
extern "C" int darc_rs_mm_compress   (int, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
#endif

struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
};
static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return FREEARC_ERRCODE_GENERAL;
  if (strcmp(what,"read")==0) {
    size_t avail=b->in_len-b->in_pos, n=(size_t)size<avail?(size_t)size:avail;
    memcpy(data,b->in+b->in_pos,n); b->in_pos+=n; return (int)n;
  }
  if (strcmp(what,"write")==0) {
    if (b->out_len+(size_t)size>b->out_cap) {
      size_t cap=b->out_cap?b->out_cap:65536;
      while (cap<b->out_len+(size_t)size) cap*=2;
      unsigned char *g=(unsigned char*)realloc(b->out,cap);
      if(!g) return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
      b->out=g; b->out_cap=cap;
    }
    memcpy(b->out+b->out_len,data,(size_t)size); b->out_len+=(size_t)size; return size;
  }
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv) {
  if (argc<2 || (argv[1][0]!='c'&&argv[1][0]!='d')) {
    fprintf(stderr,"usage: %s c MODE SKIPHDR ISFLOAT NUMCHAN WORDSIZE OFFSET | d\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }
  Buffers b={in,len,0,NULL,0,0};
  int rc;
  if (argv[1][0]=='c') {
    int mode        = argc>2? atoi(argv[2]) : 9;
    int skip_header = argc>3? atoi(argv[3]) : 0;
    int is_float    = argc>4? atoi(argv[4]) : 0;
    int num_chan    = argc>5? atoi(argv[5]) : 0;
    int word_size   = argc>6? atoi(argv[6]) : 0;
    int offset      = argc>7? atoi(argv[7]) : 0;
#ifdef USE_RUST
    rc = darc_rs_mm_compress (mode, skip_header, is_float, num_chan, word_size, offset, 0, io_callback, &b);
#else
    rc = mm_compress (mode, skip_header, is_float, num_chan, word_size, offset, 0, io_callback, &b);
#endif
  } else {
#ifdef USE_RUST
    rc = darc_rs_mm_decompress (io_callback, &b);
#else
    rc = mm_decompress (io_callback, &b);
#endif
  }
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
