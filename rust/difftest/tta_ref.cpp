/* Reference driver for differential-testing the TTA decoder port.
 *
 *     tta_ref c LEVEL NUMCHAN WORDSIZE ISFLOAT [RAW]  <in >stream
 *     tta_ref d                                        <stream >out
 *
 * Built a second time with -DUSE_RUST so `d` drives the Rust port instead. TTA
 * is ported decode-first (like REP and Dict), so there is no Rust encoder: the
 * stream always comes from the C compressor, and the test is that the Rust
 * decoder reproduces the original bytes from it -- which is what proves it reads
 * the archive format correctly.
 *
 * NUMCHAN/WORDSIZE are passed explicitly so the encoder takes its sample path
 * deterministically rather than running mmdet's autodetection (which, on
 * synthetic input, would often choose "storing" and never exercise the codec).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Compression/Compression.h"

// tta.cpp, compiled standalone here, has C++ linkage (C_TTA.cpp gives it C
// linkage only by wrapping its includes; this driver compiles tta.cpp directly,
// so match its C++ linkage). The Rust drop-in is extern "C".
int tta_compress   (int, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
int tta_decompress (CALLBACK_FUNC*, void*);
#ifdef USE_RUST
extern "C" int darc_rs_tta_decompress (CALLBACK_FUNC*, void*);
extern "C" int darc_rs_tta_compress   (int, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
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
    fprintf(stderr,"usage: %s c LEVEL NUMCHAN WORDSIZE ISFLOAT [RAW] | d\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }
  Buffers b={in,len,0,NULL,0,0};
  int rc;
  if (argv[1][0]=='c') {
    int level    = argc>2? atoi(argv[2]) : 3;
    int num_chan = argc>3? atoi(argv[3]) : 2;
    int word_size= argc>4? atoi(argv[4]) : 16;
    int is_float = argc>5? atoi(argv[5]) : 0;
    int raw_data = argc>6? atoi(argv[6]) : 0;
#ifdef USE_RUST
    rc = darc_rs_tta_compress (level, 0, is_float, num_chan, word_size, 0, raw_data, io_callback, &b);
#else
    rc = tta_compress (level, 0, is_float, num_chan, word_size, 0, raw_data, io_callback, &b);
#endif
  } else {
#ifdef USE_RUST
    rc = darc_rs_tta_decompress (io_callback, &b);
#else
    rc = tta_decompress (io_callback, &b);
#endif
  }
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
