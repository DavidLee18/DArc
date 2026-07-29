/* Reference driver for differential-testing the Tornado decoder port.
 *
 *     tornado_ref c PRESET  <in >stream
 *     tornado_ref d         <stream >out
 *
 * Built a second time with -DUSE_RUST so `d` drives the Rust port. Tornado is
 * ported decode-first, so the C compressor is the only encoder and the test is
 * that the Rust decoder reproduces the original bytes from its output.
 *
 * PRESET indexes std_Tornado_method[], which is what -mtor:N selects. The
 * presets span all four entropy back-ends, which is the point: the byte, bit,
 * huffman and arithmetic decoders share one output loop but nothing else.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Compression/Compression.h"

extern "C" {
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifdef USE_RUST
int darc_rs_tor_decompress (CALLBACK_FUNC *callback, void *auxdata);
#endif
}
struct PackMethod;
extern PackMethod std_Tornado_method[];
int tor_compress_preset (int preset, int notables, CALLBACK_FUNC *callback, void *auxdata);
#ifdef USE_RUST
extern "C" int rust_tor_compress_preset (int preset, int notables, CALLBACK_FUNC *callback, void *auxdata);
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
  if (strcmp(what,"quasiwrite")==0)  return 0;
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main (int argc, char **argv) {
  // 'c' compresses, 'd' decompresses; under -DUSE_RUST both drive the Rust
  // port. Compression is a separate mode rather than a flag because the
  // encoder covers only some presets so far and must be able to refuse.
  if (argc<2 || (argv[1][0]!='c'&&argv[1][0]!='d')) {
    fprintf(stderr,"usage: %s c PRESET [NOTABLES] [ALL_AT_ONCE] | d\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }
  Buffers b={in,len,0,NULL,0,0};
  int rc;
  if (argv[1][0]=='c') {
    int preset = argc>2? atoi(argv[2]) : 4;
    int notables = argc>3? atoi(argv[3]) : 0;
    // 4th arg sets `compress_all_at_once`, which BOTH encoders read (the C
    // inside tor_compress_chunk, the Rust via the value tornado_ccodec.cpp
    // passes). It defaults to 0 and nothing here used to set it, so preset 9 in
    // all-at-once mode -- the mode 4x4 forces, and the only caller that does --
    // was never compared. That is where the port diverges.
    compress_all_at_once = argc>4? atoi(argv[4]) : 0;
#ifdef USE_RUST
    rc = rust_tor_compress_preset (preset, notables, io_callback, &b);
#else
    rc = tor_compress_preset (preset, notables, io_callback, &b);
#endif
  } else {
#ifdef USE_RUST
    rc = darc_rs_tor_decompress (io_callback, &b);
#else
    rc = tor_decompress (io_callback, &b);
#endif
  }
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
