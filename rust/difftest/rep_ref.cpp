/* Reference driver for differential-testing the REP decoder port.
 *
 *     rep_ref c <in >stream     compress with the C original (the only encoder)
 *     rep_ref d <stream >out    decompress with the C original
 *
 * Built a second time with -DUSE_RUST so `d` drives the Rust port instead. REP
 * is ported decode-first (like Dict), so there is no Rust encoder: the stream
 * always comes from the C compressor, and the test is that the Rust decoder
 * reproduces the original bytes from it -- which is what proves it reads the
 * archive format correctly.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Compression/Compression.h"

// rep.cpp, compiled standalone here, has C++ linkage (C_REP.cpp gives it C
// linkage only by including C_REP.h's extern "C" decls first; this driver
// compiles rep.cpp directly, so match its C++ linkage).
int rep_compress   (MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
int rep_decompress (MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
#ifdef USE_RUST
extern "C" int darc_rs_rep_decompress (MemSize, int, int, int, int, int, int, CALLBACK_FUNC*, void*);
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
  if (argc<2 || (argv[1][0]!='c'&&argv[1][0]!='d')) { fprintf(stderr,"usage: %s c|d\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }
  Buffers b={in,len,0,NULL,0,0};
  // Defaults from REP_METHOD::REP_METHOD().
  const MemSize BlockSize=64u*1024*1024; const int MinCompression=100, MinMatchLen=512,
        Barrier=0x7fffffff, SmallestLen=512, HashSizeLog=0, Amplifier=1;
  int rc;
  if (argv[1][0]=='c')
    rc = rep_compress (BlockSize,MinCompression,MinMatchLen,Barrier,SmallestLen,HashSizeLog,Amplifier,io_callback,&b);
  else
#ifdef USE_RUST
    rc = darc_rs_rep_decompress (BlockSize,MinCompression,MinMatchLen,Barrier,SmallestLen,HashSizeLog,Amplifier,io_callback,&b);
#else
    rc = rep_decompress (BlockSize,MinCompression,MinMatchLen,Barrier,SmallestLen,HashSizeLog,Amplifier,io_callback,&b);
#endif
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
