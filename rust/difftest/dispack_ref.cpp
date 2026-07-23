/* Reference driver for differential-testing the DisPack decoder port.
 *
 *     dispack_ref c [blocksize]  <in >stream
 *     dispack_ref d [blocksize]  <stream >out
 *
 * Built a second time with -DUSE_RUST so `d` drives the Rust port. DisPack is
 * ported decode-first, so the C compressor is the only encoder.
 *
 * DisPack is an x86 filter, so the corpus must be actual x86 code for TAG_EXE
 * (the filtered path) to be exercised at all -- the compressor's `detect()`
 * stores non-code data raw. dispack-check.sh feeds it real binaries.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Compression/Compression.h"

extern "C" {
int darc_dispack_decompress (unsigned block_size, CALLBACK_FUNC *cb, void *aux);
int darc_dispack_compress   (unsigned block_size, CALLBACK_FUNC *cb, void *aux);
#ifdef USE_RUST
int darc_rs_dispack_decompress (unsigned block_size, CALLBACK_FUNC *cb, void *aux);
#endif
}

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
    fprintf(stderr,"usage: %s c|d [blocksize]\n",argv[0]); return 2; }
  unsigned bs = argc>2? (unsigned)strtoul(argv[2],NULL,0) : 8u*1024*1024;
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }
  Buffers b={in,len,0,NULL,0,0};
  int rc;
  if (argv[1][0]=='c') {
    rc = darc_dispack_compress (bs, io_callback, &b);
  } else {
#ifdef USE_RUST
    rc = darc_rs_dispack_decompress (bs, io_callback, &b);
#else
    rc = darc_dispack_decompress (bs, io_callback, &b);
#endif
  }
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(b.out); return 4; }
  if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(b.out); return 5; }
  free(in); free(b.out); return 0;
}
