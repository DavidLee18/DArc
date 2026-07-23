/* Reference driver for differential-testing the GRZip decoder port.
 *
 *     grzip_ref c MODE  <in >block
 *     grzip_ref d SIZE  <block >out
 *
 * Built a second time with -DUSE_RUST so `d` drives the Rust port. GRZip is
 * ported decode-first, so the C compressor is the only encoder.
 *
 * This works at the *block* level rather than the stream level: the stream
 * wrapper (grzip_decompress) is not ported yet, but GRZip_DecompressBlock is,
 * and it is where every ported stage actually runs. SIZE is the output
 * capacity, which the stream layer would otherwise supply.
 *
 * MODE is GRZip's mode word: bit1 selects ST4 over BWT, bit2 selects the MTF
 * arithmetic coder over WFC, and the upper bits carry the LZP parameters.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
int darc_grz_compress_block   (unsigned char *in, int size, unsigned char *out, int mode);
int darc_grz_decompress_block (unsigned char *in, int size, unsigned char *out);
int darc_grz_stream_compress  (int method, int blocksize, int enable_lzp, int minlen,
                               int hashlog, int altsort, int adaptive, int deltaflt,
                               int (*cb)(const char*, void*, int, void*), void *aux);
int darc_grz_stream_decompress(int (*cb)(const char*, void*, int, void*), void *aux);
#ifdef USE_RUST
int darc_rs_grzip_decompress_block (const unsigned char *in, int in_size,
                                    unsigned char *out, int out_cap);
int darc_rs_grzip_decompress (int (*cb)(const char*, void*, int, void*), void *aux);
#endif
}

/* Callback pair for the stream-level modes ("sc" / "sd"), which drive
 * grzip_compress / grzip_decompress rather than a single block. Only the stream
 * layer splits input into blocks, so this is the only way to exercise an input
 * larger than GRZ_MaxBlockSize. */
struct Buffers {
  const unsigned char *in; size_t in_len, in_pos;
  unsigned char *out; size_t out_len, out_cap;
};
static int io_callback (const char *what, void *data, int size, void *aux) {
  Buffers *b = (Buffers*) aux;
  if (size < 0) return -1;
  if (strcmp(what,"read")==0) {
    size_t avail=b->in_len-b->in_pos, n=(size_t)size<avail?(size_t)size:avail;
    memcpy(data,b->in+b->in_pos,n); b->in_pos+=n; return (int)n;
  }
  if (strcmp(what,"write")==0) {
    if (b->out_len+(size_t)size>b->out_cap) {
      size_t cap=b->out_cap?b->out_cap:65536;
      while (cap<b->out_len+(size_t)size) cap*=2;
      unsigned char *g=(unsigned char*)realloc(b->out,cap);
      if(!g) return -1;
      b->out=g; b->out_cap=cap;
    }
    memcpy(b->out+b->out_len,data,(size_t)size); b->out_len+=(size_t)size; return size;
  }
  return 0;
}

int main (int argc, char **argv) {
  int stream = (argc>1 && argv[1][0]=='s');
  const char *op = stream? argv[1]+1 : (argc>1? argv[1] : "");
  if (argc<2 || (op[0]!='c'&&op[0]!='d')) {
    fprintf(stderr,"usage: %s c MODE | d SIZE | sc | sd\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }

  int rc;
  // Generous slack: the compressor may expand incompressible input, and the
  // decompressor's capacity is supplied by the caller in the real pipeline.
  size_t out_cap = len*2 + (1<<20);
  unsigned char *out=(unsigned char*)malloc(out_cap);
  if(!out){free(in);return 3;}

  if (stream) {
    Buffers b={in,len,0,NULL,0,0};
    if (op[0]=='c') rc = darc_grz_stream_compress (1, 8*1024*1024, 1, 32, 15, 0, 0, 0, io_callback,&b);  // GRZIP_METHOD defaults
#ifdef USE_RUST
    else            rc = darc_rs_grzip_decompress (io_callback,&b);
#else
    else            rc = darc_grz_stream_decompress (io_callback,&b);
#endif
    if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(out); free(b.out); return 4; }
    if (b.out_len && fwrite(b.out,1,b.out_len,stdout)!=b.out_len){ free(in); free(out); free(b.out); return 5; }
    free(in); free(out); free(b.out); return 0;
  }

  if (op[0]=='c') {
    int mode = argc>2? (int)strtol(argv[2],NULL,0) : 0;
    rc = darc_grz_compress_block (in, (int)len, out, mode);
  } else {
    int size = argc>2? atoi(argv[2]) : (int)out_cap;
    if ((size_t)size > out_cap) size = (int)out_cap;
#ifdef USE_RUST
    rc = darc_rs_grzip_decompress_block (in, (int)len, out, size);
#else
    rc = darc_grz_decompress_block (in, (int)len, out);
#endif
  }
  if (rc<0){ fprintf(stderr,"codec returned %d\n",rc); free(in); free(out); return 4; }
  if (rc>0 && fwrite(out,1,(size_t)rc,stdout)!=(size_t)rc){ free(in); free(out); return 5; }
  free(in); free(out); return 0;
}
