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
#ifdef USE_RUST
int darc_rs_grzip_decompress_block (const unsigned char *in, int in_size,
                                    unsigned char *out, int out_cap);
#endif
}

int main (int argc, char **argv) {
  if (argc<2 || (argv[1][0]!='c'&&argv[1][0]!='d')) {
    fprintf(stderr,"usage: %s c MODE | d SIZE\n",argv[0]); return 2; }
  size_t cap=1<<20, len=0; unsigned char *in=(unsigned char*)malloc(cap); if(!in) return 3;
  for(;;){ if(len==cap){cap*=2; unsigned char*g=(unsigned char*)realloc(in,cap); if(!g){free(in);return 3;} in=g;}
    size_t n=fread(in+len,1,cap-len,stdin); if(n==0)break; len+=n; }

  int rc;
  // Generous slack: the compressor may expand incompressible input, and the
  // decompressor's capacity is supplied by the caller in the real pipeline.
  size_t out_cap = len*2 + (1<<20);
  unsigned char *out=(unsigned char*)malloc(out_cap);
  if(!out){free(in);return 3;}

  if (argv[1][0]=='c') {
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
