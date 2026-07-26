/* Forward sort-transform, C vs Rust.
 *
 *     bsc_st_enc_ref K   <in >out      K = 3..6
 *
 * stdout is the primary index as a 4-byte little-endian int followed by the
 * transformed bytes. Both are compared: the index alone decides where the
 * decoder starts unwinding, so a permutation that is right with a wrong index
 * decodes to garbage while looking plausible byte-for-byte.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C" {
int darc_bsc_init (int features);
int darc_bsc_st_encode (unsigned char *T, int n, int k, int features);
#ifdef USE_RUST
int darc_rs_bsc_st_encode (unsigned char *data, int n, int k);
#endif
}
int main(int argc, char **argv){
  int k = argc > 1 ? atoi(argv[1]) : 3;
  darc_bsc_init(0);
  size_t cap=1<<20, len=0; unsigned char*in=(unsigned char*)malloc(cap+64);
  for(;;){ if(len==cap){cap*=2; in=(unsigned char*)realloc(in,cap+64);} size_t r=fread(in+len,1,cap-len,stdin); if(!r)break; len+=r; }
  int n=(int)len; if(n<=1){free(in);return 0;}
  unsigned char*T=(unsigned char*)malloc(n+64); memcpy(T,in,n);
#ifdef USE_RUST
  int idx=darc_rs_bsc_st_encode(T,n,k);
#else
  int idx=darc_bsc_st_encode(T,n,k,0);
#endif
  unsigned char h[4]={(unsigned char)(idx&0xff),(unsigned char)((idx>>8)&0xff),(unsigned char)((idx>>16)&0xff),(unsigned char)((idx>>24)&0xff)};
  fwrite(h,1,4,stdout); if(idx>=0) fwrite(T,1,n,stdout);
  free(T); free(in); return 0;
}
