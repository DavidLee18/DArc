/* ST3 forward: C vs Rust. stdout = int32 index + the transformed n bytes. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C" {
int darc_bsc_init (int features);
int darc_bsc_st_encode (unsigned char *T, int n, int k, int features);
#ifdef USE_RUST
int darc_rs_bsc_st3_encode (unsigned char *data, int n);
#endif
}
int main(void){
  darc_bsc_init(0);
  size_t cap=1<<20, len=0; unsigned char*in=(unsigned char*)malloc(cap+64);
  for(;;){ if(len==cap){cap*=2; in=(unsigned char*)realloc(in,cap+64);} size_t r=fread(in+len,1,cap-len,stdin); if(!r)break; len+=r; }
  int n=(int)len; if(n<=1){free(in);return 0;}
  unsigned char*T=(unsigned char*)malloc(n+64); memcpy(T,in,n);
#ifdef USE_RUST
  int idx=darc_rs_bsc_st3_encode(T,n);
#else
  int idx=darc_bsc_st_encode(T,n,3,0);
#endif
  unsigned char h[4]={(unsigned char)(idx&0xff),(unsigned char)((idx>>8)&0xff),(unsigned char)((idx>>16)&0xff),(unsigned char)((idx>>24)&0xff)};
  fwrite(h,1,4,stdout); if(idx>=0) fwrite(T,1,n,stdout);
  free(T); free(in); return 0;
}
