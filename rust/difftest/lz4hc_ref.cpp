// Differential harness for the LZ4-HC encoder port.
//
// LZ4-HC is ENCODER-ONLY: it emits ordinary LZ4 blocks that the existing
// decoder already reads. So unlike every other harness here, this one does not
// require byte-identical output -- encoders legitimately choose different
// matches, and the standing rule allows a format-valid encoder for standard
// formats. What must hold instead is:
//
//   1. every block the Rust encoder produces decodes, through the *C* decoder,
//      back to the original bytes -- checked against the C library rather than
//      lz4_flex so the two Rust implementations cannot agree on a shared
//      misreading of the format; and
//   2. the compressed size stays close to the C's, since ratio is the entire
//      reason LZ4-HC exists.
//
// Modes:
//   c   <level> < in > out   compress with the C  LZ4_compress_HC
//   rs  <level> < in > out   compress with the Rust port
//   d   <origSize> < in > out   decompress with the C LZ4_decompress_safe
//
// Both encoders are given a buffer sized by LZ4_compressBound, exactly as
// C_LZ4.cpp does.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "../../Compression/LZ4/lz4.c"
#include "../../Compression/LZ4/lz4hc.c"
}

extern "C" int darc_rs_lz4_compress_hc_block(const unsigned char *src, int srcSize,
                                             unsigned char *dst, int dstCap, int level);

static int read_all(unsigned char **buf)
{
    size_t cap = 1 << 20, len = 0;
    unsigned char *p = (unsigned char *) malloc(cap);
    for (;;) {
        if (len == cap) { cap *= 2; p = (unsigned char *) realloc(p, cap); }
        size_t n = fread(p + len, 1, cap - len, stdin);
        if (n == 0) break;
        len += n;
    }
    *buf = p;
    return (int) len;
}

int main(int argc, char **argv)
{
    if (argc < 3) { fprintf(stderr, "usage: %s c|rs|d <level|origSize>\n", argv[0]); return 2; }
    const char *mode = argv[1];
    int arg = atoi(argv[2]);

    unsigned char *in = NULL;
    int inSize = read_all(&in);

    if (!strcmp(mode, "d")) {
        unsigned char *out = (unsigned char *) malloc(arg > 0 ? arg : 1);
        int n = LZ4_decompress_safe((const char *) in, (char *) out, inSize, arg);
        if (n < 0) { fprintf(stderr, "C decode failed: %d\n", n); return 1; }
        fwrite(out, 1, n, stdout);
        return 0;
    }

    int cap = LZ4_compressBound(inSize);
    unsigned char *out = (unsigned char *) malloc(cap > 0 ? cap : 1);
    int n;
    if (!strcmp(mode, "c"))
        n = LZ4_compress_HC((const char *) in, (char *) out, inSize, cap, arg);
    else if (!strcmp(mode, "rs"))
        n = darc_rs_lz4_compress_hc_block(in, inSize, out, cap, arg);
    else { fprintf(stderr, "bad mode %s\n", mode); return 2; }

    if (n <= 0) { fprintf(stderr, "compress returned %d\n", n); return 1; }
    fwrite(out, 1, n, stdout);
    return 0;
}
