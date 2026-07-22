// Reference LZ4 blocks from the same library DArc links.
//
// DArc calls LZ4_compress_default / LZ4_compress_HC and decodes with
// LZ4_decompress_safe -- the raw BLOCK format, not the frame format. The
// version byte and int32 lengths around it are DArc's own framing and stay in
// C_LZ4.cpp; only what is inside a block has to interoperate.
#include <stdio.h>
#include <string.h>
#include "Compression/LZ4/lz4.h"
#include "Compression/LZ4/lz4.c"
#include "Compression/LZ4/lz4hc.h"
#include "Compression/LZ4/lz4hc.c"

static void emit(const char* name, const unsigned char* src, int n, int hc)
{
    char out[65536];
    int cap = LZ4_compressBound(n);
    if (cap > (int)sizeof out) { printf("%s ERROR too-big\n", name); return; }
    int m = hc ? LZ4_compress_HC((const char*)src, out, n, cap, hc)
               : LZ4_compress_default((const char*)src, out, n, cap);
    if (m <= 0) { printf("%s ERROR compress=%d\n", name, m); return; }
    printf("%s %d ", name, n);
    for (int i = 0; i < m; i++) printf("%02x", (unsigned char)out[i]);
    printf("\n");
}

int main()
{
    unsigned char text[4096], runs[4096], rnd[4096];
    const char* w[] = {"the ","quick ","brown ","fox ","jumps ","over ","lazy ","dog "};
    size_t p = 0; for (int i = 0; p < sizeof text; i++) {
        const char* s = w[i % 8]; size_t l = strlen(s);
        if (p + l > sizeof text) break; memcpy(text + p, s, l); p += l;
    }
    memset(text + p, ' ', sizeof text - p);
    memset(runs, 'A', sizeof runs);
    unsigned s = 12345; for (size_t i = 0; i < sizeof rnd; i++) { s = s*1103515245u+12345u; rnd[i] = (unsigned char)(s>>16); }

    emit("text",     text, sizeof text, 0);
    emit("runs",     runs, sizeof runs, 0);
    emit("random",   rnd,  sizeof rnd,  0);
    emit("text-hc",  text, sizeof text, 9);
    emit("tiny",     (const unsigned char*)"abcabcabcabc", 12, 0);
    emit("onebyte",  (const unsigned char*)"Z", 1, 0);
    return 0;
}
