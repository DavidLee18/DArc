// Reference zstd frames from the VENDORED libzstd (1.5.6) that the archiver
// currently links, using the parameters C_Zstd.cpp sets.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "Compression/Zstd/libzstd/zstd.h"

static void emit(const char* name, const unsigned char* src, size_t n, int level, int windowLog)
{
    ZSTD_CCtx* c = ZSTD_createCCtx();
    ZSTD_CCtx_setParameter(c, ZSTD_c_compressionLevel, level);
    if (windowLog > 0) {
        ZSTD_CCtx_setParameter(c, ZSTD_c_windowLog, windowLog);
        ZSTD_CCtx_setParameter(c, ZSTD_c_enableLongDistanceMatching, 1);
    }
    size_t cap = ZSTD_compressBound(n);
    char* out = (char*)malloc(cap);
    size_t m = ZSTD_compress2(c, out, cap, src, n);
    if (ZSTD_isError(m)) { printf("%s ERROR %s\n", name, ZSTD_getErrorName(m)); }
    else {
        printf("%s %zu ", name, n);
        for (size_t i = 0; i < m; i++) printf("%02x", (unsigned char)out[i]);
        printf("\n");
    }
    free(out); ZSTD_freeCCtx(c);
}

int main()
{
    printf("# vendored libzstd %s\n", ZSTD_versionString());
    unsigned char text[4096], runs[2048], rnd[2048];
    const char* w[] = {"the ","quick ","brown ","fox ","jumps ","over ","lazy ","dog "};
    size_t p = 0; for (int i = 0; p < sizeof text; i++) {
        const char* s = w[i % 8]; size_t l = strlen(s);
        if (p + l > sizeof text) break; memcpy(text + p, s, l); p += l;
    }
    memset(text + p, ' ', sizeof text - p);
    memset(runs, 'A', sizeof runs);
    unsigned s = 12345; for (size_t i = 0; i < sizeof rnd; i++) { s = s*1103515245u+12345u; rnd[i] = (unsigned char)(s>>16); }

    emit("text-l3",   text, sizeof text, 3, 0);
    emit("text-l19",  text, sizeof text, 19, 0);
    emit("text-ldm",  text, sizeof text, 3, 20);
    emit("runs",      runs, sizeof runs, 3, 0);
    emit("random",    rnd,  sizeof rnd,  3, 0);
    emit("empty",     (const unsigned char*)"", 0, 3, 0);
    return 0;
}
