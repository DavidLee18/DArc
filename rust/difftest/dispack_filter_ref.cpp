/* Reference driver for the DisPack FORWARD filter.
 *
 *     dispack_filter_ref c  <origin> <in >out    C DisFilter
 *     dispack_filter_ref rs <origin> <in >out    the Rust port
 *     dispack_filter_ref dc  <origin> <in           C detect()   -> prints EXETYPE
 *     dispack_filter_ref drs <origin> <in           Rust detect() -> prints EXETYPE
 *
 * The existing dispack_ref.cpp drives the archiver's chunked compress/decompress
 * wrapper. This one exposes the raw block transform instead, because that is
 * the piece being ported and the piece that must be byte-exact: DisPack is one
 * of DArc's own formats, so "format-valid" does not apply.
 *
 * `origin` matters and is a parameter rather than a constant: DisFilter turns
 * relative call/jump targets into absolute ones, so the same bytes at a
 * different load address legitimately produce a different filtered stream.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Compression/Compression.h"

typedef unsigned char sU8;
typedef int sInt;
typedef unsigned int sU32;

// Defined in DisPack.cpp, which C_DisPack.cpp includes textually.
sU8 *DisFilter (sU8 *src, sInt size, sU32 origin, sU32 &outputSize);

extern "C" int darc_rs_dispack_filter (const unsigned char *src, int srcSize,
                                       unsigned origin, unsigned char *dst, int dstCap);
extern "C" int darc_rs_dispack_detect (const unsigned char *buf, int len);

// detect() gates the whole filter, so its classification is compared directly:
// EXETYPE_EXE=2, EXETYPE_DATA=1.
enum EXETYPE {EXETYPE_UNKNOWN, EXETYPE_DATA, EXETYPE_EXE};
EXETYPE detect (unsigned char *buf, int len);

static int read_all (unsigned char **buf)
{
  size_t cap = 1 << 20, len = 0;
  unsigned char *p = (unsigned char *) malloc (cap);
  for (;;) {
    if (len == cap) { cap *= 2; p = (unsigned char *) realloc (p, cap); }
    size_t n = fread (p + len, 1, cap - len, stdin);
    if (n == 0) break;
    len += n;
  }
  *buf = p;
  return (int) len;
}

int main (int argc, char **argv)
{
  if (argc < 3) { fprintf (stderr, "usage: %s c|rs <origin> <in >out\n", argv[0]); return 2; }
  unsigned origin = (unsigned) strtoul (argv[2], NULL, 0);

  unsigned char *in = NULL;
  int inSize = read_all (&in);

  if (!strcmp (argv[1], "c")) {
    sU32 outSize = 0;
    sU8 *out = DisFilter (in, inSize, origin, outSize);
    if (out == NULL) { fprintf (stderr, "C DisFilter returned NULL\n"); return 1; }
    fwrite (out, 1, outSize, stdout);
    free (out);
    return 0;
  }
  if (!strcmp (argv[1], "rs")) {
    // Worst case: every byte escapes to two, plus the ST_MAX header words.
    int cap = inSize * 2 + 4096;
    unsigned char *out = (unsigned char *) malloc (cap > 0 ? cap : 1);
    int n = darc_rs_dispack_filter (in, inSize, origin, out, cap);
    if (n < 0) { fprintf (stderr, "Rust dis_filter returned %d\n", n); return 1; }
    fwrite (out, 1, n, stdout);
    return 0;
  }
  if (!strcmp (argv[1], "dc") || !strcmp (argv[1], "drs")) {
    int v = !strcmp (argv[1], "dc") ? (int) detect (in, inSize)
                                    : darc_rs_dispack_detect (in, inSize);
    printf ("%d\n", v);
    return 0;
  }
  fprintf (stderr, "bad mode %s\n", argv[1]);
  return 2;
}
