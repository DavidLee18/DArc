/* Dump phase1's output from the C Dict encoder so a ported phase1 can be
 * compared on its own.
 *
 * DictEncode runs all seven phases, so a port of it has no validation until
 * every phase exists -- ~600 lines written blind and debugged at once. phase1's
 * result is just the word list and the byte-frequency table, and both can be
 * dumped and diffed, so each phase can be landed against evidence instead.
 *
 *   dict_phase1_ref <in     "W <at> <len> <hash> <hash0>" per word, then
 *                           "C <byte> <count>" for non-zero counts
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Pull in the codec source itself: Word, phase1 and the globals are file-scope
// there, and this is the only way to see the struct layout.
#define DICT_LIBRARY
#include "../../Compression/Dict/C_Dict.cpp"

int main (void)
{
  size_t cap = 1<<20, len = 0;
  unsigned char *in = (unsigned char*) malloc (cap);
  if (!in) return 3;
  for (;;) {
    if (len == cap) { cap *= 2; unsigned char *g = (unsigned char*) realloc (in, cap); if (!g) return 3; in = g; }
    size_t n = fread (in + len, 1, cap - len, stdin);
    if (n == 0) break;
    len += n;
  }

  if (phase1 (in, (unsigned) len) != 0) { fprintf (stderr, "phase1 failed\n"); return 4; }

  printf ("words %ld\n", (long)(NextWord - FirstWord));
  for (Word *p = FirstWord; p < NextWord; p++)
    printf ("W %ld %u %u %u\n", (long)(p->ptr - in), p->len, p->hash, p->hash0);
  for (int c = 0; c <= UCHAR_MAX; c++)
    if (char_counts[c]) printf ("C %d %d\n", c, char_counts[c]);
  return 0;
}
