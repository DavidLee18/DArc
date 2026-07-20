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

  // Which phase to stop after; default 1.
  int upto = 1;
  if (getenv("DICT_PHASE")) upto = atoi(getenv("DICT_PHASE"));

  if (phase1 (in, (unsigned) len) != 0) { fprintf (stderr, "phase1 failed\n"); return 4; }
  if (upto == 1) {
    printf ("words %ld\n", (long)(NextWord - FirstWord));
    for (Word *p = FirstWord; p < NextWord; p++)
      printf ("W %ld %u %u %u\n", (long)(p->ptr - in), p->len, p->hash, p->hash0);
    for (int c = 0; c <= UCHAR_MAX; c++)
      if (char_counts[c]) printf ("C %d %d\n", c, char_counts[c]);
    return 0;
  }

  LastWord = NextWord;
  int rc2 = phase2 ((unsigned) len, 200, 200, 200, 0);
  if (rc2) { printf ("phase2 rejected\n"); return 0; }
  if (upto == 2) {
    printf ("words %ld\n", (long)(LastWord - FirstWord));
    for (Word *p = FirstWord; p < LastWord; p++)
      printf ("W %ld %u %d\n", (long)(p->ptr - in), p->len, p->count);
    return 0;
  }

  int nodes = 0;
  int rc3 = phase3 (0, &nodes);
  if (rc3) { printf ("phase3 rejected\n"); return 0; }
  if (upto == 3) {
    printf ("nodes %d prefix %d\n", nodes, PREFIX_FOR_WEAK_CHARS);
    printf ("words %ld\n", (long)(LastWord - FirstWord));
    for (Word *p = FirstWord; p < LastWord; p++)
      printf ("W %ld %u %d\n", (long)(p->ptr - in), p->len, p->count);
    for (int c = 0; c <= UCHAR_MAX; c++)
      if (char_counts[c]) printf ("C %d %d\n", c, char_counts[c]);
    return 0;
  }

  int rc4 = phase4 (nodes);
  if (rc4) { printf ("phase4 rejected\n"); return 0; }
  if (upto == 4) {
    printf ("words %ld\n", (long)(LastWord - FirstWord));
    for (Word *p = FirstWord; p < LastWord; p++)
      printf ("W %ld %u %d %d %d\n", (long)(p->ptr - in), p->len, p->count, p->chr, p->chr2);
    return 0;
  }

  byte *outbuf = NULL; unsigned dictlen = 0;
  int rc5 = phase5 (&outbuf, &dictlen, (unsigned) len);
  if (rc5) { printf ("phase5 rejected\n"); return 0; }
  if (upto == 5) { fwrite (outbuf, 1, dictlen, stdout); return 0; }

  if (phase6()) { printf ("phase6 rejected\n"); return 0; }
  unsigned datalen = 0;
  if (phase7 (in, (unsigned) len, outbuf + dictlen, &datalen)) { printf ("phase7 rejected\n"); return 0; }
  // Dictionary followed by the encoded text -- what DictEncode returns.
  fwrite (outbuf, 1, dictlen + datalen, stdout);
  return 0;
}
