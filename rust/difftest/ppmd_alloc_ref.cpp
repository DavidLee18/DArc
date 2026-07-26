/* Reference driver for differential-testing PPMd's memory suballocator.
 *
 *     ppmd_alloc_ref SEED HEAP_KB NOPS  >trace
 *
 * Built twice: plain drives the C, `-DUSE_RUST` the Rust port. Both run the
 * SAME pseudo-random operation sequence -- the seed picks the ops, so the two
 * sides are driven identically without needing a script file -- and print a
 * trace of every offset returned plus the four layout cursors and
 * GetUsedMemory() after each step.
 *
 * Comparing offsets rather than pointers is what makes this possible: the C's
 * heap is a malloc'ed block at an arbitrary address, the Rust one is a Vec<u8>.
 * The C already works in HeapStart-relative refs internally for exactly this
 * reason (64-bit portability), so nothing is being weakened by the choice.
 *
 * Why the allocator gets its own harness when the codec does not: the model
 * branches on GetUsedMemory() and on pText/UnitsStart crossings, so an
 * allocator that is merely correct still produces a different compressed
 * stream. Testing it alone means a mismatch points at the free lists rather
 * than at the model wrapped around them.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#ifdef USE_RUST
  long     darc_rs_ppmd_sa_start        (unsigned t);
  void     darc_rs_ppmd_sa_init         (void);
  void     darc_rs_ppmd_sa_stop         (void);
  unsigned darc_rs_ppmd_sa_used         (void);
  long     darc_rs_ppmd_sa_alloc_units  (unsigned nu);
  long     darc_rs_ppmd_sa_alloc_context(void);
  void     darc_rs_ppmd_sa_free_units   (long off, unsigned nu);
  long     darc_rs_ppmd_sa_expand_units (long off, unsigned nu);
  long     darc_rs_ppmd_sa_shrink_units (long off, unsigned nu, unsigned newnu);
  void     darc_rs_ppmd_sa_special_free (long off);
  long     darc_rs_ppmd_sa_ptext        (void);
  long     darc_rs_ppmd_sa_units_start  (void);
  long     darc_rs_ppmd_sa_lo_unit      (void);
  long     darc_rs_ppmd_sa_hi_unit      (void);
  #define SA_START        darc_rs_ppmd_sa_start
  #define SA_INIT         darc_rs_ppmd_sa_init
  #define SA_STOP         darc_rs_ppmd_sa_stop
  #define SA_USED         darc_rs_ppmd_sa_used
  #define SA_ALLOC_UNITS  darc_rs_ppmd_sa_alloc_units
  #define SA_ALLOC_CTX    darc_rs_ppmd_sa_alloc_context
  #define SA_FREE_UNITS   darc_rs_ppmd_sa_free_units
  #define SA_EXPAND       darc_rs_ppmd_sa_expand_units
  #define SA_SHRINK       darc_rs_ppmd_sa_shrink_units
  #define SA_SPECIAL_FREE darc_rs_ppmd_sa_special_free
  #define SA_PTEXT        darc_rs_ppmd_sa_ptext
  #define SA_UNITS_START  darc_rs_ppmd_sa_units_start
  #define SA_LO           darc_rs_ppmd_sa_lo_unit
  #define SA_HI           darc_rs_ppmd_sa_hi_unit
#else
  long     darc_ppmd_sa_start        (unsigned t);
  void     darc_ppmd_sa_init         (void);
  void     darc_ppmd_sa_stop         (void);
  unsigned darc_ppmd_sa_used         (void);
  long     darc_ppmd_sa_alloc_units  (unsigned nu);
  long     darc_ppmd_sa_alloc_context(void);
  void     darc_ppmd_sa_free_units   (long off, unsigned nu);
  long     darc_ppmd_sa_expand_units (long off, unsigned nu);
  long     darc_ppmd_sa_shrink_units (long off, unsigned nu, unsigned newnu);
  void     darc_ppmd_sa_special_free (long off);
  long     darc_ppmd_sa_ptext        (void);
  long     darc_ppmd_sa_units_start  (void);
  long     darc_ppmd_sa_lo_unit      (void);
  long     darc_ppmd_sa_hi_unit      (void);
  #define SA_START        darc_ppmd_sa_start
  #define SA_INIT         darc_ppmd_sa_init
  #define SA_STOP         darc_ppmd_sa_stop
  #define SA_USED         darc_ppmd_sa_used
  #define SA_ALLOC_UNITS  darc_ppmd_sa_alloc_units
  #define SA_ALLOC_CTX    darc_ppmd_sa_alloc_context
  #define SA_FREE_UNITS   darc_ppmd_sa_free_units
  #define SA_EXPAND       darc_ppmd_sa_expand_units
  #define SA_SHRINK       darc_ppmd_sa_shrink_units
  #define SA_SPECIAL_FREE darc_ppmd_sa_special_free
  #define SA_PTEXT        darc_ppmd_sa_ptext
  #define SA_UNITS_START  darc_ppmd_sa_units_start
  #define SA_LO           darc_ppmd_sa_lo_unit
  #define SA_HI           darc_ppmd_sa_hi_unit
#endif
}

/* Live allocations, so frees and resizes target something real rather than a
 * random offset -- a random free would corrupt both sides identically and
 * prove nothing. */
struct Live { long off; unsigned nu; };

int main (int argc, char **argv)
{
  if (argc < 4) { fprintf (stderr, "usage: %s SEED HEAP_KB NOPS\n", argv[0]); return 2; }
  unsigned seed  = (unsigned) atoi (argv[1]);
  unsigned heap  = (unsigned) atoi (argv[2]) * 1024;
  int      nops  = atoi (argv[3]);

  if (!SA_START (heap)) { printf ("start failed\n"); return 1; }
  SA_INIT ();

  Live *live = (Live *) calloc (65536, sizeof (Live));
  int nlive = 0;

  unsigned s = seed ? seed : 1;
  #define NEXT() (s = s * 1103515245u + 12345u, (s >> 16) & 0x7fff)

  printf ("init ptext=%ld units=%ld lo=%ld hi=%ld used=%u\n",
          SA_PTEXT (), SA_UNITS_START (), SA_LO (), SA_HI (), SA_USED ());

  for (int i = 0; i < nops; i++) {
    unsigned op = NEXT () % 100;
    if (op < 40 || nlive == 0) {
      /* allocate: sizes skewed small, as the model's really are, but reaching
         past 128 units so the multi-block path in GlueFreeBlocks is used */
      /* Two NEXT() calls must be SEQUENCED. Folded into one expression they
       * are unsequenced modifications of `s`, which is undefined behaviour --
       * and worse here than usual, because the two builds could then evaluate
       * them in different orders and drive the allocators with different
       * operation sequences, producing a diff that means nothing. */
      unsigned big = NEXT () % 8;
      unsigned span = (big == 0) ? 128 : 8;
      unsigned nu = 1 + NEXT () % span;
      long off = SA_ALLOC_UNITS (nu);
      printf ("alloc %u -> %ld\n", nu, off);
      if (off >= 0 && nlive < 65536) { live[nlive].off = off; live[nlive].nu = nu; nlive++; }
    } else if (op < 55) {
      long off = SA_ALLOC_CTX ();
      printf ("ctx -> %ld\n", off);
    } else if (op < 78) {
      int k = NEXT () % nlive;
      printf ("free %ld %u\n", live[k].off, live[k].nu);
      SA_FREE_UNITS (live[k].off, live[k].nu);
      live[k] = live[--nlive];
    } else if (op < 88) {
      int k = NEXT () % nlive;
      long off = SA_EXPAND (live[k].off, live[k].nu);
      printf ("expand %ld %u -> %ld\n", live[k].off, live[k].nu, off);
      if (off >= 0) { live[k].off = off; live[k].nu += 1; }
      else          { live[k] = live[--nlive]; }
    } else if (op < 96) {
      int k = NEXT () % nlive;
      if (live[k].nu > 1) {
        unsigned newnu = 1 + NEXT () % (live[k].nu - 1);
        long off = SA_SHRINK (live[k].off, live[k].nu, newnu);
        printf ("shrink %ld %u %u -> %ld\n", live[k].off, live[k].nu, newnu, off);
        if (off >= 0) { live[k].off = off; live[k].nu = newnu; }
        else          { live[k] = live[--nlive]; }
      }
    } else {
      int k = NEXT () % nlive;
      if (live[k].nu == 1) {
        printf ("specialfree %ld\n", live[k].off);
        SA_SPECIAL_FREE (live[k].off);
        live[k] = live[--nlive];
      }
    }
    /* The cursors and used-memory figure after EVERY op: these are what the
       model actually branches on, so a divergence here is a divergence in the
       compressed stream even if every returned offset matched. */
    printf ("  ptext=%ld units=%ld lo=%ld hi=%ld used=%u live=%d\n",
            SA_PTEXT (), SA_UNITS_START (), SA_LO (), SA_HI (), SA_USED (), nlive);
  }

  SA_STOP ();
  free (live);
  return 0;
}
