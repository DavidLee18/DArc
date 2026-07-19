/* Clock support for the MicroHs runtime on the mingw-w64 cross build.
 *
 * MicroHs defines CLOCK_INIT / CLOCK_T / CLOCK_GET / CLOCK_SLEEP only in
 * runtime/unix/extra.c. Neither runtime/mingw/extra.c nor
 * runtime/windows/extra.c provides them, so eval.c compiles with the
 * "#else CLOCK_INIT" branch and every threadDelay dies at runtime with
 *
 *     ERR: threadDelay: no clock
 *
 * DArc's whole pipeline is CSP-style concurrent threads (see Process.hs), so
 * without a clock the Windows binary builds and prints its help but cannot
 * perform a single archive operation. That is exactly what the first working
 * cross-build did.
 *
 * This header is force-included (-include) ahead of the runtime sources by
 * compile-mhs-win64, together with -DCLOCK_GET=... etc, so the runtime picks
 * these up instead of taking the no-clock branch. It deliberately mirrors
 * runtime/unix/extra.c rather than inventing anything.
 *
 * Note it uses <sys/time.h> and <unistd.h>, which mingw-w64 does provide,
 * rather than <windows.h>. Pulling windows.h into eval.c's translation unit is
 * what produced the FARPROC typedef collision that stalled this build before.
 *
 * Upstream fix would be to add these to runtime/mingw/extra.c; observed on
 * MicroHs 0.15.4.0, the commit CI pins.
 */
#ifndef DARC_MHS_WIN_COMPAT_H
#define DARC_MHS_WIN_COMPAT_H

/* Pull in every header that declares what is patched below, BEFORE patching it.
 * This file is force-included ahead of the runtime sources, so anything these
 * headers declare must be seen while the names still mean what the headers
 * expect. eval.c's own later #includes then hit the include guards and are
 * no-ops, which is what makes the mkdir rename below safe. */
#include <stdint.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <direct.h>
#include <dirent.h>

/* ---- clock ------------------------------------------------------------- */

/* Nothing to set up: gettimeofday needs no initialization, same as Unix. */
static inline void darc_mhs_clock_init(void) { }

/* Current time in microseconds. */
static inline int64_t darc_mhs_clock_get(void) {
  struct timeval tv;
  (void)gettimeofday(&tv, 0);
  return (int64_t)tv.tv_sec * 1000000 + (int64_t)tv.tv_usec;
}

/* ---- mkdir ------------------------------------------------------------- */

/* Under WANT_DIR the runtime calls POSIX two-argument mkdir(path, mode), but
 * mingw's mkdir takes only a path, so eval.c fails with "too many arguments to
 * function 'mkdir'". Exactly the same mismatch DArc hit in its own create_dir
 * (see Compression/Common.h).
 *
 * Renaming the identifier is safe only because the declaring headers are
 * included above: by the time this macro exists, nothing further will try to
 * *declare* mkdir, only call it. */
static inline int darc_mhs_mkdir(const char *path, int mode) {
  (void)mode;                   /* Windows has no POSIX mode bits to apply */
  return _mkdir(path);
}
#define mkdir darc_mhs_mkdir

#endif /* DARC_MHS_WIN_COMPAT_H */
