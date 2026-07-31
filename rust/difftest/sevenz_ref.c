/* Driver for BOTH .7z readers -- the vendored C SDK and the darc-sevenz crate.
 *
 * One source, two binaries. Compiled against Compression/7z from the pinned C
 * reference it is the old reader; compiled with -DUSE_RUST and linked against
 * libdarc_sevenz.a it is the new one. Since the three entry points are the
 * entire ABI (Compression.h:374-376), nothing else has to differ, and the
 * comparison cannot accidentally be driven through different code paths.
 *
 * Usage:  sevenz_ref l|t|x <archive> [outdir]
 *
 * The SRes goes to stderr as `rc=N` so the harness can compare return codes
 * separately from the listing on stdout.
 */
#include <stdio.h>
#include <string.h>

int darc_7z_list(const char *path);
int darc_7z_extract(const char *path, const char *out_dir);
int darc_7z_test(const char *path);

int main(int argc, char **argv) {
  if (argc < 3) {
    fprintf(stderr, "usage: %s l|t|x <archive> [outdir]\n", argv[0]);
    return 64;
  }
  const char *op  = argv[1];
  const char *arc = argv[2];
  int rc;

  if (strcmp(op, "l") == 0) {
    rc = darc_7z_list(arc);
  } else if (strcmp(op, "t") == 0) {
    rc = darc_7z_test(arc);
  } else if (strcmp(op, "x") == 0) {
    if (argc < 4) { fprintf(stderr, "x needs an outdir\n"); return 64; }
    rc = darc_7z_extract(arc, argv[3]);
  } else {
    fprintf(stderr, "unknown op '%s'\n", op);
    return 64;
  }

  fflush(stdout);
  fprintf(stderr, "rc=%d\n", rc);
  return rc == 0 ? 0 : 1;
}
