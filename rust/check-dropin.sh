#!/usr/bin/env bash
# Assert that a built libdarc_codecs.a carries the drop-in aliases.
#
#   usage: check-dropin.sh <path/to/libdarc_codecs.a> [nm-command]
#
# ── Why this exists ─────────────────────────────────────────────────────────
#
# `dropin` is an OPT-IN feature (see rust/darc-codecs/Cargo.toml): the aliases
# it exports -- grzip_decompress, tor_decompress, mm_*, tta_* -- collide with
# the C implementations, and rust/difftest links both at once to compare them.
# So the archiver builds WITH the feature and the differential harness builds
# WITHOUT it.
#
# Both write the same file. `rust/` is one cargo workspace, so every build of
# darc-codecs -- including every difftest script that runs
# `cargo build --release -p darc-codecs` -- overwrites
# rust/target/release/libdarc_codecs.a with whatever feature set that command
# resolved. Run a difftest after ./compile-O2 and the archiver's staticlib is
# silently replaced by one missing six symbols.
#
# That failure does not surface at link time on macOS. The Haskell driver links
# with -undefined dynamic_lookup, so the missing symbols are deferred to the
# dynamic loader and the binary aborts at STARTUP:
#
#   dyld[…]: symbol not found in flat namespace '_grzip_decompress'
#
# which reads as a Haskell or FFI bug rather than a stale build artifact. It
# cost an afternoon once. Anything that links this archive should call this
# first -- it is one `nm` -- and anything that BUILDS it should call it after,
# to prove the feature actually took.
set -uo pipefail

LIB="${1:-}"
NM="${2:-nm}"
if [ -z "$LIB" ]; then
  echo "usage: $0 <libdarc_codecs.a> [nm-command]" >&2
  exit 2
fi
[ -f "$LIB" ] || { echo "check-dropin: no such archive: $LIB" >&2; exit 1; }

# Every #[cfg(feature = "dropin")] export in rust/darc-codecs/src/exports.rs.
# Kept as an explicit list rather than derived from the source: the point is to
# catch the archive drifting from the source, so deriving both from the same
# place would defeat it. If you add a drop-in alias, add it here too.
SYMBOLS="grzip_decompress mm_compress mm_decompress tor_decompress tta_compress tta_decompress"

command -v "$NM" >/dev/null 2>&1 || {
  # Cross builds may have no nm for the target arch. Refusing to build over a
  # missing tool would be worse than the bug; say so loudly instead of passing
  # silently, because a skipped check that prints nothing is indistinguishable
  # from a check that passed.
  echo "check-dropin: WARNING -- '$NM' not found, drop-in aliases NOT verified in $LIB" >&2
  exit 0
}

# Defined global symbols only. Rust's own mangled names contain these as
# substrings (…exports15tor_decompress…), so match the whole symbol, allowing
# the leading underscore Mach-O and mingw add.
defined="$("$NM" -g --defined-only "$LIB" 2>/dev/null \
           || "$NM" -g "$LIB" 2>/dev/null | grep -E ' [TtDdBbSs] ')"
[ -n "$defined" ] || { echo "error: $NM listed no symbols in $LIB" >&2; exit 1; }

# A here-string, NOT `echo | grep -q`: grep -q exits at the first match and the
# writer takes SIGPIPE, which under `set -o pipefail` makes the pipeline report
# failure. Every symbol then reads as missing -- the check fails loudest exactly
# when it is working.
missing=""
for s in $SYMBOLS; do
  grep -Eq "[[:space:]]_?${s}\$" <<< "$defined" || missing="$missing $s"
done

if [ -n "$missing" ]; then
  echo "error: $LIB is missing the drop-in aliases:$missing" >&2
  echo "       It was built WITHOUT --features darc-codecs/dropin -- most" >&2
  echo "       likely by a rust/difftest script, which shares this target" >&2
  echo "       directory and deliberately builds without the feature." >&2
  echo "       Rebuild:  (cd rust && cargo build --release -p darc-codecs \\" >&2
  echo "                    --features darc-codecs/dropin)" >&2
  exit 1
fi
