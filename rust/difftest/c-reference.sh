# Materialise the C codec sources at a pinned revision, for the differential
# harnesses to compile as their reference. Sourced, not executed.
#
# ── Why ──────────────────────────────────────────────────────────────────────
#
# Every <codec>-check.sh proves the Rust port matches the C by compiling BOTH
# and requiring identical output. As the port deletes the C it replaces, that
# reference disappears from the working tree — and with it the only thing that
# can demonstrate a replacement is correct. So the C is taken from git history
# instead of from the checkout.
#
# The reference is ALWAYS the pinned revision, even while the C is still present
# in the tree. Two reasons:
#
#   * A fallback that only engages once the C is deleted would sit untested
#     until the moment it becomes load-bearing. That is exactly how the MicroHs
#     cache guard shipped broken — the run that introduced it populated the
#     cache rather than restoring it, so it never took the path it broke.
#   * A fixed oracle cannot drift. Comparing against whatever C happens to be in
#     the tree lets a concurrent C change mask a Rust regression.
#
# ── How ──────────────────────────────────────────────────────────────────────
#
# `git archive` extracts Compression/ at the pinned SHA, then the CURRENT
# difftest shims are copied in beside it. That combination matters: the shims
# `#include "../../Compression/..."` by relative path, so placing them inside
# the extracted tree makes those includes resolve to the pinned C with no source
# edits at all, while leaving the harness logic itself free to evolve.
#
# Bumping the pin is a deliberate act: it changes what "correct" means for every
# harness. Do it only to pick up a genuine C-side fix, and say so in the commit.

# Last revision containing the full C codec set (zstd's libzstd was removed in
# this commit itself, and has no harness).
#
# The FULL 40-character hash, not an abbreviation: `git fetch origin <sha>`
# rejects a short SHA outright ("couldn't find remote ref"), which is how the
# shallow-clone fetch below is able to work at all. Abbreviations can also grow
# ambiguous as history does.
DARC_C_REF_SHA="5c2c6ce1244db759a17aea61cb243f3ace41fe61"

# Usage: darc_c_reference <repo-root>   → echoes the reference tree's path
darc_c_reference() {
  local root="$1"
  local sha="$DARC_C_REF_SHA"
  local cref="${TMPDIR:-/tmp}/darc-c-ref-$sha"

  # Rebuild the shim copy every time (cheap, and the shims are live source);
  # extract the pinned C only once.
  if [ ! -d "$cref/Compression" ]; then
    rm -rf "$cref"; mkdir -p "$cref"
    # CI checks out shallow (actions/checkout defaults to fetch-depth: 1), so
    # the pinned commit is usually absent. Fetch just that one commit rather
    # than making every job clone full history.
    if ! git -C "$root" rev-parse --verify --quiet "$sha^{commit}" >/dev/null; then
      git -C "$root" fetch --depth=1 --quiet origin "$sha" 2>/dev/null || true
    fi
    git -C "$root" rev-parse --verify --quiet "$sha^{commit}" >/dev/null || {
      echo "c-reference: pinned revision $sha is not available and could not be" >&2
      echo "fetched. In CI, give the checkout enough history (fetch-depth: 0)." >&2
      return 1; }
    git -C "$root" archive "$sha" Compression | tar -x -C "$cref" || {
      echo "c-reference: could not extract Compression/ at $sha" >&2
      return 1; }
  fi

  mkdir -p "$cref/rust/difftest"
  cp "$root"/rust/difftest/*.cpp "$cref/rust/difftest/" 2>/dev/null

  echo "$cref"
}
