#!/usr/bin/env bash
# Build the Haskell reference from `9a127e6` into `Tests/arc-ghc`.
#
# The source of last resort for [haskell-reference.sh], and the only one that
# needs no hosted artifact. It is also the most trustworthy: it builds the
# oracle from the repository's own history rather than fetching a binary, so
# there is nothing to pin and nothing to take on faith.
#
# The cost is the toolchain. `compile-ghc-probe` needs GHC 9.10.3, and the day
# that stops installing this stops working — which is the whole reason the
# fetch path exists alongside it.
#
# Two things that will bite:
#
#   * `compile-ghc-probe` writes objects into the SHARED `/tmp/out/`, so a
#     build here poisons a subsequent `./compile-c` of the current tree. It is
#     removed afterwards, as `docs/testing.md` instructs.
#   * The worktree is left in place. It is 30 s to build and reused by every
#     later run, and `git worktree remove` on a tree someone is using is a
#     worse failure than a stale directory.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEST="$ROOT/Tests/arc-ghc"
SHA="9a127e6"
WT="${DARC_REF_WORKTREE:-/tmp/darc-ref}"

if [ -x "$DEST" ]; then
  echo "reference: already at $DEST"
  exit 0
fi

command -v ghc >/dev/null 2>&1 || {
  cat >&2 <<MSG
No GHC on PATH, so the reference cannot be built and none was found or fetched.

The harnesses that need it will exit 2. `arc-golden-check.sh` does not need it
and still runs -- that is what it is for.

To provide one:
  git worktree add $WT $SHA
  (cd $WT && ./compile-ghc-probe)     # needs GHC 9.10.3 -> Tests/arc-ghc
MSG
  exit 2
}

if [ ! -d "$WT" ]; then
  echo "reference: adding worktree $WT at $SHA"
  git -C "$ROOT" worktree add "$WT" "$SHA" >/dev/null 2>&1 || {
    echo "could not create a worktree at $WT" >&2; exit 2; }
fi

echo "reference: building from $SHA (this takes a minute)"
( cd "$WT" && ./compile-ghc-probe ) >/dev/null 2>&1 || {
  echo "compile-ghc-probe failed in $WT" >&2; exit 2; }
[ -x "$WT/Tests/arc-ghc" ] || { echo "no arc-ghc after building in $WT" >&2; exit 2; }

mkdir -p "$(dirname "$DEST")"
cp "$WT/Tests/arc-ghc" "$DEST"
# The shared object directory, or the next ./compile-c picks up these objects.
rm -rf /tmp/out
echo "reference: built ($(shasum -a 256 "$DEST" 2>/dev/null | cut -c1-12))"
