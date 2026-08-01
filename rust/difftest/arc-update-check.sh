#!/usr/bin/env bash
# Differential-test the Rust `arc u` and `arc f` against the Haskell ones,
# BYTE FOR BYTE.
#
#   usage: arc-update-check.sh [reference-arc]
#
# `u` and `f` merge what is in an archive with what is on disk (joinLists,
# ArhiveFileList.hs:145), and the merge decides both WHICH files survive and in
# what ORDER. The differences between the modes are small and entirely
# archive-visible:
#
#   u  take whichever copy is newer; add files the archive did not have
#   f  take whichever copy is newer; add NOTHING new
#
# and both keep the ARCHIVED copy when the timestamps are equal, so an unchanged
# file is not repacked into a different position.
#
# The cases below are built so each of those rules is the only thing separating
# two runs: a file newer on disk, a file newer in the archive, a file with equal
# timestamps, a file only on disk, and a file only in the archive.
#
# By default the kept files are RECOMPRESSED rather than copied --
# splitToSolidBlocks preserves existing solid blocks only under --keep-original
# (ArhiveFileList.hs:297) -- so the output is packed exactly as a fresh archive
# would be, and any difference is the merge's.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

[ -x "$REF" ] || {
  echo "no reference binary at $REF -- build one with ./compile-ghc-probe" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-update-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

# build_tree <dir> -- a tree with files of three different ages plus a
# subdirectory, so the timestamp rules and the directory handling both matter.
build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub"
  printf 'older on disk\n'     > "$d/older.txt"
  printf 'newer on disk\n'     > "$d/newer.txt"
  printf 'same on both\n'      > "$d/same.txt"
  printf 'only on disk\n'      > "$d/onlydisk.txt"
  printf 'in a subdirectory\n' > "$d/sub/nested.txt"
  # Fixed mtimes so the two runs see identical inputs.
  touch -t 202001010000 "$d/older.txt"
  touch -t 203001010000 "$d/newer.txt"
  touch -t 202501010000 "$d/same.txt" "$d/onlydisk.txt" "$d/sub/nested.txt"
}

for m in -m0 -m1 -m4 -m9; do
  for cmd in u f; do
    for extra in "" "-r"; do
      checked=$((checked + 1))
      build_tree "$W/src"

      # A base archive holding three of the five files, so the update has
      # something to keep, something to replace, and something to add.
      rm -f "$W/ref.arc" "$W/port.arc"
      ( cd "$W/src" && "$REF" a --nodates -y "$m" "$W/ref.arc" \
          older.txt newer.txt same.txt ) >/dev/null 2>&1
      cp "$W/ref.arc" "$W/port.arc"

      ( cd "$W/src" && "$REF"  "$cmd" --nodates $extra -y "$m" "$W/ref.arc"  . ) >/dev/null 2>&1
      ( cd "$W/src" && "$PORT" "$cmd" --nodates $extra -y "$m" "$W/port.arc" . ) >/dev/null 2>&1

      if [ ! -f "$W/port.arc" ]; then
        echo "  DIFF [$m $cmd $extra]: the port wrote no archive"
        fail=$((fail + 1))
        continue
      fi
      if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
        echo "  DIFF [$m $cmd $extra]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
        fail=$((fail + 1))
      fi
    done
  done
done

echo "arc u/f: $checked archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every row passes if both sides simply leave the base archive alone. Require
# `u -r` to have actually CHANGED it, and `f` to differ from `u` -- if the two
# modes produce the same archive, nothing here distinguishes them.
build_tree "$W/src"
rm -f "$W/base.arc" "$W/u.arc" "$W/f.arc"
( cd "$W/src" && "$REF" a --nodates -y -m1 "$W/base.arc" older.txt newer.txt same.txt ) >/dev/null 2>&1
cp "$W/base.arc" "$W/u.arc"; cp "$W/base.arc" "$W/f.arc"
( cd "$W/src" && "$PORT" u --nodates -r -y -m1 "$W/u.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" f --nodates -r -y -m1 "$W/f.arc" . ) >/dev/null 2>&1
if cmp -s "$W/base.arc" "$W/u.arc"; then
  echo "SELF-TEST FAILED: `u -r` did not change the archive, so the rows above" >&2
  echo "compared two untouched copies" >&2
  exit 1
fi
if cmp -s "$W/u.arc" "$W/f.arc"; then
  echo "SELF-TEST FAILED: u and f produced the same archive, so nothing here" >&2
  echo "distinguishes the two modes" >&2
  exit 1
fi

echo "the Rust arc u/f merges exactly as the Haskell one does"
