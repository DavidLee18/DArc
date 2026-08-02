#!/usr/bin/env bash
# Differential-test the Rust `arc u`, `arc f` and `arc d` against the Haskell
# ones, BYTE FOR BYTE.
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
#   d  no disk files at all -- keep everything the filespecs do NOT match
#      (runDelete = runArchiveAdd . setArcFilter ((not.) . fullFileFilter))
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

# saw <pattern> <command...> -- true when the command's output contains it.
#
# NOT `cmd | grep -q pattern`. Under `set -o pipefail` that construction reports
# FAILURE on a successful match: grep -q exits the moment it matches, cmd gets
# SIGPIPE writing the rest, and pipefail takes the pipeline's status from the
# killed writer. It only shows up when the output is long enough that cmd is
# still writing -- so it passes on small cases and flakes on real ones, which is
# how it was found here (a listing that plainly contained the name was reported
# as not containing it). Capture first, match after.
saw () {
  local pattern="$1"; shift
  local text; text="$("$@" 2>&1)"
  grep -q -- "$pattern" <<< "$text"
}

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

# The same merge reached through OPTIONS rather than commands: `a -u`, `a -f`
# and `a --sync`. The option spellings are not aliases handled at the call site
# -- Cmdline.hs picks the mode with the command first and then the options in
# the order freshen, update, sync -- so they need their own rows.
#
# --sync is the one that DELETES: a file in the archive that the filespecs did
# not reach is dropped, which no other mode does. That is also why it gets a row
# where the disk tree is missing a file the archive has.
for m in -m0 -m1 -m4 -m9; do
  for opt in -u -f --sync; do
    checked=$((checked + 1))
    build_tree "$W/src"
    rm -f "$W/ref.arc" "$W/port.arc"
    # The base archive holds a file that is NOT on disk (gone.txt), so --sync
    # has something to remove and -u/-f have something to keep.
    printf 'only in the archive\n' > "$W/src/gone.txt"
    touch -t 202501010000 "$W/src/gone.txt"
    ( cd "$W/src" && "$REF" a --nodates -y "$m" "$W/ref.arc" \
        older.txt newer.txt same.txt gone.txt ) >/dev/null 2>&1
    rm -f "$W/src/gone.txt"
    cp "$W/ref.arc" "$W/port.arc"

    ( cd "$W/src" && "$REF"  a --nodates -r $opt -y "$m" "$W/ref.arc"  . ) >/dev/null 2>&1
    ( cd "$W/src" && "$PORT" a --nodates -r $opt -y "$m" "$W/port.arc" . ) >/dev/null 2>&1

    r=present; [ -f "$W/ref.arc" ]  || r=gone
    p=present; [ -f "$W/port.arc" ] || p=gone
    if [ "$r" != "$p" ]; then
      echo "  DIFF [$m a $opt]: reference $r, port $p"
      fail=$((fail + 1))
    elif [ "$r" = present ] && ! cmp -s "$W/ref.arc" "$W/port.arc"; then
      echo "  DIFF [$m a $opt]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
      fail=$((fail + 1))
    fi
  done
done

# The delete command. Filespecs match on the BASE NAME by default, so "a.txt"
# deletes every a.txt at any depth and "*" takes the directories too -- which
# empties the archive, and an emptied archive is REMOVED rather than written.
for m in -m0 -m1 -m4 -m9; do
  for spec in a.txt newer.txt "sub/nested.txt" "*.txt" "*"; do
    checked=$((checked + 1))
    build_tree "$W/src"
    printf 'nested a\n' > "$W/src/sub/a.txt"
    rm -f "$W/ref.arc" "$W/port.arc"
    ( cd "$W/src" && "$REF" a --nodates -r -y "$m" "$W/ref.arc" . ) >/dev/null 2>&1
    cp "$W/ref.arc" "$W/port.arc"
    ( cd "$W/src" && "$REF"  d --nodates -y "$m" "$W/ref.arc"  "$spec" ) >/dev/null 2>&1
    ( cd "$W/src" && "$PORT" d --nodates -y "$m" "$W/port.arc" "$spec" ) >/dev/null 2>&1

    r=present; [ -f "$W/ref.arc" ]  || r=gone
    p=present; [ -f "$W/port.arc" ] || p=gone
    if [ "$r" != "$p" ]; then
      echo "  DIFF [$m d $spec]: reference $r, port $p"
      fail=$((fail + 1))
    elif [ "$r" = present ] && ! cmp -s "$W/ref.arc" "$W/port.arc"; then
      echo "  DIFF [$m d $spec]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
      fail=$((fail + 1))
    fi
  done
done

echo "arc u/f/d: $checked archives, $fail differing"
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

# --sync must actually DELETE. Every --sync row above passes if --sync is
# silently treated as -u, since the two agree on every file that exists on both
# sides -- they differ only on a file the archive has and the disk does not.
# Require that file to be gone, and require --sync to differ from -u.
build_tree "$W/src"
printf 'only in the archive\n' > "$W/src/gone.txt"
touch -t 202501010000 "$W/src/gone.txt"
rm -f "$W/sync.arc" "$W/upd.arc"
( cd "$W/src" && "$REF" a --nodates -y -m1 "$W/sync.arc" older.txt gone.txt ) >/dev/null 2>&1
rm -f "$W/src/gone.txt"
cp "$W/sync.arc" "$W/upd.arc"
( cd "$W/src" && "$PORT" a --nodates -r --sync -y -m1 "$W/sync.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -u     -y -m1 "$W/upd.arc"  . ) >/dev/null 2>&1
if saw 'gone.txt' "$PORT" l "$W/sync.arc"; then
  echo "SELF-TEST FAILED: --sync left a file the disk no longer has, so it is" >&2
  echo "not deleting and the rows above compared it against -u" >&2
  exit 1
fi
if ! saw 'gone.txt' "$PORT" l "$W/upd.arc"; then
  echo "SELF-TEST FAILED: -u also dropped the file, so the archives the --sync" >&2
  echo "rows compare are not distinguishable from the -u ones" >&2
  exit 1
fi

# `d "*"` must actually have emptied something, or the removal branch is
# untested.
build_tree "$W/src"
rm -f "$W/d.arc"
( cd "$W/src" && "$REF" a --nodates -r -y -m1 "$W/d.arc" . ) >/dev/null 2>&1
[ -f "$W/d.arc" ] || { echo "SELF-TEST FAILED: no archive to delete from" >&2; exit 1; }
( cd "$W/src" && "$PORT" d --nodates -y -m1 "$W/d.arc" "*" ) >/dev/null 2>&1
if [ -f "$W/d.arc" ]; then
  echo "SELF-TEST FAILED: deleting * left the archive behind, so the removal" >&2
  echo "branch was never taken" >&2
  exit 1
fi

echo "the Rust arc u/f/d merges exactly as the Haskell one does"
