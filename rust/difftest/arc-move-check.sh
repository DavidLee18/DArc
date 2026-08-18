#!/usr/bin/env bash
# Differential-test the Rust `arc m`/`mf` and the -d/-df options against the
# Haskell ones -- the ARCHIVE byte for byte, and the DISK TREE they leave behind.
#
#   usage: arc-move-check.sh [reference-arc]
#
# ── Why this harness compares the disk, not just the archive ────────────────
#
# `m` is `a` plus `postProcessWrapper` (ArcCreate.hs:248): delete the
# successfully archived files afterwards. The archive it writes is BYTE
# IDENTICAL to the one `a` writes, so a harness that only compared archives
# would pass whether or not a single file was deleted. Every row here therefore
# checks the surviving disk tree too, and the self-tests at the end require the
# two commands to leave DIFFERENT trees.
#
#   m  / -d    delete the archived files AND the directories (deepest first)
#   mf / -df   delete only the files, leaving the directories
#
# Giving both is `CMDLINE_INCOMPATIBLE_OPTIONS`, not a union.
#
# ── The guard that must not be skipped ──────────────────────────────────────
#
# `checkThatFileWasNotChanged` (ArcCreate.hs:287) re-stats each file and deletes
# it only when the size and mtime still match what was archived. A file rewritten
# while the archive was being built keeps its new contents; the archive holds the
# old ones, so deleting it would lose data. Reproducing this needs the size and
# mtime AS SCANNED -- re-statting at deletion time compares the file with itself
# and always agrees, which is the same as not checking at all.
#
# Only files that came from DISK are deleted (`isFileOnDisk`), so an `m` that
# also carries archive files does not touch anything for them.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

. "$ROOT/rust/difftest/arc-reference.sh"
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-move-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub/deeper"
  printf 'the first file\n'  > "$d/a.txt"
  printf 'the second file\n' > "$d/b.dat"
  printf 'nested\n'          > "$d/sub/n.txt"
  printf 'deeper still\n'    > "$d/sub/deeper/d.txt"
  touch -t 202501010000 "$d/a.txt" "$d/b.dat" "$d/sub/n.txt" "$d/sub/deeper/d.txt"
}

# tree <dir> -- the surviving tree, as one comparable line.
tree() { ( cd "$1" && find . | sort | tr '\n' ' ' ); }

# run <label> <extra-args...> -- both binaries over identical fresh trees.
run() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
  build_tree "$W/r/src"; build_tree "$W/p/src"
  rm -f "$W/r/x.arc" "$W/p/x.arc"
  ( cd "$W/r/src" && "$REF"  "$@" "$W/r/x.arc" . ) >/dev/null 2>&1
  ( cd "$W/p/src" && "$PORT" "$@" "$W/p/x.arc" . ) >/dev/null 2>&1

  if ! cmp -s "$W/r/x.arc" "$W/p/x.arc"; then
    echo "  DIFF [$label]: archives differ, $(wc -c <"$W/r/x.arc" 2>/dev/null) vs $(wc -c <"$W/p/x.arc" 2>/dev/null) bytes"
    fail=$((fail + 1))
    return
  fi
  local rt pt
  rt="$(tree "$W/r/src")"; pt="$(tree "$W/p/src")"
  if [ "$rt" != "$pt" ]; then
    echo "  DIFF [$label]: surviving tree differs"
    echo "    reference: $rt"
    echo "    port:      $pt"
    fail=$((fail + 1))
  fi
}

for m in -m0 -m1 -m4 -m9; do
  run "m $m"        m  --nodates -r -y "$m"
  run "mf $m"       mf --nodates -r -y "$m"
  run "a -d $m"     a  --nodates -r -y "$m" -d
  run "a -df $m"    a  --nodates -r -y "$m" -df
done

# NOT tested here: `m -x*.dat`, which would show that a file the filters
# excluded is archived by neither side and survives on disk. `-x` is not
# implemented yet and the port now REFUSES it rather than ignoring it -- see the
# HONOURED list in darc.rs. Ignoring it is what this row originally caught: the
# port archived the .dat files and reported success.

# `u -d` on an existing archive: the files already in the archive are not disk
# files for this run, so what happens to them is the interesting part.
for m in -m1 -m4; do
  checked=$((checked + 1))
  rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
  build_tree "$W/r/src"; build_tree "$W/p/src"
  rm -f "$W/r/x.arc" "$W/p/x.arc"
  ( cd "$W/r/src" && "$REF" a --nodates -y "$m" "$W/r/x.arc" a.txt ) >/dev/null 2>&1
  ( cd "$W/p/src" && "$REF" a --nodates -y "$m" "$W/p/x.arc" a.txt ) >/dev/null 2>&1
  ( cd "$W/r/src" && "$REF"  u --nodates -r -y "$m" -d "$W/r/x.arc" . ) >/dev/null 2>&1
  ( cd "$W/p/src" && "$PORT" u --nodates -r -y "$m" -d "$W/p/x.arc" . ) >/dev/null 2>&1
  if ! cmp -s "$W/r/x.arc" "$W/p/x.arc"; then
    echo "  DIFF [u -d $m]: archives differ"; fail=$((fail + 1))
  elif [ "$(tree "$W/r/src")" != "$(tree "$W/p/src")" ]; then
    echo "  DIFF [u -d $m]: surviving tree differs"
    echo "    reference: $(tree "$W/r/src")"
    echo "    port:      $(tree "$W/p/src")"
    fail=$((fail + 1))
  fi
done

echo "arc m/mf: $checked runs, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every row passes if BOTH binaries delete nothing at all. These require the
# deletions to have happened, and to differ between m and mf.

rm -rf "$W/s"; mkdir -p "$W/s"
build_tree "$W/s/plain"; build_tree "$W/s/moved"; build_tree "$W/s/movedf"
before="$(tree "$W/s/plain")"
( cd "$W/s/plain"  && "$PORT" a  --nodates -r -y -m1 "$W/s/1.arc" . ) >/dev/null 2>&1
( cd "$W/s/moved"  && "$PORT" m  --nodates -r -y -m1 "$W/s/2.arc" . ) >/dev/null 2>&1
( cd "$W/s/movedf" && "$PORT" mf --nodates -r -y -m1 "$W/s/3.arc" . ) >/dev/null 2>&1

if [ "$(tree "$W/s/plain")" != "$before" ]; then
  echo "SELF-TEST FAILED: plain `a` deleted something" >&2
  exit 1
fi
if [ "$(tree "$W/s/moved")" = "$before" ]; then
  echo "SELF-TEST FAILED: m deleted nothing, so every row above compared two" >&2
  echo "untouched trees" >&2
  exit 1
fi
if [ "$(tree "$W/s/moved")" = "$(tree "$W/s/movedf")" ]; then
  echo "SELF-TEST FAILED: m and mf left the same tree, so nothing here" >&2
  echo "distinguishes deleting directories from not deleting them" >&2
  exit 1
fi
# mf must leave the directories and remove the files.
case "$(tree "$W/s/movedf")" in
  *sub/deeper*) ;;
  *) echo "SELF-TEST FAILED: mf removed a directory" >&2; exit 1 ;;
esac
case "$(tree "$W/s/movedf")" in
  *a.txt*) echo "SELF-TEST FAILED: mf left a file behind" >&2; exit 1 ;;
  *) ;;
esac

# The archives must be identical to what plain `a` wrote: deleting afterwards
# must not change a byte of the output.
if ! cmp -s "$W/s/1.arc" "$W/s/2.arc"; then
  echo "SELF-TEST FAILED: m wrote a different archive from a" >&2
  exit 1
fi

# `m` and `mf` together are an error, not a union.
rm -rf "$W/s/both"; build_tree "$W/s/both"
if ( cd "$W/s/both" && "$PORT" a --nodates -r -y -m1 -d -df "$W/s/4.arc" . ) >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: -d and -df together were accepted" >&2
  exit 1
fi

echo "the Rust arc m/mf archives and deletes exactly as the Haskell ones do"
