#!/usr/bin/env bash
# Differential-test the Rust solid-block grouping (`-s`) against the Haskell
# one, BYTE FOR BYTE.
#
#   usage: arc-solid-check.sh [reference-arc]
#
# A solid block is a run of files compressed as one stream: bigger blocks
# compress better and cost more to extract from, and `-s` is where that trade is
# made. `parseSolidOption` (Cmdline.hs:757) turns the option into a LIST of
# criteria, and a block ends as soon as ANY of them says so -- `newLen = minimum
# $ map (`splitLen` files) crits`. They are limits, not alternatives.
#
# ── Two rules that look like one ───────────────────────────────────────────
#
# `GroupBySize` is a MINIMUM: it counts files while the running total is below
# the limit and then adds one more, so a block reaches at least the requested
# size. `GroupByBlockSize` -- which the compressor imposes, not the user -- is a
# MAXIMUM with no such `+1`, because a block algorithm's window cannot be
# exceeded, and its limit is quadrupled first when under 8 MB (`special`).
# Reading them as the same rule gives the right answer for neither.
#
# ── Options that are not reachable ─────────────────────────────────────────
#
# `-scab` is NOT the cab preset: it prefix-matches `--charset`, which is in
# aPREFFERED_OPTIONS and wins, so it means `--charset=ab`. The preset needs
# `--solid=cab`. `-szip`, `-sarj` and `-s7z` have no such clash and work.
#
# ── The dirs half of the option is dead in this version ────────────────────
#
# `parseSolidOption` returns a grouping for DIRECTORY blocks too, and three
# presets also force `-dm0`. Only the second of those does anything:
# `createDirBlock` writes exactly one directory block regardless
# (ArcvProcessRead.hs:77), so `-sarj`'s `[GroupNone]` for directories changes
# nothing even on a tree with subdirectories -- measured. The `-dm0` override
# IS visible and was worth ~450 bytes on a sixteen-file corpus.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

[ -x "$REF" ] || {
  echo "no reference binary at $REF.

The Haskell reference was deleted; build one from a commit that still has it:
  git worktree add /tmp/darc-ref 9a127e6 && (cd /tmp/darc-ref && ./compile-ghc-probe)
then pass /tmp/darc-ref/Tests/arc-ghc as $1. For a gate that needs no
reference at all, use arc-golden-check.sh" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }
# `arcdump` is a SECOND binary, and the self-test at the end of this file is the
# only thing that uses it. Nothing built it, so on any machine that had not
# happened to build it by hand the self-test read an empty output and reported a
# failure -- which is exactly what happened the first time CI ever ran this.
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin arcdump ) || {
  echo "cargo build of arcdump failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-solid-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0
# `wc -c`, not `stat`: GNU `stat -f` means --file-system and SUCCEEDS, so the
# usual `stat -f '%z' || stat -c '%s'` pair never reaches its fallback on Linux
# and returns filesystem info instead of a size. See arc-sfx-check.sh, where
# that produced `File: unbound variable` on the first CI run.
size() { wc -c < "$1" | tr -d '[:space:]'; }
# DATA blocks, which is what the grouping decides. NOT arcdump: that lists the
# footer's block table, which holds only the SERVICE blocks (header, directory,
# footer, recovery) and never the data ones -- so it reports the same three or
# four however the files were grouped. `lt` prints the real block table.
# A block row starts with the encryption marker column -- `*` for an encrypted
# block and a SPACE otherwise -- then the position. Anchoring on `*` alone
# counts zero for every unencrypted archive, which reads as "the grouping did
# nothing".
blocks() { "$PORT" lt "$1" 2>/dev/null | grep -cE '^(\*| ) *[0-9]' || true; }

# Files of several sizes and two extensions, in a few directories: enough for
# the by-size, by-count and by-extension criteria all to bite somewhere.
mkdir -p "$W/src/a/b" "$W/src/c"
for d in . a a/b c; do
  for i in 1 2 3; do
    head -c $((i * 2500)) /dev/urandom > "$W/src/$d/f$i.bin"
    printf 'text file %d with some words in it\n' "$i" > "$W/src/$d/t$i.txt"
  done
done
touch -t 202501010000 $(find "$W/src" -type f) $(find "$W/src" -type d)

try() {
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -r -y "$@" "$W/r.arc" . ) </dev/null >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -r -y "$@" "$W/p.arc" . ) </dev/null >/dev/null 2>&1
  local r=present p=present
  [ -f "$W/r.arc" ] || r=gone
  [ -f "$W/p.arc" ] || p=gone
  if [ "$r" != "$p" ]; then
    echo "  DIFF [$*]: reference $r, port $p"
    fail=$((fail + 1))
  elif [ "$r" = present ] && ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [$*]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes, $(blocks "$W/r.arc") vs $(blocks "$W/p.arc") blocks"
    fail=$((fail + 1))
  fi
}

for m in -m0 -m1 -m4 -m9; do
  try "$m"                    # the default: one block
  try "$m" -s                 # explicit GroupAll
  try "$m" -s-                # one block per file
  try "$m" -s1f               # the same thing spelled as a count
  try "$m" -s2f
  try "$m" -s5f
  try "$m" -se                # by extension
  try "$m" -s10k              # by size
  try "$m" -s4k
  try "$m" -s100b
  try "$m" -se4f              # two criteria: the shorter wins
  try "$m" -s2f10k
  try "$m" -se10k2f           # three
  try "$m" -s7z               # the presets that are reachable as -sXXX
  try "$m" -szip
  try "$m" -sarj
  try "$m" --solid=cab        # …and the one that is not
done

echo "arc -s: $checked archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every row passes if BOTH binaries ignore -s and write one block each time.

rm -f "$W/all.arc" "$W/each.arc" "$W/two.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m1      "$W/all.arc"  . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -y -m1 -s-  "$W/each.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -y -m1 -s2f "$W/two.arc"  . ) >/dev/null 2>&1
n_all=$(blocks "$W/all.arc"); n_each=$(blocks "$W/each.arc"); n_two=$(blocks "$W/two.arc")
if [ "$n_all" -ge "$n_each" ]; then
  echo "SELF-TEST FAILED: -s- ($n_each blocks) did not split more than the" >&2
  echo "default ($n_all), so the grouping is being ignored" >&2
  exit 1
fi
if [ "$n_two" -le "$n_all" ] || [ "$n_two" -ge "$n_each" ]; then
  echo "SELF-TEST FAILED: -s2f gave $n_two blocks, not between the default" >&2
  echo "($n_all) and one-per-file ($n_each), so the count criterion is wrong" >&2
  exit 1
fi

# The uncompressed-directory presets must actually store the directory.
rm -f "$W/z.arc" "$W/s.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m1 -szip "$W/z.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -y -m1 -s7z  "$W/s.arc" . ) >/dev/null 2>&1
if ! "$ROOT/rust/target/release/arcdump" "$W/z.arc" 2>/dev/null \
     | grep 'directory block' | grep -q 'storing'; then
  echo "SELF-TEST FAILED: -szip did not store the directory block" >&2
  "$ROOT/rust/target/release/arcdump" "$W/z.arc" 2>&1 | grep 'directory block' >&2
  exit 1
fi
if "$ROOT/rust/target/release/arcdump" "$W/s.arc" 2>/dev/null \
   | grep 'directory block' | grep -q 'storing'; then
  echo "SELF-TEST FAILED: -s7z stored the directory block, so the -dm override" >&2
  echo "is being applied where it should not be" >&2
  exit 1
fi

# -m0 must IGNORE the grouping: "for fake compressors or -m0 there is no point
# in splitting the block into parts".
rm -f "$W/m0.arc" "$W/m0s.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m0     "$W/m0.arc"  . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -y -m0 -s- "$W/m0s.arc" . ) >/dev/null 2>&1
if ! cmp -s "$W/m0.arc" "$W/m0s.arc"; then
  echo "SELF-TEST FAILED: -m0 -s- differs from -m0, but aNO_COMPRESSION is" >&2
  echo "supposed to short-circuit the split" >&2
  exit 1
fi

echo "the Rust arc groups files into solid blocks exactly as the Haskell one does"
