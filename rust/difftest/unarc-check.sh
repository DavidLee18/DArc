#!/usr/bin/env bash
# `unarc` must agree with `darc`.
#
#   usage: unarc-check.sh [darc] [unarc]
#
# This used to compare the Rust `unarc` against the C++ one in Unarc/. That
# comparison did its job -- it gated the migration, over 9 archives, and passed
# 9/9 -- and then the C++ was deleted, so it cannot be run again. What replaces
# it is not a weaker version of the same test; it is a different property, and
# the one that is still falsifiable:
#
#   `unarc` owns NO format knowledge. It reads argv and calls darc-arc.
#
# So the thing to gate is the argv layer, because that is all there is. If
# `unarc x` and `darc x` ever produce different trees from one archive, the
# difference is in option parsing or in Layout, since below that they are
# literally the same function. That also lets this cover ground the C
# comparison never did: `-e` (flatten) and `-dp<path>`/`-d<path>` (extract
# elsewhere) had no tested C++ counterpart here.
#
# What it does NOT compare is console text -- `darc` prints a banner and
# progress, `unarc` prints one line. `CLAUDE.md` settles that message-identity
# is the lowest-priority property here.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DARC="${1:-$ROOT/rust/target/release/darc}"
RUNARC="${2:-$ROOT/rust/target/release/unarc}"

( cd "$ROOT/rust" && cargo build --release -q -p darc-unarc -p darc-arc ) || {
  echo "cargo build failed" >&2; exit 1; }
for b in "$DARC" "$RUNARC"; do
  [ -x "$b" ] || { echo "no binary at $b" >&2; exit 1; }
done
# They must not be the SAME binary. Both live in target/release and the SFX
# story makes copies of `unarc` under other names, so a stray `cp` -- or a
# caller passing the same path twice -- would make every comparison below
# trivially true. This is the guard that #150 needed and did not have.
if cmp -s "$DARC" "$RUNARC"; then
  echo "darc and unarc are the same file; there is nothing to compare" >&2
  exit 1
fi

W="${TMPDIR:-/tmp}/unarc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# A tree with the shapes that have broken readers before: nested directories
# (the entry ArcStructure.h's 4-byte time field turned into a zero-length file),
# an empty file, incompressible content, and enough text to make a solid block
# worth having.
mkdir -p "$W/src/sub/deeper" "$W/src/empty-dir"
printf 'hello\n'  > "$W/src/a.txt"
printf ''         > "$W/src/zero.bin"
printf 'nested\n' > "$W/src/sub/b.txt"
printf 'deeper\n' > "$W/src/sub/deeper/c.txt"
head -c 40000 /dev/urandom > "$W/src/incompressible.bin"
awk 'BEGIN{for(i=0;i<4000;i++) printf "compressible line %d\n", i%211}' > "$W/src/text.txt"

fail=0 checked=0

# one <label> <darc-create-args...>
one() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -f "$W/t.arc"; rm -rf "$W/d" "$W/r"; mkdir -p "$W/d" "$W/r"
  ( cd "$W/src" && "$DARC" a --nodates -y -r "$@" "$W/t.arc" . ) >/dev/null 2>&1
  [ -f "$W/t.arc" ] || { echo "  NO ARCHIVE [$label]: darc wrote nothing"; fail=$((fail+1)); return; }

  local rc_d=0 rc_r=0
  ( cd "$W/d" && "$DARC"  x -y "$W/t.arc" ) >/dev/null 2>&1 || rc_d=$?
  ( cd "$W/r" && "$RUNARC" x    "$W/t.arc" ) >/dev/null 2>&1 || rc_r=$?

  local bad=""
  [ "$rc_d" = "$rc_r" ] || bad="$bad exit($rc_d vs $rc_r)"
  diff -r "$W/d" "$W/r" > "$W/tree.diff" 2>&1 || bad="$bad tree"
  # ...and both must have produced the input back, or the two agreeing proves
  # only that they failed the same way.
  diff -r "$W/src" "$W/r" > "$W/orig.diff" 2>&1 || bad="$bad not-the-original"

  "$DARC"   l "$W/t.arc" >| "$W/l.darc"  2>/dev/null
  "$RUNARC" l "$W/t.arc" >| "$W/l.unarc" 2>/dev/null
  if ! cmp -s "$W/l.darc" "$W/l.unarc"; then
    bad="$bad listing"
    diff "$W/l.darc" "$W/l.unarc" >| "$W/l.diff" 2>&1
  else
    : >| "$W/l.diff"
  fi

  if [ -n "$bad" ]; then
    echo "  DIFF [$label]:$bad"
    head -4 "$W/tree.diff" 2>/dev/null | sed 's/^/      /'
    head -4 "$W/orig.diff" 2>/dev/null | sed 's/^/      orig: /'
    head -4 "$W/l.diff"    2>/dev/null | sed 's/^/      list: /'
    fail=$((fail + 1))
  fi
}

echo "darc:  $DARC"
echo "unarc: $RUNARC"

one "stored"        -m0
one "lzma"          -m4
one "tornado"       -mtor
one "grzip -mt1"    -mgrzip -mt1
one "rep+lzma"      -mrep+lzma:d1m
one "delta+lzma"    -mdelta+lzma:d1m
one "no-solid"      -m4 -s-
one "one block"     -m4 -s
one "lzma2 -mt1"    -mlzma2:d1m -mt1

echo "unarc: $checked archives, $fail differing"
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }
[ "$fail" -eq 0 ] || exit 1

# ── the option layer, which is the only layer unarc actually owns ───────────
rm -f "$W/t.arc"; ( cd "$W/src" && "$DARC" a --nodates -y -r -m4 "$W/t.arc" . ) >/dev/null 2>&1

# -e flattens: every file lands in one directory, no subdirectories survive.
rm -rf "$W/e"; mkdir -p "$W/e"
( cd "$W/e" && "$RUNARC" e "$W/t.arc" ) >/dev/null 2>&1
[ -f "$W/e/c.txt" ] || {
  echo "SELF-TEST FAILED: -e did not flatten; $W/e/c.txt is missing" >&2; exit 1; }
if [ -d "$W/e/sub" ]; then
  echo "SELF-TEST FAILED: -e kept the directory structure" >&2; exit 1
fi

# -d<path> extracts somewhere else, and `x` keeps paths there.
rm -rf "$W/out"
( cd "$W" && "$RUNARC" x "-d$W/out" "$W/t.arc" ) >/dev/null 2>&1
[ -f "$W/out/sub/deeper/c.txt" ] || {
  echo "SELF-TEST FAILED: -d did not extract into $W/out keeping paths" >&2; exit 1; }

# -dp<path> is the plain-unarc spelling of the same thing (issue #177). Reading
# it as `-d` put the `p` at the front: `-dpFolder` extracted into `pFolder`.
# The destination below is RELATIVE and begins with `p` on purpose: under the
# old parse the argument reads as `-d` + `ppdest`, so the tree lands in
# `$W/ppdest` and this case fails for the exact reason reported. An absolute
# path would not work here -- `-dp/abs/pdest` misreads as the relative path
# `p/abs/pdest`, which is a different directory again and a weaker check.
rm -rf "$W/pdest" "$W/ppdest"
( cd "$W" && "$RUNARC" x "-dppdest" "$W/t.arc" ) >/dev/null 2>&1
[ -f "$W/pdest/sub/deeper/c.txt" ] || {
  echo "SELF-TEST FAILED: -dp did not extract into $W/pdest keeping paths" >&2; exit 1; }
if [ -d "$W/ppdest" ]; then
  echo "SELF-TEST FAILED: -dp was read as -d, so the path grew a leading p" >&2; exit 1
fi

# --noarcext is accepted and ignored rather than refused, which is what the C did
# too -- it sets the flag and never reads it. Before #177 this printed usage and
# exited 2, which a caller passing it for the C's benefit reads as a hard
# failure.
"$RUNARC" l --noarcext "$W/t.arc" >/dev/null 2>&1 || {
  echo "SELF-TEST FAILED: --noarcext was refused" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
#
# Everything above passes if both extractors are broken in the same way, or if
# `diff -r` is comparing two empty directories because neither ran.
rm -rf "$W/d" "$W/r"; mkdir -p "$W/d" "$W/r"
( cd "$W/r" && "$RUNARC" x "$W/t.arc" ) >/dev/null 2>&1
printf 'sabotage\n' > "$W/d/a.txt"
if diff -r "$W/d" "$W/r" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: a tree with different contents compared equal, so the" >&2
  echo "comparison above was not looking at the files" >&2
  exit 1
fi
n=$(find "$W/r" -type f | wc -l | tr -d '[:space:]')
[ "$n" -ge 5 ] || {
  echo "SELF-TEST FAILED: unarc wrote $n files, so the runs above were" >&2
  echo "comparing near-empty trees" >&2
  exit 1; }

echo "unarc agrees with darc over $checked archives, and -e/-d/-dp do what they say"
