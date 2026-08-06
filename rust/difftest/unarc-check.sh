#!/usr/bin/env bash
# The Rust `unarc` against the C one it replaces.
#
#   usage: unarc-check.sh [c-unarc] [rust-unarc]
#
# This is the migration gate. `Unarc/unarc.cpp` + `ArcStructure.h` + `CUI.h`
# were a SECOND implementation of the archive reader, and the Rust replacement
# deliberately is not: it owns no format knowledge and calls `darc-arc`. The
# thing to prove is therefore not that two parsers agree — it is that dropping
# the second parser changed nothing a user can see.
#
# So this compares the OUTCOME on both sides: exit code, and the extracted tree
# byte for byte, over archives written with a spread of methods and shapes.
#
# What it does NOT compare is console text. The C prints a banner, progress
# redraws and a timing line; the Rust prints neither. `CLAUDE.md` settled that
# message-identity is the lowest-priority property here, and `arc-cli-check.sh`
# records what that cost when it was gated instead.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
CUNARC="${1:-$ROOT/Unarc/unarc}"
RUNARC="${2:-$ROOT/rust/target/release/unarc}"

[ -x "$CUNARC" ] || {
  echo "no C unarc at $CUNARC -- build it with:" >&2
  echo "  ./compile-c && make -C Unarc linux" >&2
  exit 2; }
( cd "$ROOT/rust" && cargo build --release -q -p darc-unarc -p darc-arc ) || {
  echo "cargo build failed" >&2; exit 1; }
[ -x "$RUNARC" ] || { echo "no Rust unarc at $RUNARC" >&2; exit 1; }
DARC="$ROOT/rust/target/release/darc"
[ -x "$DARC" ] || { echo "no darc at $DARC" >&2; exit 1; }

W="${TMPDIR:-/tmp}/unarc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# A tree with the shapes that have broken readers before: nested directories
# (the entry the 4-byte time field turned into a zero-length file), an empty
# file, a file whose name needs no escaping but whose CONTENT is incompressible,
# and enough text to make a solid block worth having.
mkdir -p "$W/src/sub/deeper" "$W/src/empty-dir"
printf 'hello\n'  > "$W/src/a.txt"
printf ''         > "$W/src/zero.bin"
printf 'nested\n' > "$W/src/sub/b.txt"
printf 'deeper\n' > "$W/src/sub/deeper/c.txt"
head -c 40000 /dev/urandom > "$W/src/incompressible.bin"
awk 'BEGIN{for(i=0;i<4000;i++) printf "compressible line %d\n", i%211}' > "$W/src/text.txt"

fail=0 checked=0

# one <label> <darc-create-args...> -- build an archive, extract it with both,
# require the same exit code and the same tree.
one() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -f "$W/t.arc"; rm -rf "$W/c" "$W/r"; mkdir -p "$W/c" "$W/r"
  ( cd "$W/src" && "$DARC" a --nodates -y -r "$@" "$W/t.arc" . ) >/dev/null 2>&1
  [ -f "$W/t.arc" ] || { echo "  NO ARCHIVE [$label]: darc wrote nothing"; fail=$((fail+1)); return; }

  local rc_c=0 rc_r=0
  ( cd "$W/c" && "$CUNARC" x "$W/t.arc" ) >/dev/null 2>&1 || rc_c=$?
  ( cd "$W/r" && "$RUNARC" x "$W/t.arc" ) >/dev/null 2>&1 || rc_r=$?

  local bad=""
  [ "$rc_c" = "$rc_r" ] || bad="$bad exit($rc_c vs $rc_r)"
  diff -r "$W/c" "$W/r" > "$W/tree.diff" 2>&1 || bad="$bad tree"
  # ...and both must actually have produced the input back, or the two agreeing
  # proves only that they failed the same way.
  diff -r "$W/src" "$W/r" > "$W/orig.diff" 2>&1 || bad="$bad not-the-original"

  # The LISTING is held against `darc`, not against the C.
  #
  # Not an oversight -- the C's listing disagrees with the archiver's, and the
  # Rust one agrees with it. On the same archive the C prints
  #
  #     1970-01-01 09:00:00       -dir- ./sub
  #     1970-01-01 09:00:00       20000 ./c.bin
  #
  # where `darc l` prints `sub` and `20.000`: the C keeps the `./` that `arc a
  # … .` stores and never groups the digits. That is the drift retiring the C++
  # is meant to end, so requiring the Rust to reproduce it would gate the bug in.
  # `unarc l` must equal `darc l` exactly, which is the one-implementation
  # property stated as a test.
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

echo "C:    $CUNARC"
echo "Rust: $RUNARC"

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

# ── the comparison must be able to fail ─────────────────────────────────────
#
# Everything above passes if both extractors are broken in the same way, or if
# `diff -r` is comparing two empty directories because neither ran. Prove it
# separates a good tree from a bad one.
rm -rf "$W/c" "$W/r"; mkdir -p "$W/c" "$W/r"
( cd "$W/r" && "$RUNARC" x "$W/t.arc" ) >/dev/null 2>&1
printf 'sabotage\n' > "$W/c/a.txt"
if diff -r "$W/c" "$W/r" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: a tree with different contents compared equal, so the" >&2
  echo "comparison above was not looking at the files" >&2
  exit 1
fi
# ...and the extraction must have produced something, or "differs from sabotage"
# would be satisfied by an empty directory.
n=$(find "$W/r" -type f | wc -l | tr -d '[:space:]')
[ "$n" -ge 5 ] || {
  echo "SELF-TEST FAILED: the Rust extractor wrote $n files, so the runs above" >&2
  echo "were comparing near-empty trees" >&2
  exit 1; }

echo "the Rust unarc extracts what the C one does, over $checked archives"
