#!/usr/bin/env bash
# Differential-test the Rust listing commands -- l, v, lt and lb -- against the
# Haskell ones.
#
#   usage: arc-list-check.sh [reference-arc]
#
# The reference defaults to Tests/arc-ghc, built by ./compile-ghc-probe. That is
# deliberate: docs/testing.md records that the MicroHs build (Tests/arc) bypasses
# the Haskell pipeline entirely under __MHS__, so it is a sound reference for
# archive BYTES and a poor one for BEHAVIOUR -- and a listing is behaviour.
# Tests/arc works here too, since listing takes the same path in both, but the
# GHC build is the one this port is defined against.
#
# What is compared: the whole listing, byte for byte, minus the banner line that
# carries the archive's path. Not a file count, not a sample -- the entry lines,
# the column widths, the digit grouping, the summary and the trailing blank line.
# The formatting IS the interface.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

. "$ROOT/rust/difftest/arc-reference.sh"

( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }
[ -x "$PORT" ] || { echo "cargo produced no $PORT" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-list-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

fail=0 checked=0 skipped=0

# --nodates so the archive is reproducible; the listing prints mtimes and no two
# runs would otherwise agree.
for m in -m0 -m1 -m4 -m9 -mx -mtor -mppmd; do
  for s in -s -s- -ms; do
    rm -f "$W/a.arc"
    ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" "$s" ../a.arc . ) >/dev/null 2>&1
    if [ ! -f "$W/a.arc" ]; then
      echo "  SKIP [$m $s]: the reference wrote no archive"
      skipped=$((skipped + 1))
      continue
    fi
    checked=$((checked + 1))
    for cmd in l v lt lb; do
      # Drop line 1 only: it is the banner, and it contains the sandbox path.
      # `lb` is the exception -- it prints no banner at all, the only listing
      # command that does not, so stripping a line would remove a real name.
      if [ "$cmd" = lb ]; then
        "$REF" lb "$W/a.arc" 2>/dev/null > "$W/ref.txt"
      else
        "$REF" "$cmd" "$W/a.arc" 2>/dev/null | tail -n +2 > "$W/ref.txt"
      fi
      "$PORT" "$cmd" "$W/a.arc" >"$W/port.txt" 2>"$W/port.err"
      if ! cmp -s "$W/ref.txt" "$W/port.txt"; then
        echo "  DIFF [$m $s $cmd]"
        diff "$W/ref.txt" "$W/port.txt" | head -6 | sed 's/^/      /'
        head -2 "$W/port.err" | sed 's/^/      /'
        fail=$((fail + 1))
      fi
    done
  done
done

echo "arc l/v/lt/lb: $checked archives x 4 commands, $fail differing, $skipped skipped"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "no archives were compared at all" >&2; exit 1; }

# The harness must be able to fail. Everything above would also pass if the
# listings were empty on both sides, or if the port never ran -- an earlier run
# of this comparison reported "10 differing" for the sole reason that the port
# binary had been built into the wrong directory and did not exist.
: > "$W/empty.arc"
if "$PORT" l "$W/empty.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port listed an empty file as a valid archive" >&2
  exit 1
fi
if ! "$PORT" l "$W/a.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port cannot list an archive it just matched" >&2
  exit 1
fi

# The four commands must actually differ from each other, or three of them are
# being compared as if they were one.
a=$("$PORT" l "$W/a.arc" | shasum); b=$("$PORT" v "$W/a.arc" | shasum)
c=$("$PORT" lt "$W/a.arc" | shasum); d=$("$PORT" lb "$W/a.arc" | shasum)
if [ "$a" = "$b" ] || [ "$a" = "$c" ] || [ "$a" = "$d" ]; then
  echo "SELF-TEST FAILED: two listing commands produced identical output" >&2
  exit 1
fi

echo "the Rust listings are byte-identical to the Haskell ones"
