#!/usr/bin/env bash
# Differential-test the Rust `arc l` against the Haskell one.
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
PORT="$ROOT/rust/target/release/arclist"

[ -x "$REF" ] || {
  echo "no reference binary at $REF" >&2
  echo "build one with ./compile-ghc-probe (or pass Tests/arc)" >&2
  exit 2
}

( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin arclist ) || {
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
    # Drop line 1 only: it is the banner, and it contains the sandbox path.
    "$REF"  l "$W/a.arc" 2>/dev/null | tail -n +2 > "$W/ref.txt"
    "$PORT"   "$W/a.arc" >"$W/port.txt" 2>"$W/port.err"
    if ! cmp -s "$W/ref.txt" "$W/port.txt"; then
      echo "  DIFF [$m $s]"
      diff "$W/ref.txt" "$W/port.txt" | head -6 | sed 's/^/      /'
      head -2 "$W/port.err" | sed 's/^/      /'
      fail=$((fail + 1))
    fi
  done
done

echo "arc l: $checked archives, $fail differing, $skipped skipped"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "no archives were compared at all" >&2; exit 1; }

# The harness must be able to fail. Everything above would also pass if the
# listings were empty on both sides, or if the port never ran -- an earlier run
# of this comparison reported "10 differing" for the sole reason that the port
# binary had been built into the wrong directory and did not exist.
: > "$W/empty.arc"
if "$PORT" "$W/empty.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port listed an empty file as a valid archive" >&2
  exit 1
fi
if ! "$PORT" "$W/a.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port cannot list an archive it just matched" >&2
  exit 1
fi

echo "the Rust listing is byte-identical to the Haskell one"
