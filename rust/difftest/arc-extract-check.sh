#!/usr/bin/env bash
# Differential-test the Rust `arc x` / `arc e` against the Haskell one.
#
#   usage: arc-extract-check.sh [reference-arc]
#
# The comparison is `diff -r` over the whole extracted tree: every path, every
# byte, and any file present on one side only. Not a file count and not a sample
# -- a port that extracts 225 of 226 files correctly and drops one is exactly
# what a count would miss.
#
# `e` is compared as well as `x`, because they differ in the one place path
# handling can go wrong: `make_disk_name` is `const ""` for `e`
# (ArhiveDirectory.hs:292), so every file lands flat in the destination and
# same-named files in different directories collide. Whatever the reference does
# with that collision, the port must do too.
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
[ -x "$PORT" ] || { echo "cargo produced no $PORT" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-extract-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

fail=0 checked=0

for m in -m0 -m1 -m4 -m9 -mtor -mppmd; do
  for s in -s -s-; do
    rm -f "$W/a.arc"
    ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" "$s" ../a.arc . ) >/dev/null 2>&1
    [ -f "$W/a.arc" ] || { echo "  SKIP [$m $s]: no archive"; continue; }

    for cmd in x e; do
      checked=$((checked + 1))
      rm -rf "$W/ref" "$W/port"; mkdir -p "$W/ref" "$W/port"

      ( cd "$W/ref" && "$REF" "$cmd" -y "$W/a.arc" ) >/dev/null 2>&1
      ref_rc=$?
      # -dp is the real option: the port takes the same command line as arc.
      "$PORT" "$cmd" "-dp$W/port" "$W/a.arc" >/dev/null 2>"$W/port.err"
      port_rc=$?

      if [ "$ref_rc" -ne 0 ] || [ "$port_rc" -ne 0 ]; then
        echo "  DIFF [$m $s $cmd]: exit ref=$ref_rc port=$port_rc"
        head -3 "$W/port.err" | sed 's/^/      /'
        fail=$((fail + 1))
        continue
      fi
      if ! diff -r "$W/ref" "$W/port" >"$W/tree.diff" 2>&1; then
        echo "  DIFF [$m $s $cmd]: extracted trees differ"
        head -6 "$W/tree.diff" | sed 's/^/      /'
        fail=$((fail + 1))
      fi
    done
  done
done

echo "arc x/e: $checked extractions, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# `diff -r` over two empty directories succeeds, so every row above would pass
# if neither side extracted anything. Prove the tree is non-empty and that a
# deliberate difference IS caught.
rm -rf "$W/port"; mkdir -p "$W/port"
"$PORT" x "-dp$W/port" "$W/a.arc" >/dev/null 2>&1
files=$(find "$W/port" -type f | wc -l | tr -d ' ')
if [ "$files" -lt 100 ]; then
  echo "SELF-TEST FAILED: the port extracted only $files files, so the tree" >&2
  echo "comparison above was comparing almost nothing" >&2
  exit 1
fi
victim=$(find "$W/port" -type f | head -1)
printf 'x' >> "$victim"
if diff -r "$W/ref" "$W/port" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: an appended byte was not caught by diff -r" >&2
  exit 1
fi

echo "the Rust arc x/e extracts the same tree as the Haskell one"
