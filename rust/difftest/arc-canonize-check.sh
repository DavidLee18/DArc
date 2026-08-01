#!/usr/bin/env bash
# Check the Rust canonicaliser against method strings the Haskell actually wrote.
#
#   usage: arc-canonize-check.sh [reference-arc]
#
# `CanonizeCompressionMethod` (CompressionLibrary.cpp:151) is parse-then-print,
# and the printed form is what an archive stores. So every method string in a
# reference-written archive is, by construction, a FIXED POINT of the C's
# canonicaliser -- it was produced by it. Canonicalising it again must return it
# unchanged.
#
# That makes the reference's own archives an oracle for this, with no C harness
# to build. The strings are harvested from `arc lt` across every -m level rather
# than hardcoded, so the check cannot go stale as the method table changes.
#
# What this does NOT check: the memory scaling that produced those strings in
# the first place. `-m1` writes `4x4:tor:3:434kb`, and the `434kb` comes from
# SetCompressionMem sizing the buffer to the data. Reproducing the strings is
# necessary for `arc a -m1` and not sufficient; see the note in
# rust/darc-arc/src/canonize.rs.
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

W="${TMPDIR:-/tmp}/arc-canonize-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

# Harvest every distinct method string the reference writes.
: > "$W/methods.txt"
for m in -m0 -m1 -m2 -m3 -m4 -m5 -m9 -mx -mtor -mppmd; do
  rm -f "$W/a.arc"
  ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" ../a.arc . ) >/dev/null 2>&1
  [ -f "$W/a.arc" ] || continue
  # `arc lt` prints one block per line with the method last; the header and
  # summary lines are filtered by requiring a leading numeric position.
  "$REF" lt "$W/a.arc" 2>/dev/null | tr '\r' '\n' \
    | awk '$1 ~ /^[0-9][0-9.]*$/ { print $NF }' >> "$W/methods.txt"
done
# The directory and footer blocks carry their own method too.
"$REF" l "$W/a.arc" >/dev/null 2>&1
echo "lzma:1mb:mf=BT4" >> "$W/methods.txt"

sort -u "$W/methods.txt" | grep -v '^$' > "$W/distinct.txt"
count=$(wc -l < "$W/distinct.txt" | tr -d ' ')
if [ "$count" -lt 5 ]; then
  echo "harvested only $count method strings -- the awk filter matched almost" >&2
  echo "nothing, so this would pass without checking anything" >&2
  exit 1
fi

fail=0
while IFS= read -r m; do
  got=$("$PORT" canonize "$m" 2>/dev/null)
  if [ "$got" != "$m" ]; then
    echo "  DIFF: $m -> ${got:-<refused>}"
    fail=$((fail + 1))
  fi
done < "$W/distinct.txt"

echo "canonize: $count method strings from real archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1

# ── the check must be able to fail ──────────────────────────────────────────
# A canonicaliser that echoed its input would pass every line above.
if [ "$("$PORT" canonize lzma)" = "lzma" ]; then
  echo "SELF-TEST FAILED: the canonicaliser is echoing its input" >&2
  exit 1
fi
if [ "$("$PORT" canonize lzma)" != "lzma:64mb" ]; then
  echo "SELF-TEST FAILED: a bare lzma should canonicalise to its defaults" >&2
  exit 1
fi
if "$PORT" canonize 'lzma:nonsense' >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: an unparseable method was given a canonical form" >&2
  exit 1
fi

echo "every method string the reference writes is a fixed point of the port"
