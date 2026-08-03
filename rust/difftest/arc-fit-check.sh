#!/usr/bin/env bash
# Check the Rust dictionary limiting against what the reference actually wrote.
#
#   usage: arc-fit-check.sh [reference-arc]
#
# The `arc lt` block table has exactly five columns -- Pos, Size, Compressed,
# Files, Method -- and the awk filters below require all five. A looser filter
# matches the summary line ("226 files, 438.744 bytes, 0 compressed") too, whose
# last field is the word "compressed"; that produced ten spurious failures on the
# first run of this script.
#
# `ArcvProcessRead.hs:122` shrinks a chain's dictionary to fit the solid block
# before compressing it, so the method string in an archive is a function of the
# chain AND of that block's unpacked size. Two checks, of different strengths:
#
#   1. EXACT PREDICTION, for the levels that use a single chain for all files:
#      fit(chain, block size) must equal the string the reference wrote. This is
#      the real check -- it would catch a wrong rounding rule, a wrong slack
#      term, or a wrong per-codec setter.
#
#   2. IDEMPOTENCE, for every data block of every level: the reference already
#      fitted these strings, so fitting them again to the same block size must
#      change nothing. Weaker, but it covers the chains that check 1 cannot
#      predict yet.
#
# What is NOT covered: which files land in which solid block. From -m2 upward
# DArc splits by file type ($text/$obj/$binary), so a level's chain is fitted
# against a subset's size, not the archive's. That is why -m4 writes 379kb where
# a single block would give 434kb. Predicting those needs the type grouping,
# which is not ported yet -- so those levels get check 2 only, and this script
# says so rather than quietly skipping them.
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

W="${TMPDIR:-/tmp}/arc-fit-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

fail=0 predicted=0 idempotent=0

# ── 1. exact prediction ─────────────────────────────────────────────────────
# (option, unfitted chain from builtinMethodSubsts / the method's own default)
while IFS='|' read -r opt chain; do
  rm -f "$W/a.arc"
  ( cd "$W/corpus" && "$REF" a --nodates -r -y "$opt" ../a.arc . ) >/dev/null 2>&1
  [ -f "$W/a.arc" ] || { echo "  SKIP [$opt]: no archive"; continue; }

  # The one non-storing data block: its unpacked size and its method.
  line=$("$REF" lt "$W/a.arc" 2>/dev/null | tr '\r' '\n' \
         | awk 'NF==5 && $1 ~ /^[0-9.]+$/ && $4 ~ /^[0-9]+$/ && $5 != "storing" { print $2, $5 }' | head -1)
  size=$(echo "$line" | awk '{gsub(/\./,"",$1); print $1}')
  got=$(echo "$line" | awk '{print $2}')
  [ -n "$size" ] && [ -n "$got" ] || { echo "  SKIP [$opt]: no data block found"; continue; }

  predicted=$((predicted + 1))
  want=$("$PORT" fit "$size" "$chain" 2>/dev/null)
  if [ "$want" != "$got" ]; then
    echo "  DIFF [$opt] block of $size bytes: reference $got, port $want"
    fail=$((fail + 1))
  fi
done <<'EOF'
-mtor|tor
-mppmd|ppmd
-m1|4x4:tor:3
EOF

# ── 2. idempotence over every level ─────────────────────────────────────────
for m in -m0 -m1 -m2 -m3 -m4 -m5 -m9 -mx -mtor -mppmd; do
  rm -f "$W/a.arc"
  ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" ../a.arc . ) >/dev/null 2>&1
  [ -f "$W/a.arc" ] || continue
  "$REF" lt "$W/a.arc" 2>/dev/null | tr '\r' '\n' \
    | awk 'NF==5 && $1 ~ /^[0-9.]+$/ && $4 ~ /^[0-9]+$/ { gsub(/\./,"",$2); print $2, $5 }' \
  | while read -r size method; do
      [ -n "$method" ] || continue
      again=$("$PORT" fit "$size" "$method" 2>/dev/null)
      if [ "$again" != "$method" ]; then
        echo "  DIFF [$m] refitting $method to $size gave $again"
      fi
    done > "$W/idem.$m.txt"
  bad=$(wc -l < "$W/idem.$m.txt" | tr -d ' ')
  cat "$W/idem.$m.txt"
  fail=$((fail + bad))
  idempotent=$((idempotent + 1))
done

echo "fit: $predicted exact predictions, $idempotent levels re-fitted, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$predicted" -ge 3 ] || { echo "fewer predictions than expected -- the awk filter" >&2
                            echo "matched nothing, so this checked almost nothing" >&2; exit 1; }

# ── the check must be able to fail ──────────────────────────────────────────
# Fitting must actually DO something, or every line above passes trivially.
if [ "$("$PORT" fit 438744 tor)" = "tor" ]; then
  echo "SELF-TEST FAILED: fitting is a no-op" >&2
  exit 1
fi
if [ "$("$PORT" fit 438744 tor)" != "tor:434kb" ]; then
  echo "SELF-TEST FAILED: the measured 434kb limit is not being reproduced" >&2
  exit 1
fi
# ...and a different block size must give a different answer.
if [ "$("$PORT" fit 10240 tor)" = "$("$PORT" fit 438744 tor)" ]; then
  echo "SELF-TEST FAILED: the limit does not depend on the data size" >&2
  exit 1
fi

echo "dictionary limiting reproduces the reference's method strings"
