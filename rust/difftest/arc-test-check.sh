#!/usr/bin/env bash
# Differential-test the Rust `arc t` against the Haskell one.
#
#   usage: arc-test-check.sh [reference-arc]
#
# `arc t` decompresses every solid block and checks every file's CRC, so this is
# the first harness that exercises the ported CODECS through the ported archive
# reader. A method whose parameters are parsed wrong shows up here as a CRC
# failure rather than as a decode error, which is why the corruption self-test
# at the end matters: a checker that cannot fail would report the same "All OK".
#
# Only the methods the port can currently decode are covered. -m1 upwards nest
# their real compressor inside `4x4`, the multithreaded chunking meta-codec,
# which is still C and not yet ported -- those rows are SKIPPED and counted, not
# quietly omitted.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/arctest"

[ -x "$REF" ] || {
  echo "no reference binary at $REF -- build one with ./compile-ghc-probe" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin arctest ) || {
  echo "cargo build failed" >&2; exit 1; }
[ -x "$PORT" ] || { echo "cargo produced no $PORT" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-test-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

fail=0 checked=0 skipped=0

# The summary line is the whole verdict: file count, packed and unpacked totals,
# and the ratio -- which is integer arithmetic in the Haskell (ratio3,
# UIBase.hs:159) and truncates where a rounding formatter would not.
for m in -m0 -mtor -mppmd; do
  for s in -s -s-; do
    rm -f "$W/a.arc"
    ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" "$s" ../a.arc . ) >/dev/null 2>&1
    [ -f "$W/a.arc" ] || { echo "  SKIP [$m $s]: no archive written"; skipped=$((skipped+1)); continue; }
    checked=$((checked + 1))

    "$REF" t "$W/a.arc" 2>&1 | tr '\r' '\n' | grep '^Tested' \
      | sed 's/[[:space:]]*$//' > "$W/ref.txt"
    "$PORT" "$W/a.arc" 2>"$W/port.err" | grep '^Tested' > "$W/port.txt"
    port_rc=${PIPESTATUS[0]}

    if [ "$port_rc" -ne 0 ]; then
      echo "  DIFF [$m $s]: the port failed a good archive (rc=$port_rc)"
      head -3 "$W/port.err" | sed 's/^/      /'
      fail=$((fail + 1))
    elif ! cmp -s "$W/ref.txt" "$W/port.txt"; then
      echo "  DIFF [$m $s]"
      diff "$W/ref.txt" "$W/port.txt" | sed 's/^/      /'
      fail=$((fail + 1))
    fi
  done
done

# The methods the port cannot decode yet, named rather than skipped in silence.
for m in -m1 -m4 -m9; do
  rm -f "$W/a.arc"
  ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" ../a.arc . ) >/dev/null 2>&1
  if "$PORT" "$W/a.arc" >/dev/null 2>&1; then
    echo "  NOTE [$m] now decodes -- move it into the compared set above"
  else
    skipped=$((skipped + 1))
  fi
done

echo "arc t: $checked archives, $fail differing, $skipped not yet decodable"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the checker must be able to fail ────────────────────────────────────────
# Every row above passes if the CRC check is a no-op. Corrupt one byte of file
# data -- not of a header, which would fail the block's own CRC and prove
# nothing about the per-file check -- and require BOTH binaries to reject it.
rm -f "$W/a.arc"
( cd "$W/corpus" && "$REF" a --nodates -r -y -m0 ../a.arc . ) >/dev/null 2>&1
cp "$W/a.arc" "$W/bad.arc"
# -m0 stores, so byte 4096 is file data: past the 8-byte header block, inside
# the stored stream, and far from any descriptor.
printf '\xff' | dd of="$W/bad.arc" bs=1 seek=4096 count=1 conv=notrunc 2>/dev/null

if "$PORT" "$W/bad.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port accepted an archive with a corrupted byte" >&2
  exit 1
fi
if "$REF" t "$W/bad.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the REFERENCE accepted the corrupted archive, so the" >&2
  echo "corruption did not land where it would be checked" >&2
  exit 1
fi

echo "the Rust arc t matches the Haskell one, and both reject a corrupted archive"
