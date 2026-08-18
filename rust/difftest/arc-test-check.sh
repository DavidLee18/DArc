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
# Every method DArc ships is covered. That includes the full chains the higher
# levels build -- dict+lzp+grzip, dict+lzp+ppmd, rep+exe+delta+4x4:lzma -- so a
# parameter parsed wrong anywhere in a chain surfaces here.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

. "$ROOT/rust/difftest/arc-reference.sh"
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
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
#
# Both sides go through the SAME normaliser, and it truncates after `Ratio N%`.
#
# Why: the progress indicator and the summary share one stream (stderr is empty
# -- measured, 0 bytes), and progress redraws with \r. Splitting on \r is not
# enough, because a progress update emitted after the summary with no \r of its
# own lands on the same line and is captured as part of it. This run on
# f6e9d21, after twelve consecutive green ones:
#
#   < Tested 226 files, 52.491 => 438.744 bytes. Ratio 11.9%   0:00} <= a.arc^G
#   > Tested 226 files, 52.491 => 438.744 bytes. Ratio 11.9%
#
# Identical verdicts; a terminal artifact reported as a difference in the
# program. Truncating after the ratio keeps every field the summary carries and
# drops only what follows it. A re-run of that same commit passed, which is what
# a timing-dependent capture looks like -- it will come back otherwise.
#
# The two sides are also captured identically now. They were not: the reference
# folded stderr in with 2>&1 and the port did not. That is harmless while the
# reference's stderr is empty, and it is precisely the asymmetry that would make
# the next difference in capture read as a difference in behaviour.
norm() {
  tr '\r' '\n' | grep '^Tested' \
    | sed 's/\(Ratio [0-9][0-9.]*%\).*/\1/; s/[[:space:]]*$//'
}

# ── the normaliser must not be a shredder ───────────────────────────────────
# Run BEFORE the loop, because a filter one character too greedy would reduce
# every comparison below to "All OK" == "All OK" and this harness would report
# 20 archives, 0 differing while checking nothing. Prove both directions: the
# artifact goes, and a summary differing in ANY field still differs after.
clean='Tested 226 files, 52.491 => 438.744 bytes. Ratio 11.9%'
dirty=$(printf 'Tested 226 files, 52.491 => 438.744 bytes. Ratio 11.9%%              0:00} <= a.arc\a')
[ "$(printf '%s\n' "$dirty" | norm)" = "$clean" ] || {
  echo "SELF-TEST FAILED: norm did not reduce the progress artifact to the summary" >&2
  printf '  got: [%s]\n' "$(printf '%s\n' "$dirty" | norm)" >&2
  exit 1; }
while read -r other; do
  [ "$(printf '%s\n' "$other" | norm)" != "$clean" ] || {
    echo "SELF-TEST FAILED: norm flattened a real difference: $other" >&2; exit 1; }
done <<'EOF'
Tested 225 files, 52.491 => 438.744 bytes. Ratio 11.9%
Tested 226 files, 52.490 => 438.744 bytes. Ratio 11.9%
Tested 226 files, 52.491 => 438.745 bytes. Ratio 11.9%
Tested 226 files, 52.491 => 438.744 bytes. Ratio 12.0%
EOF
echo "the summary normaliser drops the artifact and keeps every field"

for m in -m0 -m1 -m2 -m3 -m4 -m5 -m9 -mx -mtor -mppmd; do
  for s in -s -s-; do
    rm -f "$W/a.arc"
    ( cd "$W/corpus" && "$REF" a --nodates -r -y "$m" "$s" ../a.arc . ) >/dev/null 2>&1
    [ -f "$W/a.arc" ] || { echo "  SKIP [$m $s]: no archive written"; skipped=$((skipped+1)); continue; }
    checked=$((checked + 1))

    "$REF"  t "$W/a.arc" 2>"$W/ref.err"  | norm > "$W/ref.txt"
    "$PORT" t "$W/a.arc" 2>"$W/port.err" | norm > "$W/port.txt"
    port_rc=${PIPESTATUS[0]}

    if [ "$port_rc" -ne 0 ]; then
      echo "  DIFF [$m $s]: the port failed a good archive (rc=$port_rc)"
      head -3 "$W/port.err" | sed 's/^/      /'
      fail=$((fail + 1))
    # Two empty files compare equal. Without this, a reference that stopped
    # printing the summary -- or a normaliser one character too greedy -- would
    # turn every row below into a comparison of nothing with nothing, and this
    # harness would pass while checking that both binaries produce no output.
    elif [ ! -s "$W/ref.txt" ] || [ ! -s "$W/port.txt" ]; then
      echo "  DIFF [$m $s]: no 'Tested' summary from" \
           "$([ -s "$W/ref.txt" ] || printf 'the reference')" \
           "$([ -s "$W/port.txt" ] || printf 'the port')"
      fail=$((fail + 1))
    elif ! cmp -s "$W/ref.txt" "$W/port.txt"; then
      echo "  DIFF [$m $s]"
      diff "$W/ref.txt" "$W/port.txt" | sed 's/^/      /'
      fail=$((fail + 1))
    fi
  done
done

echo "arc t: $checked archives, $fail differing, $skipped skipped"
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

if "$PORT" t "$W/bad.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the port accepted an archive with a corrupted byte" >&2
  exit 1
fi
if "$REF" t "$W/bad.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the REFERENCE accepted the corrupted archive, so the" >&2
  echo "corruption did not land where it would be checked" >&2
  exit 1
fi

echo "the Rust arc t matches the Haskell one, and both reject a corrupted archive"
