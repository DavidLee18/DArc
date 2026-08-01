#!/usr/bin/env bash
# Differential-test the Rust `arc a` against the Haskell one, BYTE FOR BYTE.
#
#   usage: arc-create-check.sh [reference-arc]
#
# This is the strictest bar in the port and the only one that matters for
# writing: an archive that round-trips is not good enough, because an archive
# built in a different file order, or with a directory block that compresses to
# different bytes, round-trips perfectly and is still not what DArc writes.
# Three separate bugs in this writer produced archives that read back flawlessly:
#
#   * stored names without the "./" the reference keeps (remove_unsafe_dirs
#     strips it again on READ, so both listed identically)
#   * a depth-first scan, which misorders one directory out of eight
#   * a strictly level-by-level scan, which misorders two files out of 218
#
# None of them is visible without comparing bytes.
#
# Only -m0 is written, and that is a deliberate limit rather than an unfinished
# one. Every other level gets aDEFAULT_SOLID_SORT_ORDER = "gerpn"
# (Cmdline.hs:617), which reorders the files before they are packed; -m0 is the
# single level with sorting disabled. The rest of the machinery for compressed
# levels IS verified -- see arc-canonize-check.sh and arc-fit-check.sh -- and
# -m1 was byte-identical on the generated corpus precisely because that corpus
# is already in sorted order. It was NOT on a corpus with a zero-length .bin
# among .txt files. The port refuses those levels, and this harness checks that
# it refuses.
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

W="${TMPDIR:-/tmp}/arc-create-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

# A second corpus with awkward shapes the generated one does not have: an empty
# directory, a directory whose name sorts after a file's, and a chain deep
# enough that a level-by-level walk and a depth-first one disagree.
mkdir -p "$W/shapes/zzz-dir/inner" "$W/shapes/a/b/c/d" "$W/shapes/empty"
printf 'one' > "$W/shapes/aaa-file.txt"
printf 'two' > "$W/shapes/zzz-dir/x.txt"
printf 'three' > "$W/shapes/zzz-dir/inner/y.txt"
printf 'four' > "$W/shapes/a/shallow.txt"
printf 'five' > "$W/shapes/a/b/c/d/deep.txt"
: > "$W/shapes/a/b/zero-length.bin"

fail=0 checked=0

for corpus in corpus shapes; do
  for m in -m0; do
    for extra in "" "-s-"; do
      checked=$((checked + 1))
      rm -f "$W/ref.arc" "$W/port.arc"
      # --nodates: without it the archive embeds mtimes and the two runs would
      # differ for a reason that is not the port's.
      ( cd "$W/$corpus" && "$REF"  a --nodates -r -y "$m" $extra "$W/ref.arc"  . ) >/dev/null 2>&1
      ( cd "$W/$corpus" && "$PORT" a --nodates -r -y "$m" $extra "$W/port.arc" . ) >/dev/null 2>&1

      if [ ! -f "$W/port.arc" ]; then
        echo "  DIFF [$corpus $m $extra]: the port wrote no archive"
        fail=$((fail + 1))
        continue
      fi
      if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
        echo "  DIFF [$corpus $m $extra]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
        cmp -l "$W/ref.arc" "$W/port.arc" 2>/dev/null | head -3 | sed 's/^/      /'
        fail=$((fail + 1))
      fi
    done
  done
done

echo "arc a: $checked archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# `cmp` over two files the port never wrote would report nothing. Prove the
# archive is real by reading it back with the REFERENCE, and prove a one-byte
# change is caught.
if ! "$REF" t "$W/port.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference cannot test an archive the port wrote" >&2
  exit 1
fi
cp "$W/port.arc" "$W/tweak.arc"
printf '\xff' | dd of="$W/tweak.arc" bs=1 seek=100 count=1 conv=notrunc 2>/dev/null
if cmp -s "$W/ref.arc" "$W/tweak.arc"; then
  echo "SELF-TEST FAILED: a changed byte was not caught by cmp" >&2
  exit 1
fi

# The methods the port cannot write must be REFUSED, not written wrong. An
# archive that decodes correctly but is not the reference's bytes is the
# failure this repo cares most about, so silence here would be worse than an
# error.
for m in -m1 -m2 -m4 -m9 -mx -mtor; do
  rm -f "$W/nope.arc"
  if ( cd "$W/corpus" && "$PORT" a --nodates -r -y "$m" "$W/nope.arc" . ) >/dev/null 2>&1; then
    echo "SELF-TEST FAILED: the port claimed to write $m, which it cannot yet" >&2
    exit 1
  fi
done

echo "the Rust arc a writes byte-identical archives, and refuses what it cannot write"
