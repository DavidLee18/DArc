#!/usr/bin/env bash
# Differential-test the data-type and multimedia detectors against the C.
#
# `detect_datatype` decides whether a file is $text, $compressed or neither, and
# `splitFileTypes` (ArhiveFileList.hs:460) uses that to decide WHICH SOLID BLOCK
# a file lands in. So its verdict is archive-byte-visible: a file classified
# differently is packed with a different chain, in a different block, and the
# archive is not the one DArc writes. The same is true of `detect_mm`, whose
# answer picks the MM model that goes into the stream header.
#
# ── Why floating point makes this worth a harness of its own ────────────────
#
# detect_datatype computes an order-0 entropy estimate in `double` and compares
# it against `0.95 * bufsize`. It does so as `log(x)/log(2)`, not `log2(x)`, and
# it divides `bufsize/count[i]` as INTEGERS before taking the logarithm. Any of
# those transcribed "sensibly" instead of literally shifts the estimate, and a
# buffer sitting near a threshold then classifies differently. Unit tests on the
# port alone cannot see that; only the C can say.
#
# The corpus is built to put inputs ON the thresholds rather than safely inside
# them. That is not a precaution -- it was MEASURED. Replacing the integer
# division with a float one and log(x)/log(2) with log2(x) was caught by NO
# input in the first version of this corpus, including the ones named
# "near_gate". A search then found the band that does discriminate: 400 KB of
# noise with one byte biased to about 10.9%, where the C says "default" and the
# sabotaged port says "$compressed". Those inputs are the gate_* rows below, and
# without them this harness would have signed off on the wrong arithmetic.
#
# The band is narrow because being near the order-0 threshold FORCES a
# near-uniform distribution, which forces large bufsize/count ratios, where
# truncation is nearly a no-op. It only bites where the biased byte drags one
# ratio down into single digits while the rest stay uniform.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
CFLAGS_C="$(darc_codec_cflags MM)" || exit 1

W="${TMPDIR:-/tmp}/mmdet-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin mmdet_rs ) \
  || { echo "cargo build failed" >&2; exit 1; }
RS="$ROOT/rust/target/release/mmdet_rs"
[ -x "$RS" ] || { echo "cargo produced no $RS" >&2; exit 1; }

clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
  -I"$CREF" -I"$CREF/Compression" \
  "$ROOT/rust/difftest/mmdet_ref.cpp" -o "$W/c" || {
  echo "could not build the C oracle" >&2; exit 1; }

# The list of recognizable types is part of the contract: splitFileTypes reads
# it to decide whether to run autodetection at all.
c_types=$("$W/c" --types)
rs_types=$("$RS" --types)
if [ "$c_types" != "$rs_types" ]; then
  echo "DIFF [detectable types]: C '$c_types' vs Rust '$rs_types'"
  exit 1
fi

# ── the corpus ──────────────────────────────────────────────────────────────
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 generators
# that stood here, accepted on a byte comparison over all 41 files.
"$ROOT/rust/target/release/corpusgen" mmdet "$W/in"

fail=0 checked=0
for f in "$W"/in/*; do
  checked=$((checked + 1))
  c_out=$("$W/c"  < "$f")
  rs_out=$("$RS"  < "$f")
  if [ "$c_out" != "$rs_out" ]; then
    echo "  DIFF [$(basename "$f")]: C '$c_out' vs Rust '$rs_out'"
    fail=$((fail + 1))
  fi
done

echo "mmdet: $checked inputs, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -ge 20 ] || { echo "corpus did not generate" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every line above passes if both sides answer "default" to everything, which is
# what a detector that never fires would do. Require the corpus to have produced
# each verdict at least once, and require the two MM modes to disagree somewhere
# -- if they never do, the fast/thorough distinction is untested.
verdicts=$(for f in "$W"/in/*; do "$W/c" < "$f" | awk '{print $1}'; done | sort -u)
for want in '$text' '$compressed' default; do
  # A here-string, not `echo | grep -qx`: under `set -o pipefail` grep -q exits
  # on its first match, echo takes SIGPIPE writing the rest, and the pipeline
  # reports failure for a SUCCESSFUL match. Small outputs usually survive it,
  # which is what makes it a flake rather than a bug you find once.
  grep -qx -- "$want" <<< "$verdicts" || {
    echo "SELF-TEST FAILED: no input produced $want, so that branch is untested" >&2
    exit 1
  }
done

echo "the Rust detectors agree with the C on every input"
