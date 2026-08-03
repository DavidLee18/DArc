#!/usr/bin/env bash
# Differential-test the Dict port against the C original, BOTH directions.
#
# Dict is one of DArc's own formats, so "format-valid" does not apply: the
# encoder has to be byte-exact or archives move.
#
# ## Why this file exists
#
# `dict_ref.cpp` and `dict_phase1_ref.cpp` had been in the tree since the port
# landed, and no `dict-check.sh` ever did -- so nothing built them and the
# workflow never named `dict`. The C was deleted anyway. That is the identical
# situation LZP was found in (see the header of lzp-check.sh), except that here
# the gap survived the deletion rather than being closed before it.
#
# The C reference therefore comes from a pinned revision, not the working tree,
# which no longer has a C Dict at all -- see c-reference.sh.
#
# ## What has to be true for this to be a test at all
#
# Dict DECLINES on data it cannot compress: `DictEncode` returns non-zero, or
# the result fails the MinCompression ratio, and the block is stored verbatim.
# A declined block makes both implementations emit the same four-byte header
# plus the input, so a corpus of binary data yields a perfectly byte-identical
# comparison that never runs the dictionary builder. The coverage assertion at
# the bottom uses the driver's 'v' mode to count blocks the C actually ENGAGED
# on, and fails if too few did.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# Built the way DArc builds Dict: see darc_codec_cflags in c-reference.sh for
# why the makefile's flags, not an -O level, are the oracle.
CFLAGS_C="$(darc_codec_cflags Dict)" || exit 1
W="${TMPDIR:-/tmp}/dict-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }

# The Rust variant compiles the pinned C_Dict.cpp with -DDARC_RUST, exactly as
# production did: that excludes the C dict_compress/dict_decompress so the
# staticlib supplies them. $lib is a SEPARATE parameter placed after every
# source -- GNU ld resolves an archive only against undefineds it has already
# seen, so a staticlib listed first contributes nothing (links on macOS, fails
# on Linux).
cc() { local out="$1" lib="$2"; shift 2
  # shellcheck disable=SC2086  # the flag list is a word list on purpose
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" \
    "$CREF/rust/difftest/dict_ref.cpp" \
    "$CREF/Compression/Dict/C_Dict.cpp" \
    "$CREF/Compression/CompressionLibrary.cpp" \
    "$CREF/Compression/Common.cpp" \
    ${lib:+"$lib"} -o "$out"; }
cc "$W/c"  ""                             || { echo "C reference build failed" >&2; exit 1; }
cc "$W/rs" "$LIB" -DUSE_RUST -DDARC_RUST  || { echo "Rust driver build failed" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" dict "$W/in"

# ── the comparison ──────────────────────────────────────────────────────────
# Two axes. Block size bounds one dictionary; DICT_CHUNK bounds how much a
# single "read" may return, which is what decides where blocks actually break
# in the archiver -- dict_compress loops on read, so the codec sees a sequence
# of pipeline-sized buffers, not one whole file. dict_ref.cpp's own header
# records that returning everything in one read hid a real divergence.
total=0; tested=0
for bs in 8388608 1048576 65536; do
  for chunk in 0 262144 65536; do
    fail=0; n=0
    for f in "$W"/in/*; do
      n=$((n+1)); bn=$(basename "$f")
      rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr"
      DICT_CHUNK=$chunk "$W/c"  c "$bs" < "$f" >| "$W/ec" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C-compress FAILED"; fail=$((fail+1)); continue; }
      DICT_CHUNK=$chunk "$W/rs" c "$bs" < "$f" >| "$W/er" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-compress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$W/ec" "$W/er" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: ENCODER differs from the C"; fail=$((fail+1)); continue; }
      # Decode the C's own stream with both, and require the original back.
      DICT_CHUNK=$chunk "$W/c"  d "$bs" < "$W/ec" >| "$W/dc" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C-decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dc" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
      DICT_CHUNK=$chunk "$W/rs" d "$bs" < "$W/ec" >| "$W/dr" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dr" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-decode != original"; fail=$((fail+1)); }
      tested=$((tested+1))
    done
    echo "  [bs=$bs chunk=$chunk] $n inputs, $fail differing"
    total=$((total+fail))
  done
done

# ── coverage: did the dictionary builder ever actually run? ─────────────────
# 'v' mode replays dict_compress's read loop but calls DictEncode directly and
# prints "engaged" or "DECLINED" per block. Without this the suite could be
# perfectly green on a corpus Dict refuses outright.
engaged=0
for f in "$W"/in/natural_a "$W"/in/natural_b "$W"/in/natural_c "$W"/in/english; do
  [ -f "$f" ] || continue
  k=$(DICT_CHUNK=262144 "$W/c" v 8388608 < "$f" 2>&1 >/dev/null | grep -c 'engaged' || true)
  engaged=$((engaged + k))
done

[ "$tested" -gt 0 ] || { echo "no inputs were processed -- the harness reached nothing"; exit 1; }
[ "$engaged" -ge 8 ] || {
  echo "only $engaged blocks ever ENGAGED Dict's encoder; the rest declined and were"
  echo "stored, so the encoder comparison was comparing framing and nothing else."
  echo "Widen the corpus until the word counts clear MinLargeCnt/MinMediumCnt"
  echo "rather than deleting this check -- it is what makes the result mean anything."
  total=$((total+1)); }

echo "dict: $total total differing over $tested comparisons ($engaged blocks engaged the encoder)"
[ "$total" -eq 0 ] && echo "Dict matches the C original byte for byte, both directions" || exit 1
