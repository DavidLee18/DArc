#!/usr/bin/env bash
# Differential-test the BSC forward sort-transform (ST3) against the C.
#
# ST is the alternative to the BWT as BSC's block sorter, selected by
# -mbsc:b3..b6. All four orders are covered here. ST7 and ST8 have no CPU
# encoder in the C either -- they return NOT_SUPPORTED without CUDA.
#
# Byte-identity on BOTH outputs -- the primary index and the transformed bytes.
# The index alone decides where the decoder starts unwinding, so a port that
# permuted correctly but returned the wrong index would produce blocks that
# decode to garbage while looking plausible here.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds BSC: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags BSC)" || exit 1
W="${TMPDIR:-/tmp}/bsc-st-enc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_st_enc_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || { echo "C reference build failed" >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "Rust driver build failed" >&2; exit 1; }
# A build that silently produced nothing would leave both sides writing empty
# files, and `cmp` calls two empty files equal. Caught exactly that way once.
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver binary is missing" >&2; exit 1; }

# ST3 sorts rotations by their leading 3 bytes, so what matters is the bigram
# and trigram distribution: text, flat noise, a single symbol, long runs, sorted
# data, the full alphabet, and lengths down to the n <= 1 early return.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc-st-encode "$W/in"

fail=0; tested=0
for k in 3 4 5 6; do
for f in "$W"/in/*; do
  bn="$(basename "$f") ST$k"
  "$W/c"  "$k" < "$f" >| "$W/oc" 2>/dev/null || { echo "  $bn: C driver failed";    fail=$((fail+1)); continue; }
  "$W/rs" "$k" < "$f" >| "$W/or" 2>/dev/null || { echo "  $bn: Rust driver failed"; fail=$((fail+1)); continue; }
  [ -s "$W/oc" ] || { echo "  $bn: the C produced no output"; fail=$((fail+1)); continue; }
  tested=$((tested+1))
  cmp -s "$W/oc" "$W/or" || { echo "  $bn: transform or index differs from the C"; fail=$((fail+1)); }
done
done

[ "$tested" -gt 0 ] || { echo "nothing was transformed -- the harness reached nothing"; exit 1; }
[ "$fail" -eq 0 ] || { echo "bsc-st-encode: $fail failures"; exit 1; }
echo "bsc-st-encode: $tested/$tested byte-identical to the C (ST3..ST6)"
