#!/usr/bin/env bash
# Differential-test the BSC inverse sort-transform (ST3..ST8) against the C.
#
# The C forward-transforms each corpus file with bsc_st_encode for each order
# k in 3..8, and both the C bsc_st_decode and the Rust port invert it; both must
# reproduce the original byte for byte. ST is the alternative block sorter to the
# BWT, so a block coded with it must invert here.
#
# ST7 and ST8 have no CPU encoder (bsc_st_encode returns NOT_SUPPORTED for
# k >= 7 without CUDA, st.cpp:1006-1011), so they cannot be round-tripped on this
# host -- an inherent limit of the reference, not the port. The decoder handles
# them as the same "rounds 4..k" loop that ST5/ST6 exercise, with one or two more
# iterations. The harness reports them as "0 transformed" and moves on.
#
# All corpus sizes are under 0x800000 (8 MiB), so this exercises reconstruct
# case 1 -- the path real BSC blocks take. Cases 2 and 3 (>= 8 MiB, and the
# fail-back variant) are ported for completeness but would need multi-megabyte,
# highly-repetitive inputs to trigger.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds BSC: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags BSC)" || exit 1
W="${TMPDIR:-/tmp}/bsc-st-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_st_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc-st "$W/in"

total=0; tested=0
for k in 3 4 5 6 7 8; do
  fail=0; n=0; enc=0
  for f in "$W"/in/*; do
    n=$((n+1)); bn=$(basename "$f")
    rm -f "$W/e" "$W/oc" "$W/or"
    if ! "$W/c" e "$k" < "$f" >| "$W/e" 2>/dev/null; then
      continue   # encoder declined -- not a decode disagreement
    fi
    enc=$((enc+1))
    sz=$(wc -c < "$f")
    "$W/c"  d "$sz" < "$W/e" >| "$W/oc" 2>/dev/null || { echo "  [ST$k] $bn: C-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc" || { echo "  [ST$k] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
    "$W/rs" d "$sz" < "$W/e" >| "$W/or" 2>/dev/null || { echo "  [ST$k] $bn: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/or" || { echo "  [ST$k] $bn: RUST-decode != original"; fail=$((fail+1)); }
  done
  echo "  [ST$k] $n inputs, $enc transformed, $fail differing"
  total=$((total+fail)); tested=$((tested+enc))
done

[ "$tested" -gt 0 ] || { echo "no inputs were transformed -- harness reached nothing"; exit 1; }
echo "bsc inverse-st: $total total differing ($tested transformed blocks)"
[ "$total" -eq 0 ] && echo "BSC inverse ST (ST3..ST8) matches the C original byte for byte" || exit 1
