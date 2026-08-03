#!/usr/bin/env bash
# Differential-test the BSC inverse BWT against the C original.
#
# The C forward-transforms each corpus file with libsais (bsc_bwt_encode), and
# both the C bsc_bwt_decode and the Rust port invert it; both must reproduce the
# original byte for byte. This sits ABOVE the already-verified QLFC entropy core
# in the decode pipeline, and is verified before the block dispatcher and the
# inverse LZP are wired -- the same "prove each stage, then build on it" ordering
# that worked for GRZip and for QLFC.
#
# The corpus deliberately straddles 64 KiB: below it the format carries a single
# index and the decoder walks r == n; at/above it the format carries auxiliary
# checkpoints and the decoder walks r = mod+1 strided regions. Both are on the
# decode surface, so both must be exercised (the "path never reached" trap).
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
W="${TMPDIR:-/tmp}/bsc-bwt-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# libsais.c is its own translation unit (see bsc_ccodec.cpp). The Rust staticlib
# trails the sources so it links on GNU ld.
cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_bwt_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# The inverse BWT is exercised by the RUN structure of the data (the fastbits
# scan and the PSI follow depend on the bigram distribution): text, long runs, a
# single byte, two bytes, flat noise, a full alphabet, sparse. Sizes span the
# 64 KiB aux-index boundary so both the single-index and strided paths run.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc-bwt "$W/in"

fail=0; n=0; enc=0
for f in "$W"/in/*; do
  n=$((n+1)); bn=$(basename "$f")
  rm -f "$W/e" "$W/oc" "$W/or"
  if ! "$W/c" e < "$f" >| "$W/e" 2>/dev/null; then
    continue   # encoder declined (e.g. n<=1) -- not a decode disagreement
  fi
  enc=$((enc+1))
  sz=$(wc -c < "$f")
  "$W/c"  d "$sz" < "$W/e" >| "$W/oc" 2>/dev/null || { echo "  $bn: C-decode FAILED"; fail=$((fail+1)); continue; }
  cmp -s "$f" "$W/oc" || { echo "  $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
  "$W/rs" d "$sz" < "$W/e" >| "$W/or" 2>/dev/null || { echo "  $bn: RUST-decode FAILED"; fail=$((fail+1)); continue; }
  cmp -s "$f" "$W/or" || { echo "  $bn: RUST-decode != original"; fail=$((fail+1)); }
done

echo "bsc inverse-bwt: $n inputs, $enc transformed, $fail differing"
[ "$enc" -gt 0 ] || { echo "no inputs were transformed -- harness reached nothing"; exit 1; }
[ "$fail" -eq 0 ] && echo "BSC inverse BWT matches the C original byte for byte" || exit 1
