#!/usr/bin/env bash
# Differential-test the GRZip decoder port against the C original.
#
# This works at the BLOCK level rather than the stream level, because
# GRZip_DecompressBlock is where every ported stage actually runs; the stream
# wrapper around it only splits and reassembles blocks, and grzip-stage-check.sh
# covers that seam.
#
# The header used to say the stream wrapper was "not ported yet" and that the C
# compressor was the only encoder. Both directions have since been ported and
# the C for both is now deleted -- and that stale sentence is part of why the C
# survived as long as it did, so it is worth keeping this one current.
#
# The mode word selects the pipeline: bit1 picks ST4 over BWT, bit2 picks the
# MTF arithmetic coder over WFC, and the upper bits carry LZP parameters. All
# four transform/coder combinations are covered, with LZP off and on.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds GRZip: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags GRZip)" || exit 1
W="${TMPDIR:-/tmp}/grzip-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land after the sources: GNU ld resolves an archive against
# only the objects already seen, so a library placed first is silently dropped.
cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/grzip_ref.cpp" "$CREF/rust/difftest/grzip_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs for both the transforms and the entropy stage: text (skewed statistics,
# long MTF runs), records of fixed-width numbers (the Mode==-2 recursive path
# and the delta de-interleaves), runs (the RLE ladder), noise (literal-heavy,
# and where the encoder gives up and stores), and sizes around the awkward
# boundaries. Everything stays UNDER GRZ_MaxBlockSize (8 MB - 512): this is a
# block-level harness, and GRZip_CompressBlock cannot take more than one block.
# Block splitting lives in the stream wrapper, which is not ported yet -- when
# it is, the stream-level harness needs an input past that size.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" grzip "$W/in"

total=0
for mode in 0 2 4 6 0x100 0x102 0x104 0x106 0x50104 0x50100; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); name=$(basename "$f")
    sz=$(( $(wc -c < "$f") * 2 + 1048576 ))
    "$W/c"  c "$mode" < "$f"   >| "$W/s"  2>/dev/null || { echo "  [$mode] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    # The block driver is ported too, so the STREAM must match, not just the
    # round-trip. Without this the Rust compressor is never invoked at all and
    # the run is green regardless of what it would have produced.
    "$W/rs" c "$mode" < "$f"   >| "$W/s_rs" 2>/dev/null || { echo "  [$mode] $name: RUST-compress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$W/s" "$W/s_rs" || { echo "  [$mode] $name: RUST-encode != C-encode"; fail=$((fail+1)); continue; }
    "$W/c"  d "$sz"   < "$W/s" >| "$W/oc" 2>/dev/null || { echo "  [$mode] $name: C-decode FAILED";   fail=$((fail+1)); continue; }
    "$W/rs" d "$sz"   < "$W/s" >| "$W/or" 2>/dev/null || { echo "  [$mode] $name: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc" || { echo "  [$mode] $name: C-decode != original (harness bug)"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/or" || { echo "  [$mode] $name: RUST-decode != original"; fail=$((fail+1)); continue; }
  done
  echo "  [mode $mode] $n inputs, $fail differing"
  total=$((total+fail))
done

# Stream level. Only this layer splits input into blocks, so it is the only
# place an input past GRZ_MaxBlockSize (8 MB - 512) means anything -- the block
# harness above physically cannot take one. Per the Tornado lesson, a corpus
# that fits inside one block leaves the whole splitting path untested.
"$ROOT/rust/target/release/corpusgen" grzip-big > "$W/big"
sfail=0; sn=0
for f in "$W"/in/* "$W/big"; do
  sn=$((sn+1)); name=$(basename "$f")
  "$W/c"  sc < "$f"    >| "$W/ss" 2>/dev/null || { echo "  [stream] $name: C-compress FAILED"; sfail=$((sfail+1)); continue; }
  # Same defect as the block section had: without this the Rust stream
  # compressor is never invoked and the run is green whatever it would emit.
  "$W/rs" sc < "$f"    >| "$W/ss_rs" 2>/dev/null || { echo "  [stream] $name: RUST-compress FAILED"; sfail=$((sfail+1)); continue; }
  cmp -s "$W/ss" "$W/ss_rs" || { echo "  [stream] $name: RUST-encode != C-encode"; sfail=$((sfail+1)); continue; }
  "$W/c"  sd < "$W/ss" >| "$W/sc" 2>/dev/null || { echo "  [stream] $name: C-decode FAILED";   sfail=$((sfail+1)); continue; }
  "$W/rs" sd < "$W/ss" >| "$W/sr" 2>/dev/null || { echo "  [stream] $name: RUST-decode FAILED"; sfail=$((sfail+1)); continue; }
  cmp -s "$f" "$W/sc" || { echo "  [stream] $name: C-decode != original (harness bug)"; sfail=$((sfail+1)); continue; }
  cmp -s "$f" "$W/sr" || { echo "  [stream] $name: RUST-decode != original"; sfail=$((sfail+1)); continue; }
done
echo "  [stream, multi-block] $sn inputs, $sfail differing"
total=$((total+sfail))

echo "grzip decode: $total total differing"
[ "$total" -eq 0 ] && echo "GRZip decoder matches the C original byte for byte" || exit 1
