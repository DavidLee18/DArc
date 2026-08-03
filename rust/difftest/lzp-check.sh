#!/usr/bin/env bash
# Differential-test the LZP port against the C original, BOTH directions.
#
# LZP is one of DArc's own formats, so "format-valid" does not apply: the
# encoder has to be byte-exact or archives move. Both directions are ported,
# which is what licenses deleting the C -- and this harness is what backs that
# claim up.
#
# ## Why this file had to be written before the C could go
#
# `lzp_ref.cpp` has been in the tree since the port landed, but no `lzp-check.sh`
# ever existed and CI never invoked it: `run.sh` covers Delta only. So LZP's port
# was, in the repo's own words, documentation rather than a check -- while the
# C_LZP.cpp comment claimed it was "verified byte-identical over 8 inputs in both
# directions; see rust/difftest". Deleting the C on the strength of that comment
# would have removed the only executable oracle for an unverified port.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh. That matters more here than anywhere else: the working tree no
# longer HAS a C LZP.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds LZP: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags LZP)" || exit 1
W="${TMPDIR:-/tmp}/lzp-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# The Rust variant compiles the pinned C_LZP.cpp with -DDARC_RUST, exactly as
# production does: that excludes the C lzp_compress/lzp_decompress so the Rust
# staticlib supplies them instead. Without it the two definitions collide --
# the drop-ins are no longer feature-gated, now that the C is deleted.
#
# $lib is a SEPARATE parameter, placed after every source, and is not folded
# into "$@" with the -D flags. GNU ld resolves an archive only against the
# undefined symbols it has already seen, so a staticlib listed before the
# sources contributes nothing -- it links on macOS and fails on Linux with
# "undefined reference". Passing it through "$@" did exactly that and only CI
# caught it.
cc() { local out="$1" lib="$2"; shift 2
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" \
    "$CREF/rust/difftest/lzp_ref.cpp" \
    "$CREF/Compression/LZP/C_LZP.cpp" \
    "$CREF/Compression/CompressionLibrary.cpp" \
    "$CREF/Compression/Common.cpp" \
    ${lib:+"$lib"} -o "$out"; }
cc "$W/c"  ""                              || { echo "C reference build failed" >&2; exit 1; }
cc "$W/rs" "$LIB" -DUSE_RUST -DDARC_RUST   || { echo "Rust driver build failed" >&2; exit 1; }

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" lzp "$W/in"

total=0; tested=0
# Block size is a real axis: it bounds the hash table and forces the codec to
# restart per block, so a port that mishandles block boundaries only shows up
# when a file spans several.
for bs in 8388608 1048576 65536 16384; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); bn=$(basename "$f")
    rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr"
    "$W/c"  c "$bs" < "$f" >| "$W/ec" 2>/dev/null || { echo "  [bs=$bs] $bn: C-compress FAILED"; fail=$((fail+1)); continue; }
    "$W/rs" c "$bs" < "$f" >| "$W/er" 2>/dev/null || { echo "  [bs=$bs] $bn: RUST-compress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$W/ec" "$W/er" || { echo "  [bs=$bs] $bn: ENCODER differs from the C"; fail=$((fail+1)); continue; }
    # Decode the C's own stream with both, and require the original back.
    "$W/c"  d "$bs" < "$W/ec" >| "$W/dc" 2>/dev/null || { echo "  [bs=$bs] $bn: C-decompress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/dc" || { echo "  [bs=$bs] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
    "$W/rs" d "$bs" < "$W/ec" >| "$W/dr" 2>/dev/null || { echo "  [bs=$bs] $bn: RUST-decompress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/dr" || { echo "  [bs=$bs] $bn: RUST-decode != original"; fail=$((fail+1)); }
    tested=$((tested+1))
  done
  echo "  [bs=$bs] $n inputs, $fail differing"
  total=$((total+fail))
done

[ "$tested" -gt 0 ] || { echo "no inputs were processed -- harness reached nothing"; exit 1; }
echo "lzp: $total total differing over $tested comparisons"
[ "$total" -eq 0 ] && echo "LZP matches the C original byte for byte, both directions" || exit 1
