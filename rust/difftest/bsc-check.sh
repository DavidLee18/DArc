#!/usr/bin/env bash
# Differential-test the BSC QLFC decoders against the C original.
#
# Cuts at the QLFC CODER level, not the whole BSC codec: the C encodes a raw
# buffer with bsc_coder_encode_block and both sides decode it. No BWT, ST, LZP
# or block header is involved, so a failure points squarely at the range coder,
# the mixer, the model, or a decode body -- the ~1,500 lines of error-prone
# entropy code -- rather than at four interacting stages. This is done BEFORE
# the rest of BSC is wired, so the foundation is proven before more is built on
# it (the ordering that worked for GRZip).
#
# Coder 1 = QLFC static (libbsc default), 2 = adaptive, 3 = fast (Model2, no
# mixer). All three are ported.
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
W="${TMPDIR:-/tmp}/bsc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# libsais.c must be its own translation unit -- see bsc_ccodec.cpp. Trailing
# args land after the sources so the Rust staticlib links on GNU ld.
cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# QLFC codes rank/run-length pairs, so its models are exercised by the SHAPE of
# the byte distribution: skewed (text), flat (noise), long runs, a single byte,
# a large alphabet, and the awkward tiny sizes. The encoder codes whatever bytes
# it is given, so raw inputs are fine even though in the real pipeline QLFC
# follows BWT+MTF.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc that
# stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc "$W/in"

total=0; tested=0
for coder in 1 2 3; do
  name=$([ "$coder" = 1 ] && echo static || { [ "$coder" = 2 ] && echo adaptive || echo fast; })
  fail=0; n=0; enc=0
  for f in "$W"/in/*; do
    n=$((n+1)); bn=$(basename "$f")
    rm -f "$W/q" "$W/oc" "$W/or"
    if ! "$W/c" c "$coder" < "$f" >| "$W/q" 2>/dev/null; then
      # Encoder declined this input (return < 0) -- not a decode disagreement.
      continue
    fi
    enc=$((enc+1))
    sz=$(( $(wc -c < "$f") + 4096 ))
    "$W/c"  d "$coder" "$sz" < "$W/q" >| "$W/oc" 2>/dev/null || { echo "  [$name] $bn: C-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc" || { echo "  [$name] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
    "$W/rs" d "$coder" "$sz" < "$W/q" >| "$W/or" 2>/dev/null || { echo "  [$name] $bn: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/or" || { echo "  [$name] $bn: RUST-decode != original"; fail=$((fail+1)); }
  done
  echo "  [$name] $n inputs, $enc coded, $fail differing"
  total=$((total+fail)); tested=$((tested+enc))
done

[ "$tested" -gt 0 ] || { echo "no inputs were coded -- harness reached nothing"; exit 1; }
echo "bsc qlfc decode: $total total differing ($tested coded blocks)"
[ "$total" -eq 0 ] && echo "BSC QLFC (static + adaptive + fast) matches the C original byte for byte" || exit 1
