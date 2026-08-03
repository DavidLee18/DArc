#!/usr/bin/env bash
# Differential-test the QLFC STATIC ENCODER against the C original.
#
# Byte-identity, not round-tripping: this produces the entropy-coded payload of
# every -mbsc block, so an encoding that merely decodes correctly would still
# change every archive. `bsc_coder_encode_block` is the oracle.
#
# Covers all three coders: static (LIBBSC_DEFAULT_CODER, so the body a default
# -mbsc archive goes through), adaptive, and fast. They are three different
# models, not three settings of one -- fast in particular uses a different
# range-coder precision per field, P = 13 for rank and P = 11 for run.
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
W="${TMPDIR:-/tmp}/bsc-qlfc-enc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# The encoder is driven by the rank and run distributions the transform hands
# it, so the corpus spans what changes those: text, long runs, incompressible
# noise (which pushes the average rank past 32 and reaches the escape path), a
# single repeated byte, sorted data (what a block sorter actually produces), a
# full alphabet, and sizes down to a few hundred bytes.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc-qlfc-encode "$W/in"

fail=0; tested=0; declined=0
for coder in 1 2 3; do
for f in "$W"/in/*; do
  bn="$(basename "$f") coder$coder"
  "$W/c"  c "$coder" < "$f" >| "$W/oc" 2>/dev/null; rc_c=$?
  "$W/rs" c "$coder" < "$f" >| "$W/or" 2>/dev/null; rc_r=$?
  # 6 = the encoder declined the block ("not compressible"). Both sides must
  # make the same call: one coding what the other refuses is a difference, and
  # a driver that wrote nothing on refusal would hide it behind two empty files.
  if [ "$rc_c" -ne "$rc_r" ]; then
    echo "  $bn: C exited $rc_c, Rust $rc_r -- they disagree about whether to code it"
    fail=$((fail+1)); continue
  fi
  if [ "$rc_c" -eq 6 ]; then declined=$((declined+1)); continue; fi
  if [ "$rc_c" -ne 0 ]; then echo "  $bn: both drivers failed with $rc_c"; fail=$((fail+1)); continue; fi
  tested=$((tested+1))
  cmp -s "$W/oc" "$W/or" || { echo "  $bn: coded block differs from the C"; fail=$((fail+1)); }
done
done

# --- The coder LAYER: block splitting plus framing -------------------------
# bsc_coder_compress is what bsc_compress actually calls. Its splitter is NOT an
# even division -- it samples every 32nd byte, counts how often the sample
# changes, and cuts so that blocks carry equal amounts of variation.
#
# That path only runs above 2*2*65536 bytes, so the two inputs below are sized
# to force 2 and 4 blocks. Without them every case takes the single-block
# shortcut and the splitter is never executed at all -- which is exactly what
# the first run of this harness did.

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" bsc-qlfc-encode-big "$W/big"

layer=0; multi=0
for coder in 1 2 3; do
  for f in "$W"/big/*; do
    bn="$(basename "$f") coder$coder"
    "$W/c"  C "$coder" < "$f" >| "$W/oc" 2>/dev/null; rc_c=$?
    "$W/rs" C "$coder" < "$f" >| "$W/or" 2>/dev/null; rc_r=$?
    if [ "$rc_c" -ne "$rc_r" ]; then
      echo "  $bn: C exited $rc_c, Rust $rc_r"; fail=$((fail+1)); continue
    fi
    [ "$rc_c" -eq 0 ] || continue
    layer=$((layer+1))
    cmp -s "$W/oc" "$W/or" || { echo "  $bn: coder layer differs from the C"; fail=$((fail+1)); }
    # output[0] is the block count; anything above 1 means the splitter ran.
    nb=$(od -An -N1 -tu1 < "$W/oc" | tr -d ' ')
    [ "${nb:-1}" -gt 1 ] && multi=$((multi+1))
  done
done
[ "$multi" -ge 3 ] || {
  echo "the multi-block splitter was reached $multi times; the large inputs are not"
  echo "large enough, and split_blocks is going untested"; fail=$((fail+1)); }

# Byte-identity already implies the blocks decode, but a harness that only ever
# compared two empty outputs would pass too. Require real payloads.
big=0
for f in "$W"/in/text "$W"/in/sorted "$W"/in/noise; do
  "$W/c" c 1 < "$f" >| "$W/oc" 2>/dev/null || continue
  [ "$(wc -c < "$W/oc")" -gt 100 ] && big=$((big+1))
done

[ "$tested" -gt 0 ] || { echo "no blocks were coded -- the harness reached nothing"; exit 1; }
[ "$big" -ge 3 ] || { echo "only $big of 3 inputs produced a real coded block"; fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "bsc-qlfc-encode: $fail failures"; exit 1; }
echo "bsc-qlfc-encode: $tested/$tested blocks + $layer coder-layer cases byte-identical to the C"
echo "bsc-qlfc-encode: (all three coders; $declined declined by both; $multi multi-block)"
