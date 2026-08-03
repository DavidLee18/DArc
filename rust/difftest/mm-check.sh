#!/usr/bin/env bash
# Differential-test the MM port -- BOTH directions -- against the C original.
#
# Over a matrix of channel counts, word sizes, header offsets and detector
# modes: filter each input with BOTH encoders and require the two streams to be
# identical, then unfilter with both decoders and require both to reproduce the
# original. Byte-for-byte equality is the bar in both directions because MM
# defines an archive format -- for the encoder that means the autodetector must
# pick the same model, since its choice travels in the stream header.
#
# Inputs are deliberately larger than 1 MB: mm_compress reads a first block of
# up to 1 MB and then switches to 64 KB blocks, while the decoder always reads
# in 64 KB chunks, so anything smaller never exercises a running sum crossing a
# block boundary -- the one piece of state that spans the whole stream.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds MM: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags MM)" || exit 1
W="${TMPDIR:-/tmp}/mm-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land AFTER the sources, because the Rust staticlib has to:
# GNU ld resolves an archive against only the objects already seen on the
# command line, so a library placed first is silently dropped and every symbol
# comes back undefined. macOS ld does not care.
cc() { # cc <output> [args appended after the sources]
  local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/mm_ref.cpp" "$CREF/rust/difftest/mm_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"
}
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs: multimedia-shaped data at every sample width the filter implements
# (8/16/24/32-bit, mono through four channels), plus a real .wav so the WAV
# header detector fires and produces a nonzero offset, plus data the detectors
# should refuse (noise, text) so the "stored" branch of the decoder is reached.
# Sizes straddle the 1 MB first-block boundary and leave partial samples at the
# end, which the filter must pass through untouched.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" mm "$W/in"

stored=0 filtered=0          # which decoder branch each stream actually took

decode_case () {   # $1=tag  $2..=encoder args (MODE SKIPHDR ISFLOAT NUMCHAN WORDSIZE OFFSET)
  local tag="$1"; shift
  local ec="$*"
  local f fail=0 n=0
  for f in "$W"/in/*; do
    n=$((n+1)); local name; name=$(basename "$f")
    "$W/c"  c $ec < "$f"        >| "$W/stream" 2>/dev/null || { echo "  [$tag] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    # The encoder is held to BYTE equality with the C, not merely to producing
    # something decodable: MM is one of DArc's own formats, so a stream that
    # differs is a different archive. This is also the only check that reaches
    # the autodetector's model-scoring arithmetic, which decides the header.
    "$W/rs" c $ec < "$f"        >| "$W/stream_rs" 2>/dev/null || { echo "  [$tag] $name: RUST-compress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$W/stream" "$W/stream_rs" || { echo "  [$tag] $name: RUST-encode != C-encode"; fail=$((fail+1)); continue; }
    "$W/c"  d     < "$W/stream" >| "$W/oc"     2>/dev/null || { echo "  [$tag] $name: C-decode FAILED";   fail=$((fail+1)); continue; }
    "$W/rs" d     < "$W/stream" >| "$W/ors"    2>/dev/null || { echo "  [$tag] $name: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc"  || { echo "  [$tag] $name: C-decode != original (harness bug)"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/ors" || { echo "  [$tag] $name: RUST-decode != original"; fail=$((fail+1)); continue; }
    # Flags byte: 0 = autodetection gave up and the payload is stored, 1 = filtered.
    if [ "$(head -c1 "$W/stream" | od -An -tu1 | tr -d ' ')" = 0 ]
      then stored=$((stored+1)); else filtered=$((filtered+1)); fi
  done
  echo "  [$tag] $n inputs, $fail differing"
  return $fail
}

total=0
#            tag          MODE SKIP FLOAT CHAN WORD OFFSET
# Mode 9 is the archiver's default and runs the full {8,16,24,32} model set;
# mode 1 uses only {8,16} over a smaller sample, so the two reach different
# detectors and both are worth covering. Mode 9 spent this codec's entire life
# crashing -- Model::_32bit_run / _32bit_diff_run walked the buffer with a
# `long *`, 64-bit on LP64, reading pairs of samples as one and slotting a
# value up to 2^63>>24 into a 1024-entry stats row. Fixed in mmdet.cpp, so
# everything reaching autodetection below is coverage that was unreachable.
decode_case "auto d9"        9 0 0 0 0 0  ; total=$((total+$?))
decode_case "auto d9 no-hdr" 9 1 0 0 0 0  ; total=$((total+$?))
decode_case "auto d1"        1 0 0 0 0 0  ; total=$((total+$?))
decode_case "auto d1 no-hdr" 1 1 0 0 0 0  ; total=$((total+$?))
decode_case "1*8"            9 0 0 1 8  0 ; total=$((total+$?))
decode_case "3*8"            9 0 0 3 8  0 ; total=$((total+$?))
decode_case "2*16"           9 0 0 2 16 0 ; total=$((total+$?))
decode_case "1*24"           9 0 0 1 24 0 ; total=$((total+$?))
decode_case "2*24"           9 0 0 2 24 0 ; total=$((total+$?))
decode_case "2*32"           9 0 0 2 32 0 ; total=$((total+$?))
decode_case "1*32f"          9 0 1 1 32 0 ; total=$((total+$?))
decode_case "2*16 off7"      9 0 0 2 16 7 ; total=$((total+$?))
decode_case "2*16 off44"     9 0 0 2 16 44; total=$((total+$?))
decode_case "3*8 off1"       9 0 0 3 8  1 ; total=$((total+$?))
decode_case "4*16 off13"     9 0 0 4 16 13; total=$((total+$?))

echo "mm: $total total differing ($filtered filtered streams, $stored stored)"
[ "$stored"   -gt 0 ] || { echo "corpus never reached the stored branch"; exit 1; }
[ "$filtered" -gt 0 ] || { echo "corpus never reached the filtered branch"; exit 1; }
[ "$total"    -eq 0 ] && echo "MM matches the C original byte for byte, both directions" || exit 1
