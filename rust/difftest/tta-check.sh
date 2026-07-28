#!/usr/bin/env bash
# Differential-test the TTA port -- BOTH directions -- against the C original.
#
# Over a matrix of channel counts / word sizes / levels: compress each input
# with BOTH encoders and require the two streams to be identical, then decompress
# with both decoders and require both to reproduce the original. Byte-for-byte
# equality is the bar in both directions because TTA defines an archive format --
# and for the encoder that means autodetection must choose the same model, since
# its choice travels in the stream header.
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
W="${TMPDIR:-/tmp}/tta-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land AFTER the sources, because the Rust staticlib has to:
# GNU ld resolves an archive against only the objects already seen on the
# command line, so a library placed first is silently dropped and every symbol
# comes back undefined. macOS ld does not care, which is how this script passed
# locally while never having linked on Linux.
cc() { # cc <output> [args appended after the sources]
  local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/tta_ref.cpp" "$CREF/rust/difftest/tta_ccodec.cpp" \
    "$CREF/rust/difftest/mmdet_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"
}
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs: synthetic PCM-like signals -- sines, chords, ramps, noise, silence and
# near-silence -- built at raw byte level. The audio filters and the adaptive
# Rice coder behave very differently across loud/quiet and smooth/noisy input,
# so the corpus spans those. Sizes straddle the 1<<18-sample frame boundary too.
python3 - "$W/in" <<'PY'
import os,sys,math,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
def s16(vals): return b"".join(struct.pack("<h", max(-32768,min(32767,int(v)))) for v in vals)
N=100000
w("sine16_stereo",  s16([ v for i in range(N) for v in (int(20000*math.sin(i*0.03)), int(15000*math.sin(i*0.041))) ]))
w("chord16_stereo", s16([ v for i in range(N) for v in
                        (int(8000*math.sin(i*0.02)+6000*math.sin(i*0.05)),
                         int(7000*math.sin(i*0.03)+5000*math.sin(i*0.07))) ]))
w("quiet16_stereo", s16([ v for i in range(N) for v in (int(30*math.sin(i*0.03)), int(25*math.sin(i*0.05))) ]))
w("ramp16_mono",  s16([ (i%2000)-1000 for i in range(2*N) ]))
w("silence16",    b"\x00\x00"*(2*N))
w("sine8_mono",   bytes([ (128+int(100*math.sin(i*0.05)))&0xff for i in range(2*N) ]))
w("noise8",       prng(9, 2*N))
for n in (0,1,3,4,8, 1<<18, (1<<18)+1, (1<<18)-1):
    w(f"n16_{n}", s16([ int(9000*math.sin(i*0.03)) for i in range(n) ]))
# A table of ascending 32-bit little-endian integers -- NOT audio, and the shape
# that separates TTA's candidate model set from MM's.
#
# TTA has its own file-static channels[]={1,2} / bitvalues[]={8,16} in tta.cpp;
# mmdet.cpp has same-named statics holding {1,2,3,4} / {8,16,24,32}. Both call
# autodetect_by_entropy, so the set it sees depends only on the calling
# translation unit. Audio-shaped input scores about the same under either, which
# is why nothing above distinguishes them. On this input the wide set picks
# 1 channel x 32 bits, which TTA REFUSES (byte_size >= 4, not float) and stores;
# the narrow set picks 2 x 16 and compresses it ~6.7x. Passing the wrong array
# stored 32 KB that the C reduced to 4,763 bytes -- and every case above stayed
# green while it did.
w("table32", b"".join(struct.pack("<I", i*3) for i in range(8000)))
w("table32_wide", b"".join(struct.pack("<I", (i*2654435761)&0xffff) for i in range(8000)))
PY

decode_case () {   # $1=tag  $2..=encoder args ("LEVEL NUMCHAN WORDSIZE ISFLOAT [RAW]")
  local tag="$1"; shift
  local ec="$*"
  local f fail=0 n=0
  for f in "$W"/in/*; do
    n=$((n+1)); local name; name=$(basename "$f")
    "$W/c"  c $ec < "$f"        >| "$W/stream" 2>/dev/null || { echo "  [$tag] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    # Byte equality of the STREAM, not just a successful round-trip: TTA is one
    # of DArc's own formats, so an encoder that differs writes a different
    # archive. This is also the only check that reaches the encode halves of
    # entropy.cpp (the Rice coder's two adaptive parameter tracks) and
    # filters.cpp, whose compress path differs from decompress in three places
    # that are individually silent if transposed.
    "$W/rs" c $ec < "$f"        >| "$W/stream_rs" 2>/dev/null || { echo "  [$tag] $name: RUST-compress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$W/stream" "$W/stream_rs" || { echo "  [$tag] $name: RUST-encode != C-encode"; fail=$((fail+1)); continue; }
    # An EMPTY input under autodetection yields an EMPTY stream: the encoder
    # reads for detection, hits EOF, and returns before writing even the flags
    # word. There is no stream to decode, and no decoder accepts one -- the same
    # property mm-check.sh documents for MM. Both encoders agreeing that the
    # output is nothing is the whole of what can be checked here.
    if [ ! -s "$W/stream" ]; then
      [ -s "$f" ] && { echo "  [$tag] $name: empty stream for a NON-empty input"; fail=$((fail+1)); }
      continue
    fi
    "$W/c"  d     < "$W/stream" >| "$W/oc"     2>/dev/null || { echo "  [$tag] $name: C-decode FAILED";   fail=$((fail+1)); continue; }
    "$W/rs" d     < "$W/stream" >| "$W/ors"    2>/dev/null || { echo "  [$tag] $name: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc"  || { echo "  [$tag] $name: C-decode != original (harness bug)"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/ors" || { echo "  [$tag] $name: RUST-decode != original"; fail=$((fail+1)); continue; }
  done
  echo "  [$tag] $n inputs, $fail differing"
  return $fail
}

total=0
decode_case "L3 2*16"  3 2 16 0; total=$((total+$?))
decode_case "L1 2*16"  1 2 16 0; total=$((total+$?))
decode_case "L2 2*16"  2 2 16 0; total=$((total+$?))
decode_case "L3 1*16"  3 1 16 0; total=$((total+$?))
decode_case "L3 1*8"   3 1 8  0; total=$((total+$?))
decode_case "L3 2*8"   3 2 8  0; total=$((total+$?))
decode_case "raw 2*16" 3 2 16 0 1; total=$((total+$?))
# Every case above pins num_chan and word_size, which means autodetection --
# the branch that decides the stream header -- was never reached, nor was the
# storing path, nor 24-bit, nor float. Those are the encoder's remaining arms.
decode_case "auto"     3 0 0  0; total=$((total+$?))
decode_case "auto L1"  1 0 0  0; total=$((total+$?))
# level 0 goes straight to storing without reading anything for detection.
decode_case "L0"       0 0 0  0; total=$((total+$?))
decode_case "L3 1*24"  3 1 24 0; total=$((total+$?))
decode_case "L3 2*24"  3 2 24 0; total=$((total+$?))
# is_float with a 32-bit word is the only float geometry TTA accepts; every
# other combination falls through to storing, which is itself worth covering.
decode_case "float32"  3 1 32 1; total=$((total+$?))
decode_case "float-bad" 3 1 16 1; total=$((total+$?))

echo "tta: $total total differing"
[ "$total" -eq 0 ] && echo "TTA matches the C original byte for byte, both directions" || exit 1
