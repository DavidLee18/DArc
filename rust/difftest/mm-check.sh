#!/usr/bin/env bash
# Differential-test the MM decoder port against the C original.
#
# MM is ported decode-first, so the C compressor is the only encoder: filter
# each input with C (over a matrix of channel counts, word sizes, header
# offsets and detector modes), unfilter with both C and the Rust port, and
# require both to reproduce the original. Byte-for-byte equality is the bar
# because MM defines an archive format.
#
# Inputs are deliberately larger than 1 MB: mm_compress reads a first block of
# up to 1 MB and then switches to 64 KB blocks, while the decoder always reads
# in 64 KB chunks, so anything smaller never exercises a running sum crossing a
# block boundary -- the one piece of state that spans the whole stream.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
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
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$ROOT" -I"$ROOT/Compression" \
    "$ROOT/rust/difftest/mm_ref.cpp" "$ROOT/rust/difftest/mm_ccodec.cpp" \
    "$ROOT/Compression/Common.cpp" "$@" -o "$out"
}
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs: multimedia-shaped data at every sample width the filter implements
# (8/16/24/32-bit, mono through four channels), plus a real .wav so the WAV
# header detector fires and produces a nonzero offset, plus data the detectors
# should refuse (noise, text) so the "stored" branch of the decoder is reached.
# Sizes straddle the 1 MB first-block boundary and leave partial samples at the
# end, which the filter must pass through untouched.
python3 - "$W/in" <<'PY'
import os,sys,math,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
def s16(vals): return b"".join(struct.pack("<h", max(-32768,min(32767,int(v)))) for v in vals)
N=300000                                     # 300k stereo 16-bit samples = 1.2 MB
pcm = s16([ v for i in range(N) for v in (int(20000*math.sin(i*0.03)), int(15000*math.sin(i*0.041))) ])
w("sine16_stereo",  pcm)
w("chord16_stereo", s16([ v for i in range(N) for v in
                        (int(8000*math.sin(i*0.02)+6000*math.sin(i*0.05)),
                         int(7000*math.sin(i*0.03)+5000*math.sin(i*0.07))) ]))
w("quiet16_stereo", s16([ v for i in range(N) for v in (int(30*math.sin(i*0.03)), int(25*math.sin(i*0.05))) ]))
# A real 44-byte canonical WAV header, so autodetect_wav_header takes the
# offset path rather than the entropy analyzer.
hdr = (b"RIFF" + struct.pack("<I", 36+len(pcm)) + b"WAVEfmt " + struct.pack("<IHHIIHH",16,1,2,44100,44100*4,4,16)
       + b"data" + struct.pack("<I", len(pcm)))
w("wav16_stereo", hdr + pcm)
w("rgb24",   bytes([ v for i in range(400000) for v in
                     ((i*7)%256, (128+int(100*math.sin(i*0.01)))&0xff, (i//97)%256) ]))
w("pcm24_stereo", b"".join(struct.pack("<i", int(4000000*math.sin(i*0.02)) & 0xffffff)[:3] for i in range(2*N)))
w("float32_mono", b"".join(struct.pack("<f", math.sin(i*0.01)) for i in range(350000)))
w("ramp32",   b"".join(struct.pack("<I", (i*2654435761)&0xffffffff) for i in range(350000)))
w("noise8",   prng(9, 1300000))
w("silence",  b"\x00" * 1300000)
w("text",     (b"the quick brown fox jumps over the lazy dog. " * 30000))
# Empty input is deliberately absent: mm_compress writes *nothing* for it (it
# bails before emitting even the flags byte), and the C decoder rejects the
# resulting empty stream with FREEARC_ERRCODE_IO. There is no stream to agree
# about. The Rust port's behaviour on it is pinned by tests/mm.rs instead.
for n in (1,3,7,8,63, (1<<20)-1, 1<<20, (1<<20)+1, (1<<20)+7):
    w(f"n_{n}", (pcm*8)[:n])
PY

stored=0 filtered=0          # which decoder branch each stream actually took

decode_case () {   # $1=tag  $2..=encoder args (MODE SKIPHDR ISFLOAT NUMCHAN WORDSIZE OFFSET)
  local tag="$1"; shift
  local ec="$*"
  local f fail=0 n=0
  for f in "$W"/in/*; do
    n=$((n+1)); local name; name=$(basename "$f")
    "$W/c"  c $ec < "$f"        >| "$W/stream" 2>/dev/null || { echo "  [$tag] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
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

echo "mm decode: $total total differing ($filtered filtered streams, $stored stored)"
[ "$stored"   -gt 0 ] || { echo "corpus never reached the stored branch"; exit 1; }
[ "$filtered" -gt 0 ] || { echo "corpus never reached the filtered branch"; exit 1; }
[ "$total"    -eq 0 ] && echo "MM decoder matches the C original byte for byte" || exit 1
