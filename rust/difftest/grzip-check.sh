#!/usr/bin/env bash
# Differential-test the GRZip decoder port against the C original.
#
# GRZip is ported decode-first, so the C compressor is the only encoder. This
# works at the BLOCK level rather than the stream level: grzip_decompress (the
# multithreaded stream wrapper) is not ported yet, but GRZip_DecompressBlock is,
# and it is where every ported stage actually runs.
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
W="${TMPDIR:-/tmp}/grzip-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land after the sources: GNU ld resolves an archive against
# only the objects already seen, so a library placed first is silently dropped.
cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
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
python3 - "$W/in" <<'PY'
import os,sys,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",     b"the quick brown fox jumps over the lazy dog. "*9000)
w("repeats",  (b"ABCDEFGHIJKLMNOP"*64 + prng(1,128))*300)
w("runs",     b"".join(bytes([i%251])*(1+(i%97)) for i in range(6000)))
w("noise",    prng(7, 400000))
w("zeros",    b"\x00"*300000)
w("rec4",     b"".join(struct.pack("<I", i*7+3) for i in range(120000)))
w("rec2",     b"".join(struct.pack("<H", (i*11)&0xffff) for i in range(200000)))
w("mixed",    b"".join((prng(i,600) + b"pattern"*120) for i in range(200)))
w("big",      b"".join(struct.pack("<I", (i*2654435761)&0xffffffff) for i in range(1800000)))  # ~7 MB, just under one block
for n in (1,2,3,4,27,28,29,255,256,257,4096,65537):
    w(f"n_{n}", (b"the quick brown fox "*4000)[:n])
PY

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
python3 - "$W/big" <<'PY2'
import sys,struct
open(sys.argv[1],"wb").write(b"".join(
    struct.pack("<I", (i*2654435761)&0xffffffff) if i%3 else b"the quick brown fox "[:4]
    for i in range(3000000)))
PY2
sfail=0; sn=0
for f in "$W"/in/* "$W/big"; do
  sn=$((sn+1)); name=$(basename "$f")
  "$W/c"  sc < "$f"    >| "$W/ss" 2>/dev/null || { echo "  [stream] $name: C-compress FAILED"; sfail=$((sfail+1)); continue; }
  "$W/c"  sd < "$W/ss" >| "$W/sc" 2>/dev/null || { echo "  [stream] $name: C-decode FAILED";   sfail=$((sfail+1)); continue; }
  "$W/rs" sd < "$W/ss" >| "$W/sr" 2>/dev/null || { echo "  [stream] $name: RUST-decode FAILED"; sfail=$((sfail+1)); continue; }
  cmp -s "$f" "$W/sc" || { echo "  [stream] $name: C-decode != original (harness bug)"; sfail=$((sfail+1)); continue; }
  cmp -s "$f" "$W/sr" || { echo "  [stream] $name: RUST-decode != original"; sfail=$((sfail+1)); continue; }
done
echo "  [stream, multi-block] $sn inputs, $sfail differing"
total=$((total+sfail))

echo "grzip decode: $total total differing"
[ "$total" -eq 0 ] && echo "GRZip decoder matches the C original byte for byte" || exit 1
