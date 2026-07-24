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
W="${TMPDIR:-/tmp}/bsc-bwt-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# libsais.c is its own translation unit (see bsc_ccodec.cpp). The Rust staticlib
# trails the sources so it links on GNU ld.
cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_bwt_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# The inverse BWT is exercised by the RUN structure of the data (the fastbits
# scan and the PSI follow depend on the bigram distribution): text, long runs, a
# single byte, two bytes, flat noise, a full alphabet, sparse. Sizes span the
# 64 KiB aux-index boundary so both the single-index and strided paths run.
python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",     b"the quick brown fox jumps over the lazy dog. "*4000)   # >64K
w("text_sm",  b"the quick brown fox jumps over the lazy dog. "*200)    # <64K
w("runs",     b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(3000)))
w("onebyte",  b"\x5a"*80000)
w("onebyte_s",b"\x5a"*5000)
w("twobyte",  b"\x00\xff"*45000)
w("noise",    prng(9, 200000))
w("noise_sm", prng(9, 4000))
w("alphabet", bytes(i%256 for i in range(200000)))
w("sparse",   b"".join((b"\x00"*300 + bytes([i%251])) for i in range(500)))
w("skew",     bytes((0 if (i*2654435761>>28)&7 else (i%251)) for i in range(150000)))
# straddle the boundary precisely
for n in (2,3,16,255,256,257,4095,4096,65535,65536,65537,131072):
    w(f"n_{n}", prng(3,n))
PY

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
