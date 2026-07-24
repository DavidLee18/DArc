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
W="${TMPDIR:-/tmp}/bsc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# libsais.c must be its own translation unit -- see bsc_ccodec.cpp. Trailing
# args land after the sources so the Rust staticlib links on GNU ld.
cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$ROOT" -I"$ROOT/Compression" \
    "$ROOT/rust/difftest/bsc_ref.cpp" "$ROOT/rust/difftest/bsc_ccodec.cpp" \
    "$ROOT/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# QLFC codes rank/run-length pairs, so its models are exercised by the SHAPE of
# the byte distribution: skewed (text), flat (noise), long runs, a single byte,
# a large alphabet, and the awkward tiny sizes. The encoder codes whatever bytes
# it is given, so raw inputs are fine even though in the real pipeline QLFC
# follows BWT+MTF.
python3 - "$W/in" <<'PY'
import os,sys,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",     b"the quick brown fox jumps over the lazy dog. "*3000)
w("english",  (b"compression algorithms rearrange data so that "
               b"statistical redundancy can be removed by an entropy coder. ")*900)
w("runs",     b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(2000)))
w("onebyte",  b"\x5a"*80000)
w("twobyte",  (b"\x00\xff"*40000))
w("noise",    prng(9, 200000))
w("skew",     bytes((0 if (i*2654435761>>28)&7 else (i%251) ) for i in range(150000)))
w("alphabet", bytes(i%256 for i in range(200000)))
w("sparse",   b"".join((b"\x00"*300 + bytes([i%251])) for i in range(500)))
for n in (1,2,3,16,17,255,256,257,4096,65537):
    w(f"n_{n}", prng(3,n))
PY

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
