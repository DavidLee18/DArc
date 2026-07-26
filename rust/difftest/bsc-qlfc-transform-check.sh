#!/usr/bin/env bash
# Differential-test the QLFC forward transform against the C original.
#
# The transform is the stage all three QLFC encode bodies (static, adaptive,
# fast) share: it turns a block into a rank array plus the alphabet the encoder
# codes as its preamble. Cutting here means a mismatch points at the
# move-to-front walk rather than at the range coder wrapped around it -- the
# same ordering the QLFC *decoders* were built in.
#
# BOTH outputs are compared. `MTFTable` is an output of this function, not a
# scratch buffer, and a port producing the right ranks from a wrong table would
# go on to write a preamble no decoder could follow.
#
# On the SIMD question: qlfc.cpp has two transform bodies, a vectorised one and
# a scalar `#else`. Unlike LZP's six encoder bodies -- which emit different
# bytes -- these agree: building the C twice, once with -DLIBBSC_CPU_FEATURE=0,
# gives byte-identical encoder output over the corpus. So the port implements
# the scalar body and this harness compares it against whichever the host picks.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/bsc-qlfc-tr.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_qlfc_transform_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# The transform is driven by the RUN structure and the order in which symbols
# are first seen, walking backwards. So: long runs, single-symbol blocks, a full
# alphabet, blocks ending in 0 (the one special case in the table setup), a
# ranks-only-just-fit alphabet of 255 symbols, and BWT-like output, which is
# what it actually sees in a real archive.
python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)

w("text",     b"the quick brown fox jumps over the lazy dog. "*3000)
w("runs",     b"".join(bytes([i%251])*200 for i in range(500)))
w("noise",    prng(7, 200000))
w("zeros",    b"\x00"*100000)
w("one_byte", b"Q"*50000)
# The `input[n-1] == 0` special case, which reorders the table before the walk.
w("ends_zero", prng(3, 60000)[:-1] + b"\x00")
w("all_zero_runs", b"".join(b"\x00"*50 + bytes([i%255+1])*7 for i in range(2000)))
# Every byte value present, and 255 of them (one short of the alphabet), since
# the preamble's terminating repeat depends on an unused entry existing.
w("full_alphabet", bytes(range(256))*400)
w("alphabet_255",  bytes((i%255)+1 for i in range(120000)))
# Sorted-ish data, which is the shape a block sorter actually hands over.
w("bwt_like", bytes(sorted(prng(11, 150000))))
for n in (1,2,3,4,17,255,256,257,65536):
    w(f"n_{n}", (b"abracadabra"*10000)[:n])
PY

fail=0; tested=0
for f in "$W"/in/*; do
  bn=$(basename "$f")
  "$W/c"  < "$f" >| "$W/oc" 2>/dev/null || { echo "  $bn: C driver failed";    fail=$((fail+1)); continue; }
  "$W/rs" < "$f" >| "$W/or" 2>/dev/null || { echo "  $bn: Rust driver failed"; fail=$((fail+1)); continue; }
  tested=$((tested+1))
  cmp -s "$W/oc" "$W/or" || { echo "  $bn: transform output differs from the C"; fail=$((fail+1)); }
done

# Coverage: a transform that returned `index == n` for everything would emit no
# ranks at all and still compare equal on both sides. Require the corpus to
# actually produce rank arrays.
nonempty=0
for f in "$W"/in/text "$W"/in/runs "$W"/in/bwt_like; do
  "$W/c" < "$f" >| "$W/oc" 2>/dev/null || continue
  sz=$(wc -c < "$W/oc")
  [ "$sz" -gt 260 ] && nonempty=$((nonempty+1))
done

[ "$tested" -gt 0 ] || { echo "no inputs were transformed -- the harness reached nothing"; exit 1; }
[ "$nonempty" -ge 3 ] || {
  echo "only $nonempty of 3 inputs produced a rank array; the corpus is not exercising the walk"
  fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "bsc-qlfc-transform: $fail failures"; exit 1; }
echo "bsc-qlfc-transform: $tested/$tested byte-identical to the C (ranks and MTF table)"
