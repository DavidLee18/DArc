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
W="${TMPDIR:-/tmp}/bsc-qlfc-enc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
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
python3 - "$W/in" <<'CORPUS'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",       b"the quick brown fox jumps over the lazy dog. "*2000)
w("runs",       b"".join(bytes([i%251])*200 for i in range(400)))
w("longruns",   b"".join(bytes([i%7])*5000 for i in range(60)))
w("noise",      prng(7, 150000))
w("zeros",      b"\x00"*80000)
w("one_byte",   b"Q"*40000)
w("sorted",     bytes(sorted(prng(11, 120000))))
w("full_alpha", bytes(range(256))*300)
w("ends_zero",  prng(3, 50000)[:-1] + b"\x00")
for n in (256, 1024, 65536):
    w(f"n_{n}", (b"abracadabra"*10000)[:n])
CORPUS

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
echo "bsc-qlfc-encode: $tested/$tested byte-identical to the C (all three coders; $declined declined by both)"
