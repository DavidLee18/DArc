#!/usr/bin/env bash
# Differential-test the DisPack FORWARD filter (the encoder) against the C.
#
# The existing dispack-check.sh covers decode: C filters, C and Rust unfilter,
# both must return the original. This covers the other direction and is a
# stricter test -- DisPack is one of DArc's own formats, so the encoder must be
# BYTE-EXACT, not merely reversible. A filtered stream that differs from the C's
# would still round-trip through our own decoder while making archives that
# older builds decode differently.
#
# Two things this corpus has to get right, both already paid for by the decode
# port:
#
#   * REAL x86 CODE. DisPack's interesting paths -- ModR/M, SIB, the relative
#     call/jump rewrite, MTF-coded call targets, jump-table detection -- only run
#     on things that disassemble. Random bytes exercise the escape path and
#     nothing else, and a green run over random data means the test reached
#     nothing. The code here is compiled on the spot for i386 and its E8
#     relocation placeholders are rewritten into backward calls.
#   * VARIED ORIGIN. DisFilter converts relative targets to absolute ones, so
#     origin is part of the transform, not a formality. Same bytes at a
#     different load address must still agree with the C.
#
# The C reference comes from a pinned revision -- see c-reference.sh.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/dispack-filter-check.$$"; mkdir -p "$W" "$W/in"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# $lib is a separate trailing argument, never folded into "$@" with the -D
# flags: GNU ld resolves an archive only against undefined symbols it has
# already seen, so a staticlib ahead of the sources links on macOS and fails on
# Linux. That exact mistake shipped once.
cc() { local out="$1" lib="$2"; shift 2
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" \
    "$CREF/rust/difftest/dispack_filter_ref.cpp" \
    "$CREF/rust/difftest/dispack_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" \
    ${lib:+"$lib"} -o "$out"; }
cc "$W/t" "$LIB" || { echo "harness build failed" >&2; exit 1; }

python3 - "$W" <<'PY'
import os,sys,struct,subprocess
w=sys.argv[1]
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)

src=f"{w}/x.c"
open(src,"w").write('''
__attribute__((noinline)) static int a(int x){return x*3+1;}
__attribute__((noinline)) static int b(int x){return a(x)+a(x-1)+2;}
__attribute__((noinline)) static int c(int x){return b(x)*a(x)-b(x+1);}
__attribute__((noinline)) static int d(int x){return c(x)+b(x)+a(x)+c(x-2);}
__attribute__((noinline)) static int e(int x){return d(x)^c(x)^b(x)^a(x);}
__attribute__((noinline)) int top(int*p,int n){int s=0;for(int i=0;i<n;i++){s+=e(p[i])+d(s)+c(i)-b(s^i)+a(p[i&7]);if(s>100000)s=e(s)-d(i);}return s;}
''')
obj=f"{w}/x.o"
r=subprocess.run(["clang","--target=i386-unknown-linux-gnu","-m32","-O2","-c",src,"-o",obj],
                 capture_output=True)
text=b""
if r.returncode!=0:
    sys.stderr.write("i386 compile unavailable; code corpus skipped\n")
else:
    d=open(obj,"rb").read()
    e_shoff,=struct.unpack("<I",d[0x20:0x24]); ent,=struct.unpack("<H",d[0x2e:0x30])
    num,=struct.unpack("<H",d[0x30:0x32]); stx,=struct.unpack("<H",d[0x32:0x34])
    def sh(i): o=e_shoff+i*ent; return struct.unpack("<IIIIII",d[o:o+24])
    st=sh(stx)[4]
    for i in range(num):
        name,_,_,_,off,size=sh(i)
        if d[st+name:d.index(b"\0",st+name)].decode()==".text": text=d[off:off+size]

def make_code(reps,seed):
    blob=bytearray(text*reps); s=seed; i=0
    while i<len(blob)-5:
        if blob[i]==0xE8 and blob[i+1]==0==blob[i+2]==blob[i+3]==blob[i+4]:
            s=(s*1103515245+12345)&0xffffff
            blob[i+1]=s&0xff; blob[i+2]=(s>>8)&0xff; blob[i+3]=(s>>16)&0xff; blob[i+4]=0xFF
        i+=1
    return bytes(blob)

wf=lambda n,b: open(f"{w}/in/{n}","wb").write(b)
if text:
    wf("code_small", make_code(30,1))
    wf("code_big",   make_code(200,7))
    wf("code_noise", make_code(60,3)+prng(9,40000))
    open(f"{w}/HAVE_CODE","w").write("1")
# A plausible jump table: dwords that all land inside the block's address range,
# which is exactly what DetectJumpTable keys on (>=3 consecutive entries).
base=0x401000
tbl=b"".join(struct.pack("<I", base+((i*17)%0x8000)) for i in range(64))
wf("jumptable", tbl + (text if text else prng(5,4000)))
# Runs of EXACTLY TWO in-range dwords, separated by an out-of-range one. The
# threshold is "fewer than 3 is coincidence, not a table", so this is the only
# shape that distinguishes 3 from 2 -- with 64 consecutive entries above, both
# thresholds behave identically and a sabotage of the constant goes unnoticed.
pairs=bytearray()
for i in range(300):
    pairs += struct.pack("<I", base+((i*29)%0x8000))
    pairs += struct.pack("<I", base+((i*31)%0x8000))
    pairs += struct.pack("<I", 0xF0000000 + i)   # out of range: breaks the run
wf("pairs", bytes(pairs))
# Make the MTF SEARCH BOUND falsifiable. Distinct targets alone do not: every
# one is a miss, find_mtf never returns a hit, and 255-vs-254 cannot matter.
# The bound only shows on a lookup that lands at exactly index 254.
#
# add_mtf pushes to the front, so after inserting t0..t299 in order the table
# holds mtf[k] == t(299-k), putting t45 at index 254. Referencing t45 as the
# next entry is therefore found with the real bound (searches 0..=254) and
# missed with a bound one smaller -- which changes the output, because a miss
# emits a full 32-bit address instead of a one-byte index.
many=bytearray()
for i in range(300):
    many += struct.pack("<I", base+(i*4))
many += struct.pack("<I", base+(45*4))    # sits at index 254 by the above
wf("mtf_boundary", bytes(many))
wf("noise",   prng(2,200000))
wf("zeros",   b"\x00"*100000)
wf("text",    b"the quick brown fox jumps over the lazy dog. "*3000)
for n in (0,1,4,5,14,15,16,64,4096,65536):
    wf(f"n_{n}", prng(4,n))
PY

[ -f "$W/HAVE_CODE" ] || echo "  WARNING: no i386 compiler -- the code corpus is absent and the interesting paths are NOT covered"

fail=0; n=0
for origin in 0x401000 0x00400000 0x10000000 0x0; do
  d=0; c=0
  for f in "$W"/in/*; do
    [ -f "$f" ] || continue
    bn=$(basename "$f"); c=$((c+1)); n=$((n+1))
    rm -f "$W/ec" "$W/er"
    "$W/t" c  "$origin" < "$f" >| "$W/ec" 2>/dev/null || { echo "  [org=$origin] $bn: C-filter FAILED"; fail=$((fail+1)); d=$((d+1)); continue; }
    "$W/t" rs "$origin" < "$f" >| "$W/er" 2>/dev/null || { echo "  [org=$origin] $bn: RUST-filter FAILED"; fail=$((fail+1)); d=$((d+1)); continue; }
    cmp -s "$W/ec" "$W/er" || { echo "  [org=$origin] $bn: differs from the C ($(wc -c < "$W/ec") vs $(wc -c < "$W/er") bytes)"; fail=$((fail+1)); d=$((d+1)); }
  done
  echo "  [origin=$origin] $c inputs, $d differing"
done

[ "$n" -gt 0 ] || { echo "no inputs were filtered -- harness reached nothing"; exit 1; }
echo "dispack forward filter: $fail differing over $n comparisons"
[ "$fail" -eq 0 ] && echo "DisPack encoder matches the C original byte for byte" || exit 1
