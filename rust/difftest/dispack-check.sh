#!/usr/bin/env bash
# Differential-test the DisPack decoder port against the C original.
#
# DisPack is an x86 branch/call/jump filter, so its filtered path (TAG_EXE,
# DisUnFilter) only runs on data the compressor's detect() classifies as x86
# CODE. Feed it anything else and it stores raw -- and the whole disassembler,
# the actual port, is never exercised. This is the same "path never reached"
# trap as Tornado's data tables and SREP's block size, so the corpus is built
# to guarantee detect() fires: real i386 .text with its relocation-placeholder
# CALLs rewritten as backward calls (0xFF high byte), which is what detect
# keys on. A sabotage of DisUnFilter's byte order was confirmed to break the
# code inputs, so the filtered path is genuinely on the critical path here.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/dispack-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/dispack_ref.cpp" "$CREF/rust/difftest/dispack_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
# -DDARC_RUST as well: the Rust drop-in now exports dispack_decompress
# unconditionally (the C is deleted), so the pinned C_DisPack.cpp must drop
# its own definition or the two collide. The "rs" binary is meant to run the
# Rust decoder anyway; the "c" binary above links no staticlib and keeps the
# C one, so the oracle is unaffected.
cc "$W/rs" -DUSE_RUST -DDARC_RUST "$LIB" || exit 1

# The corpus: real i386 code (built here so the test is self-contained), plus
# non-code inputs that exercise the raw/TAG_DATA path, plus edge sizes.
python3 - "$W" <<'PY'
import os,sys,struct,subprocess
w=sys.argv[1]; os.makedirs(f"{w}/in",exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)

# Build a real i386 .text section, then rewrite its E8 relocation placeholders
# into backward calls so detect() sees an executable.
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
if r.returncode!=0:
    sys.stderr.write("i386 compile unavailable; skipping code corpus\n"+r.stderr.decode()[:400])
    text=b""
else:
    d=open(obj,"rb").read()
    e_shoff,=struct.unpack("<I",d[0x20:0x24]); ent,=struct.unpack("<H",d[0x2e:0x30])
    num,=struct.unpack("<H",d[0x30:0x32]); stx,=struct.unpack("<H",d[0x32:0x34])
    def sh(i): o=e_shoff+i*ent; return struct.unpack("<IIIIII",d[o:o+24])
    st=sh(stx)[4]; text=b""
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
    wf("code_small",  make_code(30,1))
    wf("code_big",    make_code(200,7))          # spans multiple chunks
    wf("code_noise",  make_code(60,3)+prng(9,40000))  # code then data
wf("noise",   prng(2,300000))
wf("zeros",   b"\x00"*200000)
wf("text",    b"the quick brown fox jumps over the lazy dog. "*4000)
for n in (0,1,4,5,64,4096,65536):
    wf(f"n_{n}", prng(4,n))
PY

# Whether the corpus actually contains x86 code (i386 cross-compile may be
# unavailable). Determined from the corpus itself, NOT from how many cases
# pass -- a sabotage that breaks every code case must still count as "code was
# tested and it failed", not "code was never tested".
code_seen=$(ls "$W"/in/code_* 2>/dev/null | wc -l | tr -d ' ')

total=0
for bs in 8388608 262144 65536; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); name=$(basename "$f")
    "$W/c"  c "$bs" < "$f"   >| "$W/s"  2>/dev/null || { echo "  [bs=$bs] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    "$W/c"  d "$bs" < "$W/s" >| "$W/oc" 2>/dev/null || { echo "  [bs=$bs] $name: C-decode FAILED";   fail=$((fail+1)); continue; }
    "$W/rs" d "$bs" < "$W/s" >| "$W/or" 2>/dev/null || { echo "  [bs=$bs] $name: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc" || { echo "  [bs=$bs] $name: C-decode != original (harness bug)"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/or" || { echo "  [bs=$bs] $name: RUST-decode != original"; fail=$((fail+1)); continue; }
  done
  echo "  [bs=$bs] $n inputs, $fail differing"
  total=$((total+fail))
done

[ "$code_seen" -gt 0 ] || { echo "no x86 code corpus (i386 cross-compile unavailable) -- filtered path UNTESTED"; exit 1; }
echo "dispack decode: $total total differing ($code_seen code cases exercising DisUnFilter)"
[ "$total" -eq 0 ] && echo "DisPack decoder matches the C original byte for byte" || exit 1
