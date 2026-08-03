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
# The reference is built the way DArc builds DisPack: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags DisPack)" || exit 1
W="${TMPDIR:-/tmp}/dispack-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
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
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen --bin difftest-util ) || exit 1

# The code corpus is REAL i386 machine code: synthetic bytes do not have the
# call density detect() keys on. The compile stays here -- orchestration is
# shell's job -- and difftest-util reads the .text section out of the object.
# An empty .text means no i386 compiler on this host, and the corpus skips its
# code inputs exactly as it always did.
cat > "$W/x.c" <<'CSRC'
__attribute__((noinline)) static int a(int x){return x*3+1;}
__attribute__((noinline)) static int b(int x){return a(x)+a(x-1)+2;}
__attribute__((noinline)) static int c(int x){return b(x)*a(x)-b(x+1);}
__attribute__((noinline)) static int d(int x){return c(x)+b(x)+a(x)+c(x-2);}
__attribute__((noinline)) static int e(int x){return d(x)^c(x)^b(x)^a(x);}
__attribute__((noinline)) int top(int*p,int n){int s=0;for(int i=0;i<n;i++){s+=e(p[i])+d(s)+c(i)-b(s^i)+a(p[i&7]);if(s>100000)s=e(s)-d(i);}return s;}
CSRC
clang --target=i386-unknown-linux-gnu -m32 -O2 -c "$W/x.c" -o "$W/x.o" 2>/dev/null \
  || echo "i386 compile unavailable; code corpus skipped" >&2
"$ROOT/rust/target/release/difftest-util" elf-text "$W/x.o" > "$W/text.bin" 2>/dev/null || : > "$W/text.bin"
# The marker the corpus generator used to write, so the run can say out loud
# that the interesting paths are uncovered rather than quietly passing.
[ -s "$W/text.bin" ] && : > "$W/HAVE_CODE"

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" dispack "$W/in" "$W/text.bin"

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
