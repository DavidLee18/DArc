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
# The reference is built the way DArc builds DisPack: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags DisPack)" || exit 1
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
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" \
    "$CREF/rust/difftest/dispack_filter_ref.cpp" \
    "$CREF/rust/difftest/dispack_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" \
    ${lib:+"$lib"} -o "$out"; }
# -DDARC_RUST: see dispack-check.sh. It drops the pinned dispack_decompress,
# which this driver never calls, while keeping DisFilter -- the C oracle here.
cc "$W/t" "$LIB" -DDARC_RUST || { echo "harness build failed" >&2; exit 1; }

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
"$ROOT/rust/target/release/corpusgen" dispack-filter "$W/in" "$W/text.bin"

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

# detect() gates the filter: a chunk it calls DATA is stored verbatim and never
# filtered at all. Its classification is therefore part of the archive, so it is
# compared directly rather than only implicitly through the filtered bytes.
dfail=0; dn=0; dexe=0
for f in "$W"/in/*; do
  [ -f "$f" ] || continue
  bn=$(basename "$f"); dn=$((dn+1))
  a=$("$W/t" dc  0 < "$f" 2>/dev/null)
  b=$("$W/t" drs 0 < "$f" 2>/dev/null)
  [ "$a" = "2" ] && dexe=$((dexe+1))
  [ "$a" = "$b" ] || { echo "  detect: $bn: C=$a Rust=$b"; dfail=$((dfail+1)); }
done
echo "  [detect] $dn inputs, $dfail differing ($dexe classified EXE)"
# Both answers must actually occur, or the comparison proves nothing: a detect()
# that always said DATA would agree with a correct one on a DATA-only corpus.
[ "$dexe" -gt 0 ] || { echo "  detect: NO input classified EXE -- the gate is untested"; dfail=$((dfail+1)); }
[ "$dexe" -lt "$dn" ] || { echo "  detect: EVERY input classified EXE -- the gate is untested"; dfail=$((dfail+1)); }
fail=$((fail+dfail))

[ "$n" -gt 0 ] || { echo "no inputs were filtered -- harness reached nothing"; exit 1; }
echo "dispack forward filter: $fail differing over $n comparisons"
[ "$fail" -eq 0 ] && echo "DisPack encoder matches the C original byte for byte" || exit 1
