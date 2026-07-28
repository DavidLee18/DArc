#!/usr/bin/env bash
# Differential-test the REP decoder port against the C original.
#
# REP is ported decode-first, so the C compressor is the only encoder: compress
# each input with C, decompress with both C and the Rust port, and require both
# to reproduce the original. Byte-for-byte equality is the bar because REP
# defines an archive format.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds REP: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags REP)" || exit 1
W="${TMPDIR:-/tmp}/rep-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land AFTER the sources, because the Rust staticlib has to:
# GNU ld resolves an archive against only the objects already seen on the
# command line, so a library placed first is silently dropped and every symbol
# comes back undefined. macOS ld does not care, which is how this script passed
# locally while never having linked on Linux.
cc() { # cc <output> [args appended after the sources]
  local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -DREP_LIBRARY -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/rep_ref.cpp" "$CREF/Compression/REP/rep.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"
}
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs: REP matches only >=512-byte repeats, so cover long repeats, block
# edges, incompressible data and the empty case.
python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("empty",b""); w("tiny",b"hello"); w("nomatch",prng(1,20000))
blk=prng(2,2000); w("one_match",blk+prng(3,5000)+blk+blk)
w("many",(blk+prng(4,600))*40); w("zeros",b"\0"*100000)
w("text",b"the quick brown fox jumps over the lazy dog. "*2000)
for n in (511,512,513,1023,1024,1025): w(f"rep_{n}",prng(6,n)*3)
PY

fail=0 n=0
for f in "$W"/in/*; do
  n=$((n+1)); name=$(basename "$f")
  "$W/c"  c < "$f"          >| "$W/stream"
  "$W/c"  d < "$W/stream"   >| "$W/oc"
  "$W/rs" d < "$W/stream"   >| "$W/ors"
  cmp -s "$f" "$W/oc"  || { echo "  $name: C-decode != original (harness)"; fail=$((fail+1)); continue; }
  cmp -s "$f" "$W/ors" || { echo "  $name: RUST-decode != original"; fail=$((fail+1)); continue; }
done
echo "rep decode: $n inputs, $fail differing"
[ "$fail" -eq 0 ] && echo "REP decoder matches the C original byte for byte" || exit 1
