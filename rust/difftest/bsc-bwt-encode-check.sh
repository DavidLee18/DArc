#!/usr/bin/env bash
# Differential-test the forward BWT against libsais.
#
# This port is NOT a translation of libsais. Every suffix of a string has a
# distinct length, so no two suffixes compare equal, so the suffix array is
# totally ordered with no ties to break -- and the BWT and its primary index are
# functions of that array. Any correct construction gives libsais's bytes.
#
# That was checked before the port was written, not assumed: a qsort/memcmp
# suffix array reproduced libsais_bwt over 21 inputs, and a second experiment
# confirmed the sampled-index convention the same way. This harness is what
# holds the property afterwards, against the real SA-IS implementation.
#
# Three entry points are compared, because DArc reaches all of them:
#
#   b  libsais_bwt      the plain transform
#   a  libsais_bwt_aux  the sampled indexes too -- libbsc.cpp's `indexes[256]`
#                       is a stack array, so the aux path is ALWAYS taken
#   f  bsc_bwt_encode   the libbsc wrapper, including the mod it derives from n
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds BSC: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags BSC)" || exit 1
W="${TMPDIR:-/tmp}/bsc-bwt-enc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_bwt_enc_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || { echo "C driver failed to build"    >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "Rust driver failed to build" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

# The corpus targets what a suffix sorter is sensitive to: long runs (deep
# recursion in SA-IS), periodic strings (many equal LMS substrings, so the
# naming step cannot terminate early), a single repeated byte (the degenerate
# case), sorted data, the full alphabet, and the small-n boundaries around the
# aux sampling rate.
python3 - "$W/in" <<'CORPUS'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",        b"the quick brown fox jumps over the lazy dog. "*2000)
w("runs",        b"".join(bytes([i%251])*200 for i in range(400)))
w("longruns",    b"".join(bytes([i%7])*5000 for i in range(60)))
w("noise",       prng(7, 150000))
w("zeros",       b"\x00"*80000)
w("one_byte",    b"Q"*40000)
w("sorted",      bytes(sorted(prng(11, 120000))))
w("full_alpha",  bytes(range(256))*300)
w("ends_zero",   prng(3, 50000)[:-1] + b"\x00")
# Periodic and near-periodic: the shapes that make LMS substrings collide, so
# SA-IS must recurse rather than resolve names in one pass.
w("periodic3",   b"abc"*30000)
w("periodic2",   b"ab"*45000)
w("fibonacci",   (lambda: (lambda f: f(f,24))(lambda f,k: b"a" if k==0 else (b"ab" if k==1 else f(f,k-1)+f(f,k-2))))())
w("almost_per",  (b"abcabcabcabd"*8000))
w("two_symbols", bytes((i*i)%2 for i in range(100000)))
w("bwt_like",    bytes(sorted(prng(5, 90000))))
for n in (2,3,4,5,17,255,256,257,1000,65535,65536):
    w(f"n_{n}", (b"abracadabra"*10000)[:n])
CORPUS

fail=0; nb=0; na=0; nf=0; skipped_a=0

run_mode() {           # $1 = mode letter, $2 = counter name
  local mode="$1"
  for f in "$W"/in/*; do
    local bn; bn="$(basename "$f") mode=$mode"
    "$W/c"  "$mode" < "$f" >| "$W/oc" 2>/dev/null; local rc_c=$?
    "$W/rs" "$mode" < "$f" >| "$W/or" 2>/dev/null; local rc_r=$?
    if [ "$rc_c" -ne "$rc_r" ]; then
      echo "  $bn: C exited $rc_c, Rust $rc_r"; fail=$((fail+1)); continue
    fi
    # rc 4 is the driver declining: n is too small for the aux sampling rate.
    if [ "$rc_c" -eq 4 ]; then skipped_a=$((skipped_a+1)); continue; fi
    if [ "$rc_c" -ne 0 ]; then
      echo "  $bn: both drivers failed with $rc_c"; fail=$((fail+1)); continue
    fi
    # Two empty files compare equal; a mode that produced nothing on both sides
    # would pass silently. Every mode here emits at least the 4-byte index.
    if [ ! -s "$W/oc" ]; then
      echo "  $bn: the C driver produced no output"; fail=$((fail+1)); continue
    fi
    case "$mode" in
      b) nb=$((nb+1)) ;; a) na=$((na+1)) ;; f) nf=$((nf+1)) ;;
    esac
    cmp -s "$W/oc" "$W/or" || { echo "  $bn: differs from the C"; fail=$((fail+1)); }
  done
}

run_mode b
run_mode a
run_mode f

# Coverage: the aux path must actually publish sampled indexes somewhere in the
# corpus, otherwise mode 'a' is only re-testing the primary index. num_indexes
# is byte 4 of mode 'f' output.
with_indexes=0
for f in "$W"/in/text "$W"/in/sorted "$W"/in/noise "$W"/in/periodic3; do
  "$W/c" f < "$f" >| "$W/oc" 2>/dev/null || continue
  ni=$(od -An -j4 -N1 -tu1 < "$W/oc" | tr -d ' ')
  [ "${ni:-0}" -gt 0 ] && with_indexes=$((with_indexes+1))
done

[ "$nb" -gt 0 ] || { echo "no inputs were transformed -- the harness reached nothing"; exit 1; }
[ "$with_indexes" -ge 3 ] || {
  echo "only $with_indexes of 4 inputs published sampled indexes; the aux path is"
  echo "not being exercised beyond its primary index"; fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "bsc-bwt-encode: $fail failures"; exit 1; }
echo "bsc-bwt-encode: $nb bwt + $na bwt_aux + $nf bsc_bwt_encode byte-identical to libsais"
echo "bsc-bwt-encode: ($skipped_a too small for the aux rate; $with_indexes/4 published indexes)"
