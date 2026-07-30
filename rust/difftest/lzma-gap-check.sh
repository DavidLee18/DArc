#!/usr/bin/env bash
# MEASURE the gap between DArc's forked LZMA and stock LZMA.
#
# This is not a port check and it is not expected to pass. Nothing in DArc uses
# rust/darc-lzma; the question is how far DArc's fork has moved from the stock
# 7-Zip encoder, because that decides whether the fork can be re-derived
# incrementally or needs the optimal parser rebuilt first.
#
# `Compression/LZMA/readme` ("List of changes made") lists ten divergences. Two of
# them -- the `maxDist[]` table and `len + 1 >= lenMain` -- change which matches
# the optimal parser selects, so they would perturb the output bytes. Items 4, 6
# and 7 change buffer and hash geometry. Whether any of that is *reachable* at the
# parameters DArc actually uses is what this measures.
#
# Two known, deliberate differences are accounted for rather than reported as
# divergence:
#
#   * DArc sets writeEndMark=1; darc-lzma emits no end marker. So the C stream
#     should be the Rust stream plus an EOPM. The classifier below calls that
#     "prefix+EOPM" and treats it as AGREEMENT of the parse.
#   * DArc sets numThreads from GetCompressionThreads(). The multi-threaded match
#     finder can emit different bytes; the C driver is built single-threaded so
#     that axis is isolated rather than mixed in.
#
# Gates on exit codes, never on grepping tool prose.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
# The SDK gets its OWN flags -- Compression/LZMA/makefile compiles 7z24/*.c with
# C7Z_CFLAGS, which unlike the wrapper set omits -fno-strict-aliasing. Merging the
# two would build an oracle DArc does not ship; PPMd is the standing proof that
# such a difference can change compressed bytes.
SDK_CFLAGS="$(darc_lzma_sdk_cflags)" || exit 1
W="${TMPDIR:-/tmp}/lzma-gap.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# ---- build the pinned C driver ------------------------------------------------
# File list taken from the pinned Compression/LZMA/makefile, not guessed.
# The WORKING TREE, not the pinned reference: the question is how far the LZMA
# DArc ships today is from stock, and the driver includes the working-tree
# C_LZMA.cpp by relative path anyway. Mixing a pinned SDK with a working-tree
# wrapper would compare neither cleanly.
SDK="$ROOT/Compression/LZMA/7z24"
DEFS="-DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT -DZ7_ST"
objs=""
# SDK sources, with the makefile's C7Z_CFLAGS.
for c in LzmaEnc LzmaDec LzFind LzFindOpt CpuArch 7zStream; do
  # shellcheck disable=SC2086
  clang -c $SDK_CFLAGS -w $DEFS -I"$SDK" -o "$W/$c.o" "$SDK/$c.c" 2>>"$W/cbuild.log" \
    || { echo "compiling SDK $c.c failed" >&2; tail -20 "$W/cbuild.log" >&2; exit 1; }
  objs="$objs $W/$c.o"
done
# Wrapper + driver, with the wrapper flag set.
# shellcheck disable=SC2086
clang++ -std=c++17 $CFLAGS_C -w $DEFS \
  -I"$ROOT" -I"$ROOT/Compression" -I"$SDK" \
  "$ROOT/rust/difftest/lzma_ref.cpp" \
  "$ROOT/Compression/Common.cpp" \
  $objs \
  -o "$W/c" 2>>"$W/cbuild.log"
if [ ! -x "$W/c" ]; then
  echo "building the pinned C LZMA driver failed:" >&2
  tail -25 "$W/cbuild.log" >&2
  exit 1
fi

# ---- build the Rust driver ----------------------------------------------------
( cd "$ROOT/rust" && cargo build --release -p darc-lzma --bin lzma_rs_ref ) >/dev/null 2>&1 \
  || { echo "building the darc-lzma driver failed" >&2; exit 1; }
RS="$ROOT/rust/target/release/lzma_rs_ref"
[ -x "$RS" ] || { echo "no darc-lzma driver at $RS" >&2; exit 1; }

# ---- corpus ------------------------------------------------------------------
python3 - "$W/in" <<'PY'
import os, sys
d = sys.argv[1]; os.makedirs(d, exist_ok=True)
def prng(seed, n):
    s = seed & 0xffffffff; o = bytearray()
    for _ in range(n):
        s = (s * 1103515245 + 12345) & 0xffffffff
        o.append((s >> 16) & 0xff)
    return bytes(o)
w = lambda n, b: open(f"{d}/{n}", "wb").write(b)
# Shapes chosen to hit different parse decisions: long matches, short matches,
# incompressible data, and the boundaries where maxDist[] would bite.
w("text",    b"the quick brown fox jumps over the lazy dog. " * 700)
w("zeros",   bytes(40000))
w("runs",    b"".join(bytes([i % 251]) * (1 + (i * 7) % 300) for i in range(300)))
w("noise",   prng(9, 40000))
w("mixed",   b"".join((b"chunk-%d-" % i) + prng(i, 200) for i in range(150)))
w("nearby",  b"".join(prng(i % 5, 500) for i in range(60)))   # repeats at short distance
w("distant", prng(1, 30000) + prng(2, 30000) + prng(1, 30000)) # repeat past 64 KB
for n in (1, 2, 17, 4096):
    w(f"n_{n}", prng(5, n))
PY

# ---- compare ------------------------------------------------------------------
# dictSize lc lp pb fb mc  -- matchFinder is fixed at kBT4=2 and algorithm at 1,
# the only configuration darc-lzma implements.
CASES=(
  "1048576 3 0 2 32 0"
  "1048576 3 0 2 64 0"
  "8388608 3 0 2 32 0"
  "65536   3 0 2 32 0"
  "1048576 0 2 0 32 0"
  "1048576 4 0 2 32 0"
  "1048576 3 0 2 32 16"
  "1048576 3 0 2 273 0"
)

same=0; eopm=0; diverged=0; failed=0; total=0
declare -a DIVERGE_DETAIL=()
for case in "${CASES[@]}"; do
  # shellcheck disable=SC2086
  set -- $case
  for f in "$W"/in/*; do
    bn=$(basename "$f"); total=$((total+1))
    if ! "$W/c" "$1" "$2" "$3" "$4" "$5" "$6" 2 1 < "$f" >| "$W/oc" 2>/dev/null; then
      failed=$((failed+1)); continue
    fi
    if ! "$RS" "$1" "$2" "$3" "$4" "$5" "$6" 2 1 < "$f" >| "$W/or" 2>/dev/null; then
      failed=$((failed+1)); continue
    fi
    cs=$(wc -c < "$W/oc" | tr -d ' '); rs=$(wc -c < "$W/or" | tr -d ' ')
    if cmp -s "$W/oc" "$W/or"; then
      same=$((same+1))
    elif [ "$cs" -gt "$rs" ] && head -c "$rs" "$W/oc" | cmp -s - "$W/or"; then
      # C is the Rust stream plus trailing bytes: the expected EOPM. The PARSE
      # agreed, which is the finding that matters.
      eopm=$((eopm+1))
    else
      diverged=$((diverged+1))
      # BSD cmp says "differ: char N, line M"; GNU says "differ: byte N, line M".
      off=$(cmp "$W/oc" "$W/or" 2>/dev/null | sed -n 's/.*\(char\|byte\) \([0-9]*\).*/\2/p' | head -1)
      pct=$(python3 -c "print(f'{100*int('${off:-0}')/max(int('$rs'),1):.1f}')" 2>/dev/null || echo "?")
      [ "${#DIVERGE_DETAIL[@]}" -lt 8 ] && DIVERGE_DETAIL+=("  [$1 lc$2 lp$3 pb$4 fb$5 mc$6] $bn: first differs at byte ${off:-?} of $rs ($pct% in), C=$cs")
    fi
  done
done

echo "lzma gap: $total comparisons"
echo "  byte-identical                       $same"
echo "  identical + trailing EOPM (parse OK) $eopm"
echo "  DIVERGED                             $diverged"
echo "  driver failures                      $failed"
if [ "${#DIVERGE_DETAIL[@]}" -gt 0 ]; then
  echo "first divergences:"
  printf '%s\n' "${DIVERGE_DETAIL[@]}"
fi

# This script's job is to REPORT, so it exits 0 whenever it measured something.
# A nonzero exit means the harness itself could not run.
[ "$total" -gt 0 ] && [ "$failed" -lt "$total" ] || { echo "nothing was measured" >&2; exit 1; }
