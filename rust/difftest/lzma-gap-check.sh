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

# ---- inputs that force the sliding window ------------------------------------
# Everything above is at most ~40 KB, which is smaller than the smallest dict
# below: the window never slides for any of it, so it cannot tell a correct
# window from a broken one. These are deliberately many times the dict size, and
# are used only with the STREAM_CASES dictionaries.
def prng4(seed, n):
    # 4 bytes per step so multi-megabyte corpora stay cheap to generate.
    s = seed & 0xffffffff; o = bytearray()
    while len(o) < n:
        s = (s * 1103515245 + 12345) & 0xffffffff
        o += s.to_bytes(4, "little")
    return bytes(o[:n])
sd = f"{d}/stream"; os.makedirs(sd, exist_ok=True)
ws = lambda n, b: open(f"{sd}/{n}", "wb").write(b)
# Incompressible: worst case for the window, every position searched.
ws("big_noise", prng4(11, 3_000_000))
# Long-range repeats *beyond* the dictionary, so matches fall out of the window
# as it slides -- the case where an off-by-one in MoveBlock changes the parse.
blk = prng4(12, 250_000)
ws("big_far_repeat", blk + prng4(13, 600_000) + blk + prng4(14, 600_000) + blk)
# Highly compressible, so the parse takes long matches across slide boundaries.
ws("big_text", b"the quick brown fox jumps over the lazy dog. " * 60_000)
# Runs whose lengths straddle the fast-bytes cap.
ws("big_runs", b"".join(bytes([i % 251]) * (1 + (i * 13) % 900) for i in range(6000)))
PY

# ---- compare ------------------------------------------------------------------
# dictSize lc lp pb fb mc mf algo
#
# `mf` is the DArc matchFinder id (C_LZMA.cpp:16): 0=BT2 1=BT3 2=BT4 3=HC4 4=HT4.
# `algo` is 0 for the fast parser, 1 for the optimal one (C_LZMA.cpp:359-362).
CASES=(
  "1048576 3 0 2 32 0 2 1"
  "1048576 3 0 2 64 0 2 1"
  "8388608 3 0 2 32 0 2 1"
  "65536   3 0 2 32 0 2 1"
  "1048576 0 2 0 32 0 2 1"
  "1048576 4 0 2 32 0 2 1"
  "1048576 3 0 2 32 16 2 1"
  "1048576 3 0 2 273 0 2 1"
)

# Every match finder x both parsers, at DArc's OWN defaults (C_LZMA.cpp:249-257:
# dict 64m, fb 32, mc auto, pb2 lc3 lp0).
#
# This axis is the one that was missing, and its absence was not visible: the 88
# comparisons above are all mf=BT4, while LZMA_METHOD's default is kHT4 -- which
# maps to (btMode=0, numHashBytes=5), i.e. Hc5, a hash CHAIN. Not one preset in
# Compression.hs names a finder, so every archive DArc has ever written with -mlzma
# used the one configuration the harness never tested. `3binary` additionally uses
# `fast`, so it needs algo=0 too.
FINDER_CASES=()
for _mf in 0 1 2 3 4; do
  for _algo in 0 1; do
    FINDER_CASES+=("67108864 3 0 2 32 0 $_mf $_algo")
  done
done

# Small dictionaries against multi-megabyte inputs: the window must slide many
# times per run. 1 MB / 3 MB is ~45 slides; 64 KB / 3 MB is ~700. Without these
# the whole streaming path is unexercised and this script reports 88/88 whether
# the window works or not.
# Small dictionary, multi-megabyte input -- and deliberately across finders, since
# the window interacts with each search differently (the chains plant a link per
# position where the trees splice a subtree).
STREAM_CASES=(
  "65536   3 0 2 32 0 2 1"
  "65536   3 0 2 273 0 2 1"
  "1048576 3 0 2 64 0 2 1"
  "65536   3 0 2 32 0 4 1"
  "65536   3 0 2 32 0 4 0"
  "65536   3 0 2 32 0 0 1"
)

same=0; eopm=0; diverged=0; failed=0; total=0
slid=0; finders_seen=""; parsers_seen=""
declare -a DIVERGE_DETAIL=()
compare_one () { # $1..$8 = params, $9 = input file, ${10} = "stream" to count as slid
  local f="$9" bn; bn=$(basename "$f"); total=$((total+1))
  if ! "$W/c" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" < "$f" >| "$W/oc" 2>/dev/null; then
    failed=$((failed+1)); return
  fi
  if ! "$RS" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" < "$f" >| "$W/or" 2>/dev/null; then
    failed=$((failed+1)); return
  fi
  [ "${10:-}" = stream ] && slid=$((slid+1))
  finders_seen="$finders_seen $7"; parsers_seen="$parsers_seen $8"
  local cs rs off pct
  cs=$(wc -c < "$W/oc" | tr -d ' '); rs=$(wc -c < "$W/or" | tr -d ' ')
  if cmp -s "$W/oc" "$W/or"; then
    same=$((same+1))
  elif [ "$cs" -gt "$rs" ] && head -c "$rs" "$W/oc" | cmp -s - "$W/or"; then
    # C is the Rust stream plus trailing bytes: a missing EOPM. The parse agreed,
    # but the driver asks for the marker, so this is a defect and not agreement.
    eopm=$((eopm+1))
  else
    diverged=$((diverged+1))
    # BSD cmp says "differ: char N, line M"; GNU says "differ: byte N, line M".
    # `sed -E`, not BRE: `\(a\|b\)` alternation is a GNU extension and matches
    # nothing under BSD sed, which is how this offset silently printed as "?".
    off=$(cmp "$W/oc" "$W/or" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
    pct=$(python3 -c "print(f'{100*int('${off:-0}')/max(int('$rs'),1):.1f}')" 2>/dev/null || echo "?")
    [ "${#DIVERGE_DETAIL[@]}" -lt 8 ] && DIVERGE_DETAIL+=("  [$1 lc$2 lp$3 pb$4 fb$5 mc$6 mf$7 algo$8] $bn: first differs at byte ${off:-?} of $rs ($pct% in), C=$cs")
  fi
}

for case in "${STREAM_CASES[@]}"; do
  # shellcheck disable=SC2086
  set -- $case
  for f in "$W"/in/stream/*; do
    compare_one "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$f" stream
  done
done

for case in "${FINDER_CASES[@]}"; do
  # shellcheck disable=SC2086
  set -- $case
  for f in "$W"/in/*; do
    [ -f "$f" ] || continue
    compare_one "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$f"
  done
done

for case in "${CASES[@]}"; do
  # shellcheck disable=SC2086
  set -- $case
  for f in "$W"/in/*; do
    [ -f "$f" ] || continue
    bn=$(basename "$f"); total=$((total+1))
    if ! "$W/c" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" < "$f" >| "$W/oc" 2>/dev/null; then
      failed=$((failed+1)); continue
    fi
    if ! "$RS" "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" < "$f" >| "$W/or" 2>/dev/null; then
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
      off=$(cmp "$W/oc" "$W/or" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
      pct=$(python3 -c "print(f'{100*int('${off:-0}')/max(int('$rs'),1):.1f}')" 2>/dev/null || echo "?")
      [ "${#DIVERGE_DETAIL[@]}" -lt 8 ] && DIVERGE_DETAIL+=("  [$1 lc$2 lp$3 pb$4 fb$5 mc$6 mf$7 algo$8] $bn: first differs at byte ${off:-?} of $rs ($pct% in), C=$cs")
    fi
  done
done

n_finders=$(printf '%s\n' $finders_seen | sort -u | wc -l | tr -d ' ')
n_parsers=$(printf '%s\n' $parsers_seen | sort -u | wc -l | tr -d ' ')
echo "lzma: $total comparisons ($slid sliding-window, $n_finders/5 match finders, $n_parsers/2 parsers)"
echo "  byte-identical                        $same"
echo "  Rust missing the end marker           $eopm"
echo "  DIVERGED                              $diverged"
echo "  driver failures                       $failed"
if [ "${#DIVERGE_DETAIL[@]}" -gt 0 ]; then
  echo "first divergences:"
  printf '%s\n' "${DIVERGE_DETAIL[@]}"
fi

# This started as a measurement and is now a GATE: darc-lzma claims byte-identity
# with DArc's C, so anything other than "all identical" is a regression.
#
# `slid` is checked separately and on purpose. Every input in the base corpus is
# smaller than the smallest dictionary, so the window never slides for any of it
# and the base cases cannot distinguish a working window from a broken one. If the
# large-input cases silently stop running, the remaining comparisons would still
# report a clean sweep -- so the absence of streaming coverage has to fail loudly
# rather than pass quietly.
rc=0
[ "$total" -gt 0 ]  || { echo "nothing was measured" >&2; rc=1; }
[ "$slid" -gt 0 ]   || { echo "no sliding-window comparison ran: streaming is unverified" >&2; rc=1; }
# Coverage of the finder and parser axes is gated for the same reason `slid` is:
# for most of this port's life every comparison was mf=BT4/algo=1, while DArc's own
# default is mf=HT4 and its 3binary preset is algo=0. A sweep that silently stops
# covering an axis reports a clean run over the configurations nobody ships.
[ "$n_finders" -eq 5 ] || { echo "only $n_finders/5 match finders were compared" >&2; rc=1; }
[ "$n_parsers" -eq 2 ] || { echo "only $n_parsers/2 parsers were compared" >&2; rc=1; }
[ "$failed" -eq 0 ] || { echo "$failed driver invocation(s) failed" >&2; rc=1; }
[ "$eopm" -eq 0 ]   || { echo "$eopm stream(s) lost the end marker" >&2; rc=1; }
[ "$diverged" -eq 0 ] || { echo "$diverged stream(s) diverged from the pinned C" >&2; rc=1; }
exit $rc
