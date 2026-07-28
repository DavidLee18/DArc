#!/usr/bin/env bash
# Differential-test PPMd var.H against the C original.
#
# # Why this harness cuts at the whole stream, and cannot cut lower
#
# Every other codec here is compared stage by stage. PPMd cannot be, because its
# output is a function of its MEMORY SUBALLOCATOR, not just of its input:
#
#   Model.cpp:245  GetUsedMemory() < (SubAllocatorSize >> 1)   decides restart
#   Model.cpp:416  if (pText >= UnitsStart) goto RESTART_MODEL
#   Model.cpp:418  if ((BYTE*) FSuccessor < UnitsStart)
#
# Measured before the port was written, so this is not an inference from
# reading: the same 200 KB input at order 16 encodes to 204797 / 205303 /
# 206007 / 206098 bytes at a 1 / 2 / 4 / 8 MB budget, all four with different
# contents. MRMethod 2 (freeze) diverges from 0 and 1 as well.
#
# The consequence for the port is the opposite of libsais, where any correct
# suffix array reproduced the C: here there is NO algorithmic latitude at all.
# SubAlloc.hpp must be reproduced exactly -- free lists, unit sizes, glue
# behaviour and the resulting addresses -- because a merely-correct allocator
# reports a different GetUsedMemory(), crosses UnitsStart at a different moment,
# restarts the model somewhere else, and changes every byte from there on.
#
# The coder, the model and the allocator are one system with no seam, so the
# stream is the only honest cut.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/ppmd-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }

# THE OPTIMISATION FLAGS ARE PART OF THE FORMAT. Copy them from
# Compression/PPMD/makefile, do not choose them.
#
# PPMd's StateCpy/SWAP type-pun through `(WORD&)` references, which violates
# strict aliasing. A compiler permitted to assume those WORD writes cannot alias
# the BYTE read in rescale()'s `if (p->Freq == 0)` reuses the value ASSIGNED
# earlier in the loop rather than reloading the slot the bubble sort has since
# overwritten -- and the two readings produce DIFFERENT COMPRESSED BYTES from
# the same source. Measured on the `dominant` corpus shape at orders 3/4/10/16:
#
#     -O1                        reuses the assigned value
#     -O2                        reuses the assigned value
#     -O0                        re-reads
#     -O1 -fno-strict-aliasing   re-reads
#
# So pinning the -O level is NOT sufficient, and an oracle built at a bare -O1
# is wrong: the makefile passes -fno-strict-aliasing, which is what real -mppmd
# archives were written with. Isolated by flag -- -fno-strict-aliasing alone
# flips the output; -fomit-frame-pointer and -funroll-loops do not.
#
# compile-win64-c carries the same warning for the same reason: a Win64 build
# whose PPMD flags drift from this list writes archives Linux cannot reproduce.
#
# There is a second reason to prefer this oracle: with -fno-strict-aliasing the
# answer no longer depends on how clever the compiler's alias analysis is, so it
# is stable across compilers and versions. The bare -O1 reading was not.
PPMD_MAKEFILE_FLAGS="-fno-exceptions -fno-rtti -O1 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops -g0"
cc() { local out="$1"; shift
  # shellcheck disable=SC2086  # the flag list is a word list on purpose
  clang++ -std=c++17 $PPMD_MAKEFILE_FLAGS -w \
    -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/ppmd_ref.cpp" "$CREF/rust/difftest/ppmd_ccodec.cpp" "$@" -o "$out"; }
cc "$W/c"                    || { echo "C driver failed to build"    >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "Rust driver failed to build" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

# The corpus targets what the MODEL is sensitive to: order (how much context is
# tracked), and memory pressure (whether the model restarts mid-stream). Sizes
# are chosen so the small budgets below genuinely exhaust the allocator.
python3 - "$W/in" <<'CORPUS'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
def _dom():
    # Small on purpose: PPMd is pathologically slow on this shape at low order
    # (minutes for 8 KB in the C as much as in the port), and 1.5 KB already
    # reaches the rescale path this input exists to cover.
    import random
    r = random.Random(3)
    return r.choices(range(256), weights=[4000] + [1]*255, k=1500)
w("text",       b"the quick brown fox jumps over the lazy dog. "*6000)
w("english",    (b"compression algorithms rearrange data so that statistical "
                 b"redundancy can be removed by an entropy coder. ")*2500)
w("noise",      prng(7, 200000))          # exhausts a small budget
w("zeros",      b"\x00"*120000)
w("runs",       b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(2000)))
w("full_alpha", bytes(range(256))*500)
w("sparse",     b"".join(b"\x00"*300 + bytes([i%251]) for i in range(400)))
w("binaryish",  bytes((i*2654435761>>16)&0xff for i in range(150000)))
# These three exhaust a 1 MB model and so reach the restart paths. Measured:
# high-entropy data at 200 KB+ is what fills the heap; text and short runs
# compress far too well to ever get there.
# A dominant symbol plus a long tail of rare ones, at LOW order. This is the
# only shape found that reaches rescale's shrink path -- zero-frequency states
# dropped while the context keeps more than one -- and it is where the port's
# one real bug lived: `EscFreq` is UINT in rescale but int in refresh, so a
# signed port diverges exactly when it wraps. Every other input in this corpus
# passed with that bug present.
w("dominant",   bytes(_dom()))
w("bignoise",   prng(3, 600000))
w("mixed",      prng(5, 300000) + b"the quick brown fox "*5000)
for n in (1,2,3,17,255,256,4096,65536):
    w(f"n_{n}", (b"abracadabra"*10000)[:n])
CORPUS

fail=0; enc=0; dec=0; declined=0

# order: PPMd var.H accepts 2..64. mem in MB. mrm: 0 restart, 1 cut off, 2 freeze.
# The grid is parameterised so a laptop can run a cheap slice while CI runs the
# whole thing. Order 2 is deliberately absent even from the full grid: PPMd is
# pathologically slow on the `dominant` shape there, in the C as much as in the
# port, so including it buys nothing but wall-clock.
# Two properties byte-identity cannot see, checked once before the grid starts
# sending stderr to /dev/null and stops timing anything.
#
# They are not hypothetical. A per-symbol `eprintln!` survived in the port
# through 798 byte-identical streams: every comparison passed, the trace went to
# /dev/null, and the only visible symptom was a CI job that took 634 seconds
# instead of twenty. In the shipped archiver it would have flooded the terminal
# with a line per input byte.
for drv in c rs; do
  "$W/$drv" c 8 8 0 < "$W/in/text" >| "$W/q" 2>| "$W/qerr"
  [ -s "$W/qerr" ] || continue
  echo "the $drv driver wrote to stderr during a normal encode:"
  head -3 "$W/qerr"
  echo "a codec must be silent -- this would print into the archiver's console"
  exit 1
done
now() { perl -MTime::HiRes=time -e 'print time'; }
t0=$(now); "$W/c"  c 8 8 0 < "$W/in/bignoise" >|/dev/null 2>&1
t1=$(now); "$W/rs" c 8 8 0 < "$W/in/bignoise" >|/dev/null 2>&1
t2=$(now)
c_secs=$(perl -e 'printf "%.3f", $ARGV[1]-$ARGV[0]' "$t0" "$t1")
r_secs=$(perl -e 'printf "%.3f", $ARGV[1]-$ARGV[0]' "$t1" "$t2")
# A ratio, not a wall clock, so it does not care how fast the runner is. Ten is
# far above the ~2x a faithful port costs in bounds checks and byte-offset
# arithmetic, and far below what stray I/O in the per-symbol path produces --
# the eprintln! above measured 470x. Skipped rather than guessed if the
# reference run was too short to time.
if awk -v c="$c_secs" 'BEGIN { exit (c >= 0.05) ? 0 : 1 }'; then
  ratio=$(perl -e 'printf "%.1f", $ARGV[0]/$ARGV[1]' "$r_secs" "$c_secs")
  echo "ppmd: encode speed vs the C on 600 KB: ${ratio}x (${c_secs}s -> ${r_secs}s)"
  awk -v r="$ratio" 'BEGIN { exit (r <= 10) ? 0 : 1 }' || {
    echo "${ratio}x is not port overhead. Look for I/O, an allocation, or a"
    echo "rebuilt table in the per-symbol path."
    exit 1; }
else
  echo "ppmd: the C reference encoded 600 KB in ${c_secs}s, too fast to time a ratio against"
fi

ORDERS="${PPMD_ORDERS:-3 4 10 16}"
MEMS="${PPMD_MEMS:-1 8}"
for order in $ORDERS; do
  for mem in $MEMS; do
    # One line per (order, budget), because this grid runs for tens of minutes
    # and printed nothing until the end. A silent run and a hung one then look
    # identical -- which is exactly how a sibling harness burned 48 minutes of a
    # CI job before anyone could tell it was stuck.
    # Printed BEFORE the work, not after: the point is to name the combination
    # a stuck run is stuck on, and a line printed on completion cannot do that.
    echo "  order=$order mem=${mem}M"
    for mrm in 0 1 2; do
      for f in "$W"/in/*; do
        bn="$(basename "$f") o$order m${mem}M mrm$mrm"

        "$W/c"  c "$order" "$mem" "$mrm" < "$f" >| "$W/ec" 2>/dev/null; rc_c=$?
        "$W/rs" c "$order" "$mem" "$mrm" < "$f" >| "$W/er" 2>/dev/null; rc_r=$?
        if [ "$rc_c" -ne "$rc_r" ]; then
          echo "  $bn: encode driver exit differs (C $rc_c, Rust $rc_r)"; fail=$((fail+1)); continue
        fi
        [ "$rc_c" -eq 0 ] || { declined=$((declined+1)); continue; }
        # Both drivers always emit the 4-byte return code, so an empty file
        # means the driver itself failed rather than the codec declining.
        [ -s "$W/ec" ] || { echo "  $bn: the C driver produced no output"; fail=$((fail+1)); continue; }
        enc=$((enc+1))
        cmp -s "$W/ec" "$W/er" || { echo "  $bn: ENCODED stream differs from the C"; fail=$((fail+1)); continue; }

        # Decode the C's stream with both, which catches a decoder that is
        # merely self-consistent with a wrong encoder.
        tail -c +5 "$W/ec" >| "$W/payload"
        "$W/c"  d "$order" "$mem" "$mrm" < "$W/payload" >| "$W/dc" 2>/dev/null
        "$W/rs" d "$order" "$mem" "$mrm" < "$W/payload" >| "$W/dr" 2>/dev/null
        tail -c +5 "$W/dc" >| "$W/dc.p"; tail -c +5 "$W/dr" >| "$W/dr.p"
        cmp -s "$f" "$W/dc.p" || { echo "  $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
        dec=$((dec+1))
        cmp -s "$f" "$W/dr.p" || { echo "  $bn: RUST decode != original"; fail=$((fail+1)); }
      done
    done
  done
done

# Coverage: the whole point of the memory axis is to reach a model restart. If
# the 1 MB budget never differs from the 8 MB one, the allocator pressure that
# makes this codec hard is going untested.
restarts=0
for f in "$W"/in/noise "$W"/in/bignoise "$W"/in/mixed; do
  "$W/c" c 16 1 0 < "$f" >| "$W/a" 2>/dev/null || continue
  "$W/c" c 16 8 0 < "$f" >| "$W/b" 2>/dev/null || continue
  cmp -s "$W/a" "$W/b" || restarts=$((restarts+1))
done

[ "$enc" -gt 0 ] || { echo "nothing was encoded -- the harness reached nothing"; exit 1; }
[ "$restarts" -ge 2 ] || {
  echo "only $restarts of 3 inputs behaved differently at 1 MB vs 8 MB; the"
  echo "memory-exhaustion path is not being exercised"; fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "ppmd: $fail failures"; exit 1; }
echo "ppmd: $enc/$enc streams byte-identical to the C, $dec decoded back"
echo "ppmd: ($declined declined by both; $restarts/3 inputs reached memory exhaustion)"
