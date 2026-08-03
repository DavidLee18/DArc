#!/usr/bin/env bash
# LZMA2 MULTI-BLOCK gate: does the Rust reproduce DArc's C when the C is splitting
# the input into blocks?
#
# This is a separate script from lzma2-check.sh on purpose. That one stubs
# GetCompressionThreads() to 1 so the block-splitting axis is isolated out of every
# other comparison; this one is the isolation's other half, and nothing else.
#
# Why the axis exists at all. C_LZMA2.cpp:86-87 sets numTotalThreads and
# numBlockThreads_Max from GetCompressionThreads(), which Cmdline.hs:295 defaults to
# the processor count. Above one block thread Lzma2EncProps_Normalize
# (Lzma2Enc.c:305-324) abandons the SOLID block and splits the input into blocks of
# clamp(dictSize * 4, 1 MiB, 256 MiB), each opening with a dictionary reset. So on
# any multicore machine the stream DArc writes for -mlzma2 is a DIFFERENT stream than
# the single-threaded one, for input larger than that block size.
#
# The port only has to match the OUTPUT, not the threading: MtCoder assigns blocks in
# index order and Lzma2Enc_MtCallback_Write (Lzma2Enc.c:695-710) writes them back in
# index order, so the stream is the ordered concatenation of independently-encoded
# blocks. Encoding them sequentially is byte-identical to encoding them in parallel.
#
# Gates on exit codes, never on grepping tool prose.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
SDK_CFLAGS="$(darc_lzma_sdk_cflags)" || exit 1
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
W="${TMPDIR:-/tmp}/lzma2-mt.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

SDK="$CREF/Compression/LZMA/7z24"
# NOTE: deliberately no -DZ7_ST. Under Z7_ST the SDK compiles MtCoder/MtDec out and
# LzmaEncProps_Normalize's numThreads default flips, so a Z7_ST oracle cannot see
# this axis at all -- it would report agreement by being unable to disagree.
DEFS="-DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT"
objs=""
# Alloc.c is omitted: it duplicates symbols Common.cpp already provides, and the
# wrapper supplies its own ISzAlloc.
for c in LzmaEnc LzmaDec LzFind LzFindOpt LzFindMt Threads CpuArch 7zStream \
         Lzma2Enc Lzma2Dec MtCoder MtDec; do
  # shellcheck disable=SC2086
  clang -c $SDK_CFLAGS -w $DEFS -I"$SDK" -o "$W/$c.o" "$SDK/$c.c" 2>>"$W/build.log" \
    || { echo "compiling SDK $c.c failed" >&2; tail -20 "$W/build.log" >&2; exit 1; }
  objs="$objs $W/$c.o"
done
# shellcheck disable=SC2086
clang++ -std=c++17 $CFLAGS_C -w $DEFS -I"$CREF" -I"$CREF/Compression" -I"$SDK" \
  "$CREF/rust/difftest/lzma2_ref.cpp" "$CREF/Compression/Common.cpp" $objs -lpthread \
  -o "$W/c" 2>>"$W/build.log"
[ -x "$W/c" ] || { echo "building the pinned C LZMA2 driver failed:" >&2; tail -25 "$W/build.log" >&2; exit 1; }

RS="${LZMA2_MT_RS:-$ROOT/rust/target/release/lzma2_rs_ref}"
if [ ! -x "$RS" ]; then
  ( cd "$ROOT/rust" && cargo build --release -p darc-lzma --bin lzma2_rs_ref ) >/dev/null 2>&1
fi
have_rust=0; [ -x "$RS" ] && have_rust=1

# ---- corpus: sizes chosen against the 4 MiB block that dictSize=1m produces ------
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" lzma2-mt "$W/in"

# dictSize lc lp pb fb mc mf algo -- dict 1m so the auto block is 4 MiB and a
# few-megabyte corpus spans several blocks without needing gigabytes.
PARAMS="1048576 3 0 2 32 0 4 1"
THREADS="1 2 4 8"

total=0; identical=0; diverged=0; failed=0; multiblock=0
declare -a FIND=()
c_out_by_threads=""

for f in "$W"/in/*; do
  bn=$(basename "$f")
  for t in $THREADS; do
    total=$((total+1))
    # shellcheck disable=SC2086
    if ! DARC_LZMA2_THREADS=$t "$W/c" $PARAMS enc < "$f" >| "$W/oc" 2>/dev/null; then
      failed=$((failed+1)); FIND+=("  C driver failed: $bn threads=$t"); continue
    fi
    # Count blocks from the stream itself: a dictionary reset is control 0xE0..0xFF
    # (mode 3) or a COPY_RESET_DIC (0x01), and there is exactly one per block.
    blocks=$(python3 - "$W/oc" <<'PYEOF'
import sys
# Count dictionary resets, which is exactly one per block (Lzma2Enc.c:106-111 sets
# needInitProp and needInitState together at every block start, so the first chunk of
# a block is mode 3 -- control >= 0xE0 -- or a COPY_RESET_DIC, control == 1).
#
# Chunk layout, Lzma2Enc.c:197-225 and :168-191:
#   control 0x00            end of stream
#   control >= 0x80         LZMA:  5 header bytes, +1 prop byte when control >= 0xC0,
#                                  then packSize payload bytes
#   control 1 or 2          copy:  3 header bytes then unpackSize raw bytes
# Both sizes are stored minus one.
b = open(sys.argv[1], "rb").read()
i, n = 1, 0                     # byte 0 is the LZMA2 property byte
while i < len(b):
    c = b[i]
    if c == 0:
        break
    if c >= 0x80:
        if i + 5 > len(b):
            break
        pack = ((b[i+3] << 8) | b[i+4]) + 1
        i += 5 + (1 if c >= 0xC0 else 0) + pack
        if c >= 0xE0:
            n += 1
    elif c in (1, 2):
        if i + 3 > len(b):
            break
        unpack = ((b[i+1] << 8) | b[i+2]) + 1
        i += 3 + unpack
        if c == 1:
            n += 1
    else:
        break
print(n)
PYEOF
) || blocks=0
    if [ "${blocks:-0}" -gt 1 ]; then multiblock=$((multiblock+1)); fi
    c_out_by_threads="$c_out_by_threads $(wc -c < "$W/oc" | tr -d ' ')"

    if [ "$have_rust" -eq 1 ]; then
      # shellcheck disable=SC2086
      if ! DARC_LZMA2_THREADS=$t "$RS" $PARAMS enc < "$f" >| "$W/or" 2>/dev/null; then
        failed=$((failed+1)); FIND+=("  Rust driver failed: $bn threads=$t"); continue
      fi
      if cmp -s "$W/oc" "$W/or"; then identical=$((identical+1))
      else
        diverged=$((diverged+1))
        off=$(cmp "$W/oc" "$W/or" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
        [ "${#FIND[@]}" -lt 10 ] && FIND+=("  DIVERGE $bn threads=$t: first differs at byte ${off:-?}; C=$(wc -c < "$W/oc" | tr -d ' ') Rust=$(wc -c < "$W/or" | tr -d ' ')")
      fi
    fi
  done
done

echo "lzma2-mt: $total C runs over $(ls "$W"/in | wc -l | tr -d ' ') inputs x $(echo $THREADS | wc -w | tr -d ' ') thread counts"
echo "  runs whose stream had >1 block        $multiblock"
if [ "$have_rust" -eq 1 ]; then
  echo "  byte-identical to the C               $identical"
  echo "  DIVERGED                              $diverged"
fi
echo "  driver failures                       $failed"
[ "${#FIND[@]}" -gt 0 ] && printf '%s\n' "${FIND[@]}"

rc=0
[ "$total" -gt 0 ]   || { echo "nothing was measured" >&2; rc=1; }
[ "$failed" -eq 0 ]  || { echo "$failed driver invocation(s) failed" >&2; rc=1; }
# The coverage gate, and it is the point of the script: if no run ever produced more
# than one block then the C never took the splitting path, and every comparison above
# is a single-block comparison lzma2-check.sh already makes. A green run that proves
# nothing is worse than a red one.
[ "$multiblock" -gt 0 ] || { echo "no run produced a multi-block stream: the splitting path was never exercised" >&2; rc=1; }
if [ "$have_rust" -eq 1 ]; then
  [ "$diverged" -eq 0 ] || { echo "$diverged stream(s) diverged from the pinned C" >&2; rc=1; }
else
  echo "  Rust: ABSENT -- C-only run; set LZMA2_MT_REQUIRE_RUST=1 to make that a failure"
  [ -z "${LZMA2_MT_REQUIRE_RUST:-}" ] || { echo "no Rust LZMA2 driver was found" >&2; rc=1; }
fi
exit $rc
