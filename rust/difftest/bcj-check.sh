#!/usr/bin/env bash
# Differential-test the BCJ x86 (`exe`) filter port -- BOTH directions -- against
# the C original, at many read granularities.
#
# `exe` rewrites the displacement of every E8/E9 (CALL/JMP rel32) into an
# absolute target. Two things make it easy to get subtly wrong:
#
#   * It carries state across calls. `_bufferPos` is the absolute stream offset
#     and is ADDED to every displacement; `_prevMask` remembers which of the
#     last three positions held a branch byte. A port that resets either between
#     buffers round-trips perfectly when the whole input arrives in one read and
#     corrupts when it does not. Hence the chunk-size sweep: the same input is
#     fed as one buffer and as many, and the C and the Rust must agree at every
#     granularity.
#   * Random data essentially never contains E8/E9 at a position whose 5th byte
#     is 0x00 or 0xFF, so a corpus of noise would pass with the filter stubbed
#     out entirely. The corpus therefore carries real x86 machine code where the
#     host has any, plus synthetic code and hand-placed branches at every
#     alignment and across the 256 KiB buffer boundary -- and the run FAILS if
#     the corpus turns out to hold no branch bytes, or if encoding never
#     actually changed anything.
#
# Everything is gated on exit codes and byte comparisons; no tool prose is
# parsed. The C reference comes from a pinned revision, not the working tree --
# see c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds the LZMA directory's C++ wrappers
# (C_BCJ.cpp lives there): see darc_codec_cflags in c-reference.sh for why the
# makefile's flags, not an -O level, are the oracle.
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
W="${TMPDIR:-/tmp}/bcj-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# The staticlib goes AFTER the source that references it: GNU ld resolves an
# archive only against the undefined symbols it has already seen. macOS ld does
# not care, which is how three harnesses here once shipped never having linked.
cc() { # cc <output> [extra args, appended after the source]
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$CREF/rust/difftest/bcj_ref.cpp" "${@:2}" -o "$1"
}
cc "$W/c"                    || { echo "building the C reference failed" >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "building the Rust driver failed"  >&2; exit 1; }

# ── Corpus ───────────────────────────────────────────────────────────────────
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen --bin difftest-util ) || exit 1
UTIL="$ROOT/rust/target/release/difftest-util"

# The synthetic corpus. The real x86-64 binaries below are found here in shell,
# because scanning the host is orchestration -- difftest-util only answers "is
# this file x86-64, and if so give me up to N bytes of it".
"$ROOT/rust/target/release/corpusgen" bcj "$W/in"

# Not "a binary" -- a binary for the RIGHT architecture. An arm64 executable is
# as branch-free as noise for this filter, and would make the corpus look richer
# than it is. ELF, Mach-O (including the x86_64 slice of a fat binary) and PE are
# all recognised, so this finds something on a Linux runner, on an Apple-silicon
# Mac, and in a Windows cross-build tree.
real=0
cands="/bin/ls /bin/bash /usr/bin/grep /usr/bin/python3 /usr/lib/dyld
/lib/x86_64-linux-gnu/libc.so.6 /usr/lib/x86_64-linux-gnu/libc.so.6
/usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/bin/perl /usr/bin/openssl
$ROOT/Tests/arc-mhs-win64.exe $ROOT/Tests/arc"
for d in /tmp/out/FreeArc /tmp/out/FreeArc-unarc; do
  [ -d "$d" ] && cands="$cands $(ls "$d"/*.o 2>/dev/null | sort | head -4)"
done
for c in $cands; do
  [ "$real" -ge 3 ] && break
  "$UTIL" x86-bytes "$c" 1200000 > "$W/cand.bin" 2>/dev/null || continue
  sz=$(wc -c < "$W/cand.bin" | tr -d ' ')
  # Small object files are fine, empty ones are not.
  [ "${sz:-0}" -ge 8192 ] || continue
  mv "$W/cand.bin" "$W/in/real${real}_$(basename "$c")"
  real=$((real + 1))
done
rm -f "$W/cand.bin"

# What the corpus actually contains, so a run that generated almost nothing
# cannot report a pass.
"$UTIL" bcj-manifest "$W/in" "$real" > "$W/manifest"
read -r _f _t _b _r < "$W/manifest"
echo "corpus: $_f files, $_t bytes, $_b E8/E9 bytes, $_r real x86 binaries"

read -r NFILES NBYTES NBRANCH NREAL < "$W/manifest" || exit 1
# A corpus with no branch bytes would pass with the filter stubbed out. This is
# the check that makes the rest of the run mean something.
[ "$NBRANCH" -ge 500 ] || { echo "corpus holds only $NBRANCH E8/E9 bytes -- it would not exercise the filter"; exit 1; }

# ── Compare ──────────────────────────────────────────────────────────────────
# Small inputs get every chunk size; big ones get the interesting subset, since
# a 1 MB file at chunk 6 is 170k round trips through the callback.
SMALL_CHUNKS="0 1 2 3 4 5 6 7 9 64 4096 65536 262143 262144 300000"
BIG_CHUNKS="0 6 7 4096 65536 262143 262144"

fail=0; cmps=0; ident=0; filtered=0; roundtrips=0
for f in "$W"/in/*; do
  bn=$(basename "$f"); sz=$(wc -c < "$f" | tr -d ' ')
  chunks=$SMALL_CHUNKS
  [ "$sz" -gt 100000 ] && chunks=$BIG_CHUNKS
  for ch in $chunks; do
    rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr" "$W/rt" "$W/xrt"

    "$W/c"  c "$ch" < "$f" >| "$W/ec" 2>/dev/null || { echo "  $bn [chunk $ch]: C encode failed";    fail=$((fail+1)); continue; }
    "$W/rs" c "$ch" < "$f" >| "$W/er" 2>/dev/null || { echo "  $bn [chunk $ch]: Rust encode failed"; fail=$((fail+1)); continue; }
    "$W/c"  d "$ch" < "$f" >| "$W/dc" 2>/dev/null || { echo "  $bn [chunk $ch]: C decode failed";    fail=$((fail+1)); continue; }
    "$W/rs" d "$ch" < "$f" >| "$W/dr" 2>/dev/null || { echo "  $bn [chunk $ch]: Rust decode failed"; fail=$((fail+1)); continue; }

    # 1. encode: byte-identical streams
    cmps=$((cmps+1))
    if cmp -s "$W/ec" "$W/er"; then ident=$((ident+1))
    else echo "  $bn [chunk $ch]: ENCODE differs from the C"; fail=$((fail+1)); fi

    # 2. decode: the filter is symmetric and defined on any input, so the raw
    #    corpus is a valid decoder corpus too.
    cmps=$((cmps+1))
    if cmp -s "$W/dc" "$W/dr"; then ident=$((ident+1))
    else echo "  $bn [chunk $ch]: DECODE differs from the C"; fail=$((fail+1)); fi

    # Did encoding change anything at all? Counted, and required to be nonzero
    # overall: a stubbed filter would be byte-identical to a stubbed filter.
    cmp -s "$f" "$W/ec" || filtered=$((filtered+1))

    # 3. round trip through the Rust, and 4. across the two implementations
    #    (C encode -> Rust decode), which no single-implementation bug survives.
    "$W/rs" d "$ch" < "$W/er" >| "$W/rt"  2>/dev/null
    "$W/rs" d "$ch" < "$W/ec" >| "$W/xrt" 2>/dev/null
    cmps=$((cmps+2))
    if cmp -s "$f" "$W/rt"; then roundtrips=$((roundtrips+1))
    else echo "  $bn [chunk $ch]: Rust round trip differs from the input"; fail=$((fail+1)); fi
    if cmp -s "$f" "$W/xrt"; then roundtrips=$((roundtrips+1))
    else echo "  $bn [chunk $ch]: Rust decode of the C stream differs from the input"; fail=$((fail+1)); fi
  done
done

[ "$cmps" -gt 0 ] || { echo "bcj: no comparisons ran -- the harness reached nothing"; exit 1; }
# Chunk sizes of 5 and below never invoke the filter (C_BCJ.cpp:20 bypasses
# anything <= 5 bytes), and plenty of the corpus is deliberately branch-free, so
# "some runs changed nothing" is expected -- "almost no run changed anything"
# means the filter is not running. A full run makes this ~110.
[ "$filtered" -ge 20 ] || { echo "bcj: only $filtered runs changed a byte -- the filter is not running"; exit 1; }
[ "$fail" -eq 0 ] || { echo "bcj: $fail failures over $cmps comparisons"; exit 1; }

echo "bcj: $ident/$((cmps/2)) streams byte-identical to the C and $roundtrips round trips exact"\
     "-- $NFILES inputs ($NBRANCH E8/E9 bytes, $NREAL real x86 binaries) x up to 15 chunk sizes,"\
     "$filtered runs actually filtered"
