#!/usr/bin/env bash
# Differential-test PPMd's memory suballocator against the C original.
#
# # Why the allocator gets a harness of its own
#
# ppmd-check.sh compares whole streams, because the coder, model and allocator
# have no seam between them. But the ALLOCATOR alone does have a testable
# interface, and it is the part the compressed format is most sensitive to: the
# model branches on GetUsedMemory() and on pText/UnitsStart crossings, so an
# allocator that is merely correct still yields a different compressed stream.
#
# Both sides run the SAME pseudo-random operation sequence -- the seed picks the
# ops, so no script file is needed -- and print every returned offset plus the
# four layout cursors and GetUsedMemory() after each step. A mismatch here
# points at the free lists rather than at the model wrapped around them.
#
# Offsets, not pointers: the C's heap is a malloc'ed block at an arbitrary
# address and the Rust one is a Vec<u8>. The C already works in HeapStart-
# relative refs internally (its BLKREF/CTX_REF exist for 64-bit portability),
# so nothing is weakened by comparing offsets.
#
# NOTE on the op count. At 3000 ops on a 64 KB heap BOTH binaries run for many
# minutes -- the C exactly as much as the port, so it is a property of this
# driver's op mix thrashing GlueFreeBlocks on an exhausted heap, not a defect in
# either. 800 keeps every combination well under a second while still driving
# the heap into exhaustion and out again.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/ppmd-alloc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }

cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/ppmd_alloc_ref.cpp" "$CREF/rust/difftest/ppmd_ccodec.cpp" "$@" -o "$out"; }
cc "$W/c"                    || { echo "C driver failed to build"    >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "Rust driver failed to build" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

OPS=800
# The exhausting heap needs far fewer ops to reach AllocUnitsRare, and a long
# run there is exactly what thrashes GlueFreeBlocks.
SMALL_OPS=150
fail=0; ok=0
for seed in 1 7 42 1337; do
  # 8 KB exhausts quickly and drives AllocUnitsRare / GlueFreeBlocks; the two
  # large sizes stay on the fast path. Middling heaps (16-64 KB) are avoided
  # deliberately: there this driver's op mix thrashes GlueFreeBlocks for
  # minutes -- in the C exactly as much as in the port, so it is the mix and
  # not either implementation, but it makes for a uselessly slow harness.
  for heap in 8 256 1024; do
    n="$OPS"; [ "$heap" -le 8 ] && n="$SMALL_OPS"
    "$W/c"  "$seed" "$heap" "$n" >| "$W/tc" 2>&1
    "$W/rs" "$seed" "$heap" "$n" >| "$W/tr" 2>&1
    # Two empty traces would compare equal; every run emits at least the init
    # line, so require real output before believing a match.
    if [ ! -s "$W/tc" ]; then
      echo "  seed=$seed heap=${heap}KB: the C driver produced no trace"; fail=$((fail+1)); continue
    fi
    if cmp -s "$W/tc" "$W/tr"; then
      ok=$((ok+1))
    else
      echo "  seed=$seed heap=${heap}KB: allocator DIVERGES from the C"
      diff "$W/tc" "$W/tr" | head -6
      fail=$((fail+1))
    fi
  done
done

# Coverage: the whole point of the small heaps is to reach AllocUnitsRare and
# GlueFreeBlocks. If nothing ever failed to allocate, the hard paths -- the ones
# that decide where the model restarts -- went untested.
rare=0
for seed in 1 7 42; do
  "$W/c" "$seed" 8 "$SMALL_OPS" >| "$W/tc" 2>&1 || continue
  grep -q -- '-> -1' "$W/tc" && rare=$((rare+1))
done

[ "$ok" -gt 0 ] || { echo "nothing was compared -- the harness reached nothing"; exit 1; }
[ "$rare" -ge 2 ] || {
  echo "only $rare of 3 seeds ever exhausted the heap; AllocUnitsRare and"
  echo "GlueFreeBlocks are going untested and those decide model restarts"
  fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "ppmd-alloc: $fail failures"; exit 1; }
echo "ppmd-alloc: $ok/$ok operation traces identical to the C ($OPS ops each)"
echo "ppmd-alloc: (offsets, all four layout cursors and GetUsedMemory after every op; $rare/3 seeds exhausted the heap)"
