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
# # The C does not always terminate, and that shapes this harness
#
# GlueFreeBlocks absorbs a following free block with
#
#     while ((p1 = p + p->NU)->Stamp == ~0U) { p->NU += p1->NU;  p1->NU = 0; }
#
# which clears the absorbed block's NU but leaves its Stamp reading ~0U. The
# heap therefore accumulates husks: twelve bytes that still say "free" and claim
# zero units. When a later block's end lands on a husk the loop adds zero,
# rewrites zero, and recomputes the same p1 -- for ever. Seed 42 on an 8 KB heap
# reaches it at operation 146, and the husk it spins on was zeroed by an earlier
# absorb in the same run.
#
# The port breaks out of that loop (see suballoc.rs); the C cannot, so no output
# the C is capable of producing depends on the difference. This harness relies
# on exactly that: every driver run is capped, a C that runs over the cap is
# reported rather than waited on, and the comparison is retried at the largest
# op count the C still returns from. A cap was not optional -- without one this
# script ran 48 minutes in CI and was killed with nothing printed.
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

# The same flag list as ppmd-check.sh, copied from Compression/PPMD/makefile.
# See the long note there: for this codec the optimisation flags are part of the
# format, so the reference is built the way the shipped codec is built rather
# than at whatever -O level seemed reasonable.
PPMD_MAKEFILE_FLAGS="-fno-exceptions -fno-rtti -O1 -fomit-frame-pointer -fno-strict-aliasing -funroll-loops -g0"
cc() { local out="$1"; shift
  # shellcheck disable=SC2086  # the flag list is a word list on purpose
  clang++ -std=c++17 $PPMD_MAKEFILE_FLAGS -w \
    -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/ppmd_alloc_ref.cpp" "$CREF/rust/difftest/ppmd_ccodec.cpp" "$@" -o "$out"; }
cc "$W/c"                    || { echo "C driver failed to build"    >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "Rust driver failed to build" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

# Seconds a single driver run may take. Generous next to the ~1s a terminating
# run needs even on a loaded runner, and small enough that the worst case (every
# combination hitting the glue loop) stays inside a few minutes.
CAP=15
# macOS ships no timeout(1) and no gtimeout(1), and this harness runs on both
# platforms, so the cap is enforced with perl rather than coreutils. 124 on
# expiry, matching timeout(1) so the convention reads the same.
capped() { # capped SECONDS OUTFILE CMD... ; 0 = finished, 124 = ran over
  local secs="$1" out="$2"; shift 2
  perl -e 'my $t = shift;
           my $pid = fork; exit 125 unless defined $pid;
           unless ($pid) { exec @ARGV; exit 127 }
           $SIG{ALRM} = sub { kill 9, $pid; waitpid $pid, 0; exit 124 };
           alarm $t; waitpid $pid, 0; exit $? >> 8' \
       "$secs" "$@" >| "$out" 2>&1
}

# Largest op count <= $3 that the C returns from. Every probe is capped, so the
# search costs at most a handful of CAPs even when the top of the range hangs.
largest_terminating() { # largest_terminating SEED HEAP_KB N
  local seed="$1" heap="$2" hi="$3" lo=1 mid
  while [ $((hi - lo)) -gt 1 ]; do
    mid=$(( (lo + hi) / 2 ))
    if capped "$CAP" "$W/probe" "$W/c" "$seed" "$heap" "$mid"; then lo="$mid"; else hi="$mid"; fi
  done
  echo "$lo"
}

OPS=800
# The small heaps need far fewer ops to reach AllocUnitsRare, and a long run
# there is exactly what walks into the glue loop.
SMALL_OPS=150
# The heap size that reliably drives allocation to FAILURE, which is what
# reaches AllocUnitsRare's last resort and the pText/UnitsStart crossing the
# model restarts on. Measured over seeds 1/7/42 at SMALL_OPS: 2 KB fails 3-5
# times per seed, 8 KB fails not at all -- the unit area there is 595 units and
# this op mix allocates about that many, so it stops just short.
EXHAUST_KB=2
fail=0; ok=0; hung=0
for seed in 1 7 42 1337; do
  # 2 KB exhausts, 8 KB is where seeds 42 and 1337 meet the C's non-terminating
  # glue loop, and the two large sizes stay on the fast path. All three shapes
  # are wanted; none of them subsumes another.
  for heap in "$EXHAUST_KB" 8 256 1024; do
    n="$OPS"; [ "$heap" -le 8 ] && n="$SMALL_OPS"
    # Progress, because a silent harness and a hung one look identical -- which
    # is precisely how the 48-minute CI run went unnoticed until it was killed.
    printf '  seed=%-5s heap=%5sKB ops=%-4s ' "$seed" "$heap" "$n"

    if ! capped "$CAP" "$W/tc" "$W/c" "$seed" "$heap" "$n"; then
      # The C is in the glue loop. First prove the port is NOT -- that is the
      # whole value of the husk break, and an assertion here is what keeps it
      # from rotting into dead code.
      if ! capped "$CAP" "$W/tr" "$W/rs" "$seed" "$heap" "$n"; then
        echo "BOTH drivers ran over ${CAP}s -- the port's husk break is not working"
        fail=$((fail + 1)); continue
      fi
      hung=$((hung + 1))
      n="$(largest_terminating "$seed" "$heap" "$n")"
      printf 'C hangs; retrying at %s ops ' "$n"
      if ! capped "$CAP" "$W/tc" "$W/c" "$seed" "$heap" "$n"; then
        echo "-- the C hangs even at $n ops, nothing to compare"
        fail=$((fail + 1)); continue
      fi
    fi

    if ! capped "$CAP" "$W/tr" "$W/rs" "$seed" "$heap" "$n"; then
      echo "the PORT ran over ${CAP}s where the C returned"
      fail=$((fail + 1)); continue
    fi
    # Two empty traces would compare equal; every run emits at least the init
    # line, so require real output before believing a match.
    if [ ! -s "$W/tc" ]; then
      echo "the C driver produced no trace"; fail=$((fail + 1)); continue
    fi
    if cmp -s "$W/tc" "$W/tr"; then
      echo "ok"
      ok=$((ok + 1))
    else
      echo "DIVERGES from the C"
      diff "$W/tc" "$W/tr" | head -6
      fail=$((fail + 1))
    fi
  done
done

# Coverage: the whole point of the small heaps is to reach AllocUnitsRare and
# GlueFreeBlocks. If nothing ever failed to allocate, the hard paths -- the ones
# that decide where the model restarts -- went untested.
rare=0
for seed in 1 7 42; do
  capped "$CAP" "$W/tc" "$W/c" "$seed" "$EXHAUST_KB" "$SMALL_OPS" || continue
  grep -q -- '-> -1' "$W/tc" && rare=$((rare + 1))
done

[ "$ok" -gt 0 ] || { echo "nothing was compared -- the harness reached nothing"; exit 1; }
[ "$rare" -ge 2 ] || {
  echo "only $rare of 3 seeds ever exhausted the heap; AllocUnitsRare and"
  echo "GlueFreeBlocks are going untested and those decide model restarts"
  fail=$((fail + 1)); }
[ "$hung" -ge 1 ] || {
  echo "no combination reached the C's non-terminating glue loop, so the port's"
  echo "husk break went unexercised. Widen the grid or raise SMALL_OPS until one"
  echo "does, rather than deleting this check -- it is the only thing testing the"
  echo "single place the port deliberately departs from the C."
  fail=$((fail + 1)); }
[ "$fail" -eq 0 ] || { echo "ppmd-alloc: $fail failures"; exit 1; }
echo "ppmd-alloc: $ok/$ok operation traces identical to the C"
echo "ppmd-alloc: (offsets, all four layout cursors and GetUsedMemory after every op;"
echo "ppmd-alloc:  $rare/3 seeds exhausted the heap; $hung combinations reached the C's"
echo "ppmd-alloc:  non-terminating glue loop, where the port returned and the C did not)"
