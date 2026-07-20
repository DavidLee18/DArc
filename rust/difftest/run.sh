#!/usr/bin/env bash
# Differential-test a ported codec against the C original.
#
#   rust/difftest/run.sh build      build the C reference driver
#   rust/difftest/run.sh selftest   round-trip the C original over sample inputs
#   rust/difftest/run.sh diff       compare C vs Rust output byte for byte
#   rust/difftest/run.sh sabotage   prove the comparison can actually fail
#
# The C original is the oracle. These codecs define the archive format, so a
# port has to be bit-exact, not merely correct: output that decompresses fine
# but differs byte-wise produces archives older builds cannot read.
#
# Comparing whole archives via Tests/run-tests.sh also catches that, but only
# one bit of signal per run and only over inputs the corpus happens to contain.
# Driving the codec directly makes it cheap to throw thousands of inputs at both
# implementations.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
cd "$ROOT"
REF=${REF:-/tmp/darc-delta-ref}
RS=${RS:-/tmp/darc-delta-rs}
WORK=${WORK:-${TMPDIR:-/tmp}/darc-difftest.$$}

build_ref () {
  # DELTA_LIBRARY suppresses Delta.cpp's own main(); Common.cpp supplies
  # MyAlloc/MyFree. Both are easy to omit and produce link errors that look
  # unrelated to the driver.
  clang++ -std=c++17 -O2 -w \
    -DDELTA_LIBRARY -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I Compression -I . \
    -o "$REF" "$HERE/delta_ref.cpp" Compression/Delta/Delta.cpp Compression/Common.cpp \
    || { echo "error: failed to build the reference driver" >&2; return 1; }
  echo "built $REF"
}

# Inputs chosen to reach different parts of the table detector, not just the
# easy path: a real binary has genuine relocation tables, the synthetic ones
# have known column structure, and the degenerate sizes probe the boundaries
# where this codec family has historically gone wrong (empty, 1 byte, and
# smaller than one record).
make_inputs () {
  mkdir -p "$WORK/in"
  head -c 200000 /dev/urandom            > "$WORK/in/random.bin"
  head -c 300000 /bin/bash 2>/dev/null   > "$WORK/in/binary.bin" || \
    head -c 300000 /bin/sh               > "$WORK/in/binary.bin"
  : > "$WORK/in/empty.bin"
  printf 'x'                             > "$WORK/in/one-byte.bin"
  head -c 7 /dev/urandom                 > "$WORK/in/tiny.bin"
  python3 "$HERE/make_inputs.py" "$WORK/in"
}

selftest () {
  [ -x "$REF" ] || build_ref || return 1
  make_inputs
  local fail=0
  for f in "$WORK"/in/*; do
    local n; n=$(wc -c < "$f" | tr -d ' ')
    if ! "$REF" c >| "$WORK/packed" < "$f" 2>"$WORK/err"; then
      echo "  $(basename "$f"): compress FAILED ($(tr -d '\n' < "$WORK/err"))"; fail=$((fail+1)); continue
    fi
    if ! "$REF" d >| "$WORK/back" < "$WORK/packed" 2>"$WORK/err"; then
      echo "  $(basename "$f"): decompress FAILED ($(tr -d '\n' < "$WORK/err"))"; fail=$((fail+1)); continue
    fi
    if cmp -s "$f" "$WORK/back"; then
      printf "  %-20s %8s bytes  round-trip OK\n" "$(basename "$f")" "$n"
    else
      printf "  %-20s %8s bytes  ROUND-TRIP MISMATCH\n" "$(basename "$f")" "$n"; fail=$((fail+1))
    fi
  done
  rm -rf "$WORK"
  [ "$fail" -eq 0 ] && echo "reference self-test: all inputs round-trip" \
                    || { echo "reference self-test: $fail failed" >&2; return 1; }
}

# Build the same driver against the Rust port and diff it with the C original.
build_rs () {
  ( cd "$ROOT/rust/darc-codecs" && cargo build --release ) >/dev/null 2>&1 \
    || { echo "error: cargo build failed" >&2; return 1; }
  clang++ -std=c++17 -O2 -w \
    -DUSE_RUST -DDELTA_LIBRARY -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I Compression -I . \
    -o "$RS" "$HERE/delta_ref.cpp" Compression/Delta/Delta.cpp Compression/Common.cpp \
    "$ROOT/rust/darc-codecs/target/release/libdarc_codecs.a" \
    || { echo "error: failed to build the Rust driver" >&2; return 1; }
}

# Compare C and Rust decompression of the same C-produced stream. `$1` names the
# Rust binary so the sabotage check can point this at a deliberately broken one.
#
# Contract: per-file detail goes to stderr, and stdout carries ONLY "<n> <fail>".
# The caller captures stdout, so anything else printed there is parsed as a
# count -- which silently produced an empty tally the first time round.
compare_with () {
  local rs="$1" quiet="${2:-}" fail=0 n=0
  for f in "$WORK"/in/*; do
    "$REF" c >| "$WORK/packed" < "$f" 2>/dev/null || continue
    "$REF" d >| "$WORK/c_out"  < "$WORK/packed" 2>/dev/null || continue
    n=$((n+1))
    "$rs" d >| "$WORK/r_out" < "$WORK/packed" 2>/dev/null || true
    if cmp -s "$WORK/c_out" "$WORK/r_out" && cmp -s "$f" "$WORK/r_out"; then
      [ -n "$quiet" ] || printf "  %-18s %9s bytes  C==Rust, round-trips\n" \
        "$(basename "$f")" "$(wc -c <"$f" | tr -d ' ')" >&2
    else
      [ -n "$quiet" ] || printf "  %-18s DIFFERS FROM C\n" "$(basename "$f")" >&2
      fail=$((fail+1))
    fi
  done
  echo "$n $fail"
}

diff_impls () {
  [ -x "$REF" ] || build_ref || return 1
  build_rs || return 1
  make_inputs
  local res n fail
  res=$(compare_with "$RS"); n=${res%% *}; fail=${res##* }
  echo "delta: $n inputs, $fail differing"
  rm -rf "$WORK"
  [ "$fail" -eq 0 ] || { echo "PORT DIFFERS FROM THE C ORIGINAL" >&2; return 1; }
  echo "delta port matches the C original byte for byte"
}

# A differential test that has never been seen to fail is not evidence. Break
# the port on purpose and confirm the comparison notices.
#
# This is not ceremony: the first input set here looked like a thorough pass at
# 8/8 matching, but only 1 of those 8 could detect a deliberately broken carry.
# For random and binary data the compressor finds almost no tables, so
# undiff_table barely runs and a wrong answer is invisible. The table-shaped
# inputs in make_inputs.py exist because of that measurement.
sabotage () {
  [ -x "$REF" ] || build_ref || return 1
  build_rs || return 1
  make_inputs
  local src="$ROOT/rust/darc-codecs/src/delta.rs" bak="$WORK/delta.rs.bak"
  cp "$src" "$bak"
  sed -i.tmp 's|carry = sum >> 8;|carry = 0;|' "$src" && rm -f "$src.tmp"
  local broken="$RS.broken" res n caught
  if ( cd "$ROOT/rust/darc-codecs" && cargo build --release ) >/dev/null 2>&1; then
    clang++ -std=c++17 -O2 -w -DUSE_RUST -DDELTA_LIBRARY -DFREEARC_UNIX \
      -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT -I Compression -I . \
      -o "$broken" "$HERE/delta_ref.cpp" Compression/Delta/Delta.cpp Compression/Common.cpp \
      "$ROOT/rust/darc-codecs/target/release/libdarc_codecs.a" 2>/dev/null
  fi
  res=$(compare_with "$broken" quiet); n=${res%% *}; caught=${res##* }
  cp "$bak" "$src"
  ( cd "$ROOT/rust/darc-codecs" && cargo build --release ) >/dev/null 2>&1
  rm -rf "$WORK"
  echo "sabotage check: $caught of $n inputs detect a broken carry"
  [ "$caught" -gt 0 ] || { echo "the differential test cannot detect a broken port" >&2; return 1; }
}

case "${1:-selftest}" in
  build)    build_ref ;;
  selftest) selftest ;;
  diff)     diff_impls ;;
  sabotage) sabotage ;;
  *) echo "usage: $0 build|selftest|diff|sabotage" >&2; exit 2 ;;
esac
