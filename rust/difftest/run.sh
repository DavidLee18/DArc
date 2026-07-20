#!/usr/bin/env bash
# Differential-test a ported codec against the C original.
#
#   rust/difftest/run.sh build      build the C reference driver
#   rust/difftest/run.sh selftest   round-trip the C original over sample inputs
#   rust/difftest/run.sh diff       compare C vs Rust output byte for byte
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
  python3 -c "
import struct,sys
out=bytearray()
for i in range(5000):
    out += struct.pack('<IIII', i*4, i*7+100, 0xAABBCCDD, i)
sys.stdout.buffer.write(bytes(out))"    > "$WORK/in/table16.bin"
  python3 -c "
import struct,sys
out=bytearray()
for i in range(20000):
    out += struct.pack('<IIII', i*4, i*8+1000, i*2, 0x11223344)
sys.stdout.buffer.write(bytes(out))"    > "$WORK/in/table16-wide.bin"
  python3 -c "
import sys
sys.stdout.buffer.write(bytes((i*3+7) % 251 for i in range(100000)))" \
                                         > "$WORK/in/sawtooth.bin"
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

# Placeholder until a Rust Delta exists. Deliberately fails rather than
# reporting success on a comparison it never made -- a check that cannot fail
# is how this branch's Windows CI went green on a binary that did not work.
diff_impls () {
  echo "error: no Rust Delta implementation to compare against yet." >&2
  echo "       Port it, expose it under the same ABI, then run this." >&2
  return 1
}

case "${1:-selftest}" in
  build)    build_ref ;;
  selftest) selftest ;;
  diff)     diff_impls ;;
  *) echo "usage: $0 build|selftest|diff" >&2; exit 2 ;;
esac
