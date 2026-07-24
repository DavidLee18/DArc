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
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh. Delta.cpp no longer exists in the checkout.
. "$HERE/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1

# rust/ is a cargo workspace, so build output lands in the WORKSPACE target
# directory. It was rust/darc-codecs/target/ before the workspace existed, and
# when that moved this script passed a path that no longer existed straight to
# clang++, which reported "no such file or directory" for a library nobody had
# asked it to find. Check it here instead, where the message can say what
# actually went wrong.
RUST_LIB="$ROOT/rust/target/release/libdarc_codecs.a"
require_rust_lib () {
  [ -f "$RUST_LIB" ] || {
    echo "error: cargo built no $RUST_LIB" >&2
    echo "       (workspace target dir; check 'cd rust && cargo build --release -p darc-codecs')" >&2
    return 1
  }
}
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
    -I "$CREF/Compression" -I "$CREF" \
    -o "$REF" "$CREF/rust/difftest/delta_ref.cpp" "$CREF/Compression/Delta/Delta.cpp" "$CREF/Compression/Common.cpp" \
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
  ( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
    || { echo "error: cargo build failed" >&2; return 1; }
  require_rust_lib || return 1
  clang++ -std=c++17 -O2 -w \
    -DUSE_RUST -DDELTA_LIBRARY -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I "$CREF/Compression" -I "$CREF" \
    -o "$RS" "$CREF/rust/difftest/delta_ref.cpp" "$CREF/Compression/Delta/Delta.cpp" "$CREF/Compression/Common.cpp" \
    "$RUST_LIB" \
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
    "$REF" c >| "$WORK/c_packed" < "$f" 2>/dev/null || continue
    "$REF" d >| "$WORK/c_out"    < "$WORK/c_packed" 2>/dev/null || continue
    n=$((n+1))
    local bad=0

    # Compression: the ported compressor must emit the same bytes as the C one.
    # Checking only decompression would miss every heuristic in the compressor --
    # which it silently did at first, reporting 0/23 for eight mutations that
    # were each detectable when compressed output was compared.
    if ! "$rs" c >| "$WORK/r_packed" < "$f" 2>/dev/null; then bad=1
    elif ! cmp -s "$WORK/c_packed" "$WORK/r_packed"; then bad=1
    fi

    # Decompression, both ways round: the port must read the C stream, and the C
    # original must read the port's.
    if [ "$bad" -eq 0 ]; then
      "$rs" d >| "$WORK/r_out" < "$WORK/c_packed" 2>/dev/null || bad=1
      cmp -s "$WORK/c_out" "$WORK/r_out" || bad=1
      cmp -s "$f" "$WORK/r_out" || bad=1
      "$REF" d >| "$WORK/x_out" < "$WORK/r_packed" 2>/dev/null || bad=1
      cmp -s "$f" "$WORK/x_out" || bad=1
    fi

    if [ "$bad" -eq 0 ]; then
      [ -n "$quiet" ] || printf "  %-20s %9s bytes  C==Rust both ways\n" \
        "$(basename "$f")" "$(wc -c <"$f" | tr -d ' ')" >&2
    else
      [ -n "$quiet" ] || printf "  %-20s DIFFERS FROM C\n" "$(basename "$f")" >&2
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

# A differential test that has never been seen to fail is not evidence.
#
# This mutates the port in ways that mirror real transcription errors and
# reports how many inputs notice each one. It is not ceremony -- every number
# below started out wrong:
#
#   * An early corpus of random data, a binary and degenerate sizes matched C on
#     14/14 and could not detect four deliberate errors. Counting the tables it
#     actually produced explained it: 14 tables across 14 inputs, all perfectly
#     monotonic, so search_for_table_boundary never took its direction-change
#     branch -- the only place `omit` and `lastpoint` are used.
#   * One mutation reported 0/13 for a different reason entirely: the sed
#     pattern had the wrong indentation and never applied. The zero meant
#     "nothing was broken", not "the test is blind". Hence apply_mutation
#     below refuses to continue if its pattern is absent.
#
# With inputs built for the heuristics (make_inputs.py) the corpus produces
# ~280 tables and every mutation is caught by at least one input.
MUT_SRC="$ROOT/rust/darc-codecs/src/delta.rs"

apply_mutation () {
  python3 - "$MUT_SRC" "$1" "$2" <<'PYEOF'
import sys
path, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
s = open(path).read()
if old not in s:
    sys.stderr.write("mutation pattern not found: %r\n" % old)
    sys.exit(1)
open(path, "w").write(s.replace(old, new, 1))
PYEOF
}

sabotage () {
  [ -x "$REF" ] || build_ref || return 1
  build_rs || return 1
  make_inputs
  local bak="$WORK/delta.rs.bak"; cp "$MUT_SRC" "$bak"
  local broken="$RS.broken" status=0

  # name | pattern | replacement
  local muts=(
    "omit++ unconditional|            omit += 1;|            // mutated"
    "lastpoint back-off|lastpoint = t - n * omit as isize;|lastpoint = t;"
    "acceptance threshold|> 30.0 + 4.0 * skip_bits|> 29.0 + 4.0 * skip_bits"
    "candidate count > 5|if count[i] > 5 {|if count[i] > 4 {"
    "immutable N exclusion|&& n != 2 && n != 4 && n != 8|&& n != 4 && n != 8"
    "itemlb > 10 adjustment|itemlb -= (itemlb > 10) as u32;|// mutated"
    "short-run limit|if bad >= 2 {|if bad >= 3 {"
    "constant-column ratio|neq * 4 < rows as i32|neq * 3 < rows as i32"
  )
  for m in "${muts[@]}"; do
    local name="${m%%|*}" rest="${m#*|}"
    local pat="${rest%%|*}" rep="${rest#*|}"
    cp "$bak" "$MUT_SRC"
    if ! apply_mutation "$pat" "$rep"; then status=1; continue; fi
    ( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1
    clang++ -std=c++17 -O2 -w -DUSE_RUST -DDELTA_LIBRARY -DFREEARC_UNIX \
      -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT -I "$CREF/Compression" -I "$CREF" \
      -o "$broken" "$CREF/rust/difftest/delta_ref.cpp" "$CREF/Compression/Delta/Delta.cpp" "$CREF/Compression/Common.cpp" \
      "$RUST_LIB" 2>/dev/null
    local res n caught
    res=$(compare_with "$broken" quiet); n=${res%% *}; caught=${res##* }
    printf "  %-24s %2s of %2s inputs detect it\n" "$name" "$caught" "$n"
    [ "$caught" -gt 0 ] || { echo "    ^ NOT DETECTED by any input" >&2; status=1; }
  done

  cp "$bak" "$MUT_SRC"
  ( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1
  rm -rf "$WORK"
  [ "$status" -eq 0 ] && echo "every mutation is caught by at least one input" \
                      || { echo "the differential test is blind to some errors" >&2; return 1; }
}

case "${1:-selftest}" in
  build)    build_ref ;;
  selftest) selftest ;;
  diff)     diff_impls ;;
  sabotage) sabotage ;;
  *) echo "usage: $0 build|selftest|diff|sabotage" >&2; exit 2 ;;
esac
