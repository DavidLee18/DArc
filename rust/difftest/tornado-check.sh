#!/usr/bin/env bash
# Differential-test the Tornado decoder port against the C original.
#
# Tornado is ported decode-first, so the C compressor is the only encoder:
# compress each input with C at every preset, decompress with both C and the
# Rust port, and require both to reproduce the original. Byte-for-byte equality
# is the bar because Tornado defines an archive format -- and `tor` is a default
# method, so these are the streams inside every -m4-and-up archive.
#
# The presets are not interchangeable: 1 selects the byte coder, 2 the bit
# coder, 3-4 huffman and 5+ the range coder. All four share one output loop and
# nothing else, so a corpus that skips any of them proves little.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds Tornado: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags Tornado)" || exit 1
W="${TMPDIR:-/tmp}/tornado-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land after the sources: GNU ld resolves an archive against
# only the objects already seen, so a library placed first is silently dropped.
cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/tornado_ref.cpp" "$CREF/rust/difftest/tornado_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# Inputs chosen for the LZ and entropy layers both: long repeats (deep matches
# and the repeat-distance codes), text (skewed symbol statistics, so the
# huffman tree rebuilds often), incompressible noise (literal-heavy), tables of
# fixed-width numbers (the data-table diffing path), and sizes around the
# buffer and flag-word boundaries.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" tornado "$W/in"

total=0
for preset in 1 2 3 4 5 7 9 11; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); name=$(basename "$f")
    "$W/c"  c "$preset" < "$f"    >| "$W/s"  2>/dev/null || { echo "  [p$preset] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    "$W/c"  d           < "$W/s"  >| "$W/oc" 2>/dev/null || { echo "  [p$preset] $name: C-decode FAILED";   fail=$((fail+1)); continue; }
    "$W/rs" d           < "$W/s"  >| "$W/or" 2>/dev/null || { echo "  [p$preset] $name: RUST-decode FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/oc" || { echo "  [p$preset] $name: C-decode != original (harness bug)"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/or" || { echo "  [p$preset] $name: RUST-decode != original"; fail=$((fail+1)); continue; }
  done
  m=$("$W/c" c "$preset" < "$W/in/text" 2>/dev/null | head -c1 | od -An -tu1 | tr -d ' ')
  echo "  [preset $preset, coder $m] $n inputs, $fail differing"
  total=$((total+fail))
done

echo "tornado decode: $total total differing"
[ "$total" -eq 0 ] && echo "Tornado decoder matches the C original byte for byte" || exit 1
