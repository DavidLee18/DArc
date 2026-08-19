#!/usr/bin/env bash
# Differential-test the REP decoder port against the C original.
#
# REP is ported decode-first, so the C compressor is the only encoder: compress
# each input with C, decompress with both C and the Rust port, and require both
# to reproduce the original. Byte-for-byte equality is the bar because REP
# defines an archive format.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds REP: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags REP)" || exit 1
W="${TMPDIR:-/tmp}/rep-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# Trailing arguments land AFTER the sources, because the Rust staticlib has to:
# GNU ld resolves an archive against only the objects already seen on the
# command line, so a library placed first is silently dropped and every symbol
# comes back undefined. macOS ld does not care, which is how this script passed
# locally while never having linked on Linux.
cc() { # cc <output> [args appended after the sources]
  local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -DREP_LIBRARY -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/rep_ref.cpp" "$CREF/Compression/REP/rep.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"
}
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1


# WHAT THIS HARNESS CANNOT REACH, and where it is gated instead.
#
# `data` in rep_decompress is a CIRCULAR buffer of the rep block size, so a
# match found after the input passes that size can reach back past the start of
# the current cycle: its source then lies AHEAD of the write position, and the
# stream spells the offset cyclically. rep_ref.cpp hardcodes the method default
# BlockSize = 64 MiB, so no corpus small enough to run here can wrap it -- and
# the case went untested for exactly that reason until issue #165.
#
# It is gated in rust/darc-codecs/tests/rep.rs
# (`a_match_across_a_buffer_wrap_round_trips`), which drives the ported encoder
# and decoder directly and can therefore pick a 64 KiB block.
#
# MARKED DIVERGENCE: the C at DARC_C_REF_SHA -- and every DArc from v2.0.0 to
# v3.0.1, and the Haskell reference, all of which carry the same bounds check --
# REJECTS a wrapped stream with FREEARC_ERRCODE_BAD_COMPRESSED_DATA. The port
# decodes it. So this comparison must not be extended with a wrapping case: the
# oracle is wrong there, and a red run would be recording the reference's bug.
# Inputs: REP matches only >=512-byte repeats, so cover long repeats, block
# edges, incompressible data and the empty case.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" rep "$W/in"

fail=0 n=0
for f in "$W"/in/*; do
  n=$((n+1)); name=$(basename "$f")
  "$W/c"  c < "$f"          >| "$W/stream"
  "$W/c"  d < "$W/stream"   >| "$W/oc"
  "$W/rs" d < "$W/stream"   >| "$W/ors"
  cmp -s "$f" "$W/oc"  || { echo "  $name: C-decode != original (harness)"; fail=$((fail+1)); continue; }
  cmp -s "$f" "$W/ors" || { echo "  $name: RUST-decode != original"; fail=$((fail+1)); continue; }
done
echo "rep decode: $n inputs, $fail differing"
[ "$fail" -eq 0 ] && echo "REP decoder matches the C original byte for byte" || exit 1
