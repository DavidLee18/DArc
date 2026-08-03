#!/usr/bin/env bash
# Differential-test the LZ4-HC encoder port against the C LZ4_compress_HC.
#
# LZ4-HC is encoder-only and emits ordinary LZ4 blocks, so the format-valid rule
# would have allowed a port that merely decodes correctly. All three strategies
# port exactly -- `lz4mid` (1-2), the `lz4hc` hash chain (3-9) and the `lz4opt`
# optimal parser (10-12) -- so this harness gates on the much stronger property:
# EVERY level must be BYTE-IDENTICAL to the C encoder.
#
# Every block is additionally decoded with the *C* LZ4_decompress_safe rather
# than lz4_flex. Two Rust implementations could otherwise share a misreading of
# the block format and agree with each other.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds LZ4: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags LZ4)" || exit 1
W="${TMPDIR:-/tmp}/lz4hc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# The staticlib goes AFTER the sources that reference it: GNU ld resolves an
# archive only against the undefined symbols it has already seen.
clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
  -I"$CREF" -I"$CREF/Compression" \
  "$CREF/rust/difftest/lz4hc_ref.cpp" "$LIB" -o "$W/t" || exit 1

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" lz4hc "$W/in"

fail=0; tested=0; identical=0
for lvl in 1 2 3 4 5 6 7 8 9 10 11 12; do
  ndiff=0; nfile=0; ctot=0; rtot=0; names=""
  for f in "$W"/in/*; do
    bn=$(basename "$f"); sz=$(wc -c < "$f")
    nfile=$((nfile+1)); tested=$((tested+1))
    rm -f "$W/ec" "$W/er" "$W/o"
    "$W/t" c  "$lvl" < "$f" >| "$W/ec" 2>/dev/null || { echo "  [L$lvl] $bn: C-compress FAILED"; fail=$((fail+1)); continue; }
    "$W/t" rs "$lvl" < "$f" >| "$W/er" 2>/dev/null || { echo "  [L$lvl] $bn: RUST-compress FAILED"; fail=$((fail+1)); continue; }
    # Correctness is absolute at every level: the C decoder must read it back.
    "$W/t" d "$sz" < "$W/er" >| "$W/o" 2>/dev/null || { echo "  [L$lvl] $bn: C could not decode the RUST block"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/o" || { echo "  [L$lvl] $bn: RUST block decoded to different bytes"; fail=$((fail+1)); continue; }
    ctot=$((ctot + $(wc -c < "$W/ec"))); rtot=$((rtot + $(wc -c < "$W/er")))
    if cmp -s "$W/ec" "$W/er"; then
      identical=$((identical+1))
    else
      ndiff=$((ndiff+1)); names="$names $bn"
      echo "  [L$lvl] $bn: differs from the C encoder"; fail=$((fail+1))
    fi
  done
  strat=$([ "$lvl" -le 2 ] && echo lz4mid || { [ "$lvl" -le 9 ] && echo lz4hc || echo lz4opt; })
  echo "  [L$lvl] ($strat) $nfile inputs, $((nfile-ndiff)) byte-identical to the C"
done

[ "$tested" -gt 0 ] || { echo "no inputs were compressed -- harness reached nothing"; exit 1; }
[ "$fail" -eq 0 ] || { echo "lz4hc: $fail failures"; exit 1; }
echo "lz4hc: $identical/$tested byte-identical -- every level matches the C exactly"
