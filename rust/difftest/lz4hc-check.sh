#!/usr/bin/env bash
# Differential-test the LZ4-HC encoder port against the C LZ4_compress_HC.
#
# LZ4-HC is encoder-only and emits ordinary LZ4 blocks, so the format-valid rule
# would have allowed a port that merely decodes correctly. It turned out that
# both strategies DArc can reach -- `lz4mid` (levels 1-2) and the `lz4hc` hash
# chain (levels 3-9) -- port exactly, so this harness gates on the much stronger
# property instead:
#
#   * levels 1-9      BYTE-IDENTICAL to the C encoder, and
#   * levels 10-12    decode correctly, with the size gap reported.
#
# Levels 10-12 select the C's `lz4opt` optimal parser, which is not ported; they
# clamp to level 9. That gap is measured rather than asserted, because asserting
# it would be pinning a known difference rather than catching a regression. It
# is small and not one-sided: level 10 comes out *smaller* than the C.
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
W="${TMPDIR:-/tmp}/lz4hc-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# The staticlib goes AFTER the sources that reference it: GNU ld resolves an
# archive only against the undefined symbols it has already seen.
clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
  -I"$CREF" -I"$CREF/Compression" \
  "$CREF/rust/difftest/lz4hc_ref.cpp" "$LIB" -o "$W/t" || exit 1

python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",     b"the quick brown fox jumps over the lazy dog. "*3000)
w("english",  (b"compression algorithms rearrange data so that "
               b"statistical redundancy can be removed by an entropy coder. ")*900)
# Repetitive inputs are load-bearing, not filler. They are the ONLY ones that
# exercise two branches: the hash chain's patternAnalysis (levels 9+, which
# triggers on a run of one repeated byte) and lz4mid's catch-back. Both were
# caught by these two files alone while every other input already matched.
w("runs",     b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(2000)))
w("onebyte",  b"\x5a"*80000)
w("twobyte",  b"\x00\xff"*40000)
w("skew",     bytes((0 if (i*2654435761>>28)&7 else (i%251)) for i in range(150000)))
w("sparse",   b"".join((b"\x00"*300 + bytes([i%251])) for i in range(500)))
w("noise",    prng(9, 200000))
w("alphabet", bytes(i%256 for i in range(200000)))
# Offsets are 16 bits, so a match further back than 65535 cannot be encoded.
# These straddle that window, where an off-by-one in lowest_match_index shows.
w("window",   prng(3,70000) + prng(3,70000)[:2000])
w("farback",  prng(5,65000) + b"MARKER"*8 + prng(7,60000) + b"MARKER"*8)
# Below LZ4_MIN_LENGTH (13) and around MFLIMIT, where the parser is skipped.
for n in (1,4,12,13,14,17,255,256,257,4096,65535,65536,65537):
    w(f"n_{n}", prng(3,n))
PY

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
      # Levels 1-9 must match the C exactly.
      [ "$lvl" -le 9 ] && { echo "  [L$lvl] $bn: differs from the C encoder"; fail=$((fail+1)); }
    fi
  done
  pct=$(python3 -c "print(f'{($rtot-$ctot)*100.0/$ctot:+.3f}%')" 2>/dev/null || echo "n/a")
  if [ "$lvl" -le 9 ]; then
    echo "  [L$lvl] $nfile inputs, $((nfile-ndiff)) byte-identical to the C"
  else
    echo "  [L$lvl] $nfile inputs, $ndiff differ (lz4opt not ported), size $pct:$names"
  fi
done

[ "$tested" -gt 0 ] || { echo "no inputs were compressed -- harness reached nothing"; exit 1; }
[ "$fail" -eq 0 ] || { echo "lz4hc: $fail failures"; exit 1; }
echo "lz4hc: $identical/$tested byte-identical; levels 1-9 match the C exactly, all levels decode"
