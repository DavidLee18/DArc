#!/usr/bin/env bash
# End-to-end differential test of the whole BSC block codec.
#
# The C bsc_compress builds a real framed block for every combination of block
# sorter (BWT, ST3..ST6), coder (static, adaptive) and LZP (off, on), and both
# the C bsc_decompress and the Rust dispatcher invert it back to the original.
# This is the first test that runs all five stages together through one entry
# point -- header validate -> QLFC -> BWT/ST -> LZP -> Adler-32 -- after each
# stage was proven in isolation.
#
# ST7/ST8 have no CPU encoder (NOT_SUPPORTED without CUDA), so they are not
# exercised here; bsc_compress declining a combination is treated as a skip, not
# a failure. All three coders (static, adaptive, fast) are covered.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds BSC: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags BSC)" || exit 1
W="${TMPDIR:-/tmp}/bsc-full-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

cc() { local out="$1"; shift
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_full_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",     b"the quick brown fox jumps over the lazy dog. "*4000)   # >64K, LZP-friendly
w("text_sm",  b"the quick brown fox jumps over the lazy dog. "*200)    # <64K
w("english",  (b"compression algorithms rearrange data so that "
               b"statistical redundancy can be removed by an entropy coder. ")*900)
w("runs",     b"".join(bytes([i%97])*(1+(i*7)%200) for i in range(3000)))
w("onebyte",  b"\x5a"*80000)
w("twobyte",  b"\x00\xff"*45000)
w("noise",    prng(9, 200000))            # incompressible -> stored-block path
w("alphabet", bytes(i%256 for i in range(200000)))
w("sparse",   b"".join((b"\x00"*300 + bytes([i%251])) for i in range(500)))
w("skew",     bytes((0 if (i*2654435761>>28)&7 else (i%251)) for i in range(150000)))
# The coder segments into >1 sub-block only above 262144 bytes (2 blocks) and
# 1048576 (4 blocks). These straddle both so the multi-block wrapper is covered.
w("big_text", b"the quick brown fox jumps over the lazy dog. "*12000)  # ~540K, 4 blocks
w("big_eng",  (b"compression algorithms rearrange data so that "
               b"statistical redundancy can be removed by an entropy coder. ")*5000)  # ~520K
w("big_noise",prng(11, 400000))           # ~400K incompressible, 2 blocks
w("big_skew", bytes((0 if (i*2654435761>>28)&7 else (i%251)) for i in range(400000)))
for n in (1,2,3,16,255,256,257,4096,65535,65536,65537):
    w(f"n_{n}", prng(3,n))
PY

# sorter: 1=BWT 3..6=ST; coder: 1=static 2=adaptive; lzp: "0 0" off / defaults on
total=0; tested=0
for sorter in 1 3 4 5 6; do
  for coder in 1 2 3; do
    for lzp in "0 0" "16 128"; do
      fail=0; enc=0
      lname=$([ "$lzp" = "0 0" ] && echo noLZP || echo LZP)
      sname=$([ "$sorter" = 1 ] && echo BWT || echo "ST$sorter")
      cname=$([ "$coder" = 1 ] && echo static || { [ "$coder" = 2 ] && echo adaptive || echo fast; })
      tag="$sname/$cname/$lname"
      for f in "$W"/in/*; do
        bn=$(basename "$f")
        rm -f "$W/e" "$W/oc" "$W/or"
        if ! "$W/c" e "$sorter" "$coder" $lzp < "$f" >| "$W/e" 2>/dev/null; then
          continue   # combination declined for this input
        fi
        enc=$((enc+1))
        sz=$(wc -c < "$f")
        "$W/c"  d "$sz" < "$W/e" >| "$W/oc" 2>/dev/null || { echo "  [$tag] $bn: C-decode FAILED"; fail=$((fail+1)); continue; }
        cmp -s "$f" "$W/oc" || { echo "  [$tag] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
        "$W/rs" d "$sz" < "$W/e" >| "$W/or" 2>/dev/null || { echo "  [$tag] $bn: RUST-decode FAILED"; fail=$((fail+1)); continue; }
        cmp -s "$f" "$W/or" || { echo "  [$tag] $bn: RUST-decode != original"; fail=$((fail+1)); }
      done
      [ "$fail" -ne 0 ] && echo "  [$tag] $enc coded, $fail differing"
      total=$((total+fail)); tested=$((tested+enc))
    done
  done
done

[ "$tested" -gt 0 ] || { echo "no blocks were coded -- harness reached nothing"; exit 1; }
echo "bsc whole-codec: $total total differing ($tested coded blocks)"
[ "$total" -eq 0 ] || exit 1
echo "BSC whole-codec decode matches the C original byte for byte"

# --- The ENCODER side ------------------------------------------------------
# Everything above encodes with the C and only compares DECODE, so it would pass
# with no Rust encoder in the tree at all. Mode `E` runs bsc_compress on both
# sides and diffs the framed block, which is the only bar that matters for an
# encoder: a legal-but-different encoding round-trips perfectly and still
# changes every archive byte.
enc_fail=0; enc_ok=0; enc_declined=0; bwt_ok=0
for sorter in 1 3 4 5 6; do
  for coder in 1 2 3; do
    for lzp in "0 0" "16 128"; do
      lname=$([ "$lzp" = "0 0" ] && echo noLZP || echo LZP)
      sname=$([ "$sorter" = 1 ] && echo BWT || echo "ST$sorter")
      cname=$([ "$coder" = 1 ] && echo static || { [ "$coder" = 2 ] && echo adaptive || echo fast; })
      tag="$sname/$cname/$lname"
      for f in "$W"/in/*; do
        bn=$(basename "$f")
        "$W/c"  E "$sorter" "$coder" $lzp < "$f" >| "$W/ec" 2>/dev/null; rc_c=$?
        "$W/rs" E "$sorter" "$coder" $lzp < "$f" >| "$W/er" 2>/dev/null; rc_r=$?
        if [ "$rc_c" -ne "$rc_r" ]; then
          echo "  [$tag] $bn: C driver exited $rc_c, Rust $rc_r"; enc_fail=$((enc_fail+1)); continue
        fi
        [ "$rc_c" -eq 0 ] || { enc_declined=$((enc_declined+1)); continue; }
        [ -s "$W/ec" ] || { echo "  [$tag] $bn: C produced no output"; enc_fail=$((enc_fail+1)); continue; }
        enc_ok=$((enc_ok+1))
        [ "$sorter" = 1 ] && bwt_ok=$((bwt_ok+1))
        cmp -s "$W/ec" "$W/er" || { echo "  [$tag] $bn: ENCODED block differs from the C"; enc_fail=$((enc_fail+1)); }
      done
    done
  done
done

# The BWT sorter is the point of this round; if it never coded, the encoder
# comparison is only re-testing the ST paths.
[ "$bwt_ok" -gt 0 ] || { echo "the BWT sorter coded nothing -- it is going untested"; exit 1; }

# LZP coverage. `bsc_compress` silently drops back to mode & 0xff when LZP does
# not pay, so "the LZP cases passed" can mean "LZP never ran". Require the
# LZP-on block to actually DIFFER from the LZP-off one, which it cannot do
# unless the LZP stage changed the data.
lzp_live=0
for f in "$W"/in/*; do
  "$W/c" E 1 1 0 0     < "$f" >| "$W/l0" 2>/dev/null || continue
  "$W/c" E 1 1 16 128  < "$f" >| "$W/l1" 2>/dev/null || continue
  cmp -s "$W/l0" "$W/l1" || lzp_live=$((lzp_live+1))
done
[ "$lzp_live" -ge 3 ] || {
  echo "LZP changed the encoded block for only $lzp_live inputs; the LZP-enabled"
  echo "configurations are passing without the LZP stage ever running"; exit 1; }
[ "$enc_fail" -eq 0 ] || { echo "bsc encoder: $enc_fail differing of $enc_ok"; exit 1; }
echo "bsc encoder: $enc_ok/$enc_ok framed blocks byte-identical to the C ($bwt_ok via BWT, $enc_declined declined by both)"
echo "bsc encoder: LZP genuinely changed the block for $lzp_live inputs"
