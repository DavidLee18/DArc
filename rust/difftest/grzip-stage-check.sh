#!/usr/bin/env bash
# GRZip's encoder stages, forward direction: C vs Rust, byte for byte.
#
# GRZip's encoder is being ported STAGE BY STAGE, and GRZip_CompressBlock cannot
# produce a comparable stream until every stage exists -- it is also recursive
# (the record filter splits a block into 2 or 4 parts that re-enter it). So each
# stage is gated on its own first, and this is the first of them.
#
# The reference input is PADDED with 64 zero bytes. The C reads up to
# MinMatchLen-1 bytes past the end at LZP.c:89 -- confirmed under ASan as a
# heap-buffer-overflow READ of size 4 -- so without padding this would compare
# against whatever malloc happened to leave there. The Rust port does not
# reproduce the overread: it reads a zero-padded view and stays in bounds, which
# is why the two agree. See the note on `lzp::encode` for why that is the right
# call rather than a divergence.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/grzip-lzp.$$"; mkdir -p "$W/in"
trap 'rm -rf "$W"' EXIT
( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 || { echo "cargo build failed"; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/grzip_ref.cpp" "$CREF/rust/difftest/grzip_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

python3 - "$W/in" <<'PY'
import os,sys,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
# Long matches (the run-flag ladder past 254), short matches (literal path),
# noise (no matches at all, and the not-compressible bail), and the 0xF2 escape.
w("repeat8",  b"abcdefgh"*40000)
w("text",     b"the quick brown fox jumps over the lazy dog. "*8000)
w("noise",    prng(7, 300000))
w("mixed",    b"".join((b"abcdefgh"*50 if i%3 else prng(i,400)) for i in range(300)))
w("f2heavy",  bytes([0xF2 if i%5==0 else (i*7)&0xff for i in range(200000)]))
w("runs",     b"".join(bytes([i&0xff])*1000 for i in range(300)))
w("zeros",    b"\x00"*400000)
# Sizes around the awkward edges, including lengths where the last position sits
# inside the region the C overreads.
for n in (32,33,40,63,64,65,1000,4096,4097):
    w(f"n_{n}", (b"abcdefgh"*2000)[:n])
for n in (4096,4097,5000):
    w(f"rnd_{n}", prng(3,n))
# Matches of EXACTLY MinMatchLen, which is the boundary `common < mml` guards.
# Nothing above produces one: repetitive data gives matches far longer, noise
# gives none, so flipping that `<` to `<=` changed no output at all. Each block
# repeats a 4-byte context and then agrees for exactly L more bytes before
# diverging, for L spanning the MinMatchLen values under test.
# Record-shaped inputs. Without these GRZip_Rec_Test returns 0 for everything
# and the record stage is untested -- exactly how TTA's autodetection sat behind
# 210 green comparisons. Modes 1 and 2 are plain 2- and 4-byte de-interleaves,
# 3 and 4 the delta-coded versions; the run below ASSERTS all four appear.
w("rec16_counter", b"".join(struct.pack("<H", (i*3)&0xffff) for i in range(60000)))
w("rec16_noisy",   b"".join(struct.pack("<H", ((i*3)&0xffff) ^ (prng(i,1)[0]&0x7)) for i in range(60000)))
w("rec32_counter", b"".join(struct.pack("<I", i*7) for i in range(30000)))
w("rec32_table",   b"".join(struct.pack("<I", 0x40000000 + (i%997)*13) for i in range(30000)))
w("rec16_flat",    b"".join(struct.pack("<H", (i%251)*17) for i in range(60000)))
w("rec32_struct",  b"".join(struct.pack("<HBB", i&0xffff, (i*5)&0xff, 0x20) for i in range(30000)))
w("rec16_desc",    b"".join(struct.pack("<H", (65535-(i*3))&0xffff) for i in range(60000)))
# Modes 1 and 2 need de-interleaving to pay off while DELTA coding does not --
# every input above is monotonic, so the delta test always fires and turns them
# into 3 and 4. These are records with one noisy field and the rest near
# constant: the positional split wins big, but successive values are unrelated,
# so the sum-vs-sum-of-deltas comparison stays on the near side.
n16 = prng(41, 60000)
w("rec16_noise_lo", b"".join(struct.pack("<H", 0x2500 | n16[i]) for i in range(60000)))
n32 = prng(43, 30000)
w("rec32_noise_lo", b"".join(struct.pack("<I", 0x40302000 | n32[i]) for i in range(30000)))
# Mode 2 additionally needs the record VALUES to stay small. The delta test
# compares Sum against MinCode*(Size>>2), and that product is `uint32 * int` --
# unsigned, 32-bit, and it WRAPS. For values around 0x40302000 with 30k records
# the product wraps to near nothing, `Sum - product` comes out enormous, and the
# test fires unconditionally: everything becomes mode 4. Keeping the values near
# 8k keeps the product under 2^32, which is the only way mode 2 is reachable at
# all. The overflow is the C's and is reproduced deliberately; this input is what
# proves the non-overflowing side of it is right too.
n32b = prng(61, 30000)
w("rec32_small",   b"".join(struct.pack("<I", 0x2000 | n32b[i]) for i in range(30000)))
# The 16-bit twin of that overflow, which is SIGNED rather than unsigned:
# MinCode*(Size>>1) is `int * int`, so with MinCode near 0xF000 and 60k records
# the product is 3.7e9 and wraps NEGATIVE. Without an input in this range,
# widening the product to 64 bits changes nothing and the sabotage passes.
n16b = prng(71, 60000)
w("rec16_high",    b"".join(struct.pack("<H", 0xF000 | n16b[i]) for i in range(60000)))
# Either side of the delta test's SLACK. It reads
#     sum - MinCode*n > sum_delta + (sum_delta >> 4)
# so there is a 6.25% band where the plain sum exceeds the delta sum and the
# filter still declines. Nothing lands in it by chance -- dropping the `>> 4`
# left the whole run green -- so these two are placed deliberately: step 33 sits
# just above the band (mode 4 either way) and step 34 inside it (mode 2 only
# because of the slack, mode 4 without it). Derived by modelling the comparison
# and confirmed against the C.
w("rec32_band_out", b"".join(struct.pack("<I", 0x2000 | ((i*33)&0xFF)) for i in range(4096)))
w("rec32_band_in",  b"".join(struct.pack("<I", 0x2000 | ((i*34)&0xFF)) for i in range(4096)))
for L in (7,8,9,15,16,17,31,32,33,63,64,65):
    blk = bytearray()
    for r in range(400):
        ctx = bytes([0xA1,0xB2,0xC3,0xD4])
        body = bytes([(r*13+i)&0xff for i in range(L)])
        blk += ctx + body + bytes([(r*77)&0xff])   # one divergent byte
    w(f"exact_{L}", bytes(blk))
PY

fail=0; total=0

# --- ST4 -------------------------------------------------------------------
# Both the transformed block AND the returned FBP matter: the driver stores FBP
# in the block header, so a port that produced the right bytes with the wrong
# index would still write an archive that decodes to garbage.
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  t < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" t < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_fbp=$(cat "$W/e.c"); r_fbp=$(cat "$W/e.rs")
  if [ "$c_fbp" != "$r_fbp" ]; then
    echo "  [st4] $(basename "$f"): FBP differs ($c_fbp vs $r_fbp)"; fail=$((fail+1)); continue
  fi
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [st4] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
st4_total=$total
echo "grzip ST4 encode: $((st4_total-fail))/$st4_total agree"
st4_fail=$fail

# --- record filter ---------------------------------------------------------
# The MODE matters as much as the bytes: it is what makes GRZip_CompressBlock
# recurse, and it is chosen by a float entropy comparison plus an integer sum
# that overflows on purpose (see rec::test).
fail=0; total=0; modes_seen=""
for f in "$W"/in/*; do
  total=$((total+1))
  "$W/c"  r < "$f" >| "$W/o.c"  2>"$W/e.c"
  "$W/rs" r < "$f" >| "$W/o.rs" 2>"$W/e.rs"
  c_m=$(cat "$W/e.c"); r_m=$(cat "$W/e.rs")
  if [ "$c_m" != "$r_m" ]; then
    echo "  [rec] $(basename "$f"): MODE differs ($c_m vs $r_m)"; fail=$((fail+1)); continue
  fi
  modes_seen="$modes_seen ${c_m#mode=}"
  cmp -s "$W/o.c" "$W/o.rs" || { echo "  [rec] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
done
rec_total=$total; rec_fail=$fail
echo "grzip Rec encode: $((rec_total-rec_fail))/$rec_total agree"
# A green run over inputs that all return mode 0 would test nothing at all.
for want in 1 2 3 4; do
  case " $modes_seen " in
    *" $want "*) ;;
    *) echo "  corpus never produced Rec mode $want -- that path is untested"; rec_fail=$((rec_fail+1));;
  esac
done

# --- LZP -------------------------------------------------------------------
fail=0; total=0
for mml in 8 16 32 64; do
  for htb in 8 12 15; do
    for f in "$W"/in/*; do
      total=$((total+1))
      rc1=0; rc2=0
      "$W/c"  l $mml $htb < "$f" >| "$W/o.c"  2>"$W/e.c"  || rc1=1
      "$W/rs" l $mml $htb < "$f" >| "$W/o.rs" 2>"$W/e.rs" || rc2=1
      c_rc=$(cat "$W/e.c"); r_rc=$(cat "$W/e.rs")
      if [ "$c_rc" != "$r_rc" ]; then
        echo "  [mml=$mml ht=$htb] $(basename "$f"): return differs ($c_rc vs $r_rc)"; fail=$((fail+1)); continue
      fi
      cmp -s "$W/o.c" "$W/o.rs" || { echo "  [mml=$mml ht=$htb] $(basename "$f"): OUTPUT differs"; fail=$((fail+1)); }
    done
  done
done
echo "grzip LZP encode: $((total-fail))/$total agree"
[ $((fail+st4_fail+rec_fail)) -eq 0 ] || exit 1
