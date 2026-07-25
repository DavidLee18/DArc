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
[ $((fail+st4_fail)) -eq 0 ] || exit 1
