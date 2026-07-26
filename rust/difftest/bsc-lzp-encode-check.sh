#!/usr/bin/env bash
# Differential-test the BSC LZP ENCODER against the C original.
#
# The first encoder-side cut into BSC. LZP is the compressor's first stage and
# depends on neither the block sorter nor the entropy coder, so a mismatch here
# points squarely at the match finder -- the context hash, the heuristic, the
# match-length tail, the escape of a literal flag byte -- and not at four
# interacting stages. Same ordering the decode side was built in.
#
# BYTE-IDENTITY is the bar, not round-tripping. LZP output is what the block
# sorter and the entropy coder then consume, so an encoding that is merely legal
# would change every -mbsc archive while decoding perfectly.
#
# bsc_lzp_encode_block (:679) picks one of SIX bodies from (hashSize, minLen),
# and they do not emit the same bytes -- see rust/darc-codecs/src/bsc/lzp_enc.rs
# for the measurement. So the grid below is not redundant coverage of one
# algorithm; each cell selects a different encoder, and PORTED below records
# which of them the Rust implements today. A cell outside PORTED that differs is
# outstanding work, reported and counted; a cell inside PORTED that differs is a
# bug and fails the run.
#
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/bsc-lzp-enc.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# libsais.c is its own translation unit (see bsc_ccodec.cpp). The Rust staticlib
# trails the sources so it links on GNU ld.
cc() { local out="$1"; shift
  clang++ -std=c++17 -O2 -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/bsc_lzp_enc_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
    "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" "$@" -o "$out"; }
cc "$W/c"                    || exit 1
cc "$W/rs" -DUSE_RUST "$LIB" || exit 1

# LZP finds repeats an order-4 context predicts, so the corpus is built around
# what makes and breaks that prediction: long exact repeats, near-repeats that
# match then diverge (the heuristic's reason to exist), matches longer than 254
# (the length continuation), literal 0xF2 bytes (the escape), and data with no
# repeats at all (every position a literal, and the whole block incompressible).
python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)

w("text",     b"the quick brown fox jumps over the lazy dog. "*4000)
w("runs",     b"".join(bytes([i%251])*300 for i in range(600)))
w("noise",    prng(7, 300000))
w("zeros",    b"\x00"*200000)
# Near-repeats: a long block repeated with one byte changed each time, so a
# match starts, runs, and dies. This is what the `heuristic` short-circuit is
# for, and a corpus of exact repeats never reaches it.
base = prng(11, 4096)
w("near",     b"".join(base[:i%4000] + bytes([(i*7)&0xff]) + base[i%4000:] for i in range(80)))
# Matches far longer than 254, so the length continuation bytes are emitted.
w("longmatch", (b"A"*100000 + prng(3, 64)) * 3)
# Literal flag bytes (0xF2) in otherwise compressible data: each must be escaped.
w("flagbyte", bytes(0xF2 if i % 13 == 0 else (i*5) & 0xff for i in range(150000)))
# Text with the flag byte sprinkled in, so escapes land INSIDE matches too.
w("flagtext", (b"lorem ipsum dolor \xf2 sit amet "*6000))
for n in (0,1,32,33,64,4096,65536,65537):
    w(f"n_{n}", (b"abcdefgh"*20000)[:n])
PY

fail=0; tested=0; ncmp=0; outstanding=0; outstanding_names=""

# The C picks a different encoder body per (hashSize, minLen) -- six of them,
# and they emit different bytes (see bsc/lzp_enc.rs). PORTED lists the pairs
# whose body the Rust implements; those must be byte-identical or this fails.
# Everything else is reported and counted, because a mismatch there is expected
# work rather than a regression -- but it is never silent, and the assertion at
# the bottom refuses to let the ported set quietly shrink.
#
#   15:72  encode_large_fast_path  <- DArc's DEFAULT, every ordinary -mbsc block
#   18:*   encode_generic          <- hashSize >= 18 leaves the specialised chain
#   23:*   encode_generic
# ALL six bodies are ported, so every cell must match. The literal is kept
# rather than dropping the mechanism: it is what makes a body regressing out of
# the ported set a hard failure rather than a quiet line of prose.
PORTED="ALL"

is_ported() {
  case "$PORTED" in ALL) return 0;; esac
  case " $PORTED " in *" $1:$2 "*) return 0;; esac
  return 1
}

for hash in 12 15 16 17 18 23; do
  for minlen in 4 8 16 32 64 72; do
    for f in "$W"/in/*; do
      bn=$(basename "$f"); tag="[h$hash:l$minlen] $bn"
      "$W/c"  "$hash" "$minlen" < "$f" >| "$W/oc" 2>/dev/null \
        || { echo "  $tag: C driver failed"; fail=$((fail+1)); continue; }
      "$W/rs" "$hash" "$minlen" < "$f" >| "$W/or" 2>/dev/null \
        || { echo "  $tag: Rust driver failed"; fail=$((fail+1)); continue; }
      tested=$((tested+1))
      # The 4-byte prefix is the result code, so this compares outcome and bytes
      # in one go.
      if cmp -s "$W/oc" "$W/or"; then
        ncmp=$((ncmp+1))
      elif is_ported "$hash" "$minlen"; then
        echo "  $tag: differs from the C -- this body IS ported, so it is a bug"
        fail=$((fail+1))
      else
        outstanding=$((outstanding+1))
        case " $outstanding_names " in
          *" h$hash:l$minlen "*) ;;
          *) outstanding_names="$outstanding_names h$hash:l$minlen" ;;
        esac
      fi
    done
  done
done

# Coverage, asserted rather than assumed. A run where every input came back
# "not compressible" would compare nothing but error codes and still be green.
coded=0
for hash in 15 23; do
  for f in "$W"/in/text "$W"/in/runs "$W"/in/longmatch; do
    "$W/c" "$hash" 72 < "$f" >| "$W/oc" 2>/dev/null || continue
    rc=$(python3 -c "import sys;d=open(sys.argv[1],'rb').read()[:4];print(int.from_bytes(d,'little',signed=True))" "$W/oc")
    [ "$rc" -gt 0 ] && coded=$((coded+1))
  done
done

# The defaults must be in the ported set and must pass. If that ever stops being
# true, every -mbsc archive changes, so it is checked by name rather than left
# to the loop above.
is_ported 15 72 || { echo "the DEFAULT parameters (15,72) are no longer in PORTED"; fail=$((fail+1)); }

[ "$tested" -gt 0 ] || { echo "no inputs were encoded -- the harness reached nothing"; exit 1; }
[ "$coded" -ge 4 ] || {
  echo "only $coded of 6 compressible cases actually produced LZP output;"
  echo "the corpus is not exercising the match finder"; fail=$((fail+1)); }
[ "$fail" -eq 0 ] || { echo "bsc-lzp-encode: $fail failures"; exit 1; }

echo "bsc-lzp-encode: $ncmp/$tested byte-identical to the C"
if [ "$outstanding" -gt 0 ]; then
  echo "bsc-lzp-encode: $outstanding comparisons differ in bodies not ported yet:$outstanding_names"
  echo "                (encode_small / small2x / medium -- see bsc/lzp_enc.rs)"
fi
