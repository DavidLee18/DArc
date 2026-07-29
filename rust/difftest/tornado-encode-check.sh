#!/usr/bin/env bash
# Differential-test the Tornado *encoder* port against the C original.
#
# The bar is byte-identity of the compressed stream, not merely a stream that
# decodes correctly. An encoder has freedom the decoder does not: a match finder
# that returns a shorter match, or the same length at a different distance,
# still produces a valid archive. Every round-trip test passes on such a port
# and it is still wrong -- the whole point of replacing the C is that the output
# does not change. So this compares `tor_compress` output to
# `darc_rs_tor_compress` output, byte for byte, and nothing weaker.
#
# Coverage is asserted, not assumed. The port covers three of the nine live
# instantiations; the rest return INVALID_COMPRESSOR. A preset the Rust refuses
# is reported as SKIPPED with its reason, so a shrinking corpus can never look
# like a passing one -- see the note in green-test-may-not-run-your-code.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds Tornado: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags Tornado)" || exit 1
W="${TMPDIR:-/tmp}/tornado-encode-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# WITHOUT the `dropin` feature, and that is load-bearing: the pinned
# tornado_ccodec.cpp defines tor_decompress itself, so a staticlib carrying the
# drop-in export collides with it ("duplicate symbol '_tor_decompress'").
# 4x4-check.sh needs the opposite. Cargo re-resolves on each run so the two are
# fine sequentially, but they must never run concurrently against the shared
# rust/target -- doing so produced empty output and stalled runs.
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

# Inputs chosen for what the *encoder* has to get right: match lengths and
# distances at code boundaries, runs that drive the repeat-distance codes,
# incompressible data (all literals), and -- the ones that matter most --
# inputs long enough to slide the window, which is where the hash rebasing,
# `read_point` arithmetic and `matchend` clamp all come into play. Anything
# under one chunk exercises none of that.
python3 - "$W/in" <<'PY'
import os,sys,struct
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",      b"the quick brown fox jumps over the lazy dog. "*20000)
w("repeats",   (b"ABCDEFGHIJKLMNOP"*64 + prng(1,256))*400)
w("noise",     prng(7, 900000))
w("zeros",     b"\x00"*700000)
w("mixed",     b"".join((prng(i,1000) + b"pattern"*200) for i in range(300)))
# Short unique prefix + noise, repeated. This shape diverges at preset 9 and at
# nothing else, and no other input here reproduces it -- the sweep was green on
# preset 9 until this was added. Found via 4x4-check.sh, whose corpus happened
# to contain it; the failure is NOT specific to all-at-once mode (it shows at
# both settings), it is specific to this data against preset 9's match finder
# (hashsize 2048mb, row 256, auxhash 512kb/4).
w("chunky",    b"".join((b"chunk-%d-" % i) + prng(i, 300) for i in range(2000)))
w("table4",    b"".join(struct.pack("<I", i*7+3) for i in range(200000)))
# Distances placed either side of the 48 KB / 192 KB / 1 MB acceptance limits
# in accept_match(), which is where a short match is rejected outright.
seg = prng(5, 300000)
w("far_matches", seg + prng(11, 40*1024) + seg[:50000]
               + prng(13, 200*1024) + seg[:50000]
               + prng(17, 1100*1024) + seg[:50000])
# Past LARGE_BUFFER_SIZE (256 KB) many times over, so the input buffer is
# refilled and slid repeatedly rather than read once.
w("big_text",  (b"the quick brown fox jumps over the lazy dog. "*400000))
w("big_noise", prng(23, 5*1024*1024))
for n in (0,1,3,4,5,15,16,17,63,64,65,255,256,257,4095,4096,65535,65536,65537):
    w(f"n_{n}", (b"the quick brown fox "*10000)[:n])
PY

total=0; ran=0; skipped=0
# Each case is "preset notables". Presets 1 and 2 are the byte and bit coders at
# hash row width 1; preset 3 is huffman at row width 2, reachable only with
# find_tables cleared while the data-table detector is unported -- and it is the
# only case that exercises MatchFinder2, so dropping it would leave that finder
# with no coverage at all. Presets 4+ need the caching finders, the 3-byte hash
# or lazy matching. Presets 7, 8 and 9 all share caching_finder==5 and so are
# one instantiation; 10 (caching 6) and 11 (caching 7) are two more. All three
# are listed because the cycled finder's N differs between them, and covering
# 7/9/11 alone would leave N==6 untested while looking complete.
# The third field is `compress_all_at_once`. It defaults to 0 and this sweep
# used to test only that: tornado_ccodec.cpp passes the live global and nothing
# set it. But C_4x4.cpp forces it to 1 around both directions (:559-566), so
# every -m3..-m9 archive compresses its $binary group in all-at-once mode --
# and that mode was compared by nothing. Adding it here is what found the
# preset-9 divergence; leaving the axis out is what hid it.
for case in "0 0 0" "1 0 0" "2 0 0" "3 0 0" "3 1 0" "4 0 0" "4 1 0" "5 0 0" "5 1 0" \
            "6 0 0" "6 1 0" "7 0 0" "8 0 0" "9 0 0" "10 0 0" "11 0 0" \
            "0 0 1" "1 0 1" "2 0 1" "3 0 1" "4 0 1" "5 0 1" "6 0 1" "7 0 1" \
            "8 0 1" "9 0 1" "10 0 1" "11 0 1"; do
  set -- $case; preset=$1; notables=$2; aao=$3
  label="preset $preset"; [ "$notables" = 1 ] && label="$label -t0"; [ "$aao" = 1 ] && label="$label aao"
  fail=0; n=0; skip=""
  for f in "$W"/in/*; do
    name=$(basename "$f")
    if ! "$W/c" c "$preset" "$notables" "$aao" < "$f" >| "$W/sc" 2>/dev/null; then
      echo "  [$label] $name: C-compress FAILED"; fail=$((fail+1)); continue
    fi
    if ! "$W/rs" c "$preset" "$notables" "$aao" < "$f" >| "$W/sr" 2>"$W/err"; then
      skip="rust refused ($(tr -d '\n' < "$W/err" | tail -c 40))"
      break
    fi
    n=$((n+1))
    if ! cmp -s "$W/sc" "$W/sr"; then
      echo "  [$label] $name: streams DIFFER ($(wc -c <"$W/sc") C vs $(wc -c <"$W/sr") Rust)"
      fail=$((fail+1))
    fi
  done
  if [ -n "$skip" ]; then
    echo "  [$label] SKIPPED -- $skip"
    skipped=$((skipped+1))
  else
    coder=$("$W/c" c "$preset" "$notables" "$aao" < "$W/in/text" 2>/dev/null | head -c1 | od -An -tu1 | tr -d ' ')
    echo "  [$label, coder $coder] $n inputs, $fail differing"
    ran=$((ran+1)); total=$((total+fail))
  fi
done

# Coverage assertion: comparing a preset with and without find_tables proves
# nothing about the data-table detector unless the detector actually fires. If
# the two C outputs were the same size, the table path was never taken and every
# "0 differing" above would be silent about it.
echo
fired=0
for preset in 3 4 5 6; do
  a=$("$W/c" c "$preset" 0 < "$W/in/table4" 2>/dev/null | wc -c)
  b=$("$W/c" c "$preset" 1 < "$W/in/table4" 2>/dev/null | wc -c)
  if [ "$a" = "$b" ]; then
    echo "  FAIL: preset $preset table detection never fired (both $a bytes)" >&2
  else
    fired=$((fired+1))
  fi
done
if [ "$fired" -ne 4 ]; then
  echo "FAIL: the data-table path was not exercised on all four presets" >&2
  exit 1
fi
echo "  data-table detector fires on presets 3-6 (compared with it both on and off)"

echo
echo "tornado encode: $ran presets compared, $skipped skipped, $total differing"
# A run that compared nothing is a failure, not a pass.
if [ "$ran" -eq 0 ]; then
  echo "FAIL: no preset was actually compared" >&2; exit 1
fi
[ "$total" -eq 0 ] \
  && echo "Tornado encoder matches the C original byte for byte on $ran presets" \
  || exit 1
