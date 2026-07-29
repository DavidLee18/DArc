#!/usr/bin/env bash
# Differential-test the Dict port against the C original, BOTH directions.
#
# Dict is one of DArc's own formats, so "format-valid" does not apply: the
# encoder has to be byte-exact or archives move.
#
# ## Why this file exists
#
# `dict_ref.cpp` and `dict_phase1_ref.cpp` had been in the tree since the port
# landed, and no `dict-check.sh` ever did -- so nothing built them and the
# workflow never named `dict`. The C was deleted anyway. That is the identical
# situation LZP was found in (see the header of lzp-check.sh), except that here
# the gap survived the deletion rather than being closed before it.
#
# The C reference therefore comes from a pinned revision, not the working tree,
# which no longer has a C Dict at all -- see c-reference.sh.
#
# ## What has to be true for this to be a test at all
#
# Dict DECLINES on data it cannot compress: `DictEncode` returns non-zero, or
# the result fails the MinCompression ratio, and the block is stored verbatim.
# A declined block makes both implementations emit the same four-byte header
# plus the input, so a corpus of binary data yields a perfectly byte-identical
# comparison that never runs the dictionary builder. The coverage assertion at
# the bottom uses the driver's 'v' mode to count blocks the C actually ENGAGED
# on, and fails if too few did.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# Built the way DArc builds Dict: see darc_codec_cflags in c-reference.sh for
# why the makefile's flags, not an -O level, are the oracle.
CFLAGS_C="$(darc_codec_cflags Dict)" || exit 1
W="${TMPDIR:-/tmp}/dict-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }

# The Rust variant compiles the pinned C_Dict.cpp with -DDARC_RUST, exactly as
# production did: that excludes the C dict_compress/dict_decompress so the
# staticlib supplies them. $lib is a SEPARATE parameter placed after every
# source -- GNU ld resolves an archive only against undefineds it has already
# seen, so a staticlib listed first contributes nothing (links on macOS, fails
# on Linux).
cc() { local out="$1" lib="$2"; shift 2
  # shellcheck disable=SC2086  # the flag list is a word list on purpose
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" \
    "$CREF/rust/difftest/dict_ref.cpp" \
    "$CREF/Compression/Dict/C_Dict.cpp" \
    "$CREF/Compression/CompressionLibrary.cpp" \
    "$CREF/Compression/Common.cpp" \
    ${lib:+"$lib"} -o "$out"; }
cc "$W/c"  ""                             || { echo "C reference build failed" >&2; exit 1; }
cc "$W/rs" "$LIB" -DUSE_RUST -DDARC_RUST  || { echo "Rust driver build failed" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

python3 - "$W/in" <<'PY'
import os,sys,random
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)

# What Dict actually accepts is narrower than "repeated words", and the corpus
# was rebuilt around measurement rather than intuition. Text made only of
# lowercase letters and spaces is REFUSED outright -- DictEncode returns -1,
# because MinWeakChars (20 by default) demands a good spread of non-word
# characters before it will believe the input is text. Measured with the
# driver's 'v' mode, over 256 KB blocks:
#
#   19-word vocabulary, lowercase + space   0/5 blocks   rc=-1
#   one word repeated 120000 times          0/5 blocks   rc=-1
#   source code, heavy punctuation          0/4 blocks   declined on ratio
#   HTML-ish markup                         0/4 blocks   declined on ratio
#   natural prose: mixed case, digits,
#     commas, full stops, newlines          5/5 blocks   engaged
#
# So the engaging inputs are the `natural_*` ones below, and they are what make
# the encoder comparison mean anything.
COMMON=("the of and to in a is that for it as was with be by on not he this but have "
        "from they which one you were all her she there would their we him been has "
        "when who will more no if out so said what up its about into than them can").split()
TOPIC=("compression algorithm dictionary preprocessor archive redundancy entropy encoder "
       "decoder statistical frequency threshold occurrence substitution replacement "
       "transformation implementation").split()
def natural(seed, n, topic_every=6):
    r=random.Random(seed); o=[]
    for i in range(n):
        ww = TOPIC[r.randrange(len(TOPIC))] if i%topic_every==0 else COMMON[r.randrange(len(COMMON))]
        if i%17==0: ww=ww.capitalize()
        o.append(ww)
        if i%11==0: o.append(str(i%1000))
        if i%13==0: o.append(",")
        if i%29==0: o.append(".")
        if i%97==0: o.append("\n")
    return (" ".join(o)).encode()
# Three vocabularies and three sizes, so the word-frequency buckets
# (MinLargeCnt 2048 / MinMediumCnt 100 / MinSmallCnt 50) are populated
# differently in each: 5, 4 and 2 engaged blocks respectively.
w("natural_a", natural(7, 200000))
w("natural_b", natural(11, 120000, topic_every=3))
w("natural_c", natural(23, 60000, topic_every=12))

# A small fixed vocabulary with punctuation: engages on its first block and
# declines on the short tail, so one file covers both outcomes.
sent = ("The quick brown fox jumps over the lazy dog, and the dog barks. "
        "Compression of text depends on repeated words appearing often. ")
w("english", (sent*3000).encode())

# These DECLINE, and that is worth testing too: the stored path has its own
# four-byte framing and the two implementations must agree on it byte for byte.
src = ("static int compute_value(struct context *ctx, int index) {\n"
       "    if (ctx == NULL || index < 0) return -1;\n"
       "    return ctx->table[index] + ctx->offset;\n"
       "}\n")
w("source",  (src*6000).encode())
w("markup",  ('<div class="row"><span id="x">value</span></div>\n'*20000).encode())
w("noise",   prng(9, 300000))
w("zeros",   b"\x00" * 200000)
w("binary",  bytes(i % 256 for i in range(300000)))

# Edge sizes, including empty and sub-word.
for n in (0,1,2,3,63,64,65,255,256,257,4095,4096,65537):
    w(f"n_{n}", (b"word " * ((n//5)+1))[:n])
PY

# ── the comparison ──────────────────────────────────────────────────────────
# Two axes. Block size bounds one dictionary; DICT_CHUNK bounds how much a
# single "read" may return, which is what decides where blocks actually break
# in the archiver -- dict_compress loops on read, so the codec sees a sequence
# of pipeline-sized buffers, not one whole file. dict_ref.cpp's own header
# records that returning everything in one read hid a real divergence.
total=0; tested=0
for bs in 8388608 1048576 65536; do
  for chunk in 0 262144 65536; do
    fail=0; n=0
    for f in "$W"/in/*; do
      n=$((n+1)); bn=$(basename "$f")
      rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr"
      DICT_CHUNK=$chunk "$W/c"  c "$bs" < "$f" >| "$W/ec" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C-compress FAILED"; fail=$((fail+1)); continue; }
      DICT_CHUNK=$chunk "$W/rs" c "$bs" < "$f" >| "$W/er" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-compress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$W/ec" "$W/er" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: ENCODER differs from the C"; fail=$((fail+1)); continue; }
      # Decode the C's own stream with both, and require the original back.
      DICT_CHUNK=$chunk "$W/c"  d "$bs" < "$W/ec" >| "$W/dc" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C-decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dc" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
      DICT_CHUNK=$chunk "$W/rs" d "$bs" < "$W/ec" >| "$W/dr" 2>/dev/null \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dr" \
        || { echo "  [bs=$bs chunk=$chunk] $bn: RUST-decode != original"; fail=$((fail+1)); }
      tested=$((tested+1))
    done
    echo "  [bs=$bs chunk=$chunk] $n inputs, $fail differing"
    total=$((total+fail))
  done
done

# ── coverage: did the dictionary builder ever actually run? ─────────────────
# 'v' mode replays dict_compress's read loop but calls DictEncode directly and
# prints "engaged" or "DECLINED" per block. Without this the suite could be
# perfectly green on a corpus Dict refuses outright.
engaged=0
for f in "$W"/in/natural_a "$W"/in/natural_b "$W"/in/natural_c "$W"/in/english; do
  [ -f "$f" ] || continue
  k=$(DICT_CHUNK=262144 "$W/c" v 8388608 < "$f" 2>&1 >/dev/null | grep -c 'engaged' || true)
  engaged=$((engaged + k))
done

[ "$tested" -gt 0 ] || { echo "no inputs were processed -- the harness reached nothing"; exit 1; }
[ "$engaged" -ge 8 ] || {
  echo "only $engaged blocks ever ENGAGED Dict's encoder; the rest declined and were"
  echo "stored, so the encoder comparison was comparing framing and nothing else."
  echo "Widen the corpus until the word counts clear MinLargeCnt/MinMediumCnt"
  echo "rather than deleting this check -- it is what makes the result mean anything."
  total=$((total+1)); }

echo "dict: $total total differing over $tested comparisons ($engaged blocks engaged the encoder)"
[ "$total" -eq 0 ] && echo "Dict matches the C original byte for byte, both directions" || exit 1
