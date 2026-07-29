#!/usr/bin/env bash
# Differential-test 4x4 -- the threading meta-codec -- across the Rust codec
# substitution, BOTH directions.
#
# ## What this checks, and why it is not the usual shape
#
# Every other harness compares a C codec against a Rust PORT of that codec. 4x4
# has no Rust port and is not getting one (RUST_PORT_PROGRESS.md section 10): it
# compresses nothing itself, it splits input into blocks and calls the library
# dispatcher with an INNER method named in its own parameter string. Porting it
# would be Rust calling C calling Rust, and a decode-first port would drop the
# parallelism that is the whole point.
#
# That decision leaves something worth testing. 4x4's output is its own framing
# wrapped around whatever the dispatcher resolves the inner method to -- and
# those inner codecs are now Rust drop-ins. So the SAME driver is built twice
# from the SAME pinned C source: once linking the pinned C codecs, once with
# -DDARC_RUST plus the Rust staticlib. The question is whether substituting Rust
# underneath 4x4 changed the stream.
#
# This matters because 4x4 is on the default path, not a curiosity:
# Compression.hs:474-481 defines 3binary..9binary as `4x4:bNm:lzma:...`, so
# -m3 through -m9 route the $binary group through it, and 1xb/2xb use
# `4x4:tor:N`.
#
# ## Three things that will bite whoever edits this
#
#   * It needs the `dropin` cargo feature, unlike every other harness. Those
#     call `darc_rs_*` directly; this one reaches Rust THROUGH the C dispatcher,
#     which calls the archiver's own symbol names (tor_decompress, rep_compress,
#     ...) and those are what `dropin` exports.
#   * The two sides come from DIFFERENT trees, and that is deliberate. The C
#     side is the pinned revision; the Rust side is the WORKING TREE's wrappers
#     plus the staticlib. The first version of this script built BOTH from the
#     pinned tree and merely added -DDARC_RUST to one -- which does nothing,
#     because at the pinned revision C_Tornado.cpp and C_REP.cpp have no
#     DARC_RUST guards at all (those exclusions were added later). So four of
#     the seven inner methods compiled the C implementation into the object
#     file, the linker resolved from there, and the "Rust" driver was running C.
#     It reported 252/252 identical while testing nothing: sabotaging the Rust
#     Tornado encoder and the Rust REP encoder both went undetected.
#   * Hence the marker assertion below. `strings` for darc-codecs panic
#     locations is the same trick the unarc-sfx CI job uses, and it is the only
#     cheap proof that the substitution actually happened.
#   * It rebuilds the staticlib WITH `--features darc-codecs/dropin`, and that
#     is incompatible with tornado-encode-check.sh / tornado-check.sh, whose
#     pinned tornado_ccodec.cpp defines tor_decompress itself and so collides
#     with the drop-in export ("duplicate symbol '_tor_decompress'"). Each
#     harness rebuilds before running, so cargo re-resolves the features and
#     they are fine SEQUENTIALLY -- but they must never run CONCURRENTLY
#     against the shared rust/target, and they must not be put in a CI step
#     that could interleave them.
#   * The inner methods are restricted to codecs that actually HAVE a Rust
#     drop-in. `lzma` is the other one the presets use, but it has no Rust port
#     at all, so that comparison would compare a build against itself while
#     dragging the whole 7-Zip SDK into the link.
# ## KNOWN FAILING CASE -- do not wire into CI until it is fixed
#
# This harness currently FAILS on `4x4:b256k:tor:9` and `4x4:b1m:tor:9`, and
# that is a real finding, not a harness defect. Holding 4x4's framing and the
# dispatcher identical and varying ONLY Tornado (pinned C engine vs the Rust
# port) still diverges: 616270 vs 616271 bytes at 256k, 607081 vs 607077 at 1m,
# identical at 64k and for tor:3/tor:6. Minimal repro ~141 KB of input, payloads
# byte-identical for 30086 bytes and then differing.
#
# Cause: Tornado's port is byte-identical at `compress_all_at_once = 0` -- the
# default, and the only value tornado-encode-check.sh ever tests, because
# tornado_ccodec.cpp passes the live global and nothing sets it. 4x4 forces it
# to 1 (C_4x4.cpp:559-566) and is the only caller that does. So preset 9 in
# all-at-once mode was never covered.
#
# No shipped preset reaches it (1xb/2xb are tor:3/tor:6), so this is not urgent,
# but Tornado is one of DArc's own formats and byte-exactness is the stated bar.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
CFLAGS_C="$(darc_codec_cflags 4x4)" || exit 1
W="${TMPDIR:-/tmp}/4x4-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# --features darc-codecs/dropin: see the note above. Building WITH it is a
# superset -- the darc_rs_* names every other harness uses stay exported -- so
# this cannot break a sibling harness that runs afterwards.
( cd "$ROOT/rust" && cargo build --release -p darc-codecs --features darc-codecs/dropin ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "the Rust staticlib is missing" >&2; exit 1; }
# Prove the drop-in names are actually there: without them the Rust variant
# silently falls back to nothing and the link fails later with a worse message.
# grep -c, not grep -q: `set -o pipefail` is on, and `grep -q` exits at the
# first match, which SIGPIPEs nm and makes the whole pipeline report failure
# even though the symbol was found. That fired on the first run of this script.
dropins=$(nm -g "$LIB" 2>/dev/null | grep -c ' T _\{0,1\}tor_decompress$')
[ "${dropins:-0}" -ge 1 ] \
  || { echo "the staticlib has no drop-in exports -- was it built without the dropin feature?" >&2; exit 1; }

# Inner codecs, all of which have Rust drop-ins. C_4x4.cpp itself plus the
# dispatcher and Common; each C_*.cpp self-registers with AddCompressionMethod,
# so a codec that is not linked is simply not a valid inner method.
WRAPPERS="4x4/C_4x4.cpp Tornado/C_Tornado.cpp REP/C_REP.cpp LZP/C_LZP.cpp
          Dict/C_Dict.cpp Delta/C_Delta.cpp CompressionLibrary.cpp Common.cpp"
# $lib is a SEPARATE parameter placed after every source: GNU ld resolves an
# archive only against undefineds it has already seen, so a staticlib listed
# first contributes nothing (links on macOS, fails on Linux).
build() { # build OUT TREE [lib...]
  local out="$1" tree="$2"; shift 2
  local src=""; for w in $WRAPPERS; do src="$src $tree/Compression/$w"; done
  # shellcheck disable=SC2086  # flag and source lists are word lists on purpose
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$tree" -I"$tree/Compression" "$@" \
    "$ROOT/rust/difftest/4x4_ref.cpp" $src -o "$out"; }

# C side: the pinned tree, all C, no staticlib.
build "$W/c"  "$CREF"                            || { echo "C reference build failed" >&2; exit 1; }
# Rust side: the WORKING TREE's wrappers -- which are the thin forwarders the
# port left behind -- over the Rust staticlib.
build "$W/rs" "$ROOT" -DDARC_RUST "$LIB"         || { echo "Rust-substituted build failed" >&2; exit 1; }
[ -x "$W/c" ] && [ -x "$W/rs" ] || { echo "a driver is missing after a clean build" >&2; exit 1; }

# The assertion that keeps this honest. Rust embeds source paths for panic
# locations; they survive into the binary as data. If the Rust driver has none,
# the staticlib was never pulled in and every comparison below is C against C.
rs_markers=$(strings -a "$W/rs" 2>/dev/null | grep -c 'darc-codecs/src/')
c_markers=$(strings -a "$W/c"  2>/dev/null | grep -c 'darc-codecs/src/')
[ "${rs_markers:-0}" -ge 5 ] || {
  echo "the Rust driver contains $rs_markers darc-codecs markers -- the staticlib was"
  echo "not linked in, so this would compare C against C and pass while testing nothing."
  exit 1; }
[ "${c_markers:-0}" -eq 0 ] || {
  echo "the C reference driver contains $c_markers darc-codecs markers -- it is not pure C."
  exit 1; }

python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
# Inputs must be big enough to span SEVERAL blocks at the sizes swept below,
# because single-block input never exercises the framing this is here to test.
w("text",    b"the quick brown fox jumps over the lazy dog. "*20000)
w("english", (b"compression algorithms rearrange data so that statistical "
              b"redundancy can be removed by an entropy coder. ")*8000)
w("mixed",   b"".join((b"chunk-%d-" % i) + prng(i, 300) for i in range(2000)))
w("runs",    b"".join(bytes([i%97])*(1+(i*7)%400) for i in range(4000)))
w("noise",   prng(9, 900000))
w("zeros",   b"\x00"*400000)
w("exe",     (b"\x7fELF\x02\x01\x01" + prng(3,120))*4000)
for n in (0,1,255,256,65537):
    w(f"n_{n}", prng(5,n))
PY

# Inner methods that have Rust drop-ins. `tor:3` and `tor:6` are exactly what
# the 1xb/2xb presets use.
INNER="tor:3 tor:6 tor:9 rep lzp dict delta"
total=0; tested=0
for bs in 64k 256k 1m; do
  for inner in $INNER; do
    m="4x4:b$bs:$inner"
    fail=0; n=0
    for f in "$W"/in/*; do
      n=$((n+1)); bn=$(basename "$f")
      rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr"
      "$W/c"  c "$m" < "$f" >| "$W/ec" 2>/dev/null \
        || { echo "  [$m] $bn: C-compress FAILED"; fail=$((fail+1)); continue; }
      "$W/rs" c "$m" < "$f" >| "$W/er" 2>/dev/null \
        || { echo "  [$m] $bn: RUST-substituted compress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$W/ec" "$W/er" \
        || { echo "  [$m] $bn: STREAM differs once Rust codecs are substituted"; fail=$((fail+1)); continue; }
      # Decode the C's stream with both, and require the original back.
      "$W/c"  d "$m" < "$W/ec" >| "$W/dc" 2>/dev/null \
        || { echo "  [$m] $bn: C-decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dc" \
        || { echo "  [$m] $bn: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
      "$W/rs" d "$m" < "$W/ec" >| "$W/dr" 2>/dev/null \
        || { echo "  [$m] $bn: RUST-substituted decompress FAILED"; fail=$((fail+1)); continue; }
      cmp -s "$f" "$W/dr" \
        || { echo "  [$m] $bn: RUST-substituted decode != original"; fail=$((fail+1)); }
      tested=$((tested+1))
    done
    [ "$fail" -eq 0 ] || echo "  [$m] $n inputs, $fail differing"
    total=$((total+fail))
  done
  echo "  [block=$bs] $(echo $INNER | wc -w | tr -d ' ') inner methods swept"
done

[ "$tested" -gt 0 ] || { echo "no inputs were processed -- the harness reached nothing"; exit 1; }
echo "4x4: $total total differing over $tested comparisons"
[ "$total" -eq 0 ] \
  && echo "4x4 framing is unchanged by the Rust codec substitution, both directions" \
  || exit 1
