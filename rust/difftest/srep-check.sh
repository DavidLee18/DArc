#!/usr/bin/env bash
# Differential-test the SREP decompressor port against the C original.
#
# SREP is not an in-process codec: the archiver spawns a `srep` binary per
# arc.ini's [External compressor:srep]. So this compares FILES produced by the
# two implementations rather than callback streams, and there is no
# DARC_RUST/fingerprint story for it.
#
# SREP has no specification and no independent implementation, so the C binary
# is the only oracle. It is built from srep/compile, which is why that build
# had to be fixed before any of this could run.
#
# ## The hash is forced to md5, and why
#
# SREP's default block hash is VMAC, which on this repo's ARM64 target is
# miscompiled in the vendored LibTomCrypt (the ulong32 bug): the reference
# `srep -d` intermittently rejects its OWN -m3f output with a VMAC checksum
# error, while every deterministic hash round-trips cleanly. Since the hash is
# orthogonal to the LZ format under test, the matrix forces -hash=md5 so the C
# round-trip guard is reliable and the port's md5 verification is exercised. A
# separate pass below covers the VMAC stream layout without depending on the C
# decoding its own VMAC output.
#
# ## Block size is not a tuning knob here, it is the test
#
# The whole point of SREP is a dictionary larger than RAM: a match whose source
# lies before the current block is re-read from the OUTPUT FILE already written.
# At the default 8 MB block size, every corpus input below fits in one block and
# that path is never reached. Disabling it in the port and re-running proves it:
#
#     -b16kb  ->  4 of 10 inputs differ   (path exercised)
#     default ->  0 of 10 inputs differ   (path never reached)
#
# So the small-block cases are mandatory, not extra. A corpus that fits in one
# block silently tests nothing that matters.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_srep_reference "$ROOT")" || exit 1

W="${TMPDIR:-/tmp}/srep-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# The reference binary is built from SREP's OWN pin, not the shared one and not
# the working tree (where the C is deleted). The shared pin predates two genuine
# SREP bug fixes and reproduces both -- see darc_srep_reference in
# c-reference.sh for what they were and how they showed up.
SREP="$CREF/Tests/srep"
if [ ! -x "$SREP" ]; then
  chmod +x "$CREF/srep/compile" 2>/dev/null || true
  ( cd "$CREF/srep" && ./compile ) >/dev/null 2>&1 || {
    echo "could not build the reference srep from the pinned tree" >&2; exit 1; }
fi
[ -x "$SREP" ] || { echo "pinned srep/compile produced no $SREP" >&2; exit 1; }
[ -x "$SREP" ] || { (cd "$ROOT/srep" && ./compile) >/dev/null 2>&1; }

( cd "$ROOT/rust" && cargo build --release -p darc-codecs --bin srep ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
# The port's own `srep` binary, invoked exactly as arc.ini does: `srep -d in out`.
DEC="$ROOT/rust/target/release/srep -d"

# Inputs aimed at a long-range matcher: far-apart duplicate regions, repeated
# sections separated by noise, long runs, and incompressible data where the
# encoder finds nothing and stores literals.
python3 - "$W/in" <<'PY'
import os,sys
d=sys.argv[1]; os.makedirs(d,exist_ok=True)
def prng(seed,n):
    s=seed; o=bytearray()
    for _ in range(n): s=(s*1103515245+12345)&0xffffffff; o.append((s>>16)&0xff)
    return bytes(o)
w=lambda n,b: open(f"{d}/{n}","wb").write(b)
w("text",    b"the quick brown fox jumps over the lazy dog. "*20000)
w("dup",     (prng(1,100000)+prng(2,50000))*6)
w("noise",   prng(9,900000))
w("runs",    b"".join(bytes([i%251])*997 for i in range(900)))
w("mixed",   b"".join(prng(i,2000)+b"COMMON-SECTION"*400 for i in range(120)))
w("farapart",prng(4,300000)+prng(5,300000)+prng(4,300000))
for n in (0,1,100,4096,65536):
    w(f"n_{n}", prng(3,n))
PY

# All four format versions, and both block regimes.
#
#   -mNo  -> I/O-LZ      : v1 when N=3 (rounded, 3-word records), else v2
#   -mNf  -> Future-LZ   : v3, records stored with their source block
#   -mN   -> Index-LZ    : v4, records in a footer  (the DEFAULT)
#
# Small block sizes are not decoration: at the default 8 MB every corpus input
# fits in one block, and the cross-block paths -- readback from the output file,
# and matches pending across blocks -- are never reached.
#
# -m5 (SREP_METHOD5, exhaustive search) is deliberately absent: the reference
# COMPRESSOR aborts on it for some inputs on Linux/glibc-x86-64 (a crash in a
# match-finder path this port does not touch -- SREP is decode-only). It also
# adds no format coverage, producing only v2 and v4, both already covered by
# -m1o/-m2o/-m4o and -m1/-m2/-m3. Every format version stays covered without it.
total=0
for opt in "-m3o" "-m1o" "-m2o" "-m4o" \
           "-m3f" "-m1f" "-m3" "-m1" "-m2" \
           "-m3o -b64kb" "-m1o -b64kb" "-m2o -b64kb" "-m3o -b16kb" \
           "-m3f -b64kb" "-m3f -b16kb" "-m1f -b16kb" \
           "-m3 -b64kb" "-m3 -b16kb" "-m1 -b16kb"; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); name=$(basename "$f")
    rm -f "$W/t.srep" "$W/t.out"
    # shellcheck disable=SC2086
    "$SREP" $opt -hash=md5 "$f" "$W/t.srep" >/dev/null 2>&1 || { echo "  [$opt] $name: C-compress FAILED"; fail=$((fail+1)); continue; }
    "$SREP" -d "$W/t.srep" "$W/t.c" >/dev/null 2>&1 || { echo "  [$opt] $name: C-decompress FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/t.c" || { echo "  [$opt] $name: C round-trip != original (harness bug)"; fail=$((fail+1)); continue; }
    $DEC "$W/t.srep" "$W/t.out" >/dev/null 2>&1 || { echo "  [$opt] $name: RUST FAILED"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/t.out" || { echo "  [$opt] $name: RUST != original"; fail=$((fail+1)); }
  done
  "$SREP" $opt -hash=md5 "$W/in/text" "$W/v.srep" >/dev/null 2>&1
  v=$(od -An -tu4 -N12 "$W/v.srep" | head -1 | awk "{print \$3%256}")
  echo "  [$opt -> v$v] $n inputs, $fail differing"
  total=$((total+fail))
done

# VMAC stream-layout pass: the default hash, whose 16-byte seed and per-block
# tag the port must consume correctly even though it does not verify them (see
# srep/hashes.rs). The C's own VMAC decode is unreliable here, so this compares
# the port against the ORIGINAL directly rather than gating on a C round-trip.
vfail=0; vn=0
for opt in "-m3" "-m3f -b16kb" "-m1o -b16kb"; do
  for f in "$W"/in/*; do
    vn=$((vn+1)); name=$(basename "$f")
    rm -f "$W/t.srep" "$W/t.out"
    # shellcheck disable=SC2086
    "$SREP" $opt -hash=vmac "$f" "$W/t.srep" >/dev/null 2>&1 || { vfail=$((vfail+1)); continue; }
    $DEC "$W/t.srep" "$W/t.out" >/dev/null 2>&1 || { echo "  [vmac $opt] $name: RUST FAILED"; vfail=$((vfail+1)); continue; }
    cmp -s "$f" "$W/t.out" || { echo "  [vmac $opt] $name: RUST != original"; vfail=$((vfail+1)); }
  done
done
echo "  [vmac carry] $vn inputs, $vfail differing"
total=$((total+vfail))

echo "srep decode: $total total differing"
[ "$total" -eq 0 ] && echo "SREP decoder matches the C original byte for byte (v1-v4)" || exit 1
