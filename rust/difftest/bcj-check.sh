#!/usr/bin/env bash
# Differential-test the BCJ x86 (`exe`) filter port -- BOTH directions -- against
# the C original, at many read granularities.
#
# `exe` rewrites the displacement of every E8/E9 (CALL/JMP rel32) into an
# absolute target. Two things make it easy to get subtly wrong:
#
#   * It carries state across calls. `_bufferPos` is the absolute stream offset
#     and is ADDED to every displacement; `_prevMask` remembers which of the
#     last three positions held a branch byte. A port that resets either between
#     buffers round-trips perfectly when the whole input arrives in one read and
#     corrupts when it does not. Hence the chunk-size sweep: the same input is
#     fed as one buffer and as many, and the C and the Rust must agree at every
#     granularity.
#   * Random data essentially never contains E8/E9 at a position whose 5th byte
#     is 0x00 or 0xFF, so a corpus of noise would pass with the filter stubbed
#     out entirely. The corpus therefore carries real x86 machine code where the
#     host has any, plus synthetic code and hand-placed branches at every
#     alignment and across the 256 KiB buffer boundary -- and the run FAILS if
#     the corpus turns out to hold no branch bytes, or if encoding never
#     actually changed anything.
#
# Everything is gated on exit codes and byte comparisons; no tool prose is
# parsed. The C reference comes from a pinned revision, not the working tree --
# see c-reference.sh for why.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The reference is built the way DArc builds the LZMA directory's C++ wrappers
# (C_BCJ.cpp lives there): see darc_codec_cflags in c-reference.sh for why the
# makefile's flags, not an -O level, are the oracle.
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
W="${TMPDIR:-/tmp}/bcj-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_codecs.a"

# The staticlib goes AFTER the source that references it: GNU ld resolves an
# archive only against the undefined symbols it has already seen. macOS ld does
# not care, which is how three harnesses here once shipped never having linked.
cc() { # cc <output> [extra args, appended after the source]
  clang++ -std=c++17 $CFLAGS_C -w -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$CREF/rust/difftest/bcj_ref.cpp" "${@:2}" -o "$1"
}
cc "$W/c"                    || { echo "building the C reference failed" >&2; exit 1; }
cc "$W/rs" -DUSE_RUST "$LIB" || { echo "building the Rust driver failed"  >&2; exit 1; }

# ── Corpus ───────────────────────────────────────────────────────────────────
DARC_ROOT="$ROOT" python3 - "$W/in" "$W/manifest" <<'PY' || exit 1
import os, struct, sys

d, manifest = sys.argv[1], sys.argv[2]
os.makedirs(d, exist_ok=True)
def w(name, b): open(f"{d}/{name}", "wb").write(bytes(b))

def prng(seed, n):
    s = seed; o = bytearray()
    for _ in range(n):
        s = (s * 1103515245 + 12345) & 0xffffffff
        o.append((s >> 16) & 0xff)
    return bytes(o)

# --- real x86 machine code, if this host has any -----------------------------
# Not "a binary" -- a binary for the RIGHT architecture. An arm64 executable
# would be as branch-free as noise for this filter's purposes, and would make
# the corpus look richer than it is. ELF, Mach-O (including the x86_64 slice of
# a fat binary) and PE are all recognised, so this finds something on a Linux
# runner, on an Apple-silicon Mac, and in a Windows cross-build tree.
CPU_TYPE_X86_64 = 0x01000007
def x86_bytes(path, cap):
    try:
        with open(path, "rb") as f: head = f.read(4096)
    except OSError:
        return None
    if len(head) < 64: return None
    def whole():
        with open(path, "rb") as f: return f.read()
    if head[:4] == b"\x7fELF":                       # ELF: e_machine == EM_X86_64
        return whole()[:cap] if head[18:20] == b"\x3e\x00" else None
    if head[:4] == b"\xcf\xfa\xed\xfe":              # Mach-O 64, little endian
        return whole()[:cap] if struct.unpack("<i", head[4:8])[0] == CPU_TYPE_X86_64 else None
    if head[:4] in (b"\xca\xfe\xba\xbe", b"\xbe\xba\xfe\xca"):   # fat Mach-O
        n = struct.unpack(">I", head[4:8])[0]
        if n > 32: return None
        data = whole()
        for i in range(n):
            cputype, _sub, off, size, _al = struct.unpack(">iiIII", data[8 + i*20 : 28 + i*20])
            if cputype == CPU_TYPE_X86_64:
                return data[off : off + min(size, cap)]
        return None
    if head[:2] == b"MZ":                            # PE: IMAGE_FILE_MACHINE_AMD64
        e = struct.unpack("<I", head[0x3c:0x40])[0]
        data = whole()
        if data[e:e+4] == b"PE\0\0" and data[e+4:e+6] == b"\x64\x86":
            return data[:cap]
        return None
    return None

candidates = [
    "/bin/ls", "/bin/bash", "/usr/bin/grep", "/usr/bin/python3", "/usr/lib/dyld",
    "/lib/x86_64-linux-gnu/libc.so.6", "/usr/lib/x86_64-linux-gnu/libc.so.6",
    "/usr/lib/x86_64-linux-gnu/libstdc++.so.6", "/usr/bin/perl", "/usr/bin/openssl",
    os.path.join(os.environ.get("DARC_ROOT", ""), "Tests/arc-mhs-win64.exe"),
    os.path.join(os.environ.get("DARC_ROOT", ""), "Tests/arc"),
]
for root in ("/tmp/out/FreeArc", "/tmp/out/FreeArc-unarc"):
    if os.path.isdir(root):
        candidates += sorted(os.path.join(root, f) for f in os.listdir(root) if f.endswith(".o"))[:4]

real = 0
for c in candidates:
    if real >= 3: break
    b = x86_bytes(c, 1_200_000)
    # Small object files are fine, empty ones are not.
    if b and len(b) >= 8192:
        w(f"real{real}_{os.path.basename(c).replace('/', '_')}", b)
        real += 1

# --- synthetic x86-shaped code ----------------------------------------------
# Plausible instruction soup with CALL/JMP rel32 whose displacements are small
# and signed, i.e. whose most significant byte is 0x00 or 0xFF -- which is
# exactly the Test86MSByte test that decides whether a branch is converted.
# Without this, an input can be full of E8 bytes and still never reach the
# conversion.
def synth(seed, n):
    s = seed; out = bytearray()
    filler = [b"\x55", b"\x48\x89\xe5", b"\x8b\x45\xfc", b"\x83\xc0\x01",
              b"\x89\x45\xfc", b"\x0f\xb6\x00", b"\x48\x83\xec\x20", b"\x31\xc0",
              b"\xc3", b"\x66\x90", b"\x0f\x1f\x40\x00"]
    while len(out) < n:
        s = (s * 1103515245 + 12345) & 0xffffffff
        r = (s >> 16) & 0xffff
        if r % 7 == 0:
            out += bytes([0xE8 if r & 1 else 0xE9])
            disp = (r % 30000) - 15000 - len(out)      # nearby target, so MSB is 00/FF
            out += struct.pack("<i", disp)
        else:
            out += filler[r % len(filler)]
    return bytes(out[:n])

w("synth_small", synth(1, 60_000))
w("synth_big",   synth(2, 700_000))          # crosses the 256 KiB buffer twice

# --- hand-placed branches ----------------------------------------------------
# Every alignment, and every spacing from 1 to 8 bytes. Spacings of 1-3 are the
# only thing that makes prevMask nonzero, which is what selects the
# kMaskToBitNumber / kMaskToAllowedStatus paths and the inner re-encoding loop.
buf = bytearray(b"\x90" * 40_000)
p = 16
for gap in list(range(1, 9)) * 60:
    for op in (0xE8, 0xE9):
        if p + 8 >= len(buf): break
        buf[p] = op
        buf[p+1:p+5] = struct.pack("<i", (p * 13) % 4096 - 2048)
        p += gap
w("adjacent", buf)

# A solid run of branch bytes: prevMask saturates and every allowed-status entry
# is visited.
w("e8_run", b"\xe8" * 30_000)
w("e9_run", b"\xe9" * 30_000)
w("e8e9",   b"\xe8\xe9" * 15_000)

# Branches whose 5 bytes straddle the 256 KiB read boundary, where the wrapper's
# remainder memmove and the carried prevMask have to agree with a single-buffer
# run. LARGE_BUFFER_SIZE is 262144 (Compression.h:41).
LB = 262144
for off in (LB - 5, LB - 4, LB - 3, LB - 2, LB - 1, LB, LB + 1):
    b = bytearray(prng(off & 0xff, LB + 64))
    for i in range(len(b)):                       # clear stray branch bytes first
        if b[i] & 0xFE == 0xE8: b[i] = 0x90
    b[off] = 0xE8
    b[off+1:off+5] = struct.pack("<i", -64)
    b[off-9] = 0xE9                               # a second one just before it
    b[off-8:off-4] = struct.pack("<i", 64)
    w(f"boundary_{off}", b)

# Branch bytes in the last handful of positions, which are never convertible and
# must simply pass through.
for tail in range(1, 9):
    b = bytearray(b"\x90" * 512)
    b[len(b) - tail] = 0xE8
    w(f"tail_{tail}", b)

# --- controls and degenerate sizes -------------------------------------------
w("noise", prng(9, 200_000))
w("zeros", b"\x00" * 50_000)
w("ff",    b"\xff" * 50_000)
w("text",  b"the quick brown fox jumps over the lazy dog. " * 2000)
for n in (0, 1, 2, 4, 5, 6, 7, 9, 10, 255, 4096):
    w(f"n_{n}", prng(3, n))
    b = bytearray(prng(4, n))
    if n >= 1: b[0] = 0xE8
    if n >= 6: b[5] = 0x00
    w(f"e8_{n}", b)

# --- report what the corpus actually contains --------------------------------
branch = total = files = 0
for f in sorted(os.listdir(d)):
    data = open(f"{d}/{f}", "rb").read()
    branch += sum(1 for x in data if x & 0xFE == 0xE8)
    total += len(data); files += 1
with open(manifest, "w") as m:
    m.write(f"{files} {total} {branch} {real}\n")
print(f"corpus: {files} files, {total} bytes, {branch} E8/E9 bytes, {real} real x86 binaries")
PY

read -r NFILES NBYTES NBRANCH NREAL < "$W/manifest" || exit 1
# A corpus with no branch bytes would pass with the filter stubbed out. This is
# the check that makes the rest of the run mean something.
[ "$NBRANCH" -ge 500 ] || { echo "corpus holds only $NBRANCH E8/E9 bytes -- it would not exercise the filter"; exit 1; }

# ── Compare ──────────────────────────────────────────────────────────────────
# Small inputs get every chunk size; big ones get the interesting subset, since
# a 1 MB file at chunk 6 is 170k round trips through the callback.
SMALL_CHUNKS="0 1 2 3 4 5 6 7 9 64 4096 65536 262143 262144 300000"
BIG_CHUNKS="0 6 7 4096 65536 262143 262144"

fail=0; cmps=0; ident=0; filtered=0; roundtrips=0
for f in "$W"/in/*; do
  bn=$(basename "$f"); sz=$(wc -c < "$f" | tr -d ' ')
  chunks=$SMALL_CHUNKS
  [ "$sz" -gt 100000 ] && chunks=$BIG_CHUNKS
  for ch in $chunks; do
    rm -f "$W/ec" "$W/er" "$W/dc" "$W/dr" "$W/rt" "$W/xrt"

    "$W/c"  c "$ch" < "$f" >| "$W/ec" 2>/dev/null || { echo "  $bn [chunk $ch]: C encode failed";    fail=$((fail+1)); continue; }
    "$W/rs" c "$ch" < "$f" >| "$W/er" 2>/dev/null || { echo "  $bn [chunk $ch]: Rust encode failed"; fail=$((fail+1)); continue; }
    "$W/c"  d "$ch" < "$f" >| "$W/dc" 2>/dev/null || { echo "  $bn [chunk $ch]: C decode failed";    fail=$((fail+1)); continue; }
    "$W/rs" d "$ch" < "$f" >| "$W/dr" 2>/dev/null || { echo "  $bn [chunk $ch]: Rust decode failed"; fail=$((fail+1)); continue; }

    # 1. encode: byte-identical streams
    cmps=$((cmps+1))
    if cmp -s "$W/ec" "$W/er"; then ident=$((ident+1))
    else echo "  $bn [chunk $ch]: ENCODE differs from the C"; fail=$((fail+1)); fi

    # 2. decode: the filter is symmetric and defined on any input, so the raw
    #    corpus is a valid decoder corpus too.
    cmps=$((cmps+1))
    if cmp -s "$W/dc" "$W/dr"; then ident=$((ident+1))
    else echo "  $bn [chunk $ch]: DECODE differs from the C"; fail=$((fail+1)); fi

    # Did encoding change anything at all? Counted, and required to be nonzero
    # overall: a stubbed filter would be byte-identical to a stubbed filter.
    cmp -s "$f" "$W/ec" || filtered=$((filtered+1))

    # 3. round trip through the Rust, and 4. across the two implementations
    #    (C encode -> Rust decode), which no single-implementation bug survives.
    "$W/rs" d "$ch" < "$W/er" >| "$W/rt"  2>/dev/null
    "$W/rs" d "$ch" < "$W/ec" >| "$W/xrt" 2>/dev/null
    cmps=$((cmps+2))
    if cmp -s "$f" "$W/rt"; then roundtrips=$((roundtrips+1))
    else echo "  $bn [chunk $ch]: Rust round trip differs from the input"; fail=$((fail+1)); fi
    if cmp -s "$f" "$W/xrt"; then roundtrips=$((roundtrips+1))
    else echo "  $bn [chunk $ch]: Rust decode of the C stream differs from the input"; fail=$((fail+1)); fi
  done
done

[ "$cmps" -gt 0 ] || { echo "bcj: no comparisons ran -- the harness reached nothing"; exit 1; }
# Chunk sizes of 5 and below never invoke the filter (C_BCJ.cpp:20 bypasses
# anything <= 5 bytes), and plenty of the corpus is deliberately branch-free, so
# "some runs changed nothing" is expected -- "almost no run changed anything"
# means the filter is not running. A full run makes this ~110.
[ "$filtered" -ge 20 ] || { echo "bcj: only $filtered runs changed a byte -- the filter is not running"; exit 1; }
[ "$fail" -eq 0 ] || { echo "bcj: $fail failures over $cmps comparisons"; exit 1; }

echo "bcj: $ident/$((cmps/2)) streams byte-identical to the C and $roundtrips round trips exact"\
     "-- $NFILES inputs ($NBRANCH E8/E9 bytes, $NREAL real x86 binaries) x up to 15 chunk sizes,"\
     "$filtered runs actually filtered"
