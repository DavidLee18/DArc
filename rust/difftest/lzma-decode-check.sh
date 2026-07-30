#!/usr/bin/env bash
# Differential harness for DArc's LZMA DECODER.
#
# ── Why this is not shaped like the encoder harnesses ────────────────────────
#
# Every other <codec>-check.sh here gates on byte-identity of the COMPRESSED
# stream, because a compressor has enormous freedom and matching the C byte for
# byte is the only proof the parse was reproduced. A decompressor has no such
# freedom: any correct decoder emits the same plaintext. Byte-identity of the
# output is therefore necessary but nearly free, and it proves almost nothing.
#
# So the gates that carry weight here are the ones that are NOT about the happy
# path:
#
#   (a) round-trip -- the encoder's stream must decode back to the input, swept
#       over the parameter space DArc actually ships, including its own default
#       matchFinder (kHT4, a 5-byte hash CHAIN, C_LZMA.cpp:253) rather than BT4;
#   (b) EOPM POSITION -- exactly where the decoder stops. Appending junk after
#       the end-of-payload marker must change nothing: not the output, not the
#       byte count, not the number of input bytes consumed. A decoder that runs
#       one symbol long or one symbol short still round-trips perfectly on every
#       stream that ends at EOF, and is broken for every stream that does not;
#   (c) MALFORMED input -- compared on CLASSIFICATION (accept vs reject) rather
#       than on bytes, plus the separate requirement that neither implementation
#       crashes. A decoder that accepts what the C rejects is a security bug, and
#       one that rejects what the C accepts makes archives unreadable;
#   (d) a content corpus that reaches the dictionary WINDOW WRAP. The window is
#       the part of a decoder a small corpus never touches: everything under
#       dicBufSize bytes decodes identically whether the wrap works or not.
#
# ── Running it before the Rust decoder exists ────────────────────────────────
#
# This script is deliberately complete against the C ALONE. Absent a Rust driver
# it runs every case, checks every expectation that does not need a second
# implementation, and says loudly that it is in C-only mode. That is not a
# degraded mode with no teeth: the accept/reject expectations below are written
# out from the C source (LzmaDec.c:537, :966, :981, :1273) rather than recorded
# from a run, so the C is checked against the source's stated contract, and the
# sabotage rehearsal at the bottom of this comment relies on exactly that.
#
#     LZMA_DEC_RS=<path>        use this binary as the Rust decoder driver
#     LZMA_DEC_RS_BIN=<name>    cargo bin name (default lzma_dec_rs_ref)
#     LZMA_DEC_REQUIRE_RUST=1   C-only mode becomes a failure (for CI, later)
#     LZMA_DEC_C=<path>         override the C driver -- used to rehearse a
#                               deliberate defect and prove this can fail
#     LZMA_DEC_QUICK=1          shrink the fuzz corpus (developer loop only)
#
# Gates on exit codes, never on grepping tool prose.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
# Sourced for its side effect of proving the pin is reachable, exactly as
# lzma-gap-check.sh does: the LZMA oracle is built from the WORKING TREE (see
# below), but a harness that silently skips the pinned-revision check would hide
# a broken pin from every other harness that does depend on it.
CREF="$(darc_c_reference "$ROOT")" || exit 1
: "$CREF"
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
# The SDK gets its OWN flags -- Compression/LZMA/makefile compiles 7z24/*.c with
# C7Z_CFLAGS, which unlike the wrapper set omits -fno-strict-aliasing. Merging
# the two would build an oracle DArc does not ship; PPMd is the standing proof
# that such a difference can change a codec's bytes.
SDK_CFLAGS="$(darc_lzma_sdk_cflags)" || exit 1

W="${TMPDIR:-/tmp}/lzma-dec.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT
mkdir -p "$W/content" "$W/stream" "$W/res"

# A corrupt stream can be productive rather than merely wrong: a mutated length
# or distance makes the decoder emit far more than the input ever held. Cap it,
# so a fuzz corpus cannot fill the disk, and report the cap distinctly (the
# driver's `capped=` field) so a capped run is never mistaken for a verdict.
export LZMA_DEC_OUT_CAP=16777216

# ---- build the C drivers ------------------------------------------------------
# File list taken from Compression/LZMA/makefile, not guessed. The WORKING TREE,
# not the pinned reference: this harness's job is to check the decoder DArc
# ships today, and lzma_dec_ref.cpp includes the working-tree C_LZMA.cpp by
# relative path anyway.
SDK="$ROOT/Compression/LZMA/7z24"
DEFS="-DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT -DZ7_ST"
objs=""
for c in LzmaEnc LzmaDec LzFind LzFindOpt CpuArch 7zStream; do
  # shellcheck disable=SC2086
  clang -c $SDK_CFLAGS -w $DEFS -I"$SDK" -o "$W/$c.o" "$SDK/$c.c" 2>>"$W/cbuild.log" \
    || { echo "compiling SDK $c.c failed" >&2; tail -20 "$W/cbuild.log" >&2; exit 1; }
  objs="$objs $W/$c.o"
done
build_driver () { # $1 = source basename, $2 = output
  # shellcheck disable=SC2086
  clang++ -std=c++17 $CFLAGS_C -w $DEFS \
    -I"$ROOT" -I"$ROOT/Compression" -I"$SDK" \
    "$ROOT/rust/difftest/$1" "$ROOT/Compression/Common.cpp" $objs \
    -o "$2" 2>>"$W/cbuild.log"
  [ -x "$2" ] || { echo "building $1 failed:" >&2; tail -25 "$W/cbuild.log" >&2; return 1; }
}
build_driver lzma_dec_ref.cpp "$W/cdec" || exit 1
# The C encoder is built too, as the fallback stream source when the Rust
# encoder is unavailable. The two are byte-identical (that is what
# lzma-gap-check.sh gates), so either produces the same corpus.
build_driver lzma_ref.cpp "$W/cenc" || exit 1

CDEC="${LZMA_DEC_C:-$W/cdec}"
[ -x "$CDEC" ] || { echo "no C decoder driver at $CDEC" >&2; exit 1; }

# ---- the encoder that produces the corpus -------------------------------------
# darc-lzma's encoder is already byte-exact against this same C (lzma-gap-check.sh
# gates on it), so using it here means the decode corpus is the stream DArc's
# Rust encoder actually emits and not merely a stream that resembles it.
ENC="$W/cenc"; ENC_WHICH="C (lzma_ref)"
( cd "$ROOT/rust" && cargo build --release -p darc-lzma --bin lzma_rs_ref ) >/dev/null 2>&1
if [ -x "$ROOT/rust/target/release/lzma_rs_ref" ]; then
  ENC="$ROOT/rust/target/release/lzma_rs_ref"; ENC_WHICH="Rust (lzma_rs_ref)"
fi

# ---- the Rust decoder driver, which may not exist yet -------------------------
RS_BIN="${LZMA_DEC_RS_BIN:-lzma_dec_rs_ref}"
RS="${LZMA_DEC_RS:-}"
if [ -z "$RS" ]; then
  ( cd "$ROOT/rust" && cargo build --release -p darc-lzma --bin "$RS_BIN" ) >/dev/null 2>&1
  RS="$ROOT/rust/target/release/$RS_BIN"
fi
HAVE_RS=0
[ -x "$RS" ] && HAVE_RS=1

# ---- corpus -------------------------------------------------------------------
# Everything the harness knows -- which stream, which parameters, and WHAT THE
# ANSWER MUST BE -- is decided here and written to a manifest. The expectations
# are derived from the C source, not recorded from a C run, so the C is measured
# against the contract rather than against itself.
#
# manifest fields, space separated:
#   group name dict lc lp pb fb mc mf algo chunk stream expect expfile
#   expconsumed expproduced maxrssmax tags
#     expect       accept | reject | reject:<rc> | any
#     expfile      path to the expected plaintext, or -
#     expconsumed  -, a number, or same:<case-name>
#     expproduced  -, a number, le:<n>, or same:<case-name>
#     maxrssmax    -, or an upper bound in BYTES
#     tags         comma-separated coverage categories, plus `dual` to also run
#                  the case with the other buffering and require the same answer
python3 - "$W" "$ENC" "${LZMA_DEC_QUICK:-0}" <<'PY' || { echo "corpus generation failed" >&2; exit 1; }
import os, subprocess, sys

W, ENC, QUICK = sys.argv[1], sys.argv[2], sys.argv[3] == "1"
C, S = os.path.join(W, "content"), os.path.join(W, "stream")
man = []

def prng(seed, n):
    s = seed & 0xffffffff; o = bytearray()
    while len(o) < n:
        s = (s * 1103515245 + 12345) & 0xffffffff
        o += s.to_bytes(4, "little")
    return bytes(o[:n])

def content(name, data):
    p = os.path.join(C, name)
    with open(p, "wb") as f: f.write(data)
    return p

def encode(name, cpath, params):
    """Run the encoder; return the stream path. Aborts the whole run on failure:
    a corpus that silently lost cases is how a sweep reports a clean pass over
    configurations it never touched."""
    p = os.path.join(S, name)
    with open(cpath, "rb") as i, open(p, "wb") as o:
        r = subprocess.run([ENC] + [str(x) for x in params], stdin=i, stdout=o,
                           stderr=subprocess.DEVNULL)
    if r.returncode != 0:
        sys.exit(f"encoder failed for {name} with {params}")
    return p

def case(group, name, params, chunk, stream, expect="any", expfile="-",
         cons="-", prod="-", rss="-", tags=""):
    man.append(" ".join(str(x) for x in
        [group, name] + list(params) + [chunk, stream, expect, expfile,
                                        cons, prod, rss, tags]))

# ── (d) the content corpus ───────────────────────────────────────────────────
# Empty and 1-byte are the degenerate ends; all-zeros and highly-repetitive make
# the parse almost entirely matches; incompressible makes it almost entirely
# literals; and a real binary is the only one of these with the byte statistics
# an archiver actually meets.
BIN = os.path.join(W, "cenc")            # a real Mach-O/ELF, whatever we are on
with open(BIN, "rb") as f: real_binary = f.read()[:300000]

CONTENTS = [
    ("empty",   b""),
    ("one",     b"\x5a"),
    ("zeros",   bytes(50000)),
    ("noise",   prng(7, 50000)),
    ("repeat",  b"".join(bytes([i % 251]) * (1 + (i * 13) % 400) for i in range(900))),
    ("text",    b"the quick brown fox jumps over the lazy dog. " * 2700),
    ("binary",  real_binary),
    ("runs",    b"".join(bytes([i % 7]) * (1 + (i * 29) % 900) for i in range(700))),
]
for n, d in CONTENTS: content(n, d)

# ── (a) round-trip over the parameter space ──────────────────────────────────
# DArc's OWN default is first, and it is the one that was easiest to miss: no
# preset in Compression.hs names a match finder, so every -mlzma archive DArc has
# written used mf=kHT4/algo=1, not the BT4 that every hand-written case reaches
# for. lc/lp/pb matter to the decoder directly -- they size and index the
# literal probability table -- so a non-default triple is swept too.
PARAMS = [
    ("dflt",  (67108864, 3, 0, 2,  32, 0, 4, 1)),   # C_LZMA.cpp:249-257
    ("bt4",   ( 1048576, 3, 0, 2,  32, 0, 2, 1)),
    ("small", (   65536, 3, 0, 2,  32, 0, 2, 1)),
    ("dmin",  (    4096, 3, 0, 2, 273, 0, 2, 1)),   # LZMA_DIC_MIN
    ("lits",  ( 1048576, 0, 2, 0,  32, 0, 2, 1)),   # lc0 lp2 pb0
    ("fast",  ( 1048576, 4, 0, 2,  32, 0, 3, 0)),   # lc4, HC4, fast parser
]
for pn, pp in PARAMS:
    for cn, cd in CONTENTS:
        cp = os.path.join(C, cn)
        st = encode(f"rt_{pn}_{cn}", cp, pp)
        case("A", f"rt_{pn}_{cn}", pp, 1, st, "accept", cp,
             prod=len(cd), tags="roundtrip")

# ── (d) dictionary window wrap ───────────────────────────────────────────────
# The decoder's ring buffer is exactly dicSize bytes (LzmaDec_Allocate), so
# nothing under that size can tell a working wrap from a missing one -- and the
# wrap is where `dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)]` earns its
# keep. Sizes straddle the boundary by one byte in each direction, then run many
# multiples past it.
for ds in (4096, 65536):
    pp = (ds, 3, 0, 2, 32, 0, 2, 1)
    sizes = [ds - 1, ds, ds + 1, 2 * ds, 2 * ds + 1]
    if not QUICK:
        sizes.append(ds * 31)
    for sz in sizes:
        for shape, data in (
            ("noise", prng(ds + sz, sz)),
            # A period that does not divide the dictionary, so matches land at a
            # different phase of the ring on every lap.
            ("cycle", (prng(3, 1021) * (sz // 1021 + 1))[:sz]),
        ):
            nm = f"wrap_{ds}_{sz}_{shape}"
            cp = content(nm, data)
            st = encode(nm, cp, pp)
            tag = "wrap" if sz > ds else "roundtrip"
            case("A", nm, pp, 1, st, "accept", cp, prod=sz, tags=tag)

# ── (b) EOPM position ────────────────────────────────────────────────────────
# The sharpest cheap test in the whole harness. Junk after the marker must be
# invisible: same output, same produced count, and the SAME consumed count --
# the decoder has to stop at the marker rather than at end of input. The
# surrounding 4x4 layer reads a shared stream, so a decoder that swallows one
# byte too many corrupts the block that follows rather than the block it read.
#
# chunk=1 throughout, and that is load-bearing. `lzma_decompress` reads through a
# 64 KiB buffer (C_LZMA.cpp:169), so the bytes it takes from the callback are not
# the bytes it consumes; feeding one byte at a time makes the marker's position
# observable from outside the decoder.
JUNK = [1, 2, 20, 65536]
eopm_bases = [("small", "text"), ("small", "noise"), ("dmin", "repeat"),
              ("dflt", "binary"), ("bt4", "one"), ("bt4", "empty")]
for pn, cn in eopm_bases:
    pp = dict(PARAMS)[pn]
    cp = os.path.join(C, cn)
    base = f"eopm_{pn}_{cn}"
    st = os.path.join(S, f"rt_{pn}_{cn}")
    with open(st, "rb") as f: raw = f.read()
    case("B", base, pp, 1, st, "accept", cp, prod=os.path.getsize(cp),
         tags="eopm_junk")
    for j in JUNK:
        nm = f"{base}_junk{j}"
        p = os.path.join(S, nm)
        # 0xA5, not zeros: a zero byte is a plausible continuation of a range
        # coder, so junk made of zeros can be decoded rather than ignored and
        # would make this test pass for the wrong reason.
        with open(p, "wb") as f: f.write(raw + b"\xa5" * j)
        case("B", nm, pp, 1, p, "accept", cp,
             cons=f"same:{base}", prod=f"same:{base}", tags="eopm_junk")

# The marker landing exactly on the 64 KiB output-buffer boundary
# (C_LZMA.cpp:170), and one byte either side of it -- the point at which the
# decode loop returns to the caller with a full buffer and must then discover
# that the next thing in the stream is the end.
pp = dict(PARAMS)["bt4"]
for sz in (65535, 65536, 65537, 131071, 131072, 131073):
    for shape, data in (("noise", prng(sz, sz)),
                        ("cycle", (prng(5, 977) * (sz // 977 + 1))[:sz])):
        nm = f"bound_{sz}_{shape}"
        cp = content(nm, data)
        st = encode(nm, cp, pp)
        case("B", nm, pp, 1, st, "accept", cp, prod=sz, tags="eopm_boundary")

# A final match that overruns the 64 KiB output limit by 1..273 bytes, forcing
# LzmaDec_WriteRem (LzmaDec.c:616-650) to carry the remainder into the next call.
# fb=273 caps matches at the maximum length, and a 61-byte period guarantees the
# parse takes maximum-length matches all the way to the end, so for every k the
# last match starts at 65536+k-273 -- before the boundary for k<273, exactly on
# it for k=273.
wr_pp = (65536, 3, 0, 2, 273, 0, 2, 1)
period = prng(17, 61)
ks = range(1, 274) if not QUICK else (1, 2, 3, 137, 271, 272, 273)
for k in ks:
    sz = 65536 + k
    nm = f"writerem_{k}"
    cp = content(nm, (period * (sz // 61 + 1))[:sz])
    st = encode(nm, cp, wr_pp)
    case("B", nm, wr_pp, 1, st, "accept", cp, prod=sz, tags="writerem")

# ── (c) malformed input ──────────────────────────────────────────────────────
# The base victim: small enough that thousands of mutations are cheap, big
# enough that its stream contains literals, matches, rep-matches and an EOPM.
vic_pp = (65536, 3, 0, 2, 32, 0, 2, 1)
vic_data = (b"the quick brown fox jumps over the lazy dog. " * 60
            + prng(21, 900) + b"abcabcabcabc" * 60)
vic_c = content("victim", vic_data)
vic_s = encode("victim", vic_c, vic_pp)
with open(vic_s, "rb") as f: victim = f.read()
case("C", "victim_ok", vic_pp, 1, vic_s, "accept", vic_c,
     prod=len(vic_data), tags="roundtrip,dual")

# props[0] across 0..255. LzmaProps_Decode (LzmaDec.c:1273) rejects d >= 9*5*5 =
# 225 outright with SZ_ERROR_UNSUPPORTED, which C_LZMA.cpp:165 turns into
# FREEARC_ERRCODE_INVALID_COMPRESSOR (-2). Below 225 the byte always decomposes
# into some (lc, lp, pb), so the props are ACCEPTED even when they are not the
# ones the stream was written with -- the failure then comes later and looks
# completely different (-7). Distinguishing those two rejections is the point.
#
# encode_props (C_LZMA.cpp:137) computes (pb*5 + lp)*9 + lc and truncates to a
# byte, so every b in 0..255 is reachable by the inverse decomposition below.
for b in range(256):
    lc, d = b % 9, b // 9
    lp, pb = d % 5, d // 5
    pp = (65536, lc, lp, pb, 32, 0, 2, 1)
    if b >= 225:
        case("C", f"props_{b}", pp, 1, vic_s, "reject:-2", tags="props")
    elif b == (2 * 5 + 0) * 9 + 3:       # the stream's own props: lc3 lp0 pb2
        case("C", f"props_{b}", pp, 1, vic_s, "accept", vic_c,
             prod=len(vic_data), tags="props")
    else:
        # Accepted as properties; the stream then decodes to nonsense or fails.
        # What must NOT happen is -2, which would mean the props were refused.
        case("C", f"props_{b}", pp, 1, vic_s, "any", tags="props")

# The same axis as a user can actually reach it: `-mlzma:8m:pbN` sets pb
# directly. MEASURED, not assumed -- the arithmetic truncates, so pb=5 is the
# value that trips the >= 225 check (25*9+3 = 228) while pb=9 wraps to
# (45*9+3) & 255 = 152 and is accepted as a different lc/lp/pb triple entirely.
for pbarg in range(16):
    v = ((pbarg * 5 + 0) * 9 + 3) & 0xFF
    pp = (65536, 3, 0, pbarg, 32, 0, 2, 1)
    exp = "reject:-2" if v >= 225 else ("accept" if pbarg == 2 else "any")
    ef = vic_c if pbarg == 2 else "-"
    case("C", f"pbarg_{pbarg}", pp, 1, vic_s, exp, ef, tags="props")

# dictSize edges. LzmaProps_Decode clamps anything below LZMA_DIC_MIN up to 4096
# (LzmaDec.c:1268), so 0/1/4095 are not rejections -- they silently become a
# 4 KiB window, which decodes correctly for any content that fits in it and
# fails for anything that does not. 0xFFFFFFFF is the one that must be watched
# rather than merely classified: it must not turn into a 4 GiB allocation.
for ds in (0, 1, 4095, 4096):
    pp = (ds, 3, 0, 2, 32, 0, 2, 1)
    case("C", f"dict_{ds}", pp, 1, vic_s, "any", tags="dictsize,dual")
case("C", "dict_4294967295", (4294967295, 3, 0, 2, 32, 0, 2, 1), 1, vic_s,
     "any", rss=1 << 30, tags="dictsize")

# A match distance beyond the decoder's window, reached the way an archive
# reaches it: encode with a dictionary large enough to find a far repeat, then
# decode with a smaller one. The third copy of block A is 400 KB behind the
# first, so at dictSize 65536 the distance exceeds checkDicSize and
# LzmaDec.c:537 must reject.
far_a, far_b = prng(31, 200000), prng(32, 200000)
far_c = content("far", far_a + far_b + far_a)
far_s = encode("far", far_c, (1048576, 3, 0, 2, 32, 0, 2, 1))
case("C", "far_ok", (1048576, 3, 0, 2, 32, 0, 2, 1), 1, far_s, "accept", far_c,
     prod=len(far_a) * 2 + len(far_b), tags="roundtrip,wrap")
case("C", "far_dict_too_small", (65536, 3, 0, 2, 32, 0, 2, 1), 1, far_s,
     "reject:-7", tags="distover,dual")

# The first byte of an LZMA stream is the range coder's cache and is always 0
# (RangeEnc_Init leaves cache=0). LzmaDec.c:966 rejects anything else.
for v in (1, 0x7f, 0x80, 0xff):
    p = os.path.join(S, f"firstbyte_{v}")
    with open(p, "wb") as f: f.write(bytes([v]) + victim[1:])
    case("C", f"firstbyte_{v}", vic_pp, 1, p, "reject:-7", tags="firstbyte,dual")

# A rep-match as the very first symbol is unrepresentable -- there is nothing to
# repeat -- and rather than test for it in the hot loop the SDK checks the range
# coder's initial code against kBadRepCode (LzmaDec.c:979-982), which is
# 0xC0000000 - 0x400 by the compile-time assertion at :666.
KBADREP = 0xC0000000 - 0x400
for label, code in (("eq", KBADREP), ("hi", 0xC0000000), ("max", 0xFFFFFFFF),
                    ("below", KBADREP - 1)):
    p = os.path.join(S, f"badrep_{label}")
    with open(p, "wb") as f:
        f.write(victim[:1] + code.to_bytes(4, "big") + victim[5:])
    # `below` is one short of the threshold: it must NOT be rejected by this
    # check. Without it the test would pass for a decoder that rejects
    # everything.
    exp = "any" if label == "below" else "reject:-7"
    case("C", f"badrep_{label}", vic_pp, 1, p, exp, tags="badrep,dual")

# ---- hand-built streams, for the three sides of LzmaDec.c:537 ---------------
# A real encoder cannot emit an out-of-range distance, so these are written by a
# minimal transcription of the SDK's own range coder (RangeEnc_ShiftLow,
# RC_BIT, LenEnc_Encode, the pos-slot tree and WriteEndMarker, LzmaEnc.c:685-2157).
# The transcription is SELF-CHECKED below: every case whose distance is legal
# must decode to the plaintext computed here, and if the range coder were wrong
# none of them would decode at all.
kProbInit, kTop = 1024, 1 << 24
kLiteralNextStates = [0,0,0,0,1,2,3,4,5,6,4,5]
kMatchNextStates   = [7,7,7,7,7,7,7,10,10,10,10,10]

class RC:
    def __init__(self):
        self.low = 0; self.range = 0xFFFFFFFF
        self.cache = 0; self.cacheSize = 0; self.out = bytearray()
    def shift_low(self):
        low = self.low & 0xFFFFFFFF; high = (self.low >> 32) & 0xFF
        self.low = (low << 8) & 0xFFFFFFFF
        if low < 0xFF000000 or high != 0:
            self.out.append((self.cache + high) & 0xFF)
            self.cache = (low >> 24) & 0xFF
            if self.cacheSize == 0: return
            h = (high + 0xFF) & 0xFF
            while True:
                self.out.append(h); self.cacheSize -= 1
                if self.cacheSize == 0: return
        else:
            self.cacheSize += 1
    def norm(self):
        if self.range < kTop:
            self.range = (self.range << 8) & 0xFFFFFFFF; self.shift_low()
    def bit(self, probs, i, b):
        ttt = probs[i]; nb = (self.range >> 11) * ttt
        if b == 0:
            self.range = nb; probs[i] = ttt + ((2048 - ttt) >> 5)
        else:
            self.low += nb; self.range -= nb; probs[i] = ttt - (ttt >> 5)
        self.norm()
    def direct(self, b):
        self.range >>= 1
        if b: self.low += self.range
        self.norm()

def pos_slot(dist):
    if dist < 2: return dist
    n = dist.bit_length() - 1
    return (n << 1) | ((dist >> (n - 1)) & 1)

class Hand:
    """Emits: N literals, one simple match with a CHOSEN reps[0], then EOPM."""
    def __init__(self, lc=3, lp=0, pb=2):
        self.lc, self.lp, self.pbMask = lc, lp, (1 << pb) - 1
        P = lambda n: [kProbInit] * n
        self.rc = RC()
        self.isMatch = [P(16) for _ in range(12)]; self.isRep = P(12)
        self.posSlotEnc = [P(64) for _ in range(4)]
        self.posEncoders = P(128); self.posAlign = P(16)
        self.lenLow = P(256); self.lenHigh = P(256)
        self.lit = P(0x300 << (lc + lp))
        self.state = 0; self.pos = 0; self.prev = 0
    def literal(self, byte):
        ps = self.pos & self.pbMask
        self.rc.bit(self.isMatch[self.state], ps, 0)
        if self.pos == 0:
            base = 0
        else:
            ls = ((self.pos & ((1 << self.lp) - 1)) << self.lc) + (self.prev >> (8 - self.lc))
            base = 0x300 * ls
        sym = byte | 0x100
        while True:
            i = base + (sym >> 8); b = (sym >> 7) & 1; sym = (sym << 1) & 0xFFFFFFFF
            self.rc.bit(self.lit, i, b)
            if sym >= 0x10000: break
        self.state = kLiteralNextStates[self.state]
        self.pos += 1; self.prev = byte
    def _len(self, sym, ps):
        low, high, rc = self.lenLow, self.lenHigh, self.rc
        base = 0
        if sym >= 8:
            rc.bit(low, 0, 1); base = 8
            if sym >= 16:
                rc.bit(low, 8, 1)
                s = (sym - 16) | 0x100
                while True:
                    i = s >> 8; b = (s >> 7) & 1; s = (s << 1) & 0xFFFFFFFF
                    rc.bit(high, i, b)
                    if s >= 0x10000: return
                return
            sym -= 8
        rc.bit(low, base, 0)
        o = base + (ps << 4)
        b = (sym >> 2) & 1; rc.bit(low, o + 1, b); m = 2 + b
        b = (sym >> 1) & 1; rc.bit(low, o + m, b); m = (m << 1) + b
        b = sym & 1;        rc.bit(low, o + m, b)
    def match(self, rep0, length):
        rc, ps = self.rc, self.pos & self.pbMask
        rc.bit(self.isMatch[self.state], ps, 1)
        rc.bit(self.isRep, self.state, 0)
        self.state = kMatchNextStates[self.state]
        self._len(length - 2, ps)
        dist = rep0 - 1
        slot = pos_slot(dist)
        probs = self.posSlotEnc[(length - 2) if length < 5 else 3]
        sym = slot + 64
        while sym < 4096:
            i = sym >> 6; b = (sym >> 5) & 1; sym <<= 1
            rc.bit(probs, i, b)
        if dist >= 4:
            footer = (slot >> 1) - 1
            if dist < 128:
                base = (2 | (slot & 1)) << footer
                m, d = 1, dist
                for _ in range(footer):
                    b = d & 1; d >>= 1; rc.bit(self.posEncoders, base + m, b)
                    m = (m << 1) | b
            else:
                p2 = ((dist | 0xF) << (32 - footer)) & 0xFFFFFFFF
                while True:
                    rc.direct(p2 >> 31); p2 = (p2 << 1) & 0xFFFFFFFF
                    if p2 == 0xF0000000: break
                m, d = 1, dist
                for _ in range(4):
                    b = d & 1; d >>= 1; rc.bit(self.posAlign, m, b); m = (m << 1) + b
        self.pos += length
    def finish(self):
        rc, ps = self.rc, self.pos & self.pbMask
        rc.bit(self.isMatch[self.state], ps, 1)
        rc.bit(self.isRep, self.state, 0)
        self.state = kMatchNextStates[self.state]
        self._len(0, ps)
        m = 1
        while m < 64: rc.bit(self.posSlotEnc[0], m, 1); m = (m << 1) + 1
        for _ in range(26): rc.direct(1)
        m = 1
        while m < 16: rc.bit(self.posAlign, m, 1); m = (m << 1) + 1
        for _ in range(5): rc.shift_low()
        return bytes(rc.out)

def hand(nlits, rep0, mlen):
    e, data = Hand(), bytearray()
    for i in range(nlits):
        b = (i * 37 + 11) & 0xFF
        e.literal(b); data.append(b)
    e.match(rep0, mlen)
    base = len(data) - rep0
    for k in range(mlen):
        src = base + k
        data.append(data[src] if 0 <= src < len(data) else 0)
    return e.finish(), bytes(data)

# LzmaDec.c:537 is `distance >= (checkDicSize == 0 ? processedPos : checkDicSize)`
# with distance = reps[0] - 1, so the boundary is reps[0] <= the limit.
# checkDicSize is 0 until processedPos reaches dicSize, which is why the same
# comparison has two completely different right-hand sides.
DS = 4096
HAND = [
    # name,              literals, reps[0],  len, expect
    ("hand_pos1_ok",            1,       1,   4, "accept"),   # distance 0 < 1
    ("hand_pos1_over",          1,       2,   4, "reject:-7"),# distance 1 >= 1
    ("hand_mid_ok",           100,     100,   4, "accept"),
    ("hand_mid_over",         100,     101,   4, "reject:-7"),
    ("hand_dic_minus1",  DS + 104, DS - 1,    4, "accept"),
    ("hand_dic_exact",   DS + 104, DS,        4, "accept"),
    ("hand_dic_plus1",   DS + 104, DS + 1,    4, "reject:-7"),
]
hpp = (DS, 3, 0, 2, 32, 0, 2, 1)
for nm, nl, r0, ml, exp in HAND:
    st, plain = hand(nl, r0, ml)
    p = os.path.join(S, nm); open(p, "wb").write(st)
    if exp == "accept":
        cp = content(nm, plain)
        case("C", nm, hpp, 1, p, exp, cp, prod=len(plain), tags="distance,dual")
    else:
        case("C", nm, hpp, 1, p, exp, tags="distance,dual")

# ── truncation ───────────────────────────────────────────────────────────────
# EVERY prefix length of a short valid stream. A decoder that reports success on
# a prefix has invented the tail; one that produces more plaintext than the whole
# stream held has invented output. Both are gated: `le:` below.
short_c = content("short", b"the quick brown fox jumps over the lazy dog. " * 9)
short_s = encode("short", short_c, vic_pp)
with open(short_s, "rb") as f: short = f.read()
case("C", "short_ok", vic_pp, 1, short_s, "accept", short_c,
     prod=os.path.getsize(short_c), tags="roundtrip")
for n in range(len(short)):
    p = os.path.join(S, f"trunc_short_{n}")
    with open(p, "wb") as f: f.write(short[:n])
    case("C", f"trunc_short_{n}", vic_pp, 1, p, "any",
         prod=f"le:{os.path.getsize(short_c)}", tags="trunc,dual")
# And on the larger victim: the first few bytes, where the range-coder init
# itself is incomplete, and the last few, where the end marker is.
for n in list(range(6)) + [len(victim) - k for k in range(1, 21)]:
    p = os.path.join(S, f"trunc_victim_{n}")
    with open(p, "wb") as f: f.write(victim[:n])
    case("C", f"trunc_victim_{n}", vic_pp, 1, p, "any",
         prod=f"le:{len(vic_data)}", tags="trunc,dual")

# ── mutation ─────────────────────────────────────────────────────────────────
# Single-bit and single-byte damage. No expectation beyond "does not crash" and
# "does not exceed the cap": what these are FOR is the cross-implementation
# accept/reject comparison, where a disagreement is the finding. In C-only mode
# they still earn their place through the `dual` sampling below, which requires
# the answer not to depend on how the input was buffered.
nbit, nbyte = (2000, 500) if not QUICK else (60, 20)
for i in range(nbit):
    off = (i * 7919) % len(victim)
    bit = i % 8
    m = bytearray(victim); m[off] ^= 1 << bit
    p = os.path.join(S, f"bit_{i}")
    with open(p, "wb") as f: f.write(bytes(m))
    case("C", f"bit_{i}", vic_pp, 1, p, "any",
         tags="mutate,dual" if i % 5 == 0 else "mutate")
for i in range(nbyte):
    off = (i * 4093) % len(victim)
    m = bytearray(victim); m[off] = (m[off] + 1 + i) & 0xFF
    p = os.path.join(S, f"byte_{i}")
    with open(p, "wb") as f: f.write(bytes(m))
    case("C", f"byte_{i}", vic_pp, 1, p, "any",
         tags="mutate,dual" if i % 5 == 0 else "mutate")

with open(os.path.join(W, "manifest"), "w") as f:
    f.write("\n".join(man) + "\n")
print(f"corpus: {len(man)} cases", file=sys.stderr)
PY

[ -s "$W/manifest" ] || { echo "empty manifest" >&2; exit 1; }

# ---- run ----------------------------------------------------------------------
# ru_maxrss is BYTES on Darwin and KiB everywhere else. Getting this backwards
# would make the 4 GiB gate either unfireable or always-firing.
case "$(uname -s)" in Darwin) RSS_UNIT=1 ;; *) RSS_UNIT=1024 ;; esac

DEC_RC=""; DEC_CONS=""; DEC_PROD=""; DEC_RSS=""; DEC_CLS=""
run_dec () { # $1 = driver, $2 = stream, $3 = stdout file, $4..$12 = argv
  local bin="$1" stream="$2" out="$3"; shift 3
  # RLIMIT_CPU rather than a timeout command: `timeout` is not on a stock macOS,
  # and a wrapper process per run would cost more than the run. A decoder that
  # loops shows up as exit 152 (SIGXCPU), which classify() calls a crash --
  # which is what a hang is.
  ( ulimit -t 30; exec "$bin" "$@" ) < "$stream" >| "$out" 2>| "$W/report"
  local ec=$?
  DEC_RC=$(sed -n -E 's/.*DARC_DEC rc=(-?[0-9]+).*/\1/p'   "$W/report" | head -1)
  DEC_CONS=$(sed -n -E 's/.*consumed=([0-9]+).*/\1/p'      "$W/report" | head -1)
  DEC_PROD=$(sed -n -E 's/.*produced=([0-9]+).*/\1/p'      "$W/report" | head -1)
  DEC_RSS=$(sed -n -E 's/.*maxrss=(-?[0-9]+).*/\1/p'       "$W/report" | head -1)
  if [ -z "$DEC_RC" ]; then DEC_CLS=crash; DEC_RC=""; return; fi
  case "$ec" in
    0) DEC_CLS=accept ;;
    1) DEC_CLS=reject ;;
    *) DEC_CLS=crash ;;
  esac
}

total=0; passed=0; f_class=0; f_out=0; f_cons=0; f_prod=0; f_rss=0; crashes=0
dual_runs=0; f_dual=0
rs_total=0; rs_out=0; rs_class=0; rs_rc=0; rs_cons=0; rs_prod=0; rs_crash=0
tags_seen=""
declare -a DETAIL=()
note () { [ "${#DETAIL[@]}" -lt 25 ] && DETAIL+=("  $1"); }

while IFS=' ' read -r grp name d lc lp pb fb mc mf algo chunk stream expect expfile expcons expprod rssmax tags <&3; do
  [ -n "${grp:-}" ] || continue
  total=$((total+1))
  tags_seen="$tags_seen ${tags//,/ }"

  run_dec "$CDEC" "$stream" "$W/oc" "$d" "$lc" "$lp" "$pb" "$fb" "$mc" "$mf" "$algo" "$chunk"
  c_cls="$DEC_CLS"; c_rc="$DEC_RC"; c_cons="$DEC_CONS"; c_prod="$DEC_PROD"; c_rss="$DEC_RSS"
  printf '%s %s %s\n' "${c_rc:-?}" "${c_cons:-?}" "${c_prod:-?}" >| "$W/res/$name"

  ok=1
  if [ "$c_cls" = crash ]; then
    crashes=$((crashes+1)); ok=0; note "CRASH  $name (C driver did not report)"
  fi

  # ---- expectation: accept / reject / reject:<rc> ----
  case "$expect" in
    any) : ;;
    accept) [ "$c_cls" = accept ] || { f_class=$((f_class+1)); ok=0
              note "CLASS  $name: expected accept, C gave $c_cls rc=${c_rc:-?}"; } ;;
    reject) [ "$c_cls" = reject ] || { f_class=$((f_class+1)); ok=0
              note "CLASS  $name: expected reject, C gave $c_cls rc=${c_rc:-?}"; } ;;
    reject:*) want="${expect#reject:}"
            if [ "$c_cls" != reject ] || [ "${c_rc:-}" != "$want" ]; then
              f_class=$((f_class+1)); ok=0
              note "CLASS  $name: expected reject rc=$want, C gave $c_cls rc=${c_rc:-?}"
            fi ;;
  esac

  # ---- expectation: exact plaintext ----
  if [ "$expfile" != "-" ] && [ "$c_cls" = accept ]; then
    if ! cmp -s "$expfile" "$W/oc"; then
      f_out=$((f_out+1)); ok=0
      # BSD cmp says "differ: char N"; GNU says "differ: byte N". `sed -E`, not
      # BRE: `\(a\|b\)` alternation is a GNU extension that matches nothing under
      # BSD sed, which is how an offset like this silently printed as "?".
      off=$(cmp "$expfile" "$W/oc" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
      note "OUTPUT $name: plaintext differs at byte ${off:-?} (expected $(wc -c < "$expfile" | tr -d ' '), got ${c_prod:-?})"
    fi
  fi

  # ---- expectation: consumed / produced ----
  if [ "$expcons" != "-" ]; then
    case "$expcons" in
      same:*) want=$(awk '{print $2}' "$W/res/${expcons#same:}" 2>/dev/null) ;;
      *)      want="$expcons" ;;
    esac
    if [ -n "$want" ] && [ "$want" != "?" ] && [ "${c_cons:-?}" != "$want" ]; then
      f_cons=$((f_cons+1)); ok=0
      note "CONSUM $name: expected $want input bytes consumed, C consumed ${c_cons:-?}"
    fi
  fi
  if [ "$expprod" != "-" ]; then
    case "$expprod" in
      same:*) want=$(awk '{print $3}' "$W/res/${expprod#same:}" 2>/dev/null); cmpop=eq ;;
      le:*)   want="${expprod#le:}"; cmpop=le ;;
      *)      want="$expprod"; cmpop=eq ;;
    esac
    if [ -n "$want" ] && [ "$want" != "?" ] && [ -n "${c_prod:-}" ]; then
      if [ "$cmpop" = eq ] && [ "$c_prod" != "$want" ]; then
        f_prod=$((f_prod+1)); ok=0
        note "PRODUC $name: expected $want output bytes, C produced $c_prod"
      elif [ "$cmpop" = le ] && [ "$c_prod" -gt "$want" ]; then
        f_prod=$((f_prod+1)); ok=0
        note "PRODUC $name: a truncated stream produced $c_prod bytes, more than the whole stream's $want"
      fi
    fi
  fi

  # ---- expectation: peak RSS ----
  if [ "$rssmax" != "-" ] && [ -n "${c_rss:-}" ] && [ "$c_rss" -ge 0 ]; then
    bytes=$((c_rss * RSS_UNIT))
    if [ "$bytes" -gt "$rssmax" ]; then
      f_rss=$((f_rss+1)); ok=0
      note "MEMORY $name: peak RSS ${bytes}B exceeds the ${rssmax}B ceiling"
    fi
  fi

  # ---- the buffering must not change the VERDICT ----
  # `lzma_decompress` reads through a 64 KiB buffer; whether the decoder sees the
  # stream in one chunk or one byte at a time is an artefact of that buffer and
  # must not reach the verdict. This is the only oracle a C-only run has for the
  # fuzz corpus, so it is applied to every truncation and to a fifth of the
  # mutations.
  #
  # The verdict and, for an ACCEPTED stream, the byte counts. Not the byte counts
  # of a REJECTED one, and that exclusion is a measured property of DArc's decode
  # loop rather than a convenience. C_LZMA.cpp:201-210 tests LzmaDec's return
  # code BEFORE writing outBuf, so when a call decodes some plaintext and then
  # hits the bad symbol, everything that call produced is dropped. With a 64 KiB
  # input buffer that is up to 64 KiB of already-valid plaintext; feeding one
  # byte at a time, the same stream emits nearly all of it before failing. So on
  # a corrupt stream `produced` is a function of the buffer size, not of the
  # stream -- 18 of the mutation and hand-built cases show it. Requiring the two
  # to agree would gate on an artefact.
  case ",$tags," in *,dual,*)
    alt=0; [ "$chunk" = 0 ] && alt=1
    dual_runs=$((dual_runs+1))
    run_dec "$CDEC" "$stream" "$W/od" "$d" "$lc" "$lp" "$pb" "$fb" "$mc" "$mf" "$algo" "$alt"
    if [ "$DEC_CLS" != "$c_cls" ] || [ "${DEC_RC:-?}" != "${c_rc:-?}" ]; then
      f_dual=$((f_dual+1)); ok=0
      note "BUFFER $name: chunk=$chunk gave $c_cls rc=${c_rc:-?}, chunk=$alt gave $DEC_CLS rc=${DEC_RC:-?}"
    elif [ "$c_cls" = accept ] && [ "${DEC_PROD:-?}" != "${c_prod:-?}" ]; then
      f_dual=$((f_dual+1)); ok=0
      note "BUFFER $name: accepted, but produced ${c_prod:-?} at chunk=$chunk and ${DEC_PROD:-?} at chunk=$alt"
    fi
  ;; esac

  # ---- the Rust decoder, when there is one ----
  if [ "$HAVE_RS" = 1 ]; then
    rs_total=$((rs_total+1))
    run_dec "$RS" "$stream" "$W/or" "$d" "$lc" "$lp" "$pb" "$fb" "$mc" "$mf" "$algo" "$chunk"
    if [ "$DEC_CLS" = crash ]; then
      rs_crash=$((rs_crash+1)); ok=0; note "CRASH  $name (Rust driver did not report)"
    else
      if [ "$DEC_CLS" != "$c_cls" ]; then
        rs_class=$((rs_class+1)); ok=0
        note "DIVERGE $name: C $c_cls rc=${c_rc:-?}, Rust $DEC_CLS rc=${DEC_RC:-?}"
      elif [ "${DEC_RC:-?}" != "${c_rc:-?}" ]; then
        # Same verdict, different FreeArc error code -- and that is still a
        # divergence, because DArc's callers act on the code. -2
        # (INVALID_COMPRESSOR) says the METHOD STRING is wrong and no archive
        # will ever read; -7 (BAD_COMPRESSED_DATA) says this stream is damaged.
        # Rehearsed: deleting LzmaDec.c:1273's props check turns 33 of the -2
        # rejections into -7 rejections, and an accept/reject comparison alone
        # sees nothing at all. Gated separately so an over-strict result is
        # legible rather than buried.
        rs_rc=$((rs_rc+1)); ok=0
        note "ERRCODE $name: both reject, C rc=${c_rc:-?} but Rust rc=${DEC_RC:-?}"
      fi
      # Counts are compared for accepted streams only: once the two disagree on
      # the verdict, comparing how far each got is noise on top of the finding.
      if [ "$DEC_CLS" = accept ] && [ "$c_cls" = accept ]; then
        [ "${DEC_CONS:-?}" = "${c_cons:-?}" ] || { rs_cons=$((rs_cons+1)); ok=0
          note "DIVERGE $name: consumed C=${c_cons:-?} Rust=${DEC_CONS:-?}"; }
        [ "${DEC_PROD:-?}" = "${c_prod:-?}" ] || { rs_prod=$((rs_prod+1)); ok=0
          note "DIVERGE $name: produced C=${c_prod:-?} Rust=${DEC_PROD:-?}"; }
        if ! cmp -s "$W/oc" "$W/or"; then
          rs_out=$((rs_out+1)); ok=0
          off=$(cmp "$W/oc" "$W/or" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
          note "DIVERGE $name: plaintext differs at byte ${off:-?}"
        fi
      fi
    fi
  fi

  [ "$ok" = 1 ] && passed=$((passed+1))
done 3< "$W/manifest"

# ---- coverage -----------------------------------------------------------------
# Gated for the same reason lzma-gap-check.sh gates its match-finder count: a
# sweep that silently stops covering an axis reports a clean run over the cases
# that were never hard. The window wrap in particular is invisible by omission --
# every stream shorter than the dictionary decodes identically with a broken one.
count_tag () { printf '%s\n' $tags_seen | grep -cx "$1" ; }
REQUIRED="roundtrip wrap eopm_junk eopm_boundary writerem props dictsize firstbyte badrep distance distover trunc mutate"
missing=""
for t in $REQUIRED; do
  n=$(count_tag "$t")
  [ "${n:-0}" -gt 0 ] || missing="$missing $t"
done

# ---- report -------------------------------------------------------------------
echo "lzma decode: $total cases, encoder = $ENC_WHICH"
for t in $REQUIRED; do printf '  %-14s %s\n' "$t" "$(count_tag "$t")"; done
echo "  buffering re-runs                     $dual_runs"
echo "C decoder:"
echo "  cases fully satisfying expectations   $passed"
echo "  accept/reject wrong                   $f_class"
echo "  plaintext wrong                       $f_out"
echo "  input-consumed count wrong            $f_cons"
echo "  output-produced count wrong           $f_prod"
echo "  peak RSS over ceiling                 $f_rss"
echo "  answer depended on buffering          $f_dual"
echo "  crashes / hangs                       $crashes"
if [ "$HAVE_RS" = 1 ]; then
  echo "Rust decoder ($RS):"
  echo "  compared                              $rs_total"
  echo "  accept/reject disagreements           $rs_class"
  echo "  same verdict, different error code    $rs_rc"
  echo "  plaintext disagreements               $rs_out"
  echo "  consumed-count disagreements          $rs_cons"
  echo "  produced-count disagreements          $rs_prod"
  echo "  crashes / hangs                       $rs_crash"
else
  echo "Rust decoder: ABSENT -- nothing was built at $RS"
  echo "  C-ONLY MODE. Every expectation above was checked against the C alone;"
  echo "  no cross-implementation comparison ran. Set LZMA_DEC_REQUIRE_RUST=1 to"
  echo "  make this a failure once the Rust driver lands."
fi
if [ "${#DETAIL[@]}" -gt 0 ]; then
  echo "findings:"
  printf '%s\n' "${DETAIL[@]}"
fi

rc=0
[ "$total" -gt 0 ]     || { echo "nothing was measured" >&2; rc=1; }
[ -z "$missing" ]      || { echo "corpus covered no case for:$missing" >&2; rc=1; }
[ "$crashes" -eq 0 ]   || { echo "$crashes C driver crash(es) or hang(s)" >&2; rc=1; }
[ "$f_class" -eq 0 ]   || { echo "$f_class case(s) where the C accepted what must be rejected, or the reverse" >&2; rc=1; }
[ "$f_out"   -eq 0 ]   || { echo "$f_out case(s) decoded to the wrong plaintext" >&2; rc=1; }
[ "$f_cons"  -eq 0 ]   || { echo "$f_cons case(s) consumed the wrong number of input bytes" >&2; rc=1; }
[ "$f_prod"  -eq 0 ]   || { echo "$f_prod case(s) produced the wrong number of output bytes" >&2; rc=1; }
[ "$f_rss"   -eq 0 ]   || { echo "$f_rss case(s) exceeded their memory ceiling" >&2; rc=1; }
[ "$f_dual"  -eq 0 ]   || { echo "$f_dual case(s) answered differently depending on input buffering" >&2; rc=1; }
if [ "$HAVE_RS" = 1 ]; then
  [ "$rs_total" -gt 0 ] || { echo "the Rust driver exists but was never run" >&2; rc=1; }
  [ "$rs_crash" -eq 0 ] || { echo "$rs_crash Rust driver crash(es) or hang(s)" >&2; rc=1; }
  [ "$rs_class" -eq 0 ] || { echo "$rs_class accept/reject disagreement(s) with the C" >&2; rc=1; }
  [ "$rs_rc"    -eq 0 ] || { echo "$rs_rc case(s) rejected with a different error code than the C" >&2; rc=1; }
  [ "$rs_out"   -eq 0 ] || { echo "$rs_out plaintext disagreement(s) with the C" >&2; rc=1; }
  [ "$rs_cons"  -eq 0 ] || { echo "$rs_cons consumed-count disagreement(s) with the C" >&2; rc=1; }
  [ "$rs_prod"  -eq 0 ] || { echo "$rs_prod produced-count disagreement(s) with the C" >&2; rc=1; }
elif [ "${LZMA_DEC_REQUIRE_RUST:-0}" = 1 ]; then
  echo "LZMA_DEC_REQUIRE_RUST=1 but no Rust decoder driver was found at $RS" >&2
  rc=1
fi
exit $rc
