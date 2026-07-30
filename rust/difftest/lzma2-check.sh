#!/usr/bin/env bash
# Differential harness for DArc's LZMA2, BOTH DIRECTIONS.
#
# ── What it gates, and why each gate exists ──────────────────────────────────
#
# ENCODE is gated on byte-identity of the whole stream, INCLUDING the leading
# single prop byte that C_LZMA2.cpp:96-98 writes outside the SDK. That byte is
# DArc's own framing; a port that got all of Lzma2Enc.c right and this byte wrong
# would write archives nothing can open, and a diff of the SDK's output alone
# would not see it.
#
# CHUNK STRUCTURE is gated separately, and the reason is diagnostic rather than
# logical. Chunking is a pure function of the stream's bytes, so a byte-identical
# stream cannot have diverging chunking -- but a DIVERGING stream is far cheaper
# to understand from "chunk 37 says unpack=48470 on one side and 48468 on the
# other" than from "first differs at byte 1794052". The trace is produced by
# PARSING the stream (lzma2_ref.cpp's `trace` direction), which means one parser
# can be pointed at both implementations' output and no SDK instrumentation is
# needed. Where a Rust tracer exists it is cross-checked against this one too.
#
# STRUCTURAL INVARIANTS are gated on every accepted stream, derived from the C
# source rather than recorded from a run:
#
#   * chunk mode 1 never appears. Lzma2Enc.c:201 needs needInitState true with
#     needInitProp false, and Lzma2EncInt_InitBlock sets both together (:106-111)
#     while :214-215 clears both together. Unreachable.
#   * modes 2 AND 3 both DO appear, which corrects the natural guess that DArc
#     only emits 3 and 0. Mode 3 is the block's first chunk when it is an LZMA
#     chunk; mode 2 is the first LZMA chunk when copy chunks came first, because
#     a copy chunk does not clear needInitState (the assignment at :190 is
#     commented out). Both are required to appear somewhere in the corpus.
#   * exactly one dictionary reset per non-empty stream, at chunk 0, and exactly
#     one block-init chunk, and it is the first LZMA chunk. All three follow from
#     the block being SOLID, which it is because the driver pins the thread count
#     to 1 (see lzma2_ref.cpp's header for what that isolates).
#   * every LZMA chunk payload starts with 0x00 -- the range coder's zero cache
#     after RangeEnc_Init. Lzma2Dec.c:414-419 rejects anything else.
#   * the trace's total unpack size equals the plaintext length.
#
# DECODE is gated on round-trip, on cross-decode BOTH WAYS (the Rust decoder over
# C streams, and -- the one that protects archives users already have -- the C
# decoder over Rust streams), and on accept-vs-reject over a set of malformed
# streams built one per documented error edge in Lzma2Dec.c. Error CODES are
# recorded where they differ rather than required to match: every data failure in
# C_LZMA2.cpp:157-158 collapses to FREEARC_ERRCODE_BAD_COMPRESSED_DATA, so the
# code carries almost no information here, unlike on the LZMA path where -2 and
# -7 mean genuinely different things.
#
# ── One thing the corpus provably cannot reach ───────────────────────────────
#
# Lzma2Enc.c:168-191 splits a copy block into 64 KiB chunks. That loop is DEAD
# CODE from `lzma2_compress`, and the bound is arithmetic, not luck:
# LzmaEnc_CodeOneBlock stops as soon as
# `RangeEnc_GetProcessed + kPackReserve >= maxPackSize` (LzmaEnc.c:2666-2667)
# with kPackReserve = 16384 and maxPackSize = LZMA2_PACK_SIZE_MAX = 65536, so a
# subblock's packSize never exceeds ~49157. `useCopyBlock` (:154) then needs
# either `packSize > (1 << 16)`, impossible at that bound, or
# `packSize + 2 >= unpackSize`, which forces unpackSize <= ~49159 < 64 KiB. The
# largest copy chunk this harness has ever observed is 48549, and the run prints
# that maximum every time so the claim stays visible rather than buried here.
#
# So instead of pretending to cover the split, the harness (a) gates `copymax <
# 65536` as the invariant that makes the reasoning above falsifiable -- if a copy
# chunk ever reaches 64 KiB, the split became reachable and this comment is wrong
# -- and (b) reaches the DECODER's 64 KiB copy path with hand-built streams,
# where it is perfectly reachable and where a Rust decoder will meet it if any
# other LZMA2 producer ever writes one.
#
# ── Running it before the Rust drivers exist ─────────────────────────────────
#
# Deliberately complete against the C ALONE. With no Rust driver it runs every
# case, checks every expectation that does not need a second implementation
# (round-trip, all the structural invariants, every accept/reject expectation,
# every coverage category), and says loudly that it is in C-only mode.
#
#     LZMA2_RS=<path>        use this binary as the Rust driver. It must take
#                            lzma2_ref.cpp's argv, including the direction
#                            selector. Pointing it at a deliberately broken build
#                            of the C is how the cross-implementation gates are
#                            rehearsed -- see rust/difftest notes in the PR.
#     LZMA2_RS_BIN=<name>    cargo bin name (default lzma2_rs_ref)
#     LZMA2_REQUIRE_RUST=1   C-only mode becomes a failure (for CI, later)
#     LZMA2_C=<path>         override the C driver, to rehearse a deliberate
#                            defect and prove the C-only gates can fail
#     LZMA2_QUICK=1          shrink the corpus (developer loop only)
#
# Gates on exit codes, never on grepping tool prose.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
# Sourced for its side effect of proving the pin is reachable, exactly as
# lzma-gap-check.sh does: the LZMA2 oracle is built from the WORKING TREE (see
# below), but a harness that silently skipped the pinned-revision check would
# hide a broken pin from every other harness that does depend on it.
CREF="$(darc_c_reference "$ROOT")" || exit 1
: "$CREF"
CFLAGS_C="$(darc_codec_cflags LZMA)" || exit 1
# The SDK gets its OWN flags -- Compression/LZMA/makefile compiles 7z24/*.c with
# C7Z_CFLAGS, which unlike the wrapper set omits -fno-strict-aliasing. Merging
# the two would build an oracle DArc does not ship; PPMd is the standing proof
# that such a difference can change a codec's bytes.
SDK_CFLAGS="$(darc_lzma_sdk_cflags)" || exit 1

W="${TMPDIR:-/tmp}/lzma2.$$"; mkdir -p "$W" || exit 1
trap 'rm -rf "$W"' EXIT
mkdir -p "$W/content" "$W/sc" "$W/sr" "$W/tc" "$W/tr" "$W/trs" "$W/stream" "$W/res"

# A corrupt stream can be productive rather than merely wrong: a mutated copy
# length makes the decoder emit far more than the input ever held. Cap it, and
# report the cap distinctly (the driver's `capped=` field) so a capped run is
# never mistaken for a verdict.
export LZMA2_OUT_CAP=134217728

# ---- build the C driver -------------------------------------------------------
# File list taken from Compression/LZMA/makefile, not guessed. LZMA2 needs six
# sources the LZMA harnesses do not: Lzma2Enc.c, Lzma2Dec.c, MtCoder.c, MtDec.c,
# Threads.c and LzFindMt.c.
#
# NOTE the DEFS line: no -DZ7_ST, unlike lzma-gap-check.sh and
# lzma-decode-check.sh. DArc's makefile passes no Z7_ST either -- it builds all
# four of those MT sources -- and for LZMA2 the difference is not cosmetic: with
# Z7_ST defined, LzmaEncProps_Normalize's default numThreads becomes 1 instead of
# 2 (LzmaEnc.c:101-107), which feeds the whole thread arithmetic in
# Lzma2EncProps_Normalize. Build the code DArc ships; pin the thread COUNT
# instead, which lzma2_ref.cpp does via its GetCompressionThreads stub.
#
# The WORKING TREE, not the pinned reference: lzma2_ref.cpp includes the
# working-tree C_LZMA2.cpp by relative path, and this harness's job is to check
# the LZMA2 DArc ships today.
# The C oracle now comes from the PINNED reference, not the working tree: the C
# LZMA/LZMA2 engine has been deleted from the tree it used to be read from. This is
# the same move every other codec's difftest made when its C went, and it is what
# keeps the gate meaningful -- the Rust is still being compared against the C DArc
# shipped, byte for byte, rather than against itself.
SDK="$CREF/Compression/LZMA/7z24"
DEFS="-DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT"
objs=""
for c in LzmaEnc LzmaDec Lzma2Enc Lzma2Dec LzFind LzFindOpt LzFindMt \
         CpuArch Threads MtCoder MtDec 7zStream; do
  # shellcheck disable=SC2086
  clang -c $SDK_CFLAGS -w $DEFS -I"$SDK" -o "$W/$c.o" "$SDK/$c.c" 2>>"$W/cbuild.log" \
    || { echo "compiling SDK $c.c failed" >&2; tail -20 "$W/cbuild.log" >&2; exit 1; }
  objs="$objs $W/$c.o"
done
# shellcheck disable=SC2086
clang++ -std=c++17 $CFLAGS_C -w $DEFS \
  -I"$CREF" -I"$CREF/Compression" -I"$SDK" \
  "$CREF/rust/difftest/lzma2_ref.cpp" "$CREF/Compression/Common.cpp" $objs \
  -lpthread -o "$W/c" 2>>"$W/cbuild.log"
[ -x "$W/c" ] || { echo "building the C LZMA2 driver failed:" >&2
                   tail -25 "$W/cbuild.log" >&2; exit 1; }

CDRV="${LZMA2_C:-$W/c}"
[ -x "$CDRV" ] || { echo "no C LZMA2 driver at $CDRV" >&2; exit 1; }
# The tracer is a pure stream parser, so it is ALWAYS the harness's own build,
# never the overridden one. Otherwise rehearsing a defect by swapping the C
# driver would swap the measuring instrument along with the thing measured, and
# a sabotage that broke both would look clean.
TRACER="$W/c"

# ---- the Rust driver, which may not exist yet ---------------------------------
RS_BIN="${LZMA2_RS_BIN:-lzma2_rs_ref}"
RS="${LZMA2_RS:-}"
if [ -z "$RS" ]; then
  ( cd "$ROOT/rust" && cargo build --release -p darc-lzma --bin "$RS_BIN" ) >/dev/null 2>&1
  RS="$ROOT/rust/target/release/$RS_BIN"
fi
HAVE_RS=0
[ -x "$RS" ] && HAVE_RS=1
# Does it understand `trace`? Optional: the harness traces both sides with its
# own parser regardless, and cross-checks a Rust tracer against it when present.
RS_TRACE=0
if [ "$HAVE_RS" = 1 ]; then
  printf '\x18\x00' | "$RS" 0 0 0 0 0 0 0 0 trace >/dev/null 2>&1 && RS_TRACE=1
fi

# ---- corpus -------------------------------------------------------------------
# Everything the harness knows -- which input, which parameters, and WHAT THE
# ANSWER MUST BE -- is decided here and written to two manifests. Expectations
# are derived from the C source, not recorded from a C run, so the C is measured
# against the contract rather than against itself.
#
# enc manifest fields, space separated:
#   name dict lc lp pb fb mc mf algo expect content tags
#     expect   accept | reject
# dec manifest fields:
#   name stream chunk expect expfile cons tags
#     expect   accept | reject | any
#     expfile  expected plaintext, or -
#     cons     -, a number, or same:<case-name>
python3 - "$W" "$W/c" "${LZMA2_QUICK:-0}" <<'PY' || { echo "corpus generation failed" >&2; exit 1; }
import hashlib, os, subprocess, sys

W, DRV, QUICK = sys.argv[1], sys.argv[2], sys.argv[3] == "1"
C, S = os.path.join(W, "content"), os.path.join(W, "stream")
enc_man, dec_man = [], []

def rnd(seed, n):
    """Genuinely incompressible, and DETERMINISTIC -- a SHA-256 counter stream
    rather than os.urandom, so the corpus is the same on every run and a
    divergence can be reproduced."""
    o = bytearray(); i = 0
    while len(o) < n:
        o += hashlib.sha256(b"%d:%d" % (seed, i)).digest(); i += 1
    return bytes(o[:n])

def prng(seed, n):
    """An LCG's low bits are strongly structured, so this is 'incompressible
    enough to be copied' without being random -- a different shape from rnd()."""
    s = seed & 0xffffffff; o = bytearray()
    while len(o) < n:
        s = (s * 1103515245 + 12345) & 0xffffffff
        o += s.to_bytes(4, "little")
    return bytes(o[:n])

def text(n):
    b = b"the quick brown fox jumps over the lazy dog. "
    return (b * (n // len(b) + 1))[:n]

def mixed(n):
    """Alternating compressible and incompressible megabytes. This is the shape
    that makes useCopyBlock (Lzma2Enc.c:154) flip repeatedly inside ONE stream,
    which no single-shape input does."""
    o = bytearray(); i = 0
    while len(o) < n:
        o += bytes(1 << 20) if i % 2 == 0 else rnd(500 + i, 1 << 20)
        i += 1
    return bytes(o[:n])

def content(name, data):
    p = os.path.join(C, name)
    with open(p, "wb") as f: f.write(data)
    return p

def enc_case(name, params, expect, cpath, tags):
    enc_man.append(" ".join(str(x) for x in
        [name] + list(params) + [expect, cpath, tags]))

def dec_case(name, stream, chunk, expect, expfile="-", cons="-", tags=""):
    dec_man.append(" ".join(str(x) for x in
        [name, stream, chunk, expect, expfile, cons, tags]))

# ── sizes ────────────────────────────────────────────────────────────────────
# LZMA2_UNPACK_SIZE_MAX is 2 MiB (Lzma2Enc.c:29), and it is the cap that ends a
# subblock -- but the EFFECTIVE boundary is lower and worth straddling too:
# LzmaEnc.c:2666 stops at `processed + kNumOpts + 300 >= maxUnpackSize`, i.e.
# 2097152 - 2348 = 2094804. Both are covered.
M2 = 2 << 20
EFF = M2 - 2048 - 300
SIZES = [0, 1, 2, 65535, 65536, 65537,
         EFF - 1, EFF, EFF + 1,
         M2 - 1, M2, M2 + 1,
         2 * M2, 3 * M2 + 12345]
if QUICK:
    # 2*M2 stays even in quick mode: it is the only size that carries the
    # size_mult2m coverage category, and dropping it makes the run fail the
    # coverage gate rather than merely run faster.
    SIZES = [0, 1, 65536, M2 - 1, M2, M2 + 1, 2 * M2]

def size_tags(n):
    t = []
    if n <= 2: t.append("size_tiny")
    if n in (EFF - 1, EFF, EFF + 1, M2 - 1, M2, M2 + 1): t.append("size_edge2m")
    if n in (2 * M2, 3 * M2 + 12345): t.append("size_mult2m")
    return t

# DArc's OWN defaults (C_LZMA2.cpp:201-211). matchFinder is kHT4 -- a five-byte
# hash CHAIN, not BT4 -- because no preset in Compression.hs names one, so every
# -mlzma2 archive DArc has written used the configuration a hand-written case
# would never pick.
DFLT = (64 << 20, 3, 0, 2, 32, 0, 4, 1)

# ── content shapes at every size ─────────────────────────────────────────────
# `rnd` and `prng` are the ones that trigger the uncompressed-chunk path;
# `mixed` is the one that makes the decision flip repeatedly within a stream.
for n in SIZES:
    for shape, fn, tag in (("text", text, "compressible"),
                           ("rnd",  lambda k, s=n: rnd(7, s), "incompressible"),
                           ("prng", lambda k, s=n: prng(3, s), "incompressible"),
                           ("mixed", lambda k, s=n: mixed(s), "mixed")):
        if shape == "mixed" and n < (1 << 21):
            continue                    # needs at least two megabyte halves
        nm = f"sz_{shape}_{n}"
        cp = content(nm, fn(n))
        enc_case(nm, DFLT, "accept", cp, ",".join([tag] + size_tags(n)) or "shape")

# One case per shape through a bintree finder as well, so the size sweep is not
# entirely one match finder.
for n in (65536, M2, M2 + 1):
    cp = os.path.join(C, f"sz_rnd_{n}")
    enc_case(f"szbt_rnd_{n}", (1 << 20, 3, 0, 2, 32, 0, 2, 1), "accept", cp,
             "incompressible,size_edge2m")

# ── a compact content set for the parameter sweep ────────────────────────────
PSET = [
    ("p_text",  text(300000)),
    ("p_rnd",   rnd(9, 300000)),
    ("p_mix",   text(120000) + rnd(11, 120000) + text(60000)),
]
for nm, d in PSET: content(nm, d)
PPATHS = [os.path.join(C, nm) for nm, _ in PSET]

def sweep(prefix, params, tags):
    for cp in PPATHS:
        enc_case(f"{prefix}_{os.path.basename(cp)}", params, "accept", cp, tags)

# match finder x parser. All five ids and both parsers, at DArc's own dictionary.
for mf in range(5):
    for algo in (0, 1):
        sweep(f"mf{mf}a{algo}", (64 << 20, 3, 0, 2, 32, 0, mf, algo),
              f"mf{mf},algo{algo},mfsweep")

# dictSize. Anything below 2 MiB is the ONLY thing that exercises the
# keepWindowSize widening at LzmaEnc.c:2729 -- `beforeSize = keepWindowSize -
# dictSize` with keepWindowSize = LZMA2_KEEP_WINDOW_SIZE = 2 MiB -- so half of
# these are deliberately small.
for ds in (4096, 65536, 262144, 1 << 20, (1 << 21) - 1, 1 << 21, 8 << 20, 64 << 20):
    tag = "dict_small" if ds < (1 << 21) else "dict_big"
    sweep(f"d{ds}", (ds, 3, 0, 2, 32, 0, 4, 1), tag)

for fb in (5, 32, 64, 273):
    sweep(f"fb{fb}", (1 << 20, 3, 0, 2, fb, 0, 2, 1), "fb")
for mc in (0, 8, 64):
    sweep(f"mc{mc}", (1 << 20, 3, 0, 2, 32, mc, 2, 1), "mc")

# lc/lp/pb across the lc + lp <= 4 region Lzma2Enc_SetProps enforces
# (Lzma2Enc.c:470-471), and then the region it must refuse.
for lc, lp, pb in ((0,0,0), (0,4,0), (4,0,0), (2,2,0), (3,0,2),
                   (1,3,4), (0,2,1), (3,1,2), (2,1,3)):
    sweep(f"lclp{lc}{lp}{pb}", (1 << 20, lc, lp, pb, 32, 0, 2, 1), "lclp")
for lc, lp in ((4,1), (3,2), (0,5), (5,0), (9,0), (4,4)):
    enc_case(f"reject_lclp_{lc}_{lp}", (1 << 20, lc, lp, 2, 32, 0, 2, 1),
             "reject", PPATHS[0], "lclp_reject")

# ── decode corpus ────────────────────────────────────────────────────────────
def run(args, data):
    p = subprocess.run([DRV] + [str(a) for a in args], input=data,
                       capture_output=True)
    return p.returncode, p.stdout

def encode(name, data, params=DFLT):
    rc, out = run(list(params) + ["enc"], data)
    if rc != 0:
        sys.exit(f"corpus: encoding {name} failed")
    p = os.path.join(S, name)
    with open(p, "wb") as f: f.write(out)
    return p, out

def stream_bytes(props, chunks, term=True, tail=b""):
    o = bytearray([props])
    for c in chunks: o += c
    if term: o.append(0)
    return bytes(o) + tail

def copy_chunk(data, reset):
    n = len(data) - 1
    return bytes([1 if reset else 2, (n >> 8) & 0xFF, n & 0xFF]) + data

# A victim with both chunk kinds in it: text first (so the block opens with an
# LZMA chunk, mode 3), then incompressible (so copy chunks follow).
vic_plain = text(150000) + rnd(21, 150000) + text(50000)
vic_c = content("victim", vic_plain)
vic_s, victim = encode("victim", vic_plain)
dec_case("victim_ok", vic_s, 1, "accept", vic_c, str(len(victim)), "roundtrip")

# An all-copy victim, so the copy-chunk decode path has a base case of its own.
cp_plain = rnd(22, 120000)
cp_c = content("copyvictim", cp_plain)
cp_s, copyvic = encode("copyvictim", cp_plain)
dec_case("copyvictim_ok", cp_s, 1, "accept", cp_c, str(len(copyvic)), "roundtrip")

# ---- hand-built copy streams -------------------------------------------------
# The decoder's 64 KiB copy chunk, which lzma2_compress provably never emits (see
# the header). Anything else that writes LZMA2 does, so a Rust decoder has to
# handle it, and nothing in a round-trip corpus would ever show it.
for nm, payloads in (
    ("hand_copy1",        [rnd(31, 1)]),
    ("hand_copy64k",      [rnd(32, 65536)]),
    ("hand_copy64k_x3",   [rnd(33, 65536), rnd(34, 65536), rnd(35, 7)]),
    ("hand_copy_max_min", [rnd(36, 65536), rnd(37, 1)]),
    ("hand_copy_many",    [rnd(38 + i, 4096) for i in range(20)]),
):
    plain = b"".join(payloads)
    cpath = content(nm, plain)
    st = stream_bytes(0x18, [copy_chunk(p, i == 0) for i, p in enumerate(payloads)])
    p = os.path.join(S, nm)
    with open(p, "wb") as f: f.write(st)
    dec_case(nm, p, 1, "accept", cpath, str(len(st)), "dec_copy64k")

# Junk after the 0x00 terminator must be invisible: same plaintext, and the SAME
# consumed count. chunk=1 throughout, because lzma2_decompress reads through a
# 64 KiB buffer (C_LZMA2.cpp:127) -- feeding one byte at a time is what makes the
# terminator's position observable from outside.
junk_plain = rnd(41, 1000)
junk_c = content("junkbase", junk_plain)
junk_st = stream_bytes(0x18, [copy_chunk(junk_plain, True)])
jp = os.path.join(S, "junk_base")
with open(jp, "wb") as f: f.write(junk_st)
dec_case("junk_base", jp, 1, "accept", junk_c, str(len(junk_st)), "dec_junk")
for j in (1, 2, 20, 65536):
    p = os.path.join(S, f"junk_{j}")
    # 0xA5, not zeros: a zero byte is a valid LZMA2 terminator, so junk made of
    # zeros would be consumed as structure and the test would pass for the wrong
    # reason.
    with open(p, "wb") as f: f.write(junk_st + b"\xa5" * j)
    dec_case(f"junk_{j}", p, 1, "accept", junk_c, "same:junk_base", "dec_junk")
# The same for a real encoder stream, which ends inside an LZMA chunk rather than
# inside a copy chunk.
dec_case("junk_vic_base", vic_s, 1, "accept", vic_c, str(len(victim)), "dec_junk")
for j in (1, 20, 65536):
    p = os.path.join(S, f"junk_vic_{j}")
    with open(p, "wb") as f: f.write(victim + b"\xa5" * j)
    dec_case(f"junk_vic_{j}", p, 1, "accept", vic_c, "same:junk_vic_base", "dec_junk")

# ---- rejection, one case per documented error edge --------------------------

# (1) a stream opening with control 2 (Lzma2Dec.c:110-114). needInitLevel starts
# at 0xE0, and an uncompressed control that is not 1 with needInitLevel == 0xE0
# is an immediate error -- the first chunk MUST reset the dictionary.
p = os.path.join(S, "rej_ctl2")
with open(p, "wb") as f: f.write(stream_bytes(0x18, [copy_chunk(rnd(51, 100), False)]))
dec_case("rej_ctl2", p, 1, "reject", tags="reject_ctl2")
# ...and control 3, which trips the `b > 2` half of the same test.
p = os.path.join(S, "rej_ctl3")
with open(p, "wb") as f:
    ch = bytearray(copy_chunk(rnd(52, 100), False)); ch[0] = 3
    f.write(stream_bytes(0x18, [bytes(ch)]))
dec_case("rej_ctl3", p, 1, "reject", tags="reject_ctl2")

# (2) an LZMA chunk whose control is below needInitLevel (Lzma2Dec.c:117-118).
# The first chunk of a stream needs control >= 0xE0; 0x80/0xA0/0xC0 are the three
# LZMA modes that are not "reset dic", and all three must be refused HERE while
# being perfectly legal later in a stream.
lz = victim[1:]          # the victim's first chunk is an LZMA chunk, control 0xE?
for ctl in (0x80, 0xA0, 0xC0):
    m = bytearray(victim)
    m[1] = (m[1] & 0x1F) | ctl
    p = os.path.join(S, f"rej_needinit_{ctl:02x}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_needinit_{ctl:02x}", p, 1, "reject", tags="reject_needinit")

# (3) prop >= 225 in a chunk's prop byte (Lzma2Dec.c:148-149). The victim's first
# chunk has control 0xE? so it carries a prop byte, at offset 6 (props, control,
# u1, u0, p1, p0, prop).
assert victim[1] & 0x40, "victim's first chunk must carry a prop byte"
for v in (225, 226, 240, 255):
    m = bytearray(victim); m[6] = v
    p = os.path.join(S, f"rej_prop{v}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_prop{v}", p, 1, "reject", tags="reject_prop225")
# The neighbour that must NOT be refused by that check: 224 decomposes to lc=8,
# which the lc+lp test below then refuses -- so it is still a rejection, but for
# the other reason. 93 is the victim's own lc3 lp0 pb2 and must be accepted.
dec_case("prop93_ok", vic_s, 1, "accept", vic_c, str(len(victim)), "reject_prop225")

# (4) a prop byte with lc + lp > 4 (Lzma2Dec.c:154-155). encode is
# (pb*5 + lp)*9 + lc, so these are chosen by inverse decomposition and each is
# below 225 -- they must fail the SECOND test, not the first.
for v in (8, 13, 31, 224, 134):
    lc = v % 9; d = v // 9; lp = d % 5
    assert v < 225 and lc + lp > 4, (v, lc, lp)
    m = bytearray(victim); m[6] = v
    p = os.path.join(S, f"rej_lclp{v}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_lclp{v}", p, 1, "reject", tags="reject_lclp")
# And the boundary that must be ACCEPTED as properties: lc + lp == 4 exactly.
for lc, lp, pb in ((4, 0, 0), (0, 4, 0), (2, 2, 1)):
    v = (pb * 5 + lp) * 9 + lc
    m = bytearray(victim); m[6] = v
    p = os.path.join(S, f"prop_lclp4_{lc}{lp}{pb}")
    with open(p, "wb") as f: f.write(bytes(m))
    # Accepted as PROPERTIES; the stream then decodes to nonsense or fails on a
    # later symbol. What must not happen is a refusal of the property byte, and
    # the cross-implementation comparison is what makes `any` worth running.
    dec_case(f"prop_lclp4_{lc}{lp}{pb}", p, 1, "any", tags="reject_lclp")

# (5) the LEADING prop byte > 40 at allocate (Lzma2Dec.c:59-60). This is DArc's
# own framing byte, not the SDK's, so a port that never wrote it would fail here
# rather than silently.
#
# 40 itself is deliberately absent: it means dicSize = 0xFFFFFFFF and makes
# LzmaDec_Allocate ask for 4 GiB. Testing the rejection does not require
# rehearsing an out-of-memory condition on the developer's machine.
for v in (41, 42, 100, 200, 255):
    m = bytearray(victim); m[0] = v
    p = os.path.join(S, f"rej_dicprop{v}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_dicprop{v}", p, 1, "reject", tags="reject_dicprop")
# Small leading props that ARE legal: 0 is a 4 KiB window, which is smaller than
# the stream was written with, so this is accepted as a property and then fails
# on a distance -- a different failure entirely, and one worth comparing across
# implementations.
for v in (0, 1, 18, 24, 40 - 1):
    m = bytearray(victim); m[0] = v
    p = os.path.join(S, f"dicprop{v}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"dicprop{v}", p, 1, "any", tags="reject_dicprop")

# (6) truncation at each header-byte boundary. Six of them for an LZMA-first
# stream (control, unpack hi, unpack lo, pack hi, pack lo, prop) plus the leading
# LZMA2 prop byte before them, and three more for a copy-first stream, whose
# header is shorter. A decoder that reports success on any of these has invented
# the tail.
for n in range(0, 9):
    p = os.path.join(S, f"rej_trunc_lzma_{n}")
    with open(p, "wb") as f: f.write(victim[:n])
    dec_case(f"rej_trunc_lzma_{n}", p, 1, "reject", tags="reject_trunc")
for n in range(0, 6):
    p = os.path.join(S, f"rej_trunc_copy_{n}")
    with open(p, "wb") as f: f.write(copyvic[:n])
    dec_case(f"rej_trunc_copy_{n}", p, 1, "reject", tags="reject_trunc")
# Truncation deeper in, where the header is complete and the PAYLOAD is short --
# the same failure as (7) but reached by removing bytes rather than by lying
# about the size.
for n in (10, 50, len(victim) // 2, len(victim) - 1):
    p = os.path.join(S, f"rej_trunc_body_{n}")
    with open(p, "wb") as f: f.write(victim[:n])
    dec_case(f"rej_trunc_body_{n}", p, 1, "reject", tags="reject_trunc")

# (7) a packSize that overruns the input. The pack field is stored biased by one
# at offsets 4..5 of an LZMA chunk; raising it makes the decoder ask for bytes
# the stream does not contain, and C_LZMA2.cpp:168 turns "needs more input with
# nothing left" into BAD_COMPRESSED_DATA.
for hi, lo, nm in ((0xFF, 0xFF, "max"), (0x80, 0x00, "half"), (0x00, 0xFF, "small")):
    m = bytearray(victim); m[4] = hi; m[5] = lo
    p = os.path.join(S, f"rej_packover_{nm}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_packover_{nm}", p, 1, "reject", tags="reject_packover")
# The copy chunk's equivalent: an unpack size larger than the bytes present.
m = bytearray(copyvic); m[2] = 0xFF; m[3] = 0xFF
p = os.path.join(S, "rej_packover_copy")
with open(p, "wb") as f: f.write(bytes(m))
dec_case("rej_packover_copy", p, 1, "any", tags="reject_packover")

# (8) an LZMA chunk payload whose first byte is not 0x00 (Lzma2Dec.c:414-419 in
# the parse path; the decode path rejects it through the range coder). The
# victim's first chunk payload begins right after its prop byte, at offset 7.
for v in (0x01, 0x80, 0xFF):
    m = bytearray(victim); m[7] = v
    p = os.path.join(S, f"rej_firstbyte_{v:02x}")
    with open(p, "wb") as f: f.write(bytes(m))
    dec_case(f"rej_firstbyte_{v:02x}", p, 1, "reject", tags="reject_firstbyte")

# (9) an empty input, and a stream that is nothing but the terminator.
p = os.path.join(S, "rej_empty")
with open(p, "wb") as f: f.write(b"")
dec_case("rej_empty", p, 1, "reject", tags="reject_trunc")
empty_c = content("empty", b"")
p = os.path.join(S, "only_term")
with open(p, "wb") as f: f.write(bytes([0x18, 0x00]))
dec_case("only_term", p, 1, "accept", empty_c, "2", "roundtrip")

with open(os.path.join(W, "enc-manifest"), "w") as f:
    f.write("\n".join(enc_man) + "\n")
with open(os.path.join(W, "dec-manifest"), "w") as f:
    f.write("\n".join(dec_man) + "\n")
print(f"corpus: {len(enc_man)} encode cases, {len(dec_man)} decode cases",
      file=sys.stderr)
PY

[ -s "$W/enc-manifest" ] || { echo "empty encode manifest" >&2; exit 1; }
[ -s "$W/dec-manifest" ] || { echo "empty decode manifest" >&2; exit 1; }

# ---- runners ------------------------------------------------------------------
# RLIMIT_CPU rather than a timeout command: `timeout` is not on a stock macOS,
# and a wrapper process per run would cost more than the run. A codec that loops
# shows up as exit 152 (SIGXCPU), which classify() below calls a crash -- which
# is what a hang is.
RC=""; CONS=""; PROD=""; CLS=""
run_drv () { # $1 bin, $2 stdin, $3 stdout, $4 cpu-seconds, $5.. argv
  local bin="$1" sin="$2" sout="$3" cpu="$4"; shift 4
  ( ulimit -t "$cpu"; exec "$bin" "$@" ) < "$sin" >| "$sout" 2>| "$W/report"
  local ec=$?
  RC=$(sed -n -E 's/.*DARC_LZMA2 .*rc=(-?[0-9]+).*/\1/p' "$W/report" | head -1)
  CONS=$(sed -n -E 's/.*consumed=([0-9]+).*/\1/p'        "$W/report" | head -1)
  PROD=$(sed -n -E 's/.*produced=([0-9]+).*/\1/p'        "$W/report" | head -1)
  if [ -z "$RC" ]; then CLS=crash; return; fi
  case "$ec" in
    0) CLS=accept ;;
    1) CLS=reject ;;
    *) CLS=crash ;;
  esac
}

# Trace field extraction. The tracer's own report line, not the codec's.
TF=""
tfield () { sed -n -E "s/.*[[:space:]]$1=([0-9]+).*/\1/p" "$W/tstat" | head -1; }
trace_of () { # $1 = stream, $2 = trace output file  -> sets TF_* globals
  ( ulimit -t 60; exec "$TRACER" 0 0 0 0 0 0 0 0 trace ) < "$1" >| "$2" 2>| "$W/tstat"
  TF_PARSED=$(tfield parsed);   TF_TRAIL=$(tfield trailing)
  TF_MODE1=$(tfield mode1);     TF_MODE2=$(tfield mode2)
  TF_MODE3=$(tfield mode3);     TF_INIT=$(tfield init)
  TF_LATE=$(tfield initlate);   TF_RESETS=$(tfield dicresets)
  TF_BAD=$(tfield badfirst);    TF_COPYMAX=$(tfield copymax)
  TF_COPY=$(tfield copy);       TF_LZMA=$(tfield lzma)
  TF_CHUNKS=$(tfield chunks);   TF_UNPACK=$(tfield unpack)
}

# Both helpers always succeed and always print something. `cmp` and `diff` exit
# nonzero precisely when they find what these are asked to describe, and with
# `set -o pipefail` that status propagates -- a `|| echo ?` fallback on the call
# site then fires on every real difference and splices a stray "?" into the
# message. Swallow the status here, once.
first_diff_line () {
  local d; d=$(diff "$1" "$2" 2>/dev/null | head -3 | tr '\n' '|')
  printf '%s' "${d:-?}"; return 0
}
first_diff_byte () {
  # BSD cmp says "differ: char N"; GNU says "byte N". `sed -E`, not BRE:
  # `\(a\|b\)` alternation is a GNU extension that matches nothing under BSD sed.
  local o; o=$(cmp "$1" "$2" 2>/dev/null | sed -n -E 's/.*(char|byte) ([0-9]+).*/\2/p' | head -1)
  printf '%s' "${o:-?}"; return 0
}

declare -a DETAIL=()
note () { [ "${#DETAIL[@]}" -lt 30 ] && DETAIL+=("  $1"); }

# ---- encode phase -------------------------------------------------------------
e_total=0; e_pass=0; e_class=0; e_struct=0; e_rt=0; e_crash=0
r_enc=0; r_bytes=0; r_class=0; r_rc=0; r_trace=0; r_rstrace=0; r_crash=0
x_c_over_rs=0; x_rs_over_c=0; x_runs=0
mfs_seen=""; algos_seen=""; tags_seen=""
saw_copy=0; saw_lzma=0; saw_mode2=0; saw_mode3=0; saw_multi=0; copymax_all=0

while IFS=' ' read -r name d lc lp pb fb mc mf algo expect cpath tags <&3; do
  [ -n "${name:-}" ] || continue
  e_total=$((e_total+1))
  tags_seen="$tags_seen ${tags//,/ }"
  ok=1
  ARGS=("$d" "$lc" "$lp" "$pb" "$fb" "$mc" "$mf" "$algo" enc)

  run_drv "$CDRV" "$cpath" "$W/sc/$name" 300 "${ARGS[@]}"
  c_cls="$CLS"; c_rc="$RC"
  if [ "$c_cls" = crash ]; then
    e_crash=$((e_crash+1)); ok=0; note "CRASH  enc $name (C driver did not report)"
  elif [ "$c_cls" != "$expect" ]; then
    e_class=$((e_class+1)); ok=0
    note "CLASS  enc $name: expected $expect, C gave $c_cls rc=${c_rc:-?}"
  fi

  if [ "$c_cls" = accept ] && [ "$expect" = accept ]; then
    mfs_seen="$mfs_seen $mf"; algos_seen="$algos_seen $algo"
    plainlen=$(wc -c < "$cpath" | tr -d ' ')

    # ---- structural invariants of the C stream ----
    trace_of "$W/sc/$name" "$W/tc/$name"
    if [ "${TF_PARSED:-0}" != 1 ]; then
      e_struct=$((e_struct+1)); ok=0; note "STRUCT $name: the C stream does not parse as LZMA2"
    else
      [ "${TF_TRAIL:-0}" = 0 ] || { e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: $TF_TRAIL byte(s) after the terminator"; }
      [ "${TF_MODE1:-0}" = 0 ] || { e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: chunk mode 1 appeared ${TF_MODE1} time(s) -- Lzma2Enc.c:201 says it cannot"; }
      [ "${TF_LATE:-0}" = 0 ] || { e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: $TF_LATE block-init chunk(s) after the first LZMA chunk"; }
      [ "${TF_BAD:-0}" = 0 ] || { e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: $TF_BAD LZMA chunk payload(s) do not start with 0x00"; }
      [ "${TF_UNPACK:-0}" = "$plainlen" ] || { e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: chunks account for ${TF_UNPACK:-?} bytes, plaintext is $plainlen"; }
      if [ "${TF_COPYMAX:-0}" -ge 65536 ]; then
        e_struct=$((e_struct+1)); ok=0
        note "STRUCT $name: a copy chunk reached ${TF_COPYMAX}B -- the 64 KiB split at Lzma2Enc.c:168 is reachable after all; this harness's dead-code claim is wrong"
      fi
      if [ "$plainlen" -gt 0 ]; then
        [ "${TF_RESETS:-0}" = 1 ] || { e_struct=$((e_struct+1)); ok=0
          note "STRUCT $name: ${TF_RESETS:-?} dictionary resets, expected exactly 1 (SOLID block)"; }
        [ "${TF_INIT:-0}" -le 1 ] || { e_struct=$((e_struct+1)); ok=0
          note "STRUCT $name: ${TF_INIT} block-init chunks, expected at most 1"; }
      fi
      [ "${TF_COPY:-0}" -gt 0 ] && saw_copy=$((saw_copy+1))
      [ "${TF_LZMA:-0}" -gt 0 ] && saw_lzma=$((saw_lzma+1))
      [ "${TF_MODE2:-0}" -gt 0 ] && saw_mode2=$((saw_mode2+1))
      [ "${TF_MODE3:-0}" -gt 0 ] && saw_mode3=$((saw_mode3+1))
      [ "${TF_CHUNKS:-0}" -gt 1 ] && saw_multi=$((saw_multi+1))
      [ "${TF_COPYMAX:-0}" -gt "$copymax_all" ] && copymax_all="${TF_COPYMAX:-0}"
    fi

    # ---- round-trip through the C decoder ----
    # The C-only teeth on the encode side: a stream that is structurally valid
    # and decodes to something other than its input is broken whether or not a
    # second implementation exists to disagree with it.
    run_drv "$CDRV" "$W/sc/$name" "$W/rt.out" 120 0 0 0 0 0 0 0 0 dec
    if [ "$CLS" != accept ] || ! cmp -s "$cpath" "$W/rt.out"; then
      e_rt=$((e_rt+1)); ok=0
      note "RTRIP  $name: the C stream did not decode back (cls=$CLS rc=${RC:-?}, first diff byte $(first_diff_byte "$cpath" "$W/rt.out"))"
    fi
  fi

  # ---- the Rust driver, when there is one ----
  if [ "$HAVE_RS" = 1 ]; then
    r_enc=$((r_enc+1))
    run_drv "$RS" "$cpath" "$W/sr/$name" 300 "${ARGS[@]}"
    if [ "$CLS" = crash ]; then
      r_crash=$((r_crash+1)); ok=0; note "CRASH  enc $name (Rust driver did not report)"
    else
      if [ "$CLS" != "$c_cls" ]; then
        r_class=$((r_class+1)); ok=0
        note "DIVERGE enc $name: C $c_cls rc=${c_rc:-?}, Rust $CLS rc=${RC:-?}"
      else
        [ "${RC:-?}" = "${c_rc:-?}" ] || r_rc=$((r_rc+1))    # recorded, not gated
      fi
      if [ "$CLS" = accept ] && [ "$c_cls" = accept ]; then
        if ! cmp -s "$W/sc/$name" "$W/sr/$name"; then
          r_bytes=$((r_bytes+1)); ok=0
          note "DIVERGE enc $name: streams differ at byte $(first_diff_byte "$W/sc/$name" "$W/sr/$name") (C $(wc -c < "$W/sc/$name" | tr -d ' ')B, Rust $(wc -c < "$W/sr/$name" | tr -d ' ')B)"
        fi
        # Chunk structure, traced by THIS harness's parser on both streams, so a
        # divergence is localised to a chunk triple rather than a byte offset.
        trace_of "$W/sr/$name" "$W/tr/$name"
        if ! cmp -s "$W/tc/$name" "$W/tr/$name"; then
          r_trace=$((r_trace+1)); ok=0
          note "DIVERGE enc $name: chunk trace differs -- $(first_diff_line "$W/tc/$name" "$W/tr/$name")"
        fi
        # And, if the Rust driver traces too, its tracer against ours.
        if [ "$RS_TRACE" = 1 ]; then
          run_drv "$RS" "$W/sr/$name" "$W/trs/$name" 60 0 0 0 0 0 0 0 0 trace
          if ! cmp -s "$W/tr/$name" "$W/trs/$name"; then
            r_rstrace=$((r_rstrace+1)); ok=0
            note "DIVERGE $name: the Rust tracer disagrees with the C tracer on the SAME stream -- $(first_diff_line "$W/tr/$name" "$W/trs/$name")"
          fi
        fi
        # Cross-decode, both ways. Direction two -- the C decoder over a Rust
        # stream -- is the one that decides whether archives written by a ported
        # encoder can be read by everything already deployed.
        x_runs=$((x_runs+1))
        run_drv "$RS" "$W/sc/$name" "$W/x1.out" 120 0 0 0 0 0 0 0 0 dec
        if [ "$CLS" != accept ] || ! cmp -s "$cpath" "$W/x1.out"; then
          x_rs_over_c=$((x_rs_over_c+1)); ok=0
          note "CROSS  $name: the Rust decoder did not reproduce the plaintext from the C stream (cls=$CLS rc=${RC:-?})"
        fi
        run_drv "$CDRV" "$W/sr/$name" "$W/x2.out" 120 0 0 0 0 0 0 0 0 dec
        if [ "$CLS" != accept ] || ! cmp -s "$cpath" "$W/x2.out"; then
          x_c_over_rs=$((x_c_over_rs+1)); ok=0
          note "CROSS  $name: the C decoder did not reproduce the plaintext from the RUST stream (cls=$CLS rc=${RC:-?})"
        fi
      fi
    fi
  fi

  [ "$ok" = 1 ] && e_pass=$((e_pass+1))
done 3< "$W/enc-manifest"

# ---- decode phase -------------------------------------------------------------
d_total=0; d_pass=0; d_class=0; d_out=0; d_cons=0; d_crash=0
dr_total=0; dr_class=0; dr_rc=0; dr_out=0; dr_cons=0; dr_crash=0

while IFS=' ' read -r name stream chunk expect expfile cons tags <&3; do
  [ -n "${name:-}" ] || continue
  d_total=$((d_total+1))
  tags_seen="$tags_seen ${tags//,/ }"
  ok=1

  run_drv "$CDRV" "$stream" "$W/dc.out" 60 0 0 0 0 0 0 0 0 dec "$chunk"
  c_cls="$CLS"; c_rc="$RC"; c_cons="$CONS"
  printf '%s %s %s\n' "${c_rc:-?}" "${c_cons:-?}" "${PROD:-?}" >| "$W/res/$name"

  if [ "$c_cls" = crash ]; then
    d_crash=$((d_crash+1)); ok=0; note "CRASH  dec $name (C driver did not report)"
  fi
  case "$expect" in
    any) : ;;
    *) [ "$c_cls" = "$expect" ] || { d_class=$((d_class+1)); ok=0
         note "CLASS  dec $name: expected $expect, C gave $c_cls rc=${c_rc:-?}"; } ;;
  esac
  if [ "$expfile" != "-" ] && [ "$c_cls" = accept ]; then
    if ! cmp -s "$expfile" "$W/dc.out"; then
      d_out=$((d_out+1)); ok=0
      note "OUTPUT dec $name: plaintext differs at byte $(first_diff_byte "$expfile" "$W/dc.out")"
    fi
  fi
  if [ "$cons" != "-" ]; then
    case "$cons" in
      same:*) want=$(awk '{print $2}' "$W/res/${cons#same:}" 2>/dev/null) ;;
      *)      want="$cons" ;;
    esac
    if [ -n "${want:-}" ] && [ "$want" != "?" ] && [ "${c_cons:-?}" != "$want" ]; then
      d_cons=$((d_cons+1)); ok=0
      note "CONSUM dec $name: expected $want input bytes consumed, C consumed ${c_cons:-?}"
    fi
  fi

  if [ "$HAVE_RS" = 1 ]; then
    dr_total=$((dr_total+1))
    run_drv "$RS" "$stream" "$W/dr.out" 60 0 0 0 0 0 0 0 0 dec "$chunk"
    if [ "$CLS" = crash ]; then
      dr_crash=$((dr_crash+1)); ok=0; note "CRASH  dec $name (Rust driver did not report)"
    else
      if [ "$CLS" != "$c_cls" ]; then
        dr_class=$((dr_class+1)); ok=0
        note "DIVERGE dec $name: C $c_cls rc=${c_rc:-?}, Rust $CLS rc=${RC:-?}"
      else
        # RECORDED, not gated. Every data failure in C_LZMA2.cpp:157-158
        # collapses to -7, so the code distinguishes almost nothing here; a
        # mismatch is worth surfacing but is not on its own a defect.
        [ "${RC:-?}" = "${c_rc:-?}" ] || { dr_rc=$((dr_rc+1))
          note "ERRCODE dec $name: both $CLS, C rc=${c_rc:-?} but Rust rc=${RC:-?} (recorded, not gated)"; }
        if [ "$CLS" = accept ]; then
          cmp -s "$W/dc.out" "$W/dr.out" || { dr_out=$((dr_out+1)); ok=0
            note "DIVERGE dec $name: plaintext differs at byte $(first_diff_byte "$W/dc.out" "$W/dr.out")"; }
          [ "${CONS:-?}" = "${c_cons:-?}" ] || { dr_cons=$((dr_cons+1)); ok=0
            note "DIVERGE dec $name: consumed C=${c_cons:-?} Rust=${CONS:-?}"; }
        fi
      fi
    fi
  fi

  [ "$ok" = 1 ] && d_pass=$((d_pass+1))
done 3< "$W/dec-manifest"

# ---- coverage -----------------------------------------------------------------
# Gated for the same reason lzma-gap-check.sh gates its match-finder count: a
# sweep that silently stops covering an axis reports a clean run over the cases
# that were never hard. The copy-chunk category is the sharp one here -- a corpus
# that stopped producing incompressible content would exercise none of
# useCopyBlock and would still look perfect.
count_tag () { printf '%s\n' $tags_seen | grep -cx "$1"; }
REQUIRED="compressible incompressible mixed size_tiny size_edge2m size_mult2m
mfsweep mf0 mf1 mf2 mf3 mf4 algo0 algo1 dict_small dict_big fb mc lclp
lclp_reject roundtrip dec_copy64k dec_junk reject_ctl2 reject_needinit
reject_prop225 reject_lclp reject_dicprop reject_trunc reject_packover
reject_firstbyte"
missing=""
for t in $REQUIRED; do
  n=$(count_tag "$t")
  [ "${n:-0}" -gt 0 ] || missing="$missing $t"
done
n_mf=$(printf '%s\n' $mfs_seen | sort -u | grep -c .)
n_algo=$(printf '%s\n' $algos_seen | sort -u | grep -c .)

# Derived coverage: measured from the traces, not from tags. A tag says a case
# was ASKED for; these say the case actually produced the chunk kind it was
# supposed to produce.
derived_missing=""
[ "$saw_copy"  -gt 0 ] || derived_missing="$derived_missing copy-chunks"
[ "$saw_lzma"  -gt 0 ] || derived_missing="$derived_missing lzma-chunks"
[ "$saw_mode2" -gt 0 ] || derived_missing="$derived_missing chunk-mode-2"
[ "$saw_mode3" -gt 0 ] || derived_missing="$derived_missing chunk-mode-3"
[ "$saw_multi" -gt 0 ] || derived_missing="$derived_missing multi-chunk-streams"

# ---- report -------------------------------------------------------------------
echo "lzma2: $e_total encode cases, $d_total decode cases"
echo "  match finders compared                $n_mf/5"
echo "  parsers compared                      $n_algo/2"
for t in $REQUIRED; do printf '  %-22s %s\n' "$t" "$(count_tag "$t")"; done
echo "  streams with copy chunks              $saw_copy"
echo "  streams with lzma chunks              $saw_lzma"
echo "  streams whose init chunk is mode 2    $saw_mode2"
echo "  streams whose init chunk is mode 3    $saw_mode3"
echo "  streams with more than one chunk      $saw_multi"
echo "  largest copy chunk seen               ${copymax_all}B (64 KiB split unreachable; see header)"
echo "C encoder:"
echo "  cases fully satisfying expectations   $e_pass"
echo "  accept/reject wrong                   $e_class"
echo "  structural invariant violated         $e_struct"
echo "  did not round-trip                    $e_rt"
echo "  crashes / hangs                       $e_crash"
echo "C decoder:"
echo "  cases fully satisfying expectations   $d_pass"
echo "  accept/reject wrong                   $d_class"
echo "  plaintext wrong                       $d_out"
echo "  input-consumed count wrong            $d_cons"
echo "  crashes / hangs                       $d_crash"
if [ "$HAVE_RS" = 1 ]; then
  echo "Rust ($RS), tracer: $([ "$RS_TRACE" = 1 ] && echo present || echo absent):"
  echo "  encode comparisons                    $r_enc"
  echo "  encode accept/reject disagreements    $r_class"
  echo "  encode stream byte divergences        $r_bytes"
  echo "  encode chunk-trace divergences        $r_trace"
  echo "  Rust tracer vs C tracer               $r_rstrace"
  echo "  encode rc differed (recorded)         $r_rc"
  echo "  cross-decode runs                     $x_runs"
  echo "  Rust decoder over C streams, failed   $x_rs_over_c"
  echo "  C decoder over Rust streams, failed   $x_c_over_rs"
  echo "  decode comparisons                    $dr_total"
  echo "  decode accept/reject disagreements    $dr_class"
  echo "  decode plaintext disagreements        $dr_out"
  echo "  decode consumed disagreements         $dr_cons"
  echo "  decode rc differed (recorded)         $dr_rc"
  echo "  crashes / hangs                       $((r_crash + dr_crash))"
else
  echo "Rust: ABSENT -- nothing was built at $RS"
  echo "  C-ONLY MODE. Every expectation above was checked against the C alone;"
  echo "  no cross-implementation comparison ran. Set LZMA2_REQUIRE_RUST=1 to"
  echo "  make this a failure once the Rust driver lands."
fi
if [ "${#DETAIL[@]}" -gt 0 ]; then
  echo "findings:"
  printf '%s\n' "${DETAIL[@]}"
fi

rc=0
[ "$e_total" -gt 0 ]  || { echo "no encode case was measured" >&2; rc=1; }
[ "$d_total" -gt 0 ]  || { echo "no decode case was measured" >&2; rc=1; }
[ -z "$missing" ]     || { echo "corpus covered no case for:$missing" >&2; rc=1; }
[ -z "$derived_missing" ] || { echo "no stream actually exercised:$derived_missing" >&2; rc=1; }
[ "$n_mf" -eq 5 ]     || { echo "only $n_mf/5 match finders were encoded with" >&2; rc=1; }
[ "$n_algo" -eq 2 ]   || { echo "only $n_algo/2 parsers were encoded with" >&2; rc=1; }
[ "$e_crash" -eq 0 ]  || { echo "$e_crash C encoder crash(es) or hang(s)" >&2; rc=1; }
[ "$d_crash" -eq 0 ]  || { echo "$d_crash C decoder crash(es) or hang(s)" >&2; rc=1; }
[ "$e_class" -eq 0 ]  || { echo "$e_class encode case(s) accepted what must be rejected, or the reverse" >&2; rc=1; }
[ "$e_struct" -eq 0 ] || { echo "$e_struct structural invariant violation(s) in C streams" >&2; rc=1; }
[ "$e_rt"    -eq 0 ]  || { echo "$e_rt C stream(s) did not decode back to their input" >&2; rc=1; }
[ "$d_class" -eq 0 ]  || { echo "$d_class decode case(s) accepted what must be rejected, or the reverse" >&2; rc=1; }
[ "$d_out"   -eq 0 ]  || { echo "$d_out decode case(s) produced the wrong plaintext" >&2; rc=1; }
[ "$d_cons"  -eq 0 ]  || { echo "$d_cons decode case(s) consumed the wrong number of input bytes" >&2; rc=1; }
if [ "$HAVE_RS" = 1 ]; then
  [ "$r_enc" -gt 0 ]      || { echo "the Rust driver exists but was never run" >&2; rc=1; }
  [ "$x_runs" -gt 0 ]     || { echo "no cross-decode ran: neither direction is verified" >&2; rc=1; }
  [ "$r_crash" -eq 0 ]    || { echo "$r_crash Rust encode crash(es) or hang(s)" >&2; rc=1; }
  [ "$dr_crash" -eq 0 ]   || { echo "$dr_crash Rust decode crash(es) or hang(s)" >&2; rc=1; }
  [ "$r_class" -eq 0 ]    || { echo "$r_class encode accept/reject disagreement(s) with the C" >&2; rc=1; }
  [ "$r_bytes" -eq 0 ]    || { echo "$r_bytes stream(s) diverged from the C byte for byte" >&2; rc=1; }
  [ "$r_trace" -eq 0 ]    || { echo "$r_trace stream(s) diverged in chunk structure" >&2; rc=1; }
  [ "$r_rstrace" -eq 0 ]  || { echo "$r_rstrace stream(s) where the two tracers disagree" >&2; rc=1; }
  [ "$x_rs_over_c" -eq 0 ] || { echo "$x_rs_over_c C stream(s) the Rust decoder could not read" >&2; rc=1; }
  [ "$x_c_over_rs" -eq 0 ] || { echo "$x_c_over_rs Rust stream(s) the C decoder could not read -- archives written by this port would be unreadable" >&2; rc=1; }
  [ "$dr_class" -eq 0 ]   || { echo "$dr_class decode accept/reject disagreement(s) with the C" >&2; rc=1; }
  [ "$dr_out" -eq 0 ]     || { echo "$dr_out decode plaintext disagreement(s) with the C" >&2; rc=1; }
  [ "$dr_cons" -eq 0 ]    || { echo "$dr_cons decode consumed-count disagreement(s) with the C" >&2; rc=1; }
elif [ "${LZMA2_REQUIRE_RUST:-0}" = 1 ]; then
  echo "LZMA2_REQUIRE_RUST=1 but no Rust LZMA2 driver was found at $RS" >&2
  rc=1
fi
exit $rc
