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
# The case builder is Rust: it writes the content files, synthesises the
# hand-built and mutated streams, and emits both manifests. Accepted on a byte
# comparison against the python3 heredoc that stood here -- every content file,
# every stream file, and both manifests, in full and quick mode.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin lzma2-cases ) || exit 1
"$ROOT/rust/target/release/lzma2-cases" "$W" "$W/c" "${LZMA2_QUICK:-0}" \
  || { echo "corpus generation failed" >&2; exit 1; }

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
