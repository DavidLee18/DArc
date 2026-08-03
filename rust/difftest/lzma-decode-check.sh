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
# ships today, and lzma_dec_ref.cpp includes the PINNED C_LZMA.cpp by
# relative path anyway.
# The C oracle now comes from the PINNED reference, not the working tree: the C
# LZMA/LZMA2 engine has been deleted from the tree it used to be read from. This is
# the same move every other codec's difftest made when its C went, and it is what
# keeps the gate meaningful -- the Rust is still being compared against the C DArc
# shipped, byte for byte, rather than against itself.
SDK="$CREF/Compression/LZMA/7z24"
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
    -I"$CREF" -I"$CREF/Compression" -I"$SDK" \
    "$CREF/rust/difftest/$1" "$CREF/Compression/Common.cpp" $objs \
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
# The case builder is Rust: it writes the content files, runs the encoder over
# them, synthesises the hand-built and mutated streams, and emits the manifest.
# Accepted on a byte comparison against the python3 heredoc that stood here --
# every content file, every stream file and the manifest, in full mode (3267
# cases, 324 content, 2983 streams) and again under LZMA_DEC_QUICK=1.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin lzma-dec-cases ) || exit 1
"$ROOT/rust/target/release/lzma-dec-cases" "$W" "$ENC" "${LZMA_DEC_QUICK:-0}" \
  || { echo "corpus generation failed" >&2; exit 1; }

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
