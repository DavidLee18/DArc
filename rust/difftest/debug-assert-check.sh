#!/usr/bin/env bash
# Run the codecs with OVERFLOW CHECKS enabled, which no other harness does.
#
# ## Why this exists
#
# It was written for `debug_assert!`. The workspace carried 22 of them, every
# other harness builds `--release` where `debug_assert!` compiles to nothing, and
# so all 22 were dead in the only configuration ever tested. That was not
# hypothetical: the Tornado presets 7-11 divergence fixed alongside this file
# was precisely a guard firing into the void. `MatchFinder`'s default
# `update_hash1` was
#
#     fn update_hash1(&mut self, buf: &[u8], p: usize) {
#         let _ = (buf, p);
#         debug_assert!(false, "update_hash1 called on a finder that does not define it");
#     }
#
# `Hash3` defined `update_hash1` only inherently, so `CombineMF` -- which holds
# its auxiliary finder as `Box<dyn MatchFinder>` -- reached that default. In
# release it silently did nothing and the encoder diverged from the C thousands
# of positions later.
#
# **There are no `debug_assert!`s left.** Every one became an `assert!`, so the
# assertions now fire in the builds that ship and every other harness exercises
# them. What is still debug-only, and still tested nowhere else, is
# `-C overflow-checks`: an arithmetic wrap that release computes silently panics
# here.
#
# ## Why overflow checks are the point now
#
# Clippy can prove 2249 `as` casts in this workspace MIGHT truncate, 1462 might
# lose a sign, and 785 might wrap. Most are lossless by construction -- masked or
# shifted first, `(u & 0xFF) as u8` -- and clippy simply cannot carry the bound.
# A few genuinely discard bits, and those are safe only because the DECODER
# discards the same bits: `bsc/qlfc_enc.rs`'s `rank_history` feeds the
# probability model, not the payload, so both sides stay in lockstep and a wrap
# costs ratio rather than correctness.
#
# "Safe only because both sides agree" is a much weaker guarantee than "safe",
# and this codebase has already paid for the difference. `bsc/qlfc_enc.rs:55-58`:
#
#     // `int nSymbols` in the C, not a byte. It reaches 256 on a block using the
#     // whole alphabet, and only the ASSIGNMENT to `rank` truncates. Typed as u8
#     // here it overflowed -- caught by the differential harness on the
#     // full-alphabet input, and by this crate building with overflow-checks on.
#
# That is this harness's job, and it is why the scope below is no longer Tornado
# alone: the four densest arithmetic files in the crate are `bsc/qlfc_enc.rs`
# (254 flagged casts), `grzip/bwt.rs` (234), `bsc/qlfc.rs` (218) and
# `tornado/matchfinder.rs` (201), followed by `lz4hc.rs` (130).
#
# ## What this checks, and what it does NOT
#
# This is not a differential test. It does not compare Rust output against the C
# -- the other harnesses do that, and doing it here would only duplicate them
# more slowly. It drives the Rust codec through a debug build and fails if a
# panic reaches stderr. The signal is a panic, not a byte mismatch.
#
# **The exit status cannot be that signal.** Every codec entry point goes
# through `ffi::guard`, whose `catch_unwind` turns a panic into
# FREEARC_ERRCODE_GENERAL -- which is indistinguishable from a codec legitimately
# declining an input. Rust's panic hook still writes to stderr before the
# unwind is caught, so stderr is the only thing that separates the two. `probe`
# below greps it on EVERY invocation, whatever the exit status, and a run that
# only checked exit codes would report a clean sweep while every block panicked.
#
# Declines are counted and printed per codec for the same reason: a section that
# declines every input has tested nothing, and would otherwise read as a pass.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
W="${TMPDIR:-/tmp}/debug-assert.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# The point of the whole file: NO --release. Overflow checks on.
#
# Note this writes target/debug, a different directory from the release
# staticlib every other harness uses, so running this cannot disturb them and
# they cannot disturb it -- unlike the release-profile feature clash between
# 4x4-check.sh and the tornado harnesses.
( cd "$ROOT/rust" && cargo build -p darc-codecs ) >/dev/null 2>&1 \
  || { echo "cargo debug build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/debug/libdarc_codecs.a"
[ -f "$LIB" ] || { echo "no debug staticlib at $LIB" >&2; exit 1; }

# Prove the build really has overflow checks compiled in. Without this the
# harness would pass vacuously on a release artifact, which is the exact failure
# mode it exists to prevent.
#
# grep -c, not grep -q: `set -o pipefail` is on and `grep -q` exits at the first
# match, SIGPIPEing `strings` and failing the pipeline even on success. That bug
# was fixed in 4x4-check.sh earlier the same day and then written again here.
#
# The probe is rustc's own overflow panic message, which exists ONLY when
# `-C overflow-checks` is on. It deliberately is not an assertion message: every
# assertion in this crate is now unconditional, so an assertion string is present
# in a release artifact too and would prove nothing about the profile this file
# needs.
checks=$(strings -a "$LIB" 2>/dev/null | grep -c 'attempt to .* with overflow')
[ "${checks:-0}" -ge 1 ] || {
  echo "the debug staticlib carries no overflow-check panic messages -- overflow" >&2
  echo "checks are compiled out, so this run would prove nothing." >&2
  exit 1; }

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1
CORPUSGEN="$ROOT/rust/target/release/corpusgen"

fired=0; ran=0; declined=0

# One Rust-side invocation.
#
# Returns 0 clean, 1 declined (non-zero exit, no panic), 2 panicked. See the
# header: the exit status alone cannot tell 1 from 2, so stderr is checked
# first and unconditionally.
probe() {   # probe <label> <outfile> <cmd...>
  local label="$1" out="$2"; shift 2
  rm -f "$W/err"
  "$@" >| "$out" 2>| "$W/err"; local rc=$?
  if grep -qi 'panicked\|assertion failed\|attempt to' "$W/err" 2>/dev/null; then
    echo "  PANIC: $label"
    sed 's/^/     /' "$W/err" | head -6
    fired=$((fired + 1)); return 2
  fi
  if [ "$rc" -ne 0 ]; then declined=$((declined + 1)); return 1; fi
  ran=$((ran + 1)); return 0
}

# Trailing arguments land after the sources: GNU ld resolves an archive against
# only the objects it has already seen, so a library placed first is silently
# dropped. Every driver here is built -DUSE_RUST against the DEBUG staticlib.
build() {  # build <out> <cflags> <source...>
  local out="$1" flags="$2"; shift 2
  # shellcheck disable=SC2086  # both flag lists are word lists on purpose
  clang++ -std=c++17 $flags -w -DUSE_RUST \
    -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" "$@" "$LIB" -o "$out"
}

# ---------------------------------------------------------------------------
# Tornado -- four match finders and two entropy back-ends, a different
# combination per preset.
# ---------------------------------------------------------------------------
CF_TOR="$(darc_codec_cflags Tornado)" || exit 1
build "$W/tor" "$CF_TOR" \
  "$CREF/rust/difftest/tornado_ref.cpp" "$CREF/rust/difftest/tornado_ccodec.cpp" \
  "$CREF/Compression/Common.cpp" || { echo "tornado driver build failed" >&2; exit 1; }

"$CORPUSGEN" debug-assert "$W/tor-in"

# Every preset, both `all_at_once` settings, both directions. `notables` is
# swept on the presets whose data-table detector is reachable, matching
# tornado-encode-check.sh.
t0=$ran
for case in "0 0" "1 0" "2 0" "3 0" "3 1" "4 0" "4 1" "5 0" "5 1" \
            "6 0" "6 1" "7 0" "8 0" "9 0" "10 0" "11 0"; do
  set -- $case; preset=$1; notables=$2
  for aao in 0 1; do
    for f in "$W"/tor-in/*; do
      bn=$(basename "$f")
      # Tornado declines nothing: here a non-zero exit IS a failure, unlike the
      # codecs below which refuse combinations by design.
      probe "tornado encode preset=$preset notables=$notables aao=$aao input=$bn" \
            "$W/s" "$W/tor" c "$preset" "$notables" "$aao" < "$f"
      case $? in
        2) continue ;;
        1) echo "  FAILED (non-zero exit, no panic): tornado encode preset=$preset input=$bn"
           fired=$((fired + 1)); continue ;;
        *) : ;;
      esac
      # Preset 0 is STORING, and its stream is deliberately undecodable: the
      # C's dispatch builds an LZ77_ByteCoder while still writing 0 as the
      # encoding method (Tornado.cpp:331-333), and the C's own decoder switch
      # rejects STORING with BAD_COMPRESSED_DATA (:522). The port reproduces
      # that faithfully, so round-tripping it reports 20 spurious -7 failures.
      # Its ENCODE leg still runs above, which is where its assertions are.
      [ "$preset" = 0 ] && continue
      probe "tornado decode preset=$preset input=$bn" "$W/o" "$W/tor" d < "$W/s"
      case $? in
        2) continue ;;
        1) echo "  FAILED (non-zero exit, no panic): tornado decode preset=$preset input=$bn"
           fired=$((fired + 1)); continue ;;
        *) : ;;
      esac
      # Not a differential check, but a round-trip that loses data would mean
      # the sweep is exercising something other than the codec.
      cmp -s "$f" "$W/o" || {
        echo "  ROUND-TRIP LOST DATA: tornado preset=$preset input=$bn"
        fired=$((fired + 1)); }
    done
  done
done
echo "  tornado:  $((ran - t0)) clean runs over 16 preset/notables x 2 all_at_once"

# ---------------------------------------------------------------------------
# GRZip -- `grzip/bwt.rs` is 234 flagged casts, the second densest file in the
# crate, and every mode reaches a different transform chain.
# ---------------------------------------------------------------------------
CF_GRZ="$(darc_codec_cflags GRZip)" || exit 1
build "$W/grz" "$CF_GRZ" \
  "$CREF/rust/difftest/grzip_ref.cpp" "$CREF/rust/difftest/grzip_ccodec.cpp" \
  "$CREF/Compression/Common.cpp" || { echo "grzip driver build failed" >&2; exit 1; }

"$CORPUSGEN" grzip "$W/grz-in"

g0=$ran; gd0=$declined
for mode in 0 2 4 6 0x100 0x102 0x104 0x106 0x50104 0x50100; do
  for f in "$W"/grz-in/*; do
    bn=$(basename "$f")
    sz=$(( $(wc -c < "$f") * 2 + 1048576 ))
    probe "grzip encode mode=$mode input=$bn" "$W/s" "$W/grz" c "$mode" < "$f" || continue
    probe "grzip decode mode=$mode input=$bn" "$W/o" "$W/grz" d "$sz" < "$W/s" || continue
    cmp -s "$f" "$W/o" || {
      echo "  ROUND-TRIP LOST DATA: grzip mode=$mode input=$bn"
      fired=$((fired + 1)); }
  done
done

# The stream layer is the only one that splits input into blocks, so it is the
# only place the 8 MB - 512 block ceiling and its arithmetic are reached.
"$CORPUSGEN" grzip-big >| "$W/grz-big"
probe "grzip stream encode (big)" "$W/ss" "$W/grz" sc < "$W/grz-big" \
  && probe "grzip stream decode (big)" "$W/sr" "$W/grz" sd < "$W/ss" \
  && { cmp -s "$W/grz-big" "$W/sr" || {
         echo "  ROUND-TRIP LOST DATA: grzip stream (big)"; fired=$((fired + 1)); }; }
echo "  grzip:    $((ran - g0)) clean runs over 10 modes + the stream layer ($((declined - gd0)) declined)"

# ---------------------------------------------------------------------------
# BSC -- `bsc/qlfc_enc.rs` (254) and `bsc/qlfc.rs` (218) are the densest files
# in the crate. `E` runs the whole encoder, so one sweep reaches the sorters
# (bwt_enc, st), LZP and QLFC together.
# ---------------------------------------------------------------------------
CF_BSC="$(darc_codec_cflags BSC)" || exit 1
# libsais.c must be its own translation unit -- see bsc_ccodec.cpp.
build "$W/bsc" "$CF_BSC" \
  "$CREF/rust/difftest/bsc_full_ref.cpp" "$CREF/rust/difftest/bsc_ccodec.cpp" \
  "$CREF/Compression/BSC/libbsc/bwt/libsais/libsais.c" \
  || { echo "bsc driver build failed" >&2; exit 1; }

"$CORPUSGEN" bsc-full "$W/bsc-in"

# sorter: 1=BWT 3..6=ST; coder: 1=static 2=adaptive 3=fast; lzp off/on.
# Combinations are declined by design for some inputs -- that is what the
# decline counter is for.
b0=$ran; bd0=$declined
for sorter in 1 3 4 5 6; do
  for coder in 1 2 3; do
    for lzp in "0 0" "16 128"; do
      for f in "$W"/bsc-in/*; do
        bn=$(basename "$f"); sz=$(wc -c < "$f")
        tag="sorter=$sorter coder=$coder lzp=${lzp// /,} input=$bn"
        # The Rust ENCODER leg. `E` is the only mode that switches implementation
        # under -DUSE_RUST, and it always exits 0 -- it emits the return CODE
        # even on refusal, so a declining combination still reaches the encoder.
        # shellcheck disable=SC2086  # $lzp is deliberately two arguments
        probe "bsc encode $tag" "$W/e" "$W/bsc" E "$sorter" "$coder" $lzp < "$f" || continue

        # The Rust DECODER leg needs a block `d` can read, and `E` does not emit
        # one: its leading int is that return code, where `d` expects the
        # ORIGINAL size. Mode `e` writes what `d` wants and deliberately always
        # uses the C, which is what makes it a usable fixture here.
        # shellcheck disable=SC2086
        "$W/bsc" e "$sorter" "$coder" $lzp < "$f" >| "$W/eb" 2>/dev/null || continue
        probe "bsc decode $tag" "$W/o" "$W/bsc" d "$sz" < "$W/eb" || continue
        cmp -s "$f" "$W/o" || {
          echo "  ROUND-TRIP LOST DATA: bsc $tag"; fired=$((fired + 1)); }
      done
    done
  done
done
echo "  bsc:      $((ran - b0)) clean runs over 5 sorters x 3 coders x 2 LZP ($((declined - bd0)) declined)"

# ---------------------------------------------------------------------------
# lz4hc -- 130 flagged casts, and the three strategies (lz4mid, lz4hc, lz4opt)
# have entirely separate match-search arithmetic.
# ---------------------------------------------------------------------------
CF_LZ4="$(darc_codec_cflags LZ4)" || exit 1
build "$W/lz4" "$CF_LZ4" "$CREF/rust/difftest/lz4hc_ref.cpp" \
  || { echo "lz4hc driver build failed" >&2; exit 1; }

"$CORPUSGEN" lz4hc "$W/lz4-in"

# `rs` is the Rust encoder; `d` is the C decoder, used only to confirm the block
# the Rust encoder produced is a legal one. There is no Rust LZ4 decoder to
# reach, so this leg is a sanity check rather than extra coverage.
l0=$ran; ld0=$declined
for lvl in 1 2 3 4 5 6 7 8 9 10 11 12; do
  for f in "$W"/lz4-in/*; do
    bn=$(basename "$f"); sz=$(wc -c < "$f")
    probe "lz4hc encode L$lvl input=$bn" "$W/e" "$W/lz4" rs "$lvl" < "$f" || continue
    "$W/lz4" d "$sz" < "$W/e" >| "$W/o" 2>/dev/null || {
      echo "  C COULD NOT DECODE the Rust block: lz4hc L$lvl input=$bn"
      fired=$((fired + 1)); continue; }
    cmp -s "$f" "$W/o" || {
      echo "  ROUND-TRIP LOST DATA: lz4hc L$lvl input=$bn"; fired=$((fired + 1)); }
  done
done
echo "  lz4hc:    $((ran - l0)) clean runs over 12 levels ($((declined - ld0)) declined)"

# ---------------------------------------------------------------------------
# Each section must have reached its codec. A section that declined everything
# has tested nothing, and without this it would read as a pass -- the failure
# mode this whole file exists to prevent.
# ---------------------------------------------------------------------------
[ "$((ran - t0))" -gt 0 ] || { echo "tornado ran nothing"; exit 1; }
[ "$((ran - g0))" -gt 0 ] || { echo "grzip ran nothing";   exit 1; }
[ "$((ran - b0))" -gt 0 ] || { echo "bsc ran nothing";     exit 1; }
[ "$((ran - l0))" -gt 0 ] || { echo "lz4hc ran nothing";   exit 1; }

echo "debug-assert: $ran clean runs, $declined declined, $fired panic(s)/failure(s)"
[ "$fired" -eq 0 ] || exit 1
echo "no assertion or overflow check fires across tornado, grzip, bsc and lz4hc"
