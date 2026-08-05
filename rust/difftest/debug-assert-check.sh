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
# them. What is still debug-only, and still tested nowhere else, is `-C
# overflow-checks`: an `as`-free arithmetic wrap that release computes silently
# panics here. With ~2400 `as` casts in the crate that is a live class of bug,
# so this file keeps its job with a narrower remit -- and the liveness probe
# below had to change with it, since the assertion messages it used to look for
# are in the release artifact too now.
#
# ## What this checks, and what it does NOT
#
# This is not a differential test. It does not compare against the C at all --
# the other harnesses do that, and doing it here would only duplicate them more
# slowly. What it does is drive the codec through a debug build and fail if an
# assertion or an overflow check fires, or the process dies. The signal is a
# panic, not a byte mismatch.
#
# Tornado is the whole scope today: its four match finders and two entropy
# back-ends are the densest arithmetic in the crate, and every preset reaches a
# different combination. Extending to another codec is mechanical -- add its
# `*_ref.cpp` pair to `CODECS` below with the arguments its driver expects.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
CFLAGS_C="$(darc_codec_cflags Tornado)" || exit 1
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
# `-C overflow-checks` is on. It deliberately is not an assertion message any
# more: every assertion in this crate is now unconditional, so an assertion
# string is present in a release artifact too and would prove nothing about the
# profile this file needs.
checks=$(strings -a "$LIB" 2>/dev/null | grep -c 'attempt to .* with overflow')
[ "${checks:-0}" -ge 1 ] || {
  echo "the debug staticlib carries no overflow-check panic messages -- overflow" >&2
  echo "checks are compiled out, so this run would prove nothing." >&2
  exit 1; }

cc() { local out="$1"; shift
  # shellcheck disable=SC2086  # the flag list is a word list on purpose
  clang++ -std=c++17 $CFLAGS_C -w -DUSE_RUST \
    -DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT \
    -I"$CREF" -I"$CREF/Compression" \
    "$CREF/rust/difftest/tornado_ref.cpp" "$CREF/rust/difftest/tornado_ccodec.cpp" \
    "$CREF/Compression/Common.cpp" "$LIB" -o "$out"; }
cc "$W/rs" || { echo "debug driver build failed" >&2; exit 1; }
[ -x "$W/rs" ] || { echo "driver missing after a clean build" >&2; exit 1; }

( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" debug-assert "$W/in"

# Every preset, both `all_at_once` settings, both directions. `notables` is
# swept on the presets whose data-table detector is reachable, matching
# tornado-encode-check.sh.
fired=0; ran=0
for case in "0 0" "1 0" "2 0" "3 0" "3 1" "4 0" "4 1" "5 0" "5 1" \
            "6 0" "6 1" "7 0" "8 0" "9 0" "10 0" "11 0"; do
  set -- $case; preset=$1; notables=$2
  for aao in 0 1; do
    for f in "$W"/in/*; do
      bn=$(basename "$f")
      rm -f "$W/s" "$W/o" "$W/err"
      if ! "$W/rs" c "$preset" "$notables" "$aao" < "$f" >| "$W/s" 2>| "$W/err"; then
        echo "  ASSERTION or failure: encode preset=$preset notables=$notables aao=$aao input=$bn"
        sed 's/^/     /' "$W/err" | head -6
        fired=$((fired + 1)); continue
      fi
      if grep -qi 'panicked\|assertion' "$W/err" 2>/dev/null; then
        echo "  ASSERTION on stderr: encode preset=$preset notables=$notables aao=$aao input=$bn"
        sed 's/^/     /' "$W/err" | head -6
        fired=$((fired + 1)); continue
      fi
      # Preset 0 is STORING, and its stream is deliberately undecodable: the
      # C's dispatch builds an LZ77_ByteCoder while still writing 0 as the
      # encoding method (Tornado.cpp:331-333), and the C's own decoder switch
      # rejects STORING with BAD_COMPRESSED_DATA (:522). The port reproduces
      # that faithfully, so round-tripping it reports 20 spurious -7 failures.
      # Its ENCODE leg still runs above, which is where its assertions are.
      if [ "$preset" = 0 ]; then ran=$((ran + 1)); continue; fi
      # Decode too: the decoder has its own assertions, and a round-trip is the
      # cheapest way to reach them.
      if ! "$W/rs" d < "$W/s" >| "$W/o" 2>| "$W/err"; then
        echo "  ASSERTION or failure: decode preset=$preset input=$bn"
        sed 's/^/     /' "$W/err" | head -6
        fired=$((fired + 1)); continue
      fi
      if grep -qi 'panicked\|assertion' "$W/err" 2>/dev/null; then
        echo "  ASSERTION on stderr: decode preset=$preset input=$bn"
        sed 's/^/     /' "$W/err" | head -6
        fired=$((fired + 1)); continue
      fi
      # Not a differential check, but a round-trip that loses data would mean
      # the run below is exercising something other than the codec.
      cmp -s "$f" "$W/o" || { echo "  ROUND-TRIP LOST DATA: preset=$preset input=$bn"; fired=$((fired + 1)); continue; }
      ran=$((ran + 1))
    done
  done
done

[ "$ran" -gt 0 ] || { echo "nothing ran -- the harness reached no codec"; exit 1; }
echo "debug-assert: $ran clean runs, $fired assertion(s)/failure(s)"
[ "$fired" -eq 0 ] \
  && echo "no debug assertion fires across 16 preset/notables combinations x 2 all_at_once x 9 inputs" \
  || exit 1
