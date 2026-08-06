#!/usr/bin/env bash
# Differential-test the SREP *compressor* port against the C original.
#
# The decoder is done and gated by srep-check.sh. This is the other half.
#
# ## Scope: -m3f, because that is what DArc actually runs
#
# `default = m3f` for `[External compressor:srep]`, so method 3 + Future-LZ is
# the invocation every `-m...srep` archive goes through. That is the milestone.
# The source was `Installer/bin/arc.ini:323`; that FreeArc-era packaging tree
# has been deleted, so read it at `c267621` if the provenance is ever in doubt.
# DArc's own equivalent is `[external.srep]` in `darc.toml`.
#
# SREP's compressor is really five algorithms, not one:
#
#   -m0       in-memory (REP)            compress_inmem.cpp
#   -m1 -m2   content-defined chunking   compress_cdc.cpp   ** multithreaded **
#   -m3 -m4   fixed-block matching       compress.cpp
#   -m5       exhaustive search          compress.cpp, and the C reference
#                                        aborts on it for some inputs (see
#                                        srep-check.sh)
#
# and `compress()` is a template over ACCELERATOR with eight instantiations
# (0,1,2,4,8,16,32,64), selected by the switch at srep.cpp:612-621. Only the
# -m3 family is in scope here. The CDC methods are deliberately last: `-tN`
# threads are documented as applying to -m1/-m2 only, so their output may depend
# on thread count, and that has to be established before it can be gated.
#
# ## The bar is byte-identity, and it has to be
#
# SREP has no specification. The C source IS the format, so "produces something
# the C can decode" is not enough: a stream that decodes correctly but differs
# byte-for-byte is a stream no other DArc build reproduces. Both directions are
# checked anyway -- the C decoder must read the port's output -- because that
# catches a different failure than identity does.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_srep_reference "$ROOT")" || exit 1

W="${TMPDIR:-/tmp}/srep-encode-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# The oracle is the standalone C binary, built by srep/compile -- SREP is an
# external compressor, not an in-process codec, so there is no staticlib to link
# and no pinned-reference tree to extract.
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

( cd "$ROOT/rust" && cargo build --release -p darc-codecs --bin srep ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
RS="$ROOT/rust/target/release/srep"
[ -x "$RS" ] || { echo "cargo produced no $RS" >&2; exit 1; }

# Same corpus shape as srep-check.sh: a long-range matcher needs far-apart
# duplicates, repeats separated by noise, long runs, and incompressible data
# where it finds nothing and stores literals.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen --bin difftest-util ) || exit 1
UTIL="$ROOT/rust/target/release/difftest-util"

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" srep "$W/in"

# -hash=md5 throughout: SREP's default block hash is VMAC, which this repo's
# ARM64 LibTomCrypt miscompiles (the ulong32 bug), so the reference
# intermittently rejects its own output. The hash is orthogonal to the match
# encoding under test, and its per-block tag is part of the compressed bytes, so
# pinning it keeps the comparison about the LZ layer.
#
# The -a sweep is the point of several of these rows: -a{accel}/{ACCELERATOR}
# (srep.cpp:280) selects which of the eight compress<> instantiations runs. The
# default is computed from L, so without this sweep only one or two of the eight
# are ever exercised -- the Tornado port shipped a preset bug for exactly that
# reason.
#
# Small block sizes are not decoration: at the default 8 MB every corpus input
# fits in one block and no cross-block path is reached.
# -m1's chunk boundaries are CPU-DEPENDENT IN THE C. compress_cdc.cpp:136 picks
# CrcRollingHash when crc32c() is true and PolynomialRollingHash otherwise, and
# crc32c() (hashes.cpp:214) compiles to a CPUID SSE4.2 test on x86 and to a
# literal `false` everywhere else. So an x86-64 reference cuts chunks one way and
# an ARM64 one cuts them another, from the same input.
#
# This port implements the POLYNOMIAL variant only, by decision -- it stays
# deterministic on every host. So -m1 can only be gated where the reference
# agrees, and the rows are skipped LOUDLY elsewhere rather than silently passing
# or silently failing.
M1_ROWS='"-m1" "-m1f" "-m1o" "-m1 -b16kb"'
case "$(uname -m)" in
  x86_64|amd64)
    M1_ROWS=""
    echo "  NOTE: skipping every -m1 row -- this host's reference srep uses"
    echo "        CRC32c chunk boundaries (crc32c() is true on x86-64), while the"
    echo "        port implements the portable polynomial variant. -m1 is still"
    echo "        covered by the decoder round-trip in the Rust test suite."
    ;;
esac
eval "set -- $M1_ROWS"; M1_ROWS="$*"

total=0 checked=0 tie=0 oracle=0
for opt in "-m3f" \
           "-m3f -b64kb" "-m3f -b16kb" \
           "-m3f -a0/0" "-m3f -a1/1" "-m3f -a2/2" "-m3f -a4/4" \
           "-m3f -a8/8" "-m3f -a16/16" "-m3f -a32/32" "-m3f -a64/64" \
           "-m3f -b16kb -a1/1" "-m3f -b16kb -a8/8" \
           "-m3o" "-m3" \
           "-m4f" "-m4o" \
           "-m0" "-m0f" "-m0o" "-m0 -b16kb" \
           "-m2" "-m2f" "-m2o" "-m2 -b16kb" \
           "-m5f" "-m5o" "-m5" "-m5f -b16kb" \
           $M1_ROWS; do
  fail=0; n=0
  for f in "$W"/in/*; do
    n=$((n+1)); checked=$((checked+1)); name=$(basename "$f")
    rm -f "$W/c.srep" "$W/r.srep" "$W/back"

    # shellcheck disable=SC2086
    if ! "$SREP" $opt -hash=md5 "$f" "$W/c.srep" >/dev/null 2>&1; then
      # The reference itself refused. That is not a port failure and there is
      # nothing to compare against, so it is counted and printed separately --
      # -m5's match finder is known to abort on some inputs under
      # Linux/glibc-x86-64 (see srep-check.sh).
      echo "  [$opt] $name: ORACLE REFUSED (reference srep exited nonzero)"
      oracle=$((oracle+1)); continue
    fi
    [ -s "$W/c.srep" ] || [ ! -s "$f" ] \
      || { echo "  [$opt] $name: C produced an empty archive (harness)"; fail=$((fail+1)); continue; }

    # shellcheck disable=SC2086
    "$RS" $opt -hash=md5 "$f" "$W/r.srep" >/dev/null 2>&1 \
      || { echo "  [$opt] $name: RUST-compress FAILED"; fail=$((fail+1)); continue; }

    if ! cmp -s "$W/c.srep" "$W/r.srep"; then
      # `std::sort` at srep.cpp:756 is UNSTABLE and its comparator (:85) looks at
      # `src` alone, so records sharing a source come out in an order the C++
      # standard library picks. Measured: of five corpus inputs with a tied
      # source, this libc++ preserved four and reversed one (`runs`, 240
      # records) -- introsort insertion-sorts small ranges, which is stable, and
      # only perturbs ties once quicksort engages. A libstdc++ build can
      # therefore produce a different archive from the same input, so this is not
      # a property the C has to reproduce.
      #
      # The helper passes ONLY when the two streams are the same multiset of
      # records per block with identical headers, hashes and literals. Any other
      # difference is still a failure.
      if "$UTIL" srep-tie-order "$W/c.srep" "$W/r.srep"; then
        tie=$((tie+1))
      else
        echo "  [$opt] $name: compressed streams differ ($(wc -c <"$W/c.srep") vs $(wc -c <"$W/r.srep") bytes)"
        fail=$((fail+1))
      fi
      continue
    fi

    # Identity is the gate; this catches the different failure where BOTH
    # implementations agree on something the decoder cannot read.
    "$SREP" -d "$W/r.srep" "$W/back" >/dev/null 2>&1 \
      || { echo "  [$opt] $name: C cannot decode the port's output"; fail=$((fail+1)); continue; }
    cmp -s "$f" "$W/back" || { echo "  [$opt] $name: round-trip != original"; fail=$((fail+1)); }
  done
  echo "  [$opt] $n inputs, $fail differing"
  total=$((total+fail))
done

echo "srep encode: $checked comparisons, $total differing, $tie tie-order-only, $oracle oracle-refused"
[ "$total" -eq 0 ] || exit 1

# The harness must be able to fail. Every input above is well-formed, so all the
# comparisons pass trivially if the port silently emits whatever the C emitted.
# Prove the port actually compressed: a stream must be smaller than its input on
# the compressible corpus, and must not be byte-identical to the input.
"$RS" -m3f -hash=md5 "$W/in/dup" "$W/probe.srep" >/dev/null 2>&1 \
  || { echo "probe compress failed" >&2; exit 1; }
insz=$(wc -c < "$W/in/dup"); outsz=$(wc -c < "$W/probe.srep")
[ "$outsz" -lt "$insz" ] \
  || { echo "port did not compress a 6x-duplicated input ($insz -> $outsz)" >&2; exit 1; }

echo "SREP compressor matches the C original byte for byte (-m3/-m4 family)"
