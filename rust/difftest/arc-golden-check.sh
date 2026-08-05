#!/usr/bin/env bash
# The archiver's format gate, after the Haskell reference was deleted.
#
# Every other arc-*-check.sh compares two binaries. That stops working the
# moment there is only one, and CLAUDE.md names format compatibility as the
# highest-risk failure mode here: a change that compresses fine but writes
# archives older builds cannot read passes every build check there is.
#
# So this records the reference's verdict instead of re-deriving it. The
# manifest in golden/ holds one SHA-256 per case, produced by `--record` against
# `Tests/arc-ghc` while the two builds were still byte-identical -- 19 harnesses
# green at d6ebeb6. From here the port is checked against those bytes forever.
#
# ── Three kinds of case, because compatibility is not the bar ───────────────
#
# CLAUDE.md no longer treats the reference as the specification: DArc is not a
# drop-in replacement, and where conformance and correctness conflict the better
# behaviour wins. That leaves this file with cases the reference cannot record,
# so the manifest carries a `kind` in column 3. Two-column lines are `ref` and
# every one of the original 105 is still written exactly that way.
#
#   ref     the reference's bytes. A move is a REGRESSION -- do not re-record
#           from the port, which would replace the thing being checked with the
#           thing doing the checking.
#   port    the port deliberately writes different bytes. Column 4 is what the
#           REFERENCE writes, which is what makes an accidental return to it
#           detectable; column 5 on is why. Added by hand, never by --record,
#           because only a person can state the reason.
#   refuse  the input must be rejected: non-zero exit, under 128, no archive.
#           A signal death is `crashed` and is NOT a pass -- the reference
#           aborts on `-mlzma:lc300`, and that abort is the defect #136 fixed.
#
# `--record` regenerates ONLY the ref lines and carries port/refuse lines over
# verbatim. Without that, one re-record would delete every divergence and the
# deletion would look like routine maintenance.
#
# The header also pins `# ref-cases: N`. Reclassifying a reference case as a
# deliberate divergence is sometimes right and is always a decision; making the
# count move with it puts that decision in the diff, beside the reason. It does
# not make laundering impossible -- nothing can, when the port is the only thing
# that can produce the new bytes -- it makes it deliberate.
#
# Self-test: running this against `Tests/arc-ghc` must FAIL, and fail in the
# specific ways the divergences describe -- RECONVERGED on the port case,
# ACCEPTED on lc259 and the dictionary overflow, CRASHED on lc300 and lp300 --
# while all 105 ref cases still pass. That single run exercises every new
# failure path, which is why there is no separate sabotage script for it.
#
# ── What the cases may and may not do ───────────────────────────────────────
#
# A checked-in hash has to be MACHINE-INDEPENDENT, which the differential
# harnesses never had to care about: they ran two binaries on one machine, so
# anything that varied with the host cancelled out.
#
#   * `--nodates` on every case. Stored mtimes are otherwise the filesystem's.
#   * `-m0` or an EXPLICITLY parameterised chain -- never a preset. A preset is
#     fitted before it is written, and a fitted chain is written into the
#     archive, so anything the fitting reads becomes archive-visible.
#   * `grzip`, `4x4` and `lzma2` ONLY with an explicit `-mtN`. Three things
#     read the thread count and turn it into archive bytes: GRZip's and 4x4's
#     memory formulas, and LZMA2's encoder, which above one block thread stops
#     emitting one solid block and splits the input. Without `-mtN` all three
#     bake the recording machine's core count into the hash.
#   * `-rr` in absolute bytes, never a percentage. A percentage is taken of the
#     archive size and then clamped -- in the C, at getPhysicalMemory/2.
#
# An earlier version of this list claimed `-mt1` did NOT pin the thread count,
# on the evidence that adding it changed no hash. That evidence was worthless:
# every case it was tried on used a method whose output does not depend on
# threads, so of course nothing moved. It is now measured directly --
# `-mgrzip -lc16m -mt1` and `-mt8` produce different archives, and the port
# matches the reference in both -- which is what makes the three methods above
# recordable at all.
#
# A case that violates one of those will record fine and fail on another
# machine, which reads as a format regression and is not one. Add cases in that
# style.
#
# ── A limit that never binds tests nothing ──────────────────────────────────
#
# Every case here used the small tree, which is under 200 KB. `limitDictionary`
# shrinks the chain to the DATA size before anything else, so on a tree that
# small no memory limit and no block-size cap is ever reached: the arithmetic
# under test is never executed. Four format bugs lived behind that for the
# whole life of this file -- GRZip's block cap being 1 GB instead of 8 MB-512,
# BSC's decompression figure and setter, LZ4's setter, and `-lc`/`-ld` not
# reaching the solid-block grouping.
#
# `build_big_tree` exists for exactly that: ~12 MB, so a limit put ABOVE the
# codec's own constants and BELOW the data size actually binds. When adding a
# case for a limit, check that the figure you pass changes the stored method
# string -- if it does not, the case is decoration.
#
# Usage:
#   arc-golden-check.sh                     check the port against the manifest
#   arc-golden-check.sh --record REFERENCE  rewrite the manifest from REFERENCE
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
MANIFEST="$ROOT/rust/difftest/golden/manifest.txt"

record=0
BIN="$ROOT/rust/target/release/darc"
case "${1:-}" in
  --record)
    record=1
    BIN="${2:-$ROOT/Tests/arc-ghc}"
    [ -x "$BIN" ] || { echo "no reference binary at $BIN" >&2; exit 2; }
    ;;
  "") ;;
  *) BIN="$1" ;;
esac

# Every case `cd`s into its tree before running, so a relative binary path would
# stop resolving there -- and it fails by producing NO archive, which this script
# would then happily record as the expected result "gone".
case "$BIN" in
  /*) ;;
  *)  BIN="$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")" ;;
esac
[ -x "$BIN" ] || { echo "no binary at $BIN" >&2; exit 2; }

if [ "$record" = 0 ]; then
  ( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
    echo "cargo build failed" >&2; exit 1; }
  [ -s "$MANIFEST" ] || {
    echo "no manifest at $MANIFEST -- record one first" >&2; exit 2; }
fi

# The big tree's incompressible file comes from here. Built in BOTH modes --
# recording needs it as much as checking does, and a missing one would produce
# a tree that silently differs from the recorded one.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen ) || {
  echo "cannot build corpusgen" >&2; exit 1; }
CORPUSGEN="$ROOT/rust/target/release/corpusgen"
[ -x "$CORPUSGEN" ] || { echo "no corpusgen at $CORPUSGEN" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-golden-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

sha() {
  # macOS has shasum, Linux has sha256sum, and neither has both.
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

# A fixed tree: fixed names, fixed contents, fixed sizes. No mtimes are set
# because every case passes --nodates, which zeroes what is stored.
build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub/deeper" "$d/other"
  printf 'a\n'                     > "$d/a.txt"
  printf 'bbbb\n'                  > "$d/b.dat"
  printf 'n\n'                     > "$d/sub/n.txt"
  printf 'deep\n'                  > "$d/sub/deep.dat"
  printf 'q\n'                     > "$d/sub/deeper/q.txt"
  printf 'o\n'                     > "$d/other/o.txt"
  # Something big enough and repetitive enough for a real codec to work on,
  # and generated rather than random so it is identical everywhere.
  awk 'BEGIN{for(i=0;i<4000;i++) printf "line %d of compressible text\n", i%97}' \
    > "$d/text.txt"
  head -c 65536 /dev/zero | tr '\0' 'Z' > "$d/zeros.bin"
  # A zero-byte file, which is its own decoder case: it produces an EMPTY data
  # block, and a decoder handed no bytes has no header to read. `-mtta` over
  # one failed with a bare "codec returned -6" until `decompress_chain` learned
  # to skip the codec for an empty block.
  : > "$d/empty.bin"
}

# ~12 MB, deterministic, and larger than every block-size constant a method
# carries -- GRZip's 8 MB-512 cap is the largest. Generated rather than random
# so it is byte-identical on every machine, and compressible enough that the
# codecs do real work.
#
# The size is the point. On the small tree the dictionary limit shrinks every
# chain to ~180 KB before any -lc/-ld/-md is consulted, so those options reach
# arithmetic that is never wrong because it is never run.
build_big_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/big"
  # THREE files, ~12 MB total. Not six: at 21 MB the reference itself HANGS on
  # `-mlz4` -- deterministically, five trials for five hangs, with and without
  # a limit, at 0.2% CPU for 39 minutes before it was killed. 19 MB is fine and
  # 12 MB is fine, so it is that tree's size-and-shape, not lz4 as such. The
  # port does not hang on any of them. A case the REFERENCE cannot produce
  # cannot be recorded, so the tree stays on the side of the cliff that works.
  local i
  for i in 1 2 3; do
    awk -v n="$i" 'BEGIN{
      for (j = 0; j < 60000; j++)
        printf "block %d line %d of repeating but not identical text\n", n, j % 1000
    }' > "$d/big/f$i.txt"
  done
  # One file that does not compress, so a block-size cap has something
  # incompressible to split.
  #
  # From `corpusgen`, NOT from awk. This was
  # `awk 'BEGIN{srand(7); ... rand() ...}'`, which is not portable -- BSD awk
  # and gawk have different PRNGs, so the file, and every archive built over
  # it, differed between macOS and Linux. All 20 big cases failed CI on both
  # Linux runners while passing on macOS and Windows. `big-store` was among
  # them, which is what identified it: `-m0` stores bytes verbatim, so that
  # archive can only move if the INPUT moved.
  #
  # This is the reason CLAUDE.md says corpora come from `corpusgen`. The
  # determinism screen that passed this file varied the thread count and never
  # the platform, which is why it did not catch it either.
  "$CORPUSGEN" prng 7 1600000 > "$d/big/noise.bin"
}

# A tree the multimedia path can actually act on: the groups file matches on
# EXTENSION and the detector probes CONTENT, so both halves have to be real.
# Separate from build_tree so that adding it does not move the 93 hashes that
# were already recorded.
#
# The SAMPLE DATA COMES FROM corpusgen, and that is not a style choice. The
# first version generated the PCM with
#
#     awk '{ v = int(9000*sin(i/17)); printf "%c%c%c%c", v%256, ... }'
#
# which is not portable twice over: `v` goes negative for half a sine wave, and
# `printf "%c"` with a negative argument is implementation-defined. All ten
# `mm-*` cases passed on macOS and failed on both Linux runners -- the other 95
# were fine, which is what identified the TREE rather than the codecs.
#
# This is the same failure as the `srand()`/`rand()` corpus in #128, in a file
# that already carries the rule. `corpusgen` exists precisely so that corpora
# are bytes a typed program produced, not bytes an awk implementation happened
# to choose.
build_mm_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d"
  # Integer arithmetic and non-negative throughout, so these two are safe.
  awk 'BEGIN{for(i=0;i<3000;i++) printf "line %d of compressible prose\n", i%211}' > "$d/doc.txt"
  awk 'BEGIN{for(i=0;i<2000;i++) printf "second file line %d\n", i%97}' > "$d/other.txt"
  head -c 40000 /dev/zero | tr '\0' 'B' > "$d/plain.dat"
  # A 16-bit stereo WAV: the 44-byte header, then real PCM from corpusgen.
  { printf 'RIFF'; printf '\x24\x40\x00\x00'; printf 'WAVEfmt '
    printf '\x10\x00\x00\x00\x01\x00\x02\x00\x44\xac\x00\x00\x10\xb1\x02\x00\x04\x00\x10\x00'
    printf 'data'; printf '\x00\x40\x00\x00'
    "$CORPUSGEN" sine 16384
  } > "$d/s.wav"
  # A 24-bit BMP: header, then a gradient. `repeat` is deterministic and the
  # bytes are a real image's worth of smoothly varying values.
  { printf 'BM'
    printf '\x36\x0c\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x28\x00\x00\x00\x40\x00\x00\x00'
    printf '\x40\x00\x00\x00\x01\x00\x18\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x13\x0b\x00\x00'
    printf '\x13\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00'
    "$CORPUSGEN" sine 12288
  } > "$d/s.bmp"
}

results="$W/results.txt"
: > "$results"

# run <case-id> <command...>  -- each `%%` starts another invocation, all on the
# same tree and the same archive, so update and copy chains can be expressed.
run() {
  local id="$1"; shift
  local tree="$W/t"
  case "$id" in
    big-*) build_big_tree "$tree" ;;
    mm-*)  build_mm_tree "$tree" ;;
    *)     build_tree "$tree" ;;
  esac
  rm -f "$W/g.arc"
  local args=()
  for tok in "$@"; do
    case "$tok" in
      '%%')
        ( cd "$tree" && "$BIN" "${args[@]}" ) >/dev/null 2>&1
        args=()
        ;;
      *) args+=("$tok") ;;
    esac
  done
  ( cd "$tree" && "$BIN" "${args[@]}" ) >/dev/null 2>&1
  # A case whose filters match nothing leaves no archive at all; that is a
  # result too, and "gone" has to be recorded rather than skipped.
  if [ -f "$W/g.arc" ]; then
    printf '%s  %s\n' "$(sha "$W/g.arc")" "$id" >> "$results"
  else
    printf '%s  %s\n' "gone" "$id" >> "$results"
  fi
}

# run_refuse <case-id> <command...> -- a case whose expected result is a REFUSAL,
# not bytes.
#
# These exist because the port deliberately refuses inputs the reference accepts
# or crashes on, so there is no reference hash to record and nothing else gates
# them. `-mlzma:lc300` aborted the process here until #136 (300 narrowed to
# lc44, sizing the literal-probability array at `0x300 << 44`); unit tests cover
# the parser, but nothing covered the CLI, and the parser is not what the user
# runs.
#
# FOUR outcomes, and the fourth is the whole reason this is not just "did it
# write an archive":
#
#   accepted -- an archive exists, so the input was not refused
#   silent   -- exited 0 and wrote nothing: neither a refusal nor a result, and
#               usually a sign this script invoked the binary wrongly
#   crashed  -- died on a signal (128+n; a Rust abort is 134)
#   refused  -- exited non-zero, under 128, and wrote nothing
#
# `crashed` must NOT collapse into `refused`. The reference ABORTS on
# `-mlzma:lc300` -- 300 narrows to lc44 and the literal-probability allocation
# asks for 27 petabytes -- and that abort is precisely the defect #136 fixed. A
# check that accepted any non-zero exit would have passed on the crashing build
# and would pass again if the port ever regressed to it.
run_refuse() {
  local id="$1"; shift
  local tree="$W/t"
  build_tree "$tree"
  rm -f "$W/g.arc"
  local rc=0
  ( cd "$tree" && "$BIN" "$@" ) >/dev/null 2>&1 || rc=$?
  local outcome
  if [ -f "$W/g.arc" ]; then
    outcome=accepted
  elif [ "$rc" = 0 ]; then
    outcome=silent
  elif [ "$rc" -ge 128 ]; then
    outcome=crashed
  else
    outcome=refused
  fi
  printf '%s  %s\n' "$outcome" "$id" >> "$results"
}

A="$W/g.arc"

# ── stored: the format itself, with no codec in the way ─────────────────────
run store-dot            a --nodates -y -m0 "$A" .
run store-r              a --nodates -y -m0 -r "$A" .
run store-named-dir      a --nodates -y -m0 "$A" sub
run store-named-dir-sl   a --nodates -y -m0 "$A" sub/
run store-named-deep     a --nodates -y -m0 "$A" sub/deeper
run store-mask           a --nodates -y -m0 -r "$A" '*.txt'
run store-two-specs      a --nodates -y -m0 -r "$A" sub other

# ── the directory-entry rules, both passes ──────────────────────────────────
run dirs-on              a --nodates -y -m0 -r --dirs   "$A" .
run dirs-off             a --nodates -y -m0 -r --nodirs "$A" .
run dirs-named           a --nodates -y -m0 --dirs   "$A" sub
run dirs-named-off       a --nodates -y -m0 --nodirs "$A" sub
run dirs-n-filter        a --nodates -y -m0 -r --dirs "$A" . -n'*.txt'
run dirs-named-n-filter  a --nodates -y -m0 --dirs "$A" sub -n'*.txt'

# ── selection ───────────────────────────────────────────────────────────────
run filter-n             a --nodates -y -m0 -r "$A" . -n'*.txt'
run filter-x             a --nodates -y -m0 -r "$A" . -x'*.dat'
run filter-both          a --nodates -y -m0 -r "$A" . -n'*' -x'*.dat'
run filter-fullnames     a --nodates -y -m0 -r --fullnames "$A" . -x'sub/n.txt'
run filter-size          a --nodates -y -m0 -r "$A" . -sm10b
run filter-none-match    a --nodates -y -m0 -r "$A" . -n'*.nomatch'

# ── solid grouping: the block boundaries, which are the archive's shape ─────
run solid-default        a --nodates -y -m0 -r "$A" .
run solid-off            a --nodates -y -m0 -r -s- "$A" .
run solid-by-ext         a --nodates -y -m0 -r -se "$A" .
run solid-1k             a --nodates -y -m0 -r -s1k "$A" .
run solid-2files         a --nodates -y -m0 -r -s2 "$A" .

# ── recovery records: absolute sizes only ───────────────────────────────────
run rr-4096b             a --nodates -y -m0 -r -rr4096b "$A" .
run rr-8192b             a --nodates -y -m0 -r -rr8192b "$A" .
run rr-sectors           a --nodates -y -m0 -r -rr'2*512' "$A" .
run rr-none              a --nodates -y -m0 -r -rr- "$A" .

# ── update and copy chains: two writes over one archive ─────────────────────
run update-u             a --nodates -y -m0 -r "$A" a.txt '%%' u --nodates -y -m0 "$A" .
run update-f             a --nodates -y -m0 -r "$A" . '%%' f --nodates -y -m0 "$A" .
run copy-ch              a --nodates -y -m0 -r "$A" . '%%' ch --nodates -y "$A"
run copy-ch-rr           a --nodates -y -m0 -r "$A" . '%%' ch --nodates -y -rr4096b "$A"
run copy-c-lock          a --nodates -y -m0 -r "$A" . '%%' k --nodates -y "$A"
run delete-d             a --nodates -y -m0 -r "$A" . '%%' d --nodates -y "$A" '*.dat'

# ── real codecs, threads pinned ─────────────────────────────────────────────
run m-lzma               a --nodates -y -r -mlzma:1m:normal "$A" .
run m-ppmd               a --nodates -y -r -mppmd:8m:o6 "$A" .
run m-tor                a --nodates -y -r -mtor:3 "$A" .
run m-rep-lzma           a --nodates -y -r -mrep:8m+lzma:1m "$A" .
run m-delta-lzma         a --nodates -y -r -mdelta+lzma:1m "$A" .
run m-lzp                a --nodates -y -r -mlzp:8m:64 "$A" .
run m-dict-lzma          a --nodates -y -r -mdict:32k+lzma:1m "$A" .
run m-lzma-solid-off     a --nodates -y -r -s- -mlzma:1m:normal "$A" .
run m-lzma-rr            a --nodates -y -r -rr4096b -mlzma:1m:normal "$A" .

# ── the five methods darc-arc could not write OR READ until recently ────────
#
# mm, tta, bsc, lz4 and zstd had no `Method` variant, so an archive using any
# of them was rejected in both directions -- `arc x` exited 2 with "compression
# method ... is not supported yet" and extracted nothing.
#
# None of the five reads the processor count, so none needs the exclusion
# above. bsc and lz4 DO cap their solid blocks by their own block size, which
# is why each gets a row with an explicit one.
run m-tta                a --nodates -y -r -mtta "$A" .
run m-tta-level          a --nodates -y -r -mtta:m1 "$A" .
run m-tta-geometry       a --nodates -y -r -mtta:2*16 "$A" .
run m-mm                 a --nodates -y -r -mmm "$A" .
run m-mm-mode            a --nodates -y -r -mmm:d1 "$A" .
run m-mm-geometry        a --nodates -y -r -mmm:2*16f "$A" .
run m-mm-reorder         a --nodates -y -r -mmm:r1 "$A" .
run m-bsc                a --nodates -y -r -mbsc:1m "$A" .
run m-bsc-sorter         a --nodates -y -r -mbsc:1m:b2 "$A" .
run m-bsc-nolzp          a --nodates -y -r -mbsc:1m:h0 "$A" .
run m-lz4                a --nodates -y -r -mlz4 "$A" .
run m-lz4-hc             a --nodates -y -r -mlz4:hc "$A" .
run m-lz4-block          a --nodates -y -r -mlz4:b64k "$A" .
run m-zstd               a --nodates -y -r -mzstd "$A" .
run m-zstd-level         a --nodates -y -r -mzstd:19 "$A" .
run m-zstd-long          a --nodates -y -r -mzstd:3:long20 "$A" .

# ── the -m VALUE grammar ────────────────────────────────────────────────────
#
# `-m` carries a second grammar in its value (Cmdline.hs:241): `-mt` is a
# thread count, `-ms` adds a $compressed chain, `-md` a dictionary size. The
# port read every one of them as a method NAME and rejected them as codecs that
# do not exist. `-md` is the archive-visible one, and `setDictionary` is
# `mapLast` -- the LAST method of the chain only, which a one-method chain
# cannot tell apart from "all of them".
run mopt-threads          a --nodates -y -r -m4 -mt1 "$A" .
run mopt-store-compressed a --nodates -y -r -m4 -ms "$A" .
run mopt-dict-16m         a --nodates -y -r -m4 -md16m "$A" .
run mopt-dict-letter      a --nodates -y -r -m4 -mda "$A" .
run mopt-dict-explicit    a --nodates -y -r -mlzma:1m -md64m "$A" .
run mopt-dict-chain       a --nodates -y -r -mrep:8m+lzma:1m -md1m "$A" .

# ── lzma2, which had no `Method` variant either ─────────────────────────────
#
# Every one pins `-mt1`. LZMA2 is the one codec whose ENCODER reads the thread
# count: above one block thread `Lzma2EncProps_Normalize` abandons the solid
# block and splits the input into dictionary-reset blocks, so the stream itself
# differs. Without `-mt1` these hashes would be the recording machine's core
# count.
run m-lzma2              a --nodates -y -r -mlzma2:1m -mt1 "$A" .
run m-lzma2-dict         a --nodates -y -r -mlzma2:d64k -mt1 "$A" .
run m-lzma2-fb           a --nodates -y -r -mlzma2:1m:fb64 -mt1 "$A" .
run m-lzma2-mf           a --nodates -y -r -mlzma2:1m:mf=BT4 -mt1 "$A" .
run m-lzma2-fast         a --nodates -y -r -mlzma2:1m:a0 -mt1 "$A" .
run m-lzma2-bits         a --nodates -y -r -mlzma2:1m:pb1:lc1:lp1 -mt1 "$A" .
run m-lzma2-chain        a --nodates -y -r -mrep:8m+lzma2:1m -mt1 "$A" .
# The multi-block stream, pinned to a count that is not the machine's.
run m-lzma2-mt2          a --nodates -y -r -mlzma2:d64k -mt2 "$A" .

# ── limits that actually BIND, on a tree big enough to reach them ───────────
#
# These are the cases the small tree could not express. Each figure is chosen
# to sit below the ~12 MB of data and above (or across) the codec's own
# constant, so the limiting arithmetic runs. Four format bugs were found by
# this section's first run; before it, `-lc`/`-ld`/`-md` were covered only by
# cases where they could not possibly bind.
#
# `-md20m` on GRZip is the specific shape that found the block cap: it is over
# the 8 MB-512 maximum, so it must clamp there rather than pass through.
run big-store            a --nodates -y -r -m0 "$A" .
run big-lzma-ld8m        a --nodates -y -r -mlzma:8m -ld8m "$A" .
run big-lzma-lc16m       a --nodates -y -r -mlzma:8m -lc16m "$A" .
run big-lzma-md4m        a --nodates -y -r -mlzma:8m -md4m "$A" .
run big-bsc-ld8m         a --nodates -y -r -mbsc -ld8m "$A" .
run big-bsc-ld32m        a --nodates -y -r -mbsc -ld32m "$A" .
run big-bsc-md4m         a --nodates -y -r -mbsc -md4m "$A" .
run big-lz4-ld1m         a --nodates -y -r -mlz4 -ld1m "$A" .
run big-lz4-md20m        a --nodates -y -r -mlz4 -md20m "$A" .
run big-tor-lc8m         a --nodates -y -r -mtor -lc8m "$A" .
run big-tor-lc4m         a --nodates -y -r -mtor -lc4m "$A" .
run big-rep-lc8m         a --nodates -y -r -mrep -lc8m "$A" .
run big-lzp-ld8m         a --nodates -y -r -mlzp -ld8m "$A" .
run big-dict-md256k      a --nodates -y -r -mdict -md256k "$A" .
run big-lzma2-lc16m      a --nodates -y -r -mlzma2 -lc16m -mt1 "$A" .
run big-lzma2-ld8m       a --nodates -y -r -mlzma2 -ld8m -mt1 "$A" .
# GRZip and 4x4 are admissible with the count pinned, and this is where their
# thread-scaled memory formulas are finally exercised.
run big-grzip-md20m      a --nodates -y -r -mgrzip -md20m -mt1 "$A" .
run big-grzip-lc16m      a --nodates -y -r -mgrzip -lc16m -mt1 "$A" .
run big-grzip-ld20m      a --nodates -y -r -mgrzip -ld20m -mt1 "$A" .
run big-4x4-md1m         a --nodates -y -r -m4x4:tor -md1m -mt1 "$A" .

# ── multimedia, -ma and -mc: the options with no golden coverage at all ─────
#
# These need a GROUPS FILE. Without one every file is $binary, $wav and $bmp
# are unreachable, and -mm/-ma become no-ops that record a hash proving
# nothing -- which is exactly how a routing bug survived until #129. The path
# is absolute and is not stored in the archive, so it cannot vary the hash.
#
# -mt1 on every one: -m4 reaches 4x4 and $bmp reaches grzip, and the rule at
# the top of this file says those are recordable only with the count pinned.
#
# `mm-groups`, `mm-max` and `mm-ma2` deliberately record the SAME hash. -m4's
# preset already selects max multimedia and its level is 4, so `-mmmax` and
# `-ma2` are no-ops on it -- that equality is the property, and it is why -mm
# is an override of a choice the preset already made rather than the switch
# that turns multimedia on. Each still pins its own command line, so a change
# that made either diverge would fail here.
GROUPFILE="$ROOT/Tests/darc.groups"
run mm-groups            a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 "$A" .
run mm-off               a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -mm- "$A" .
run mm-fast              a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -mmfast "$A" .
run mm-max               a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -mmmax "$A" .
run mm-ma0               a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -ma0 "$A" .
run mm-ma2               a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -ma2 "$A" .
# -m2's level is 2, so -ma1 crosses the `detect_level <= 1` line where -ma2
# does not. Without this the -ma cases here would all sit on one side of it.
run mm-m2-ma1            a --nodates -y -r -mt1 --groups="$GROUPFILE" -m2 -ma1 "$A" .
run mm-ma0-solid-off     a --nodates -y -r -mt1 --groups="$GROUPFILE" -m5 -ma1 -s- "$A" .
run mm-mc-tta            a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -mc-tta "$A" .
run mm-mc-rep            a --nodates -y -r -mt1 --groups="$GROUPFILE" -m4 -mc-rep "$A" .

# ── darc.toml cannot be golden-recorded, and that is structural ────────────
#
# Tried and removed. This file runs ONE command line through ONE binary, and
# the reference cannot read a `darc.toml` -- so recording a `-cfg` case from it
# feeds it a file it ignores. Both attempts recorded the same hash as a plain
# `-m9`, which the port then failed by correctly applying the config.
#
# It is the same shape as encryption, which is ungoldenable because the salt is
# random: a case whose two sides cannot be made to agree by construction does
# not belong here. `arc-config-check.sh` covers it instead, and can, because it
# is differential -- each binary gets the config syntax it understands.

# ── two more whole-archive shapes ───────────────────────────────────────────
run nodata               a --nodates -y -r --nodata -m0 "$A" .
run crconly              a --nodates -y -r --crconly -m0 "$A" .

# ── deliberate divergence: the port writes bytes the reference does not ─────
#
# `-ld` at or below LZMA's 2 MB overhead. The C's `if (mem > 2mb)` guard means a
# small limit is silently dropped and the dictionary is left alone, so `-ld1m`
# on `d64m` produced an archive needing 66 MB to open. The port floors the
# dictionary at 4 KB instead (`memlimit.rs`). The big tree, because a limit that
# never binds tests nothing -- see the note at the top of this file.
run big-lzma-d64m-ld1m   a --nodates -y -r -mlzma:d64m -ld1m "$A" .

# ── refusals: inputs that must be rejected, not accepted and not aborted ────
#
# Every one of these is a method string the reference either accepts (writing an
# archive whose header disagrees with what compressed it) or crashes on.
run_refuse lzma-lc9      a --nodates -y -mlzma:lc9 "$A" .
run_refuse lzma-lc259    a --nodates -y -mlzma:lc259 "$A" .
run_refuse lzma-lc300    a --nodates -y -mlzma:lc300 "$A" .
run_refuse lzma-lp300    a --nodates -y -mlzma:lp300 "$A" .
run_refuse lzma-pb5      a --nodates -y -mlzma:pb5 "$A" .
run_refuse lzma2-lc9     a --nodates -y -mlzma2:lc9 "$A" .
run_refuse lzma-dict-overflow a --nodates -y -mlzma:d5000000000 "$A" .

sort -o "$results" "$results"

if [ "$record" = 1 ]; then
  # "gone" is a legitimate result for exactly one case -- the filter that
  # matches nothing. Any more than that means the binary was not running at
  # all, and recording it would bake "produces no archive" in as correct.
  gone=$(grep -c '^gone  ' "$results")
  if [ "$gone" -gt 1 ]; then
    echo "refusing to record: $gone cases produced no archive, expected 1" >&2
    grep '^gone  ' "$results" | sed 's/^/  /' >&2
    exit 2
  fi
  # `port` and `refuse` lines are carried over verbatim. The reference CANNOT
  # produce them -- that is what makes them divergences -- so regenerating from
  # it would delete every one of them and the deletion would look like a normal
  # re-record. Their ids are also dropped from the freshly recorded set, since
  # the reference's answer for those cases is precisely what we rejected.
  carried=""
  kept=0
  if [ -s "$MANIFEST" ]; then
    carried="$W/carried"
    awk '$1 !~ /^#/ && ($3=="port" || $3=="refuse")' "$MANIFEST" > "$carried"
    kept=$(grep -c . "$carried" 2>/dev/null || echo 0)
    if [ "$kept" -gt 0 ]; then
      # Drop the reference's version of anything we deliberately own.
      awk 'NR==FNR {own[$2]=1; next} !($2 in own)' "$carried" "$results" > "$W/refonly"
      mv "$W/refonly" "$results"
    fi
  fi
  mkdir -p "$(dirname "$MANIFEST")"
  {
    echo "# Generated by arc-golden-check.sh --record"
    echo "# Reference: $BIN"
    echo "#"
    echo "# Columns:  <sha|outcome>  <case-id>  [kind]  [reference]  [reason...]"
    echo "#"
    echo "#   kind absent or 'ref' -- the Haskell reference's bytes, recorded"
    echo "#     before it was deleted. Do NOT regenerate these from the port:"
    echo "#     that replaces the thing being checked with the thing doing the"
    echo "#     checking. Re-record from a build of d6ebeb6 or earlier instead."
    echo "#   'port' -- the port deliberately writes DIFFERENT bytes. Column 4"
    echo "#     is what the reference writes, so an accidental return to it is"
    echo "#     detected too; column 5 on is why. Added by hand, never by"
    echo "#     --record, because only a person can state the reason."
    echo "#   'refuse' -- the input must be rejected: non-zero exit, no archive."
    echo "#"
    echo "# ref-cases: $(grep -cv '^#' "$results" 2>/dev/null || echo 0)"
    cat "$results"
    [ "$kept" -gt 0 ] && cat "$carried"
  } > "$MANIFEST"
  echo "recorded $(grep -c . "$results") reference cases to $MANIFEST"
  [ "$kept" -gt 0 ] && echo "carried over $kept port/refuse case(s) unchanged"
  exit 0
fi

fail=0
checked=0
missing=0
n_ref=0
n_port=0
n_refuse=0
while read -r want id kind refbytes why; do
  case "$want" in '#'*|'') continue ;; esac
  # Two-column lines are the original format and mean `ref`. Every one of the
  # reference-recorded cases is still written that way, untouched.
  [ -n "$kind" ] || kind=ref
  checked=$((checked + 1))
  got="$(awk -v k="$id" '$2==k {print $1}' "$results")"
  if [ -z "$got" ]; then
    echo "  MISSING [$id]: the manifest has a case this script no longer runs"
    missing=$((missing + 1))
    continue
  fi
  case "$kind" in
    ref)
      n_ref=$((n_ref + 1))
      if [ "$got" != "$want" ]; then
        echo "  DIFF [$id]: expected $want, got $got"
        echo "      This is a REGRESSION against the Haskell reference. Do not"
        echo "      re-record it from the port. If the change is deliberate, it"
        echo "      becomes a 'port' line with a justification -- see the header."
        fail=$((fail + 1))
      fi
      ;;
    port)
      n_port=$((n_port + 1))
      if [ "$got" = "$refbytes" ]; then
        # The case is back to writing what the reference writes, so whatever
        # this line documents has been undone. Recording the reference's hash
        # alongside ours is what makes this detectable at all.
        echo "  RECONVERGED [$id]: now writes the REFERENCE's bytes ($refbytes)"
        echo "      The deliberate divergence is gone: $why"
        fail=$((fail + 1))
      elif [ "$got" != "$want" ]; then
        echo "  DIFF [$id]: expected $want, got $got"
        echo "      A deliberately divergent case moved. Its recorded reason is:"
        echo "        $why"
        echo "      If this change is intended, update the hash AND the reason."
        fail=$((fail + 1))
      fi
      ;;
    refuse)
      n_refuse=$((n_refuse + 1))
      case "$got" in
        refused) ;;
        accepted)
          echo "  ACCEPTED [$id]: an input that must be refused produced an archive"
          echo "      $why"
          fail=$((fail + 1)) ;;
        crashed)
          echo "  CRASHED [$id]: died on a signal instead of refusing cleanly"
          echo "      This is the reference's behaviour, not an acceptable one:"
          echo "      $why"
          fail=$((fail + 1)) ;;
        *)
          # `silent` -- exited 0 and wrote nothing. Not a refusal, and not a
          # result either; most likely this script invoked the binary wrongly.
          echo "  NOT REFUSED [$id]: outcome was '$got', expected 'refused'"
          echo "      $why"
          fail=$((fail + 1)) ;;
      esac
      ;;
    *)
      echo "  BAD KIND [$id]: '$kind' is not one of ref/port/refuse"
      fail=$((fail + 1)) ;;
  esac
done < "$MANIFEST"

# ...and the other direction: a case added without recording it would otherwise
# pass silently, having been compared against nothing.
#
# Field-aware, not `grep "  $id\$"`: ids stopped being the last field on the
# line when `port` and `refuse` gained a reason, and a trailing-anchor grep
# would have reported every one of them as unrecorded.
while read -r got id; do
  awk -v k="$id" '$2==k {found=1} END {exit !found}' "$MANIFEST" || {
    echo "  UNRECORDED [$id]: this case is not in the manifest (it produced $got)"
    echo "      A 'ref' case is recorded with --record against the reference."
    echo "      A 'port' or 'refuse' case is added BY HAND, with its reason --"
    echo "      deliberately, because only a person can say why it diverges."
    fail=$((fail + 1))
  }
done < "$results"

# The anti-laundering check. Reclassifying a reference-recorded case as a
# deliberate divergence is sometimes right and is always a decision; requiring
# the count to move with it puts that decision in the diff, next to the reason.
# It does not make laundering impossible -- nothing can, when the port is the
# only thing that can produce the new bytes -- it makes it deliberate.
expect_ref="$(awk '/^# ref-cases:/ {print $3}' "$MANIFEST")"
if [ -n "$expect_ref" ] && [ "$n_ref" != "$expect_ref" ]; then
  echo "  REF COUNT [$n_ref, header says $expect_ref]: a case changed provenance."
  echo "      If that is intended, update '# ref-cases:' in the manifest too."
  fail=$((fail + 1))
fi

echo "arc golden: $checked cases ($n_ref reference, $n_port divergent, $n_refuse refusal), $fail failing, $missing missing"
[ "$fail" = 0 ] && [ "$missing" = 0 ] || exit 1
echo "the Rust arc writes the reference's bytes where it should, its own where it must, and refuses what it must"
