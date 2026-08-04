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
  awk 'BEGIN{srand(7); for(j=0;j<40000;j++){s="";for(k=0;k<40;k++) s=s sprintf("%c",65+int(rand()*26)); print s}}' \
    > "$d/big/noise.txt"
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
run big-4x4-lc32m        a --nodates -y -r -m4x4:tor -lc32m -mt1 "$A" .

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
  mkdir -p "$(dirname "$MANIFEST")"
  {
    echo "# Generated by arc-golden-check.sh --record"
    echo "# Reference: $BIN"
    echo "#"
    echo "# These are the Haskell reference's bytes, recorded before it was"
    echo "# deleted. Do NOT regenerate them from the port: that would replace"
    echo "# the thing being checked with the thing doing the checking. If a case"
    echo "# legitimately changes, build the reference from a commit that still"
    echo "# has the Haskell (d6ebeb6 or earlier) and re-record from THAT."
    cat "$results"
  } > "$MANIFEST"
  echo "recorded $(grep -c . "$results") cases to $MANIFEST"
  exit 0
fi

fail=0
checked=0
missing=0
while read -r want id; do
  case "$want" in '#'*|'') continue ;; esac
  checked=$((checked + 1))
  got="$(awk -v k="$id" '$2==k {print $1}' "$results")"
  if [ -z "$got" ]; then
    echo "  MISSING [$id]: the manifest has a case this script no longer runs"
    missing=$((missing + 1))
  elif [ "$got" != "$want" ]; then
    echo "  DIFF [$id]: expected $want, got $got"
    fail=$((fail + 1))
  fi
done < "$MANIFEST"

# ...and the other direction: a case added without recording it would otherwise
# pass silently, having been compared against nothing.
while read -r _got id; do
  grep -q "  $id\$" "$MANIFEST" || {
    echo "  UNRECORDED [$id]: this case is not in the manifest"
    fail=$((fail + 1))
  }
done < "$results"

echo "arc golden: $checked recorded cases, $fail differing, $missing missing"
[ "$fail" = 0 ] && [ "$missing" = 0 ] || exit 1
echo "the Rust arc still writes the bytes the Haskell reference wrote"
