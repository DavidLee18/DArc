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
#   * NO `grzip` and NO `4x4`. They are the only two methods whose memory
#     formulas read the processor count (`compression_threads`, memlimit.rs:288,
#     which is GetCompressionThreads), so they are the only two that could make
#     a recorded hash depend on the machine that recorded it.
#   * `-rr` in absolute bytes, never a percentage. A percentage is taken of the
#     archive size and then clamped -- in the C, at getPhysicalMemory/2.
#
# An earlier version of this list also claimed `-mt1` pinned the thread count.
# It does not: `-mt1` is a -m VALUE naming a method modifier, not a thread
# option, and a later `-m` simply replaces it. Dropping it changed no hash,
# which is the proof it was doing nothing.
#
# A case that violates one of those will record fine and fail on another
# machine, which reads as a format regression and is not one. Add cases in that
# style.
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
}

results="$W/results.txt"
: > "$results"

# run <case-id> <command...>  -- each `%%` starts another invocation, all on the
# same tree and the same archive, so update and copy chains can be expressed.
run() {
  local id="$1"; shift
  local tree="$W/t"
  build_tree "$tree"
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
run m-lzma-solid-off     a --nodates -y -mt1 -r -s- -mlzma:1m:normal "$A" .
run m-lzma-rr            a --nodates -y -mt1 -r -rr4096b -mlzma:1m:normal "$A" .

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
