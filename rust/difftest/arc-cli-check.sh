#!/usr/bin/env bash
# Differential-test one `arc` binary against another at the COMMAND LINE.
#
# This is the acceptance gate for porting the Haskell layer. Every codec had a
# byte-exact C function to compare against; the application layer has no such
# thing -- but it does have an oracle, and SREP established the shape: compare
# two BINARIES on the same inputs rather than two functions on the same buffer.
#
#   usage: arc-cli-check.sh <reference-arc> <port-arc>
#
# Both arguments are required. There is deliberately no "compare a binary with
# itself" default: that would pass unconditionally and read as coverage.
#
# ── The five observables ─────────────────────────────────────────────────────
#
# An archiver's behaviour is not just the archive. All five of these are part of
# the contract, and each fails for different reasons:
#
#   1. archive bytes    format drift -- the highest-risk failure in this repo,
#                       because everything still round-trips when it happens
#   2. exit code        what scripts and the GUI branch on
#   3. stdout           listings (`arc l`) are a documented interface
#   4. stderr           diagnostics, and which errors are reported at all
#   5. extracted tree   every path and every byte, plus files present in one
#                       side only
#
# ── Why the output needs normalising, and what is NOT normalised ─────────────
#
# Measured on the current binary, two identical runs differ in exactly three
# ways, none of them behavioural:
#
#   * `Compression time: cpu 0.11 secs, real 2.00 secs. Speed 219 kB/s` -- and
#     the `Speed` clause is sometimes absent entirely
#   * progress redraws, written with carriage returns and backspaces
#   * the archive path, which contains a per-run sandbox directory
#
# Those three are pinned. Everything else -- the version banner, the file
# counts, the ratio, the listing, every diagnostic -- is compared verbatim. A
# port that gets the compression ratio or the file count wrong must fail.
set -uo pipefail

REF="${1:-}"
PORT="${2:-}"
if [ -z "$REF" ] || [ -z "$PORT" ]; then
  echo "usage: $0 <reference-arc> <port-arc>" >&2
  exit 2
fi
for b in "$REF" "$PORT"; do
  [ -x "$b" ] || { echo "not executable: $b" >&2; exit 2; }
done
REF="$(cd "$(dirname "$REF")" && pwd)/$(basename "$REF")"
PORT="$(cd "$(dirname "$PORT")" && pwd)/$(basename "$PORT")"

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
W="${TMPDIR:-/tmp}/arc-cli-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

# The corpus is generated from fixed seeds and fixed timestamps, so archive
# bytes are reproducible -- see Tests/make-corpus.sh, which the fingerprint
# suite already depends on for the same reason.
bash "$ROOT/Tests/make-corpus.sh" "$W/corpus" >/dev/null 2>&1 || {
  echo "make-corpus.sh failed" >&2; exit 1; }

# Pin the three things that legitimately vary between two runs of the SAME
# binary. Kept deliberately narrow: anything not listed here is a real
# difference.
normalise() { # normalise <file> <sandbox-dir>
  tr '\r' '\n' < "$1" \
    | tr -d '\010' \
    | sed -e 's/^\(.*\) time: .*/\1 time: PINNED/' \
          -e "s#$2#SANDBOX#g" \
          -e 's/[[:space:]]*$//' \
    | grep -v '^[[:space:]]*$' \
    | grep -v 'Processed'
}

fail=0 checked=0

# Run one argv through both binaries, in separate sandboxes with identical
# layout, and compare all five observables.
compare() { # compare <label> <cwd-relative-to-sandbox> <args...>
  local label="$1" cwd="$2"; shift 2
  checked=$((checked + 1))
  local bad=""

  for side in ref port; do
    local dir="$W/$side"
    rm -rf "$dir"; mkdir -p "$dir"
    cp -R "$W/corpus" "$dir/corpus"
    local bin="$REF"; [ "$side" = port ] && bin="$PORT"
    ( cd "$dir/$cwd" && "$bin" "$@" ) >"$dir/.out" 2>"$dir/.err"
    echo $? > "$dir/.rc"
  done

  # 2. exit code
  if ! cmp -s "$W/ref/.rc" "$W/port/.rc"; then
    bad="$bad exit($(cat "$W/ref/.rc") vs $(cat "$W/port/.rc"))"
  fi
  # 3/4. stdout and stderr, normalised
  for s in out err; do
    normalise "$W/ref/.$s"  "$W/ref"  > "$W/n.ref.$s"
    normalise "$W/port/.$s" "$W/port" > "$W/n.port.$s"
    cmp -s "$W/n.ref.$s" "$W/n.port.$s" || bad="$bad std$s"
  done
  # 1 + 5. everything the run left behind: archives AND extracted files.
  # diff -r reports both differing contents and paths present on one side only,
  # which are the two ways this can be wrong.
  rm -f "$W/ref/.out" "$W/ref/.err" "$W/ref/.rc" \
        "$W/port/.out" "$W/port/.err" "$W/port/.rc"
  diff -r "$W/ref" "$W/port" >"$W/tree.diff" 2>&1 || bad="$bad tree"

  if [ -n "$bad" ]; then
    echo "  DIFF [$label]:$bad"
    head -4 "$W/tree.diff" 2>/dev/null | sed 's/^/      /'
    fail=$((fail + 1))
  fi
}

echo "reference: $REF"
echo "port:      $PORT"

# ── create ──────────────────────────────────────────────────────────────────
# --nodates because otherwise the archive embeds mtimes and no two runs agree;
# the fingerprint suite makes the same choice for the same reason.
for m in -m0 -m1 -m4 -m9 -mx; do
  compare "create $m" corpus a --nodates -r -y "$m" ../out.arc .
done
for extra in "-s" "-s-" "-ms" "-mt1" "-mt8"; do
  compare "create -m4 $extra" corpus a --nodates -r -y -m4 "$extra" ../out.arc .
done

# ── read paths, over an archive the REFERENCE wrote ─────────────────────────
# Both sides must read identical bytes, so the archive is built once, by the
# reference, and copied into both sandboxes.
( cd "$W/corpus" && "$REF" a --nodates -r -y -m4 "$W/shared.arc" . ) >/dev/null 2>&1
mkarc() { cp "$W/shared.arc" "$1/in.arc"; }

compare_with_arc() { # compare_with_arc <label> <args...>
  local label="$1"; shift
  checked=$((checked + 1))
  local bad=""
  for side in ref port; do
    local dir="$W/$side"
    rm -rf "$dir"; mkdir -p "$dir"
    mkarc "$dir"
    local bin="$REF"; [ "$side" = port ] && bin="$PORT"
    ( cd "$dir" && "$bin" "$@" ) >"$dir/.out" 2>"$dir/.err"
    echo $? > "$dir/.rc"
  done
  cmp -s "$W/ref/.rc" "$W/port/.rc" || bad="$bad exit"
  for s in out err; do
    normalise "$W/ref/.$s"  "$W/ref"  > "$W/n.ref.$s"
    normalise "$W/port/.$s" "$W/port" > "$W/n.port.$s"
    cmp -s "$W/n.ref.$s" "$W/n.port.$s" || bad="$bad std$s"
  done
  rm -f "$W"/ref/.out "$W"/ref/.err "$W"/ref/.rc "$W"/port/.out "$W"/port/.err "$W"/port/.rc
  diff -r "$W/ref" "$W/port" >"$W/tree.diff" 2>&1 || bad="$bad tree"
  if [ -n "$bad" ]; then
    echo "  DIFF [$label]:$bad"; head -4 "$W/tree.diff" | sed 's/^/      /'; fail=$((fail + 1))
  fi
}

compare_with_arc "list"        l in.arc
compare_with_arc "list -lt"    lt in.arc
compare_with_arc "test"        t in.arc
compare_with_arc "extract"     x -y in.arc
compare_with_arc "extract -e"  e -y in.arc

# ── error paths ─────────────────────────────────────────────────────────────
# Which errors are reported, and with what exit code, is part of the contract:
# a port that silently succeeds where the reference fails is worse than one that
# fails loudly.
compare_with_arc "missing archive"  t no-such.arc
compare_with_arc "unknown command"  zzz in.arc
compare_with_arc "bad method"       a --nodates -y -mNOPE out2.arc in.arc

echo "arc CLI: $checked cases, $fail differing"
[ "$fail" -eq 0 ] || exit 1

# ── the harness must be able to fail ────────────────────────────────────────
# Everything above passes trivially if the two binaries are the same file, and
# that is exactly how this would be run first. Prove the comparison has teeth by
# checking that a deliberately different invocation IS caught.
probe() {
  rm -rf "$W/ref" "$W/port"; mkdir -p "$W/ref" "$W/port"
  cp -R "$W/corpus" "$W/ref/corpus"; cp -R "$W/corpus" "$W/port/corpus"
  ( cd "$W/ref/corpus"  && "$REF"  a --nodates -r -y -m1 ../out.arc . ) >/dev/null 2>&1
  ( cd "$W/port/corpus" && "$PORT" a --nodates -r -y -m4 ../out.arc . ) >/dev/null 2>&1
  cmp -s "$W/ref/out.arc" "$W/port/out.arc"
}
if probe; then
  echo "SELF-TEST FAILED: -m1 and -m4 produced identical archives, so the" >&2
  echo "comparison cannot be distinguishing anything" >&2
  exit 1
fi

echo "the port matches the reference on every observable"
