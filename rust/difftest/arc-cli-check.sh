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
# ── This compares BEHAVIOUR, not wording ────────────────────────────────────
#
# It used to compare stdout and stderr verbatim, and by 2026-08 it was failing
# **18 of 18 cases** -- unnoticed, because CI ran no arc-*-check.sh but
# arc-golden-check. Every one of those failures was wording or verbosity, not
# behaviour. Measured on the same inputs:
#
#   * the reference prints a version banner on every command; the port prints
#     none
#   * on create, the reference prints `Compressing 226 files...`, a progress
#     redraw, `Compressed ... Ratio 100.0%` and a timing line; the port prints
#     only `All OK` -- while writing a BYTE-IDENTICAL archive
#   * both print an `Extracted N files` summary, in different shapes -- and the
#     reference's count is of ENTRIES, so it says 226 on a tree of 218 files and
#     8 directories. The port said the same until #140. (That commit claimed the
#     reference printed no such line; it does, and the line was missed by
#     tailing output whose progress is drawn with carriage returns.)
#   * diagnostics go to stdout on the reference and to stderr on the port, and
#     the sentences differ
#
# `CLAUDE.md` settled which of those matter: DArc is not a drop-in replacement,
# and message-identity is the lowest-priority property here. A harness that
# fails on all of it reports nothing and gets ignored -- which is exactly what
# happened.
#
# So the observables are now:
#
#   1. archive bytes    identical. Format drift is still the highest-risk
#                       failure here, and this is unchanged.
#   2. extracted tree   identical -- every path and byte, plus files present on
#                       one side only.
#   3. exit code        identical. This is what scripts branch on, and the two
#                       builds already agree on it everywhere, including the
#                       error paths.
#   4. LISTING DATA     identical. `arc l` is a documented interface, and its
#                       rows and totals are its content. The table format
#                       already matches; the banner does not, so the banner
#                       goes and the rows stay.
#   5. failure reporting  a non-zero exit must come with a diagnostic on ONE of
#                       the two streams. Which stream, and in what words, is not
#                       compared; staying silent about a failure is.
#
# What is deliberately NOT compared: the banner, progress redraws, timing, the
# create/extract summary lines, and the wording of any diagnostic. A port that
# lists the wrong files, writes different bytes, extracts a different tree, or
# fails silently still fails.
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

# The data-bearing lines of a run, from BOTH streams together.
#
# Stream-agnostic on purpose: the reference puts diagnostics on stdout and the
# port puts them on stderr, and neither is more correct.
#
# **A WHITELIST, not a blacklist, and that is the whole point.** The first
# version of this listed the reporting lines to drop -- banner, progress,
# timing, the create/extract summaries -- and it failed intermittently: one run
# in four flagged a different case each time, and five isolated repeats of the
# failing command never reproduced it. That is what a blacklist does. The
# reference draws progress with carriage returns, so a flush can land mid-line
# and produce a fragment that begins with none of the prefixes being filtered;
# no list of things to drop is ever closed.
#
# A list of things to KEEP is closed. Three shapes carry data:
#
#   * a listing row, which begins with its ISO date
#   * the totals line, `226 files, 438.744 bytes, 0 compressed`
#   * the final status, `All OK`
#
# Anything else is reporting and is ignored. The substance -- archive bytes, the
# extracted tree, the exit code -- is checked separately and verbatim, so this
# only has to cover the listing interface.
data_only() { # data_only <stdout> <stderr> <sandbox-dir>
  cat "$1" "$2" \
    | tr '\r' '\n' \
    | tr -d '\010' \
    | sed -e "s#$3#SANDBOX#g" -e 's/[[:space:]]*$//' \
    | grep -E '^[0-9]{4}-[0-9]{2}-[0-9]{2} |^[0-9][0-9.,]* files, |^All OK$'
}

# Did this side say anything at all about a failure?
#
# The wording differs and the stream differs, so neither is compared -- but a
# build that exits non-zero while printing nothing has told the user nothing,
# and that IS behaviour. Anything beyond the banner counts.
reported_something() { # reported_something <stdout> <stderr>
  cat "$1" "$2" | tr '\r' '\n' | tr -d '\010' \
    | grep -Ev '^DArc [0-9]' | grep -qE '[^[:space:]]'
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

  # 3. exit code
  if ! cmp -s "$W/ref/.rc" "$W/port/.rc"; then
    bad="$bad exit($(cat "$W/ref/.rc") vs $(cat "$W/port/.rc"))"
  fi
  # 4. the data-bearing output, both streams together
  data_only "$W/ref/.out"  "$W/ref/.err"  "$W/ref"  >| "$W/n.ref"
  data_only "$W/port/.out" "$W/port/.err" "$W/port" >| "$W/n.port"
  if ! cmp -s "$W/n.ref" "$W/n.port"; then
    bad="$bad data"
    diff "$W/n.ref" "$W/n.port" >| "$W/data.diff" 2>&1
  else
    : >| "$W/data.diff"
  fi
  # 5. a failure must be reported, on either stream, in any words
  if [ "$(cat "$W/ref/.rc")" != 0 ]; then
    reported_something "$W/ref/.out"  "$W/ref/.err"  || bad="$bad ref-silent-failure"
    reported_something "$W/port/.out" "$W/port/.err" || bad="$bad port-silent-failure"
  fi
  # 1 + 5. everything the run left behind: archives AND extracted files.
  # diff -r reports both differing contents and paths present on one side only,
  # which are the two ways this can be wrong.
  rm -f "$W/ref/.out" "$W/ref/.err" "$W/ref/.rc" \
        "$W/port/.out" "$W/port/.err" "$W/port/.rc"
  diff -r "$W/ref" "$W/port" >"$W/tree.diff" 2>&1 || bad="$bad tree"

  if [ -n "$bad" ]; then
    echo "  DIFF [$label]:$bad"
    head -6 "$W/data.diff" 2>/dev/null | sed "s|^|      data: |"
    head -4 "$W/tree.diff" 2>/dev/null | sed "s|^|      |"
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
  data_only "$W/ref/.out"  "$W/ref/.err"  "$W/ref"  >| "$W/n.ref"
  data_only "$W/port/.out" "$W/port/.err" "$W/port" >| "$W/n.port"
  if ! cmp -s "$W/n.ref" "$W/n.port"; then
    bad="$bad data"
    diff "$W/n.ref" "$W/n.port" >| "$W/data.diff" 2>&1
  else
    : >| "$W/data.diff"
  fi
  if [ "$(cat "$W/ref/.rc")" != 0 ]; then
    reported_something "$W/ref/.out"  "$W/ref/.err"  || bad="$bad ref-silent-failure"
    reported_something "$W/port/.out" "$W/port/.err" || bad="$bad port-silent-failure"
  fi
  rm -f "$W"/ref/.out "$W"/ref/.err "$W"/ref/.rc "$W"/port/.out "$W"/port/.err "$W"/port/.rc
  diff -r "$W/ref" "$W/port" >"$W/tree.diff" 2>&1 || bad="$bad tree"
  if [ -n "$bad" ]; then
    echo "  DIFF [$label]:$bad"
    head -6 "$W/data.diff" 2>/dev/null | sed "s|^|      data: |"
    head -4 "$W/tree.diff" | sed "s|^|      |"
    fail=$((fail + 1))
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

# ── the summaries must be TRUE ──────────────────────────────────────────────
#
# Everything above compares the two builds. That deliberately says nothing about
# the summary lines, because they differ by design -- the reference prints a
# create summary the port does not, the port prints an `Extracted` line the
# reference does not, and `data_only` filters both.
#
# Which left a hole exactly the size of the bug fixed in #140: `Extracted 1
# files, 6 bytes` printed directly under `ERROR: refusing unsafe path`, because
# the count came from the entries the FILTERS selected rather than the files
# written. Nothing compared it against anything, so nothing objected.
#
# These are not differential checks and could not be. They hold ONE binary's
# output against ground truth: what is on the disk, and what its own listing
# says. Each is applied to whichever binary prints the line, so the reference
# exercises them too rather than the port grading itself.
digits() { tr -d '.,' ; }   # `438.744` is 438744; the separator is cosmetic

# truthful <binary> <label> <strict>
#
# Every summary this binary prints must be true. `strict=0` reports a mismatch
# without failing the run, which is how the REFERENCE is checked: it prints
#
#     Extracted 226 files, 33.111 => 438.744 bytes. Ratio 7.5%
#
# on a tree of 218 files and 8 directories -- entries, not files written, the
# same miscount the port carried until #140. That is a real finding and worth
# printing, but this harness gates the PORT; failing it because the reference is
# untruthful would be red for something no change here can fix.
#
# (#140 justified its fix by claiming the reference printed no such line. It
# does. The line was missed by tailing output whose progress is drawn with
# carriage returns, and the fix is a deliberate divergence rather than a free
# one -- see the site in darc.rs.)
truthful() {
  local bin="$1" label="$2" strict="${3:-1}"
  local d="$W/truth.$label"
  rm -rf "$d"; mkdir -p "$d/out"
  cp "$W/shared.arc" "$d/in.arc"

  # 1. `Extracted N files, M bytes` against what actually landed.
  ( cd "$d/out" && "$bin" x -y ../in.arc ) >"$d/x.out" 2>"$d/x.err"
  local line
  line=$(tr '\r' '\n' < "$d/x.out" | tr -d '\010' | grep -E '^Extracted ' | head -1)
  if [ -n "$line" ]; then
    local said_n said_b real_n real_b
    said_n=$(echo "$line" | sed -E 's/^Extracted ([0-9.,]+) files.*/\1/' | digits)
    said_b=$(echo "$line" | sed -E 's/.* ([0-9.,]+) bytes.*/\1/' | digits)
    real_n=$(find "$d/out" -type f | wc -l | tr -d '[:space:]')
    real_b=$(find "$d/out" -type f -exec wc -c {} + | tail -1 | awk '{print $1}')
    if [ "$said_n" != "$real_n" ] || [ "$said_b" != "$real_b" ]; then
      echo "  $([ "$strict" = 1 ] && echo UNTRUE || echo "NOTE (not gated)") [$label extract]: said \"$line\" but wrote $real_n files, $real_b bytes"
      [ "$strict" = 1 ] && fail=$((fail + 1))
    fi
    checked=$((checked + 1))
  fi

  # 2. The listing's own totals against its own rows.
  "$bin" l "$d/in.arc" >"$d/l.out" 2>/dev/null
  local rows tot_n
  rows=$(tr '\r' '\n' < "$d/l.out" | grep -cE '^[0-9]{4}-[0-9]{2}-[0-9]{2} ')
  tot_n=$(tr '\r' '\n' < "$d/l.out" | grep -oE '^[0-9][0-9.,]* files,' | head -1 | sed -E 's/ files,//' | digits)
  if [ -z "$tot_n" ] || [ "$rows" -eq 0 ]; then
    # A parse that matches nothing must not read as agreement -- that is the
    # vacuous pass this whole file keeps rediscovering.
    echo "  UNPARSED [$label list]: $rows rows, totals '$tot_n' -- the check found nothing to compare"
    fail=$((fail + 1))
  elif [ "$tot_n" != "$rows" ]; then
    echo "  UNTRUE [$label list]: totals say $tot_n files, the table has $rows rows"
    fail=$((fail + 1))
  fi
  checked=$((checked + 1))

  # 3. `Tested N files, A => B bytes` against that same listing.
  "$bin" t "$d/in.arc" >"$d/t.out" 2>/dev/null
  line=$(tr '\r' '\n' < "$d/t.out" | tr -d '\010' | grep -E '^Tested ' | head -1)
  if [ -n "$line" ]; then
    local t_n t_b l_b
    t_n=$(echo "$line" | sed -E 's/^Tested ([0-9.,]+) files.*/\1/' | digits)
    t_b=$(echo "$line" | sed -E 's/.*=> ([0-9.,]+) bytes.*/\1/' | digits)
    l_b=$(tr '\r' '\n' < "$d/l.out" | grep -oE 'files, [0-9][0-9.,]* bytes' | head -1 \
          | sed -E 's/files, //; s/ bytes//' | digits)
    if [ "$t_n" != "$rows" ] || { [ -n "$l_b" ] && [ "$t_b" != "$l_b" ]; }; then
      echo "  UNTRUE [$label test]: said \"$line\" against a listing of $rows files, $l_b bytes"
      fail=$((fail + 1))
    fi
    checked=$((checked + 1))
  fi
}

truthful "$PORT" port 1   # the port is gated
truthful "$REF"  ref  0   # the reference is reported, not gated -- see above

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

# ── and the DATA comparison specifically must have teeth ────────────────────
#
# The check above proves the tree comparison works. It says nothing about
# `data_only`, which is the part that was just loosened -- and a filter one line
# too greedy would silently reduce it to comparing "All OK" with "All OK".
#
# So: list two archives that genuinely hold different files, and require the
# reduced output to differ. If this passes, the listing rows are still being
# compared; if it does not, the filters have eaten the content.
rm -rf "$W/probe"; mkdir -p "$W/probe/a" "$W/probe/b"
printf 'one\n' > "$W/probe/a/only-in-a.txt"
printf 'two\n' > "$W/probe/b/only-in-b.txt"
( cd "$W/probe/a" && "$PORT" a --nodates -y -m0 "$W/probe/a.arc" . ) >/dev/null 2>&1
( cd "$W/probe/b" && "$PORT" a --nodates -y -m0 "$W/probe/b.arc" . ) >/dev/null 2>&1
"$PORT" l "$W/probe/a.arc" >| "$W/probe/a.out" 2>| "$W/probe/a.err"
"$PORT" l "$W/probe/b.arc" >| "$W/probe/b.out" 2>| "$W/probe/b.err"
data_only "$W/probe/a.out" "$W/probe/a.err" "$W/probe" >| "$W/probe/a.data"
data_only "$W/probe/b.out" "$W/probe/b.err" "$W/probe" >| "$W/probe/b.data"
if [ ! -s "$W/probe/a.data" ]; then
  echo "SELF-TEST FAILED: data_only reduced a listing to nothing, so the" >&2
  echo "comparison would pass on any two archives" >&2
  exit 1
fi
if cmp -s "$W/probe/a.data" "$W/probe/b.data"; then
  echo "SELF-TEST FAILED: two archives with different contents reduced to the" >&2
  echo "same data, so the listing rows are no longer being compared" >&2
  exit 1
fi

echo "the port matches the reference on every observable"
