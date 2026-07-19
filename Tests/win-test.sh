#!/usr/bin/env bash
# Smoke-test the cross-built Windows binary under Wine.
#
# A PE header only proves the toolchain was wired up correctly. The first
# working cross-build produced a valid PE32+ image that printed its help and
# then failed every single archive operation ("ERR: threadDelay: no clock"),
# so "it builds" and "it works" have to be checked separately.
#
# Usage: Tests/win-test.sh [path-to-exe]     (run from the repo root)
#
# This script must exit non-zero when anything fails. The first version printed
# "CREATE FAILED" three times and still exited 0, so the CI job went green on a
# binary that could not create a single archive -- the same class of defect this
# branch exists to fix, reintroduced in the check itself.
set -uo pipefail

EXE=${1:-Tests/arc-mhs-win64.exe}

# Wine refuses to create its configuration directory when the parent is not
# owned by the current user, which is the case for /tmp on the CI runners:
#   wine: '/tmp' is not owned by you, refusing to create a configuration directory there
# Keep the prefix under HOME, which is always ours.
export WINEDEBUG=${WINEDEBUG:--all}
export WINEPREFIX=${WINEPREFIX:-$HOME/.darc-wineprefix}

WORK=${WORK:-${TMPDIR:-/tmp}/darc-win-test.$$}

[ -f "$EXE" ] || { echo "error: $EXE not found -- run ./compile-mhs-win64 first" >&2; exit 2; }
command -v wine >/dev/null 2>&1 || { echo "error: wine not found in PATH" >&2; exit 2; }

fail=0
note_fail () { echo "  $*"; fail=$((fail+1)); }

echo "--- wine ---"
wine --version 2>&1 | head -1
wineboot -i >/dev/null 2>&1 || true      # first run initialises the prefix
echo

echo "--- arc --help ---"
if wine "$EXE" --help > "$WORK.help" 2> "$WORK.help.err"; then
  if grep -qi 'command' "$WORK.help"; then
    echo "  help output looks right"
  else
    note_fail "no recognisable help output"
    head -5 "$WORK.help" "$WORK.help.err" | sed 's/^/     /'
  fi
else
  note_fail "--help exited $?"
  head -5 "$WORK.help" "$WORK.help.err" | sed 's/^/     /'
fi
echo

echo "--- round-trip a small tree ---"
rm -rf "$WORK"; mkdir -p "$WORK/in/sub"
echo "hello windows"            > "$WORK/in/a.txt"
printf 'binary\x00\x01\x02data' > "$WORK/in/sub/b.bin"
head -c 20000 /dev/urandom      > "$WORK/in/big.bin"

for m in -m0 -m1 -m4; do
  out="$WORK/out$m"
  rm -rf "$out"; mkdir -p "$out"; rm -f "$WORK/t.arc"

  if ! wine "$EXE" a --nodates -r -y $m "$WORK/t.arc" "$WORK/in" > "$WORK/c.log" 2>&1; then
    note_fail "$m: create failed"; tail -3 "$WORK/c.log" | sed 's/^/     /'; continue
  fi
  if ! wine "$EXE" t -y "$WORK/t.arc" > "$WORK/t.log" 2>&1; then
    note_fail "$m: integrity test failed"; tail -3 "$WORK/t.log" | sed 's/^/     /'; continue
  fi
  if ! wine "$EXE" x -y -dp"$out" "$WORK/t.arc" > "$WORK/x.log" 2>&1; then
    note_fail "$m: extract failed"; tail -3 "$WORK/x.log" | sed 's/^/     /'; continue
  fi

  # Archives store paths with the leading separator stripped, so the extracted
  # tree lands at a known place. Derived rather than searched for by name:
  # "find -name in -print -quit" picks whichever entry readdir yields first,
  # which is what made the main suite report phantom empty directories.
  root="$out/${WORK#/}/in"
  if [ ! -d "$root" ]; then
    note_fail "$m: no extracted tree at $root"
    find "$out" -maxdepth 4 -type d | head -5 | sed 's/^/     /'
    continue
  fi
  if diff -r "$WORK/in" "$root" >/dev/null 2>&1; then
    echo "  $m: round-trip OK ($(wc -c < "$WORK/t.arc" | tr -d ' ') bytes)"
  else
    note_fail "$m: content mismatch"
    diff -rq "$WORK/in" "$root" 2>&1 | head -3 | sed 's/^/     /'
  fi
done

echo
if [ "$fail" -eq 0 ]; then
  echo "windows smoke test: all checks passed"
  rm -rf "$WORK" "$WORK".help "$WORK".help.err
  exit 0
fi
echo "windows smoke test: $fail check(s) failed" >&2
exit 1
