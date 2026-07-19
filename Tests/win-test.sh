#!/usr/bin/env bash
# Smoke-test the cross-built Windows binary under Wine.
#
# A PE header only proves the toolchain was wired up correctly. The first
# working cross-build produced a valid PE32+ image that printed its help and
# then failed every single archive operation ("ERR: threadDelay: no clock"),
# so "it builds" and "it works" have to be checked separately.
#
# Usage: Tests/win-test.sh [path-to-exe]     (run from the repo root)
export WINEDEBUG=-all
export WINEPREFIX=${WINEPREFIX:-/tmp/wineprefix}
EXE=${1:-Tests/arc-mhs-win64.exe}

[ -f "$EXE" ] || { echo "error: $EXE not found -- run ./compile-mhs-win64 first" >&2; exit 2; }
command -v wine >/dev/null 2>&1 || { echo "error: wine not found" >&2; exit 2; }

echo "--- wine boot ---"
wine --version 2>&1 | head -1
wineboot -i >/dev/null 2>&1
echo

echo "--- arc --help ---"
wine "$EXE" --help > /tmp/help.txt 2>/tmp/help.err
echo "exit=$?"
grep -qi 'command' /tmp/help.txt && echo "  help output looks right" || { echo "  NO recognisable help:"; head -5 /tmp/help.txt /tmp/help.err; }

echo
echo "--- round-trip a small tree ---"
rm -rf /tmp/wt && mkdir -p /tmp/wt/in/sub /tmp/wt/out
echo "hello windows" > /tmp/wt/in/a.txt
printf 'binary\x00\x01\x02data' > /tmp/wt/in/sub/b.bin
head -c 20000 /dev/urandom > /tmp/wt/in/big.bin

for m in -m0 -m1 -m4; do
  rm -rf /tmp/wt/out; mkdir -p /tmp/wt/out; rm -f /tmp/wt/t.arc
  if ! wine "$EXE" a --nodates -r -y $m /tmp/wt/t.arc /tmp/wt/in >/tmp/wt/c.log 2>&1; then
    echo "  $m: CREATE FAILED"; tail -3 /tmp/wt/c.log | sed 's/^/     /'; continue
  fi
  if ! wine "$EXE" t -y /tmp/wt/t.arc >/tmp/wt/t.log 2>&1; then
    echo "  $m: TEST FAILED"; tail -3 /tmp/wt/t.log | sed 's/^/     /'; continue
  fi
  if ! wine "$EXE" x -y -dp/tmp/wt/out /tmp/wt/t.arc >/tmp/wt/x.log 2>&1; then
    echo "  $m: EXTRACT FAILED"; tail -3 /tmp/wt/x.log | sed 's/^/     /'; continue
  fi
  root=$(find /tmp/wt/out -type d -name in -print -quit)
  if [ -n "$root" ] && diff -r /tmp/wt/in "$root" >/dev/null 2>&1; then
    echo "  $m: round-trip OK ($(wc -c </tmp/wt/t.arc) bytes)"
  else
    echo "  $m: CONTENT MISMATCH (extracted $(find /tmp/wt/out -type f | wc -l) files)"
  fi
done
