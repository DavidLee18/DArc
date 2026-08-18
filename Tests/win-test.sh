#!/usr/bin/env bash
# Smoke-test a Windows binary, either under Wine on a Unix host or natively on
# Windows itself.
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

EXE=${1:-Tests/darc-win64.exe}

[ -f "$EXE" ] || { echo "error: $EXE not found -- run ./compile-mhs-win64 first" >&2; exit 2; }

# Everything below runs from a scratch directory using *relative* paths. That is
# not tidiness: MSYS/Git-Bash rewrites POSIX-looking arguments into Windows
# paths before handing them to a native .exe, and "-dp/tmp/whatever" is exactly
# the shape that mangling trips over. Relative arguments pass through untouched,
# so one command line works under both Wine and Windows -- and the extracted
# tree then lands somewhere predictable instead of at a location derived from
# the absolute path of the source tree.
case "$EXE" in
  /*) ;;
  *)  EXE="$PWD/$EXE" ;;
esac

# On Windows the binary is native and runs directly; anywhere else it needs Wine.
case "${OSTYPE:-}" in
  msys*|cygwin*|win32*) WINE="" ;;
  *)                    WINE="wine" ;;
esac

if [ -n "$WINE" ]; then
  command -v "$WINE" >/dev/null 2>&1 || { echo "error: wine not found in PATH" >&2; exit 2; }
  # Wine refuses to create its configuration directory when the parent is not
  # owned by the current user, which is the case for /tmp on the CI runners:
  #   wine: '/tmp' is not owned by you, refusing to create a configuration directory there
  # Keep the prefix under HOME, which is always ours.
  export WINEDEBUG=${WINEDEBUG:--all}
  export WINEPREFIX=${WINEPREFIX:-$HOME/.darc-wineprefix}
fi

# Invoke the binary the way this host requires. A function rather than an array
# because macOS ships bash 3.2, where expanding an *empty* array under `set -u`
# is an "unbound variable" error.
run_arc () {
  if [ -n "$WINE" ]; then "$WINE" "$EXE" "$@"; else "$EXE" "$@"; fi
}

WORK=${WORK:-${TMPDIR:-/tmp}/darc-win-test.$$}

fail=0
note_fail () { echo "  $*"; fail=$((fail+1)); }

echo "--- host ---"
if [ -n "$WINE" ]; then
  wine --version 2>&1 | head -1
  wineboot -i >/dev/null 2>&1 || true      # first run initialises the prefix
else
  echo "native Windows"
fi

# Which architecture is under test, read out of the PE header rather than
# guessed from the environment.
#
# The obvious-looking $PROCESSOR_ARCHITECTURE is wrong here: it is per-process,
# and Git Bash is an x64 binary that runs emulated on an ARM64 host, so it
# reports "AMD64" on an ARM64 runner. PROCESSOR_ARCHITEW6432 does not rescue
# it either -- that one is specific to 32-bit-on-64-bit WOW64 and is unset
# under x64-on-ARM64 emulation. And what the reader wants is the architecture
# of the *binary* anyway, which is the thing being claimed.
#
# `file` would report it, but it is not installed everywhere -- which is the
# same reason the MZ check in compile-mhs-win64 is open-coded. Little-endian
# fields assembled a byte at a time, so this does not depend on the byte order
# of the host either. e_lfanew sits at 0x3c; the machine word is 4 bytes past
# the "PE\0\0" signature it points at.
read_le () {  # read_le <byte-offset> <length> -> decimal value
  local byte value=0 shift_by=0
  for byte in $(od -An -tu1 -j "$1" -N "$2" "$EXE"); do
    value=$(( value + byte * (1 << shift_by) )); shift_by=$(( shift_by + 8 ))
  done
  echo "$value"
}
case "$(printf '0x%04x' "$(read_le $(( $(read_le 60 4) + 4 )) 2)")" in
  0x8664) arch_name="x86-64" ;;
  0xaa64) arch_name="ARM64"  ;;
  *)      arch_name="unrecognised PE machine" ;;
esac
echo "exe: $EXE ($arch_name)"
echo

rm -rf "$WORK"; mkdir -p "$WORK/in/sub"
cd "$WORK" || { echo "error: cannot enter $WORK" >&2; exit 2; }

echo "--- arc --help ---"
if run_arc --help > help.out 2> help.err; then
  if grep -qi 'command' help.out; then
    echo "  help output looks right"
  else
    note_fail "no recognisable help output"
    head -5 help.out help.err | sed 's/^/     /'
  fi
else
  note_fail "--help exited $?"
  head -5 help.out help.err | sed 's/^/     /'
fi
echo

echo "--- round-trip a small tree ---"
echo "hello windows"            > in/a.txt
printf 'binary\x00\x01\x02data' > in/sub/b.bin
head -c 20000 /dev/urandom      > in/big.bin

for m in -m0 -m1 -m4; do
  out="out$m"
  rm -rf "$out"; mkdir -p "$out"; rm -f t.arc

  if ! run_arc a --nodates -r -y $m t.arc in > c.log 2>&1; then
    note_fail "$m: create failed"; tail -3 c.log | sed 's/^/     /'; continue
  fi
  if ! run_arc t -y t.arc > t.log 2>&1; then
    note_fail "$m: integrity test failed"; tail -3 t.log | sed 's/^/     /'; continue
  fi
  if ! run_arc x -y -dp"$out" t.arc > x.log 2>&1; then
    note_fail "$m: extract failed"; tail -3 x.log | sed 's/^/     /'; continue
  fi

  # A relative input path means the archive stores "in/...", so the extracted
  # tree is at a known place. Derived rather than searched for by name: an
  # earlier "find -name in -print -quit" picked whichever entry readdir yielded
  # first, which is what made the main suite report phantom empty directories.
  if [ ! -d "$out/in" ]; then
    note_fail "$m: no extracted tree at $out/in"
    find "$out" -maxdepth 4 -type d | head -5 | sed 's/^/     /'
    continue
  fi
  if diff -r in "$out/in" >/dev/null 2>&1; then
    echo "  $m: round-trip OK ($(wc -c < t.arc | tr -d ' ') bytes)"
  else
    note_fail "$m: content mismatch"
    diff -rq in "$out/in" 2>&1 | head -3 | sed 's/^/     /'
  fi
done

echo
echo "--- a list file written on Windows (issue #160) ---"
# The reported case, on a real Windows kernel. `\` is a path separator here and
# is what both the shell and a locally-authored list file produce, so a filespec
# spelled with it has to resolve -- it did not, and the run wrote no archive
# while printing "All OK".
#
# This belongs HERE rather than in rust/difftest: the fix is Windows-only by
# design (`\` is a legal character in a POSIX file name, so the translation is
# the identity there), which means no harness on Linux or macOS can execute the
# branch that matters. Only a Windows binary can, and this script is the one
# thing that runs one -- under Wine on Linux and natively on ARM64 Windows both.
#
# The backslash is passed through `%s`, where printf does no escape processing.
# Written as `'0007\\0.paz'` it reads as an octal escape waiting to happen, and
# the point of the test is lost the moment the byte is not the byte.
BS='\'
mkdir -p lst/0007 lst/bin64
printf 'paz payload\n' > lst/0007/0.paz
printf 'exe payload\n' > lst/bin64/prog.exe
# CRLF, because a list file written on Windows has them.
printf '0007%s0.paz\r\nbin64%sprog.exe\r\n' "$BS" "$BS" > win.lst
printf '0007/0.paz\nbin64/prog.exe\n'                   > posix.lst

rm -f wsep.arc psep.arc
if ! run_arc a --nodates -y --diskpath=lst wsep.arc @win.lst > w.log 2>&1; then
  note_fail "backslash list file: create failed"; tail -3 w.log | sed 's/^/     /'
elif [ ! -f wsep.arc ]; then
  # The reported symptom exactly: exit 0, "All OK", and no archive.
  note_fail "backslash list file: no archive written (issue #160)"
  tail -3 w.log | sed 's/^/     /'
else
  # Both spellings must name the same files. Compared as ARCHIVES rather than
  # as listings, so a difference in what was stored cannot hide behind a
  # difference in how it is printed.
  run_arc a --nodates -y --diskpath=lst psep.arc @posix.lst > p.log 2>&1
  if cmp -s wsep.arc psep.arc; then
    echo "  a Windows-spelled list file archives the same bytes as a POSIX-spelled one"
  else
    note_fail "backslash and forward-slash list files produced different archives"
  fi
  # ...and the files really came back, with the relative names preserved.
  rm -rf outlst; mkdir -p outlst
  if run_arc x -y -dpoutlst wsep.arc > xl.log 2>&1 &&
     [ -f outlst/0007/0.paz ] && [ -f outlst/bin64/prog.exe ]; then
    echo "  0007/0.paz and bin64/prog.exe extracted under their stored names"
  else
    note_fail "backslash list file: extracted tree is wrong"
    find outlst -type f | head -5 | sed 's/^/     /'
  fi
fi

# The other half of #160: matching nothing must be REPORTED. An empty archive is
# erased either way, so the only observable is the exit code and the warning --
# which is precisely why this went unnoticed as `All OK`.
rm -f none.arc
if run_arc a --nodates -y none.arc nosuchfile.txt > n.log 2>&1; then
  note_fail "a filespec matching nothing exited 0 (issue #160)"
elif [ -f none.arc ]; then
  note_fail "a filespec matching nothing left an archive behind"
else
  echo "  a filespec matching nothing is reported, and leaves no archive"
fi
echo

if [ "$fail" -eq 0 ]; then
  echo "windows smoke test: all checks passed"
  cd /; rm -rf "$WORK"
  exit 0
fi
echo "windows smoke test: $fail check(s) failed" >&2
exit 1
