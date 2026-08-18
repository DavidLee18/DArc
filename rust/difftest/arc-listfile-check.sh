#!/usr/bin/env bash
# Differential-test `@listfile` expansion, filespec shapes, `-dp`/`--diskpath`
# and `-sc`/`--charset` against the Haskell reference, BYTE FOR BYTE -- and by
# exit code, which it did not compare until a command that archived nothing was
# found reporting success.
#
#   usage: arc-listfile-check.sh [reference-arc]
#
# `replace_list_files` (Cmdline.hs:778) replaces `@file` with the file's
# non-empty lines, in the 'l' charset, in the FILESPECS and in the values of
# `-n` and `-x`. `linesCRLF` splits on CR, LF or CRLF and strips byte order
# marks.
#
# ── Filespec shapes ────────────────────────────────────────────────────────
#
# A list file's whole output is names, so this exercises every filespec shape
# too — and that is how it was found that the port supported NONE of them except
# a directory. `scan` only ever called read_dir, so `arc a x.arc a.txt` failed
# with ENOTDIR and `arc a x.arc '*.txt'` archived nothing. Every harness before
# this one passed `.`.
#
# ── The reference is NOT an oracle for non-ASCII list files ────────────────
#
# Measured, three ways, on the same input (a UTF-8 BOM followed by `a.txt`):
#
#   Tests/arc-ghc   ERROR: fromUTF: illegal UTF-8 character
#   Tests/arc       accepts it
#   this port       accepts it
#
# `fileGetBinary` under GHC decodes the file through the locale BEFORE
# `utf8_to_unicode` runs, so the BOM arrives as one U+FEFF character, which is
# > 0xFF and falls into `fromUTF'`'s error arm. Under MicroHs the bytes arrive
# raw and decode correctly. The same double decoding makes `-sc0` (identity)
# accidentally WORK on the GHC build — U+FEFF survives to `linesCRLF`, which
# strips it — where MicroHs and this port both treat the three BOM bytes
# literally and find no such file.
#
# So every row below keeps list file contents ASCII. Non-ASCII is checked
# against `Tests/arc` instead, and only when it is present.
#
# The same artifact bites command-line arguments: MicroHs mangles a non-ASCII
# argv entry (`can't open file "cafÃ©.txt"`) while the GHC build rejects it.
# Neither can archive a file whose name is not ASCII; this port can.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"
MHS="$ROOT/Tests/arc"

[ -x "$REF" ] || {
  echo "no reference binary at $REF.

The Haskell reference was deleted; build one from a commit that still has it:
  git worktree add /tmp/darc-ref 9a127e6 && (cd /tmp/darc-ref && ./compile-ghc-probe)
then pass /tmp/darc-ref/Tests/arc-ghc as $1. For a gate that needs no
reference at all, use arc-golden-check.sh" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-listfile-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

mkdir -p "$W/src/sub"
printf 'aaa\n'   > "$W/src/a.txt"
printf 'bbbb\n'  > "$W/src/b.txt"
printf 'ccccc\n' > "$W/src/c.dat"
printf 'dddd\n'  > "$W/src/sub/d.txt"
printf 'eeee\n'  > "$W/src/sub/e.dat"
touch -t 202501010000 $(find "$W/src" -type f) $(find "$W/src" -type d)

printf 'a.txt\nb.txt\n'          > "$W/plain.lst"
printf 'a.txt\r\nsub/d.txt\r\n'  > "$W/crlf.lst"
printf 'a.txt\rb.txt\r'          > "$W/cr.lst"
printf 'a.txt\n\n\nb.txt\n'      > "$W/blanks.lst"
printf 'c.dat\n'                 > "$W/excl.lst"
: > "$W/empty.lst"

# try <options-and-filespecs...> -- compare bytes, and present/absent.
try() {
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -y -m0 "$W/r.arc" "$@" ) </dev/null >/dev/null 2>&1
  local rrc=$?
  ( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/p.arc" "$@" ) </dev/null >/dev/null 2>&1
  local prc=$?
  local r=present p=present
  [ -f "$W/r.arc" ] || r=absent
  [ -f "$W/p.arc" ] || p=absent
  local label; label="$(printf '%s ' "$@" | sed "s|$W/||g")"
  if [ "$r" != "$p" ]; then
    echo "  DIFF [$label]: reference $r, port $p"
    fail=$((fail + 1))
  # The EXIT CODE, which this compared for a long time by not comparing it at
  # all. Present/absent cannot see the difference between "wrote nothing and
  # said so" and "wrote nothing and said All OK", and that gap is exactly what
  # let `a` over a filespec matching nothing report success: every row here
  # whose filespecs select no file agreed on `absent` while the reference
  # exited 1 and the port exited 0.
  elif [ "$rrc" != "$prc" ]; then
    echo "  DIFF [$label]: exit code -- reference $rrc, port $prc"
    fail=$((fail + 1))
  elif [ "$r" = present ] && ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [$label]: archives differ"
    echo "    reference: $("$PORT" l "$W/r.arc" 2>/dev/null | grep -E '^[0-9]{4}' | awk '{print $NF}' | tr '\n' ' ')"
    echo "    port:      $("$PORT" l "$W/p.arc" 2>/dev/null | grep -E '^[0-9]{4}' | awk '{print $NF}' | tr '\n' ' ')"
    fail=$((fail + 1))
  fi
}

# ── filespec shapes, with no list file in sight ─────────────────────────────
try .
try a.txt
try a.txt b.txt
try '*.txt'
try '*.dat'
try 'sub/d.txt'
try 'sub/*.txt'
try -r '*.txt'
try -r '*.dat'
try 'nosuch.txt'
try '*.nosuch'

# ── list files ──────────────────────────────────────────────────────────────
try "@$W/plain.lst"
try "@$W/crlf.lst"
try "@$W/cr.lst"
try "@$W/blanks.lst"
try "@$W/empty.lst"
try "@$W/plain.lst" "@$W/excl.lst"
try . "-x@$W/excl.lst"
try . "-n@$W/plain.lst"
try . "-n@$W/empty.lst"
try "@$W/plain.lst" "-x@$W/excl.lst"
try -r "@$W/crlf.lst"

# ── -dp/--diskpath, which relocates where files are READ ────────────────────
# Untested here at all until a list file and -dp used together were reported to
# archive nothing. -dp leaves the STORED name alone, so `--diskpath=sub d.txt`
# must store `d.txt` and not `sub/d.txt` -- and with a list file it must still
# do so, which is the combination that was reported.
printf 'd.txt\ne.dat\n' > "$W/sub.lst"
try --diskpath=sub d.txt
try --diskpath=sub '*.txt'
try --diskpath=sub "@$W/sub.lst"
try -dpsub "@$W/sub.lst"
try --diskpath=sub/ "@$W/sub.lst"
try --diskpath=. "@$W/plain.lst"
try -r --diskpath=. "@$W/crlf.lst"

# ── -sc, in both spellings ──────────────────────────────────────────────────
try -sc0    "@$W/plain.lst"
try -sc8    "@$W/plain.lst"
try -sc0l   "@$W/plain.lst"
try -sc8l   "@$W/plain.lst"
try -scl0   "@$W/plain.lst"
try -sclutf8 "@$W/plain.lst"
try -scsutf8 "@$W/plain.lst"
try -sc--   "@$W/plain.lst"
try -scz    "@$W/plain.lst"
try -sc0q   "@$W/plain.lst"

echo "arc listfiles: $checked runs, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────

# 1. A list file must SELECT, not just be accepted. `@plain.lst` names two of
#    the five files, so it must differ from `.` in both directions.
rm -f "$W/all.arc" "$W/two.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "$W/all.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/two.arc" "@$W/plain.lst" ) >/dev/null 2>&1
n_all=$("$PORT" l "$W/all.arc" 2>/dev/null | grep -cE '^[0-9]{4}' || true)
n_two=$("$PORT" l "$W/two.arc" 2>/dev/null | grep -cE '^[0-9]{4}' || true)
if [ "$n_two" != 2 ] || [ "$n_all" -le "$n_two" ]; then
  echo "SELF-TEST FAILED: the list file selected $n_two of $n_all entries, not 2" >&2
  exit 1
fi

# 2. `-n@empty.lst` must select NOTHING. The expanded list is empty, and reading
#    that as "no -n given" would archive everything -- the difference between an
#    empty archive and a full one, which is why FileFilter keeps `include_given`
#    separate from `include`.
rm -f "$W/none.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "-n@$W/empty.lst" "$W/none.arc" . ) >/dev/null 2>&1
if [ -f "$W/none.arc" ]; then
  echo "SELF-TEST FAILED: -n@empty.lst produced an archive; an empty list file" >&2
  echo "must select nothing, not everything" >&2
  exit 1
fi

# 3. A missing list file must be an ERROR, not an empty expansion.
if ( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/gone.arc" "@$W/nosuch.lst" ) >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: a missing list file was accepted" >&2
  exit 1
fi

# 4. -dp must actually RELOCATE, and must not leak into the stored name.
#    Both `try --diskpath=sub …` rows above compare present/absent and bytes,
#    so they would agree perfectly if -dp were ignored on both sides and each
#    wrote nothing. Pin the effect itself: the file lives only in sub/, so
#    finding it at all proves -dp was applied, and the name proves it was
#    applied to the READ and not to the name.
rm -f "$W/dp.arc"
( cd "$W/src" && "$PORT" a --nodates -y -m0 --diskpath=sub "$W/dp.arc" d.txt ) >/dev/null 2>&1
got=$("$PORT" l "$W/dp.arc" 2>/dev/null | grep -E '^[0-9]{4}' | awk '{print $NF}')
if [ "$got" != "d.txt" ]; then
  echo "SELF-TEST FAILED: --diskpath=sub d.txt stored [$got], not [d.txt]" >&2
  exit 1
fi

# 5. A filespec that matches NOTHING must be reported, not called success.
#    The archive is erased either way -- that part was always right -- so
#    present/absent cannot see this, and `All OK` with exit 0 over a file that
#    does not exist is what made the report read as "it prints All OK but the
#    archive is not created". Falsified in both directions: the same command
#    over a filespec that DOES match must succeed and say so.
out=$( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/none2.arc" nosuch.txt 2>&1 )
rc=$?
if [ "$rc" = 0 ] || [ -f "$W/none2.arc" ]; then
  echo "SELF-TEST FAILED: a filespec matching nothing exited $rc and left" >&2
  echo "$([ -f "$W/none2.arc" ] && echo 'an archive' || echo 'no archive')" >&2
  exit 1
fi
case "$out" in
  *WARNING*) ;;
  *) echo "SELF-TEST FAILED: nothing matched and no warning was printed: [$out]" >&2
     exit 1 ;;
esac
rm -f "$W/some.arc"
out=$( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/some.arc" a.txt 2>&1 )
rc=$?
if [ "$rc" != 0 ] || [ ! -f "$W/some.arc" ]; then
  echo "SELF-TEST FAILED: a filespec that DOES match exited $rc: [$out]" >&2
  exit 1
fi

# 6. A -sc domain this port does not apply must be refused, not ignored.
if ( cd "$W/src" && "$PORT" a --nodates -y -m0 -scf0 "$W/f.arc" . ) >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: -scf0 was accepted, but this port does not apply the" >&2
  echo "filesystem charset" >&2
  exit 1
fi

# ── non-ASCII: against Tests/arc, which reads list files as raw bytes ───────
# Skipped when the MicroHs build is absent. The GHC probe cannot do this at all
# -- see the header.
if [ -x "$MHS" ]; then
  printf 'x\n' > "$W/src/plain2.txt"
  touch -t 202501010000 "$W/src/plain2.txt"
  printf '\xef\xbb\xbfplain2.txt\n' > "$W/bom.lst"
  rm -f "$W/m.arc" "$W/q.arc"
  ( cd "$W/src" && "$MHS"  a --nodates -y -m0 "$W/m.arc" "@$W/bom.lst" ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -y -m0 "$W/q.arc" "@$W/bom.lst" ) >/dev/null 2>&1
  if ! cmp -s "$W/m.arc" "$W/q.arc"; then
    echo "SELF-TEST FAILED: the port and Tests/arc disagree on a BOM-prefixed" >&2
    echo "UTF-8 list file, which is the case the GHC probe cannot judge" >&2
    exit 1
  fi
  rm -f "$W/src/plain2.txt"
  echo "non-ASCII list files checked against Tests/arc (the GHC probe cannot judge them)"
fi

echo "the Rust arc expands list files and filespecs exactly as the Haskell one does"
