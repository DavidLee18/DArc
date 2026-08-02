#!/usr/bin/env bash
# Differential-test the Rust file-selection filters against the Haskell ones:
# `-n` (include), `-x` (exclude), `--fullnames`, `--dirs`/`--nodirs`, and the
# filespecs on the READ commands.
#
#   usage: arc-filter-check.sh [reference-arc]
#
# ── One rule, three places ─────────────────────────────────────────────────
#
# DArc has a single file-selection predicate, `opt_file_filter` (Cmdline.hs:493),
# and reuses it. What changes per command is only whether the filespecs are
# ANDed in and whether the result is negated (Arc.hs:243-272):
#
#   a u f m        the disk scan            filter alone
#   ch c k t l v   archive selection        filespecs AND filter
#   d              archive selection        NOT (filespecs AND filter)
#   a u f j        archive selection        const True -- unfiltered
#
# The last row is the surprising one and is checked below: for a/u/f/j the
# filespecs select DISK files, so the archive's own entries survive whatever
# they are named.
#
# ── Directories never go through the name filter ───────────────────────────
#
# Both sides short-circuit them, and they agree: `test_dirs` (Arc.hs:270) when
# reading, `accept_f` (FileInfo.hs:462) when writing. A directory is decided by
# --dirs/--nodirs, or failing that by whether any n/s/t filter exists -- never
# by matching its name against -n or -x.
#
# `-x` is NOT an n/s/t filter (Cmdline.hs:498 lists only -n and the size/time
# ones), which is why `arc a -x*.dat` keeps the `sub` entry and `arc a -n*.txt`
# drops it. Reading those two outcomes as "directories are filtered by name"
# fits both and is wrong; `--dirs -n*.txt` is what separates the two readings,
# so it has a row of its own.
#
# ── The one divergence ─────────────────────────────────────────────────────
#
# Under `--dirs` the reference writes the top-level directory of each filespec
# TWICE. This port writes it once. Those rows therefore compare deduplicated
# NAME LISTS rather than bytes; every other row is byte-identity. See
# `filter::write_dirs`.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

[ -x "$REF" ] || {
  echo "no reference binary at $REF -- build one with ./compile-ghc-probe" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-filter-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub/deeper"
  printf 'a\n' > "$d/a.txt"
  printf 'b\n' > "$d/b.dat"
  printf 'n\n' > "$d/sub/n.txt"
  printf 'p\n' > "$d/sub/deep.dat"
  printf 'q\n' > "$d/sub/deeper/q.txt"
  touch -t 202501010000 "$d/a.txt" "$d/b.dat" "$d/sub/n.txt" \
        "$d/sub/deep.dat" "$d/sub/deeper/q.txt"
}
build_tree "$W/src"

# names <binary> <archive> -- the entry names, one per line.
names() { "$1" l "$2" 2>/dev/null | grep -E '^[0-9]{4}-' | awk '{print $NF}'; }

# create <label> <options...> -- both binaries create, compare BYTES.
create() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -r -y -m0 "$@" "$W/r.arc" . ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "$@" "$W/p.arc" . ) >/dev/null 2>&1
  # A filter that matches nothing leaves NO archive: an empty one is removed
  # rather than written. Both sides absent is a match, not a `cmp` failure on
  # two missing files.
  local r=present p=present
  [ -f "$W/r.arc" ] || r=gone
  [ -f "$W/p.arc" ] || p=gone
  if [ "$r" != "$p" ]; then
    echo "  DIFF [a $label]: reference $r, port $p"
    fail=$((fail + 1))
  elif [ "$r" = present ] && ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [a $label]: $(wc -c <"$W/r.arc") vs $(wc -c <"$W/p.arc") bytes"
    echo "    reference: $(names "$REF" "$W/r.arc" | tr '\n' ' ')"
    echo "    port:      $(names "$PORT" "$W/p.arc" | tr '\n' ' ')"
    fail=$((fail + 1))
  fi
}

# create_names <label> <options...> -- compare DEDUPLICATED name lists only.
# For --dirs, where the reference duplicates the top-level directory.
create_names() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -r -y -m0 "$@" "$W/r.arc" . ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "$@" "$W/p.arc" . ) >/dev/null 2>&1
  local rn pn
  rn="$(names "$REF" "$W/r.arc" | sort -u | tr '\n' ' ')"
  pn="$(names "$PORT" "$W/p.arc" | sort -u | tr '\n' ' ')"
  if [ "$rn" != "$pn" ]; then
    echo "  DIFF [a $label]: name sets differ"
    echo "    reference: $rn"
    echo "    port:      $pn"
    fail=$((fail + 1))
  fi
}

create "-x*.dat"              "-x*.dat"
create "-x*.txt"              "-x*.txt"
create "-n*.txt"              "-n*.txt"
create "-n*.dat"              "-n*.dat"
create "-nn.txt"              "-nn.txt"
create "-x n.txt"             "-xn.txt"
create "-x and -n"            "-x*.dat" "-n*"
create "-n then -x"           "-n*"     "-x*.dat"
create "-n narrow -x narrow"  "-n*.txt" "-xa.txt"
create "two -x"               "-x*.dat" "-xa.txt"
create "two -n"               "-n*.txt" "-n*.dat"
create "--nodirs"             "--nodirs"
create "--nodirs -x*.dat"     "--nodirs" "-x*.dat"
create "--fullnames -x"       "--fullnames" "-xsub/n.txt"
create "--fullnames -n"       "--fullnames" "-nsub/*"
create "-x matching nothing"  "-xnothing.here"
create "-n matching nothing"  "-nnothing.here"

# The reference duplicates the top-level directory under --dirs; compare names.
create_names "--dirs"           "--dirs"
create_names "--dirs -n*.txt"   "--dirs" "-n*.txt"
create_names "--dirs -x*.dat"   "--dirs" "-x*.dat"

# ── the READ commands: filespecs AND the filter ─────────────────────────────
rm -f "$W/base.arc"
( cd "$W/src" && "$REF" a --nodates -r -y -m0 "$W/base.arc" . ) >/dev/null 2>&1

read_cmp() {
  local label="$1"; shift
  checked=$((checked + 1))
  local r p
  r="$("$REF"  "$@" "$W/base.arc" 2>/dev/null | grep -E '^[0-9]{4}-' | awk '{print $NF}' | sort | tr '\n' ' ')"
  p="$("$PORT" "$@" "$W/base.arc" 2>/dev/null | grep -E '^[0-9]{4}-' | awk '{print $NF}' | sort | tr '\n' ' ')"
  if [ "$r" != "$p" ]; then
    echo "  DIFF [$label]: selection differs"
    echo "    reference: $r"
    echo "    port:      $p"
    fail=$((fail + 1))
  fi
}

read_cmp "l"                l
read_cmp "l -x*.dat"        l "-x*.dat"
read_cmp "l -n*.txt"        l "-n*.txt"
read_cmp "l --nodirs"       l "--nodirs"
read_cmp "l --dirs -n*.txt" l "--dirs" "-n*.txt"
read_cmp "l --fullnames -x" l "--fullnames" "-xsub/n.txt"
read_cmp "v -x*.dat"        v "-x*.dat"

# A filespec on a read command: `l x.arc '*.txt'`. Filespecs also turn OFF the
# default directory inclusion, which is a second thing this row covers.
checked=$((checked + 1))
r="$("$REF"  l "$W/base.arc" '*.txt' 2>/dev/null | grep -E '^[0-9]{4}-' | awk '{print $NF}' | sort | tr '\n' ' ')"
p="$("$PORT" l "$W/base.arc" '*.txt' 2>/dev/null | grep -E '^[0-9]{4}-' | awk '{print $NF}' | sort | tr '\n' ' ')"
if [ "$r" != "$p" ]; then
  echo "  DIFF [l with a filespec]: reference [$r] port [$p]"
  fail=$((fail + 1))
fi

# ── the archive-side filter for d and ch ────────────────────────────────────
#
# `ch` takes NO filespecs -- `is_CMD_WITHOUT_ARGS` (Options.hs:305) -- so its
# rows pass none and rely on the default `*`. Passing one is
# "command \"ch\" shouldn't have additional arguments"; that is checked in the
# self-tests below, because a row where BOTH binaries reject the command reads
# as a pass here.
for pair in "d:-x*.dat" "d:-n*.txt" "ch:-x*.dat" "ch:-n*.txt"; do
  cmd="${pair%%:*}"; opt="${pair#*:}"
  spec='*'; [ "$cmd" = ch ] && spec=""
  checked=$((checked + 1))
  cp "$W/base.arc" "$W/r.arc"; cp "$W/base.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  "$cmd" --nodates -y -m0 "$opt" "$W/r.arc" $spec ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" "$cmd" --nodates -y -m0 "$opt" "$W/p.arc" $spec ) >/dev/null 2>&1
  rp=present; [ -f "$W/r.arc" ] || rp=gone
  pp=present; [ -f "$W/p.arc" ] || pp=gone
  if [ "$rp" != "$pp" ]; then
    echo "  DIFF [$cmd $opt]: reference $rp, port $pp"; fail=$((fail + 1))
  elif [ "$rp" = present ] && ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [$cmd $opt]: $(wc -c <"$W/r.arc") vs $(wc -c <"$W/p.arc") bytes"
    echo "    reference: $(names "$REF" "$W/r.arc" | tr '\n' ' ')"
    echo "    port:      $(names "$PORT" "$W/p.arc" | tr '\n' ' ')"
    fail=$((fail + 1))
  fi
done

# `u` with a filter: the filter selects DISK files, and the archive's own
# entries are kept unfiltered (`cmd_archive_filter = const True`).
for opt in "-x*.dat" "-n*.txt"; do
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF" a --nodates -y -m0 "$W/r.arc" b.dat ) >/dev/null 2>&1
  cp "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  u --nodates -r -y -m0 "$opt" "$W/r.arc" . ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" u --nodates -r -y -m0 "$opt" "$W/p.arc" . ) >/dev/null 2>&1
  if ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [u $opt]: $(wc -c <"$W/r.arc") vs $(wc -c <"$W/p.arc") bytes"
    echo "    reference: $(names "$REF" "$W/r.arc" | tr '\n' ' ')"
    echo "    port:      $(names "$PORT" "$W/p.arc" | tr '\n' ' ')"
    fail=$((fail + 1))
  fi
done

echo "arc filters: $checked comparisons, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every row passes if BOTH binaries ignore every filter -- which is exactly the
# bug this harness was written for: the port used to accept -x and archive the
# excluded files anyway.

rm -f "$W/all.arc" "$W/some.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m0            "$W/all.arc"  . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "-x*.dat"  "$W/some.arc" . ) >/dev/null 2>&1
all="$(names "$PORT" "$W/all.arc" | sort | tr '\n' ' ')"
some="$(names "$PORT" "$W/some.arc" | sort | tr '\n' ' ')"
if [ "$all" = "$some" ]; then
  echo "SELF-TEST FAILED: -x changed nothing, so every row above compared two" >&2
  echo "unfiltered archives" >&2
  exit 1
fi
case "$some" in
  *.dat*) echo "SELF-TEST FAILED: -x*.dat left a .dat file in the archive" >&2; exit 1 ;;
  *) ;;
esac
case "$some" in
  *a.txt*) ;;
  *) echo "SELF-TEST FAILED: -x*.dat also removed the .txt files" >&2; exit 1 ;;
esac

# -n and -x must not be the same thing.
rm -f "$W/inc.arc"
( cd "$W/src" && "$PORT" a --nodates -r -y -m0 "-n*.txt" "$W/inc.arc" . ) >/dev/null 2>&1
if [ "$(names "$PORT" "$W/inc.arc" | sort | tr '\n' ' ')" = "$some" ]; then
  echo "SELF-TEST FAILED: -n*.txt and -x*.dat produced the same archive, so" >&2
  echo "nothing here distinguishes including from excluding" >&2
  exit 1
fi

# The read side must filter too -- it did not, and `arc l x.arc '*.txt'` listed
# every file in the archive.
if [ "$("$PORT" l "$W/all.arc" '*.txt' 2>/dev/null | grep -cE '^[0-9]{4}-')" \
   = "$("$PORT" l "$W/all.arc" 2>/dev/null | grep -cE '^[0-9]{4}-')" ]; then
  echo "SELF-TEST FAILED: a filespec on `l` selected everything, so the read" >&2
  echo "rows above compared two unfiltered listings" >&2
  exit 1
fi

# `ch` must REFUSE a filespec, as the reference does. Accepting it silently is
# how a row in arc-copy-check.sh tested nothing for a whole session: the
# filespec became the archive name, both binaries failed identically, and the
# comparison of two untouched archives passed.
cp "$W/base.arc" "$W/refuse.arc"
if "$PORT" ch --nodates -y -m0 "$W/refuse.arc" a.txt >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: ch accepted a filespec" >&2
  exit 1
fi
if "$REF" ch --nodates -y -m0 "$W/refuse.arc" a.txt >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference accepted a filespec for ch, so this" >&2
  echo "port is refusing something it should not" >&2
  exit 1
fi
# …while `d` and `j`, the two exceptions, must still accept one.
cp "$W/base.arc" "$W/accept.arc"
if ! "$PORT" d --nodates -y -m0 "$W/accept.arc" a.txt >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: d rejected a filespec, so the refusal is too broad" >&2
  exit 1
fi

echo "the Rust arc filters files exactly as the Haskell one does"
