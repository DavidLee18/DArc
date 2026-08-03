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
# Every row is byte-identity. There is no longer a `--dirs` exception: the
# reference used to write the top-level directory of each filespec twice, which
# is fixed in FileInfo.hs:462 -- see `filter::write_dirs`.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

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

# A second tree with three distinct sizes and three distinct ages, so the size
# and time filters have boundaries to land on either side of.
sizes_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub"
  head -c 50   /dev/zero | tr '\0' 'a' > "$d/small.txt"
  head -c 5000 /dev/zero | tr '\0' 'b' > "$d/big.txt"
  head -c 500  /dev/zero | tr '\0' 'c' > "$d/sub/mid.txt"
  touch -t 202001010000 "$d/small.txt"
  touch -t 203001010000 "$d/big.txt"
  touch -t 202501010000 "$d/sub/mid.txt"
}
sizes_tree "$W/sizes"

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

# create_spec <label> <filespec> <options...> -- as `create`, but the filespec
# is given instead of always being ".". That distinction is the whole of the
# addDir pass: "." names no child of ".", so it emits nothing and every row
# using it is blind to the pass entirely.
create_spec() {
  local label="$1" spec="$2"; shift 2
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -y -m0 "$@" "$W/r.arc" "$spec" ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -y -m0 "$@" "$W/p.arc" "$spec" ) >/dev/null 2>&1
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

# ── the size and time filters ───────────────────────────────────────────────
#
# Over the `sizes` tree, and WITHOUT --nodates: the time filters read the file's
# real mtime, and --nodates only changes what is stored.
#
# The time options are given by their LONG spellings on purpose. `-ta` is
# ambiguous with `--type`, which is in aPREFFERED_OPTIONS and wins, so `-ta…`
# is "--type=a…: only arc format is supported" -- in the reference as much as
# here. `-sm`/`-sl` are themselves preferred and so survive their clash with
# `-s`, which is why those rows use the short form.
sizes() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/sizes" && "$REF"  a -r -y -m0 "$@" "$W/r.arc" . ) >/dev/null 2>&1
  ( cd "$W/sizes" && "$PORT" a -r -y -m0 "$@" "$W/p.arc" . ) >/dev/null 2>&1
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

sizes "-sm100"              "-sm100"
sizes "-sm1k"               "-sm1k"
sizes "-sl1000"             "-sl1000"
sizes "-sm100 -sl1000"      "-sm100" "-sl1000"
sizes "-sm0"                "-sm0"
sizes "-sm 50 boundary"     "-sm50"
sizes "-sl 50 boundary"     "-sl50"
sizes "-sm10k selects none" "-sm10k"
sizes "TimeAfter abs"       "--TimeAfter=20240101000000"
sizes "TimeBefore abs"      "--TimeBefore=20240101000000"
sizes "TimeAfter future"    "--TimeAfter=20260101000000"
sizes "TimeAfter short"     "--TimeAfter=2024"
sizes "TimeNewer 1d"        "--TimeNewer=1d"
sizes "TimeOlder 1d"        "--TimeOlder=1d"
sizes "TimeNewer huge"      "--TimeNewer=99999"
sizes "size and time"       "-sm100" "--TimeAfter=20240101000000"

# The short spellings that lose to --type must lose the same way on both sides.
sizes "-ta loses to --type"  "-ta20240101000000"

# `--dirs` is byte-identity like everything else. It used to be name-lists only:
# `accept_f` (FileInfo.hs:462) served both the addDir pass and the main walk, and
# forcing it true made the addDir pass accept every SIBLING of the named
# directory -- duplicating the entry the walk already emitted, and storing
# directories that were never named. Fixed by giving the pass its own arm.
create "--dirs"           "--dirs"
create "--dirs -n*.txt"   "--dirs" "-n*.txt"
create "--dirs -x*.dat"   "--dirs" "-x*.dat"
create "--dirs -r"        "--dirs" "-r"

# ── the addDir pass: a filespec that NAMES a directory ──────────────────────
#
# Every row above passes ".", where the pass emits nothing. These pass a real
# directory name, which must produce an entry for THAT directory -- and whose
# predicate is `include_dirs `defaultVal` True`: --dirs/--nodirs decide it and
# the n/s/t filters do not, so `-n*.txt` keeps `sub` while dropping the
# subdirectories the walk found under it.
for spec in sub sub/ ./sub sub/deeper; do
  create_spec "$spec"               "$spec"
  create_spec "$spec --dirs"        "$spec" "--dirs"
  create_spec "$spec --nodirs"      "$spec" "--nodirs"
  create_spec "$spec -n*.txt"       "$spec" "-n*.txt"
  create_spec "$spec --dirs -n*.txt" "$spec" "--dirs" "-n*.txt"
  create_spec "$spec -r"            "$spec" "-r"
done

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

# The size and time filters must SELECT something and not everything, or their
# rows compare two identical unfiltered archives.
rm -f "$W/sz.arc" "$W/all2.arc"
( cd "$W/sizes" && "$PORT" a -r -y -m0 "$W/all2.arc" . ) >/dev/null 2>&1
( cd "$W/sizes" && "$PORT" a -r -y -m0 -sm100 "$W/sz.arc" . ) >/dev/null 2>&1
n_all=$(names "$PORT" "$W/all2.arc" | grep -c . || true)
n_sz=$(names "$PORT" "$W/sz.arc" | grep -c . || true)
if [ "$n_sz" = "$n_all" ] || [ "$n_sz" = 0 ]; then
  echo "SELF-TEST FAILED: -sm100 selected $n_sz of $n_all, so the size rows" >&2
  echo "compared an unfiltered archive or an empty one" >&2
  exit 1
fi
rm -f "$W/tm.arc"
( cd "$W/sizes" && "$PORT" a -r -y -m0 --TimeAfter=20240101000000 "$W/tm.arc" . ) >/dev/null 2>&1
n_tm=$(names "$PORT" "$W/tm.arc" | grep -c . || true)
if [ "$n_tm" = "$n_all" ] || [ "$n_tm" = 0 ]; then
  echo "SELF-TEST FAILED: --TimeAfter selected $n_tm of $n_all, so the time" >&2
  echo "rows compared an unfiltered archive or an empty one" >&2
  exit 1
fi

echo "the Rust arc filters files exactly as the Haskell one does"
