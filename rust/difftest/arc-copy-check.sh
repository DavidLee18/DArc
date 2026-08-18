#!/usr/bin/env bash
# Differential-test the Rust archive-COPYING commands against the Haskell ones,
# BYTE FOR BYTE.
#
#   usage: arc-copy-check.sh [reference-arc]
#
# `ch`, `c`, `k` and `j` are all one code path: `runArchiveAdd` with a different
# archive filter and a different source of files (Arc.hs:122-131).
#
#   ch   re-pack the archive, keeping the files the filespecs match
#   c    ch with -z: replace the comment, read from stdin
#   k    ch plus the lock, after which every modifying command is refused
#   j    add the files of the archives named on the command line
#
# ── The option that decides everything: whether -m was given ────────────────
#
# `opt_recompress` is `--recompress || (is_COPYING_COMMAND && (-m given ||
# --nodata || --crconly))`, and `opt_keep_original` is its negation for those
# commands (Cmdline.hs:372-378). So:
#
#   arc ch -m0 x.arc     re-compresses every block with -m0
#   arc ch     x.arc     KEEPS each block's own compression, copying whole
#                        solid blocks verbatim (ArcvProcessRead.hs:126)
#
# Both are tested for every command, because they are different code and only
# the second one is what a user types. Missing it made `arc d` without -m write
# 279 bytes where the reference wrote 249: the port repacked with the -m4
# default what the reference had copied.
#
# A block is copied only when the surviving files are exactly that block, in
# order, starting at its first byte -- `isWholeSolidBlock` (ArhiveFileList.hs:387).
# `-s-` archives make that easy to arrange and easy to break, so the corpus is
# built both ways.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"

. "$ROOT/rust/difftest/arc-reference.sh"
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || {
  echo "cargo build failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-copy-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

# saw <pattern> <command...> -- see arc-update-check.sh for why this is not
# `cmd | grep -q`.
saw () {
  local pattern="$1"; shift
  local text; text="$("$@" 2>&1)"
  grep -q -- "$pattern" <<< "$text"
}

build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub"
  printf 'the first file\n'    > "$d/a.txt"
  printf 'the second file\n'   > "$d/b.txt"
  printf 'the third file\n'    > "$d/c.txt"
  printf 'in a subdirectory\n' > "$d/sub/nested.txt"
  head -c 40000 /dev/zero | tr '\0' 'Z' > "$d/big.bin"
  touch -t 202501010000 "$d/a.txt" "$d/b.txt" "$d/c.txt" "$d/sub/nested.txt" "$d/big.bin"
}

build_tree "$W/src"

# base <archive> <make-options...> -- a fresh source archive.
base() {
  local arc="$1"; shift
  rm -f "$arc"
  ( cd "$W/src" && "$REF" a --nodates -r -y "$@" "$arc" . ) >/dev/null 2>&1
}

# compare <label> <command-and-OPTIONS...> -- run the command on a copy of each
# side's archive and require the results to match byte for byte.
#
# The archive name is appended LAST, so everything passed here must be an
# OPTION. Passing a filespec makes it the archive name and the archive a
# filespec, and both binaries then fail identically -- which reads as a pass.
# That is not hypothetical: a `ch … a.txt` row here did exactly that, and it
# tested nothing until `ch` was taught to refuse filespecs at all.
compare() {
  local label="$1"; shift
  checked=$((checked + 1))
  cp "$W/base.arc" "$W/ref.arc"
  cp "$W/base.arc" "$W/port.arc"
  ( cd "$W/src" && "$REF"  "$@" "$W/ref.arc"  ) </dev/null >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" "$@" "$W/port.arc" ) </dev/null >/dev/null 2>&1
  local r=present p=present
  [ -f "$W/ref.arc" ]  || r=gone
  [ -f "$W/port.arc" ] || p=gone
  if [ "$r" != "$p" ]; then
    echo "  DIFF [$label]: reference $r, port $p"
    fail=$((fail + 1))
  elif [ "$r" = present ] && ! cmp -s "$W/ref.arc" "$W/port.arc"; then
    echo "  DIFF [$label]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
    fail=$((fail + 1))
  fi
}

# ── ch / k / d, each with and without -m, over three source layouts ──────────
# The layouts differ in how blocks are cut, which is what decides whether a
# surviving group is a whole block and can be copied.
for make in "-m1" "-m1 -s-" "-m4"; do
  for m in "" "-m0" "-m1" "-m9"; do
    base "$W/base.arc" $make
    compare "ch [$make] $m"        ch --nodates -y $m
    compare "k  [$make] $m"        k  --nodates -y $m
    # `d` is one of the two copying commands that DOES take filespecs, so its
    # spec goes after the archive -- which `compare` cannot express. Done
    # inline below instead.
    checked=$((checked + 1))
    cp "$W/base.arc" "$W/ref.arc"; cp "$W/base.arc" "$W/port.arc"
    ( cd "$W/src" && "$REF"  d --nodates -y $m "$W/ref.arc"  c.txt ) >/dev/null 2>&1
    ( cd "$W/src" && "$PORT" d --nodates -y $m "$W/port.arc" c.txt ) >/dev/null 2>&1
    if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
      echo "  DIFF [d [$make] $m]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
      fail=$((fail + 1))
    fi
    # A filespec that leaves a solid block PARTIALLY selected: the block cannot
    # be copied and has to be repacked with its own chain, not with the -m
    # default. This is the case a naive keep_original gets wrong -- and it has
    # to be reached through `d`, because `ch` refuses filespecs.
    checked=$((checked + 1))
    cp "$W/base.arc" "$W/ref.arc"; cp "$W/base.arc" "$W/port.arc"
    ( cd "$W/src" && "$REF"  d --nodates -y $m "$W/ref.arc"  a.txt ) >/dev/null 2>&1
    ( cd "$W/src" && "$PORT" d --nodates -y $m "$W/port.arc" a.txt ) >/dev/null 2>&1
    if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
      echo "  DIFF [d part [$make] $m]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
      fail=$((fail + 1))
    fi
  done
done

# ── j, with and without -m ──────────────────────────────────────────────────
for make in "-m1" "-m1 -s-" "-m4"; do
  for m in "" "-m1"; do
    checked=$((checked + 1))
    base "$W/base.arc" $make
    rm -f "$W/extra.arc"
    ( cd "$W/src" && "$REF" a --nodates -y -m1 "$W/extra.arc" a.txt b.txt ) >/dev/null 2>&1
    cp "$W/base.arc" "$W/ref.arc"; cp "$W/base.arc" "$W/port.arc"
    ( cd "$W/src" && "$REF"  j --nodates -y $m "$W/ref.arc"  "$W/extra.arc" ) >/dev/null 2>&1
    ( cd "$W/src" && "$PORT" j --nodates -y $m "$W/port.arc" "$W/extra.arc" ) >/dev/null 2>&1
    if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
      echo "  DIFF [j [$make] $m]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
      fail=$((fail + 1))
    fi
  done
done

# ── c: the comment, read from stdin and terminated by a lone "." ────────────
for m in "" "-m1"; do
  checked=$((checked + 1))
  base "$W/base.arc" -m1
  cp "$W/base.arc" "$W/ref.arc"; cp "$W/base.arc" "$W/port.arc"
  printf 'a comment\nwith two lines\n.\n' | "$REF"  c --nodates -y $m "$W/ref.arc"  >/dev/null 2>&1
  printf 'a comment\nwith two lines\n.\n' | "$PORT" c --nodates -y $m "$W/port.arc" >/dev/null 2>&1
  if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
    echo "  DIFF [c $m]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
    fail=$((fail + 1))
  fi
done

# The comment must SURVIVE an ordinary update. It did not: `arc u` on a
# commented archive used to come out exactly the comment's length shorter.
#
# The filespecs differ by command and that is not incidental: for u/f they
# select DISK files, for d and ch they select files inside the ARCHIVE. Passing
# "." to `ch` asks it to keep only the entries matching ".", which is none of
# them -- so the archive is emptied and removed. That is correct behaviour and a
# wrong test.
for pair in "u:." "f:." "d:c.txt" "ch:"; do
  cmd="${pair%%:*}"; spec="${pair#*:}"
  checked=$((checked + 1))
  rm -f "$W/base.arc"
  ( cd "$W/src" && "$REF" a --nodates -r -y -m1 --archive-comment='kept comment' \
      "$W/base.arc" . ) >/dev/null 2>&1
  cp "$W/base.arc" "$W/ref.arc"; cp "$W/base.arc" "$W/port.arc"
  ( cd "$W/src" && "$REF"  "$cmd" --nodates -r -y -m1 "$W/ref.arc"  $spec ) >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" "$cmd" --nodates -r -y -m1 "$W/port.arc" $spec ) >/dev/null 2>&1
  if ! cmp -s "$W/ref.arc" "$W/port.arc"; then
    echo "  DIFF [$cmd with a comment]: $(wc -c <"$W/ref.arc") vs $(wc -c <"$W/port.arc") bytes"
    fail=$((fail + 1))
  fi
done

echo "arc ch/c/k/j: $checked archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────

# 1. `ch` WITHOUT -m must keep the source's compression, and `ch -m0` must not.
#    If both produced the same archive, every "no -m" row above would be
#    comparing two identical repacks and keep_original would be untested.
base "$W/base.arc" -m1
cp "$W/base.arc" "$W/keep.arc"; cp "$W/base.arc" "$W/repack.arc"
"$PORT" ch --nodates -y     "$W/keep.arc"   >/dev/null 2>&1
"$PORT" ch --nodates -y -m0 "$W/repack.arc" >/dev/null 2>&1
if cmp -s "$W/keep.arc" "$W/repack.arc"; then
  echo "SELF-TEST FAILED: ch and ch -m0 produced the same archive, so nothing" >&2
  echo "here distinguishes keeping the original compression from repacking" >&2
  exit 1
fi
if ! saw '4x4:tor' "$PORT" lt "$W/keep.arc"; then
  echo "SELF-TEST FAILED: ch without -m did not keep the -m1 chain, so the" >&2
  echo "blocks were repacked rather than copied" >&2
  "$PORT" lt "$W/keep.arc" >&2
  exit 1
fi
if saw '4x4:tor' "$PORT" lt "$W/repack.arc"; then
  echo "SELF-TEST FAILED: ch -m0 still carries the -m1 chain, so -m was ignored" >&2
  exit 1
fi

# 2. `k` must actually lock, and a locked archive must refuse to be modified.
#    Every k row passes if the lock is silently dropped on both sides.
base "$W/base.arc" -m1
cp "$W/base.arc" "$W/locked.arc"
"$PORT" k --nodates -y "$W/locked.arc" >/dev/null 2>&1
if "$PORT" ch --nodates -y -m0 "$W/locked.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: a locked archive accepted ch, so k is not locking" >&2
  exit 1
fi
if "$REF" ch --nodates -y -m0 "$W/locked.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference accepted ch on the archive the port" >&2
  echo "locked, so the port is not writing the lock flag" >&2
  exit 1
fi

# 3. `j` must actually add the other archive's files. The extra archive has to
#    hold a file the base does NOT, or the join is a no-op by construction and
#    this proves nothing -- which is how it read before: the extra archive was
#    built from a file already in the base, so the count could not move.
base "$W/base.arc" -m1
rm -rf "$W/only"; mkdir -p "$W/only"
printf 'only in the extra archive\n' > "$W/only/extra-only.txt"
touch -t 202501010000 "$W/only/extra-only.txt"
rm -f "$W/extra.arc"
( cd "$W/only" && "$REF" a --nodates -y -m1 "$W/extra.arc" extra-only.txt ) >/dev/null 2>&1
if ! saw 'extra-only' "$PORT" l "$W/extra.arc"; then
  echo "SELF-TEST FAILED: the extra archive was not built" >&2
  exit 1
fi
rm -f "$W/joined.arc"; cp "$W/base.arc" "$W/joined.arc"
"$PORT" j --nodates -y "$W/joined.arc" "$W/extra.arc" >/dev/null 2>&1
if ! saw 'extra-only' "$PORT" l "$W/joined.arc"; then
  echo "SELF-TEST FAILED: j did not bring in the extra archive's file, so the" >&2
  echo "j rows above compared two archives neither side had joined" >&2
  exit 1
fi

# 4. `d` must actually delete, or every d row compares two untouched archives.
base "$W/base.arc" -m1
cp "$W/base.arc" "$W/del.arc"
"$PORT" d --nodates -y "$W/del.arc" c.txt >/dev/null 2>&1
if saw 'c\.txt' "$PORT" l "$W/del.arc"; then
  echo "SELF-TEST FAILED: d left c.txt in the archive" >&2
  exit 1
fi

echo "the Rust arc ch/c/k/j copy archives exactly as the Haskell ones do"
