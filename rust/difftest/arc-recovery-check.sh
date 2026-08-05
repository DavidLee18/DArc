#!/usr/bin/env bash
# Differential-test the Rust recovery records against the Haskell ones,
# BYTE FOR BYTE -- and then have the reference USE the port's.
#
#   usage: arc-recovery-check.sh [reference-arc]
#
# ── What is written ────────────────────────────────────────────────────────
#
# `writeRecoveryBlocks` (ArcRecover.hs:76) cuts the archive into sectors, stores
# a CRC32 per sector, and XORs each sector into recovery sector `i mod N`. Two
# blocks come out, both RECOVERY_BLOCK and both stored: the XOR sectors, and the
# CRCs behind a small header giving the geometry.
#
# The layout has a detail worth stating, because getting it wrong shifts every
# byte after it:
#
#   [header][data][dir][footer #1]      <- the protected region
#   [recovery 0][recovery 1]
#   [footer #2]                         <- lists header + dir + recovery
#
# The footer is written TWICE. Footer #1 closes the archive and is itself
# protected, and its RECOVERY FIELD IS EMPTY -- the archive has no recovery info
# at that point. Footer #2 records the setting and lists the recovery blocks,
# but NOT footer #1. Both mistakes were made here and both were caught by
# comparing block tables: the wrong footer list gave six blocks against five,
# and the non-empty first footer shifted everything by four bytes.
#
# ── Geometry ───────────────────────────────────────────────────────────────
#
# The sector size falls out of the requested percentage -- 4% -> 512, 2% ->
# 1024, 1% -> 2048 -- so the rows below sweep the amount as well as the explicit
# `N%;SS` and `N*SS` forms. `-rr0%` is the CRC-only mode: no XOR sectors at all,
# which detects damage without being able to repair it, and which is why the
# recorded version differs (0.39 rather than 0.36).
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
# `arcdump` is a SECOND binary, and the self-test at the end of this file is the
# only thing that uses it. Nothing built it, so on any machine that had not
# happened to build it by hand the self-test read an empty output and reported a
# failure -- which is exactly what happened the first time CI ever ran this.
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin arcdump ) || {
  echo "cargo build of arcdump failed" >&2; exit 1; }

W="${TMPDIR:-/tmp}/arc-recovery-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

saw () {
  local pattern="$1"; shift
  local text; text="$("$@" 2>&1)"
  grep -q -- "$pattern" <<< "$text"
}

# Big enough that the sector size actually varies with the percentage: at a few
# hundred bytes every setting collapses onto the same geometry.
mkdir -p "$W/src"
head -c 300000 /dev/urandom > "$W/src/big.bin"
printf 'a small file\n' > "$W/src/a.txt"
touch -t 202501010000 "$W/src/big.bin" "$W/src/a.txt"

size() { stat -f '%z' "$1" 2>/dev/null || stat -c '%s' "$1" 2>/dev/null; }

# create <options...> -- both binaries create with -rr and compare bytes.
create() {
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -y -m0 "$@" "$W/r.arc" . ) </dev/null >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -y -m0 "$@" "$W/p.arc" . ) </dev/null >/dev/null 2>&1
  if ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [a $*]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes"
    fail=$((fail + 1))
  fi
}

for opt in -rr -rr1% -rr2% -rr4% -rr8% -rr0% -rr100b -rr1k \
           "-rr2%;1024" "-rr1%;512" "-rr0*4kb" "-rr8*4096" "-rr0*65536"; do
  create "$opt"
done

# `rr…` is `ch -rr…`: the same copy path with the setting from the command name.
for cmd in rr rr1% rr4% rr0% rr2%';'1024; do
  checked=$((checked + 1))
  rm -f "$W/base.arc"
  ( cd "$W/src" && "$REF" a --nodates -y -m0 "$W/base.arc" . ) >/dev/null 2>&1
  cp "$W/base.arc" "$W/r.arc"; cp "$W/base.arc" "$W/p.arc"
  "$REF"  "$cmd" --nodates -y "$W/r.arc" </dev/null >/dev/null 2>&1
  "$PORT" "$cmd" --nodates -y "$W/p.arc" </dev/null >/dev/null 2>&1
  if ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [$cmd]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes"
    fail=$((fail + 1))
  fi
done

# The setting is remembered in the archive, so a later command with no -rr
# re-adds the SAME amount. That is `old_recovery` feeding back in, and it is the
# only reason the default "--" does anything at all.
for first in -rr1% -rr4%; do
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF" a --nodates -y -m0 "$first" "$W/r.arc" . ) >/dev/null 2>&1
  cp "$W/r.arc" "$W/p.arc"
  "$REF"  ch --nodates -y "$W/r.arc" </dev/null >/dev/null 2>&1
  "$PORT" ch --nodates -y "$W/p.arc" </dev/null >/dev/null 2>&1
  if ! cmp -s "$W/r.arc" "$W/p.arc"; then
    echo "  DIFF [ch after $first]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes"
    fail=$((fail + 1))
  fi
done

# -rr- removes it again.
checked=$((checked + 1))
rm -f "$W/r.arc" "$W/p.arc"
( cd "$W/src" && "$REF" a --nodates -y -m0 -rr4% "$W/r.arc" . ) >/dev/null 2>&1
cp "$W/r.arc" "$W/p.arc"
"$REF"  ch --nodates -y -rr- "$W/r.arc" </dev/null >/dev/null 2>&1
"$PORT" ch --nodates -y -rr- "$W/p.arc" </dev/null >/dev/null 2>&1
if ! cmp -s "$W/r.arc" "$W/p.arc"; then
  echo "  DIFF [ch -rr-]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes"
  fail=$((fail + 1))
fi

# `-rr+` means what a bare `-rr` means: the archive's own setting, or the
# recommended amount if it had none (ArcRecover.hs:93). `rr_ok` (Cmdline.hs:641)
# used to reject the value before that case could run, which made a documented
# spelling an INVALID_OPTION_VALUE; both now list it. On a NEW archive there is
# no old setting, so the two spellings must agree byte for byte -- and each must
# agree across the two builds.
checked=$((checked + 1))
rm -f "$W/r.arc" "$W/p.arc" "$W/r-bare.arc" "$W/p-bare.arc"
( cd "$W/src" && "$REF"  a --nodates -y -m0 -rr+ "$W/r.arc"      . ) </dev/null >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m0 -rr+ "$W/p.arc"      . ) </dev/null >/dev/null 2>&1
( cd "$W/src" && "$REF"  a --nodates -y -m0 -rr  "$W/r-bare.arc" . ) </dev/null >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m0 -rr  "$W/p-bare.arc" . ) </dev/null >/dev/null 2>&1
for f in r.arc p.arc r-bare.arc p-bare.arc; do
  if [ ! -s "$W/$f" ]; then
    echo "  DIFF [-rr+]: $f was not written -- the value was refused"
    fail=$((fail + 1))
  fi
done
if ! cmp -s "$W/r.arc" "$W/p.arc"; then
  echo "  DIFF [-rr+]: $(size "$W/r.arc") vs $(size "$W/p.arc") bytes"
  fail=$((fail + 1))
fi
if ! cmp -s "$W/r.arc" "$W/r-bare.arc"; then
  echo "  DIFF [-rr+]: reference disagrees with its own bare -rr"
  fail=$((fail + 1))
fi
if ! cmp -s "$W/p.arc" "$W/p-bare.arc"; then
  echo "  DIFF [-rr+]: port disagrees with its own bare -rr"
  fail=$((fail + 1))
fi

# Values that are still nonsense must be refused by both.
for spec in "x" "1z"; do
  checked=$((checked + 1))
  rm -f "$W/r.arc" "$W/p.arc"
  ( cd "$W/src" && "$REF"  a --nodates -y -m0 "-rr$spec" "$W/r.arc" . ) </dev/null >/dev/null 2>&1
  r=ok; [ -f "$W/r.arc" ] || r=refused
  ( cd "$W/src" && "$PORT" a --nodates -y -m0 "-rr$spec" "$W/p.arc" . ) </dev/null >/dev/null 2>&1
  p=ok; [ -f "$W/p.arc" ] || p=refused
  if [ "$r" != "$p" ]; then
    echo "  DIFF [-rr$spec]: reference $r, port $p"
    fail=$((fail + 1))
  elif [ "$r" != refused ]; then
    echo "  DIFF [-rr$spec]: both ACCEPTED a value rr_ok rejects"
    fail=$((fail + 1))
  fi
done

echo "arc recovery: $checked archives, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Every row passes if BOTH binaries ignore -rr and write a plain archive.

rm -f "$W/plain.arc" "$W/prot.arc"
( cd "$W/src" && "$PORT" a --nodates -y -m0        "$W/plain.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m0 -rr4%  "$W/prot.arc"  . ) >/dev/null 2>&1
if [ "$(size "$W/plain.arc")" = "$(size "$W/prot.arc")" ]; then
  echo "SELF-TEST FAILED: -rr4% did not change the archive size, so every row" >&2
  echo "above compared two unprotected archives" >&2
  exit 1
fi
if ! saw 'recovery block' "$ROOT/rust/target/release/arcdump" "$W/prot.arc"; then
  echo "SELF-TEST FAILED: no recovery block in the protected archive" >&2
  exit 1
fi
if saw 'recovery block' "$ROOT/rust/target/release/arcdump" "$W/plain.arc"; then
  echo "SELF-TEST FAILED: a recovery block in an archive written without -rr" >&2
  exit 1
fi

# The amount must actually track the percentage, or every -rrN% row is the same
# archive under different names.
rm -f "$W/one.arc" "$W/eight.arc"
( cd "$W/src" && "$PORT" a --nodates -y -m0 -rr1% "$W/one.arc"   . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m0 -rr8% "$W/eight.arc" . ) >/dev/null 2>&1
if [ "$(size "$W/one.arc")" = "$(size "$W/eight.arc")" ]; then
  echo "SELF-TEST FAILED: -rr1% and -rr8% produced the same size" >&2
  exit 1
fi

# ── the reference must be able to USE what the port wrote ───────────────────
# Byte-identity already implies this, but only as long as both sides are wrong
# together is impossible -- and here it is not: a geometry both binaries compute
# identically and wrongly would still compare equal. So the recovery info is
# exercised: the reference must accept the intact archive and REJECT a damaged
# one, which requires the stored CRCs to describe the real sectors.
if ! saw 'All OK' "$REF" t "$W/prot.arc"; then
  echo "SELF-TEST FAILED: the reference will not test the port's protected archive" >&2
  "$REF" t "$W/prot.arc" >&2
  exit 1
fi
cp "$W/prot.arc" "$W/damaged.arc"
# Corrupt a byte well inside the protected data, away from any header.
printf 'ZZZZZZZZZZZZZZZZ' | dd of="$W/damaged.arc" bs=1 seek=100000 conv=notrunc 2>/dev/null
if "$REF" t "$W/damaged.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference reported a damaged archive as OK, so the" >&2
  echo "CRCs the port stored do not describe the archive's sectors" >&2
  exit 1
fi

echo "the Rust arc writes recovery records exactly as the Haskell one does,"
echo "and the reference detects damage using them"
