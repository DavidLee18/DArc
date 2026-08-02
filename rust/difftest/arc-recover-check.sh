#!/usr/bin/env bash
# Differential-test the Rust `arc r` -- repair from recovery records -- against
# the Haskell one, comparing the REPAIRED ARCHIVES byte for byte.
#
#   usage: arc-recover-check.sh [reference-arc]
#
# ── The identity being tested ──────────────────────────────────────────────
#
# Each recovery sector holds the XOR of every archive sector mapped to it. Read
# them all back and XOR them into it and everything cancels except the damage:
# with exactly one bad sector `n` in a group, the recovery sector ends up
# holding `S_n ^ S_n'`, so XORing it with the damaged sector reproduces the
# original. Two bad sectors in one group leave `S_a ^ S_a' ^ S_b ^ S_b'`, which
# identifies neither -- `partition (null.tail)` over the groups is exactly that
# distinction (ArcRecover.hs:326).
#
# So the damage patterns below are chosen around the GROUPING, not around byte
# offsets: sectors that collide on one recovery sector are unrecoverable however
# far apart they are in the file. On the corpus here, offsets 50000 and 150000
# are sectors 97 and 292, and 97 % 13 == 292 % 13 == 6 -- one recovery sector,
# both lost. That row is included precisely because it looks recoverable.
#
# ── What is compared ───────────────────────────────────────────────────────
#
# The repaired file, byte for byte. Plus, where full recovery is possible, that
# it equals the ORIGINAL -- two binaries that "repaired" identically and wrongly
# would agree with each other but not with the archive that was damaged.
#
# NOT compared: console output. This port prints no banners, no progress
# indicators and no stage lines for any command, so `arc r` is consistent with
# `l`, `t` and `x` in that respect. The numbers it does print are the same:
# `showMemory`, which is " kbytes" with a space and rounding, NOT `showMem`'s
# "kb" -- both exist and the recovery messages use the former.
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

W="${TMPDIR:-/tmp}/arc-recover-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

mkdir -p "$W/src"
head -c 200000 /dev/urandom > "$W/src/big.bin"
printf 'a small file\n' > "$W/src/a.txt"
touch -t 202501010000 "$W/src/big.bin" "$W/src/a.txt"

# clobber <file> <offset...> -- 16 bytes of Z at each offset.
clobber() {
  local f="$1"; shift
  for off in "$@"; do
    printf 'ZZZZZZZZZZZZZZZZ' | dd of="$f" bs=1 seek="$off" conv=notrunc 2>/dev/null
  done
}

# try <label> <rr-option> <offsets...>
try() {
  local label="$1" rr="$2"; shift 2
  checked=$((checked + 1))
  rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
  ( cd "$W/src" && "$REF" a --nodates -y -m0 "$rr" "$W/orig.arc" . ) >/dev/null 2>&1
  cp "$W/orig.arc" "$W/r/x.arc"; cp "$W/orig.arc" "$W/p/x.arc"
  clobber "$W/r/x.arc" "$@"
  clobber "$W/p/x.arc" "$@"

  ( cd "$W/r" && "$REF"  r "$W/r/x.arc" ) >/dev/null 2>&1
  ( cd "$W/p" && "$PORT" r "$W/p/x.arc" ) >/dev/null 2>&1

  local rf="$W/r/fixed.x.arc" pf="$W/p/fixed.x.arc"
  local rp=present pp=present
  [ -f "$rf" ] || rp=absent
  [ -f "$pf" ] || pp=absent
  if [ "$rp" != "$pp" ]; then
    echo "  DIFF [$label]: reference $rp, port $pp"
    fail=$((fail + 1))
    return
  fi
  if [ "$rp" = absent ]; then
    return  # both refused, which is the answer for an unrecoverable pattern
  fi
  if ! cmp -s "$rf" "$pf"; then
    echo "  DIFF [$label]: repairs differ"
    fail=$((fail + 1))
  fi
}

# Single damaged sectors: always recoverable.
try "1 sector"          -rr4% 100000
try "1 sector, early"   -rr4% 1000
try "1 sector, late"    -rr4% 199000
try "inside footer #1"  -rr4% 200170
# Several sectors that land on different recovery sectors.
try "3 scattered"       -rr4% 20000 90000 170000
try "adjacent sectors"  -rr4% 100000 100600
try "4 spread"          -rr4% 5000 60000 120000 180000
# A collision: 50000 and 150000 are sectors 97 and 292, and both are 6 mod 13.
try "collision"         -rr4% 50000 150000
# More recovery info: bigger groups, so more survives.
try "1 sector at 8%"    -rr8% 100000
try "3 scattered at 8%" -rr8% 20000 90000 170000
try "2 at 1%"           -rr1% 30000 130000
# CRC-only: damage is DETECTED but nothing can be repaired, so both must refuse.
try "0% cannot repair"  -rr0% 100000

echo "arc r: $checked patterns, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────

rm -rf "$W/s"; mkdir -p "$W/s"
( cd "$W/src" && "$REF" a --nodates -y -m0 -rr4% "$W/s/x.arc" . ) >/dev/null 2>&1
cp "$W/s/x.arc" "$W/s/pristine.arc"

# 1. A single-sector repair must reproduce the ORIGINAL bytes. Two binaries
#    that repaired identically and WRONGLY would pass every row above.
clobber "$W/s/x.arc" 100000
if cmp -s "$W/s/x.arc" "$W/s/pristine.arc"; then
  echo "SELF-TEST FAILED: clobbering changed nothing, so no row above damaged" >&2
  echo "anything and every repair compared two intact archives" >&2
  exit 1
fi
( cd "$W/s" && "$PORT" r "$W/s/x.arc" ) >/dev/null 2>&1
if [ ! -f "$W/s/fixed.x.arc" ]; then
  echo "SELF-TEST FAILED: the port repaired nothing" >&2
  exit 1
fi
if ! cmp -s "$W/s/fixed.x.arc" "$W/s/pristine.arc"; then
  echo "SELF-TEST FAILED: the repair does not reproduce the original archive" >&2
  exit 1
fi
# …and the reference must accept the port's repair.
if ! "$REF" t "$W/s/fixed.x.arc" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference will not test the port's repaired archive" >&2
  exit 1
fi

# 2. An UNDAMAGED archive must produce no output file at all: `r` says
#    "Archive ok, no need to restore it!" and stops.
rm -rf "$W/ok"; mkdir -p "$W/ok"
cp "$W/s/pristine.arc" "$W/ok/x.arc"
( cd "$W/ok" && "$PORT" r "$W/ok/x.arc" ) >/dev/null 2>&1
if [ -f "$W/ok/fixed.x.arc" ]; then
  echo "SELF-TEST FAILED: r wrote a fixed archive for an undamaged one" >&2
  exit 1
fi

# 3. An archive with NO recovery info must be refused rather than silently
#    copied.
rm -rf "$W/none"; mkdir -p "$W/none"
( cd "$W/src" && "$REF" a --nodates -y -m0 "$W/none/x.arc" . ) >/dev/null 2>&1
if ( cd "$W/none" && "$PORT" r "$W/none/x.arc" ) >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: r accepted an archive with no recovery records" >&2
  exit 1
fi
if [ -f "$W/none/fixed.x.arc" ]; then
  echo "SELF-TEST FAILED: r wrote a file for an archive it cannot repair" >&2
  exit 1
fi

# 4. `r` must REFUSE to overwrite an existing fixed.* rather than clobber a
#    previous repair.
rm -rf "$W/twice"; mkdir -p "$W/twice"
cp "$W/s/pristine.arc" "$W/twice/x.arc"
clobber "$W/twice/x.arc" 100000
: > "$W/twice/fixed.x.arc"
if ( cd "$W/twice" && "$PORT" r "$W/twice/x.arc" ) >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: r overwrote an existing fixed.x.arc" >&2
  exit 1
fi
if [ -s "$W/twice/fixed.x.arc" ]; then
  echo "SELF-TEST FAILED: r wrote over the existing file" >&2
  exit 1
fi

echo "the Rust arc r repairs archives exactly as the Haskell one does"
