#!/usr/bin/env bash
# Round-trip encrypted archives, and cross-check that two builds interoperate.
#
#   enc-roundtrip.sh <writer-arc> [reader-arc]
#
# One binary  -> plain round-trip (create with -p, extract, diff).
# Two binaries -> the compatibility guarantee: does <reader> open what <writer>
# encrypted? Run it both ways (C-build writes / Rust-build reads, and reverse)
# to prove a DARC_RUST=1 build and a stock build produce interchangeable
# encrypted archives.
#
# Covers every cipher x mode plus the -128 key-size variant, and asserts a
# wrong password is rejected.
#
# There is no longer an ARM64 caveat. A stock C build used to miscompile Serpent
# -- ulong32 was 64-bit there and serpent.c's key expansion assumes 32 -- so
# C<->Rust serpent cross-checks failed on ARM64. That is fixed (ulong32 is now
# uint32_t), and ALL ciphers must interoperate in both directions on every
# target. A serpent cross-check failure is now a REGRESSION, not an expectation.
#
# Note this script needs stdin: on a failed decrypt the archiver prompts for a
# password, so run it with </dev/null in automation or it blocks forever rather
# than reporting a failure.
set -u
WRITER="$1"; READER="${2:-$1}"
W="${TMPDIR:-/tmp}/enc-rt.$$"; rm -rf "$W"; mkdir -p "$W/in"
echo "the quick brown fox" > "$W/in/a.txt"
head -c 30000 /dev/urandom > "$W/in/b.bin"
printf 'x%.0s' $(seq 1 4000) > "$W/in/c.txt"
PW="correct horse battery staple"
fail=0
for spec in "aes" "aes/cfb" "blowfish" "blowfish/cfb" "serpent" "serpent/cfb" "twofish" "twofish/cfb" "aes-128"; do
  arc="$W/t.arc"; out="$W/out"; rm -f "$arc"; rm -rf "$out"; mkdir -p "$out"
  if ! "$WRITER" a --nodates -r -y -p"$PW" -ae"$spec" "$arc" "$W/in" >"$W/c.log" 2>&1; then
    echo "  $spec  CREATE FAILED"; tail -2 "$W/c.log" | sed 's/^/     /'; fail=$((fail+1)); continue
  fi
  if ! "$READER" x -y -p"$PW" -dp"$out" "$arc" >"$W/x.log" 2>&1; then
    echo "  $spec  EXTRACT FAILED (reader)"; tail -2 "$W/x.log" | sed 's/^/     /'; fail=$((fail+1)); continue
  fi
  if diff -r "$W/in" "$out/${W#/}/in" >/dev/null 2>&1; then
    echo "  $spec  OK ($(wc -c < "$arc" | tr -d ' ') bytes)"
  else
    echo "  $spec  CONTENT MISMATCH"; fail=$((fail+1))
  fi
done
rm -f "$W/t.arc"; "$WRITER" a --nodates -r -y -p"$PW" -aeaes "$W/t.arc" "$W/in" >/dev/null 2>&1
if "$READER" t -y -p"wrong password" "$W/t.arc" >/dev/null 2>&1; then
  echo "  wrong-password  ACCEPTED (should reject!)"; fail=$((fail+1))
else
  echo "  wrong-password  correctly rejected"
fi
rm -rf "$W"
[ "$fail" -eq 0 ] && echo "ALL OK" || { echo "$fail FAILED"; exit 1; }
