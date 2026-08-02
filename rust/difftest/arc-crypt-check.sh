#!/usr/bin/env bash
# Differential-test the Rust `arc`'s ENCRYPTION against the Haskell one.
#
#   usage: arc-crypt-check.sh [reference-arc]
#
# ── Why this harness does not compare bytes ─────────────────────────────────
#
# Every other archive harness here asserts byte-identity, because that is the
# only bar that catches a divergence which still decodes. Encryption cannot
# use it: generateEncryption (Encryption.hs:36) draws a fresh salt and IV from
# the OS entropy source for EVERY BLOCK, so two runs over the same input with
# the same password differ, and so do two blocks of the same archive. That is
# the design working -- a reused salt across blocks would be the classic CTR
# failure -- and it means "the bytes match" is not available to check.
#
# What replaces it is CROSS-DECRYPTION, which is strictly what the feature
# promises: an archive either side writes must open on the other. Each row
# below therefore runs
#
#     port writes  -> reference tests and extracts -> compare against the tree
#     reference writes -> port tests and extracts  -> compare against the tree
#
# and the extracted files are compared with the originals, not merely reported
# as OK. `arc t` alone would pass on an archive whose CRCs were computed over
# the same wrong bytes it stored.
#
# ── What is checked besides "it round-trips" ────────────────────────────────
#
# A round trip is symmetric, so a shared mistake survives it. Three things are
# checked that a round trip cannot see:
#
#   * the archive is ACTUALLY ENCRYPTED -- the plaintext must not appear in it,
#     and the same input packed without -p must produce different bytes;
#   * a WRONG PASSWORD is refused by both, rather than producing garbage; and
#   * the stored method string carries a salt and check code and NO KEY.
#
# The last one is the failure with real consequences: an archive that carries
# `:k` next to its ciphertext is unencrypted in every sense that matters, and
# it would pass a round trip perfectly.
#
# ── The key is not decoded as hexadecimal ───────────────────────────────────
#
# C_Encryption.cpp's decode16 uses char2int (Common.h:594), which maps 'a' to 0
# rather than to 10. The key and IV that reach the cipher are therefore not the
# bytes their hex appears to name, while the salt and check code -- decoded on
# the Haskell side by Utils.hs:582 -- are ordinary hex. A port that decodes all
# four correctly VERIFIES EVERY PASSWORD and then fails every CRC, because the
# check code is derived from the salt and never passes through the broken
# decoder. This harness is what catches that: the unit tests could not, since
# both halves of a round trip would be wrong together.
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

W="${TMPDIR:-/tmp}/arc-crypt-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0

# A tree with content long enough to cross a cipher block and a chunk boundary,
# a subdirectory, and a file whose bytes are a recognisable marker so "is it
# really encrypted" can be answered by looking.
build_tree() {
  local d="$1"
  rm -rf "$d"; mkdir -p "$d/sub"
  printf 'MARKER-PLAINTEXT-SHOULD-NEVER-APPEAR-IN-THE-ARCHIVE\n' > "$d/marker.txt"
  # 200 KB, so a data block spans many cipher blocks and the 256 KB read the
  # streaming path uses is exercised on both sides of its boundary.
  head -c 200000 /dev/zero | tr '\0' 'K' > "$d/big.bin"
  printf 'second file\n'      > "$d/b.txt"
  printf 'in a subdirectory\n' > "$d/sub/nested.txt"
  touch -t 202501010000 "$d/marker.txt" "$d/big.bin" "$d/b.txt" "$d/sub/nested.txt"
}

# extract_and_compare <binary> <archive> <password-args...>
#   Extract with <binary> and diff the result against the source tree.
extract_and_compare() {
  local bin="$1" arc="$2"; shift 2
  rm -rf "$W/out"; mkdir -p "$W/out"
  ( cd "$W/out" && "$bin" x -y "$@" "$arc" ) >/dev/null 2>&1
  # The archive stores "./name" for a `.` filespec, so the tree lands under
  # out/. -- compare whichever of the two exists.
  local got="$W/out"
  [ -d "$W/out/." ] && [ -e "$W/out/marker.txt" ] || got="$W/out"
  diff -r "$W/src" "$got" >/dev/null 2>&1
}

build_tree "$W/src"

for m in -m0 -m1 -m4 -m9; do
  for spec in "-pSECRET" "-hpSECRET" "-pDATAPW -hpHEADPW"; do
    # ── the port writes, the reference reads ─────────────────────────────
    checked=$((checked + 1))
    rm -f "$W/port.arc"
    ( cd "$W/src" && $PORT a --nodates -r -y "$m" $spec "$W/port.arc" . ) >/dev/null 2>&1
    if [ ! -f "$W/port.arc" ]; then
      echo "  FAIL [$m $spec]: the port wrote no archive"; fail=$((fail + 1)); continue
    fi
    if ! ( "$REF" t $spec "$W/port.arc" 2>&1 | grep -q 'All OK' ); then
      echo "  FAIL [$m $spec]: the reference cannot test the port's archive"
      fail=$((fail + 1))
    elif ! extract_and_compare "$REF" "$W/port.arc" $spec; then
      echo "  FAIL [$m $spec]: the reference extracted the port's archive to different bytes"
      fail=$((fail + 1))
    fi

    # ── the reference writes, the port reads ─────────────────────────────
    checked=$((checked + 1))
    rm -f "$W/ref.arc"
    ( cd "$W/src" && "$REF" a --nodates -r -y "$m" $spec "$W/ref.arc" . ) >/dev/null 2>&1
    if [ ! -f "$W/ref.arc" ]; then
      echo "  FAIL [$m $spec]: the reference wrote no archive"; fail=$((fail + 1)); continue
    fi
    if ! ( $PORT t $spec "$W/ref.arc" 2>&1 | grep -q 'All OK' ); then
      echo "  FAIL [$m $spec]: the port cannot test the reference's archive"
      fail=$((fail + 1))
    elif ! extract_and_compare "$PORT" "$W/ref.arc" $spec; then
      echo "  FAIL [$m $spec]: the port extracted the reference's archive to different bytes"
      fail=$((fail + 1))
    fi
  done
done

# Every cipher and mode, both directions. `-ae` is the option; the port must
# agree with the reference on the default key size for each, which is where
# blowfish's 448-bit outlier and the aes/serpent/twofish 256 live.
for ae in aes aes-128 aes-192 blowfish serpent twofish aes/cfb blowfish/cfb serpent/cfb twofish/cfb; do
  checked=$((checked + 2))
  rm -f "$W/port.arc" "$W/ref.arc"
  ( cd "$W/src" && $PORT a --nodates -r -y -m1 -ae"$ae" -pSECRET "$W/port.arc" . ) >/dev/null 2>&1
  ( cd "$W/src" && "$REF" a --nodates -r -y -m1 -ae"$ae" -pSECRET "$W/ref.arc" . ) >/dev/null 2>&1
  if ! ( "$REF" t -pSECRET "$W/port.arc" 2>&1 | grep -q 'All OK' ); then
    echo "  FAIL [-ae $ae]: the reference cannot read the port's archive"; fail=$((fail + 1))
  fi
  if ! ( $PORT t -pSECRET "$W/ref.arc" 2>&1 | grep -q 'All OK' ); then
    echo "  FAIL [-ae $ae]: the port cannot read the reference's archive"; fail=$((fail + 1))
  fi
done

# A wrong password must be REFUSED, not decoded into garbage, in both
# directions. `-p-` forbids prompting, so a hung read is not mistaken for a
# refusal.
for m in -m0 -m4; do
  checked=$((checked + 2))
  rm -f "$W/port.arc"
  ( cd "$W/src" && $PORT a --nodates -r -y "$m" -pRIGHT "$W/port.arc" . ) >/dev/null 2>&1
  if "$REF" t -pWRONG -op- "$W/port.arc" >/dev/null 2>&1; then
    echo "  FAIL [$m]: the reference ACCEPTED a wrong password on the port's archive"
    fail=$((fail + 1))
  fi
  if $PORT t -pWRONG -op- "$W/port.arc" >/dev/null 2>&1; then
    echo "  FAIL [$m]: the port ACCEPTED a wrong password on its own archive"
    fail=$((fail + 1))
  fi
done

echo "arc encryption: $checked cross-decryptions, $fail failing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────
# Everything above passes if -p is silently ignored on BOTH sides: the archives
# would round-trip perfectly and be plaintext. Three properties rule that out.

rm -f "$W/enc.arc" "$W/plain.arc"
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET "$W/enc.arc" . )  >/dev/null 2>&1
( cd "$W/src" && $PORT a --nodates -r -y -m0          "$W/plain.arc" . ) >/dev/null 2>&1

# 1. The marker must be findable in the unencrypted archive and absent from the
#    encrypted one. Checking only the second half would pass on an empty file.
if ! grep -aq 'MARKER-PLAINTEXT' "$W/plain.arc"; then
  echo "SELF-TEST FAILED: the marker is not in the UNENCRYPTED archive either," >&2
  echo "so its absence from the encrypted one proves nothing" >&2
  exit 1
fi
if grep -aq 'MARKER-PLAINTEXT' "$W/enc.arc"; then
  echo "SELF-TEST FAILED: the plaintext is present in the encrypted archive" >&2
  exit 1
fi

# 2. The stored method must name a salt and a check code and must NOT carry a
#    key. A `:k` in an archive is the key sitting next to the ciphertext.
methods="$("$REF" lt -pSECRET "$W/enc.arc" 2>/dev/null)"
case "$methods" in
  *aes-256/ctr*) ;;
  *) echo "SELF-TEST FAILED: no encryption method in the stored block table" >&2
     echo "$methods" >&2; exit 1 ;;
esac
case "$methods" in
  *':s'*) ;;
  *) echo "SELF-TEST FAILED: the stored method carries no salt" >&2; exit 1 ;;
esac
case "$methods" in
  *':c'*) ;;
  *) echo "SELF-TEST FAILED: the stored method carries no check code" >&2; exit 1 ;;
esac
if grep -aq ':k' <<< "$methods"; then
  echo "SELF-TEST FAILED: the archive stores the KEY next to the ciphertext" >&2
  echo "$methods" >&2
  exit 1
fi

# 3. Two encryptions of the same input must differ -- a fixed salt or IV would
#    make every row above pass while reusing a keystream across archives.
rm -f "$W/e1.arc" "$W/e2.arc"
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET "$W/e1.arc" . ) >/dev/null 2>&1
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET "$W/e2.arc" . ) >/dev/null 2>&1
if cmp -s "$W/e1.arc" "$W/e2.arc"; then
  echo "SELF-TEST FAILED: two encryptions of the same input are identical," >&2
  echo "so the salt and IV are not being drawn fresh" >&2
  exit 1
fi

echo "the Rust arc encrypts and decrypts exactly as the Haskell one does"
