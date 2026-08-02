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
# ── Two hex decodings, and the ":h1" that tells them apart ──────────────────
#
# decode16 in C_Encryption.cpp used to call char2int (Common.h:594), which maps
# 'a' to 0 rather than to 10 -- folding the key's 16 hex values onto 10 and
# costing about 0.75 bits per nibble. The salt and check code never went through
# it: those are decoded on the Haskell side by Utils.hs:582, which is ordinary
# hex. So a build that decodes all four correctly VERIFIES EVERY PASSWORD and
# then fails every CRC, since the check code cannot see the mismatch.
#
# It is fixed. Archives now carry ":h1", meaning "the key and IV are real
# hexadecimal", and archives without it are still read the old way. The rows
# below check all three halves of that:
#
#   * new archives, both directions, carry :h1 and cross-decrypt;
#   * an archive written WITHOUT the fix (-ae aes:h0, the escape hatch for
#     builds that predate the parameter) still cross-decrypts, which is what
#     keeps the legacy read path alive; and
#   * the two formats are not interchangeable -- forcing the wrong decoding
#     must fail rather than quietly produce different bytes.
#
# A build with no case for 'h' rejects the whole method string, so an old binary
# meeting a new archive says "invalid compression method or parameters" instead
# of reporting a corrupt archive. That is deliberate and is not testable here,
# since every binary in the tree now has the parameter.
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

# saw <pattern> <command...> -- true when the command's output contains it.
#
# NOT `cmd | grep -q pattern`. Under `set -o pipefail` that reports FAILURE on a
# successful match: grep -q exits at the first match, cmd takes SIGPIPE writing
# the rest, and pipefail takes the status from the killed writer. It only bites
# when the output is long enough that cmd is still writing, so it passes on
# small archives and flakes on real ones. Capture first, match after.
saw () {
  local pattern="$1"; shift
  local text; text="$("$@" 2>&1)"
  grep -q -- "$pattern" <<< "$text"
}

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

# The last spec writes the OLD format on purpose: "-ae aes:h0" overrides the
# ":h1" the command line inserts, which is the only way to exercise the legacy
# key decoding now that nothing writes it by default.
for m in -m0 -m1 -m4 -m9; do
  for spec in "-pSECRET" "-hpSECRET" "-pDATAPW -hpHEADPW" "-pSECRET -aeaes:h0"; do
    # ── the port writes, the reference reads ─────────────────────────────
    checked=$((checked + 1))
    rm -f "$W/port.arc"
    ( cd "$W/src" && $PORT a --nodates -r -y "$m" $spec "$W/port.arc" . ) >/dev/null 2>&1
    if [ ! -f "$W/port.arc" ]; then
      echo "  FAIL [$m $spec]: the port wrote no archive"; fail=$((fail + 1)); continue
    fi
    if ! saw 'All OK' "$REF" t $spec "$W/port.arc"; then
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
    if ! saw 'All OK' $PORT t $spec "$W/ref.arc"; then
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
  if ! saw 'All OK' "$REF" t -pSECRET "$W/port.arc"; then
    echo "  FAIL [-ae $ae]: the reference cannot read the port's archive"; fail=$((fail + 1))
  fi
  if ! saw 'All OK' $PORT t -pSECRET "$W/ref.arc"; then
    echo "  FAIL [-ae $ae]: the port cannot read the reference's archive"; fail=$((fail + 1))
  fi
done

# The two hex decodings must be distinguishable and NOT interchangeable. An
# archive written with :h0 and one written with :h1 differ only in that
# parameter and in which bytes the same key hex decodes to, so this is the one
# check that separates them.
checked=$((checked + 2))
rm -f "$W/h1.arc" "$W/h0.arc"
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET            "$W/h1.arc" . ) >/dev/null 2>&1
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET -aeaes:h0  "$W/h0.arc" . ) >/dev/null 2>&1
if ! saw ':h1' "$REF" lt -pSECRET "$W/h1.arc"; then
  echo "  FAIL: the default archive does not record :h1"; fail=$((fail + 1))
fi
if saw ':h1' "$REF" lt -pSECRET "$W/h0.arc"; then
  echo "  FAIL: -ae aes:h0 still recorded :h1, so the override is ignored"
  fail=$((fail + 1))
fi

# The PBKDF2 iteration count. ENCRYPTION_METHOD's constructor default moved from
# FreeArc's 1000 to OWASP's 210000 for PBKDF2-HMAC-SHA512, and it has to move on
# both sides at once -- the two binaries would still cross-decrypt if only one
# had moved, because each archive names its own count and the reader obeys it.
# So a mismatch here is invisible to every other row: it would simply mean one
# binary writes weak archives.
checked=$((checked + 2))
for bin in "$PORT" "$REF"; do
  rm -f "$W/n.arc"
  ( cd "$W/src" && "$bin" a --nodates -r -y -m0 -pSECRET "$W/n.arc" . ) >/dev/null 2>&1
  if ! saw ':n210000:' "$REF" lt -pSECRET "$W/n.arc"; then
    echo "  FAIL: $(basename "$bin") did not write the default iteration count"
    "$REF" lt -pSECRET "$W/n.arc" 2>/dev/null | grep '^\*' | head -1
    fail=$((fail + 1))
  fi
done

# An explicit count still wins, and an archive carrying one is read with IT and
# not with the default -- which is what keeps every archive written before the
# default moved readable. Cross-decrypted, so both sides are checked.
checked=$((checked + 2))
rm -f "$W/n1000.arc"
( cd "$W/src" && $PORT a --nodates -r -y -m0 -pSECRET -aeaes:n1000 "$W/n1000.arc" . ) >/dev/null 2>&1
if ! saw ':n1000:' "$REF" lt -pSECRET "$W/n1000.arc"; then
  echo "  FAIL: -ae aes:n1000 did not reach the archive"; fail=$((fail + 1))
fi
if ! saw 'All OK' "$REF" t -pSECRET "$W/n1000.arc"; then
  echo "  FAIL: the reference cannot read an archive with a non-default count"
  fail=$((fail + 1))
fi

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

# 4. The fix must CHANGE something. Both formats round-trip, so every row above
#    would pass if ":h1" were parsed and then ignored -- the failure this whole
#    parameter exists to prevent. Encrypt one block with each decoding, from the
#    SAME key and IV, and require different bytes.
h1_out=$($PORT crypt-probe h1 2>/dev/null)
h0_out=$($PORT crypt-probe h0 2>/dev/null)
if [ -z "$h1_out" ] || [ -z "$h0_out" ]; then
  echo "SELF-TEST FAILED: the crypt-probe produced nothing, so the two hex" >&2
  echo "decodings were never compared" >&2
  exit 1
fi
if [ "$h1_out" = "$h0_out" ]; then
  echo "SELF-TEST FAILED: the corrected and legacy hex decodings produce the" >&2
  echo "same ciphertext from the same key, so :h1 is being ignored" >&2
  exit 1
fi

echo "the Rust arc encrypts and decrypts exactly as the Haskell one does,"
echo "in both the corrected and the legacy hex format"
