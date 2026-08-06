#!/usr/bin/env bash
# Differential-test the encryption port (rust/darc-crypto) against the vendored
# LibTomCrypt it replaced.
#
# This is the check that went missing when the C came out. The old CI job
# compared a DARC_NO_RUST `arc` against a DARC_RUST one; both the opt-out and
# C_Encryption.cpp's `#ifndef DARC_RUST` fallbacks are gone, so the second
# implementation no longer exists in the working tree. It does exist in
# history, and that is where the oracle comes from here -- the same pinned
# revision every other codec harness compares against (see c-reference.sh).
#
# Encryption has no "format-valid is enough" escape hatch. An archive written
# with -p opens only if the key derivation and the cipher stream agree to the
# byte, so every comparison below is byte-for-byte, in both directions, at
# every key size and both modes.
#
# ── Three binaries, not two ─────────────────────────────────────────────────
#
#   c     the pinned C exactly as it shipped
#   c32   the pinned C with ONE header replaced: the working tree's
#         tomcrypt_macros.h, which types ulong32 as uint32_t
#   rs    the working tree's DARC_RUST shim, forwarding to libdarc_crypto.a
#
# `c32` exists because the pinned tomcrypt_macros.h types ulong32 as `unsigned
# long` on every LP64 target except x86_64, and serpent.c's key expansion
# rotates it with a raw shift-or that is a rotate at 32 bits and garbage at 64.
# So the shipped ARM64 C binaries encrypt `-ae serpent` differently from every
# other architecture -- a C bug the port fixes by construction. On x86_64 the
# two references are the same program; elsewhere they differ for serpent only,
# and the harness asserts exactly that rather than skipping the cipher. A skip
# would have been indistinguishable from the port being wrong.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1
# The DARC_RUST side compiles the FORWARDING shim -- C_Encryption.cpp with its
# LibTomCrypt body deleted -- which is no longer in the working tree. It has its
# own pin, distinct from the reference's. See c-reference.sh.
WREF="$(darc_wrapper_tree "$ROOT")" || exit 1
# The reference is built the way DArc builds _Encryption: see darc_codec_cflags
# in c-reference.sh for why the makefile's flags, not an -O level, are the
# oracle.
CFLAGS_C="$(darc_codec_cflags _Encryption)" || exit 1
W="${TMPDIR:-/tmp}/crypto-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

( cd "$ROOT/rust" && cargo build --release -p darc-crypto ) >/dev/null 2>&1 \
  || { echo "cargo build failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_crypto.a"

DEFS="-DFREEARC_UNIX -DFREEARC_INTEL_BYTE_ORDER -DFREEARC_64BIT"

# The C side compiles the PINNED C_Encryption.cpp without -DDARC_RUST, so the
# vendored LibTomCrypt is what runs. The Rust side compiles the WORKING TREE's
# C_Encryption.cpp with -DDARC_RUST, so what is tested is the production
# forwarding shim itself and not a harness-local copy of it -- an argument
# swapped in that shim would corrupt every encrypted archive while a harness
# that called darc_rs_docrypt directly stayed green.
build_c() { # <output> <tree>
  local out="$1" tree="$2"
  clang++ -std=c++17 $CFLAGS_C -w $DEFS \
    -I"$tree" -I"$tree/Compression" -I"$tree/Compression/_Encryption/headers" \
    "$tree/rust/difftest/crypto_ref.cpp" "$tree/rust/difftest/crypto_ccodec.cpp" \
    "$tree/Compression/Common.cpp" -o "$out"
}

build_c "$W/c" "$CREF" || { echo "building the pinned C reference failed" >&2; exit 1; }

# The 32-bit-ulong32 reference: a copy of the pinned tree with one header
# overlaid. Everything else, including C_Encryption.cpp itself, is still the
# pinned source.
CREF32="$W/cref32"
cp -R "$CREF" "$CREF32" || exit 1
# The header now lives beside this oracle in rust/cryptref/, not in
# Compression/_Encryption -- that directory holds only C_Encryption.cpp since the
# vendored LibTomCrypt was deleted from the working tree. It is kept because it is
# a FIX, not a copy: it types ulong32 as uint32_t where the pinned header says
# `unsigned`, which is 64 bits on LP64 targets other than x86-64 and breaks
# serpent's key expansion. Its own comment records the shipped arm64 builds that
# were affected.
cp "$ROOT/rust/cryptref/tomcrypt_macros.h" \
   "$CREF32/Compression/_Encryption/headers/tomcrypt_macros.h" || exit 1
grep -q 'typedef uint32_t ulong32' "$CREF32/Compression/_Encryption/headers/tomcrypt_macros.h" \
  || { echo "rust/cryptref/tomcrypt_macros.h no longer types ulong32 as uint32_t;" >&2
       echo "the c32 reference would be identical to c and would prove nothing" >&2; exit 1; }
build_c "$W/c32" "$CREF32" || { echo "building the 32-bit-ulong32 reference failed" >&2; exit 1; }

# The staticlib goes AFTER the sources that reference it: GNU ld resolves an
# archive only against the undefined symbols it has already seen.
#
# Compiled out of $WREF, not $ROOT. crypto_ccodec.cpp reaches its subject by
# `#include "../../Compression/_Encryption/C_Encryption.cpp"`, relative to the
# shim's own location -- so the shim has to sit inside the tree whose C it is
# meant to pick up. darc_wrapper_tree copies the live shims in for exactly this.
clang++ -std=c++17 $CFLAGS_C -w $DEFS -DDARC_RUST \
  -I"$WREF" -I"$WREF/Compression" \
  "$WREF/rust/difftest/crypto_ref.cpp" "$WREF/rust/difftest/crypto_ccodec.cpp" \
  "$WREF/Compression/Common.cpp" "$LIB" -o "$W/rs" \
  || { echo "building the Rust-backed shim failed" >&2; exit 1; }

fail=0

# ── The cipher id table ─────────────────────────────────────────────────────
# Cipher ids are not part of the archive format; they are positions in
# LibTomCrypt's registration table, which the Rust side hard-codes to match.
# A silent shift here would decrypt every archive with the wrong algorithm, so
# it is checked before anything else and through each side's own find_cipher.
for name in aes blowfish serpent twofish nope; do
  ic=$("$W/c" info "$name"); ir=$("$W/rs" info "$name")
  if [ "$ic" != "$ir" ]; then
    echo "  cipher table: '$name' is '$ic' in the C and '$ir' in the Rust shim"
    fail=$((fail+1))
  fi
done
[ "$fail" -eq 0 ] || { echo "crypto: cipher id/geometry tables disagree"; exit 1; }

# ── Corpus ──────────────────────────────────────────────────────────────────
# Sizes clustered around the two boundaries a stateful mode can desynchronize
# at: the cipher block (8 or 16 bytes) and docrypt's LARGE_BUFFER_SIZE read
# chunk (256 KB). CTR and CFB carry state across chunks, so a port that reset
# it per read would pass every input below 256 KB.
( cd "$ROOT/rust" && cargo build --release -q -p darc-codecs --bin corpusgen --bin difftest-util ) || exit 1

# Corpus from corpusgen -- a literal transcription of the python3 heredoc
# that stood here, accepted on a byte comparison over every file it writes.
"$ROOT/rust/target/release/corpusgen" crypto "$W/in"

# Keys and IVs are built byte by byte rather than from a small integer padded
# with zeroes. A 56-byte blowfish key that is 54 zeros and two data bytes is
# satisfied by an implementation that reads only part of it, and a counter
# starting at zero never carries -- both are exactly the mistakes this is
# looking for.
# The IV's two low bytes are 0xff so the little-endian counter carries out of
# byte 0 on the very first increment, and out of byte 1 soon after.
genhex() { "$ROOT/rust/target/release/difftest-util" genhex "$1" "$2" "$3"; }

# Key sizes per cipher: every length DArc can select with -ae CIPHER-BITS, plus
# each cipher's maximum (what parse_ENCRYPTION uses when no size is given).
# Blowfish's 56 is that maximum and the only one over 32 bytes.
CASES="aes:16,24,32 blowfish:8,32,56 serpent:16,24,32 twofish:16,24,32"

tested=0; cases=0
serpent_c_diffs=0; serpent_cases=0; other_c_diffs=0
# A plain string rather than an array: `${arr[@]}` on an empty array is an
# unbound-variable error under `set -u` in bash 3.2, which is what /bin/bash
# still is on macOS.
trivial=""

for spec in $CASES; do
  cipher="${spec%%:*}"
  ivlen=$("$W/c" info "$cipher" | cut -d' ' -f2)
  iv=$(genhex iv "$ivlen" 0)
  for keylen in $(echo "${spec##*:}" | tr ',' ' '); do
    key=$(genhex key "$keylen" "$keylen")
    for mode in ctr cfb; do
      nontrivial=0
      for f in "$W"/in/*; do
        bn=$(basename "$f"); tag="[$cipher-$((keylen*8))/$mode] $bn"
        cases=$((cases+1))

        "$W/c"   e "$cipher" "$mode" "$key" "$iv" < "$f" >| "$W/ec"   2>/dev/null \
          || { echo "  $tag: pinned C driver failed";  fail=$((fail+1)); continue; }
        "$W/c32" e "$cipher" "$mode" "$key" "$iv" < "$f" >| "$W/ec32" 2>/dev/null \
          || { echo "  $tag: 32-bit-ulong32 C driver failed"; fail=$((fail+1)); continue; }
        "$W/rs"  e "$cipher" "$mode" "$key" "$iv" < "$f" >| "$W/er"   2>/dev/null \
          || { echo "  $tag: Rust driver failed"; fail=$((fail+1)); continue; }
        tested=$((tested+1))

        # The bar: the port reproduces a correctly-built C, byte for byte.
        # Both stdouts carry the result code ahead of the payload, so this
        # also catches one side refusing a key size the other accepted.
        cmp -s "$W/ec32" "$W/er" || { echo "  $tag: ciphertext differs from the C"; fail=$((fail+1)); }

        # Track the pinned C separately: it is the same program as c32 except
        # for serpent on non-x86_64, and that expectation is asserted below.
        if ! cmp -s "$W/ec" "$W/ec32"; then
          if [ "$cipher" = serpent ]; then serpent_c_diffs=$((serpent_c_diffs+1))
          else
            other_c_diffs=$((other_c_diffs+1))
            echo "  $tag: the ulong32 width changes the output of a cipher that should not care"
            fail=$((fail+1))
          fi
        fi
        # Only non-empty inputs count towards the serpent expectation below:
        # an empty input yields an empty ciphertext from every build, so those
        # cases agree no matter how badly the key schedule is miscompiled.
        [ "$cipher" = serpent ] && [ -s "$f" ] && serpent_cases=$((serpent_cases+1))

        # A cipher that returned its input unchanged would satisfy every
        # comparison above on both sides at once.
        if [ -s "$f" ] && ! cmp -s <(tail -c +5 "$W/er") "$f"; then nontrivial=$((nontrivial+1)); fi

        # Both decryption directions. CFB is not symmetric, so decrypt is a
        # genuinely separate code path rather than the same call twice.
        "$W/rs"  d "$cipher" "$mode" "$key" "$iv" < <(tail -c +5 "$W/ec32") >| "$W/dr" 2>/dev/null
        "$W/c32" d "$cipher" "$mode" "$key" "$iv" < <(tail -c +5 "$W/er")   >| "$W/dc" 2>/dev/null
        cmp -s <(tail -c +5 "$W/dr") "$f" || { echo "  $tag: Rust could not decrypt the C's ciphertext"; fail=$((fail+1)); }
        cmp -s <(tail -c +5 "$W/dc") "$f" || { echo "  $tag: the C could not decrypt the Rust ciphertext"; fail=$((fail+1)); }
      done
      [ "$nontrivial" -gt 0 ] || trivial="$trivial $cipher-$((keylen*8))/$mode"
    done
  done
done

# ── PBKDF2 ──────────────────────────────────────────────────────────────────
# The key derivation is checked on its own because a wrong key produces a
# perfectly well-formed archive that simply never opens again. Empty password
# and empty salt are included: they are what a caller passing nothing produces,
# and the two implementations are free to disagree there.
kdf=0
# `-` stands for the empty string: a bare empty field cannot be written in a
# whitespace-separated table, and both are cases the two implementations are
# free to disagree on.
while read -r pwd salt iters outlen; do
  [ -n "$pwd$salt$iters$outlen" ] || continue
  [ "$pwd"  = - ] && pwd=""
  [ "$salt" = - ] && salt=""
  "$W/c"  kdf "$pwd" "$salt" "$iters" "$outlen" >| "$W/kc" || { echo "  kdf C driver failed"; fail=$((fail+1)); continue; }
  "$W/rs" kdf "$pwd" "$salt" "$iters" "$outlen" >| "$W/kr" || { echo "  kdf Rust driver failed"; fail=$((fail+1)); continue; }
  kdf=$((kdf+1))
  cmp -s "$W/kc" "$W/kr" || { echo "  kdf('$pwd','$salt',$iters,$outlen): derived keys differ"; fail=$((fail+1)); }
done <<'KDF'
password - 1 32
password 0011223344556677 1000 32
password 0011223344556677 1 1
password 0011223344556677 2 64
correct-horse-battery-staple 00112233445566778899aabbccddeeff 4096 32
p 0f 10000 16
KDF

# ── The one input the two implementations legitimately disagree on ──────────
# An EMPTY password. LibTomCrypt's hmac_init returns CRYPT_INVALID_KEYSIZE when
# keylen==0 (mac/hmac/hmac_init.c:48); pkcs_5_alg2 propagates it, and
# Pbkdf2Hmac is declared `void`, so the C never writes the key at all and the
# caller keeps whatever was in the buffer -- an uninitialised allocaBytes, in
# EncryptionLib.hs:29. The Rust pbkdf2 derives a real key instead.
#
# DArc cannot reach it: ArcvProcessCompress.hs:82 encrypts only when
# `password > ""`, and Encryption.hs:92 abandons decryption on an empty
# password rather than deriving from it. So this is a divergence on input no
# archive can contain, and the port is right not to reproduce a C error path.
#
# It is asserted rather than dropped: if the C ever starts writing a key here,
# or the Rust stops, that is a change worth seeing. The driver zeroes its key
# buffer before the call, so "the C wrote nothing" reads as all zeros.
"$W/c"  kdf "" "0011223344556677" 1 32 >| "$W/kc"
"$W/rs" kdf "" "0011223344556677" 1 32 >| "$W/kr"
zeros=$("$ROOT/rust/target/release/difftest-util" all-zeros "$W/kc" 4)
if [ "$zeros" != 1 ]; then
  echo "empty-password KDF: the C now writes a key where it used to fail; the"
  echo "divergence documented above no longer holds and should be re-examined."
  fail=$((fail+1))
fi
if cmp -s "$W/kc" "$W/kr"; then
  echo "empty-password KDF: the Rust side now returns the C's empty result too."
  echo "Either pbkdf2 started refusing zero-length passwords or the driver changed."
  fail=$((fail+1))
fi

# ── Assertions ──────────────────────────────────────────────────────────────
[ "$tested" -gt 0 ]  || { echo "no cases ran -- the harness reached nothing"; exit 1; }
[ "$tested" -eq "$cases" ] || { echo "$((cases-tested)) of $cases cases were skipped"; fail=$((fail+1)); }
[ "$kdf" -eq 6 ]     || { echo "$kdf key-derivation cases ran, expected the 6 in the table"; fail=$((fail+1)); }
[ -z "$trivial" ] || {
  echo "these configurations returned the plaintext unchanged:$trivial"; fail=$((fail+1)); }
[ "$other_c_diffs" -eq 0 ] || fail=$((fail+1))

# The serpent expectation, stated per architecture rather than skipped.
arch=$(uname -m)
if [ "$arch" = x86_64 ] || [ "$arch" = amd64 ]; then
  if [ "$serpent_c_diffs" -ne 0 ]; then
    echo "serpent: the pinned C and the 32-bit-ulong32 build differ on x86_64, where"
    echo "the pinned tomcrypt_macros.h already picks a 32-bit ulong32. Something other"
    echo "than the known typedef bug is at work."
    fail=$((fail+1))
  fi
  serpent_note="identical to the pinned C (x86_64: ulong32 is already 32-bit there)"
else
  if [ "$serpent_c_diffs" -ne "$serpent_cases" ]; then
    echo "serpent: expected all $serpent_cases cases to differ between the pinned C and the"
    echo "32-bit-ulong32 build on $arch, but $serpent_c_diffs did. Either the pin was moved past"
    echo "the ulong32 fix -- in which case delete this branch -- or the bug's reach changed."
    fail=$((fail+1))
  fi
  serpent_note="differs from the pinned C on all $serpent_cases cases, which is the known"
  serpent_note="$serpent_note ulong32 bug in the shipped $arch binaries; the port matches correct Serpent"
fi

[ "$fail" -eq 0 ] || { echo "crypto: $fail failures"; exit 1; }
echo "crypto: $tested cipher cases + $kdf key derivations byte-identical to the C"
echo "crypto: serpent $serpent_note"
