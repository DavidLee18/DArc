#!/usr/bin/env bash
# Differential-test the .7z reader port against the vendored 7-Zip SDK.
#
# Compression/7z was 13,789 lines, of which 13,563 were a vendored copy of the
# LZMA SDK's .7z reader. It is now the darc-sevenz crate over sevenz-rust2. This
# harness is what licenses that deletion.
#
# ── What "equal" means here, and why it is not byte-identity ─────────────────
#
# Every other harness in this directory demands byte-identical OUTPUT because
# DArc emits those bytes. DArc has never written a .7z -- Arc7z.hs shells out to
# 7zz for a/u/d -- so there is no encoder to match. The bar is that both readers
# agree on what an archive CONTAINS:
#
#   list      identical stdout, unsorted, and identical SRes. Both walk archive
#             file order, so any difference here is a real difference.
#   test      identical stdout after sorting. The port streams in block order
#             rather than file order (decoding in file order is O(n^2) on a
#             solid archive), so the progress lines are a set, not a sequence.
#   extract   identical extracted TREE -- every path and every byte. This is the
#             one that matters; the chatter is not.
#
# ── The comparison is deliberately ASYMMETRIC ───────────────────────────────
#
# The vendored SDK was compiled with most of its own decoders switched off, so
# it refuses archives the port reads:
#
#   PPMd                 7zDec.c ships `/* #define Z7_PPMD_SUPPORT */`
#                        COMMENTED OUT, and the makefile never defined it -- so
#                        k_PPMD was missing from IS_MAIN_METHOD while Ppmd7.c
#                        was compiled and linked anyway.
#   ARM64, ARMT filters  gated on Z7_USE_FILTER_ARM64 / _ARMT, never defined.
#   Delta ahead of BCJ2  outside CheckSupportedFolder's rigid 4-coder template.
#
# Demanding equal return codes would therefore fail on a corpus containing any
# of them -- and a corpus that avoided them would be avoiding exactly the cases
# where the two implementations differ most. So the contract is one-directional:
#
#   * where the C SUCCEEDS, the port must succeed and agree, byte for byte;
#   * where the C REFUSES with SZ_ERROR_UNSUPPORTED (4), the port may succeed;
#     that is recorded and printed, not failed;
#   * the port failing where the C succeeded is ALWAYS a failure.
#
# Trees are compared only when BOTH extractions returned 0. Comparing them after
# a failure compares partial leftovers: on encrypted.7z the C creates the
# directory entry before hitting the AES coder and the port, which streams
# blocks before directory entries, creates nothing -- both correctly return 4.
#
# ── Corpus ──────────────────────────────────────────────────────────────────
#
# Generated with the system 7-Zip, so the inputs are written by the reference
# implementation rather than by either reader under test. If no 7z binary
# exists the harness FAILS rather than reporting success over an empty corpus:
# a check that silently tests nothing is worse than no check, because it reads
# as coverage.
#
# DARC_SEVENZ_EXTRA_CORPUS may name a directory of additional .7z files (the
# sevenz-rust2 crate ships real 7-Zip-authored ones covering BCJ2, PPMd, delta
# and ARM64). They are used if present and skipped if not.
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
# The C reference comes from a pinned revision, not the working tree -- see
# c-reference.sh. Compression/7z only exists there now.
. "$ROOT/rust/difftest/c-reference.sh"
CREF="$(darc_c_reference "$ROOT")" || exit 1

W="${TMPDIR:-/tmp}/sevenz-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

[ -f "$CREF/Compression/7z/C_7z.c" ] || {
  echo "pinned reference has no Compression/7z -- cannot build the oracle" >&2; exit 1; }

# ── Build both readers ──────────────────────────────────────────────────────
( cd "$ROOT/rust" && cargo build --release -p darc-sevenz ) >/dev/null 2>&1 \
  || { echo "cargo build -p darc-sevenz failed" >&2; exit 1; }
LIB="$ROOT/rust/target/release/libdarc_sevenz.a"
[ -f "$LIB" ] || { echo "cargo produced no $LIB" >&2; exit 1; }

# Rust staticlibs need the platform's runtime pulled in by hand when the link is
# driven by the C compiler.
case "$(uname -s)" in
  Darwin) RUSTDEPS=(-framework CoreFoundation -framework Security -lc++) ;;
  *)      RUSTDEPS=(-lpthread -ldl -lm) ;;
esac

# The SDK flags are the ones Compression/7z/makefile used; _7ZIP_ST/Z7_ST pick
# the single-threaded decoder, which is what DArc built.
clang -std=c99 -O2 -w -D_7ZIP_ST -DZ7_ST -I"$CREF/Compression/7z/sdk" \
  "$ROOT/rust/difftest/sevenz_ref.c" \
  "$CREF/Compression/7z/C_7z.c" "$CREF"/Compression/7z/sdk/*.c \
  -o "$W/c" || { echo "building the C oracle failed" >&2; exit 1; }

# Library AFTER the sources: GNU ld resolves an archive only against undefineds
# it has already seen, so a staticlib placed first links on macOS and silently
# drops every symbol on Linux.
clang -std=c99 -O2 -w "$ROOT/rust/difftest/sevenz_ref.c" \
  "$LIB" "${RUSTDEPS[@]}" \
  -o "$W/rs" || { echo "building the Rust driver failed" >&2; exit 1; }

# Prove the two binaries are genuinely different programs. If a build silently
# produced the same thing twice, every comparison below would trivially pass.
cmp -s "$W/c" "$W/rs" && { echo "the two drivers are identical -- one build did not take effect" >&2; exit 1; }

# ── Corpus ──────────────────────────────────────────────────────────────────
SEVENZIP=""
for cand in 7zz 7z 7za; do
  command -v "$cand" >/dev/null 2>&1 && { SEVENZIP="$cand"; break; }
done
[ -n "$SEVENZIP" ] || {
  echo "no 7zz/7z/7za in PATH -- cannot generate the corpus, refusing to report a pass" >&2
  exit 1; }

SRC="$W/src"; mkdir -p "$SRC/nested/deep" "$SRC/emptydir"
python3 - "$SRC" <<'PY'
import os, sys
d = sys.argv[1]
def prng(seed, n):
    s = seed; o = bytearray()
    for _ in range(n):
        s = (s * 1103515245 + 12345) & 0xffffffff
        o.append((s >> 16) & 0xff)
    return bytes(o)
w = lambda p, b: open(os.path.join(d, p), "wb").write(b)
w("empty.bin", b"")
w("tiny.txt", b"hello")
w("text.txt", b"the quick brown fox jumps over the lazy dog.\n" * 5000)
w("random.bin", prng(7, 300000))
w("zeros.bin", b"\0" * 200000)
w("nested/a.txt", b"nested file\n" * 100)
w("nested/deep/b.bin", prng(11, 65536))
# An x86-ish body, so BCJ/BCJ2 has something to transform rather than passing
# incompressible noise straight through.
body = bytearray()
for i in range(20000):
    body += b"\xe8" + (i * 7 % 4294967296).to_bytes(4, "little") + b"\x90\x8b\xc0"
w("codeish.bin", bytes(body))
# Non-ASCII name, since names are UTF-16 in the container and UTF-8 out of it.
w("ünicode-日本語.txt", "unicode name\n".encode())
PY

mkarc() { # mkarc <name> <7z args...>
  local name="$1"; shift
  ( cd "$SRC" && "$SEVENZIP" a -y "$W/corpus/$name.7z" "$@" . ) >/dev/null 2>&1
}
mkdir -p "$W/corpus"
mkarc copy      -m0=Copy
mkarc lzma      -m0=LZMA
mkarc lzma2     -m0=LZMA2
mkarc ppmd      -m0=PPMd
mkarc bcj2      -mx9 -m0=BCJ2
mkarc delta     -m0=Delta -m1=LZMA2
mkarc solid     -mx9 -ms=on
mkarc nonsolid  -mx5 -ms=off
mkarc hdrcomp   -mx9 -mhc=on
mkarc small     -mx1

# Optional extra archives, authored elsewhere.
extra="${DARC_SEVENZ_EXTRA_CORPUS:-}"
if [ -n "$extra" ] && [ -d "$extra" ]; then
  for f in "$extra"/*.7z; do
    [ -f "$f" ] || continue
    cp "$f" "$W/corpus/extra-$(basename "$f")"
  done
fi

count=$(find "$W/corpus" -name '*.7z' | wc -l | tr -d ' ')
[ "$count" -gt 0 ] || { echo "corpus is empty -- 7z produced nothing" >&2; exit 1; }

# ── Compare ─────────────────────────────────────────────────────────────────
run() { # run <binary> <op> <archive> [outdir] -> stdout in $W/out, rc line in $W/rc
  local bin="$1" op="$2" arc="$3" outdir="${4:-}"
  if [ -n "$outdir" ]; then
    "$bin" "$op" "$arc" "$outdir" >| "$W/out" 2>| "$W/err"
  else
    "$bin" "$op" "$arc" >| "$W/out" 2>| "$W/err"
  fi
  grep -o 'rc=[-0-9]*' "$W/err" | tail -1 >| "$W/rc"
}

fail=0 checked=0 wider=0 agreed=0
for arc in "$W"/corpus/*.7z; do
  name=$(basename "$arc" .7z)
  checked=$((checked + 1))

  # -- list: strict, unsorted. Header parsing needs no decoder, so both readers
  #    must agree here on EVERY archive, including ones only the port can decode.
  run "$W/c"  l "$arc"; cp "$W/out" "$W/l.c";  crc=$(cat "$W/rc")
  run "$W/rs" l "$arc"; cp "$W/out" "$W/l.rs"; rrc=$(cat "$W/rc")
  [ "$crc" = "$rrc" ] || { echo "  $name: list SRes differs ($crc vs $rrc)"; fail=$((fail+1)); }
  cmp -s "$W/l.c" "$W/l.rs" || {
    echo "  $name: list output differs"; diff "$W/l.c" "$W/l.rs" | head -6; fail=$((fail+1)); }

  # -- test --------------------------------------------------------------------
  run "$W/c"  t "$arc"; sort "$W/out" >| "$W/t.c";  tc=$(cat "$W/rc")
  run "$W/rs" t "$arc"; sort "$W/out" >| "$W/t.rs"; tr_=$(cat "$W/rc")

  # -- extract -----------------------------------------------------------------
  rm -rf "$W/x.c" "$W/x.rs"; mkdir -p "$W/x.c" "$W/x.rs"
  run "$W/c"  x "$arc" "$W/x.c";  xc=$(cat "$W/rc")
  run "$W/rs" x "$arc" "$W/x.rs"; xr=$(cat "$W/rc")

  case "$xc:$xr" in
    "rc=0:rc=0")
      agreed=$((agreed + 1))
      [ "$tc" = "$tr_" ] || { echo "  $name: test SRes differs ($tc vs $tr_)"; fail=$((fail+1)); }
      cmp -s "$W/t.c" "$W/t.rs" || {
        echo "  $name: test output differs"; diff "$W/t.c" "$W/t.rs" | head -6; fail=$((fail+1)); }
      # diff -r compares every file's contents AND reports paths present on only
      # one side -- the two ways an extraction can be wrong.
      if ! diff -r "$W/x.c" "$W/x.rs" >| "$W/treediff" 2>&1; then
        echo "  $name: extracted trees differ"; head -8 "$W/treediff"; fail=$((fail+1))
      fi
      ;;
    "rc=4:rc=0")
      # The C was built without that decoder. Allowed, and worth seeing.
      wider=$((wider + 1))
      echo "  $name: C refuses (SZ_ERROR_UNSUPPORTED), port reads it"
      ;;
    *)
      if [ "$xc" = "$xr" ]; then
        # Both refused, for the same reason. Trees are partial leftovers here
        # and are deliberately not compared.
        agreed=$((agreed + 1))
      else
        echo "  $name: extract SRes differs ($xc vs $xr)"
        [ "$xc" = "rc=0" ] && echo "    the port FAILED where the C succeeded"
        fail=$((fail+1))
      fi
      ;;
  esac
done

echo "7z reader: $checked archives, $agreed agreeing, $wider readable only by the port, $fail differing"
[ "$fail" -eq 0 ] || exit 1

# ── The harness must be able to fail ────────────────────────────────────────
# Every archive above is well-formed, so all three comparisons agree trivially
# if BOTH readers silently do nothing. Prove they actually read: a truncated
# archive must be refused, and a valid one must list a non-empty catalogue.
head -c 200 "$W/corpus/lzma2.7z" >| "$W/truncated.7z"
run "$W/rs" t "$W/truncated.7z"
[ "$(cat "$W/rc")" != "rc=0" ] || {
  echo "a truncated archive was accepted -- the reader is not reading" >&2; exit 1; }
run "$W/rs" l "$W/corpus/lzma2.7z"
[ "$(wc -l < "$W/out")" -gt 5 ] || {
  echo "listing a good archive produced no entries -- the reader is not reading" >&2; exit 1; }

echo "the darc-sevenz reader agrees with the vendored 7-Zip SDK on every archive"
