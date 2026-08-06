#!/usr/bin/env bash
# rust/include/*.h must be byte-identical to the pinned Compression/ copies.
#
#   usage: c-header-check.sh
#
# WHY THIS EXISTS
#
# `Compression/` is not in the working tree; the differential harnesses read
# their C from two pins (see c-reference.sh). Two headers could not follow it,
# because `darc-codecs` and `darc-crypto` run bindgen over them at cargo-build
# time and a build script cannot `git archive` without breaking offline and
# vendored builds. So Compression.h and Common.h were MOVED to rust/include/,
# where the crates that consume them own them.
#
# That leaves two copies of one contract: rust/include/ (what Rust is generated
# from) and the pinned Compression/ (what every C oracle and every forwarder is
# compiled against). If they drift, bindgen and the C disagree about the ABI --
# CALLBACK_FUNC's signature, MemSize's width, the FREEARC_ERRCODE_* values --
# and the failure is a silently wrong number crossing the boundary, not a
# compile error. The whole reason bindgen is used here rather than hand-written
# declarations is that hand transcription had already produced 41 helpers whose
# declarations disagreed with their definitions, 8 of them truncating a `long`
# return to `int`. Two copies with no check would reintroduce exactly that.
#
# So: byte-identical, or this fails. If a header genuinely has to change, change
# it here AND bump DARC_WRAPPER_REF_SHA to a commit carrying the same text --
# which is a deliberate act, as it should be.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$ROOT/rust/difftest/c-reference.sh"

WREF="$(darc_wrapper_tree "$ROOT")" || exit 1

fail=0 checked=0
for h in Compression.h Common.h; do
  ours="$ROOT/rust/include/$h"
  theirs="$WREF/Compression/$h"
  checked=$((checked + 1))
  if [ ! -f "$ours" ]; then
    echo "  MISSING: $ours" >&2; fail=$((fail + 1)); continue
  fi
  if [ ! -f "$theirs" ]; then
    echo "  MISSING at the pin: Compression/$h" >&2; fail=$((fail + 1)); continue
  fi
  if ! cmp -s "$ours" "$theirs"; then
    echo "  DRIFT: rust/include/$h differs from Compression/$h at DARC_WRAPPER_REF_SHA"
    diff "$theirs" "$ours" | head -12 | sed 's/^/      /'
    fail=$((fail + 1))
  fi
done

echo "c-headers: $checked compared, $fail differing"
[ "$checked" -eq 2 ] || { echo "expected to compare 2 headers" >&2; exit 1; }
[ "$fail" -eq 0 ] || exit 1

# ── the comparison must be able to fail ─────────────────────────────────────
#
# `cmp -s` on two paths that do not exist is not a pass, but a typo in either
# path would make every future run compare nothing and say so cheerfully. Prove
# the comparison separates identical from different.
probe="${TMPDIR:-/tmp}/c-header-check.$$"; mkdir -p "$probe"
trap 'rm -rf "$probe"' EXIT
cp "$ROOT/rust/include/Common.h" "$probe/same.h"
{ cat "$ROOT/rust/include/Common.h"; echo "/* sabotage */"; } > "$probe/different.h"
cmp -s "$ROOT/rust/include/Common.h" "$probe/same.h" || {
  echo "SELF-TEST FAILED: identical files compared unequal" >&2; exit 1; }
if cmp -s "$ROOT/rust/include/Common.h" "$probe/different.h"; then
  echo "SELF-TEST FAILED: a modified header compared equal, so the check above" >&2
  echo "would not have noticed drift" >&2
  exit 1
fi

echo "the ABI headers bindgen reads are the ones the pinned C compiles against"
