#!/usr/bin/env bash
# Differential-test SFX modules (`-sfx`, the `s…` command) and volumes (`-v`)
# against the Haskell ones, BYTE FOR BYTE -- and the NAMES they produce.
#
#   usage: arc-sfx-check.sh [reference-arc]
#
# ── SFX ────────────────────────────────────────────────────────────────────
#
# An SFX module is a stub prepended to the archive so the file runs as an
# extractor. Block positions are absolute and all shift, but the footer stores
# them RELATIVE to itself, so the encoded values never change -- only
# `ftSFXSize`, which a reader derives from the first block's position.
#
#   -sfx-        drop the stub the input archive had
#   -sfx--       copy it across (the default)
#   -sfxMODULE   prepend the named file
#
# ── The name changes, and it changes TWICE ─────────────────────────────────
#
# `changeSfxExt` (ArcCreate.hs:336) swaps the extension: on Unix
# aDEFAULT_SFX_EXTENSION is the EMPTY STRING, so `-sfxMODULE` turns `x.arc` into
# `x` and `-sfx-` turns `x` back into `x.arc`.
#
# It is applied in two places: up front for an archive that does not exist yet
# (ArcCreate.hs:68), and again by `renameArchiveAsSFX` (:172) AFTER writing, in
# every case. Implementing only the first gives the right bytes under the wrong
# name for `ch -sfx-` on an existing archive -- which is how the second was
# found.
#
# On top of that, `addArcExtension` (Cmdline.hs:770) appends `.arc` to any
# archive name with no extension, for every command. So an SFX archive named
# `x` cannot be reached as `x` without `--noarcext`, because `arc l x` looks for
# `x.arc`. Every row below therefore passes --noarcext where the name has no
# extension.
#
# ── Volumes ────────────────────────────────────────────────────────────────
#
# `-v` splits the FINISHED archive into `.001`, `.002`… and removes the
# original. There is no per-volume header and no cross-volume structure: it is a
# plain byte split, which is why the message says to reassemble with `cat`. The
# read side exists in C (`darc_join_volumes`) and has NO CALLER, so nothing
# reads volumes back automatically -- checked, not assumed.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REF="${1:-$ROOT/Tests/arc-ghc}"
PORT="$ROOT/rust/target/release/darc"
# Any file will do: this harness only ever PREPENDS the module and measures the
# result, so what is inside it is irrelevant here -- `arc-tiny.linux.sfx` was
# chosen originally because it was the smallest of the three C++ tiers. Those
# are gone with Unarc/; the SFX module is now the darc-unarc binary, which is
# also what `unarc` is, because it detects at runtime whether it has an archive
# appended to it.
MODULE="$ROOT/rust/target/release/unarc"

[ -x "$REF" ] || {
  echo "no reference binary at $REF.

The Haskell reference was deleted; build one from a commit that still has it:
  git worktree add /tmp/darc-ref 9a127e6 && (cd /tmp/darc-ref && ./compile-ghc-probe)
then pass /tmp/darc-ref/Tests/arc-ghc as $1. For a gate that needs no
reference at all, use arc-golden-check.sh" >&2
  exit 2
}
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc -p darc-unarc ) || {
  echo "cargo build failed" >&2; exit 1; }
# After the build, not before: the module is a cargo artifact now, so a missing
# one means the build did not produce it rather than "run make first".
[ -f "$MODULE" ] || {
  echo "no SFX module at $MODULE -- cargo built no darc-unarc binary" >&2
  exit 2
}

W="${TMPDIR:-/tmp}/arc-sfx-check.$$"; mkdir -p "$W"
trap 'rm -rf "$W"' EXIT

fail=0 checked=0
# Portable and unambiguous. This was
#
#     stat -f '%z' "$1" 2>/dev/null || stat -c '%s' "$1" 2>/dev/null
#
# which assumes `stat -f` FAILS wherever it is not BSD. On Linux GNU `stat -f`
# means --file-system and SUCCEEDS, so the `||` never fires and `size` returns a
# block of filesystem info beginning `File: "..."`. The caller then does
# arithmetic on that and bash reports `File: unbound variable` -- which is
# exactly what CI hit the first time this harness ever ran on Linux. `wc -c`
# has no BSD/GNU split to get wrong.
size() { wc -c < "$1" | tr -d '[:space:]'; }

mkdir -p "$W/src"
printf 'one file\n' > "$W/src/a.txt"
printf 'another\n'  > "$W/src/b.txt"
head -c 40000 /dev/urandom > "$W/src/c.bin"
touch -t 202501010000 "$W/src/a.txt" "$W/src/b.txt" "$W/src/c.bin"

# names <dir> <prefix> -- what landed there, as one comparable line.
names() { ( cd "$1" && ls | grep -E "^$2" | sort | tr '\n' ' ' ); }

# create <label> <options...> -- both create into their own directory, then the
# NAMES and the BYTES are compared.
create() {
  local label="$1"; shift
  checked=$((checked + 1))
  rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
  ( cd "$W/src" && "$REF"  a --nodates -y "$@" "$W/r/x.arc" . ) </dev/null >/dev/null 2>&1
  ( cd "$W/src" && "$PORT" a --nodates -y "$@" "$W/p/x.arc" . ) </dev/null >/dev/null 2>&1
  local rn pn
  rn="$(names "$W/r" x)"; pn="$(names "$W/p" x)"
  if [ "$rn" != "$pn" ]; then
    echo "  DIFF [$label]: names differ, reference [$rn] port [$pn]"
    fail=$((fail + 1))
    return
  fi
  for f in "$W/r"/x*; do
    local b; b="$(basename "$f")"
    if ! cmp -s "$f" "$W/p/$b"; then
      echo "  DIFF [$label]: $b differs, $(size "$f") vs $(size "$W/p/$b") bytes"
      fail=$((fail + 1))
      return
    fi
  done
}

for m in -m0 -m1 -m4; do
  create "sfx $m"          "$m" "-sfx$MODULE"
  create "no sfx $m"       "$m"
  create "volumes 20k $m"  "$m" -v20k
  create "volumes 8k $m"   "$m" -v8k
  create "volumes 1m $m"   "$m" -v1m
  create "sfx+volumes $m"  "$m" "-sfx$MODULE" -v1m
done

# The three -sfx forms on an EXISTING archive, where the name changes after
# writing rather than before.
for opt in "" "-sfx-" "-sfx$MODULE"; do
  checked=$((checked + 1))
  rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
  ( cd "$W/src" && "$REF" a --nodates -y -m1 "-sfx$MODULE" "$W/r/x.arc" . ) >/dev/null 2>&1
  ( cd "$W/src" && "$REF" a --nodates -y -m1 "-sfx$MODULE" "$W/p/x.arc" . ) >/dev/null 2>&1
  ( cd "$W/r" && "$REF"  ch --nodates -y -m0 --noarcext $opt "$W/r/x" ) </dev/null >/dev/null 2>&1
  ( cd "$W/p" && "$PORT" ch --nodates -y -m0 --noarcext $opt "$W/p/x" ) </dev/null >/dev/null 2>&1
  rn="$(names "$W/r" x)"; pn="$(names "$W/p" x)"
  if [ "$rn" != "$pn" ]; then
    echo "  DIFF [ch ${opt:-default}]: names differ, reference [$rn] port [$pn]"
    fail=$((fail + 1))
    continue
  fi
  for f in "$W/r"/x*; do
    b="$(basename "$f")"
    cmp -s "$f" "$W/p/$b" || {
      echo "  DIFF [ch ${opt:-default}]: $b differs, $(size "$f") vs $(size "$W/p/$b")"
      fail=$((fail + 1))
    }
  done
done

# `addArcExtension`: an extensionless name gets `.arc`, for every command.
checked=$((checked + 1))
rm -rf "$W/r" "$W/p"; mkdir -p "$W/r" "$W/p"
( cd "$W/src" && "$REF"  a --nodates -y -m1 "$W/r/plain" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m1 "$W/p/plain" . ) >/dev/null 2>&1
if [ "$(names "$W/r" plain)" != "$(names "$W/p" plain)" ]; then
  echo "  DIFF [addArcExtension]: reference [$(names "$W/r" plain)] port [$(names "$W/p" plain)]"
  fail=$((fail + 1))
fi

echo "arc sfx/volumes: $checked runs, $fail differing"
[ "$fail" -eq 0 ] || exit 1
[ "$checked" -gt 0 ] || { echo "nothing was compared" >&2; exit 1; }

# ── the comparison must be able to fail ─────────────────────────────────────

rm -rf "$W/s"; mkdir -p "$W/s"
( cd "$W/src" && "$PORT" a --nodates -y -m1                "$W/s/plain.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m1 "-sfx$MODULE"  "$W/s/withsfx.arc" . ) >/dev/null 2>&1

# 1. The stub must actually be there, and the archive must be bigger by its size.
if [ ! -f "$W/s/withsfx" ]; then
  echo "SELF-TEST FAILED: -sfx did not rename withsfx.arc to withsfx" >&2
  exit 1
fi
plain=$(size "$W/s/plain.arc"); withsfx=$(size "$W/s/withsfx"); stub=$(size "$MODULE")
if [ "$((withsfx - plain))" != "$stub" ]; then
  echo "SELF-TEST FAILED: the SFX archive is $((withsfx - plain)) bytes larger," >&2
  echo "not the module's $stub -- the stub was not prepended whole" >&2
  exit 1
fi
# …and it must be the module's own bytes, at offset 0. `head -c`, not `cmp -n`:
# BSD cmp has no -n, so that flag is REJECTED rather than honoured and the
# check fails for a reason that has nothing to do with the archive.
head -c "$stub" "$W/s/withsfx" > "$W/s/prefix.bin"
if ! cmp -s "$MODULE" "$W/s/prefix.bin"; then
  echo "SELF-TEST FAILED: the archive does not start with the module's bytes" >&2
  exit 1
fi
# The reference must still read it.
if ! "$REF" t --noarcext "$W/s/withsfx" >/dev/null 2>&1; then
  echo "SELF-TEST FAILED: the reference cannot test the port's SFX archive" >&2
  exit 1
fi

# 2. Volumes must actually split, and rejoin to the unsplit archive.
rm -rf "$W/v"; mkdir -p "$W/v"
( cd "$W/src" && "$PORT" a --nodates -y -m1       "$W/v/whole.arc" . ) >/dev/null 2>&1
( cd "$W/src" && "$PORT" a --nodates -y -m1 -v8k  "$W/v/split.arc" . ) >/dev/null 2>&1
if [ -f "$W/v/split.arc" ]; then
  echo "SELF-TEST FAILED: -v left the unsplit archive behind" >&2
  exit 1
fi
n=$(ls "$W/v" | grep -c 'split.arc.[0-9]' || true)
if [ "$n" -lt 2 ]; then
  echo "SELF-TEST FAILED: -v8k produced $n volume(s), so nothing was split" >&2
  exit 1
fi
cat "$W/v"/split.arc.* > "$W/v/rejoined.arc"
if ! cmp -s "$W/v/rejoined.arc" "$W/v/whole.arc"; then
  echo "SELF-TEST FAILED: the volumes do not rejoin to the unsplit archive" >&2
  exit 1
fi

echo "the Rust arc writes SFX archives and volumes exactly as the Haskell one does"
