#!/usr/bin/env bash
# Round-trip archives through the standalone extractor and through a real
# self-extracting archive.
#
#   sfx-roundtrip.sh <arc> <unarc> <sfx-module>
#
# Unarc and the SFX modules are a SECOND, independent implementation of the
# reader: they share the codecs with `arc` but not the archive-structure
# parser, which is Haskell in the archiver and C++ in ArcStructure.h. Nothing
# else in CI exercises that second parser, and it is what rotted -- it was
# reading the file-time field as 4 bytes while ByteStream.hs had long been
# writing 8, so every field after it (the directory flags and the CRCs) came
# out of the wrong offset.
#
# Two things are checked, and they are not the same:
#   1. `unarc x` reproduces the tree, for every method the archiver can pick.
#   2. An archive with a module attached extracts itself when RUN.
# The second is the feature; the first is what tells you which layer broke.
set -u
ARC="$1"; UNARC="$2"; SFX="$3"
for f in "$ARC" "$UNARC" "$SFX"; do
  [ -f "$f" ] || { echo "missing: $f" >&2; exit 1; }
done
# Absolute, because everything below runs from inside the work directory: a
# relative "Tests/arc" resolves to nothing there, and the failure reads as
# "the archiver could not create it" rather than "the path was wrong".
abspath() { ( cd "$(dirname "$1")" && printf '%s/%s\n' "$(pwd)" "$(basename "$1")" ); }
ARC=$(abspath "$ARC"); UNARC=$(abspath "$UNARC"); SFX=$(abspath "$SFX")
W="${TMPDIR:-/tmp}/sfx-rt.$$"; rm -rf "$W"; mkdir -p "$W/in/sub"
trap 'rm -rf "$W"' EXIT

# A nested directory is not decoration: directory entries are stored as
# zero-byte members with a separate flag, and reading that flag from the wrong
# offset produces a zero-byte FILE that the next member cannot be created
# under. A flat corpus never notices.
echo "the quick brown fox" > "$W/in/a.txt"
head -c 40000 /dev/urandom     > "$W/in/b.bin"
python3 -c "import sys; open(sys.argv[1],'w').write('hello world '*5000)" "$W/in/sub/c.txt"
printf '\x7fELF\x02\x01\x01%.0s' $(seq 1 500) > "$W/in/sub/d.exe"

fail=0; tested=0

# Every method a user can select, not just the defaults: the presets chain
# several codecs together, and a module missing any ONE of them fails the whole
# chain. That is how 4x4, BSC, DisPack, LZ4 and Zstd turned out to be absent
# from the module's link list.
#
# `dict` and `lzp` are named individually even though -m9 already chains Dict,
# because -m9 only reaches it when the *corpus* is classified $text by
# detect_datatype -- and the four files above are too small and too artificial
# to be. So -m9 passed this test for as long as Dict was completely unreadable
# here: unarc reported "archive data corrupted" for every real -m9 archive
# containing a text file, and nothing in CI noticed. Name the codec, do not
# trust a preset to reach it.
for m in 0 1 2 3 4 5 9 x tor lzma ppmd grzip bsc dispack lz4 zstd dict lzp; do
  rm -f "$W/t.arc"; rm -rf "$W/out"; mkdir -p "$W/out"
  if ! ( cd "$W" && "$ARC" a --nodates -r -y -m$m t.arc in ) >"$W/c.log" 2>&1; then
    echo "  -m$m: the ARCHIVER failed to create it"; tail -2 "$W/c.log" | sed 's/^/     /'
    fail=$((fail+1)); continue
  fi
  tested=$((tested+1))
  if ! ( cd "$W/out" && "$UNARC" x "$W/t.arc" -o+ ) >"$W/x.log" 2>&1 </dev/null; then
    echo "  -m$m: unarc failed:$(tr -d '\n' < "$W/x.log" | tail -c 70)"; fail=$((fail+1)); continue
  fi
  if diff -r "$W/in" "$W/out/in" >/dev/null 2>&1; then echo "  -m$m: OK"
  else echo "  -m$m: extracted tree differs"; fail=$((fail+1)); fi
  [ -d "$W/out/in/sub" ] || { echo "  -m$m: 'in/sub' is not a directory"; fail=$((fail+1)); }
done

[ "$tested" -gt 0 ] || { echo "no archives were created -- nothing was tested"; exit 1; }

# The listing path, separately: it reads the same directory block but none of
# the compressed data, so a structure-parsing regression shows up here even
# when every codec is fine.
n=$( "$UNARC" l "$W/t.arc" 2>/dev/null | grep -c . )
[ "$n" -gt 4 ] || { echo "  unarc l printed $n lines -- it is not reading the directory"; fail=$((fail+1)); }

# ── The feature itself ──────────────────────────────────────────────────────
rm -f "$W/sfx.arc" "$W/sfx"; rm -rf "$W/sx"; mkdir -p "$W/sx"
( cd "$W" && "$ARC" a --nodates -r -y -m4 sfx.arc in ) >"$W/s.log" 2>&1 \
  || { echo "  sfx: create failed"; tail -3 "$W/s.log" | sed 's/^/     /'; exit 1; }
# On Unix aDEFAULT_SFX_EXTENSION is empty, so `ch -sfx=` renames sfx.arc to sfx.
( cd "$W" && "$ARC" ch -y -sfx="$SFX" sfx.arc ) >>"$W/s.log" 2>&1 \
  || { echo "  sfx: attaching the module failed"; tail -3 "$W/s.log" | sed 's/^/     /'; exit 1; }
out="$W/sfx"; [ -f "$out" ] || out="$W/sfx.arc"
[ -f "$out" ] || { echo "  sfx: no output archive was produced"; exit 1; }

# The module is prepended, so the result must be BIGGER than the module alone
# -- a check that a zero-length copy cannot pass.
[ "$(wc -c < "$out")" -gt "$(wc -c < "$SFX")" ] \
  || { echo "  sfx: output is no larger than the module itself"; fail=$((fail+1)); }

cp "$out" "$W/sx/selfx"; chmod +x "$W/sx/selfx"
if ( cd "$W/sx" && ./selfx -x -y -s1 ) >"$W/r.log" 2>&1 </dev/null; then
  if diff -r "$W/in" "$W/sx/in" >/dev/null 2>&1
    then echo "  sfx: OK -- the archive extracted ITSELF"
    else echo "  sfx: it ran but the tree differs"; fail=$((fail+1)); fi
else
  echo "  sfx: running the archive failed:$(tr -d '\n' < "$W/r.log" | tail -c 70)"; fail=$((fail+1))
fi

# And `arc` must still read an archive that has a module glued to its front.
# --noarcext because the SFX archive has no extension on Unix
# (aDEFAULT_SFX_EXTENSION is empty there), and without it the archiver appends
# ".arc" and looks for a file that does not exist.
rm -rf "$W/back"; mkdir -p "$W/back"
"$ARC" x -y --noarcext -dp"$W/back" "$out" >"$W/b.log" 2>&1 </dev/null \
  && echo "  sfx: the archiver still reads it" \
  || { echo "  sfx: the ARCHIVER can no longer read its own SFX archive"; fail=$((fail+1)); }

[ "$fail" -eq 0 ] && echo "ALL OK ($tested methods + self-extraction)" || { echo "$fail FAILED"; exit 1; }
