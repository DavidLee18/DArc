#!/usr/bin/env bash
# `-mm`, `-mc`, `-ma`, and the file-type routing all three operate on.
#
# These are one subject. `-ma` decides whether file types are probed at all,
# `-mm` adds or removes the `$wav`/`$bmp` entries
# of the decoded compressor list, `-mc` removes entries and links from it, and
# neither means anything unless files actually REACH those entries — which is
# `getDefaultType`, from the groups file.
#
# That last part is why this file exists. The port hardcoded `$binary` as every
# file's default type, so `$wav` was unreachable, the multimedia branch in
# `filetype::classify` was dead code, and `-m9` compressed a WAV with the
# general binary chain where the reference used `tta`. Nothing caught it,
# because every earlier harness ran WITHOUT a groups file — and with no groups
# file the hardcoded answer is the correct one.
#
# So: every case here passes --groups. A run of these cases without it compares
# two binaries that both do nothing interesting, and passes.
#
# Usage:  arc-multimedia-check.sh /path/to/arc-ghc
set -uo pipefail

REF="${1:-}"
[ -n "$REF" ] || { echo "usage: $0 <reference-arc>" >&2; exit 2; }
[ -x "$REF" ] || { echo "no reference binary at $REF" >&2; exit 2; }
case "$REF" in /*) ;; *) REF="$(cd "$(dirname "$REF")" && pwd)/$(basename "$REF")" ;; esac

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
( cd "$ROOT/rust" && cargo build --release -q -p darc-arc --bin darc ) || exit 1
PORT="$ROOT/rust/target/release/darc"
# NOT named GROUPS: bash reserves that as a read-only array of the user's
# group ids, so the assignment is ignored and the variable reads back as a
# numeric gid. The check below then reported "no groups file at 20".
GROUPFILE="$ROOT/Tests/darc.groups"
[ -f "$GROUPFILE" ] || { echo "no groups file at $GROUPFILE" >&2; exit 2; }

W="${TMPDIR:-/tmp}/arc-mm-check.$$"; mkdir -p "$W/c"
trap 'rm -rf "$W"' EXIT
cd "$W/c" || exit 1

for i in 1 2; do
  awk -v n="$i" 'BEGIN{for(j=0;j<30000;j++) printf "block %d row %d text\n", n, j%777}' > "f$i.txt"
done
# A real WAV and a real BMP. The extension is what the groups file matches on,
# and the CONTENT is what detectMM then probes -- both halves are needed, so a
# file named .wav full of text would not exercise this.
{ printf 'RIFF'; printf '\x24\x40\x00\x00'; printf 'WAVEfmt '
  printf '\x10\x00\x00\x00\x01\x00\x02\x00\x44\xac\x00\x00\x10\xb1\x02\x00\x04\x00\x10\x00'
  printf 'data'; printf '\x00\x40\x00\x00'
  awk 'BEGIN{for(i=0;i<4096;i++){v=int(9000*sin(i/17));printf "%c%c%c%c", v%256,int(v/256)%256,v%256,int(v/256)%256}}'
} > s.wav
{ printf 'BM'
  printf '\x36\x0c\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x28\x00\x00\x00\x40\x00\x00\x00'
  printf '\x40\x00\x00\x00\x01\x00\x18\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x13\x0b\x00\x00'
  printf '\x13\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00'
  awk 'BEGIN{for(i=0;i<4096;i++) printf "%c%c%c", i%256,(i*3)%256,(i*7)%256}'
} > s.bmp

sha() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

pass=0; fail=0; hashes=()
g() {
  local name="$1"; shift
  rm -f "$W/r.arc" "$W/p.arc"
  "$REF"  a --nodates -y -r --groups="$GROUPFILE" "$@" "$W/r.arc" . >/dev/null 2>&1
  "$PORT" a --nodates -y -r --groups="$GROUPFILE" "$@" "$W/p.arc" . >/dev/null 2>&1
  if [ ! -f "$W/r.arc" ]; then echo "SKIP $name -- the reference wrote no archive"; return; fi
  if [ ! -f "$W/p.arc" ]; then printf 'DIFF [%s]: the port wrote no archive\n' "$name"; fail=$((fail+1)); return; fi
  local r p; r="$(sha "$W/r.arc")"; p="$(sha "$W/p.arc")"
  hashes+=("$r $name")
  if [ "$r" = "$p" ]; then pass=$((pass+1))
  else printf 'DIFF [%s]: expected %s, got %s\n' "$name" "$r" "$p"; fail=$((fail+1)); fi
}

# The bases are the point as much as the options are: they are what failed
# before getDefaultType existed, with no -mm or -mc anywhere on the line.
g base-m9   -m9
g base-m4   -m4
g mm-off    -m9 -mm-
g mm-fast   -m9 -mmfast
g mm-max    -m9 -mmmax
g mm-plus   -m9 -mm+
g mm-off-m4 -m4 -mm-
g mm-max-m4 -m4 -mmmax
g mc-tta    -m9 -mc-tta
g mc-rep    -m4 -mc-rep
g mc-exe    -m4 -mc-exe
g mc-two    -m4 -mc-rep -mc-exe

# `-ma` -- the autodetection LEVEL. Two behaviours only, either side of
# `detect_level <= 1`: at 0 or 1 the reference partitions by the type the
# groups file assigns and probes nothing; at 2 and above it probes contents.
# `-ma` unset tracks the compression level, so -m1 never probes and -m4 does.
g ma-off      -m4 -ma-
g ma-0        -m4 -ma0
g ma-1        -m4 -ma1
g ma-2        -m4 -ma2
g ma-9        -m4 -ma9
g ma-m2-1     -m2 -ma1
g ma-m9-0     -m9 -ma0
g ma-m9-2     -m9 -ma2
# -s- is the case that caught the ordering: with solid off the sorted list
# starts with a binary file, so a first-appearance bucket order writes the
# blocks the wrong way round. merge_by_type sorts by the CHAIN STRING.
g ma-solid-off -m5 -ma1 -s-
g ma-s1k      -m4 -ma0 -s1k

echo "arc multimedia: $((pass+fail)) cases, $fail differing"
u=$(printf '%s\n' "${hashes[@]}" | cut -d' ' -f1 | sort -u | wc -l | tr -d ' ')
echo "distinct reference archives: $u of ${#hashes[@]}"
# Collisions here are EXPECTED and are themselves a result: -m9's preset
# already selects max multimedia, so -mmmax and -mm+ are no-ops on it. That is
# the evidence for "-mm overrides a choice the preset already made" rather than
# "-mm turns multimedia on". Only -mm- and -mmfast move -m9.

# The self-test: -mm- must change the archive. If it does not, the groups file
# is not being applied and every case above is comparing two no-ops.
rm -f "$W/r.arc" "$W/p.arc"
"$PORT" a --nodates -y -r --groups="$GROUPFILE" -m9      "$W/r.arc" . >/dev/null 2>&1
"$PORT" a --nodates -y -r --groups="$GROUPFILE" -m9 -mm- "$W/p.arc" . >/dev/null 2>&1
if [ "$(sha "$W/r.arc")" = "$(sha "$W/p.arc")" ]; then
  echo "SELF-TEST FAILED: -mm- changed nothing, so \$wav/\$bmp are unreachable" >&2
  exit 1
fi

[ "$fail" -eq 0 ] || exit 1
echo "the Rust arc routes file types and applies -mm/-mc as the Haskell one does"
