#!/bin/sh
# mm ':r1' byte-reordering round-trip, over the block-boundary edge cases.
#
# The reorder path transposes each block, and the transpose is parameterised by
# the BLOCK LENGTH -- so its edges live at block boundaries, not at file
# boundaries, and a plain corpus round-trip walks straight past them. Two
# properties matter and neither is visible on a "nice" input:
#
#   * the encoder's FIRST block is up to BUFSIZE (1 MB) while later ones are
#     roundDown(BUFFER_SIZE,N) = 64 KB, so the decoder's buffer must grow;
#   * the encoder rounds the first block down to a whole number of samples, so
#     ONLY a final short block can carry a partial sample.
#
# That second one hid a real bug: reorder_bytes indexed its output with
# `i*bufsize/X`, i.e. (i*bufsize)/X, which drifts past i*(bufsize/X) by
# floor(i*s/X) once the tail s = bufsize%X reaches 2. Remainders 0 and 1 are
# identical either way, so the whole failure lived in "final block is at least
# one sample long AND 2+ bytes past a sample boundary" -- which needs an input
# just over 1 MB to reach at all. It failed its CRC when it fired.
#
# So this sweeps s across every residue for each geometry, and pins sizes just
# above the 1 MB first-block cut. Run from Tests/ with ./arc already built.
set -e
cd "$(dirname "$0")"
ARC=./arc
[ -x "$ARC" ] || { echo "no ./arc -- build first"; exit 1; }

WORK=/tmp/mm-reorder-check.$$
mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

BUFSIZE=1048576
fail=0
total=0

# num_chan word_size   (X = num_chan * ceil(word_size/8))
for cfg in "1 8" "1 16" "2 8" "2 16" "2 32" "3 24" "4 16" "6 8" "5 16"; do
  set -- $cfg
  ch=$1; ws=$2
  bs=$(( (ws + 7) / 8 ))
  X=$(( ch * bs ))
  s=0
  while [ $s -lt $X ]; do
    # final block = two whole samples plus an s-byte partial one
    n=$(( BUFSIZE + X * 2 + s ))
    f="$WORK/in.raw"
    python3 -c "
import sys, math
n = $n
b = bytearray()
for i in range(n):
    b.append((int(30000*math.sin(i/50.0)) >> (8*(i%2))) & 0xff)
sys.stdout.buffer.write(bytes(b))
" > "$f"
    a="$WORK/t.arc"
    rm -f "$a"
    ( cd "$WORK" && "$OLDPWD/$ARC" a -m"mm:c$ch:w$ws:o0:r1" -mc- t.arc in.raw ) >/dev/null 2>&1
    total=$(( total + 1 ))
    if "$ARC" t "$a" 2>&1 | grep -q "All OK"; then
      :
    else
      echo "  FAILED  c$ch w$ws  X=$X  tail=$s  size=$n"
      fail=$(( fail + 1 ))
    fi
    s=$(( s + 1 ))
  done
done

# And the case with no geometry at all. When autodetection refuses the input,
# N is 0, the header is a bare '\0' and there is no flags byte to carry the
# reorder bit -- so the encoder must not emit block-length prefixes either. It
# did, and `-mmm:r1` on any text or already-compressed file produced an archive
# that failed its CRC. These run with autodetection ON (no :c/:w) so the refusal
# is real rather than forced.
for kind in text random; do
  f="$WORK/nomm.raw"
  python3 -c "
import sys
if '$kind' == 'text':
    sys.stdout.buffer.write(b'the quick brown fox jumps over the lazy dog. ' * 30000)
else:
    s = 12345; o = bytearray()
    for _ in range(1350000):
        s = (s * 1103515245 + 12345) & 0xffffffff
        o.append((s >> 16) & 0xff)
    sys.stdout.buffer.write(bytes(o))
" > "$f"
  a="$WORK/t.arc"
  rm -f "$a"
  ( cd "$WORK" && "$OLDPWD/$ARC" a -m"mm:r1" -mc- t.arc nomm.raw ) >/dev/null 2>&1
  total=$(( total + 1 ))
  if "$ARC" t "$a" 2>&1 | grep -q "All OK"; then
    :
  else
    echo "  FAILED  autodetect-refused ($kind) with :r1"
    fail=$(( fail + 1 ))
  fi
done

echo "mm:r1 reorder: $(( total - fail ))/$total passed"
[ $fail -eq 0 ] || exit 1
