#!/usr/bin/env bash
# Generate the deterministic test corpus.
#
# Everything here is produced from fixed seeds and fixed byte patterns, so the
# corpus is identical on every machine and every run. That matters because the
# archive fingerprints in fingerprints.txt are taken over archives built from
# it -- if the corpus drifted, every fingerprint would drift with it and the
# format check would be worthless.
#
# Timestamps are forced to a fixed date for the same reason; combined with
# --nodates on the archiver side it makes archive bytes reproducible.
set -eu

# LC_ALL=C is REQUIRED, not tidiness. The binary files below are written with
# awk's printf "%c", and in a UTF-8 locale gawk treats the argument as a
# character rather than a byte: every value >= 0x80 comes out as a two-byte
# UTF-8 sequence. mawk (Ubuntu's default awk) and BSD awk emit a single byte
# regardless, so the corpus differed only on machines that had gawk installed
# *and* a UTF-8 locale -- which is exactly the GitHub runner image, LANG set
# to C.UTF-8.
#
# The sizes make it unmistakable: pattern.bin is all bytes >= 0x80 and doubled,
# 80000 -> 160000; random1.bin has about half its bytes >= 0x80 and grew
# 65536 -> 98332; zeros.bin comes from /dev/zero and was unaffected. CI was
# therefore building its archives over a different corpus than every local run,
# and the files meant to be incompressible were UTF-8 text.
#
# This predates the fingerprint baseline and was invisible until the format
# check was actually enforced, at which point it presented as "the archiver is
# not deterministic on Linux".
export LC_ALL=C

DIR="${1:-corpus}"
rm -rf "$DIR"
mkdir -p "$DIR"

# --- deterministic pseudo-random bytes, no external tools -------------------
# A tiny LCG in awk. Not good randomness, but we need *repeatable* bytes, not
# good ones, and this avoids depending on /dev/urandom, openssl, or python.
prng () {  # prng <seed> <bytes>
  awk -v seed="$1" -v n="$2" 'BEGIN{
    s=seed
    for(i=0;i<n;i++){ s=(s*1103515245+12345)%2147483648; printf "%c", int(s/65536)%256 }
  }'
}

mkdir -p "$DIR/text" "$DIR/binary" "$DIR/many" "$DIR/edge" "$DIR/nested/a/b/c"

# --- highly compressible text ----------------------------------------------
# Exercises the text-model codecs (ppmd, grzip, dict, lzp) and long-match
# finding (rep, tor).
for i in 1 2 3; do
  { for _ in $(seq 200); do
      echo "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs."
    done
  } > "$DIR/text/repetitive$i.txt"
done

# Text with structure but low repetition -- closer to real prose.
awk 'BEGIN{
  for(i=0;i<2000;i++) printf "line %d: value=%d status=%s\n", i, i*7919%1000, (i%3==0?"ok":"pending")
}' > "$DIR/text/structured.txt"

# --- incompressible binary --------------------------------------------------
# Should end up stored, not compressed. Catches regressions in the
# "don't waste time on incompressible data" path and in -ms handling.
prng 12345 65536  > "$DIR/binary/random1.bin"
prng 98765 32768  > "$DIR/binary/random2.bin"

# --- highly compressible binary ---------------------------------------------
head -c 100000 /dev/zero > "$DIR/binary/zeros.bin"
# Decimal, not 0xDE/0xAD/0xBE/0xEF: hex literals are a gawk extension. mawk and
# BSD awk parse "0xDE" as 0, and printf "%c" with 0 emits the character "0", so
# on those awks this file was 80000 bytes of ASCII 0x30 -- highly compressible,
# and the exact opposite of the repeating binary pattern it is here to provide.
# gawk produced the intended bytes, so the corpus depended on which awk was
# installed. 222,173,190,239 are the same values, spelled portably.
awk 'BEGIN{for(i=0;i<20000;i++) printf "%c%c%c%c", 222,173,190,239}' > "$DIR/binary/pattern.bin"

# --- table-structured data --------------------------------------------------
# Exercises the multimedia/delta/table detectors (mm, tta, delta, dispack).
awk 'BEGIN{for(i=0;i<8000;i++){v=i*3; printf "%c%c%c%c", v%256,int(v/256)%256,0,0}}' \
  > "$DIR/binary/table32.bin"

# --- many small files -------------------------------------------------------
# Exercises solid blocking, the file-list diff/merge engine, and directory
# encoding. 200 files is enough to cross grouping thresholds without making
# CI slow.
for i in $(seq -w 1 200); do
  echo "small file $i containing a short line of text" > "$DIR/many/f$i.txt"
done

# --- edge cases -------------------------------------------------------------
: > "$DIR/edge/empty.txt"                                  # zero bytes
printf 'no trailing newline' > "$DIR/edge/no-newline.txt"
printf 'a' > "$DIR/edge/one-byte.txt"
echo "unicode content: naive cafe zurich zoo" > "$DIR/edge/ascii-name.txt"
echo "content" > "$DIR/edge/name with spaces.txt"
echo "content" > "$DIR/edge/name.with.many.dots.txt"
printf 'line1\r\nline2\r\n' > "$DIR/edge/crlf.txt"          # CRLF must survive verbatim

# --- nested directories -----------------------------------------------------
echo "depth 3" > "$DIR/nested/a/b/c/deep.txt"
echo "depth 1" > "$DIR/nested/a/shallow.txt"

# --- fix all timestamps so nothing depends on when this ran -----------------
find "$DIR" -exec touch -t 202001010000.00 {} +

echo "corpus generated in $DIR"
find "$DIR" -type f | wc -l | xargs echo "  files:"
du -sh "$DIR" 2>/dev/null | cut -f1 | xargs echo "  size: "
