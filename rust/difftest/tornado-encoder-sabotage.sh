#!/usr/bin/env bash
# Does the Tornado encoder round-trip suite actually exercise the encoders?
#
# The suite went green on its first run. That is the situation in which a test
# has most often turned out here to be passing without running the code it
# names, so each encoder path gets a deliberate one-line break and the suite has
# to notice. A sabotage that survives means the test does not reach that code.
#
# Discipline this harness enforces on itself, learned the hard way:
#   * the baseline copy must exist before anything is edited, or a failed
#     restore silently corrupts the tree;
#   * every edit must be confirmed to have applied, or a no-op patch reports
#     "not caught" while nothing was ever broken;
#   * the tree is restored and verified byte-identical at the end, on every
#     exit path.
set -u

cd "$(dirname "$0")/.." || exit 1   # rust/
SRC=darc-codecs/src/tornado
BACKUP=$(mktemp -d)
FAILED=0

for f in range.rs huffman.rs vle.rs lz77_enc.rs out_stream.rs; do
    cp "$SRC/$f" "$BACKUP/$f" || { echo "FATAL: cannot back up $f"; exit 1; }
    [ -s "$BACKUP/$f" ] || { echo "FATAL: backup of $f is empty"; exit 1; }
done

restore() {
    for f in range.rs huffman.rs vle.rs lz77_enc.rs out_stream.rs; do
        cp "$BACKUP/$f" "$SRC/$f" || { echo "FATAL: restore of $f failed"; exit 1; }
        cmp -s "$BACKUP/$f" "$SRC/$f" || { echo "FATAL: $f differs after restore"; exit 1; }
    done
}
trap 'restore; rm -rf "$BACKUP"' EXIT

# sabotage <name> <file> <from> <to> <expect-broken-methods>
sabotage() {
    local name=$1 file=$2 from=$3 to=$4
    restore
    # Confirm the target text is actually present before editing, so a stale
    # pattern cannot masquerade as an uncaught sabotage.
    if ! grep -qF -- "$from" "$SRC/$file"; then
        echo "BROKEN HARNESS: [$name] pattern not found in $file:"
        echo "    $from"
        FAILED=1
        return
    fi
    python3 - "$SRC/$file" "$from" "$to" <<'PY'
import sys, pathlib
p = pathlib.Path(sys.argv[1]); s = p.read_text()
n = s.count(sys.argv[2])
assert n == 1, f"pattern occurs {n} times, need exactly 1"
p.write_text(s.replace(sys.argv[2], sys.argv[3]))
PY
    if [ $? -ne 0 ]; then
        echo "BROKEN HARNESS: [$name] edit did not apply cleanly"
        FAILED=1
        return
    fi
    if ! grep -qF -- "$to" "$SRC/$file"; then
        echo "BROKEN HARNESS: [$name] replacement text absent after edit"
        FAILED=1
        return
    fi

    # Both suites: the round-trips in tests/tornado.rs, and the in-module unit
    # tests that assert the encoder's *choices*. The latter exist because a
    # round-trip is blind to a choice that is merely worse, not wrong.
    local out
    out=$(cargo test -p darc-codecs --test tornado 2>&1; cargo test -p darc-codecs tornado:: 2>&1)
    # Order matters: a failing test also makes cargo print "error: test failed",
    # so the compile check has to be specific and has to come second. Getting
    # this backwards reported every caught sabotage as INCONCLUSIVE.
    if echo "$out" | grep -q "test result: FAILED"; then
        echo "caught:     [$name]"
    elif echo "$out" | grep -q "^error\[E[0-9]\|could not compile"; then
        echo "INCONCLUSIVE: [$name] did not compile (sabotage was not testable)"
        FAILED=1
    else
        echo "SURVIVED:   [$name]  <-- the suite does not reach this code"
        FAILED=1
    fi
}

echo "=== baseline ==="
if cargo test -p darc-codecs --test tornado 2>&1 | grep -q "test result: FAILED" \
   || cargo test -p darc-codecs tornado:: 2>&1 | grep -q "test result: FAILED"; then
    echo "FATAL: clean tree does not pass; nothing below is meaningful"
    exit 1
fi
echo "clean tree passes"

echo
echo "=== sabotages ==="

# ARICODER: the 32-bit truncation in ShiftLow. Widening it is precisely the bug
# that survived a C-vs-Rust diff in GRZip and needed a round-trip to find.
sabotage "ari: ShiftLow widened to 64 bits" range.rs \
    'self.low = ((self.low as u32) << 8) as u64;' \
    'self.low = (self.low << 8) & 0xff_ffff_ffff;'

# ARICODER: the two-piece split for wide bit fields.
sabotage "ari: putlowerbits splits at 16 not 15" range.rs \
    'self.rc.encode(mask32(x, 15), 1, 15);' \
    'self.rc.encode(mask32(x, 16), 1, 16);'

# HUFCODER: symbol codes come from the encoder-side tree walk.
sabotage "huf: encoder code table off by one" huffman.rs \
    'self.code[s] = buf[i].code;' \
    'self.code[s] = buf[i].code + 1;'

# BITCODER + HUFCODER: the bit buffer's spill-over remainder.
sabotage "bits: putbits leftover shifted one too far" out_stream.rs \
    'self.bitbuf = (x as u64) >> (n - self.bitcount);' \
    'self.bitbuf = (x as u64) >> (n - self.bitcount + 1);'

# BITCODER: the combined length/distance symbol layout.
sabotage "bit: len/dist code packed at 4 bits not 5" lz77_enc.rs \
    'self.out.putbits(9, 256 + ((lcode as u32) << 5) + dcode as u32);' \
    'self.out.putbits(9, 256 + ((lcode as u32) << 4) + dcode as u32);'

# BYTECODER: the two-bit flag word that says literal/short/medium/long.
sabotage "byte: medium match flagged as long" lz77_enc.rs \
    'self.flags = self.flags.wrapping_add(self.flagbit.wrapping_mul(2));' \
    'self.flags = self.flags.wrapping_add(self.flagbit.wrapping_mul(3));'

# BYTECODER: the flag word is written behind the data it describes.
sabotage "byte: flag word slot not reserved" lz77_enc.rs \
    'self.out.set_anchor_here();
            self.out.advance(4);' \
    'self.out.set_anchor_here();
            self.out.advance(5);'

# HUFCODER + ARICODER: the rep-distance history shuffle.
sabotage "generic: repdist history shuffles wrong slot" lz77_enc.rs \
    'self.prev[2] = old[1];' \
    'self.prev[2] = old[2];'

# HUFCODER + ARICODER: the length escape window for data tables.
sabotage "generic: length escape shift dropped" lz77_enc.rs \
    'len += 4;' \
    'len += 5;'

# All four: the distance code lookup's three ranges.
sabotage "vle: distance range boundary moved" vle.rs \
    'if d < 512 {' \
    'if d < 511 {'

echo
if [ "$FAILED" -eq 0 ]; then
    echo "RESULT: every sabotage was caught"
else
    echo "RESULT: at least one sabotage survived or the harness broke -- see above"
fi
exit "$FAILED"
