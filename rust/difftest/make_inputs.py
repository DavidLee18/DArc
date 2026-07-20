"""Table-shaped inputs for the Delta differential test.

These exist because of a measurement, not a guess. An input set of random data,
a real binary, and a few degenerate sizes matched 8/8 between C and Rust and
looked like a thorough pass -- but when the Rust carry handling was broken on
purpose, only 1 of those 8 noticed. For random and binary data the compressor
finds almost no tables, so undiff_table barely runs and a wrong answer never
shows up in the output.

The inputs below are built so the transforms actually run: multi-byte
little-endian counters whose consecutive rows differ by small amounts, which is
what makes byte-wise subtraction borrow across the bytes of a column. With these
added, 4 of 8 detect the same broken carry.
"""
import os
import random
import struct
import sys

d = sys.argv[1]


def w(name, b):
    with open(os.path.join(d, name), "wb") as f:
        f.write(bytes(b))


# Counters of several element widths: the carry path, at several table shapes.
for n_bytes, rows in ((2, 20000), (3, 15000), (4, 20000), (8, 10000), (16, 6000)):
    out = bytearray()
    for i in range(rows):
        v = (i * 7 + 1000) & ((1 << (8 * n_bytes)) - 1)
        out += v.to_bytes(n_bytes, "little")
    w(f"counter{n_bytes}.bin", out)

# Monotonic and constant columns together: exercises the diff path, the column
# reordering, and unreorder_table's early return when every column is one kind.
out = bytearray()
for i in range(20000):
    out += struct.pack("<IIII", i * 3, 0xDEADBEEF, i * 65537, 0x11223344)
w("mixed-cols.bin", out)

# A column that increments past 2^24, so carries propagate the full element width.
out = bytearray()
for i in range(20000):
    out += struct.pack("<QQ", 0x0100000000 + i * 13, i * 257)
w("wide-carry.bin", out)

# A table buried in noise, so table-boundary detection has to find its extent
# rather than being handed a block that is entirely table.
random.seed(1234)
out = bytearray(random.randbytes(30000))
for i in range(10000):
    out += struct.pack("<IIII", i * 5, i * 9 + 3, 0xCAFEBABE, i)
out += random.randbytes(30000)
w("embedded.bin", out)


# ---------------------------------------------------------------------------
# Inputs aimed at the compressor's heuristics.
#
# The first corpus here matched C byte-for-byte on 14/14 inputs and still could
# not detect four deliberately introduced errors, including a wrong reading of
#     difflb < itemlb? len++,omit=0 : useless++,omit++;
# and a changed acceptance threshold. Counting the tables actually detected
# explained it: 14 tables across 14 inputs, all of them perfectly monotonic, so
# search_for_table_boundary never took its direction-change branch -- which is
# the only place `omit` and `lastpoint` are used -- and the threshold was never
# near enough to matter.
#
# These are built to make those paths run: direction reversals, wrap-around,
# many small tables at varied widths and spacings, and table sizes that sit
# either side of the acceptance test.
# ---------------------------------------------------------------------------

def le(v, w):
    return (v & ((1 << (8 * w)) - 1)).to_bytes(w, "little")


# Columns that climb then fall, repeatedly: forces the direction-change branch,
# which is where `omit`, `lastpoint` and the bad-run counter come into play.
out = bytearray()
for i in range(30000):
    phase = (i // 40) % 2
    v = i * 11 if phase == 0 else 500000 - i * 11
    out += le(v, 4) + le(i * 3, 4)
w("zigzag.bin", out)

# Counters that wrap, producing a sign flip in the int16 view every 2^16.
out = bytearray()
for i in range(40000):
    out += le(i * 2731, 4) + le(i * 65533, 4)
w("wrapping.bin", out)

# Many independent small tables of differing widths, separated by noise of
# differing lengths, so the skip field -- and therefore skipBits in the
# acceptance test -- takes a wide range of values.
random.seed(99)
out = bytearray()
for t, width in enumerate((2, 3, 4, 5, 6, 8, 12, 16)):
    for rep in range(6):
        out += random.randbytes(17 + t * 29 + rep * 7)
        rows = 40 + rep * 25
        for i in range(rows):
            out += le(i * (t + 2) + rep, width)
w("many-tables.bin", out)

# Tables sized either side of the acceptance test, so a shifted threshold
# changes which of them are accepted.
out = bytearray()
for rows in range(8, 90, 3):
    out += random.randbytes(23)
    for i in range(rows):
        out += le(i * 5 + 7, 4)
w("threshold-edge.bin", out)

# Runs that are monotonic but only briefly, so the len>=4 test and the
# consecutive-short-run counter both matter.
out = bytearray()
for i in range(30000):
    seg = i // 3
    out += le(seg * 17 + (i % 3) * 4099, 4)
w("short-runs.bin", out)

# A fine sweep across the acceptance test. The coarser threshold-edge.bin above
# stepped rows by 3 and never landed a candidate close enough to the boundary
# for a 30 -> 29 change in
#     useful*sqrt(N) > 30 + 4*skipBits
# to alter any decision -- that sabotage went undetected by all 13 inputs. This
# steps one row at a time at several widths and several skip distances, so some
# candidate sits exactly on the line whichever way it is nudged.
for width in (2, 3, 4, 6, 8):
    out = bytearray()
    for gap in (5, 40, 300, 2000):
        for rows in range(6, 70):
            out += random.randbytes(gap)
            for i in range(rows):
                out += le(i * 3 + 11, width)
    w(f"threshold-sweep{width}.bin", out)
