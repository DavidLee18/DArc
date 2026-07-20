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
