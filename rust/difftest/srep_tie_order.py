#!/usr/bin/env python3
"""Decide whether two .7z-era SREP Future-LZ streams differ ONLY by the order of
match records that share a source position.

Why this exists
---------------

`srep.cpp:756` sorts the collected matches with

    std::sort (lz_matches, lz_matches+lz_matches_count, order_by_LZ_match_src);

and `order_by_LZ_match_src` (:85) compares `src` alone. `std::sort` is **not
stable**, so when two matches share a source the C++ standard library decides
their relative order, and nothing in the format or the algorithm pins it.

Measured on this repo's macOS/libc++ build: of five corpus inputs containing a
tied source, four came out in collection order and one -- `runs`, whose stream
has 240 records -- came out reversed. libc++'s introsort insertion-sorts small
ranges, which is stable, and only perturbs ties once quicksort partitioning
engages. A libstdc++ build can therefore produce a *different* archive from the
same input, so byte-identity across toolchains is not a property the C has here.

Demanding the port reproduce it would mean reimplementing one particular
libc++'s introsort, and would still be wrong on Linux. So the harness requires
byte-identity, and treats this one difference as a pass:

    exit 0  the streams are equal as multisets of records per block, i.e. the
            only difference is the order of records sharing a source
    exit 1  anything else -- a different record, a different count, a different
            header, or a difference outside the record area

Everything else stays a failure. In particular the record multiset, every block
header, every hash and all literal bytes must match exactly.
"""

import struct
import sys


def parse(path):
    """Return (archive_header_words, [(block_header, records, literals)])."""
    d = open(path, "rb").read()
    if len(d) < 16:
        raise ValueError(f"{path}: too short to hold an archive header")
    w = lambda o: struct.unpack_from("<I", d, o)[0]

    hdr = [w(0), w(4), w(8), w(12)]
    hash_seed_size = (hdr[2] >> 16) & 255
    hash_size = ((hdr[2] >> 24) + 16) & 255
    base_len = hdr[3]

    off = 16 + hash_seed_size
    blocks = []
    start = 0
    while off < len(d):
        if off + 12 + hash_size > len(d):
            raise ValueError(f"{path}: truncated block header at {off}")
        literal_bytes, blen, stat_size = w(off), w(off + 4), w(off + 8)
        bh = (literal_bytes, blen, stat_size, d[off + 12 : off + 12 + hash_size])
        off += 12 + hash_size

        # Records are 4 words each: lit_len, offset_lo, offset_hi, len.
        # Future-LZ records are SOURCE-anchored: src = block_pos + lit_len.
        end = off + stat_size
        pos = start
        recs = []
        while off < end:
            lit, lo, hi, ln = w(off), w(off + 4), w(off + 8), w(off + 12)
            off += 16
            src = pos + lit
            recs.append((src, src + (lo | (hi << 32)), ln * 1 + base_len))
            pos = src
        literals = d[off : off + literal_bytes]
        off += literal_bytes
        blocks.append((bh, recs, literals))
        start += blen
    return hdr, blocks


def main(argv):
    if len(argv) != 3:
        print("usage: srep_tie_order.py <a.srep> <b.srep>", file=sys.stderr)
        return 2
    try:
        ha, ba = parse(argv[1])
        hb, bb = parse(argv[2])
    except Exception as e:  # a stream we cannot parse is a failure, not a pass
        print(f"  tie-check: cannot parse ({e})")
        return 1

    if ha != hb:
        print("  tie-check: archive headers differ")
        return 1
    if len(ba) != len(bb):
        print(f"  tie-check: block count differs ({len(ba)} vs {len(bb)})")
        return 1

    reordered = 0
    for i, (a, b) in enumerate(zip(ba, bb)):
        (bha, ra, la), (bhb, rb, lb) = a, b
        if bha != bhb:
            print(f"  tie-check: block {i} header or hash differs")
            return 1
        if la != lb:
            print(f"  tie-check: block {i} literal bytes differ")
            return 1
        if ra == rb:
            continue
        if sorted(ra) != sorted(rb):
            print(f"  tie-check: block {i} record SETS differ -- a real divergence")
            return 1
        # Same multiset, different order. Confirm every difference is confined
        # to a group of records sharing a source; anything else is real.
        by_src_a, by_src_b = {}, {}
        for s, dst, ln in ra:
            by_src_a.setdefault(s, []).append((dst, ln))
        for s, dst, ln in rb:
            by_src_b.setdefault(s, []).append((dst, ln))
        if sorted(by_src_a) != sorted(by_src_b):
            print(f"  tie-check: block {i} source positions differ")
            return 1
        for s in by_src_a:
            if sorted(by_src_a[s]) != sorted(by_src_b[s]):
                print(f"  tie-check: block {i} src={s} records differ")
                return 1
            if by_src_a[s] != by_src_b[s]:
                reordered += 1

    print(f"  tie-check: identical except {reordered} tied-source group(s) reordered")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
