/*
 * entropy.c
 *
 * Description:  TTA entropy coding functions.
 * Developed by: Alexander Djourik <sasha@iszf.irk.ru>
 *               Pavel Zhilin <pzh@iszf.irk.ru>
 *
 * Copyright (c) 1999-2002 Alexander Djourik. All rights reserved.
 *
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * aint with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Please see the file COPYING in this directory for full copyright
 * information.
 */

#include <stdlib.h>
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <stdlib.h>   // malloc.h is a glibc header; the BSDs put malloc in stdlib.h
#else
#include <malloc.h>
#endif
#include "entropy.h"
#include "ttaenc.h"

#define mymax(a,b)               ((a)>(b)? (a) : (b))

// The bit arrays are addressed as 32-bit words: bit_array_*_bits>>5 is a word
// index and (pos<<2) the matching byte offset. The original code walked them
// with "unsigned long", which is 64 bits on LP64 (64-bit Linux/macOS) -- so
// every word access landed at twice the intended byte offset and the grow-check
// reserved half the bytes actually written. That is the root cause of TTA being
// unusable on 64-bit builds: the encoder overran bit_array_write (a heap
// overflow in put_unary) and produced garbage, which in turn drove the adaptive
// Rice parameter past the end of bit_mask32 in encode_frame. Pin the word type
// to exactly 32 bits so the index arithmetic and the pointer stride agree.
typedef unsigned int tta_word;   /* must be exactly 32 bits wide */

const unsigned long bit_mask32[33] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff
};

const unsigned long bit_shift[40] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000,
    0x80000000, 0x80000000, 0x80000000, 0x80000000,
    0x80000000, 0x80000000, 0x80000000, 0x80000000
};

const unsigned long *shift_16 = bit_shift + 4;

unsigned char *bit_array_read;
unsigned long bit_array_read_size, bit_array_read_bits;

unsigned char *bit_array_write;
unsigned long bit_array_write_size, bit_array_write_bits;

void
init_bit_array_write (void) {
    bit_array_write = (unsigned char *) malloc1d (BASE_SIZE, sizeof(char));
    bit_array_write_bits = 0;
    bit_array_write_size = BASE_SIZE;
}

void
init_bit_array_read (unsigned long size) {
    bit_array_read_size = size;
    // The readers below fetch whole 32-bit words, so a frame whose byte length
    // is not a multiple of 4 has a final partial word. Round the allocation up
    // (plus one spare word) so touching that word stays inside the buffer;
    // malloc1d is calloc, so the padding reads as zero. The bounds in
    // get_binary/get_unary are expressed against this rounded word count.
    bit_array_read = (unsigned char *) malloc1d (((size + 3) & ~3UL) + 4, sizeof(char));
    bit_array_read_bits = 0;
}

// Number of 32-bit words the read buffer covers, rounding the final partial
// word up -- it is backed by the padding allocated above.
#define bit_array_read_words   (((bit_array_read_size) + 3) >> 2)

long
get_len (void) {
    return (bit_array_write_bits >> 3) + ((bit_array_write_bits & 7UL)? 1:0);
}

__inline void
put_binary (unsigned long value, unsigned long bits) {
    unsigned long fbit = bit_array_write_bits & 0x1FUL;
    unsigned long rbit = 32 - fbit;
    unsigned long pos = bit_array_write_bits >> 5;

    if ((pos << 2) + 8 > bit_array_write_size) {
        bit_array_write = (unsigned char *) realloc (bit_array_write, bit_array_write_size += STEP_SIZE);
        if (!bit_array_write) tta_error (MEMORY_ERROR, NULL);
    }
    tta_word *s = ((tta_word *)bit_array_write) + pos;

    *s &= bit_mask32[fbit];
    *s |= (value & bit_mask32[bits]) << fbit;
    if (bits > rbit) *(++s) = value >> rbit;

    bit_array_write_bits += bits;
}

__inline void
put_unary (unsigned long value) {
    unsigned long fbit = bit_array_write_bits & 0x1FUL;
    unsigned long rbit = 32 - fbit;
    unsigned long pos = bit_array_write_bits >> 5;

    // The unary code spans from word "pos" to pos + value/32 + 1, i.e. up to
    // (pos<<2) + value/8 + 8 bytes. The old check added "value" (a bit count)
    // to a byte offset, which happens to be conservative for long codes but
    // under-reserves for short ones -- with value 0 it demanded only (pos<<2)
    // bytes while still writing 8. Compute the real span.
    if ((pos << 2) + (value >> 3) + 8 > bit_array_write_size) {
        bit_array_write = (unsigned char *) realloc (bit_array_write, bit_array_write_size += mymax(STEP_SIZE,value/8+10));
        if (!bit_array_write) tta_error (MEMORY_ERROR, NULL);
    }
    tta_word *s = ((tta_word *)bit_array_write) + pos;

    *s &= bit_mask32[fbit];
    if (value < rbit) *s |= (bit_mask32[value]) << fbit;
    else {
        unsigned long unary = value;
        *s++ |= (bit_mask32[rbit]) << fbit; unary -= rbit;
        for (;unary > 32; unary -= 32) *s++ = bit_mask32[32];
        if (unary) *s = bit_mask32[unary];
    }

    bit_array_write_bits += (value + 1);
}

__inline void
get_binary (unsigned long *value, unsigned long bits) {
    unsigned long fbit = bit_array_read_bits & 0x1FUL;
    unsigned long rbit = 32 - fbit;
    unsigned long pos = bit_array_read_bits >> 5;
    tta_word *s = ((tta_word *) bit_array_read) + pos;

    *value = 0;

    // pos is a word index and bit_array_read_size a byte count, so the original
    // guard was off by sizeof(word) and effectively never fired. Compare like
    // for like, and check the second word separately since it is only touched
    // when the field straddles a word boundary.
    if (pos >= bit_array_read_words) return;

    if (bits <= rbit)
        *value = (*s >> fbit) & bit_mask32[bits];
    else {
        if (pos + 1 >= bit_array_read_words) return;
        *value = (*s++ >> fbit) & bit_mask32[rbit];
        *value |= (*s & bit_mask32[bits - rbit]) << rbit;
    }

    bit_array_read_bits += bits;
}

__inline void
get_unary (unsigned long *value) {
    unsigned long fbit = bit_array_read_bits & 0x1FUL;
    unsigned long rbit = 32 - fbit;
    unsigned long pos = bit_array_read_bits >> 5;
    tta_word *s = ((tta_word *) bit_array_read) + pos;
    unsigned long mask = 1;

    *value = 0;

    // Same unit mismatch as get_binary (word index vs byte count), plus the
    // continuation walk below had no bound at all -- a corrupt frame whose unary
    // run never terminates walked s off the end of the buffer. Bound both
    // against the last whole word. A valid frame always terminates inside the
    // buffer, so this does not change its decoding.
    tta_word *s_end = ((tta_word *) bit_array_read) + bit_array_read_words;
    if (s >= s_end) return;

    if ((*s >> fbit) == bit_mask32[rbit]) {
        *value += rbit; fbit = 0;
        while (s + 1 < s_end && *(++s) == bit_mask32[32]) *value += 32;
    }
    for (mask <<= fbit; s < s_end && (*s & mask); mask <<= 1) (*value)++;

    bit_array_read_bits += (*value + 1);
}

void
encode_frame (long *data, unsigned long len) {
    long *p;
    unsigned long value;
    unsigned long unary, binary;

    unsigned long k;
    unsigned long k0 = 10;
    unsigned long k1 = 10;
    unsigned long sum0 = shift_16[k0];
    unsigned long sum1 = shift_16[k1];


    for (p = data; p < data + len; p++) {
        // convert sample from signed to unsigned
        value = ENC(*p);

        // encode Rice unsigned
        k = k0;

        sum0 += value - (sum0 >> 4);
        // The Rice parameter indexes bit_mask32[0..32] below, but k0/k1 were
        // incremented with no ceiling, so a loud enough frame drove k to 33 and
        // read past the table. Clamp at 32; decode_frame applies the identical
        // clamp so the two sides stay in lock-step.
        if (k0 > 0 && sum0 < shift_16[k0])
            k0--;
        else if (k0 < 32 && sum0 > shift_16[k0 + 1])
            k0++;

        if (value >= bit_shift[k]) {
            value -= bit_shift[k];
            k = k1;

            sum1 += value - (sum1 >> 4);
            if (k1 > 0 && sum1 < shift_16[k1])
            	k1--;
            else if (k1 < 32 && sum1 > shift_16[k1 + 1])
            	k1++;

            unary = 1 + (value >> k);
        } else {
            unary = 0;
        }

        // put Rice code
        if (unary>=50) {
            put_unary (50);
            put_binary (unary, 32);
        } else {
            put_unary (unary);
        }
        if (k) {
            binary = value & bit_mask32[k];
            put_binary(binary, k);
        }
    }
}

void
decode_frame (long *data, unsigned long len) {
    long *p, value;
    unsigned long unary, binary;

    unsigned long k;
    unsigned long k0 = 10;
    unsigned long k1 = 10;
    unsigned long sum0 = shift_16[k0];
    unsigned long sum1 = shift_16[k1];
    int depth;

    for (p = data; p < data + len; p++) {

	// decode Rice unsigned
        get_unary (&unary);
        if (unary==50) {
            get_binary (&unary, 32);
        }

        switch (unary) {
        case 0:  depth = 0; k = k0; break;

        default: depth = 1; k = k1;
        	 unary--;
        }

        if (k) {
            get_binary(&binary, k);
            value = (unary << k) + binary;
        } else {
            value = unary;
        }

        switch (depth) {
        case 1:
        	sum1 += value - (sum1 >> 4);
        	if (k1 > 0 && sum1 < shift_16[k1])   k1--;
        	else if (k1 < 32 && sum1 > shift_16[k1 + 1])    k1++;   // clamp mirrors encode_frame
        	value += bit_shift[k0];
                // no break!
        default:
        	sum0 += value - (sum0 >> 4);
        	if (k0 > 0 && sum0 < shift_16[k0])   k0--;
        	else if (k0 < 32 && sum0 > shift_16[k0 + 1])    k0++;   // clamp mirrors encode_frame
        }

        *p = DEC(value);
    }
}

/* eof */

