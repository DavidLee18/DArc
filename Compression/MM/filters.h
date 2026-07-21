/*
 * filters.h
 *
 * Description:  TTA filters definitions and prototypes
 * Developed by: Alexander Djourik <sasha@iszf.irk.ru>
 *               Pavel Zhilin <pzh@iszf.irk.ru>
 *
 * Copyright (c) 1999-2003 Alexander Djourik. All rights reserved.
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

#ifndef FILTERS_H
#define FILTERS_H

#define MAX_ORDER   32
#define BUF_SIZE    256

#ifndef M_LN2
#define M_LN2       0.69314718055994530942
#endif

// x is a signed sample. Casting it to uint64 made the final ">> k" a *logical*
// shift, so a negative sample came back as a huge positive -- and once the
// filter state below is 32-bit, that value is truncated on the way in and the
// frame can no longer be reconstructed. Do the wrap in unsigned 32-bit (as the
// original 32-bit code did) and the final shift signed, so it stays arithmetic.
#define PREDICTOR1(x, k)    ((tta_i32)(((tta_i32)(((uint32)(x) << (k)) - (uint32)(x))) >> (k)))

// The adaptive filter state below is 32-bit by design. Its polynomial predictor
// builds cascading differences (pl[n-1] = pl[n] - pl[n-1], chained) whose
// magnitude is only kept in check by 32-bit wraparound. Held in a 64-bit "long"
// -- which is what "long" is on LP64 -- nothing wraps and the state runs away:
// residuals of order 2^59 come out of a +-6000 input, which both destroys the
// compression ratio and overflows the entropy coder's 32-bit escape
// (put_binary(unary,32)), so frames stopped round-tripping. Pin it to exactly
// 32 bits so the arithmetic wraps as the format expects.
typedef int tta_i32;   /* must be exactly 32 bits wide */

void filters_compress   (long *data, unsigned long len, long level, long byte_size);
void filters_decompress (long *data, unsigned long len, long level, long byte_size);

#endif  /* FILTERS_H */
