/*
 * ttaenc.h
 *
 * Description:  TTA main definitions and prototypes
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

#ifndef TTAENC_H
#define TTAENC_H

// Everything this header used to declare -- the format constants, the error
// codes, SWAP16/SWAP32, tta_error, malloc1d and malloc2d -- belonged to
// tta.cpp, entropy.cpp and filters.cpp, which are gone: the codec is
// rust/darc-codecs/src/tta.rs. Nothing outside those three files ever used any
// of it. What is left is the pair of entry points, kept here because C_TTA.h
// includes this header inside C_TTA.cpp's extern "C" block, which is what gives
// them the C linkage the Rust exports match.

int tta_compress (int level, int skip_header, int is_float, int num_chan, int word_size, int offset, int raw_data, CALLBACK_FUNC *callback, void *auxdata);
int tta_decompress (CALLBACK_FUNC *callback, void *auxdata);

#endif  /* TTAENC_H */
