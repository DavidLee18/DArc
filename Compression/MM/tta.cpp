/*
 * ttaenc.c
 *
 * Description:  TTA lossless audio encoder/decoder.
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

#ifdef _WIN32
#pragma  pack(1)
#endif

#include "../Compression.h"
#include "ttaenc.h"
#include "entropy.h"
#include "filters.h"
#include "mmdet.h"

#ifndef TTA_LIBRARY
#define print_message(s)   fprintf s
#else
#define print_message(s)
#endif

// Number of channels that wave file may have. Used for auto-detection when
// WAV header doesn't exist or don't taken into account. This array is zero-ended
static int channels[] = {1,2,0};

// Number of bits per word that wave file may have. Used for auto-detection, too
static int bitvalues[] = {8,16,0};


void tta_error (long error, const char *name)
{
    switch (error) {
    case COMMAND_ERROR:
        fprintf (stderr, "Error:   unknown command '%s'\n%s\n", name, LINE); break;
    case FORMAT_ERROR:
        fprintf (stderr, "Error:   not compatible file format\n%s\n", LINE); break;
    case FIND_ERROR:
        fprintf (stderr, "Error:   file(s) not found '%s'\n%s\n\n", name, LINE); exit(1);
    case CREATE_ERROR:
        fprintf (stderr, "Error:   problem creating directory '%s'\n%s\n\n", name, LINE); exit(1);
    case OPEN_ERROR:
        fprintf (stderr, "Error:   can't open file '%s'\n%s\n\n", name, LINE); exit(1);
    case FILE_ERROR:
        fprintf (stderr, "\nError:   file is corrupted\n%s\n", LINE); break;
    case WRITE_ERROR:
        fprintf (stderr, "\nError:   can't write to output file\n%s\n\n", LINE); exit(1);
    case READ_ERROR:
        fprintf (stderr, "\nError:   can't read from input file\n%s\n\n", LINE); exit(1);
    case MEMORY_ERROR:
        fprintf (stderr, "\nError:   insufficient memory available\n%s\n\n", LINE); exit(1);
    }
}

void *malloc1d (size_t num, size_t size)
{
    void    *array;

    if ((array = calloc (num, size)) == NULL)
        tta_error (MEMORY_ERROR, NULL);

    return (array);
}

long **malloc2d (long num, unsigned long len)
{
    long    i, **array, *tmp;

    array = (long **) calloc (num, sizeof(long *) + len * sizeof(long));
    if (array == NULL) tta_error (MEMORY_ERROR, NULL);

    // One allocation holds num row pointers followed by num*len samples, so
    // the data starts immediately past the pointer table. Step over it in
    // units of the pointer -- "(long *)(array + num)" -- not in units of long.
    //
    // The original "(long *) array + num" advances by num * sizeof(long) over
    // a table that is num * sizeof(long *) bytes. Those are the same size only
    // where long is as wide as a pointer. On Windows they are not: long is 4
    // bytes and pointers are 8, so the data block began halfway inside the
    // pointer table and the first samples written overwrote the row pointers.
    // Both encode and decode then died with a page fault on an address like
    // 0x00007f3600000000 -- a live heap pointer whose low half had been
    // replaced by sample data.
    //
    // Equivalent on LP64, where the two strides coincide, so archives written
    // by Linux and macOS builds are unchanged by this.
    for(i = 0, tmp = (long *) (array + num); i < num; i++)
        array[i] = tmp + i * len;

    return (array);
}

#ifndef FREEARC_DECOMPRESS_ONLY
static long read_wave (long *data, void *rest, void **bufferptr, void *prevbuf, long prevsize, long byte_size, long num_chan, unsigned long len, CALLBACK_FUNC *callback, void *auxdata)
{
    long i, rest_bytes, elements;
    char *buffer = (char*) malloc1d (len + 2, num_chan*byte_size);
    long wanted = len*num_chan*byte_size;
    long use_prevsize = mymin(prevsize,wanted);
    memcpy (buffer, prevbuf, use_prevsize);
    long bytes_read =  wanted <= prevsize?  0  :  callback ("read", buffer+prevsize, wanted-prevsize, auxdata);

    if (bytes_read >= 0) {  // If read ok
        bytes_read += use_prevsize;
        rest_bytes = bytes_read%(num_chan*byte_size);
        memcpy (rest, buffer+bytes_read-rest_bytes, rest_bytes);
        elements = (bytes_read/(num_chan*byte_size)) * num_chan;

        switch (byte_size) {
        case 1: {
                    unsigned char *sbuffer = (unsigned char *)buffer;
                    for (i = 0; i < elements; i++)
                        data[i] = (long) sbuffer[i] - 0x80;
                    break;
                }
        case 2: {
                    short *sbuffer = (short*)buffer;
                    for (i = 0; i < elements; i++)
                        data[i] = (long) sbuffer[i];
                    break;
                }
        case 3: {
                    // Read exactly the three bytes of the sample and sign-extend
                    // through a 32-bit word. The original dereferenced a "long"
                    // here, which is 8 bytes on LP64: it read five bytes past
                    // each sample and the ">> 8" then shifted in the following
                    // samples' bits instead of the sign.
                    unsigned char *sbuffer = (unsigned char *)buffer;
                    for (i = 0; i < elements; i++) {
                        unsigned char *q = sbuffer + i * 3;
                        uint32 t = (uint32)q[0] | ((uint32)q[1] << 8) | ((uint32)q[2] << 16);
                        data[i] = (long) ((tta_i32)(t << 8) >> 8);
                    }
                    break;
                }
        case 4: {
                    // 4-byte elements (32-bit float). "long" is 8 bytes on LP64,
                    // so this read two elements at a time and ran off the buffer.
                    tta_i32 *sbuffer = (tta_i32*)buffer;
                    for (i = 0; i < elements; i++)
                        data[i] = sbuffer[i];
                    break;
                }
        }
    }
    *bufferptr = buffer;
    return (bytes_read);
}
#endif

static long write_wave (long **data, long byte_size, long num_chan, unsigned long len, CALLBACK_FUNC *callback, void *auxdata)
{
    long    n;
    long    i, res;
    void    *buffer;

    buffer = malloc1d (len * num_chan + 2, byte_size);

    switch (byte_size) {
    case 1: {
                unsigned char *sbuffer = (unsigned char *)buffer;
                for (i = 0; i < (len * num_chan); i+= num_chan)
                for (n = 0; n < num_chan; n++) sbuffer[i+n] = (unsigned char) (data[n][i/num_chan] + 0x80);
                break;
            }
    case 2: {
                short *sbuffer = (short*)buffer;
                for (i = 0; i < (len * num_chan); i+= num_chan)
                for (n = 0; n < num_chan; n++) sbuffer[i+n] = (short) data[n][i/num_chan];
                break;
            }
    case 3: {
                // Store exactly three bytes per sample. The original assigned
                // through a "long *", writing 8 bytes at a 3-byte stride: each
                // store clobbered the next samples (later stores happened to
                // repair them) and the last one ran past the buffer.
                unsigned char *sbuffer = (unsigned char *)buffer;
                for (i = 0; i < (len * num_chan); i+= num_chan)
                for (n = 0; n < num_chan; n++) {
                    uint32 v = (uint32) data[n][i/num_chan];
                    unsigned char *q = sbuffer + (i+n) * 3;
                    q[0] = (unsigned char) v;
                    q[1] = (unsigned char)(v >> 8);
                    q[2] = (unsigned char)(v >> 16);
                }
                break;
            }
    case 4: {
                // 4-byte elements; "long" would write 8 and overrun the buffer.
                tta_i32 *sbuffer = (tta_i32*)buffer;
                for (i = 0; i < (len * num_chan); i+= num_chan)
                for (n = 0; n < num_chan; n++) sbuffer[i+n] = (tta_i32) data[n][i/num_chan];
                break;
            }
    }

    res = callback ("write", buffer, byte_size * len * num_chan, auxdata);
    free (buffer);
    return res;
}

#ifndef FREEARC_DECOMPRESS_ONLY
void split_int (long *data, long frame_len, long num_chan, long **buffer)
{
    long    i, j, n;

    for (i = 0; i < frame_len; i++)
    for (j = 0; j < num_chan; j++) {
        buffer[j][i] = data[i * num_chan + j];
    }

    if (num_chan > 1)
    for (i = 0, n = (num_chan - 1); i < frame_len; i++) {
        for (j = 0; j < n; j++)
            buffer[j][i] = buffer[j+1][i] - buffer[j][i];
        buffer[n][i] = buffer[n][i] - (buffer[n-1][i] / 2);
    }
}
#endif

void combine_int (long frame_len, long num_chan, long **buffer)
{
    long    i, j, n;

    if (num_chan > 1)
    for (i = 0, n = (num_chan - 1); i < frame_len; i++) {
        buffer[n][i] = buffer[n][i] + (buffer[n-1][i] / 2);
        for (j = n; j > 0; j--)
            buffer[j-1][i] = buffer[j][i] - buffer[j-1][i];
    }
}

#ifndef FREEARC_DECOMPRESS_ONLY
void split_float (long *data, long frame_len, long num_chan, long **buffer)
{
    long    i, j;

    for (i = 0; i < frame_len; i++)
    for (j = 0; j < num_chan; j++) {
        unsigned long t = data[i * num_chan + j];
        unsigned long negative = (t & 0x80000000)? -1:1;
        unsigned long data_hi = (t & 0x7FFF0000) >> 16;
        unsigned long data_lo = (t & 0x0000FFFF);

        buffer[j][i] = data_hi - 0x3F80;
        buffer[j+num_chan][i] = (SWAP16(data_lo) + 1) * negative;
    }
}
#endif

void combine_float (long frame_len, long num_chan, long **buffer)
{
    long    i, j;

    for (i = 0; i < frame_len; i++)
    for (j = 0; j < num_chan; j++) {
        // abs() takes an int; buffer[] is long (64-bit on LP64), so this
        // truncated its argument. labs() is the matching one.
        unsigned long negative = buffer[j+num_chan][i] & 0x80000000;
        unsigned long data_hi = buffer[j][i];
        unsigned long data_lo = labs(buffer[j+num_chan][i]) - 1;

        data_hi += 0x3F80;
        buffer[j][i] = (data_hi << 16) | SWAP16(data_lo) | negative;
    }
}


#ifndef FREEARC_DECOMPRESS_ONLY
// DARC_RUST=1 selects the Rust port of the encoder (rust/darc-codecs, tta.rs +
// mmdet.rs), excluded rather than redeclared for the same reason as the decoder
// below: both have C linkage, so GNU ld would report a multiple definition.
//
// Unlike C_MM.h, ttaenc.h declares this with the SAME signature as the
// definition, so the definition really does inherit the C linkage that
// C_TTA.cpp's `extern "C"` block intends -- which is what lets the drop-in take
// the symbol. C_TTA::compress reaches it through LoadFromDLL's fallback, so the
// replacement applies there too.
//
// read_wave/split_int/split_float above and the encode halves of entropy.cpp
// and filters.cpp are now unreferenced, but stay: they are compiled into this
// same translation unit and cost only a few unused bytes, while removing them
// would widen the exclusion for no gain.
//
// Verified byte-identical to the C encoder across levels 1-3, 8/16/24-bit,
// mono/stereo, float, raw mode, the storing path and autodetection; see
// rust/difftest/tta-check.sh, which now compares the produced STREAM.
#endif

// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// tta_decompress is declared in ttaenc.h, which C_TTA.h pulls in inside
// C_TTA.cpp's extern "C" block, so this definition has C linkage and shares a
// symbol with the Rust export. Excluded rather than redeclared: with both
// present the linker resolves from this object and never pulls the Rust one --
// and, both being C-linkage, GNU ld reports a multiple definition. So the
// switch has to remove this definition, not merely add a declaration elsewhere.
// The same is true of the other codecs (C_Dict.cpp, C_LZP.cpp, rep.cpp).
//
// The encoder (tta_compress) and everything it shares -- read_wave, split_*,
// the entropy encoder, the filters -- stay compiled; only this entry point is
// replaced. Verified byte-identical to the C decoder over a matrix of channel
// counts, word sizes and levels; see rust/difftest/tta-check.sh.


#ifndef TTA_LIBRARY
// DRIVER ************************************************************************
// This demo program shows how to use TTA sound wave compressor

// to do:
// 1. -w4 - error!!! the same for other encodings where compression really doesn't work
// 2. 12*0 - stored file, compression params in each block, TTA header for grzip algo
// 3. cobalp?
// 4. class Buffer?

// HOUSEKEEPING ********************************************************************
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <time.h>
#include <windows.h>

#include "../Compression.h"
#include "../Common.cpp"
#include "ttaenc.h"

#ifdef _WIN32
    #define ERASE_STDERR fprintf (stderr, "%78s\r", "")
#else
    #define ERASE_STDERR fprintf (stderr, "\033[2K")
#endif
#define LINE "------------------------------------------------------------"



FILE       *fin, *fout;
time_t     stime;
uint64     input_byte_count;
uint64     output_byte_count;
int        show_stat = 1;
int        unpack = 0;    // Unpack previously compressed data
uint64     data_size;     // Input file size

int readFILE (/*void* param,*/ void* buf, int size)
{
    //FILE *fin = (FILE*)param;
    int res = read (fin, buf, size);
    if (res>0)  input_byte_count += res;
    return res;
}

int writeFILE (/*void* param,*/ void* buf, int size)
{
    if (output_byte_count==0) {
        // print process banner
        !unpack
          ? fprintf (stdout, "Encode:  processing ..\r")
          : fprintf (stdout, "Decode:  processing ..\r");
    }

    //FILE *fout = (FILE*)param;
    write (fout, buf, size);
    output_byte_count += size;

    if (show_stat && size>4) {
        ERASE_STDERR;
        if ( !unpack ) {
            fprintf (stdout, "Encode:  wrote %.0f bytes, %.0f%% complete, ratio: %.2f, time: %d\r",
                (double) output_byte_count,
                (double) input_byte_count/ (data_size + 1) * 100,
                (double) output_byte_count/(input_byte_count + 1),
                (int) (time (NULL) - stime));
        } else {
            fprintf (stdout, "Decode:  wrote %.0f bytes, %.0f%% complete, ratio: %.2f, time: %d\r",
                (double) output_byte_count,
                (double) input_byte_count/(data_size + 1) * 100,
                (double) output_byte_count/(input_byte_count + 1),
                (int) (time (NULL) - stime));
        }
    }
    return 0;
}

// Command-line parsing, reading the input data, calling encode/decode, and writing the output data
int main (int argc, char **argv)
{
    int tta_level   = 3;  // Compression level (1..3, higher means tighter and slower compression)
    int skip_header = 0;  // Skip file header detection
    int is_float    = 0;  // Floating-point data format
    int num_chan    = 0;  // Channels count
    int word_size   = 0;  // Size of each encoded value, in bits
    int offset      = 0;  // File offset where MM data start (header is copied intact)
    int raw_data    = 0;  // Write raw predictor's output without using entropy encoder

    while (argc>1) {
    	if (argv[1][0] == '-') {
            switch( tolower(argv[1][1]) ) {
                case 'd':   unpack++;                    break;
                case 'm':   tta_level = atoi(argv[1]+2); break;
                case 's':   skip_header++;               break;
                case 'f':   is_float++;                  break;
                case 'c':   num_chan  = atoi(argv[1]+2); break;
                case 'w':   word_size = atoi(argv[1]+2); break;
                case 'o':   offset    = atoi(argv[1]+2); break;
                case 'r':   raw_data  = atoi(argv[1]+2); break;
                default :   printf( "\n Unknown option '%s'\n", argv[1]);
                            exit(1);
            }
        } else {
            int a, b, c;
            if (sscanf (argv[1], "%d+%d*%d", &a, &b, &c)==3)
                offset=a, num_chan=b, word_size=c, is_float=0;
            else if (sscanf (argv[1], "%d*%d", &a, &b)==2)
                num_chan=a, word_size=b, is_float=0;
            else break;
        }
        argv++, argc--;
    }

    if (argc != 2  &&  argc != 3) {
        printf( "\n Usage: tta [options] original-file [packed-file]");
        printf( "\n   -m# -- compression level [1..3], default %d", tta_level);
        printf( "\n   -s  -- skip WAV header detection");
        printf( "\n   -f  -- floating-point data format");
        printf( "\n   -c# -- channels count");
        printf( "\n   -w# -- word size, in bits (8/16)");
        printf( "\n   -o# -- offset of MM data in file (=header size)");
        printf( "\n   -r# -- output raw data (no entropy coder). -r1/-r2");
        printf( "\n   c*w -- use c channels w bits each (example: 3*8)");
        printf( "\n   o+c*w -- use c channels w bits each starting from offset o");
        printf( "\n" );
        printf( "\n For decompress: tta -d packed-file [unpacked-file]");
        printf( "\n" );
        exit(1);
    }
    fin = fopen( argv[1], "rb" );
    if (fin == NULL) {
        printf( "\n Can't open %s for read\n", argv[1]);
        exit(2);
    }

    // Write the output data if an output file was specified
    fout = fopen( argc==3? argv[2] : "NUL", "wb" );
    if (fout == NULL) {
        printf( "\n Can't open %s for write\n", argv[2]);
        exit(3);
    }

    // clear statistics
    input_byte_count = output_byte_count = 0;
    stime = time(NULL);
    data_size = filelength(fileno(fin));

    // Perform compression or decompression
    !unpack
      ? tta_compress   (tta_level, skip_header, is_float, num_chan, word_size, offset, raw_data, readFILE, fin, writeFILE, fout)
      : tta_decompress (readFILE, fin, writeFILE, fout);

    // Print final stats
    ERASE_STDERR;

    if ( !unpack ) {
        fprintf (stdout, "Encode:  wrote %d bytes, done, ratio: %.2f, time: %d\n",
            (int) output_byte_count,
            (float) output_byte_count/(input_byte_count + 1),
            (int) (time (NULL) - stime));
    } else {
        fprintf (stdout, "Decode:  wrote %d bytes, done, ratio: %.2f, time: %d\n",
            (int) (output_byte_count),
            (float) output_byte_count/(input_byte_count + 1),
            (int) (time (NULL) - stime));
    }
    fprintf (stdout, "%s\n", LINE);

    return 0;
}

#endif

