extern "C" {
#include "../Compression.h"
}
#define MMD_LIBRARY

#ifndef FREEARC_DECOMPRESS_ONLY
#include "mmdet.cpp"

#ifndef MM_LIBRARY
#define print_message(s)   fprintf s
#else
#define print_message(s)
#endif


// PREPROCESSING ROUTINES *************************************************************************

// Run through buffer diffing 8-bit elements
void diff1 (void *buf, int bufsize, int N, void *_base)
{
    byte *base=(byte*)_base, x;
    for (byte *p=(byte*)buf; p+N<=(byte*)buf+bufsize; p+=N)
/*
        if (N==3) {
          int b=p[0], g=p[1], r=p[2];
          b-=g, r-=g;
          //int y = (r+2*g+b)/4; b-=g, r-=g; g=y;

			int tmp;

			g = r - b; // Co
			tmp = b + (p[1] >> 1);
			b = g - tmp; // Cg
			r = tmp + (p[0] >> 1); // Y

          p[0]=b-base[0], p[1]=g-base[1], p[2]=r-base[2];
          base[0]=b, base[1]=g, base[2]=r;



          int fb=p[0], y=p[1], fr=p[2];
          //fb+=fr*7/8, fr-=fb*17/32, fb+=y, y-=fb*3/8;
          fr-=y, y+=fr/2, fb-=y, y+=fb*3/8;
          p[0]=fb-base[0], p[1]=y-base[1], p[2]=fr-base[2];
          base[0]=fb, base[1]=y, base[2]=fr;
        } else
*/
        for (int i=0; i<N; i++)
            x=p[i], p[i]-=base[i], base[i]=x;
}

// Run through buffer diffing 16-bit elements
void diff2 (void *buf, int bufsize, int N, void *_base)
{
    uint16 *base=(uint16*)_base, x;
    for (uint16 *p=(uint16*)buf; p+N<=(uint16*)((char*)buf+bufsize); p+=N)
        for (int i=0; i<N; i++)
            x=p[i], p[i]-=base[i], base[i]=x;
}

// Run through buffer diffing 24-bit elements
void diff3 (void *buf, int bufsize, int N, void *_base)
{
    uint32 *base=(uint32*)_base; uint x;
    for (char *p=(char*)buf; p+N*3<=((char*)buf+bufsize); p+=N*3)
        for (int i=0; i<N; i++)
            x=value24(p+i*3), setvalue24(p+i*3, x-base[i]), base[i]=x;
}

// Run through buffer diffing 32-bit elements
void diff4 (void *buf, int bufsize, int N, void *_base)
{
    uint32 *base=(uint32*)_base, x;
    for (uint32 *p=(uint32*)buf; p+N<=(uint32*)((char*)buf+bufsize); p+=N)
        for (int i=0; i<N; i++)
            x=p[i], p[i]-=base[i], base[i]=x;
}

// Reorder buffer contents so that data for each byte of each channel are placed continuosly
//
// The destination index is R*i, NOT i*bufsize/X. Those differ: C evaluates the
// latter as (i*bufsize)/X, which exceeds i*(bufsize/X) by floor(i*s/X) once the
// trailing remainder s = bufsize%X is 2 or more. The rows then drift past the
// end of the transposed region, so a byte is dropped into the tail area and
// overwritten by the copy loop below, while an earlier slot is never written at
// all and keeps whatever malloc returned. The result is lossy AND
// nondeterministic -- an mm:r1 stream built over such a block fails its CRC.
//
// s=0 and s=1 come out identical either way (i*s/X < 1 for every i < X when
// s <= 1), which is why this survived: the encoder rounds the FIRST block down
// to a whole number of samples, so only a final short block can hit it, and
// only when it is both at least one sample long and 2+ bytes past a sample
// boundary. Reproduced with a 1048582-byte input at c2:w16.
BYTE* reorder_bytes (BYTE *buf, int bufsize, int N, int width)
{
    int X = N*width;
    if (X <= 0 || bufsize < X)  return buf;   // nothing to transpose (X==0 would divide by zero)
    BYTE *newbuf = (BYTE*) malloc(bufsize);
    if (!newbuf)  return buf;
    int R = bufsize/X;
    for (int i=0; i<X; i++)
        for (int j=0; j<R; j++)
            newbuf[i*R+j] = buf[i+j*X];
    for (int i=bufsize-(bufsize%X); i<bufsize; i++)
        newbuf[i] = buf[i];
    memcpy (buf, newbuf, bufsize);
    free(newbuf);
    return buf;
}

// Reorder buffer contents so that each channel data are placed continuosly
BYTE* reorder_words (BYTE *buf, int bufsize, int N, int width)
{
    return buf;
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Run through buffer undiffing 8-bit elements
// Inverse of reorder_bytes. The forward transform sends buf[i+j*X] to
// out[i*R+j] with X = N*width and R = bufsize/X, gathering the k-th byte of
// every sample into one run; the trailing bufsize%X bytes pass through
// untransposed. This undoes exactly that.
BYTE* unreorder_bytes (BYTE *buf, int bufsize, int N, int width)
{
    int X = N*width;
    if (X <= 0 || bufsize < X)  return buf;      // nothing was transposed
    BYTE *newbuf = (BYTE*) malloc(bufsize);
    if (!newbuf)  return NULL;
    int R = bufsize/X;
    for (int i=0; i<X; i++)
        for (int j=0; j<R; j++)
            newbuf[i+j*X] = buf[i*R+j];
    for (int i=bufsize-(bufsize%X); i<bufsize; i++)
        newbuf[i] = buf[i];
    memcpy (buf, newbuf, bufsize);
    free (newbuf);
    return buf;
}

void undiff1 (void *buf, int bufsize, int N, void *_base)
{
    char *base=(char*)_base;
    for (char *p=(char*)buf; p+N<=(char*)buf+bufsize; p+=N)
        for (int i=0; i<N; i++)
            p[i] = base[i] += p[i];
}

// Run through buffer undiffing 16-bit elements
void undiff2 (void *buf, int bufsize, int N, void *_base)
{
    uint16 *base=(uint16*)_base;
    for (uint16 *p=(uint16*)buf; p+N<=(uint16*)((char*)buf+bufsize); p+=N)
        for (int i=0; i<N; i++)
            p[i] = base[i] += p[i];
}

// Run through buffer undiffing 24-bit elements
void undiff3 (void *buf, int bufsize, int N, void *_base)
{
    uint32 *base=(uint32*)_base;
    for (char *p=(char*)buf; p+N*3<=((char*)buf+bufsize); p+=N*3)
        for (int i=0; i<N; i++)
            base[i] += value24(p+i*3), setvalue24(p+i*3, base[i]);
}

// Run through buffer undiffing 32-bit elements
void undiff4 (void *buf, int bufsize, int N, void *_base)
{
    uint32 *base=(uint32*)_base;
    for (uint32 *p=(uint32*)buf; p+N<=(uint32*)((char*)buf+bufsize); p+=N)
        for (int i=0; i<N; i++)
            p[i] = base[i] += p[i];
}


// COMPRESSION METHOD IMPLEMENTATION **************************************************************

#ifndef FREEARC_DECOMPRESS_ONLY
// DARC_RUST=1 selects the Rust port of the encoder (rust/darc-codecs, mm.rs +
// mmdet.rs), excluded rather than redeclared for the same reason as the decoder
// below: both are C-linkage and GNU ld reports a multiple definition.
//
// mmdet.cpp stays compiled. It is #included above and tta.cpp calls
// autodetect_wav_header/autodetect_by_entropy directly (tta.cpp:322-324), so
// the detector cannot go until TTA's encoder is ported too. Only this function
// leaves. The diff routines above stay for the same reason they always did --
// plain non-static globals costing a few unreferenced bytes.
//
// Verified byte-identical to the C encoder over the same matrix the decoder
// uses, including all four autodetection modes; see rust/difftest/mm-check.sh,
// which now compares the produced STREAM, not just the round-trip.
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// mm_decompress is declared in C_MM.h, which C_MM.cpp pulls in inside its
// extern "C" block, so this definition inherits C linkage and shares a symbol
// with the Rust export. Excluded rather than redeclared: with both present the
// linker resolves from this object and never pulls the Rust one -- and, both
// being C-linkage, GNU ld reports a multiple definition. So the switch has to
// remove this definition, not merely add a declaration elsewhere. The same is
// true of the other codecs (C_Dict.cpp, C_LZP.cpp, rep.cpp, tta.cpp).
//
// The encoder (mm_compress), the diff routines it uses and all of mmdet.cpp
// stay compiled; only this entry point is replaced. The undiff routines above
// are left in place too: they are this function's whole body, but they are
// plain non-static globals, so leaving them costs a few unreferenced bytes
// while dropping them would widen the exclusion for no gain. Verified
// byte-identical to the C decoder over a matrix of channel counts, word sizes,
// header offsets and detector modes; see rust/difftest/mm-check.sh.



#ifndef MM_LIBRARY
// DRIVER ************************************************************************
// This demo program shows how to use MM preprocessor

#include "../Common.cpp"

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

    if (show_stat && size>1000) {
        ERASE_STDERR;
        if ( !unpack ) {
            fprintf (stdout, "Encode:  wrote %.0f bytes, %.0f%% complete, time: %d\r",
                (double) output_byte_count,
                (double) input_byte_count/ (data_size + 1) * 100,
                (int) (time (NULL) - stime));
        } else {
            fprintf (stdout, "Decode:  wrote %.0f bytes, %.0f%% complete, time: %d\r",
                (double) output_byte_count,
                (double) input_byte_count/(data_size + 1) * 100,
                (int) (time (NULL) - stime));
        }
    }
    return size;
}

// Parse the command line, read the input data, call encode/decode, and write the output data
int main (int argc, char **argv)
{
    int mode        = 9;  // Detection speed mode (1 - fastest, 9 - most accurate)
    int skip_header = 0;  // Skip file header detection
    int is_float    = 0;  // Floating-point data format
    int num_chan    = 0;  // Channels count
    int word_size   = 0;  // Size of each encoded value, in bits
    int offset      = 0;  // File offset where MM data start (header is copied intact)
    int reorder     = 0;  // Reorder bytes/words

    while (argc>1) {
    	if (argv[1][0] == '-') {
            switch( tolower(argv[1][1]) ) {
                case 'd':   if (argv[1][2])   mode = atoi(argv[1]+2);
                            else              unpack++;  break;
                case 's':   skip_header++;               break;
                case 'f':   is_float++;                  break;
                case 'c':   num_chan  = atoi(argv[1]+2); break;
                case 'w':   word_size = atoi(argv[1]+2); break;
                case 'o':   offset    = atoi(argv[1]+2); break;
                case 'r':   reorder   = atoi(argv[1]+2); break;
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
        printf( "\n Usage: mm [options] original-file [packed-file]");
        printf( "\n   -d# -- detection speed mode (1 - fastest, 9 - most accurate)");
        printf( "\n   -s  -- skip WAV header detection");
        printf( "\n   -f  -- floating-point data format");
        printf( "\n   -c# -- channels count");
        printf( "\n   -w# -- word size, in bits (8/16)");
        printf( "\n   -o# -- offset of MM data in file (=header size)");
        printf( "\n   -r# -- reorder data. -r1 - reorder bytes, -r2 - reorder words (unfinished!)");
        printf( "\n   c*w -- use c channels w bits each (example: 3*8)");
        printf( "\n   o+c*w -- use c channels w bits each starting from offset o");
        printf( "\n" );
        printf( "\n For decompress: mm -d packed-file [unpacked-file]");
        printf( "\n" );
        exit(1);
    }
    fin = fopen (argv[1], "rb");
    if (fin == NULL) {
        printf( "\n Can't open %s for read\n", argv[1]);
        exit(2);
    }

    // Write the output data if an output file was specified
    fout = fopen (argc==3? argv[2] : "NUL", "wb");
    if (fout == NULL) {
        printf ("\n Can't open %s for write\n", argv[2]);
        exit(3);
    }

    // clear statistics
    input_byte_count = output_byte_count = 0;
    stime = time(NULL);
    data_size = filelength(fileno(fin));

    // Perform compression or decompression
    !unpack
      ? mm_compress   (mode, skip_header, is_float, num_chan, word_size, offset, reorder, readFILE, writeFILE)
      : mm_decompress (readFILE, writeFILE);

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

#endif  // !defined (MM_LIBRARY)
