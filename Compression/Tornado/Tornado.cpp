// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
// (c) Joachim Henke
// GPL'ed code of Tornado - fast LZ77 compression algorithm.
#include "../Compression.h"
// MatchFinder.cpp held the encoder's match finders and was deleted with the
// rest of the C encoder; these are the only two things the surviving code
// needed from it. PtrVal is used by C_Tornado.cpp's SetDictionary, and the
// debug macros by the decompressor's output loop below.
typedef uint   HashVal;      // Result of hashing function
typedef uint32 PtrVal;       // Pointers to buf stored in HTable
typedef uint32 HintVal;      // Cached bytes from buf stored in HTable

// Maximum number of bytes used for hashing in any match finder.
// If this value will be smaller than real, we can hash bytes in buf that are not yet read
// Also it's number of bytes reserved after bufend in order to simplify p+N<=bufend checks
#define MAX_HASHED_BYTES 12


#ifdef DEBUG
void check_match (BYTE *p, BYTE *q, int len)
{
    if (p==q || memcmp(p,q,len))   printf("Bad match:  ");
}
void print_literal (int pos, BYTE c)
{
    printf (isprint(c)? "%08x: '%c'\n" : "%08x: \\0x%02x\n", pos, c);
}
void print_match (int pos, int len, int dist)
{
    printf ("%08x: %3d %6d\n", pos, len, -dist);
}
#else
#define check_match(p,q,len)
#define print_literal(pos,c)
#define print_match(pos,len,dist)
#endif

#include "EntropyCoder.cpp"
#include "LZ77_Coder.cpp"
#include "DataTables.cpp"

// Compression method parameters
struct PackMethod
{
    int  number;            // Preset number
    int  encoding_method;   // Coder (0 - storing, 1 - bytecoder, 2 - bitcoder, 3 - huffman, 4 - arithmetic)
    bool find_tables;       // Enable searching for MM tables
    int  hash_row_width;    // Length of hash row
    uint hashsize;          // Hash size
    int  caching_finder;    // Force/prohibit using caching match finder
    uint buffer;            // Buffer (dictionary) size
    int  match_parser;      // Match parser (1 - greedy, 2 - lazy, 3 - flexible, 4 - optimal, 5 - even better)
    int  hash3;             // 2/3-byte hash presence and type
    int  shift;             // How much bytes to shift out/keep when window slides
    int  update_step;       // How much bytes are skipped in mf.update()
    uint auxhash_size;      // Auxiliary hash size
    int  auxhash_row_width; // Length of auxiliary hash row
};

extern "C" {
// Main compression and decompression routines
int tor_compress   (PackMethod m, CALLBACK_FUNC *callback, void *auxdata);
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata);
}

enum { STORING=0, BYTECODER=1, BITCODER=2, HUFCODER=3, ARICODER=4 };
enum { GREEDY=1, LAZY=2 };

// Preconfigured compression modes
PackMethod std_Tornado_method[] =
    //                 tables row  hashsize  caching buffer parser  hash3 shift update   auxhash
    { {  0, STORING,   false,   0,        0, 0,      1*mb,  0     ,   0,    0,  999,       0,    0 }
    , {  1, BYTECODER, false,   1,    16*kb, 0,      1*mb,  GREEDY,   0,    0,  999,       0,    0 }
    , {  2, BITCODER,  false,   1,    64*kb, 0,      2*mb,  GREEDY,   0,    0,  999,       0,    0 }
    , {  3, HUFCODER,  true,    2,   128*kb, 0,      4*mb,  GREEDY,   0,    0,  999,       0,    0 }
    , {  4, HUFCODER,  true,    2,     2*mb, 1,      8*mb,  GREEDY,   0,    0,  999,       0,    0 }
    , {  5, ARICODER,  true,    4,     2*mb, 1,     16*mb,  LAZY  ,   1,    0,  999,       0,    0 }
    , {  6, ARICODER,  true,    8,    32*mb, 1,     64*mb,  LAZY  ,   1,    0,    4,       0,    0 }
    , {  7, ARICODER,  true,   32,   128*mb, 5,    256*mb,  LAZY  ,   2,    0,    1,  128*kb,    4 }
    , {  8, ARICODER,  true,  128,   512*mb, 5,   1024*mb,  LAZY  ,   2,    0,    1,  128*kb,    4 }
    , {  9, ARICODER,  true,  256,  2048*mb, 5,   1024*mb,  LAZY  ,   2,    0,    1,  512*kb,    4 }
    , { 10, ARICODER,  true,  256,  2048*mb, 6,   1024*mb,  LAZY  ,   2,    0,    1,    2*mb,   32 }
    , { 11, ARICODER,  true,  200,  1600*mb, 7,   1024*mb,  LAZY  ,   2,    0,    1,  512*mb,  256 }
    };

// Default compression parameters are equivalent to option -5
const int default_Tornado_method = 5;

// If data table was not encountered in last table_dist bytes, don't check next table_shift bytes in order to make things faster
const int table_dist=256*1024, table_shift=128;

// Minimum lookahead for next match which compressor tries to guarantee.
// Also minimum amount of allocated space after end of buf (this allows to use things like p[11] without additional checks)
#define LOOKAHEAD 256

// Output buffer size
uint tornado_compressor_outbuf_size (uint buffer, int bytes_to_compress = -1)
{return bytes_to_compress!=-1? bytes_to_compress+(bytes_to_compress/8)+512 :
        compress_all_at_once?  buffer+(buffer/8)+512 :
                               HUGE_BUFFER_SIZE;}


// The LZ77 compressor lived here, guarded by FREEARC_DECOMPRESS_ONLY. It is
// gone: the Rust port in rust/darc-codecs/src/tornado/ is byte-identical to it
// on every preset 0-11, covering all nine instantiations the archiver ever
// built (rust/difftest/tornado-encode-check.sh, which compares against a
// pinned revision of this file rather than the working tree).
//
// Unarc never compiled any of it -- Unarc/makefile builds with
// -DFREEARC_DECOMPRESS_ONLY -- so nothing there changes.


// LZ77 decompressor ******************************************************************************

// If condition is true, write data to outstream
#define WRITE_DATA_IF(condition)                                                                  \
{                                                                                                 \
    if (condition) {                                                                              \
        if (decoder.error() != FREEARC_OK)  goto finished;                                        \
        tables.undiff_tables (write_start, output);                                               \
        debug (printf ("==== write %08x:%x ====\n", write_start-outbuf+offset, output-write_start)); \
        WRITE (write_start, output-write_start);                                                  \
        tables.diff_tables (write_start, output);                                                 \
        write_start = output;  /* next time we should start writing from this pos */              \
                                                                                                  \
        /* Check that we should shift the output pointer to start of buffer */                    \
        if (output >= outbuf + bufsize) {                                                         \
            offset_overflow |= (offset > (uint64(1) << 63));                                      \
            offset      += output-outbuf;                                                         \
            write_start -= output-outbuf;                                                         \
            write_end   -= output-outbuf;                                                         \
            tables.shift (output,outbuf);                                                         \
            output      -= output-outbuf;  /* output = outbuf; */                                 \
        }                                                                                         \
                                                                                                  \
        /* If we wrote data because write_end was reached (not because */                         \
        /* table list was filled), then set write_end into its next position */                   \
        if (write_start >= write_end) {                                                           \
            /* Set up next write chunk to HUGE_BUFFER_SIZE or until buffer end - whatever is smaller */ \
            write_end = write_start + mymin (outbuf+bufsize-write_start, HUGE_BUFFER_SIZE);       \
        }                                                                                         \
    }                                                                                             \
}


template <class Decoder>
int tor_decompress0 (CALLBACK_FUNC *callback, void *auxdata, int _bufsize, int minlen)
{
    //SET_JMP_POINT (FREEARC_ERRCODE_GENERAL);
    int errcode = FREEARC_OK;                             // Error code of last "write" call
    Decoder decoder (callback, auxdata, _bufsize);        // LZ77 decoder parses raw input bitstream and returns literals&matches
    if (decoder.error() != FREEARC_OK)  return decoder.error();
    uint bufsize = compress_all_at_once? _bufsize : mymax (_bufsize, HUGE_BUFFER_SIZE);   // Make sure that outbuf is at least 8mb in order to avoid excessive disk seeks (not required in programs compiled for one-shot compression)
    BYTE *outbuf = (byte*) malloc (bufsize+PAD_FOR_TABLES*2);  // Circular buffer for decompressed data
    if (!outbuf)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    outbuf += PAD_FOR_TABLES;       // We need at least PAD_FOR_TABLES bytes available before and after outbuf in order to simplify datatables undiffing
    BYTE *output      = outbuf;     // Current position in decompressed data buffer
    BYTE *write_start = outbuf;     // Data up to this point was already writen to outsream
    BYTE *write_end   = outbuf + mymin (bufsize, HUGE_BUFFER_SIZE); // Flush buffer when output pointer reaches this point
    if (compress_all_at_once)  write_end = outbuf + bufsize + 1;    // All data should be written after decompression finished
    uint64 offset = 0;                    // Current outfile position corresponding to beginning of outbuf
    int offset_overflow = 0;              // Flags that offset was overflowed so we can't use it for match checking
    DataTables tables;                    // Info about data tables that should be undiffed
    for (;;) {
        // Check whether next input element is a literal or a match
        if (decoder.is_literal()) {
            // Decode it as a literal
            BYTE c = decoder.getchar();
            print_literal (output-outbuf+offset, c);
            *output++ = c;
            WRITE_DATA_IF (output >= write_end);  // Write next data chunk to outstream if required

        } else {
            // Decode it as a match
            UINT len  = decoder.getlen(minlen);
            UINT dist = decoder.getdist();
            print_match (output-outbuf+offset, len, dist);

            // Both match-copy loops below are "do {...} while (--len)", so len==0
            // wraps the UINT counter to 4G and runs off the end of the buffer.
            // A real match is always >=1 (getlen returns minlen+extra, minlen>=1)
            // and EOF is signalled by len==IMPOSSIBLE_LEN, which is nonzero, so
            // rejecting len==0 here is transparent to valid streams. len comes
            // straight from the decoder, and minlen from a header byte (buf[1]),
            // so a corrupt stream can make it 0.
            if (len==0)  {errcode=FREEARC_ERRCODE_BAD_COMPRESSED_DATA; goto finished;}

            // Check for simple match (i.e. match not requiring any special handling, >99% of matches fail to this category)
            if (output-outbuf>=dist && write_end-output>len) {
                BYTE *p = output-dist;
                do   *output++ = *p++;
                while (--len);

            // Check that it's a proper match
            } else if (len<IMPOSSIBLE_LEN) {
                // Check that compressed data are not broken
                if (dist>bufsize || len>2*_bufsize || (output-outbuf+offset<dist && !offset_overflow))  {errcode=FREEARC_ERRCODE_BAD_COMPRESSED_DATA; goto finished;}
                // Slow match copying route for cases when output-dist points before buffer beginning,
                // or p may wrap at buffer end, or output pointer may run over write point
                BYTE *p  =  output-outbuf>=dist? output-dist : output-dist+bufsize;
                do {
                    *output++ = *p++;
                    if (p==outbuf+bufsize)  p=outbuf;
                    WRITE_DATA_IF (output >= write_end);
                } while (--len);

            // Check for special len/dist code used to encode EOF
            } else if (len==IMPOSSIBLE_LEN && dist==IMPOSSIBLE_DIST) {
                WRITE_DATA_IF (TRUE);  // Flush outbuf
                goto finished;

            // Otherwise it's a special code used to represent info about diffed data tables
            } else {
                len -= IMPOSSIBLE_LEN;
                if (len==0 || dist*len > 2*_bufsize)  {errcode=FREEARC_ERRCODE_BAD_COMPRESSED_DATA; goto finished;}
                stat (printf ("\n%d: Start %x, end %x, length %d      ", len, int(output-outbuf+offset), int(output-outbuf+offset+len*dist), len*dist));
                // Add new table to list: len is row length of table and dist is number of rows
                tables.add (len, output, dist);
                // If list of data tables is full then flush it by preprocessing
                // and writing to outstream already filled part of outbuf
                WRITE_DATA_IF (tables.filled());
            }
        }
    }
finished:
    free(outbuf-PAD_FOR_TABLES);
    // Return decoder error code, errcode or FREEARC_OK
    return decoder.error() < 0 ?  decoder.error() :
           errcode         < 0 ?  errcode
                               :  FREEARC_OK;
}


// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs).
//
// tor_decompress is declared inside the `extern "C"` block at the top of this
// file, so this definition inherits C linkage and shares a symbol with the Rust
// export. Excluded rather than redeclared: with both present the linker
// resolves from this object and never pulls the Rust one -- and, both being
// C-linkage, GNU ld reports a multiple definition. So the switch has to remove
// this definition, not merely add a declaration elsewhere. The same is true of
// the other codecs (C_Dict.cpp, C_LZP.cpp, rep.cpp, tta.cpp, mm.cpp).
//
// tor_decompress0 is a template with no other caller, so excluding this entry
// point leaves it uninstantiated and emits nothing; the encoder and everything
// it shares stay compiled. Verified byte-identical to the C decoder across all
// four entropy back-ends; see rust/difftest/tornado-check.sh.
#ifndef DARC_RUST
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode;
    // First 6 bytes of compressed data are encoding method, minimum match length and buffer size
    BYTE buf[2];          READ (buf, 2);
    uint encoding_method; encoding_method = buf[0];
    uint minlen;          minlen          = buf[1];
    uint bufsize;         READ4 (bufsize);

    switch (encoding_method) {
    case BYTECODER:
            return tor_decompress0 <LZ77_ByteDecoder> (callback, auxdata, bufsize, minlen);

    case BITCODER:
            return tor_decompress0 <LZ77_BitDecoder>  (callback, auxdata, bufsize, minlen);

    case HUFCODER:
            return tor_decompress0 <LZ77_Decoder <HuffmanDecoder<EOB_CODE> > > (callback, auxdata, bufsize, minlen);

    case ARICODER:
            return tor_decompress0 <LZ77_Decoder <ArithDecoder<EOB_CODE> >   > (callback, auxdata, bufsize, minlen);
    default:
            errcode = FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    }
finished: return errcode;
}
#endif  // !DARC_RUST (tor_decompress)


/*
LZ77 model:
    -no lz if len small and dist large: don't have much sense with our MINLEN=4
    -hash4+3: only 1% gain even on ghc.exe
    -hash5+4: 48->46.7 mb but 2x slower (22->46sec: 240mb compressed using 16mb hash)
    -0x65a8e9b4 for hash
    +combined len+dist encoding a-la cabarc - will make decoding a bit faster, but who cares? :)
    -save into hash records unused part of hash value in order to make
        fast check of usability of this hash slot (like it is already
        done in REP); would be especially helpful on larger hashes
    -save into hash record 4 bits of p[5] - would be useful to skip trying second..fourth hash records
    +save into hash record 4 bytes of data
    +lazy search (and return of 3-byte strings) for highest compression mode
+l8... - added 1 extra second per 280 mb processed
+compare with ideal hash function crc+crc+..
    (((CRCTab[(x)&255] ^ _rotr(CRCTab[((x)>>8)&255],8) ^ _rotr(CRCTab[((x)>>16)&255],16) ^ _rotr(CRCTab[((x)>>24)&255],24)) >> HashShift) & HashMask)
+store unused hash bits + a few more chars in hash   (1.5x speedup)
    491->367 (340 for hash4x), 91->68, 51->43 secs
    +use the first byte for a hash of 4 bytes
    +separate loops for len=3,4,5,6
    +using t, quickly check matches up to length 7 in the len3..5 loops and when checking the first string
    re-check the match lengths of the strings in the hash chain
+fast arithmetics! total=2^n
    a separate buffer for reading bit fields; or better, bits+arith in a single data stream
+lazy matches                                        (+3.5% compression)
    unsuccessfully tried:
      ush good_length; - reduce lazy search above this match length
      ush max_lazy;    - do not perform lazy search above this match length
      ush nice_length; - quit search above this match length
+arith / huffman / bitio                         (+10% compresion for bit i/o, +20% for huffman)
    byte i/o -> class: +0.3 sec on !all
+3-byte strings
+drop short distant strings
    +compression can be improved by 0.3% by also dropping 6-byte strings
+better hash multiplier
-5% less compression of src (l4 h22) compared to mmdet. strange?
-several encoding tables: after char, after small string, large string
-add custom MF for l=4/8 (3/6?) what means -1 sec. on !all
    don't have much meaning because caching MF isn't any worser
+FIXED: MatchFinder2 is incompatible with 3-byte words / lazy matching (update_hash assumes updates of at least 3 bytes)
+FAST_COMPILE - only 4 models actually used by -1..-12
+make hash_row_width part of the MatchFinder class
+FIXED: caching MF - odd words must be initialized with the contents of the start of the buffer
+sliding window for higher modes (-4/-5 - m.buffer/2, -6 and up - m.buffer/4)
+write data to outstreams in 16mb chunks
+64k-1m non-sliding window for -1..-3
+improved caching MF - memory accesses only for matches>=7 or last check
-max_lazy may improve speed/ratio for -4..-6 modes
-don't check more than one real string (option? only for 2-element hash?)
    -skip checking second string if first is large enough
+[almost] full hash_update for highest modes
+IMPOSSIBLE_LEN/IMPOSSIBLE_DIST for EOF encoding, encode() for first 2 chars
+FIXED: -s- -p2 problem (was returning len==0 instead of MINLEN-1)
-in the lazy search, take the previous match length into account, skipping the 3-byte and part of the 4-byte search
+TOO_FAR checks moved into caching MF
+output buffer now flushed only when reading next input chunk
+tor_(de)compress - returns error code or FREEARC_OK
+freearc: block the reading thread while data is being written
+7z's lazy heuristic
  +when searching for a string - if newlen=len+1 and newdist>dist*64 - ignore it
+2-byte strings, +repdist, +repboth, +repchar
+handling of small files!
+restore the bytecoder
  +large len - a few bytes representation to ensure no overflows
+auto-decrease hash (and buf) for small files
+extend the next match backwards in the lazy matcher
-repdistN+-delta - 0.4% on texts
+HuffmanEncoder::encode2
+fixed: use of the initial value repdist0=1 in the REPCHAR check
        use of the pseudo-distance from MMx for the REPCHAR check (note: the decoder must have the same queue of recent distances)
        a diffed table crossing a buffer shift
          data restoration must be done after the reverse diff, otherwise that diff will write garbage into the element following the restored one
        use of p->table_len instead of the truncated len
        write_end could run past the buffer boundary
        read_next_chunk must return 0 if there is nothing left to compress (the last match ran to the end of the already-read data and no new data could be read)
        101..104 was used somewhat sloppily for data table codes
-context-based char encoding
  separate coder table after \0 or after \0..\31
+diffing tables
-repboth, repchar1..3
-split caching hash into two parts - pointers and data
  +cyclic hash for large N
+ChangePair in MFN
  -ChangePair for len1-len2>1
for a sufficiently long and distant match, drop it from the hash on the assumption that the current string will replace it perfectly well
  -do the shift separately, after the match search loop (tried with a non-split CMF)
block-static arithmetic coder - may improve compression by 1-2%
? caching MF for -l2
+ 5/6-byte main hash for highest modes (-7 and up)
hash3+lazy - combine in a different order, since there is no point searching for a 3-byte string after a match?
fill the end of the buffer with random data and remove the p+len<bufend checks
  replace the p+len<=bufend checks with a single one in compress0()
limit the distance checked in -1/-2/-3? so as not to exceed the cache size
rolz 1+2+3+4
minor thoughts:
  small outbuf for -5 and higher modes
  increase HUFBLOCKSIZE for -2/-3  (100k - -0.2sec)

text files -5/-6: disable 2/3-byte searching, repchar and use encode(..., MINLEN=4), switch to hufcoder(?)
hufcoder: disable REPDIST, +fast qsort<>
huf&ari: EOB, check for text-like stats, switch into text mode

use only one bit for flag in bytecoder
bitcoder: 30-bit length encoding - make it a part of 8-bit encoding
huf/ari - improve "first block" encoding, adaptation (currently, up to 1/64 of codespace is wasted),
  +EOB code
? output data in blocks matching the input chunks, storing blocks that did not compress
    header = 1 byte flags + 3 bytes len
more fine-grained disttables for small len
-1,-2,-3?: +no MM, no REP*
huf/ari: use cnt+=10 instead of cnt++ - should increase coding precision (this increases the table size, which slows coding down; possibly the problem can be solved by using 3-level coding tables)
ST4/BWT sorting for exhaustive string searching

speeding up tor:5
  -speeding up the lazy search (Kadach)
  speeding up match comparison (idea from the QuickLZ author)
  -look for MM tables by rep* codes
  optimize huf and switch to it
  for texts:
    don't use 2/3-byte matches
    use huf with a large block instead of arithmetic coding
    don't check for repchar/repdist/repboth
    don't look for MM tables

speeding up / improving compression for tor:7-12
  +use the shift-free hashing technique and -u1
  +2/3hash: increase the size, insert all strings
  +search the large hash for strings of length >=6/7, pushing shorter ones into the auxiliary hash
  skip the characters 0/' ' when hashing
  check matches at repdist distances


+-h1mb in cmdline
+-z/-d options, by default auto depending on file extension
+-h1m -9 == -9 -h1m (apply the preset selection first, then the options that refine it)
+-odir/ -odir\ -od:
+64-bit insize/outsize
+-b128k, m.hashsize instead of hashlog, print block/hashsize in help with k/m suffix
+CHECK mallocs
+dir_exists=file_exists(dir\.) || end_with(:/\)
+progress indicator in console title
-t, -f force overwrite, -k keep src files, stdin->stdout by default
make non-inline as much functions as possible (optimize .exe size): +MatchFinder.cpp +LZ77_Coder.cpp
****Tornado 0.2 compressing VC, 41243 kb     --noheader option disables this
****-1: 16kb hash1...: done 5%
****-1: 16kb hash1...: 17876 kb (12.7%), 23.333 sec, 88.6 mb/s
.tor signature, version, flags, crc
? write the compressed data before reading the next chunk and use storing when there is no compression (zero out the huf/ari table)
? halve the hash back down (first check the effect on other files, 200-300 kb on all)
+print predefined methods definitions in help screen
-mem should demonstrate compression modes from -1 to -9?  -bench for my internal tests
tor_compress: when compressing a file ==buffer there is a redundant data move before reading 0 bytes :)

Changes in 0.2:
    lazy parsing
    3-byte matches
    huffman coder
    sliding window

Changes in 0.3:
    repdist&repchar0 codes
    2-byte matches
    optimized lz parsing
    table preprocessing
    gzip-like cmdline interface?

    -1 thor e1, quicklz
    -2 thor e2, slug
    -3 thor e3, gzip -1
    -4 gzip, rar -m1
    -5 thor, 7zip -mx1
    -6 uharc -mz
    -7 bzip2, rar -m2

Changes in 0.4:
    Cyclic caching MF (makes -9..-11 modes faster)
    Full 2/3-byte hashing in -9..-11 modes which improved compression a bit
    Improved console output to provide more information

*/
