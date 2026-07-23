/*
    REP is an LZ77-family algorithm, i.e. it founds matches and outputs them as
    (len,offset) pairs. It is oriented toward very fast compression and small
    memory overhead (1/4 of buffer size), but limited to rather large values of
    mimimal match length (say, 32), and don't search for optimum match. It's
    intended to preprocess data before using full-fledged compressors. and in
    this area it beats RZIP and, to some degree, LZP preprocessors. Small
    memory overhead means that RZIP/LZP/REP are capable to find matches at very
    long distances and this algorithm does it much better than RZIP and LZP.
    The algorithm implemented in functions REPEncode() and REPDecode().

    Main differences comparing to RZIP:
    1) Sliding window which slides at 1/16 of buffer size each time
    2) Almost ideal hash function (see update_hash)
    3) Direct hashing without hash chains which 1.5x cuts memory requirements
    4) Tags are not saved in hashtable, which again halves memory requirements.
         Instead, a few lower bits of hash table entry are used to save a few
         bits of tag (see chksum)
    5) Hash size is proportional to buffer size (which is equal to the maximum
         search distance) and by default limited to 1/4 of buffer size
    6) In order to find strings of length >=MinLen, blocks of length L=MinLen/2
         are indexed via hash. Of all those possible blocks, only 1/sqrt(L) are
         indexed and only 1/sqrt(L) are searched. It is alternative to solution
         described in RZIP paper where 1/L of blocks are indexed and each block
         searched. This means that logb(sqrt(L)) lower bits of hash entry are
         zeroes which allows to use trick 4.


References for RZIP algorithm explanation and implementations:
http://samba.org/~tridge/phd_thesis.pdf
http://rzip.samba.org/ftp/rzip/rzip-2.1.tar.gz
http://ck.kolivas.org/apps/lrzip/lrzip-0.18.tar.bz2
http://www.edcassa-ict.nl/lrzip.zip
http://www.edcassa-ict.nl/rzip21.zip

TAYLOR, R., JANA, R., AND GRIGG, M. 1997. Checksum testing of remote
synchronisation tool. Technical Report 0627 (November), Defence Science and
Technology Organisation, Canberra, Australia. (p.72)


References for LZP algorithm implementations:
http://magicssoft.ru/content/download/GRZipII/GRZipIISRC.zip
http://www.compression.ru/ds/lzp.rar


** Detailed algorithm description *********************************************************

    This algorithm is a variant of LZ77, i.e. it finds repeated strings in
    the input data and encodes them as (len,offset). Its distinguishing
    feature is that it is oriented toward finding matches of fairly large
    length at large distances. Because of that it uses memory very
    efficiently - as a rule, the search structures require no more than 25%
    of the size of the search window. At the same time it finds practically
    all matches when the minimal length (MinLen) of the searched strings is
    512 bytes, and about 98% - in one of my experiments that searched for
    matches with length from 32 bytes. In practice this algorithm is aimed
    at use as a preprocessor that reduces the redundancy of a file and/or
    finds matches at distances unreachable for the main compression
    algorithm, and in this role it competes with algorithms such as LZP by
    Ilya Grebnev and RZIP. Moreover, as experiments show, for a preprocessor
    the optimal value of the minimal searched string lies exactly in this
    range - 32-512 bytes. This algorithm finds far more matches than
    LZP/RZIP, and besides that, its speed increases as MinLen grows.

    The algorithm is implemented by the functions REPEncode() and REPDecode(),
    and uses a combination of ideas from LZP, RZIP and my own. The search for
    matches is done in a sliding window - the input data is read in blocks of
    1/16 of the buffer size, and this means that at any moment at least 15/16
    of the buffer holds previous data that is scanned for matches. To simplify
    the algorithm, neither input blocks nor matches may cross the buffer
    boundary.

    As usual, in order to find strings of length from MinLen, a checksum (CS)
    is computed for every MinLen-long block of the file and stored in a hash
    table. Since the algorithm targets large values of MinLen, fast
    computation of the CS over blocks of such length is a problem. This
    problem is solved by using a "rolling CS", i.e. one that can be quickly
    recomputed when a new byte is appended at the end of the block and one
    byte is dropped at its beginning (see update_hash).

    Picking the best formula for rolling hashing was a separate adventure. In
    the end the simple formula hash = p[-1] + PRIME*p[-2] +
    PRIME*PRIME*p[-3] + ..., where PRIME is a prime number, turned out to be
    the fastest one and to give a rather uniform distribution. Of course, all
    computations are done modulo 1<<32, kindly provided to us by the CPU :)

    Further, additional measures were used to reduce memory requirements and
    increase speed. Consider for example how the algorithm works for
    MinLen=512. Since any 512-byte block includes a 256-byte block starting
    at a position that is a multiple of 256, it is enough for us to insert
    into the hash table references only to those blocks and to look for
    matches only with them. Of course, when checking a match we are not
    limited to exactly 256 bytes, but try to extend it as far as possible in
    both directions. This is exactly what allows a significant reduction of
    memory usage while guaranteeing that almost all matches are found - at
    least when MinLen is large enough.

    However one can go even further - instead of inserting every 256th block
    into the hash table but searching every single one, we can for example
    insert every 32nd and search every 8th, or insert every 2nd and search
    every 128th. Of course, the optimum is to insert and search every 16th
    block. More precisely, we should insert one block every 16 bytes, and
    search the first 16 blocks out of every 256, i.e. we insert the blocks
    starting at positions 0, 16, 32..., and search the blocks starting at
    positions 0, 1, 2..., 15, 256. 257... In this way, for MinLen=512 an
    8-fold speedup is achieved (thanks to an 8-fold reduction in the number
    of memory accesses) compared to a straightforward implementation - at the
    cost, admittedly, of increased memory requirements (from 1/64 of the
    buffer size up to 1/4, which in my opinion is quite acceptable).

    Finally, the last trick is using the low bits of a hash table entry to
    store several bits of the hash function value (chksum) - those, of
    course, that are not part of the index into the hash table. This lets us
    filter out most false matches without comparing the contents of the
    blocks, and thereby reduce the number of memory accesses and speed the
    program up even more.

    The algorithm uses hashing with direct addressing, without secondary
    hashing, which makes the implementation very simple. The hash value of
    a 256-byte block (in the general case this block's size is L=MinLen/2)
    is used as an index into the hash table (hasharr[hash&HashMask]); on a
    collision the new block simply replaces the earlier one. In practice
    this (practically) does not degrade compression. Let me emphasize once
    more that this algorithm, unlike full-fledged LZ77 implementations,
    does not look for the optimal (longest) match, but checks only one
    reference - to the last block that took this hash slot and whose CS
    therefore presumably matches the CS of the current block.

    Hash size (HashSize): while developing the algorithm I assumed it should
    be 2-4 times larger than the number of elements that would have to be
    inserted into it. In practice, however, it turned out that having the
    same number of slots is quite enough, and for MinLen=32 - even four times
    (!) fewer. That is, for example, for a 32 mb block with MinLen=512 every
    16th 256-byte block is inserted into the hash and the total number of
    inserted elements is 32M/16=2M, i.e. 8 mb, and the hash is created
    exactly of that size. For MinLen=32 the total number of elements is
    32M/4=8M, but we create a hash table four times smaller, so we get the
    same 8 mb. Thus the hash table size chosen automatically by the algorithm
    never exceeds 1/4 of the input buffer size. If you want to set another
    value - use the HashBits parameter (the -h option). Increasing HashSize
    with small MinLen can slightly improve the compression ratio.

    Amplifier: as described above, the search checks only a fraction of the
    blocks, which would be guaranteed to be enough to find all strings of
    length >=MinLen - if our hashing were perfect. However our hashing is
    not perfect, and because of that a part of the potential hits is lost.
    The Amplifier parameter (the -a option) allows requesting that a larger
    number of blocks be tested (exactly Amplifier times more). Thus, for
    the most thorough search one can simply set Amplifier to a sufficiently
    large value, say 99. Of course, this reduces the speed and increases
    compression only slightly.

    Barrier and SmallestLen: some algorithms, in particular ppmd, benefit
    if the preprocessor uses a smaller value of MinLen for large distances.
    These two parameters allow setting a two-step threshold for selecting
    matches, for example "in the first megabyte - MinLen=128, further on
    MinLen=32" is specified via MinLen=128, Barrier=1<<20, SmallestLen=32
    (the options -l128 -d1048576 -s32). The string search is then tuned,
    naturally, to find strings of length from SmallestLen instead of MinLen.


** Benchmarks using 1GHz processor ****************************************************************

Test results for 26mb:
        Compression time   Compressed size
-l8192  0.5 seconds
 -l512  1.1
 -l128  1.4
  -l32  2.5                12.7 mb
lrzip   2.6                14.1
lzp:h20 6.5                13.1
lzp:h13 3.0                20.6

Compression speed on incompressible data:
-l8192  52 mb/sec
 -l512  25 mb/sec
 -l128  17 mb/sec
  -l32   8 mb/sec
lrzip    8 mb/sec


** REP with a dictionary larger than RAM***********************************************************

by the way, since the conversation is revolving around rep with large
dictionaries. by default it uses, say, 1gb for the data itself and four times
less - for the index. the data history has to be kept only because the simple
4-byte hash computed over those 512 bytes is unreliable in terms of collisions.
now imagine that instead of it we store a 16-byte cryptographically strong
hash - like md5. then the history can be thrown away entirely. moreover, the
fact that the hash table takes a quarter of the volume of the indexed data is
not mandatory. if we store such a hash for every 256-byte block of data, that
guarantees us finding all matches of length from 511 (since any such match
includes at least one full 256-byte block starting at a 256-byte
boundary). i.e. to search for strings of length 511+ with an N-gb history,
N/16 gb of memory is enough

problems arise only during decompression :D  if during compression we don't
need the old data - it is enough to be 100% sure that it matches, then during
decompression we do after all have to copy it from the old place :D  if we
assume that we will keep all this data on disk instead of in RAM, then copying
each string will require a disk read operation whose overhead is practically
equal to the disk seek time - i.e. 10 ms for a hard drive and 1 ms for a very
good flash drive

let's imagine that we want to provide a decompression speed of, say, 1 mb/s.
that means that every 10 ms we must decompress at least 10 kb, which in turn
is guaranteed only if our rep encodes only matches of length
10kb+

say, if we settle on encoding strings of length 4kb+, then compression will
require N/128 gb of memory (i.e. your 18 gigs can be combed through using
only 160 megs of ram) and the decompression speed will be limited to 400 kb/s.
it's just a pity about the hard drive :D

Ghost, try it out of curiosity - how does the compression of your data change
when going from rep:512 (the default) to rep:4096? with further lzma processing
and without it. of course, this is only a rough estimate, since in reality rep
currently can't handle large distances :(  maybe i should add that quickly,
without the ability to decompress...

*/


// HOUSEKEEPING ********************************************************************
#include "../Compression.h"


#ifdef REP_LIBRARY
#define stat1(nextmsg,size)
#else
void stat1 (char *nextmsg, int Size);
#endif


// COMMAND-LINE OPTIONS ************************************************************************
#ifndef REP_LIBRARY
// Amount of information printed to stdout
//   0   errors only
//   1   general statistics
//   2   detailed information about the process
static int verbose = 0;

#endif


// HELPER FUNCTIONS ****************************************************************************

// Exponentiation
inline static unsigned power (unsigned base, unsigned n)
{
    int result;
    for (result=1; n != 0; result *= base, n--);
    return result;
}

// The largest power of base not exceeding sqrt(n),
// for example sqrtb(36,2) = 4
inline static unsigned sqrtb (unsigned n, unsigned base = 2)
{
    int result;
    for (result=1; (n/=base*base) != 0; result *= base);
    return result;
}

// Finds the address of the match start, going backwards from *p and *q
static inline byte* find_match_start (byte* p, byte* q, byte* start)
{
    while (q>start)   if (*--p != *--q)  return q+1;
    return q;
}

// Finds the address of the first mismatching byte, going forward from *p and *q
static inline byte* find_match_end (byte* p, byte* q, byte* end)
{
    while (q<end && *p==*q) p++,q++;
    return q;
}

// Copies data from buffer to buffer, going in order of increasing addresses
// (this is important, since the buffers may overlap and in that case we need
// to replicate the existing data)
static inline void memcpy_lz_match (byte* p, byte* q, unsigned len)
{
    if (len)
    do *p++ = *q++;
    while (--len);
}


// Buffer used to organize several independent write streams in the program.
// The buffer can write 32-bit numbers into itself. Later the contents of the
// buffer are flushed into the output stream.
// Additionally the buffer supports reading back data previously written to it.
// The end of the written part of the buffer is max(p,end), where p is the
// current pointer and end is the maximum position of previously written data.
// No overflow check is performed, since the algorithm guarantees that
// overflow will not happen.
struct Buffer
{
    byte*  buf;                 // address of the allocated buffer
    byte*  p;                   // current read/write pointer inside this buffer
    byte*  end;                 // address just past the end of the read/written data
    byte*  bufend;              // end of the allocated buffer
    byte   smallbuf[16];        // small buffer used for writing individual values
    int    len()                { return mymax(p,end)-buf; }
    Buffer (int size)           { buf=p=end= size<sizeof(smallbuf)? smallbuf : (byte*) BigAlloc(size);  bufend=buf+size;}
    void   free ()              { if (bufend>buf+sizeof(smallbuf))  BigFree(buf);  buf=p=end=NULL; }
    void   put32(int x)         { *(int32*)p = x; p+= sizeof(int32); }  // only for FREEARC_INTEL_BYTE_ORDER!
    void   put(void *b, int n)  { memcpy(p,b,n); p+= n; }
// For reading data
    void   rewind()             { end=mymax(p,end); p=buf; }
    int    get32()              { int x = *(int32*)p; p+= sizeof(int32); return x; }
    bool   eof()                { return p>=end; }
// For FWRITE
    int    remainingSpace()     { return bufend-p; }
    void   empty()              { p=end=buf; }
};

// Write a 32-bit number to the output stream
#define Put32(x)                                           \
{                                                          \
    Buffer header(sizeof(int32));                          \
    header.put32 (x);                                      \
    FWRITE (header.buf, header.len());                     \
    header.free();                                         \
}


// MAIN ALGORITHM ************************************************************************

/*
    To find matches of length from MinLen bytes we must store in the hash the values
    of the check function over blocks of length L = MinLen/2 bytes with a period of k = sqrt(L) bytes.
    In that hash table we look for matches for the blocks starting in the first test=k bytes
    of each block of length L bytes.
*/

#define update_hash(sub,add)                        \
{                                                   \
    hash = hash*PRIME + add - sub*cPOWER_PRIME_L;   \
}

#define chksum         ((hash>>28)&k1)
#define PRIME          153191           /* or any other prime number */
#define POWER_PRIME_L  power(PRIME,L)

const int MAX_READ = 8*mb;  // Max. amount of input data read at a time


// Compute the number of hash elements
MemSize CalcHashSize (MemSize HashBits, MemSize BlockSize, MemSize k)
{
    // The hash size should match the number of values we want to store in it, but not exceed a quarter of the buffer size / the amount of input data (Size/16*sizeof(int)==Size/4)
    return HashBits>0? (1<<HashBits) : roundup_to_power_of(BlockSize/3*2,2) / mymax(k,16);
}

#ifndef FREEARC_DECOMPRESS_ONLY
// DARC_RUST=1 selects the Rust port of the encoder (rust/darc-codecs), verified
// byte-identical to this function (rust/difftest/rep_ref.cpp), and excluded here
// for the same reason as the decoder: two definitions of rep_compress fail the
// GNU ld link.
#ifndef DARC_RUST
int rep_compress (unsigned BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata)
{
    // ALGORITHM PARAMETER SETUP  (a copy of this is in REP_METHOD::GetCompressionMem!)
    if (SmallestLen>MinMatchLen)  SmallestLen=MinMatchLen;
    int L = roundup_to_power_of (SmallestLen/2, 2);  // Size of the blocks whose CS is stored in the hash
    int k = sqrtb(L*2), k1=k-1, test=mymin(k*Amplifier,L), cPOWER_PRIME_L = POWER_PRIME_L;
    int HashSize, HashMask=0, *hasharr=NULL, hash=0;  int errcode=FREEARC_OK;
    int Base=0, last_i=0, last_match=0;    // last_match points to the end of last match written, we shouldn't start new match before it
#ifdef DEBUG
    int matches=0, total=0, lit=0;
#endif
    byte *buf = (byte*) BigAlloc(BlockSize);   // Buffer where the input data will be placed
    if (buf==NULL)  return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;    // Error: not enough memory
    FOPEN();

    int bsize = (mymin(BlockSize,MAX_READ)/SmallestLen+1) * sizeof(int32);    // Max. amount of data that can be written into the lengths or offsets buffer
    Buffer lens(bsize), offsets(bsize), datalens(bsize), dataOffsets(bsize);  // Buffers for storing separately the lengths, match offsets, lengths of the uncompressed blocks and the blocks themselves. This grouping allows increasing the final compression ratio

    // Each iteration of this loop reads, processes and writes one data block
    // of min(1/8 of the buffer, 8mb) in size. This provides sliding window behavior,
    // i.e. the ability to search for matches in previous data over almost the whole buffer length
    for (int FirstTime=1; ; FirstTime=0) {

        // READING THE INPUT DATA
        int Size = callback ("read", buf+Base, mymin (BlockSize-Base, FirstTime? MAX_READ : mymin (BlockSize/8, MAX_READ)), auxdata);
        if (Size < 0)  {errcode=Size; goto finished;}   // Error: can't read input data
        if (FirstTime) {
            HashSize = CalcHashSize (HashBits, BlockSize, k);
            HashMask = HashSize-1;
            hasharr  = (int *) BigAlloc (HashSize * sizeof(int));
            if (HashSize && hasharr==NULL)  {errcode=FREEARC_ERRCODE_NOT_ENOUGH_MEMORY; goto finished;}   // Error: not enough memory
            memset (hasharr, 0, HashSize * sizeof(int));
            debug (verbose>0 && MinMatchLen==SmallestLen && printf(" Buf %d mb, MinLen %d, Hash %d mb, Amplifier %d\n", ((Size-1)>>20)+1, MinMatchLen, (HashSize*sizeof(int))>>20, test/k));
            debug (verbose>0 && MinMatchLen!=SmallestLen && printf(" Buf %d mb, MinLen %d, Barrier %d, Smallest Len %d, Hash %d mb, Amplifier %d\n", ((Size-1)>>20)+1, MinMatchLen, Barrier, SmallestLen, (HashSize*sizeof(int))>>20, test/k));
            Put32 (BlockSize);   // Write the dictionary size to the output stream
        }
        if (Size == 0) break;  // No more input data
        debug (verbose>0 && printf(" Bytes read: %u\n", Size));
        if (Base==0)  {   // The first time or after wrapping around the buffer boundary
            hash=0;  for (int i=0; i < mymin(L,Size); i++)  update_hash (0, buf[i]);  // Initial hash value - the CS of the first L bytes of the buffer
        }
        int literals=0; lens.empty(), offsets.empty(), datalens.empty(), dataOffsets.empty();  // Clear the buffers

        // MAIN LOOP THAT FINDS REPEATED STRINGS IN THE INPUT DATA
        for (int i=last_i; i+L*2 < Base+Size; last_i=i) {   // We process L bytes per loop iteration + we need L bytes of lookahead

            // LOOK FOR A MATCH IN THE FIRST test BYTES OF THE BLOCK OF LENGTH L
            for (int j=0; j<test; j++, i++) {
                if (i>=last_match) {   // We check for a match only if the previously found match has already ended
                    int match = hasharr[hash&HashMask];
                    if (match && chksum==(match&k1)) {  // The low bits of the match value hold the checksum chksum. Comparing it lets us skip a useless block comparison in the case of a hash collision (one hasharray element used for different hash values)
                        match &= ~k1;   // Strip the CS from match. Now i and match are the addresses of presumably matching blocks of length L
                        if (match>=i && match<Base+Size)  goto no_match;  // match points into data not yet processed, i.e. it is definitely stale
                        // The smallest/largest value that an index based on i may take
                        // during the search, so that an index based on match
                        // does not go outside the buffer and does not peek into future data
                        int LowBound  = match<i? i-match : match-(Base+Size)>i? 0 : i - (match-(Base+Size));
                        int HighBound = BlockSize - match + i;
                        // Find the real start and end of the match, comparing forward and backward from buf[i] <=> buf[match]
                        // i is bounded from below and above by last_match and Base+Size, respectively
                        int start = find_match_start (buf+match, buf+i, buf+mymax(last_match,LowBound)) - buf;
                        int end   = find_match_end   (buf+match, buf+i, buf+mymin(Base+Size,HighBound)) - buf;
                        // start and end are the boundaries of the match around i. Check that the found match has length >=MinMatchLen (or SmallestLen, if the distance is >Barrier)
                        if (end-start >= (i-match<Barrier? MinMatchLen : SmallestLen) ) {
                            int offset = i-match;  if (offset<0)  offset+=BlockSize;
                            // Match found! Write information about it into the output buffers
                            dataOffsets.put32 (last_match);         // Address of the uncompressed data
                               datalens.put32 (start-last_match);   // Length of the uncompressed data
                                offsets.put32 (offset);             // Offset of the match
                                   lens.put32 (end-start);          // Length of the match
                            // Remember the end position of the found match and print debug statistics
                            debug ((matches++, total += end-start, lit += start-last_match));
                            debug (verbose>1 && printf ("Match %d %d %d  (lit %d)\n", -offset, start, end-start, start-last_match));
                            literals += start-last_match;  last_match=end;
                        }
                    }
                }
      no_match: // We store new blocks into the table every k bytes. If Amplifier=1, this line fires only at j=0, and the remaining blocks are indexed in the following loop
                if ((i&k1) == 0)  hasharr[hash&HashMask] = i + chksum;
                update_hash (buf[i], buf[i+L]);  // Update the sliding hash, adding buf[i+L] into it and removing buf[i]
            }
            // NB! Align to a position that is a multiple of k!

            // STORE NEW BLOCKS INTO THE TABLE EVERY k BYTES UNTIL THE END OF THE CURRENT BLOCK OF LENGTH L
            while ((i&(L-1)) != 0) {
                hasharr[hash&HashMask] = i + chksum;
                for (int j=0; j<k; j++, i++)   update_hash (buf[i], buf[i+L]);
            }
        }

        // OUTPUT OF THE COMPRESSED DATA TO THE OUTPUT STREAM AND PREPARATION FOR THE NEXT PORTION OF DATA
        Base += Size;
        if (Base==BlockSize)  last_i=Base;       // Encode all data up to the end of the buffer
        if (last_match > last_i) {               // If the last match ends in a not yet indexed area
          datalens.put32 (0);                    //   Nothing needs to be encoded, but datalens must still be exactly one entry longer than lens/offsets
        } else {
          // Write into the output buffers the remaining data from the last found match up to the last indexed position
          dataOffsets.put32 (last_match);          // Address of the remaining data
             datalens.put32 (last_i-last_match);   // Length of the remaining data
          literals  += last_i-last_match;
          last_match = last_i;
        }
        if (Base==BlockSize) {       // If we wrap around the buffer boundary
          Base=last_match=last_i=0;  //   Yes! Start filling the buffer from the beginning!
        }
        // Write the size of the compressed data and the number of found matches into the buffer
        int outsize = sizeof(int32)*2+lens.len()+offsets.len()+datalens.len()+literals;
        QUASIWRITE (outsize);
        Put32 (outsize-sizeof(int32));
        Put32 (lens.len()/sizeof(int32));
        // Output the buffer contents and the uncompressed data to the output stream
        FWRITE (    lens.buf,     lens.len());
        FWRITE ( offsets.buf,  offsets.len());
        FWRITE (datalens.buf, datalens.len());
        dataOffsets.rewind(); datalens.rewind();
        while (!dataOffsets.eof()) {
            FWRITE (buf + dataOffsets.get32(), datalens.get32());
        }
        FFLUSH();
        // Debug statistics
        debug (verbose>0 && printf(" Total %d bytes in %d matches (%d + %d = %d)\n", total, matches, sizeof(int32)*2+lens.len()+offsets.len()+datalens.len(), lit, sizeof(int32)*2+lens.len()+offsets.len()+datalens.len()+lit));
    }

    // Write the final block containing the uncompressed remainder of the data, and 0 - the end-of-data marker
   {int datalen = Base-last_match;
    Put32 (sizeof(int32)*2 + datalen);  // Length of the compressed block
    Put32 (0);                          //   0 matches in this block
    Put32 (datalen);                    //   Length of the remaining data
    FWRITE (buf+last_match, datalen);   //   The data itself
    Put32 (0);}                         //   EOF flag (see below)
finished:
    FCLOSE();
    BigFree(hasharr);
    BigFree(buf);
    lens.free(); offsets.free(); datalens.free(); dataOffsets.free();
    return errcode>=0? 0 : errcode;
}
#endif  // !DARC_RUST (rep_compress)
#endif // FREEARC_DECOMPRESS_ONLY


// Classical LZ77 decoder with sliding window
// DARC_RUST=1 selects the Rust port of the decoder (rust/darc-codecs). It is
// verified byte-identical to this function (rust/difftest/rep_ref.cpp), and
// must be EXCLUDED here rather than merely provided alongside: with both
// definitions present the link fails with "multiple definition of
// rep_decompress" (GNU ld) or silently resolves from this object, the same
// linkage trap the other ported codecs avoid. The encoder (rep_compress) is
// still C -- REP is ported decode-first.
#ifndef DARC_RUST
int rep_decompress (unsigned BlockSize, int MinCompression, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode, bufsize, ComprSize; byte *data0=NULL, *data, *buf0=NULL;

    // The actual dictionary size is stored in the input data
    READ4(BlockSize);
    data = data0 = (byte*) BigAlloc (BlockSize);

    // Buffer where the input data will be placed
    bufsize = mymin(BlockSize,MAX_READ)+1024;
    buf0 = (byte*) BigAlloc (bufsize);
    if (data0==NULL || buf0==NULL)  ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);

    // A loop, each iteration of which processes one block of compressed data
    for (byte *last_data=data; ; last_data=data) {

        // Read one block of compressed data
        READ4(ComprSize);
        if (ComprSize == 0)  break;    // EOF flag (see above)

        // ComprSize is read straight from the (possibly corrupt) stream. A
        // negative value made READ below memcpy a negative -> huge size; a
        // value too small to hold the block header let the table parsing walk
        // off the input buffer. The smallest legal block is the num field plus
        // datalens[0], i.e. 2*sizeof(int32). Reject anything below that.
        if (ComprSize < (int)(2*sizeof(int32)))  ReturnErrorCode (FREEARC_ERRCODE_BAD_COMPRESSED_DATA);

        if (ComprSize > bufsize)
        {
            BigFree(buf0); bufsize=ComprSize; buf0 = (byte*) BigAlloc(bufsize);
            if (buf0==NULL)  ReturnErrorCode (FREEARC_ERRCODE_NOT_ENOUGH_MEMORY);       // Error: not enough memory
        }
        byte *buf = buf0;

        READ(buf, ComprSize);

        // The block header contains the size of the lens/offsets/datalens tables; then come the tables themselves and finally the uncompressed data
        byte *buf_end  = buf0 + ComprSize;   // end of the input block
        byte *data_end = data0 + BlockSize;  // end of the output buffer

        int         num = *(int32*)buf;  buf += sizeof(int32);           // Number of matches (= the number of entries in the lens/offsets/datalens tables)
        // num is untrusted. It sizes three tables (lens, offsets: num each;
        // datalens: num+1) plus the num field itself: sizeof(int32)*(3*num+2)
        // bytes, which must fit inside the block. Validate it in 64-bit BEFORE
        // deriving the table pointers, because a corrupt num otherwise overflows
        // the pointer arithmetic and every datalens[i]/lens[i] read is wild.
        if (num < 0  ||  (int64)sizeof(int32) * (3*(int64)num + 2) > ComprSize)
            ReturnErrorCode (FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        int32*     lens =  (int32*)buf;  buf += num*sizeof(int32);
        int32*  offsets =  (int32*)buf;  buf += num*sizeof(int32);
        int32* datalens =  (int32*)buf;  buf += (num+1)*sizeof(int32);   // More precisely, datalens contains num+1 entries

        // Each iteration of this loop copies one block of uncompressed data and one match, which are interleaved in our implementation of the compression process
        for (int i=0; i<num; i++) {
            // Every copy length is untrusted. Validate the literal against both
            // the remaining input (buf_end) and the remaining output (data_end),
            // and the match against the output, before copying -- otherwise a
            // corrupt datalens[i]/lens[i] overruns the data0 heap block. The
            // decompressor is fed raw archive bytes, so this fires on ordinary
            // corruption, not only crafted input (verified: a single flipped
            // byte in a -mrep archive reached the old unchecked memcpy).
            int dl = datalens[i];
            if (dl < 0  ||  dl > buf_end - buf  ||  dl > data_end - data)
                ReturnErrorCode (FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            memcpy (data, buf, dl);  buf += dl;  data += dl;
            debug (verbose>1 && printf ("Match %d %d %d\n", -offsets[i], data-data0, lens[i]));
            // If the offset falls before the start of the buffer, subtract BlockSize from it in order to "wrap" around the buffer boundary
            int offset = offsets[i] <= data-data0 ?  offsets[i] : offsets[i]-BlockSize;
            int ln = lens[i];
            // offset must land the match source inside [data0, data); ln must
            // keep the destination inside the output buffer.
            if (offset <= 0  ||  offset > data - data0  ||  ln < 0  ||  ln > data_end - data)
                ReturnErrorCode (FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            memcpy_lz_match (data, data-offset, ln);  data += ln;
        }
        // Plus one more block of uncompressed data at the very end (possibly of zero length)
        int dl = datalens[num];
        if (dl < 0  ||  dl > buf_end - buf  ||  dl > data_end - data)
            ReturnErrorCode (FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
        memcpy (data, buf, dl);  buf += dl;  data += dl;

        // Output the decompressed data, print debug statistics and prepare for the next loop iteration
        WRITE(last_data, data-last_data);
        debug (verbose>0 && printf( " Decompressed: %u => %u bytes\n", ComprSize+sizeof(int32), data-last_data) );
        if (data==data0+BlockSize)  data=data0;   // wrapping around the buffer boundary can happen only at the end of a block
        // NB! check that buf==buf0+Size, data==data0+UncomprSize, and add buffer overflowing checks inside cycle
    }
    errcode = FREEARC_OK;
finished:
    BigFree(data0);  BigFree(buf0);  return errcode;
}

#endif  // !DARC_RUST (rep_decompress)

/* to do:
+1. sliding window, In() function to read data
+2. memory deallocation, including on errors
+3. save pointers to unmatched blocks instead of copying data
4. Check whether the block compressed poorly, and replace it with a single literal.
     More precisely, restore the previous value of last_match and clear all output buffers
5. last_small_match - if a small match is found at a short distance (<Barrier),
     then ignore small matches at large distances (>Barrier) until this one ends.
     This will let us stop taking the bread from the big guys :)
6. -l8192 -s512
7. buffer data for Out() in 256k blocks

Fixed bugs:
1. Buffer boundary overflow check: offset<data-data0 instead of <=
2. last_match was not reset on loop exit when Base=0 and Size=0
*/
