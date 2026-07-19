/*

DICT - a dictionary substitution algorithm. The dictionary is built
       while the program runs; the dictionary that was used is written
       out ahead of the encoded data

Macros controlling compilation:
PPMD_VERSION - build a version of the program aimed at better compression by PPM algorithms
DEBUG        - enable collection of the statistics printed by the -v options
DICT_LIBRARY - omit main() and the other functions needed only by the standalone program

To-do list:
 + experiments with GOOD_WORD
 + use rare characters (though "for exe, doc and the like" this didn't work at all :()
 + relax the word requirements for "fast mode" (NB! make this parameterizable)
 + dictionary/encoding statistics
 + FindWord sped up 7-fold
 + fixed bugs such as an empty dictionary
 + try [a-z]+ words - no luck!
 + data scanning - use a hash that guarantees finding the word (as in encoding)
 + sort the dictionary to improve its compression (1- and 2-byte strings separately)
 + dictionary encoding: word separator and how many letters to repeat from the previous word
 + -DDICT_LIBRARY - do not define the main() function
 - DictDecode() - use read/write functions so it can work in a fixed amount of memory
 - provide an exit path if too few words are found, too few weak chars, and in other borderline cases
 - tune the word selection parameters based on the input data (ghc-src - VERY_GOOD_CNT=8000, rus - option_fast...)
 - dictionary encoding: word endings
 - exe: we love any chars! ;)
 - make all variables local and use malloc
 - use chr+' ' to encode a stolen chr - impossible


HOW THE ALGORITHM WORKS

1. Walk over the file, building a list of words:
   * a hash table of 4-byte entries records the word's counter (2 bytes) and its parent's hash (2 more bytes)
   * the parent hash is used to detect collisions; on a collision up to 13 rehashes are performed
   * for each word we look at its counter in the table:
       counter=0 -> add the word to the list being built and set counter:=1
       counter<5 -> counter++
       otherwise -> append the next letter from the input text to the word and repeat the loop
   * on leaving the loop we restart it from the next two-letter word of the input text

2. Walk the word list and hand the counters of single-child words over to their children (such words are
   easy to spot by their counter having the minimum possible value), on the assumption that those
   counters record occurrences of those very children before permission to create them was granted.
   Then walk the word list from the end to the beginning (so that derived, longer words are handled
   first) and hand the counters of overly rare words over to their parents. The counters are taken from
   the hash table built in the first step

3. Now we have a list of the most frequently used words with their counters, plus the frequencies of the
   individual characters in the input text, which are easy to gather along the way in the first pass.
   Having sorted both lists by frequency, we easily learn which characters can be used for encoding
   words and which words will be encoded by one- and which by two-letter abbreviations

4. Assign codes to the words

5. Write the dictionary to the output stream

6. Build a small hash for all the words in use and their partial prefixes.
   This hash must let us scan the text up to the end of a word and determine its code.
   Since the number of words is <10k and their total length is <100k, this hash should fit entirely
   into the CPU cache

7. Encode the text using this hash


HOW TO PREVENT PHANTOM WORDS WITH FREQUENCY 4 FROM APPEARING?

1. Walk the word list from beginning to end. If the parent has the minimum frequency that
   a word with a child can possibly have, that means the current word is its
   only child, and it is reasonable to assume that all occurrences of the parent word
   were in fact the current word. In this case take that frequency over, marked as "phantom frequency".
   As a result all phantom frequencies should "roll down" to the longest words of that
   branch (IMPLEMENTED AS THE FIRST PART OF STAGE 2)

2. Alternatively - make a second pass over the file, counting the frequencies of already known words
   without creating new ones


DISTRIBUTING WORDS BETWEEN ONE-BYTE AND TWO-BYTE CODES

1. Give one-byte codes to the most frequent words, regardless of their length (IMPLEMENTED)
2. Hand the codes of overly rare characters over to words (IMPLEMENTED)


WRITE THE TEXT OF ONE-BYTE WORDS AT THE START OF THE FILE, AND OF TWO-BYTE ONES AT THEIR FIRST USE

0x11 0x12 abcde .... 0x11 0x12 (encodes the word abcde). The _lengths_ of all words are still written at the start of the file


FOR THE WORD TESTS (MIN_VISITS_TO_HAVE_SON, ALLOW_TO_EXTEND_WORD, GOOD_WORD) USE
CHARACTER AND LETTER-PAIR PROBABILITIES THAT CAN BE GATHERED IN A PRELIMINARY PASS

For instance, ALLOW_TO_EXTEND_WORD(c1,c2) = True if the probability of c2 following c1 is < 10%
In general, the anti-context modelling principle should apply - remember what is genuinely unusual


IMPROVING COMPRESSION

1. An extra pass to gather real statistics (dictionary optimization with a relaxed GOOD_WORD criterion)
2. When forming a word, take into account the probabilities p0[-1], p0[0], p[-1], p[0] and the cnt/cnt0 ratio
     so as to avoid forming words like "yteString" as much as possible
3. If the word 'Message' is among Good_Words, reduce the count for the word 'essage' fourfold
     (provided there are no more than 2 words of the form '?essage'). This gets rid of tons of garbage
4. For binary files: use more characters (the criterion there is currently word_count>10*char_count) (IMPLEMENTED)


IMPROVING SPEED

1. Step 4 bytes at a time, start the word search at 2 bytes (IMPLEMENTED)
2. After a collision, look for a match within the CPU cache line first (16/32 bytes). Pointless - the number of hash collisions never exceeds 10-20%
3. Place "abc", "abcd", "abcde" in a single CPU cache line and only do a full update_hash once it is exhausted
4. Bring back the binary search, but perform it only after 1-2-4-byte indexing. To make it
     exact, the input text has to be compared against both words it fell between, and from the
     one closer in letter count we walk back along the chain of its prefix words.
     Separate the binary search array from the array of full word information so that
     it can slide into the cache with ease (10k words - 40kb, plus 100kb of their text)


REDUCING MEMORY CONSUMPTION

1. The number of words created is 50-100 times smaller than the file size. Each word requires
     16 bytes in the FirstWord array and 4 bytes in the scan_hash hash, but in the latter it is desirable
     to have a 4-fold surplus of entries to keep performance high.
     That comes to 32 bytes per word, which with a word count equal to 1/32 of the input data volume
     takes as much memory as the entire input file. Besides, for atypical files it is desirable
     to be able to create a larger number of words
2. The hash table can be halved (IMPLEMENTED), the Word structure shrunk to 9 bytes (byte len, byte3 hash, byte hash0_high, cnt_t count)
     That gives 17 bytes per word (instead of 32), with 10-20% degradation of overall performance
3. FirstWord - use a list of blocks instead of a fixed-size table
4. If only words of even (or multiple-of-4) length are inserted into the hash table,
     their number can be cut by half or by a factor of four

*/


// HEURISTICS FOR SELECTING STRINGS ****************************************************************
// Number of repetitions of a word before its "children" are added to the dictionary (depends on its length)
#define MIN_VISITS_TO_HAVE_SON(len)  ((len)>10? 2 : 5)

// A philosophical question - may a word ending with character c1 have a child by character c2?
#define ALLOW_TO_EXTEND_WORD(c1,c2)  (char_class(c1)==char_class(c2))
// To settle this question all characters are split into classes (as in any self-respecting society).
// Every decent word must consist only of characters belonging to a single class.
// There are currently two classes: control characters plus space, and everything else

// When cleaning up the dictionary, keep only the words whose use yields a large enough gain.
// Here cnt/len are the word's counter/length, cnt0 is its parent's counter
#define GOOD_WORD(cnt,cnt0,len)  (cnt>MinLargeCnt                                     \
                                  || (cnt0?  cnt>MinMediumCnt && cnt>cnt0*MinRatio    \
                                          :  cnt>MinSmallCnt))                        \

// Is this word worth encoding even with two bytes?
#define GOOD_2BYTE_WORD(len,cnt)  (len>=4)


// ALGORITHM CONSTANTS ****************************************************************
// Nominal one-byte-string length for those bytes that start two-byte strings
#define USE_DICT2                    1
// This character is never used for word codes; instead it is treated specially
#define RESERVED_CHAR                ' '
// Max. length of an encodable word
#define MAX_WORD_LEN                 254


// HOUSEKEEPING ********************************************************************
#include <stdio.h>
#include <ctype.h>
#include <limits.h>

#include "../Compression.h"

typedef int            count_t;  // Word counters

#ifdef PPMD_VERSION
// Full-size counters, improve PPMD compression by 0.2%
#define SCNT_MAX INT_MAX
typedef int      scnt_t;
typedef unsigned hash0_t;
#else
// To save memory and get better/faster compression with lzma/ppmonstr
#define SCNT_MAX SHRT_MAX
typedef short  scnt_t;
typedef ushort hash0_t;
// If anyone ever needs a version able to compress for both ppmd and lzma,
// then for it these definitions should look like:
//#define SCNT_MAX (option_ppmd? INT_MAX : SHRT_MAX)
//typedef int scnt_t;
#endif

#ifdef DEBUG
// A bit of statistics
int FindWord_calls     // number of FindWord calls
  , OutByte            // number of bytes written out
  , OutByte2           // number of bytes encoded with two bytes
  , OutWord            // number of words written out
  , used_hash1[13+1]   // number of hash collisions during parsing
  , used_hash2[13+1]   // number of hash collisions during encoding
  , depth_cnt[MAX_WORD_LEN+2]      // number of hash lookups at depth (word length) n
  , matrix_cnt[MAX_WORD_LEN+2][MAX_WORD_LEN+2]   // number of retreats from position i by j steps back
  , mc[MAX_WORD_LEN+2]                           // column sums of matrix_cnt
  , increment_cnt[MAX_WORD_LEN+2]  // number of increments of a too-small counter for words of various lengths in phase1
  , addword_cnt[MAX_WORD_LEN+2]    // number of AddWord operations for words of various lengths in phase1
  , badword_cnt[MAX_WORD_LEN+2]    // number of counter increments in bad_word for words of various lengths in phase1
  ;
#endif

#ifdef DICT_LIBRARY
#define stat1(nextmsg)
#define stat2(nextmsg)
#else
void stat1 (char *nextmsg);
void stat2 (char *nextmsg);
#endif


// COMMAND-LINE OPTIONS ************************************************************************
// Amount of information printed to stdout
//   0   errors only
//   1   summary statistics for each step of the process
//   2   information about the surviving words only
//   3   information about all words and every stage of their life
//   4   + information about the encoding process
static int verbose = 0;

// Print the execution time of each step of the algorithm
int print_timings = 0;

// Use only one-byte codes for words
int use_plain_dictionary = 0;


#ifndef FREEARC_DECOMPRESS_ONLY


// DATA STRUCTURES ***********************************************************************
count_t char_counts[UCHAR_MAX+1];   // Occurrence counters for individual characters in the source text

// Split all characters into several classes. A word must consist of characters of a single class only
char char_class_table[UCHAR_MAX+1] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 0-15
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 16-31
#if 1
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  //  !"#$%&'()*+,-./
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0123456789:;<=>?
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // @ABCDEFGHIJKLMNO
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // PQRSTUVWXYZ[\]^_
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // `abcdefghijklmno
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // pqrstuvwxyz{|}~
#else  // Alternative variant, gives a gain on 4dos.doc and Montezuma's Daughter
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //  !"#$%&'()*+,-./
    1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,  // 0123456789:;<=>?
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // @ABCDEFGHIJKLMNO
    1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,  // PQRSTUVWXYZ[\]^_
    0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // `abcdefghijklmno
    1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,  // pqrstuvwxyz{|}~
#endif
#if 1
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
#elif 0
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
#else
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // Cyrillic capitals A..P    CP-866
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // Cyrillic capitals R..YA
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // Cyrillic small letters a..p
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 176-191 -
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 192-207  - box-drawing characters
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 208-223 -
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // Cyrillic small letters r..ya
    2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0   // YO/yo, 6 Ukrainian letters, then assorted junk
#endif
};

#define char_class(c)   (char_class_table[(unsigned)(c)])


// HELPER PROCEDURES ***********************************************************************

// Structure holding the information about a single dictionary word
struct Word
{
    unsigned len;     // word length
    byte    *ptr;     // start of the word
union {
struct {              // These values are used while building and cleaning up the dictionary:
    unsigned hash;    //   hash of the word, used to find its counter in cnt[]
    unsigned hash0;   //   hash of the parent word
};
struct {              // These values are used after the dictionary cleanup:
    count_t  count;   //   number of uses of this word
    byte chr, chr2;   //   byte(s) this word will be encoded with
};};
#ifdef DEBUG
    count_t use_count;  // number of uses of this word during encoding
#endif
};

// Array of the words found in the text
Word *FirstWord;

// Address of the first free entry in FirstWord
Word *NextWord;

// Address of the end of the dictionary. In the later steps of the algorithm it marks the end of the used part of the table
Word *LastWord;

// Add the next word to the dictionary
inline void AddWord (byte *ptr, unsigned len, unsigned hash, unsigned hash0)
{
    NextWord->ptr   = ptr;
    NextWord->len   = len;
    NextWord->hash  = hash;
    NextWord->hash0 = hash0;
    NextWord++;
}

// Returns the length of the common prefix of two words
int common_prefix_length (Word *a, Word *b)
{
    int len = mymin (a->len, b->len), i;
    for (i=0; i<len && a->ptr[i] == b->ptr[i]; i++);
    return i;
}

// Comparison function for sorting words by descending frequency
int __cdecl count_desc_order (const Word *a, const Word *b)   { return b->count - a->count; }

#ifdef DEBUG
// Comparison function for sorting words by descending actual usage frequency
int __cdecl use_count_desc_order (const Word *a, const Word *b)   { return b->use_count - a->use_count; }
#endif

// Lexicographic word comparison function. A word that is a prefix of another is considered smaller
int lexicographical_order (const Word *a, const Word *b)
{
  unsigned alen = a->len, blen = b->len;
  int cmp = memcmp (a->ptr, b->ptr, mymin(alen,blen));
  return cmp? cmp : alen-blen;
}



// This byte is used as a prefix for those characters that fell victim to words
byte PREFIX_FOR_WEAK_CHARS;



// Character statistics
struct char_stats {
    byte     chr;     // the character itself
    count_t  count;   // its frequency counter
};

// Comparison function for sorting characters by ascending frequency
int char_count_asc_order (const char_stats *a, const char_stats *b)   { return a->count - b->count; }



// Recompute a word's hash when the next letter is appended: hash("ab"),'c' -> hash("abc")
inline unsigned update_hash (unsigned hash, byte c)
{
    return hash*137 + c + 219;
}

// Rehashing used when a collision is detected
inline unsigned rehash (unsigned hash, byte c)
{
    return hash + c*256 + 317;
}



// FIRST PART OF COMPRESSION: BUILDING THE DICTIONARY *******************************************

// Hash table used while building the dictionary
struct stats {
    scnt_t  count;   // frequency counter
    hash0_t hash0;   // hash of the parent word
}
*scan_hash;

// Find the slot in the hash array holding the information about the word hash with parent phash
#define SEARCH_IN_HASH(hash,phash,c,x,on_found,on_long_hash_chain)                                                       \
{                                                                                                                        \
    hash0_t h; int n=13; debug ((used_hash1[n]++, depth_cnt[p+x-p0]++));                                                 \
    debug (verbose>3 && printf( "Hash   %08x %08x %08x c=%02x %d\n", phash, hash, scan_hash[hash&mask].hash0, c, x));    \
    while ((h = scan_hash[hash&mask].hash0) != 0) {                                                                      \
        if (h == (hash0_t)phash)   goto on_found;  /* Hash slot corresponding to the word [p0..p) found */               \
        debug (used_hash1[n-1]++);                                                                                       \
        if (--n == 0)  goto on_long_hash_chain;  /* Protection against infinite loops and simply overlong hash chains */ \
        hash = rehash (hash, c);                                                                                         \
        debug (verbose>3 && printf( "Rehash %08x %08x %08x\n", phash, hash, scan_hash[hash&mask].hash0));                \
    }                                                                                                                    \
}

// Add the word [p0,p) with hash hash and parent phash to the dictionary and the hash table
#define ADDWORD(hash,phash)                                                                                        \
{                                                                                                                  \
    unsigned len = p-p0;                                                                                           \
    debug (verbose>2 && printf( "AddWord '%.*s' len=%d %08x %08x\n", len, p0, len, hash, phash));                  \
                                                                                                                   \
    /* Check for dictionary overflow, word "overflow" and zeroing of the low 16 bits of the hash */                \
    if (NextWord<LastWord && len<=MAX_WORD_LEN && (hash0_t)hash!=0) {                                              \
        unsigned h = hash&mask, ph = phash&mask;                                                                   \
        scan_hash[ph].count++;                                                                                     \
        scan_hash[h].count = 1;                                                                                    \
        scan_hash[h].hash0 = phash;                                                                                \
        AddWord (p0,len,h,ph);                                                                                     \
        debug (addword_cnt[len]++);                                                                                \
    }                                                                                                              \
}

#define WORD_STEP 4

// running times of the various phase1 variants
//         old d:1 e:4 e-h2 f-h1 f:WS8 WS4 WS2 WS1 f3:WS4 WS1 g:WS1   g4  WS1
//ghc-src  2.7 2.0 2.0 2.2  2.4    2.3 2.1 2.2 2.7    1.9 2.2   2.1   1.8 1.9
//javadoc  6.1 5.0 2.9 3.0  3.3    2.8 3.0 4.0 6.5    2.8 5.3   5.1   2.4 3.7

// First pass - building the word list via AddWord and counting the word frequencies in the scan_hash hash table.
// Along the way the byte frequencies of the input data are counted as well.
//
// The dictionary grows dynamically in lzw style: if it already contains the word "abc"
// with MIN_VISITS_TO_HAVE_SON(length) repetitions, then the word "abcd" is added to the dictionary.
// To find the longest word starting at the current position in the input stream
// and already present in the dictionary, a trie (digital search tree) is used,
// mapped onto the closed-hashing hash table scan_hash. The new hash value
// when a character is appended to a word is computed by the update_hash function.
//
// The search is speculative - instead of sequentially checking words of length 2, 3, 4
// and so on, we look for the presumed end of the word by testing the ALLOW_TO_EXTEND_WORD predicate.
// While it returns true we check only every WORD_STEP = 4th word
// (with lengths 2, 6, 10...). Only once we are sure that the word just checked is not
// in the dictionary do we start checking shorter words (for example, 2, 6, 10 - not found, 7, 8, 9;
// or another scenario: 2, 6, 9 - ! ALLOW_TO_EXTEND_WORD, 7, 8).
// Disabling the speculative search (WORD_STEP = 1) improves compression a little at the cost of speed
int phase1 (byte *buf, unsigned bufsize)
{
    // Maximum allowed number of words - 1/32 of the input data volume
    unsigned max_words = roundup_to_power_of (mymax(bufsize/32,32768), 2);
    FirstWord = (Word*) malloc (max_words * sizeof (Word));
    LastWord = FirstWord+max_words;
    NextWord = FirstWord;

    // To reduce the number of collisions the hash size is twice the maximum number of words
    unsigned scanhash_size = max_words*2, mask = scanhash_size-1;
    scan_hash = (stats*) calloc (scanhash_size, sizeof (stats));
    // Do not let words use hash entry zero, since that would make finding their children impossible
    // The same applies to hash entries whose index is a multiple of 2^16 (with 16 bits used to store the hash function value in scan_hash)
    for (int i=0; i<scanhash_size; i+= 1<<(sizeof(hash0_t)*8)) {
        scan_hash[i].hash0 = 1;
        if (sizeof(hash0_t) >= sizeof(int))    break;   // Without this check we could loop forever on entry zero ;)
    }

    byte *p = buf,                  // pointer to the next character to process
         *endbuf = buf+bufsize-WORD_STEP-1;  // end of the part of the buffer being processed

    do {
        byte *p0 = p;             // pointer to the start of the word currently being processed
        unsigned c1 = *p++;  unsigned c = *p;  // the next-to-last and last characters
        unsigned hash0;           // the hash of the word [p0,p) will be kept here
        // If not even a two-byte word can be formed at this position - hoist the sails and cast off right away :)
        if (! ALLOW_TO_EXTEND_WORD (c1, c))   {debug (badword_cnt[1]++); goto end;}
        p++;
        hash0 = (c1 << 8) + c + 16;

        // Let's find the 2-byte word
        SEARCH_IN_HASH (hash0, hash0, hash0, 0, found2, end);
        // Word not found, let's add it to the dictionary
        ADDWORD (hash0, hash0);  goto end;

    found2:
        // Find the shortest word starting at the current position
        // that is either not yet in the dictionary (we do ADDWORD)
        // or has too small a counter (we do INCWORD)
        // A small note - in the comments inside this loop, by an n-byte word I
        // mean a word of length p-p0+n. That is purely for brevity. And in general,
        // each step of this loop deals with words of length from p-p0 to p-p0+WORD_STEP-1
        do {
            unsigned hash=hash0, hash1=hash0; int i;

            // First the initial pass - we extend the word while we keep meeting decent characters :)
            // We update the hash function value, but so far make no
            // attempt to verify that these words are actually in the dictionary.
            // You may name this speculative search after me :)
            // This approach turns out to pay off, since in practice words
            // are as a rule bounded precisely by incompatible characters and not by anything else
            for (i=0; i<WORD_STEP; i++) {
                c1 = c; c = p[i];
                if (! ALLOW_TO_EXTEND_WORD (c1,c))  goto search_max;
                hash = update_hash (hash1=hash, c);
            }

            // The stars are in our favour - a word of the maximum possible length made up entirely of good and excellent letters
            debug (matrix_cnt [p-p0+i] [0] ++);
            SEARCH_IN_HASH (hash, hash1, c1, i, next_cycle, found_max);  goto search_less;
    next_cycle: // A word of the maximum length (WORD_STEP) was found - we must look for even longer words!
            { p += i;
              scnt_t *counter_p = &scan_hash[hash&mask].count;
              int counter = *counter_p;   // this word's counter
              if (counter>=MIN_VISITS_TO_HAVE_SON(p-p0))  {hash0=hash; continue;}
              if (counter>0) {*counter_p = counter+1; debug (increment_cnt[p-p0]++); break;}
              ADDWORD (hash, hash1);
              break;
            }

    search_max:  // We look for a word of length i - the maximum possible one (unsuitable characters follow)
            if (i>0)  {
                debug (matrix_cnt [p-p0+i] [0] ++);
                SEARCH_IN_HASH (hash, hash1, c1, i, found_max, found_max);
                goto search_less;
            }
    found_max:  // The word was found, but we cannot extend it because what follows are
                // unsuitable characters (or we ran into an overly long chain in the hash)
                // - all that is left is to increment its counter
            { p += i;
              scnt_t *counter_p = &scan_hash[hash&mask].count;
              int counter = *counter_p;
              if (counter<=0)  {ADDWORD (hash, hash1); break;}
              if (counter < SCNT_MAX-1)  *counter_p = counter+1;
              debug (badword_cnt[p-p0]++);
              break;
            }

    search_less: int maxi = i;  // Index of the last filled entry in the hash array
            // And now the second pass - having established the maximum theoretically possible word length above
            // (since incompatible characters follow it), but having made sure that a word that long
            // is absent from the dictionary, we go through the words from the beginning, looking for the longest one
            // that is nevertheless present in the dictionary
            unsigned h0=hash0,h1,h2,h3=hash0;
            for (i=1; i<maxi; i++) {
                byte c = *p++;
                h1 = h2 = update_hash (h0, c);
                debug (matrix_cnt [p-i+maxi-p0] [maxi-i] ++);
                SEARCH_IN_HASH (h2, h0, c, i, next, found_max);  p--; goto found;
                next: h0 = h1; h3 = h2;
            }
            h2 = hash;

    found:  // A word of length i is in the dictionary, one of length i+1 is not. 0<=i<WORD_STEP
            scnt_t *counter_p = &scan_hash[h3&mask].count;
            int counter = *counter_p;   // this word's counter
            if (counter>=MIN_VISITS_TO_HAVE_SON(p-p0)) {
                // The word found already has enough visits. We must add to the dictionary
                // a new word extended by one character compared to this one
                p++;  ADDWORD (h2, h0);
            } else {
                // This word's visit counter is still too small - just increment it
                *counter_p = counter+1;
                debug (increment_cnt[p-p0]++);
            }
            break;
        } while (p < endbuf);

    end:while (p0<p)  char_counts[*p0++]++;  // counting byte frequencies in the input data

    } while (p < endbuf);

    while (p<buf+bufsize)  char_counts[*p++]++;

    LastWord = NextWord;  // this is now the end of the dictionary

#ifdef DEBUG
    // Printing the debug statistics
    debug (verbose>1 && printf( "                 depth                                 increment addword badword\n") );
    int dc=0, ic=0, ac=0, bc=0;
    for (int n=0; n<=MAX_WORD_LEN+1; n++) {
       dc += depth_cnt[n];  ic += increment_cnt[n];  ac += addword_cnt[n];  bc += badword_cnt[n];
       for (int m=0; m<=MAX_WORD_LEN+1; m++)  mc[m] += matrix_cnt[n][m]; }
    debug (verbose>1 && printf( "Summary     : %8d %7d %7d %7d %7d %9d %7d %7d\n", dc, mc[0], mc[1], mc[2], mc[3], ic, ac, bc) );
    for (int n=0; n<=MAX_WORD_LEN+1; n++)
    debug (verbose>1 && printf( "Word len %3d: %8d %7d %7d %7d %7d %9d %7d %7d\n", n, depth_cnt[n], matrix_cnt[n][0], matrix_cnt[n][1], matrix_cnt[n][2], matrix_cnt[n][3], increment_cnt[n], addword_cnt[n], badword_cnt[n]) );
    debug (verbose>1 && printf( " Hash collisions:") );
    for (int n=13; n>=0; --n)   debug (verbose>1 && printf( " %d", used_hash1[n]) );
    debug (verbose>1 && printf( "\n") );
    debug (verbose>0 && printf( " Collected words: %d         ", LastWord-FirstWord) );
#endif

    return 0;  // All right
}


// Walk the word list first from the beginning to the end, handing the counters of single-child parents to their children.
// And then from the end to the beginning (so that derived, longer words are handled first),
// handing the counters of overly rare words to their parents. The counters are taken from the very same hash table
int phase2 (unsigned bufsize, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio)
{
    debug (int PromotedWords=0);  // Counter for printing statistics while debugging

    // Hand the counters of words having only one child over to their children.
    // Such words appear in the dictionary only because of the step-by-step procedure used to build it
    stat2 ("Hand the counters of single-child parents to their children");
    for (Word *p=FirstWord; p<LastWord; p++) {
        // Move the data about the current word into local variables
        debug (byte *ptr = p->ptr);  unsigned len = p->len, hash = p->hash, hash0 = p->hash0;
        // Counters of the current word and of its parent
        count_t cnt = scan_hash[hash].count, cnt0 = scan_hash[hash0].count;

        // If the parent word has the standard minimum frequency, indicating
        // that it has only one "child", or a negative frequency, indicating the same thing,
        // then hand the parent word's frequency over to the current one
        if (cnt0 == MIN_VISITS_TO_HAVE_SON(len-1)+1 || cnt0 < 0) {
            scan_hash[hash0].count = 0;              // exclude the parent word
            count_t sumcnt = mymin(abs(cnt) + abs(cnt0), SCNT_MAX);  // handing its frequency over to the current word
            // If the current word has only one child (which can be judged from its counter
            // being equal to the minimum value at which a child may be created, plus 1),
            // then we mark its counter with a minus sign so that its only child
            // takes this frequency over later on
            scan_hash[hash].count  =  (cnt == MIN_VISITS_TO_HAVE_SON(len)+1) ?  -sumcnt : sumcnt;
            debug (verbose>2 && printf( "Promoted '%.*s' %d -> %d (%d)\n", len, ptr, cnt, scan_hash[hash].count, cnt0));
            debug (PromotedWords++);
        }
    }
    debug (verbose>0 && printf( " Promoted words: %d\n", PromotedWords));

    // Hand the counters of bad, useless words over to their parents
    stat2 ("Hand the counters of bad children to their parents");  Word *q=LastWord;
    for (Word *p=LastWord; --p>=FirstWord;) {
        // Move the data about the current word into local variables
        debug (byte *ptr = p->ptr; unsigned len = p->len);  unsigned hash = p->hash, hash0 = p->hash0;
        // Counters of the current word and of its parent
        count_t cnt = abs(scan_hash[hash].count), cnt0 = abs(scan_hash[hash0].count);
        debug (verbose>1 && cnt >= 2000 && printf( "INTERESTING_WORD '%.*s' %d (%d)\n", len, ptr, cnt, cnt0));

        // If the current word has a sufficient number of repetitions/length
        if (GOOD_WORD (cnt, cnt0, len)) {
            p->count = cnt;   // get rid of the possible minus sign in the counter
            *--q = *p;        // and move the word into the list of those that survived the dreadful purge :)
            debug (verbose>1 && printf( "GOOD_WORD '%.*s' %d (%d)\n", len, ptr, cnt, cnt0));
        } else {
            scan_hash[hash0].count = mymin(cnt+cnt0,SCNT_MAX);  // handing its frequency over to the parent word
            debug (verbose>2 && printf( "BadWord '%.*s' %d (%d)\n", len, ptr, cnt, cnt0));
        }
    }
    FreeAndNil (scan_hash);  // The fiddler is no longer needed :)

    // Move the surviving words to the start of the FirstWord array and shrink it to keep only them
    int good_words = LastWord-q;
    memmove (FirstWord, q, good_words*sizeof(Word));
    // LastWord must be derived from the pointer realloc returns, not the one
    // passed in: realloc is free to move the block, and it does here. Setting
    // LastWord first left it dangling into the old allocation, so phase3's
    // qsort ran with an element count computed from two unrelated pointers and
    // read past the end of the array. Fatal on ARM64 (SIGBUS); on x86-64 it
    // silently produced a corrupt dictionary instead.
    Word *shrunk = (Word*) realloc (FirstWord, good_words*sizeof(Word));
    if (shrunk)  FirstWord = shrunk;      // keep the old block if realloc fails
    LastWord = FirstWord + good_words;
    debug (verbose>0 && printf( " Good words: %d                ", good_words) );

    return good_words>0? 0 : -1;  // All right if there is at least one good word
}


// Now we have a list of the most frequently used words with their frequencies, plus the frequencies of
// the individual characters in the input text, which were also collected in the first stage.
// Having sorted both lists by frequency, we easily learn which characters can be used for
// encoding words and which words will be encoded by one- and which by two-letter abbreviations.
// The *nodes value returned by the function is the number of words at the start of the FirstWord array that must
// be encoded with a single byte. LastWord-FirstWord gives the total number of encodable words
int phase3 (int MinWeakChars, int *nodes)
{
    // Sort the surviving words in order of decreasing usage frequency
    qsort( FirstWord, LastWord-FirstWord, sizeof(Word),
           (int (__cdecl *)(const void*, const void*)) count_desc_order);

    // Sort the characters in order of increasing usage frequency
    char_stats chars[UCHAR_MAX+1];
    for (int c=0; c<=UCHAR_MAX; c++) {
        chars[c].chr   = c;
        chars[c].count = char_counts[c];
        debug (verbose>1 && printf( "Freq %02x: %d\n", c, char_counts[c]) );
    }
    qsort( chars, UCHAR_MAX+1, sizeof(char_stats),
           (int (__cdecl *)(const void*, const void*)) char_count_asc_order);

    // Determine how many words can be encoded with one-byte characters
    int n=0;
    for (Word *p=FirstWord; p<LastWord && n<=UCHAR_MAX; p++, n++) {
        // We look for the first character with a large enough frequency that it makes no sense to give away for encoding words
        debug (verbose>1 && printf("Ccnt=%d, Wcnt=%d\n", chars[n].count, p->count) );
        if (chars[n].count >= p->count)  break;
        char_counts[chars[n].chr] = 0;  // make this character "conditionally free" so it can be used for encoding words
    }
    if (n<=MinWeakChars)  return -1;  // If so few characters could be freed up for encoding words, this file is most likely binary

    // Give the last of the freed characters away as the prefix for those characters whose codes were handed to words
    byte c = chars[--n].chr;
    char_counts[c] = 1;
    PREFIX_FOR_WEAK_CHARS = c;

    // Now we know how many characters can be used for encoding words
    int avail_count = n;
    debug (verbose>0 && printf( " Weak chars: %d\n", avail_count) );

    // Return the number of words that must be encoded with a single byte
    // It may be less than avail_count, since we use these codes for
    // two-byte words as well. That is not entirely correct, but from a practical point of view
    // it should not matter much
    int word_count = LastWord-FirstWord;
    *nodes = (use_plain_dictionary || word_count<=avail_count)
                ? mymin (word_count, avail_count)                   // if only one-byte words are to be used, or there is enough room for all the words found (hmm, unlikely ;)
                : mymax (avail_count - (word_count+259)/256, 0);    // if both kinds of words are used
    return 0;  // All right
}


// Assign codes to the words remaining in the dictionary
int phase4 (int nodes)
{
    Word *p = FirstWord;           // pointer to the word currently being processed
    Word *TwoByteWords = p+nodes;  // the first word that needs to be given a two-byte code

    // Sort the one- and two-byte words separately in lexicographic order
    // to improve the compression ratio of the dictionary itself
    qsort( FirstWord, nodes, sizeof(Word),
           (int (__cdecl *)(const void*, const void*)) lexicographical_order);  // NB! A patricia-qsort would be faster
    qsort( TwoByteWords, LastWord-FirstWord-nodes, sizeof(Word),
           (int (__cdecl *)(const void*, const void*)) lexicographical_order);  // NB! A patricia-qsort would be faster

    // First loop - assign one-byte codes to the most useful words
    int c;
    for( c=0; c<=UCHAR_MAX && p<TwoByteWords; c++ ) {
        if( !char_counts[c] && c!=RESERVED_CHAR ) {
            debug (verbose>1 && printf( "Word1 %02x '%.*s' %d\n", c, p->len, p->ptr, p->count));
            p->chr = (byte)c;
            p->chr2 = RESERVED_CHAR;  p++;
        }
    }

    // Second loop - assign two-byte codes to all the remaining words
    for (; c<=UCHAR_MAX && p<LastWord; c++) {
        if (char_counts[c] || c==RESERVED_CHAR)  continue;    // This character cannot be used - it occurs in the text or is reserved
        for (int c2=0; c2<=UCHAR_MAX && p<LastWord; c2++) {
            if (c2 == RESERVED_CHAR)  continue;               // This character cannot be used
            while (p<LastWord && !GOOD_2BYTE_WORD(p->len, p->count))  (*p++).count=0;   // Skip the words that were only worth encoding with one-byte codes
            if (p<LastWord) {
                debug (verbose>1 && printf( "Word2 %02x %02x '%.*s' %d\n", c, c2, p->len, p->ptr, p->count));
                p->chr = (byte)c;
                p->chr2 = (byte)c2;  p++;
            }
        }
    }
    LastWord = p;  // LastWord now points to the last encoded word

    // And now we can slap one-byte codes onto a few more words (thanks to those hundreds of very small words that turned down two-byte codes - see !GOOD_2BYTE_WORD above)
    for ( p=TwoByteWords; c<=UCHAR_MAX && p<LastWord; c++) {
        if (!char_counts[c] && c!=RESERVED_CHAR) {
            debug (verbose>1 && printf( "Word1 %02x '%.*s' %d\n", c, p->len, p->ptr, p->count));
            p->chr = (byte)c;
            p->chr2 = RESERVED_CHAR;  p++;
        }
    }
#ifdef DEBUG
    int nodes1 = p-TwoByteWords;  // number of extra one-byte words, for statistics
    int sumcnt1=0, sumcnt2=0;     // total frequency of one/two-byte words, for statistics
#endif

    // Get rid of the words that never received codes:
    //   To do so, sort the words in order of decreasing frequency
    qsort( FirstWord, LastWord-FirstWord, sizeof(Word),
           (int (__cdecl *)(const void*, const void*)) count_desc_order);
    //   And find the last word with a non-zero frequency
    for (p = FirstWord; p<LastWord && p->count; p++) {
        debug (p->chr2 == RESERVED_CHAR?  sumcnt1 += p->count : sumcnt2 += p->count);
    }
    LastWord = p;   // Now all the words from FirstWord to LastWord have codes
    debug (verbose>0 && printf( " Final words: %d = %d+%d + %d\n", LastWord-FirstWord, nodes, nodes1, LastWord-FirstWord-nodes-nodes1) );
    debug (verbose>0 && printf( " Counts: %dw1 + %dw2\n", sumcnt1, sumcnt2) );
    return 0;  // All right
}


#define put_byte(c)      (*outptr++ = (c))

#endif // FREEARC_DECOMPRESS_ONLY
#define dict2(i,j)       (dict2_var [i*(UCHAR_MAX+1) + j])
#define dict_len(i)      (dict[i]? dict[i]->len : 0)
#define dict_ptr(i)      (dict[i]->ptr)
#define dict2_len(i,j)   (dict2(i,j)? dict2(i,j)->len : 0)
#define dict2_ptr(i,j)   (dict2(i,j)->ptr)
#ifndef FREEARC_DECOMPRESS_ONLY

// Allocate memory for the output buffer and write the dictionary at its start
int phase5 (byte **outbuf, unsigned *outsize, unsigned bufsize)
{
    // If it turns out that encoding is impossible, an error code will be stored in this variable
    int retcode = 0;

    // Allocate memory for the word matrix
    Word **dict      = (Word**) calloc ( UCHAR_MAX+1,                sizeof (Word*));
    Word **dict2_var = (Word**) calloc ((UCHAR_MAX+1)*(UCHAR_MAX+1), sizeof (Word*));
    byte *char_in_use = (byte*) calloc ( UCHAR_MAX+1,                sizeof (byte));

    // Fill in the word matrix to simplify writing the dictionary to the output stream
    Word USE_DICT2_WORD[1]; USE_DICT2_WORD->len = USE_DICT2;
    for (Word *p = FirstWord; p<LastWord; p++) {
        if (p->chr2 == RESERVED_CHAR) {
            dict[p->chr] = p;               // Put the word into the first-level dictionary
        } else {
            dict2(p->chr,p->chr2) = p;      // Put the word into the second-level dictionary
            dict[p->chr] = USE_DICT2_WORD;  // Mark in the first-level dictionary that this code is used for two-byte words
            // Mark in the char_in_use array all the characters used in this word
            for (int c=0; c < p->len; c++) {
                char_in_use[ p->ptr[c] ] = 1;
            }
        }
    }
    // Find a character that does not occur in any word. It will be used
    // as the separator when encoding words
    byte word_sep;
    for (int c=UCHAR_MAX; c>=0; c--) {
        if (!char_in_use[c])  {word_sep=c; goto found;}
    }
    retcode = -1;  // All characters are used up by the words, which makes encoding the dictionary impossible
    goto done;
found:
    {
    // Allocate for the packed data the size of the input file
    // plus 200 kb - that should be enough in any case :)
    *outbuf = (byte*) malloc (bufsize+200000);
    byte *outptr = *outbuf;        // current pointer in the output buffer

    // Write the dictionary to the output stream in 5 rounds:
    //   lengths of the one-byte words, prefix lengths of the two-byte words,
    //   contents of the one-byte words, remaining contents of the two-byte words,
    //   the special character used as the prefix for encoding "stolen" characters
    // The lengths are written for all 256 possible codes (or subcodes) at once,
    //   where 0 means unused codes and 1 (USE_DICT2) means codes starting two-byte words
    for (int i=0; i<=UCHAR_MAX; i++) {
        put_byte (dict_len(i));
    }
    debug (verbose>1 && printf( "  Dict1 lenghts: %d\n", outptr-*outbuf) );
    Word *prev_word = NULL;
    for (int i=0; i<=UCHAR_MAX; i++) {
        if (dict[i] == USE_DICT2_WORD) {
            for (int j=0; j<=UCHAR_MAX; j++) {
                unsigned n = 0;
                if (dict2(i,j) && prev_word) {
                    n = common_prefix_length (dict2(i,j), prev_word);
                }
                put_byte (n);
                prev_word = dict2(i,j);
            }
        }
    }
    debug (verbose>1 && printf( "  Dict2 prefixes: %d\n", outptr-*outbuf) );
    for (int i=0; i<=UCHAR_MAX; i++) {
        if (dict[i] == USE_DICT2_WORD)  continue;
        for (int c=0; c < dict_len(i); c++) {
            put_byte (dict_ptr(i)[c]);
        }
    }
    debug (verbose>1 && printf( "  Dict1 strings: %d\n", outptr-*outbuf) );
    prev_word = NULL;
    put_byte (word_sep);  // Tell the decoder the word separator
    for (int i=0; i<=UCHAR_MAX; i++) {
        if (dict[i] == USE_DICT2_WORD) {
            for (int j=0; j<=UCHAR_MAX; j++) {
                int n = 0;
                if (dict2(i,j) && prev_word) {
                    n = common_prefix_length (dict2(i,j), prev_word);
                }
                // Write the word's text minus the prefix that is to be copied from the previous word
                for (int c=n; c < dict2_len(i,j); c++) {
                    put_byte (dict2_ptr(i,j)[c]);
                }
                put_byte (word_sep);  // Tell the decoder that the current word has ended
                prev_word = dict2(i,j);
            }
        }
    }
    debug (verbose>1 && printf( "  Dict2 strings: %d\n", outptr-*outbuf) );

    // Write the code of the character used as the prefix for homeless characters
    put_byte (PREFIX_FOR_WEAK_CHARS);
    // Statistics - the total size of the dictionary
    debug (verbose==1 && printf( " Dictionary: %d bytes  ", outptr-*outbuf) );

    // Return the length of the encoded dictionary
    *outsize = outptr - *outbuf;
    }
    // Free the internal arrays and exit with the code of a possible error
done:
    FreeAndNil (dict);
    FreeAndNil (dict2_var);
    FreeAndNil (char_in_use);
    return retcode;
}



// COMPRESSION: ENCODING WITH THE DICTIONARY BUILT ABOVE ******************************************

// Max. length of a word that can be stored directly inside the CodeWord structure
#define DIRECT_CHARS 12

// Structure holding the information about a word during encoding
struct CodeWord
{
union {
    byte str[DIRECT_CHARS]; // contents of the word if its length is <= 12
    byte *ptr;              // contents of the word if its length is > 12
};
    byte len;               // word length
    byte chr, chr2;         // byte(s) this word is encoded with
#ifdef DEBUG
    count_t count;          // number of uses of this word while parsing the text
    count_t use_count;      // number of uses of this word during encoding
    Word   *orig_word;      // reference to the original word, needed to count its total number of uses
#endif
};

unsigned  hashsize;
unsigned  hashmask;
CodeWord *codewords_hash;
ushort   *hashbits;
byte     *words_text;

// We build a small hash for all the words in use and their partial prefixes.
// This hash must let us scan the text up to the end of a word and determine its code.
// Since the number of words is normally <10k and their total length is <100k, this hash should
// fit entirely into the CPU cache (the size of the hash accessed for every byte of the
// input text = hashsize*2 = the number of unique bytes in the dictionary*8, rounded up to a power of two;
// the size of the hash accessed once per word = hashsize*16).
// Typically that is 128-512 kb and 1-4 mb respectively; and only 100-200 kb of the second table is actually used.
// Plus another 10-50 kb for storing the text of overly long words (with length >12)
// Thus encoding should get by with 256-1024 kb of cache, depending on the size of the dictionary
int phase6 ()
{
    // Sort the words in lexicographic order to ensure that references
    // to parent words get created (say, if the dictionary contains words of 5 and 8 spaces,
    // then the word hash indices corresponding to strings of 6 and 7 spaces will also contain
    // the information for encoding the 5-space word. Thus, for an input string of, for example,
    // 7 spaces and a tab, the last entry of our hash chain (the one corresponding to
    // 7 spaces) will hold a reference to the representation of the 5-space word)
    qsort( FirstWord, LastWord-FirstWord, sizeof(Word),
           (int (__cdecl *)(const void*, const void*)) lexicographical_order);  // NB! A patricia-qsort would be faster

    // Statistics: print the list of words in lexicographic order
    for (Word *p = FirstWord; p<LastWord; p++) {
        debug (verbose>3 && printf( "SortedWord '%.*s' %d\n", p->len, p->ptr, p->count));
    }

    // Count the number of unique bytes in the words. This value determines the total number of entries
    // that will be created in the hash table. We create a hash table at least four times
    // larger to keep lookup performance high (so that there are <20% collisions)
    unsigned unique_bytes = 0, words_len = 0;
    for (Word *p = FirstWord; p<LastWord; p++) {
        unique_bytes +=  p->len - (p==FirstWord? 0 : common_prefix_length (p, p-1));
        if (p->len>DIRECT_CHARS)  words_len += p->len;
    }
    hashsize = roundup_to_power_of (unique_bytes*4, 2);
    hashmask = hashsize-1;
    hashbits = (ushort*) calloc (hashsize, sizeof(ushort));
    codewords_hash = (CodeWord*) calloc (hashsize, sizeof(CodeWord));

    // This buffer will be used to store the text of long words
    // (the text of short words is placed directly into the CodeWord entry)
    words_text = (byte*) malloc (words_len);
    byte *wordsptr = words_text;

    // We fill the codewords_hash hash table with words
    // and build the hash chains in hashbits for letter-by-letter lookup of those words
    for (Word *p = FirstWord; p<LastWord; p++) {
        unsigned hash = hashsize + p->ptr[0];
        CodeWord *longest_word_so_far = NULL;
        for (int i=1; i < p->len; i++) {
            byte c = p->ptr[i];
            unsigned hash0 = hash;
            hash = update_hash (hash, c);  int n=13;
            while (hashbits [hash & hashmask]  &&  hashbits[hash&hashmask] != (ushort)hash0) {
                if (--n == 0)  goto NextWord;  // Protection against infinite loops
                hash = rehash (hash, c);
            }
            hashbits [hash & hashmask] = hash0;

            // Propagating the prefix words of the current one along its hash chain
            CodeWord *newp = &codewords_hash [hash & hashmask];
            if (newp->len) {
                longest_word_so_far = newp;     // Remember the longest word so far that is a prefix of the current one
            } else if (longest_word_so_far) {
                *newp = *longest_word_so_far;   // The longest word that can be encoded in this situation
                debug (newp->count = -newp->count);
            }
        }
        {   // Put the word into the word hash table
            CodeWord *newp = &codewords_hash [hash & hashmask];
            newp->len  = p->len;
            newp->chr  = p->chr;
            newp->chr2 = p->chr2;
            // The word's text is either stored entirely in this entry (if it does not exceed 12 bytes)
            // or moved into words_text, to which we keep a reference
            if (p->len <= DIRECT_CHARS)  memcpy (newp->str, p->ptr, p->len);
            else newp->ptr = wordsptr, memcpy (wordsptr, p->ptr, p->len), wordsptr += p->len;
            debug (newp->count = p->count);
            debug (newp->orig_word = p);
            debug (verbose>2 && printf( "RecordWord hash=%d len=%d '%.*s'\n", hash & hashmask, p->len, p->len, p->ptr));
        }
NextWord: ;
    }
    return 0;  // All right
}


// Find in the dictionary the longest word that the text pointed to by p0 starts with
// NB! The FindWord algorithm must exactly match the dictionary hashing algorithm in phase6,
// otherwise we risk failing to find some of the words
inline CodeWord *FindWord (byte *p0, byte *endbuf)
{
    debug (verbose>3 && printf( "FindWord '%.50s'\n", p0));
    debug (FindWord_calls++);

    // Starting from a two-byte word of the input text, we grow its size byte by byte,
    // checking that the word is still marked in the hash as belonging to the dictionary.
    // When this procedure ends, hash0 holds the hash of the longest word of the input text
    // present in the dictionary (if the dictionary contains 5- and 8-space words while the input data
    // contains only 7 spaces, then hash0 will be the hash of 7 spaces and at that address in code_words
    // there will be the 5-space word - believe it or not)
    byte *p = p0;
    unsigned hash0 = hashsize + *p++;
    do {
        // Compute the hash of the word [p0,p)
        byte c = *p++;
        unsigned hash = update_hash (hash0, c);

        // The rehashing loop. h=0 means the slot in the hash table is empty,
        // i.e. the word [p0..p) is not in the dictionary. h!=hash0 means a hash collision - the slot is not empty,
        // but it is not occupied by us either. The search must continue with a secondary rehash
        ushort h; int n=13;  debug (used_hash2[n]++);
        while ((h = hashbits [hash & hashmask]) != 0) {
            if (h == (ushort)hash0)   goto found;  // Hash slot corresponding to the word [p0..p) found
            hash = rehash (hash, c);
            debug (used_hash2[n-1]++);
            if (--n == 0)  break;  // Protection against infinite loops and simply overlong hash chains
        }
        // We end up here if the word is not in the dictionary (or, very rarely,
        // when the hash chain is too long). A fine moment to leave the loop :)
        break;

found:  hash0 = hash;    // The word was found, we move on to looking for a word one character longer
    } while (p<endbuf);  // But it is still better not to run past the end of the buffer :D

    p--; // No word ending at p was found, so now we check the word that is one byte shorter
    CodeWord *word = &codewords_hash [hash0 & hashmask];
    int len = word->len;
    if (len==0 || len>endbuf-p)  return NULL;
    byte *ptr = len>DIRECT_CHARS? word->ptr : word->str;
    if (memcmp (p0, ptr, len) != 0)   return NULL;
    return word;
}

#define put_Byte(c)      (*outptr++ = (c))
#define put_Word(word)   (put_Byte (word->chr),  \
                         (word->chr2 != RESERVED_CHAR)  &&  put_Byte (word->chr2))

// We encode the text using the hash built above
int phase7 (byte *buf, unsigned bufsize, byte *outbuf, unsigned *outsize)
{
    byte *p = buf,                // current pointer in the input buffer
         *endbuf = buf+bufsize,   // end of the input buffer
         *outptr = outbuf;        // current pointer in the output buffer
    do {
        // Find in the dictionary the longest word starting at the current position
        CodeWord *word = FindWord (p,endbuf);
        // And once the right word has been found..
        if (word) {
            put_Word (word);   // Write 1 or 2 bytes - the code of the word found
            p += word->len;    // Skip the word in the input text
            debug (OutWord++);
            debug (word->use_count++);
            debug (word->orig_word->use_count++);
            debug (verbose>3 && printf( "OutWord %02x %02x '%.*s' %d\n", word->chr, word->chr2, word->len, word->len>DIRECT_CHARS? word->ptr : word->str, word->count));
        } else {
            // No word found - copy a single character from the input stream to the output
            byte c = *p++;
            if (! char_counts[c] || c==PREFIX_FOR_WEAK_CHARS) {
                // This character was out of luck - it gave its code away to words
                // and now, poor thing, has to identify itself with a special prefix
                put_Byte (PREFIX_FOR_WEAK_CHARS);
                debug (OutByte2++);
            }
            put_Byte (c);
            debug (OutByte++);
            debug (verbose>3 && printf( "OutByte %02x '%c'\n", c, c));
        }
    } while (p < endbuf);  // Repeat until the end of the input buffer

#ifdef DEBUG
    // Debug statistics: word usage frequency during encoding, in decreasing order
    debug (qsort( FirstWord, LastWord-FirstWord, sizeof(Word),
                  (int (__cdecl *)(const void*, const void*)) use_count_desc_order));
    int short_count=0, long_count=0;
    for (Word *p = FirstWord; p<LastWord; p++) {
        if (p->chr2 == RESERVED_CHAR)
            debug (verbose>1 && printf( "UsedWord1 %02x '%.*s' %d => %d\n", p->chr, p->len, p->ptr, p->count, p->use_count));
        else
            debug (verbose>1 && printf( "UsedWord2 %02x %02x '%.*s' %d => %d\n", p->chr, p->chr2, p->len, p->ptr, p->count, p->use_count));
        debug ((p->len > DIRECT_CHARS ?  long_count:short_count) += p->use_count);
    }
    debug (verbose>0 && printf( "    Usage: %ds+%dl\n", short_count, long_count) );
    debug (verbose>0 && printf( " Data: %d bytes = %d+%dc + %dw1 + %dw2\n", outptr-outbuf, OutByte, OutByte2, OutWord-(outptr-outbuf-FindWord_calls-OutByte2), outptr-outbuf-FindWord_calls-OutByte2) );
    debug (verbose>1 && printf( " Hash collisions (%d):", FindWord_calls) );
    for (int n=13; n>=0; --n)   debug (verbose>1 && printf( " %d", used_hash2[n]) );
#endif

    FreeAndNil (FirstWord);
    FreeAndNil (hashbits);
    FreeAndNil (codewords_hash);
    FreeAndNil (words_text);

    // Return the length of the encoded text
    *outsize = outptr - outbuf;
    return 0;  // All right
}


// Call the given function and, if it returned an error code, leave DictEncode() after freeing all memory
#define check(call)  { int code = call;                   \
                       if (code) {                        \
                           FreeAndNil (scan_hash);        \
                           FreeAndNil (FirstWord);        \
                           FreeAndNil (hashbits);         \
                           FreeAndNil (codewords_hash);   \
                           FreeAndNil (words_text);       \
                           FreeAndNil (*outbuf);          \
                           return code;                   \
                       }                                  \
                     }                                    \

// Compress the input data buf[bufsize] and return the address of the output buffer and the size of the packed data
int DictEncode (byte *buf, unsigned bufsize, byte **outbuf, unsigned *outsize, int MinWeakChars, int MinLargeCnt, int MinMediumCnt, int MinSmallCnt, int MinRatio)
{
    unsigned dictlen, datalen; int nodes; *outbuf = NULL;
    stat1 ("1. Build the dictionary, find the frequencies of words and individual characters");
    check (phase1 (buf, bufsize));
    stat1 ("2. Remove the overly rare words from the dictionary");
    check (phase2 (bufsize,MinLargeCnt,MinMediumCnt,MinSmallCnt,MinRatio));
    stat1 ("3. Determine the bytes that can be used for encoding words");
    check (phase3 (MinWeakChars, &nodes));
    stat1 ("4. Assign codes to the words remaining in the dictionary");
    check (phase4 (nodes));
    stat1 ("5. Write the dictionary to the file");
    check (phase5 (outbuf, &dictlen, bufsize));  // The output buffer is created right here
    stat1 ("6. Create a hash for fast lookup among the remaining words");
    check (phase6 ());
    stat1 ("7. Encode the text");
    check (phase7 (buf, bufsize, *outbuf+dictlen, &datalen));
    // Return the size and the address of the output buffer
    *outsize = dictlen + datalen;
    *outbuf  = (byte*) realloc (*outbuf, *outsize);
    return 0;  // All right
}

#endif // FREEARC_DECOMPRESS_ONLY


// DECOMPRESSION **************************************************************************

// Matrix dictionary used for decoding
struct dict_entry
{
    unsigned len;     // word length
    byte *ptr;        // start of the word
}
dict[UCHAR_MAX+1];

#define get_byte()       (*ptr++)
#define put_byte(c)      (*outptr++ = (c))
#define put_word(p,len)  (memcpy (outptr, (p), (len)), outptr += (len))

// Decompress the input data buf[bufsize] into outbuf and return the size of the decompressed data
int DictDecode (byte *buf, unsigned bufsize, byte *outbuf, unsigned *outsize)
{
    int retcode = 0;
    byte *ptr = buf,
         *end = buf+bufsize,
         *outptr = outbuf;     // current pointer in the output buffer

    // Matrix dictionary used for decoding 2-byte words
    dict_entry *dict2_var = (dict_entry*) calloc ((UCHAR_MAX+1)*(UCHAR_MAX+1), sizeof (dict_entry));

    stat1 ("READING THE DICTIONARY");
    // Consists of 5 loops:
    //   1. Read 256 bytes - these are the lengths of all the words encoded with a single byte
    //        (0 means this byte encodes no word, 1 means that the codes of 256 words start with this byte)
    //   2. Read the lengths of all the words encoded with two bytes
    //        (256*n bytes, where n is the number of ones read in the previous stage)
    //   3. Read the text of all the one-byte words
    //   4. Read the text of all the two-byte words
    //   5. Create pseudo-words for decoding those characters that gave their codes away to words
    int dictsize = 0, words2 = 0;
    for( int i=0; i<=UCHAR_MAX; i++ ) {
        dictsize += dict[i].len = get_byte();
    }
    for( int i=0; i<=UCHAR_MAX; i++ ) {
        if( dict[i].len==USE_DICT2 ) {
            for( int j=0; j<=UCHAR_MAX; j++ ) {
                dictsize += dict2(i,j).len = get_byte();
                words2++;
            }
        }
    }
    // Buffer for storing the text of the words (memory for it is currently allocated by guesswork, but with a big margin :)
    byte *words = (byte*) malloc (dictsize+UCHAR_MAX+1+words2*20+100000), *wordptr = words;
    for( int i=0; i<=UCHAR_MAX; i++ ) {
        if (dict[i].len == USE_DICT2)  continue;
        dict[i].ptr = wordptr;
        for( int k=0; k<dict[i].len; k++ ) {
            *wordptr++ = get_byte();
        }
    }
    {
    byte word_sep = get_byte();
    byte *prevptr = NULL;
    for( int i=0; i<=UCHAR_MAX; i++ ) {
        if( dict[i].len==USE_DICT2 ) {
            for( int j=0; j<=UCHAR_MAX; j++ ) {
                dict2(i,j).ptr = wordptr;
                // Copy the start of the word from the previous one
                for( int k=0; k<dict2(i,j).len; k++ ) {
                    if (prevptr==NULL)  {retcode = -1; goto done;}  // Error in the input data - copying data from a previous word that does not exist :)
                    *wordptr++ = *prevptr++;
                }
                // And read the rest of the word from the input stream
                for(;;) {
                    byte c = get_byte();
                    if (c==word_sep) break;
                    *wordptr++ = c;
                }
                dict2(i,j).len = wordptr - dict2(i,j).ptr;
                prevptr = dict2(i,j).ptr;
            }
        }
    }
    // The prefix used for encoding the stolen characters
    byte prefix = get_byte();
    dict[prefix].len = USE_DICT2;
    // Create the pseudo-words that encode the stolen characters
    for (int j=0; j<=UCHAR_MAX; j++) {
        dict2(prefix,j).len = 1;
        dict2(prefix,j).ptr = wordptr;
        *wordptr++ = (byte)j;
    }


    stat1 ("DECODE THE TEXT USING THE DICTIONARY READ ABOVE");
    while( ptr<end ) {
        byte c = get_byte();
        dict_entry &d = dict[c];

        // If this byte encodes no word, then output it as is
        if (d.len == 0) {
            put_byte(c);

        // If this byte is the start of a two-byte word code, then output that word
        } else if( d.len == USE_DICT2 ) {
            byte c2 = get_byte();
            dict_entry &d = dict2(c,c2);
            put_word (d.ptr, d.len);

        // Otherwise this byte is the start of a one-byte word code
        } else {
            put_word (d.ptr, d.len);
        }
    }
    }
done:
    FreeAndNil (words);
    FreeAndNil (dict2_var);

    // Store the length of the decoded text and return the (un)successful completion code
    *outsize = outptr-outbuf;
    return retcode;
}



#ifndef DICT_LIBRARY
// HOUSEKEEPING ********************************************************************
#include <windows.h>
#include <io.h>
#include "../Common.cpp"

// Print the execution time of the current step of the algorithm and remember the name of the next step
void stat1 (char *nextmsg)
{
    if (! print_timings) return;
    stat2 (NULL);

    static char *msg = NULL;
    static LARGE_INTEGER Frequency, PerformanceCount0, PerformanceCountStart, PerformanceCountEnd;

    if (msg==NULL) {
        QueryPerformanceFrequency (&Frequency);
        QueryPerformanceCounter (&PerformanceCount0);
    } else {
        QueryPerformanceCounter (&PerformanceCountEnd);
        double seconds = double(PerformanceCountEnd.QuadPart - PerformanceCountStart.QuadPart)/Frequency.QuadPart;
        printf( "%s: %lf seconds\n", msg, seconds);
        if (nextmsg==NULL) {
            double seconds = double(PerformanceCountEnd.QuadPart - PerformanceCount0.QuadPart)/Frequency.QuadPart;
            printf( "Total %lf seconds\n", seconds);
        }
    }

    msg = nextmsg;
    QueryPerformanceCounter (&PerformanceCountStart);
}

// Same as stat1(), but for measuring the execution time of the algorithm's substeps
void stat2 (char *nextmsg)
{
    if (! print_timings) return;

    static char *msg = NULL;
    static LARGE_INTEGER Frequency, PerformanceCountStart, PerformanceCountEnd;

    if (msg==NULL) {
        QueryPerformanceFrequency (&Frequency);
    } else {
        QueryPerformanceCounter (&PerformanceCountEnd);
        double seconds = double(PerformanceCountEnd.QuadPart - PerformanceCountStart.QuadPart)/Frequency.QuadPart;
        printf( "%s: %lf seconds\n", msg, seconds);
    }

    msg = nextmsg;
    QueryPerformanceCounter (&PerformanceCountStart);
}


// Command-line parsing, reading the input data, calling DictEncode/DictDecode, and writing the output data
int main (int argc, char **argv)
{
    // Decompression instead of compression?
    int unpack = 0;

    // Word selection parameters used by default
    int MinLargeCnt  = 2048;    // Minimum "large" counter
    int MinMediumCnt = 100;     // Minimum "medium" counter
    int MinSmallCnt  = 50;      // Minimum "small" counter
    int MinRatio     = 4;       // Minimum "ratio"

    if( argv[1] && argv[1][0] == '-' ) {
        char *p = argv[1]+1;
        while( *p ) {
            switch( tolower(*p) ) {
                case 'p':   MinLargeCnt=8192; MinMediumCnt=400; MinSmallCnt=100; MinRatio=4; break;
                case 'f':   MinLargeCnt=2048; MinMediumCnt=100; MinSmallCnt= 50; MinRatio=0; break;
                case '1':   use_plain_dictionary++;   break;
                case 'v':   verbose++;                break;
                case 't':   print_timings++;          break;
                case 'd':   unpack++;                 break;
                default :   printf( "\n Unknown option '%c'\n", *p);
                            exit(1);
            }
            p++;
        }
        argv++, argc--;
    }
    if (argc != 2  &&  argc != 3) {
        printf( "\n Usage: dict [-fp1vvvt] original-file [packed-file]");
        printf( "\n   -p  --  ppmd/ppmonstr-optimized compression");
        printf( "\n   -f  --  fast&dirty compression");
        printf( "\n   -1  --  use plain dictionary (only 1-byte codes)");
#ifdef DEBUG
        printf( "\n   -v  --  increment verbosity level (0 - default, 4 - maximum)");
#else
        printf( "\n   -v  --  verbosity level (you should recompile program with -DDEBUG to enable this option)");
#endif
        printf( "\n   -t  --  print operation timings");
        printf( "\n" );
        printf( "\n For decompress: dict -d[vvvt] packed-file [unpacked-file]");
        printf( "\n" );
        exit(1);
    }
    FILE *fin = fopen( argv[1], "rb" );
    if (fin == NULL) {
        printf( "\n Can't open %s for read\n", argv[1]);
        exit(2);
    }

    // The buffer where the input data will be placed, and its size
    unsigned bufsize = filelength(fileno(fin));
    byte *buf = (byte*) malloc(bufsize);
    if (buf == NULL) {
        printf( "\n Can't alloc %u bytes\n", bufsize);
        exit(4);
    }

    // Read the input data
    unsigned bytes = file_read (fin, buf, bufsize);
    if (bytes != bufsize) {
        printf( "\n Can't read entire input file");
        exit(5);
    }
    debug (verbose>0 && printf( " Bytes read: %u\n", bufsize) );

    byte *outbuf; unsigned outsize; int ret;
    if (!unpack) {
        // Perform the encoding and obtain the address of the output buffer and the size of the output data
        ret = DictEncode (buf, bufsize, &outbuf, &outsize, 0, MinLargeCnt, MinMediumCnt, MinSmallCnt, MinRatio);
    } else {
        // Read the size of the original data from the start of the input buffer
        // and use it to allocate memory for the output buffer
        outsize = *(unsigned*)buf;
        buf += sizeof(unsigned); bufsize -= sizeof(unsigned);
        outbuf = (byte*) malloc (outsize);
        // Perform the decoding and obtain the size of the output data
        ret = DictDecode (buf, bufsize, outbuf, &outsize);
    }

    // Print the last line of statistics
    stat1 (NULL);

    // Write the output data if everything is OK and an output file was specified
    if (!ret  &&  argc == 3) {
        FILE *fout = fopen( argv[2], "wb" );
        if (fout == NULL) {
            printf( "\n Can't open %s for write\n", argv[2]);
            exit(3);
        }
        if (!unpack)  file_write (fout, &bufsize, sizeof(bufsize));  // Prepend the size of the original file to the encoded data
        file_write (fout, outbuf, outsize);
    }

    return 0;
}

#endif

