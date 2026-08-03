// Handling of archives created by FreeArc:
//   reading and decoding the Footer block and the directory blocks


#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "../Environment.h"
#include "../Compression/Compression.h"

#define aSIGNATURE make4byte(65,114,67,1)  /* Signature of FreeArc archives: ArC */
#define MAX_FOOTER_DESCRIPTOR_SIZE 4096    /* Maximum size of an archive block descriptor */

/******************************************************************************
** Class implementing arrays that know their own size :) **********************
******************************************************************************/
#ifdef __cplusplus
template <typename T> class ARRAY
{
public:
  int size;                         // Number of elements in the array
  T  *data;                         // Data stored in the array
  bool autodelete;                  // Automatically delete data when the array itself is deleted

  void setsize (int _size)          {size = _size; data = size? new T[size] : NULL; autodelete=TRUE;}
  // delete[], not delete: setsize allocates with new T[]. Mismatching the two
  // is undefined behaviour even for a trivially-destructible T, and every
  // DIRECTORY_BLOCK does it several times -- ASan aborts on the first one.
  void resize (int _size)           {if(autodelete) delete[] data; setsize(_size);}   // Change the length of an already existing array
  void set (int _size, void* ptr)   {resize(0); size=_size, data=(T*)ptr, autodelete=FALSE;}  // Use the given chunk of memory as the array contents
  ARRAY (int _size=0)               {setsize (_size);}       // Create an array of length _size
  ~ARRAY()                          {resize(0);}
  T& operator[] (int i)             {return data[i];}
  T& operator() (int i)             {return data[i];}
};
#endif  // __cplusplus


/******************************************************************************
** Class abstracting work with files ******************************************
******************************************************************************/
#ifdef __cplusplus

enum MODE {READ_MODE, WRITE_MODE}; // file opening mode
class MYFILE
{
public:
  int handle;
  TCHAR *filename;
  char *utf8name, *utf8lastname, *oemname;

  void SetBaseDir (char *utf8dir)    // Set base dir
  {
    strcpy (utf8name, utf8dir);
    if (utf8name[0] != '\0')  strcat (utf8name, STR_PATH_DELIMITER);
    utf8lastname = strchr(utf8name, 0);
  }

#ifdef FREEARC_WIN
#  ifdef FREEARC_GUI                 // Win32 GUI *****************************************
  void setname (FILENAME _filename)  {strcpy (utf8lastname, _filename);
                                      utf8_to_utf16 (utf8name, filename);}
  CFILENAME displayname (void)       {return filename;}

#  else                              // Win32 console *************************************
  void setname (FILENAME _filename)  {strcpy (utf8lastname, _filename);
                                      utf8_to_utf16 (utf8name, filename);
                                      CharToOemW (filename, oemname);}
  FILENAME displayname (void)        {return oemname;}
#  endif

#else                                // Linux *********************************************
  void setname (FILENAME _filename)  {strcpy (utf8lastname, _filename);  filename = utf8name;}
  FILENAME displayname (void)        {return utf8name;}

#endif                               // END ***********************************************

  void init()                             {handle=-1;
#ifdef FREEARC_WIN
                                           filename = (TCHAR*) malloc (MY_FILENAME_MAX*4);
#  endif
                                           oemname  = (char*)  malloc (MY_FILENAME_MAX);
                                           utf8name = (char*)  malloc (MY_FILENAME_MAX*4);
                                           *utf8name=0; utf8lastname=utf8name;}

  MYFILE ()                               {init();}
  MYFILE (FILENAME filename)              {init(); setname (filename);}
  MYFILE (FILENAME filename, MODE mode)   {init(); open (filename, mode);}
  ~MYFILE()                               {if (isopen()) close();
                                           if ((char*)filename!=utf8name)  free(filename);
                                           free(oemname); free(utf8name);}
  bool   exists (void)                    {return file_exists(filename);}

  MYFILE& open (FILENAME _filename, MODE mode)    // Opens the file for reading or writing
  {
    setname (_filename);
    return open (mode);
  }
  MYFILE& open (MODE mode)    // Opens the file for reading or writing
  {
    if (mode==WRITE_MODE)  BuildPathTo (filename);
#ifdef FREEARC_WIN
    handle = ::_wopen (filename, mode==READ_MODE? O_RDONLY|O_BINARY : O_WRONLY|O_BINARY|O_CREAT|O_TRUNC, S_IREAD|S_IWRITE);
#else
    handle =   ::open (filename, mode==READ_MODE? O_RDONLY : O_WRONLY|O_CREAT|O_TRUNC, S_IREAD|S_IWRITE);
#endif
    CHECK (handle>=0, (s,"ERROR: can't open file %s", utf8name));
    return *this;
  }
  void SetFileDateTime (time_t mtime)   {::SetFileDateTime (filename, mtime);}   // Sets the file mtime
  void close()    // Closes the file
  {
    CHECK (::close(handle)==0, (s,"ERROR: can't close file %s", utf8name));
    handle = -1;
  }
  bool isopen()  {return handle>=0;}

#ifdef FREEARC_WIN
  FILESIZE size    ()                {return _filelengthi64 (handle);}            // Returns the file size
  FILESIZE curpos  ()                {return _lseeki64 (handle, 0,   SEEK_CUR);}  // Current position in the file
  void     seek    (FILESIZE pos)    {CHECK( _lseeki64 (handle, pos, SEEK_SET) == pos, (s,"ERROR: file seek operation failed"));}       // Moves to the given position in the file
#else
  FILESIZE size    ()                {return myfilelength (handle);}
  FILESIZE curpos  ()                {return lseek (handle, 0,   SEEK_CUR);}
  void     seek    (FILESIZE pos)    {CHECK( lseek (handle, pos, SEEK_SET) == pos, (s,"ERROR: file seek operation failed"));}
#endif

  FILESIZE tryRead (void *buf, FILESIZE size)   {int result = ::read (handle, buf, size); CHECK(result>=0, (s,"ERROR: file read operation failed")); return result;}           // Returns the number of bytes read, which may be less than requested
  void     read    (void *buf, FILESIZE size)   {CHECK (tryRead (buf, size) == size, (s,"ERROR: can't read %lu bytes", (unsigned long)size));}         // Raises an exception if the given number of bytes could not be read
  void     write   (void *buf, FILESIZE size)   {CHECK (::write (handle, buf, size) == size, (s,"ERROR: file write operation failed"));}
};

#endif  // __cplusplus


/******************************************************************************
** Synonyms for the simple types used in the program **************************
******************************************************************************/
typedef time_t   XFILETIME;        // file date/time
typedef int      BOOL;             // boolean type
typedef uint32   CRC;              // file CRC
typedef char*    COMPRESSOR;       // compression method
typedef int      BLOCKTYPE;        // archive block type:
enum {DESCR_BLOCK=0, HEADER_BLOCK, DATA_BLOCK, DIR_BLOCK, FOOTER_BLOCK, RECOVERY_BLOCK};

struct BLOCK                       // information about an archive block
{
  BLOCKTYPE  type;
  COMPRESSOR compressor;
  FILESIZE   pos;
  FILESIZE   origsize;
  FILESIZE   compsize;
  CRC        crc;
};

struct BLOCK_DESCRIPTOR : BLOCK {};// archive block descriptor


/******************************************************************************
** Reading a data stream ******************************************************
******************************************************************************/
class MEMORY_BUFFER
{
public:
    char *buf;         // address of the buffer start, needed to free the memory
    char *bufend;      // address just past the buffer, used to check for overrunning it
    char *p;           // current read pointer

    MEMORY_BUFFER () {buf = NULL;}
    ~MEMORY_BUFFER() {free (buf);}

    // Use the buffer to read data from file `file` at position `pos` of length `len`
    MEMORY_BUFFER& open (MYFILE &file, FILESIZE pos, FILESIZE size)
    {
      free (buf);                      // Free the previously used buffer
      buf = (char*) malloc (size+8);   // We allocate 8 extra bytes so that integers can be decoded quickly without fear of running past the buffer boundary
      CHECK (buf, (s,"ERROR: can't alloc %lu memory bytes", (unsigned long)(size+8)));
      file.seek (pos);
      file.read (buf, size);
      p=buf, bufend=p+size;
      return *this;
    }

    // Read the data from the file into the buffer, decompress it and check its CRC
    MEMORY_BUFFER& openCompressedCheckCRC (COMPRESSOR compressor, FILESIZE origsize, MYFILE &file, FILESIZE pos, FILESIZE compsize, CRC right_crc)
    {
      open (file, pos, compsize);
      char *origbuf = (char*) malloc (origsize+8);  // 8 extra bytes as a safety margin when performing readInteger
      int result = DecompressMem (compressor, buf, compsize, origbuf, origsize);
      CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, (s,"ERROR: unsupported compression method \"%s\"", compressor));
      CHECK (result==origsize, (s,"ERROR: archive structure corrupted (decompression of control block failed)"));
      free(buf), p=buf=origbuf, bufend=buf+origsize;
      CRC crc = CalcCRC (buf, origsize);
      CHECK (crc==right_crc, (s,"ERROR: archive structure corrupted (control block failed CRC check)"))
      return *this;
    }

    // Read the data from the file into the buffer and check that its CRC matches the value stored in the last bytes of that data
    MEMORY_BUFFER& openWithCRCAtEnd (MYFILE &file, FILESIZE pos, FILESIZE size)
    {
      open (file, pos, size);
      bufend -= sizeof(CRC);
      CRC right_crc = *(CRC*)bufend;
      CRC crc = CalcCRC (buf, size-sizeof(CRC));
      CHECK (crc==right_crc, (s,"ERROR: archive structure corrupted (descriptor failed CRC check)"))
      return *this;
    }


    // Has the end of the buffer been reached?
    bool eof ()         {return p>=bufend;}
    // Advance the read pointer n bytes forward and check that we have not run past the end of the buffer :)
    void skip (int n)   {p+=n; CHECK(p<=bufend, (s,"ERROR: archive structure corrupted (bad data)"));}

    // Read an integer in the variable-length format
    uint64 readInteger()
    {
      uint32 x = *(uint32*)p;
           if ((x&  1)==  0)  {skip(1); return (x & ((1u<< 8)-1))>>1;}
      else if ((x&  3)==  1)  {skip(2); return (x & ((1u<<16)-1))>>2;}
      else if ((x&  7)==  3)  {skip(3); return (x & ((1u<<24)-1))>>3;}
      else if ((x& 15)==  7)  {skip(4); return (x               )>>4;}
      uint64 y = *(uint64*)p;
           if ((x& 31)== 15)  {skip(5); return (y & ((uint64(1)<<40)-1))>>5;}
      else if ((x& 63)== 31)  {skip(6); return (y & ((uint64(1)<<48)-1))>>6;}
      else if ((x&127)== 63)  {skip(7); return (y & ((uint64(1)<<56)-1))>>7;}
      else if ((x&255)==127)  {skip(8); return (y                      )>>8;}
      else                    {skip(1); uint64 y = *(uint64*)p; skip(8); return y;}
    }

    template <typename T> MEMORY_BUFFER &read (T *x)   {*x = readInteger();                       return *this;}
    template <typename T> MEMORY_BUFFER &read1(T *x)   {*x = *(uint8 *)p & ((1u<< 8)-1); skip(1); return *this;}
    template <typename T> MEMORY_BUFFER &read2(T *x)   {*x = *(uint16*)p & ((1u<<16)-1); skip(2); return *this;}
    template <typename T> MEMORY_BUFFER &read4(T *x)   {*x = *(uint32*)p               ; skip(4); return *this;}
    template <typename T> MEMORY_BUFFER &read8(T *x)   {*x = *(uint64*)p               ; skip(8); return *this;}

    // Read `n` values and build a structured array out of them
    template <typename T> MEMORY_BUFFER &read (int n, ARRAY<T> *array)
    {
      array->resize(n);
      iterate (n, read( &((*array)[i]) ));
      return *this;
    }
    // Same as the previous one, but one-byte values are read
    template <typename T> MEMORY_BUFFER &read1 (int n, ARRAY<T> *array)
    {
      array->resize(n);
      iterate (n, read1( &((*array)[i]) ));
      return *this;
    }
    // Same as the previous one, but four-byte values are read
    template <typename T> MEMORY_BUFFER &read4 (int n, ARRAY<T> *array)
    {
      array->resize(n);
      iterate (n, read4( &((*array)[i]) ));
      return *this;
    }
    // Same as the previous one, but eight-byte values are read
    template <typename T> MEMORY_BUFFER &read8 (int n, ARRAY<T> *array)
    {
      array->resize(n);
      iterate (n, read8( &((*array)[i]) ));
      return *this;
    }

    // Read the number of elements in the array from the buffer and then its contents
    template <typename T> MEMORY_BUFFER &read( ARRAY<T> *array)
    {
      int n; read (&n);        // read the number of elements in the array
      return read (n, array);  // proceed to reading the array elements
    }

    MEMORY_BUFFER &read (char *x)     // Read a character
    {
      *x = *(char*)p;
      skip(1);
      return *this;
    }

    MEMORY_BUFFER &read (char* *x)    // Read a string
    {
      char *end = (char*) memchr( p, '\0', (uint8*)bufend - (uint8*)p);
      CHECK(end, (s,"ERROR: archive structure corrupted (bad string)"));
      *x = (char*)p;         // The string read will point directly into the buffer
      p = end+1;
      return *this;
    }

    MEMORY_BUFFER &read (BLOCK_DESCRIPTOR *x)    // Read an archive block descriptor
    {
      read (&x->type);
      read (&x->compressor);
      read (&x->pos);
      read (&x->origsize);
      read (&x->compsize);
      read4(&x->crc);
      return *this;
    }
};


/*****************************************************************************************************
** Local archive block descriptor, i.e. the one located in the archive right after the block itself **
*****************************************************************************************************/

struct LOCAL_BLOCK_DESCRIPTOR : BLOCK
{
  MEMORY_BUFFER buffer;  // Buffer used to read the descriptor. The compressor value read will point to a string in this buffer

  // Read the local block descriptor from the archive
  LOCAL_BLOCK_DESCRIPTOR (MYFILE &arcfile, FILESIZE descr_pos)
  {
    FILESIZE descr_size  =  mymin (arcfile.size()-descr_pos, MAX_FOOTER_DESCRIPTOR_SIZE);
    buffer.openWithCRCAtEnd (arcfile, descr_pos, descr_size);
    uint32 sign;
    buffer.read4 (&sign );
    buffer.read  (&type );
    buffer.read  (&compressor );
    buffer.read  (&origsize );
    buffer.read  (&compsize );
    buffer.read4 (&crc );
    CHECK (sign==aSIGNATURE && origsize>0 && compsize>0 && compsize<=descr_pos, (s,"ERROR: archive structure corrupted (strange descriptor)"));
    pos = descr_pos-compsize;
    //printf("%4.4s %d %s %u %u %08x\n", &sign, type, compressor, origsize, compsize, crc);
  }
};

// Local descriptor of the FOOTER BLOCK
struct FOOTER_BLOCK_LOCAL_DESCRIPTOR : LOCAL_BLOCK_DESCRIPTOR
{
  // Read the local block descriptor and perform the additional checks that only make sense for the FOOTER BLOCK
  FOOTER_BLOCK_LOCAL_DESCRIPTOR (MYFILE &arcfile, FILESIZE descr_pos)  :  LOCAL_BLOCK_DESCRIPTOR (arcfile, descr_pos)
  {
    CHECK (type==FOOTER_BLOCK, (s,"ERROR: archive structure corrupted (footer block not found)"));
  }
};

// Find the FOOTER BLOCK descriptor in the archive file and return its position
FILESIZE FindFooterDescriptor (MYFILE &arcfile)
{
  char buf[MAX_FOOTER_DESCRIPTOR_SIZE];
  FILESIZE arcsize = arcfile.size();
  FILESIZE size = mymin (arcsize, MAX_FOOTER_DESCRIPTOR_SIZE);  // we will look for the signature in the last size bytes of the archive
  arcfile.seek (arcsize-size);
  arcfile.read (buf, size);
  for (char *ptr=buf+size-sizeof(uint32); ; ptr--) {
    if (*(uint32*)ptr == aSIGNATURE)    return (arcsize-size)+(ptr-buf);   // Position in the file of the signature at which the FOOTER BLOCK descriptor starts
    CHECK (ptr>buf, (s,"ERROR: this is not FreeArc archive or this archive is corrupt"));   // The signature was not found in the last MAX_FOOTER_DESCRIPTOR_SIZE bytes of the archive
  }
}


/******************************************************************************
** Information about the archive structure (i.e. all the service blocks) ******
******************************************************************************/
class ARCHIVE
{
private:
  MEMORY_BUFFER buffer;  // Buffer holding the contents of the FOOTER BLOCK. It is destroyed only when the archive is closed, since we use references to the data stored in it
public:
  MYFILE arcfile;        // Archive file. Opened when the ARCHIVE is created and closed when it is destroyed
  ARRAY <BLOCK_DESCRIPTOR> control_blocks_descriptors;   // Descriptors of the archive service blocks, read from the FOOTER BLOCK
  int                      arcLocked;  // Flag indicating that the archive is locked against modification
  ARRAY <char>             arcComment; // Archive comment. May contain null characters
  FILESIZE                 SFXSize;    // Size of the SFX module preceding the archive

  ARCHIVE (FILENAME arcname) : arcfile (arcname, READ_MODE) {}   // Opens the archive file
  void read_structure();               // Reads the descriptions of the service blocks
};

// Reads the descriptions of the service blocks from the FOOTER BLOCK
void ARCHIVE::read_structure()
{
  FILESIZE pos = FindFooterDescriptor (arcfile);            // Find the FOOTER BLOCK descriptor in the archive
  FOOTER_BLOCK_LOCAL_DESCRIPTOR arcFooter (arcfile, pos);   // Read this descriptor and decode it
  buffer.openCompressedCheckCRC (arcFooter.compressor, arcFooter.origsize, arcfile, arcFooter.pos, arcFooter.compsize, arcFooter.crc); // Read the contents of the FOOTER BLOCK into the buffer
  buffer.read (&control_blocks_descriptors);                // Decode the descriptors of the archive service blocks from the buffer
  iterate_array (i, control_blocks_descriptors) {
    control_blocks_descriptors[i].pos  =  arcFooter.pos - control_blocks_descriptors[i].pos; // Replace the relative block addresses (stored as an offset from the start of THIS block) with absolute ones
    //printf("%d %d\n", control_blocks_descriptors[i].pos, control_blocks_descriptors[i].compsize);
  }
  SFXSize = control_blocks_descriptors[0].pos;   // everything located before the first archive block can safely be considered the SFX module :)
  buffer.read1 (&arcLocked);                     // 1 byte: 1 - the archive is locked against further changes, 0 - it is not
  int cmtlen;  buffer.read (&cmtlen);            // Old-style comment - in UCS4
  arcComment.set (cmtlen, buffer.p);
  for (int i=0; i<cmtlen; i++)  arcComment[i] = buffer.p[i*4];
  buffer.skip (cmtlen*4);
  char *rr_settings; if (!buffer.eof())  buffer.read (&rr_settings);
  if (!buffer.eof()) {
    buffer.read (&cmtlen);                       // The comment is encoded as a character array with an explicitly given length
    if (cmtlen>0)  arcComment.set (cmtlen, buffer.p);
  }
  //printf("%d %d %*.*s\n", arcLocked, arcComment.size, arcComment.size, arcComment.size, &arcComment[0]);
}


/******************************************************************************
** Directory block ************************************************************
******************************************************************************/
class DIRECTORY_BLOCK
{
public:
  MYFILE &arcfile;                     // Archive file this glorious directory block belongs to
private:
  MEMORY_BUFFER buffer;                // Buffer holding the entire directory in binary form. The decoded file names refer to this buffer, so it is not deleted until work with the directory is finished

  int               dirs_in_block;     // Number of directories recorded in this DIRECTORY BLOCK
  ARRAY <FILENAME>  dirs;              // Directory names
  ARRAY <int>       dir_numbers;       // Directory number for each of the files
public:
  FILENAME  dirname (int i)  {return dirs[dir_numbers[i]];}  // Directory name of the i-th file
  FILENAME  fullname(int i, char buffer[]);                  // Full name of the i-th file
  int               total_files;       // Number of files described in this directory block
  ARRAY <FILENAME>  name;              // File names (without the directory name)
  ARRAY <FILESIZE>  size;              // File sizes
  ARRAY <XFILETIME> time;              // File modification times
  ARRAY <BOOL>      isdir;             // Boolean "is this a directory?" flags
  ARRAY <CRC>       crc;               // File CRCs

  int                       num_of_blocks;  // Number of data blocks
  ARRAY <int>               num_of_files;   // Number of files in each data block, which after the header is read is replaced by the number of the first file IN THE NEXT block, for block_start()/block_end()
  ARRAY <BLOCK_DESCRIPTOR>  data_block;     // Descriptions of the data blocks (compressor, position in the archive, length)

  int block_start (int block_num)  {return block_num>0? num_of_files[block_num-1] : 0;}  // Number of the first file in data block block_num
  int block_end   (int block_num)  {return num_of_files[block_num];}                     // Number of the first file in the next data block (i.e. the last one in this block + 1)

  DIRECTORY_BLOCK (ARCHIVE &arc, BLOCK &block_info);   // Reads the contents of the directory block from the archive and decodes it so as to provide fast access to the description of any file and any data block
};

DIRECTORY_BLOCK::DIRECTORY_BLOCK (ARCHIVE &arc, BLOCK &block_info) : arcfile (arc.arcfile)
{
  // Read the directory contents into the buffer, decompress it and check the CRC
  CHECK (block_info.type == DIR_BLOCK, (s,"INTERNAL ERROR: must be dir block"));
  buffer.openCompressedCheckCRC (block_info.compressor, block_info.origsize, arcfile, block_info.pos, block_info.compsize, block_info.crc);

  // Read the total number of solid blocks and the information about each of them - number of files, compressor,
  // the offset of the solid block start relative to the directory block, and the compressed size
  buffer.read  (&num_of_blocks);    buffer.read  (num_of_blocks, &num_of_files);
  ARRAY <COMPRESSOR> compressors;   buffer.read  (num_of_blocks, &compressors);
  ARRAY <FILESIZE>   offsets;       buffer.read  (num_of_blocks, &offsets);
  ARRAY <FILESIZE>   compsizes;     buffer.read  (num_of_blocks, &compsizes);

  // Reconstruct data_block[] from the data we read
  data_block.setsize (num_of_blocks);
  iterate_array (i, data_block)
  {
    data_block[i].type       = DATA_BLOCK;
    data_block[i].compressor = compressors[i];
    data_block[i].pos        = block_info.pos - offsets[i];    // Compute the absolute address of the block in the archive from its offset relative to the directory block
    data_block[i].origsize   = 0;               // And who needs that anyway?
    data_block[i].compsize   = compsizes[i];
    data_block[i].crc        = 0;               // The CRC of data blocks is not stored - there is no point in it
    //printf("datablock %s %d %d\n", data_block[i].compressor, data_block[i].pos, data_block[i].compsize);
  }

  // Count the total number of files in this directory and change num_of_files[block_num] so that this array can be used to determine the files belonging to data block block_num
  total_files=0;  iterate (num_of_blocks, (total_files += num_of_files[i], num_of_files[i] = total_files));

  // Read the directory names and convert the directory separator characters to the ones used on this platform
  buffer.read  (&dirs);
  iterate_array (i, dirs)    replace (dirs[i], UNSUPPORTED_PATH_DELIMITERS, PATH_DELIMITER);

  // Read the information about the individual files
  buffer.read  (total_files, &name);
  buffer.read  (total_files, &dir_numbers);
  buffer.read  (total_files, &size);
  // EIGHT bytes, not four. ByteStream.hs:599 writes CTime as a fixed 64-bit
  // little-endian value, and :394 records that only FreeArc/Arc.exe 0.67 on
  // 32-bit wrote it at the native 4-byte stride. Reading 4 here left the buffer
  // 4*N bytes short of where the next field starts, so `isdir` and `crc` -- the
  // two fields that follow -- were read from the wrong offset entirely. That is
  // why directories came out as zero-byte FILES and why every extracted file
  // failed its CRC check: the data was fine, the flags and checksums were not.
  buffer.read8 (total_files, &time);
  buffer.read1 (total_files, &isdir);
  buffer.read4 (total_files, &crc);

  //iterate( total_files, printf("%s %s %d %d\n", dirname(i), name[i], size[i], isdir[i]));
  //printf("%d files\n", total_files);
}

// Full name of the i-th file
FILENAME DIRECTORY_BLOCK::fullname (int i, char buffer[])
{
  strcpy (buffer, dirname(i));
  if (buffer[0] != '\0')  strcat (buffer, STR_PATH_DELIMITER);
  strcat (buffer, name[i]);
  return buffer;
}

