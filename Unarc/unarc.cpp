// to do: selecting files by name ("name" or "dir/name"),
//        decryption of data/headers
//        appending ".arc", listfiles/-ap/-kb
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <wchar.h>

// SFX module is just unarc.cpp compiled with FREEARC_SFX defined
#ifdef FREEARC_SFX
#define NAME           "SFX"
#else
#define NAME           "unpacker"
#endif

#define HEADER1        "FreeArc 0.51 "
#define HEADER2        "  http://freearc.org  2009-04-28\n"

// Access to the archive structure
#include "ArcStructure.h"

// The whole user dialogue is described in the pluggable modules included here
#ifdef FREEARC_GUI
#include "gui\gui.h"
#include "gui\gui.cpp"
#else
#include "CUI.h"
#endif
UI UI;


/******************************************************************************
** Information about the command being executed by the extractor **************
******************************************************************************/
class COMMAND
{
public:
  char cmd;             // The command being executed
  FILENAME arcname;     // Name of the archive processed by the command
  FILENAME *filenames;  // Names of the files inside the archive processed by the command
  FILENAME outpath;     // The -dp option
  FILENAME runme;       // File to run after extraction
  BOOL wipeoutdir;      // Delete the files from outpath after runme finishes?
  BOOL ok;              // Is the command executing successfully?
  int  silent;          // The -s option
  BOOL yes;             // The -o+ option
  BOOL no;              // The -o- option
  BOOL noarcext;        // The --noarcext option
  BOOL nooptions;       // The -- option

  bool list_cmd()  {return cmd=='l' || cmd=='v';}   // True if this is an archive listing command

  // Command line parsing
  COMMAND (int argc, char *argv[])
  {
#ifdef FREEARC_WIN
    // Instead of those ANSI-codepage encoded argv[] strings provide true UTF-8 data!
    WCHAR **argv_w = CommandLineToArgvW (GetCommandLineW(), &argc);
    argv = (char**) malloc ((argc+1) * sizeof(*argv));
    for (int i=0; i<argc; i++)
    {
      argv[i] = (char*) malloc (_tcslen (argv_w[i]) * 4 + 1);
      utf16_to_utf8 (argv_w[i], argv[i]);
      argv[i] = (char*) realloc (argv[i], strlen(argv[i]) + 1);
    }
    argv[argc] = NULL;
#endif
    // Default options
    noarcext  = FALSE;
    nooptions = FALSE;
    outpath = "";
    runme = NULL;
    wipeoutdir = FALSE;
    yes = FALSE;
    no  = FALSE;
    silent = 0;
#ifdef FREEARC_SFX
    arcname = argv[0];
    cmd     = 'x';

#ifdef FREEARC_INSTALLER
    // Installer by default extracts itself into some temp directory, runs setup.exe and then remove directory's contents
    if (argv[1] == NULL)
    {
        silent = 2;

        // Get TEMP path and convert it into UTF-8
        CFILENAME TempPathW = (TCHAR*)   malloc (MY_FILENAME_MAX * 4);
         FILENAME TempPath  = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        GetTempPathW(MY_FILENAME_MAX, TempPathW);
        utf16_to_utf8 (TempPathW, TempPath);

        // Create unique tempdir
        outpath = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        for (unsigned i = (unsigned) GetTickCount(); ; )
        {
            i = i*54322457 + 137;
            sprintf (outpath, "%s%s%u", TempPath, "installer", i);
            utf8_to_utf16 (outpath, TempPathW);
            if (_wmkdir(TempPathW) == 0)   break;  // Break on success
        }
        free(TempPathW);

        // Run setup.exe from this dir
        runme   = (FILENAME) malloc (MY_FILENAME_MAX * 4);
        sprintf (runme, "%s%s%s", outpath, STR_PATH_DELIMITER, "setup.exe");

        // Delete extracted files afterwards
        wipeoutdir = TRUE;
    }
#endif

    // Parse options
    for (ok=TRUE; ok && *++argv; )
    {
      if (argv[0][0]=='-' || strequ(argv[0],"/?") || strequ(argv[0],"/help"))
      {
             if (strequ(argv[0],"-l"))       cmd = 'l', silent = silent || 2;
        else if (strequ(argv[0],"-v"))       cmd = 'v', silent = silent || 2;
        else if (strequ(argv[0],"-e"))       cmd = 'e', silent = silent || 2;
        else if (strequ(argv[0],"-x"))       cmd = 'x', silent = silent || 2;
        else if (strequ(argv[0],"-t"))       cmd = 't', silent = silent || 2;
        else if (strequ(argv[0],"-y"))       yes = TRUE;
        else if (strequ(argv[0],"-n"))       no  = TRUE;
        else if (start_with(argv[0],"-d"))   outpath = argv[0]+2;
        else if (strequ(argv[0],"-s"))       silent = 1;
        else if (strequ(argv[0],"-s0"))      silent = 0;
        else if (strequ(argv[0],"-s1"))      silent = 1;
        else if (strequ(argv[0],"-s2"))      silent = 2;
        else if (strequ(argv[0],"--"))       nooptions=TRUE;
        else ok=FALSE;
      }
      else break;
    }

    filenames = argv;            // the rest of arguments are filenames
    if (ok)  return;

    // Display help
    char *helpMsg = (char*) malloc(1000+strlen(arcname));
    sprintf (helpMsg,
#ifdef FREEARC_GUI
           HEADER1 NAME HEADER2
#else
           HEADER2
#endif
           "Usage: %s [options] [filenames...]\n"
           "Available options:\n"
#ifndef FREEARC_GUI
           "  -l       - display archive listing\n"
           "  -v       - display verbose archive listing\n"
#endif
           "  -x       - extract files\n"
           "  -e       - extract files without pathnames\n"
           "  -t       - test archive integrity\n"
           "  -d{Path} - set destination path\n"
           "  -y       - answer Yes on all overwrite queries\n"
           "  -n       - answer No  on all overwrite queries\n"
           "  -s[1,2]  - silent mode\n"
           "  --       - no more options\n"
           , drop_dirname(arcname));
#ifdef FREEARC_GUI
    MessageBoxW (NULL, MYFILE(helpMsg).displayname(), _T("Command-line help"), MB_OK | MB_ICONERROR);
#else
    printf("%s", MYFILE(helpMsg).displayname());
#endif

#else
    cmd     = ' ';
    arcname = NULL;
    for (ok=TRUE; ok && *++argv; )
    {
      if (argv[0][0]=='-')
      {
        if (strequ(argv[0],"--noarcext"))    noarcext =TRUE;
        else if (strequ(argv[0],"-o+"))      yes      =TRUE;
        else if (strequ(argv[0],"-o-"))      no       =TRUE;
        else if (start_with(argv[0],"-dp"))  outpath = argv[0]+3;
        else if (strequ(argv[0],"--"))       nooptions=TRUE;
        else ok=FALSE;
      }
      else if (cmd==' ')   cmd = argv[0][0], ok = ok && strlen(argv[0])==1;
      else if (!arcname)   arcname = argv[0];
      else break;
    }

    filenames = argv;            // the rest of arguments are filenames
    ok = ok && strchr("lvtex",cmd) && arcname;
    if (ok)  return;
    printf(HEADER2
           "Usage: unarc command [options] archive[.arc] [filenames...]\n"
           "Available commands:\n"
           "  l - display archive listing\n"
           "  v - display verbose archive listing\n"
           "  e - extract files into current directory\n"
           "  x - extract files with pathnames\n"
           "  t - test archive integrity\n"
           "Available options:\n"
           "  -dp{Path}   - set destination path\n"
           "  -o+         - overwrite existing files\n"
           "  -o-         - don't overwrite existing files\n"
           "  --noarcext  - don't add default extension to archive name\n"
           "  --          - no more options\n");
#endif
  }

  // TRUE if the i-th file of the directory block dirblock should be included in processing
  BOOL accept_file (DIRECTORY_BLOCK *dirblock, int i)
  {
    if (!*filenames)  return TRUE;            // No file name was given on the command line - so every file must be processed
    for (FILENAME *f=filenames; *f; f++) {
      if (strequ (dirblock->name[i], *f))
        return TRUE;                          // Got a match!
    }
    return FALSE;                             // No matching name was found
  }
};


/******************************************************************************
** Implementation of the archive listing command ******************************
******************************************************************************/
#ifdef FREEARC_GUI
void ListHeader (COMMAND &) {}
void ListFooter (COMMAND &) {}
void ListFiles (DIRECTORY_BLOCK *, COMMAND &) {}
#else

uint64 total_files, total_bytes, total_packed;

void ListHeader (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("Date/time                  Size Filename\n"
              "----------------------------------------\n");
  else
      printf ("Date/time              Attr            Size          Packed      CRC Filename\n"
              "-----------------------------------------------------------------------------\n");
  total_files=total_bytes=total_packed=0;
}

void ListFooter (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("----------------------------------------\n");
  else
      printf ("-----------------------------------------------------------------------------\n");
  printf ("%.0lf files, %.0lf bytes, %.0lf compressed", double(total_files), double(total_bytes), double(total_packed));
}

void ListFiles (DIRECTORY_BLOCK *dirblock, COMMAND &command)
{
  int  b=0;                // current_data_block
  bool Encrypted = FALSE;  // is the current solid block encrypted?
  uint64 packed=0;
  iterate_var (i, dirblock->total_files) {
    // Advance the solid block number if we have moved past the last file belonging to it
    if (i >= dirblock->block_end(b))
      b++;
    // If this is the first file in the solid block - gather the block-related information
    if (i == dirblock->block_start(b))
    { // Record the whole packed size of the block on its first file
      packed = dirblock->data_block[b].compsize;
      // Remember the solid block information so it can be used for every file in this solid block
      char *c = dirblock->data_block[b].compressor;
      Encrypted = strstr (c, "+aes-")!=NULL || strstr (c, "+serpent-")!=NULL || strstr (c, "+blowfish-")!=NULL || strstr (c, "+twofish-")!=NULL;
    }


    if (command.accept_file (dirblock, i)) { //   If this file has to be processed
      unsigned long long filesize = dirblock->size[i];
      char timestr[100];  FormatDateTime (timestr, 100, dirblock->time[i]);

      if (command.cmd=='l')
          printf (dirblock->isdir[i]? "%s       -dir-" : "%s %11.0lf", timestr, double(filesize));
      else
          printf ("%s %s %15.0lf %15.0lf %08x", timestr, dirblock->isdir[i]? ".D.....":".......", double(filesize), double(packed), dirblock->crc[i]);
      printf ("%c", Encrypted? '*':' ');

      // Print filename using console encoding
      static char filename[MY_FILENAME_MAX*4];
      dirblock->fullname (i, filename);
      static MYFILE file;  file.setname (filename);
      printf ("%s\n", file.displayname());

      total_files++;
      total_bytes  += filesize;
      total_packed += packed;    packed = 0;
    }
  }
}
#endif

/******************************************************************************
** Implementation of the archive extraction and testing commands **************
******************************************************************************/

// Variables reflecting the state of the input data reading process
MYFILE *infile;          // The archive file being read from
FILESIZE bytes_left;     // Number of bytes left to read before this solid block's packed data is exhausted

// Variables reflecting the state of the decompressed data writing process
COMMAND *cmd;             // The command being executed
DIRECTORY_BLOCK *dir;     // The directory the files being extracted belong to
int curfile;              //   Index in the directory of the file currently being extracted
BOOL included;            //   Is the current file included in processing or are we just skipping it?
int extractUntil;         //   Index of the last file that has to be extracted from this solid block
MYFILE outfile;           // The file being extracted from the archive
char fullname[MY_FILENAME_MAX*4]; // Full name of the file currently being extracted
FILESIZE bytes_to_write;  // How many bytes are still left to write in the current file
FILESIZE writtenBytes;    // How many bytes have been decompressed in total in the current archive
FILESIZE archive_pos;     // Current position in the archive
CRC crc;                  // CRC of the data written to the file
enum PASS {FIRST_PASS, SECOND_PASS};  // First/second pass over the solid block (the first extracts directories and empty files, the second all the rest)

// Emergency exit procedure
void quit(void)   {if (outfile.isopen())  outfile.close(), delete_file(outfile.filename);
                   exit (FREEARC_EXIT_ERROR);}

// Action taken on an error in CHECK()
#undef  ON_CHECK_FAIL
#define ON_CHECK_FAIL()   quit()

// * The procedures below provide an abstract way of working with the current output file,
// * hiding details such as the differences between the e/x/t commands, the difference between directories and files,
// * and the fact that some files may be excluded from processing

// Open the next output file and print a message about extracting it
void outfile_open (PASS pass)
{
  crc = INIT_CRC;
  bytes_to_write = dir->size[curfile];
  if (pass==SECOND_PASS && bytes_to_write==0)
    return;  // Directories and empty files were extracted in first pass
  included = cmd->accept_file (dir, curfile);
  char *xname = cmd->cmd=='e'? dir->name[curfile]
                             : dir->fullname (curfile, fullname);
  outfile.setname (xname);

  if (included && cmd->cmd!='t')
    if (dir->isdir[curfile])
      {if (cmd->cmd!='e')  BuildPathTo (outfile.filename), create_dir (outfile.filename);}
    else
      {if (outfile.exists())
       {
         if (cmd->no)  included = FALSE;
         else if (!cmd->yes)
         {
           char answer = UI.AskOverwrite (outfile.displayname(), dir->size[curfile], dir->time[curfile]);
           switch (answer)
           {
             case 'y': break;
             case 'n': included = FALSE;  break;
             case 'a': cmd->yes = TRUE;   break;
             case 's': cmd->no  = TRUE;   included = FALSE;  break;
             case 'q': quit();
           }
         }
       }
       if (included)  outfile.open (WRITE_MODE);}

  if (pass==FIRST_PASS || dir->size[curfile]>0)   // Don't report the extraction of directories/empty files twice
    if (!(dir->isdir[curfile] && cmd->cmd!='x'))  // Don't report testing of directories ;)
      if (!UI.ProgressFile (dir->isdir[curfile], included? (cmd->cmd=='t'? "Testing":"Extracting"):"Skipping", MYFILE(xname).displayname(), bytes_to_write))
        quit();
}

// Write data to the output file
void outfile_write (void *buf, int size)
{
  crc = UpdateCRC (buf, size, crc);
  if (included && cmd->cmd!='t' && size)
    outfile.write(buf,size);
  if (!UI.ProgressWrite (writtenBytes += size))  quit();
}

// Close the output file
void outfile_close()
{
  if (included)
  {
    CHECK ((crc^INIT_CRC) == dir->crc[curfile], (s,"ERROR: file %s failed CRC check", outfile.utf8name));
    if (cmd->cmd!='t' && !dir->isdir[curfile])
      outfile.close();
      outfile.SetFileDateTime (dir->time[curfile]);
  }
  included = FALSE;
}

// Read/write callback function for the decompressor
int callback_func (const char *what, void *buf, int size, void *auxdata)
{
  if (strequ (what, "read")) {
    int read_bytes = mymin (bytes_left, size);
    if (read_bytes==0)  return 0;
    if (!UI.ProgressRead (archive_pos))  quit();
    int len = infile->tryRead (buf, read_bytes);
    if (len>0)  bytes_left -= len,  archive_pos += len;
    return len;

  } else if (strequ (what, "write")) {
    int origsize = size;
    if (curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // We ran into a dumb decompressor unable to stop decompression on request :(
    while (size>0) {
      int n = mymin (bytes_to_write, size);   // Write however much is left until the end of the file or
      outfile_write (buf,n);                  // however much data is left in the buffer - whichever is smaller
      bytes_to_write -= n;
      if (bytes_to_write==0) {                // If the file has been written to the end - move on to the next one
        outfile_close();
        if (++curfile > extractUntil)  return FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED;   // If every file we have to extract from this block has already been extracted, ask the decompressor to stop decompressing
        outfile_open(SECOND_PASS);
      }
      buf=(uint8*)buf+n; size-=n;
    }
    return origsize;     // Signal a successful write and ask it to continue decompressing

  } else return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

// Extract or test the files from the solid block numbered block_num of the directory block dirblock
void ExtractFiles (DIRECTORY_BLOCK *dirblock, int block_num, COMMAND &command)
{
  cmd = &command;
  dir = dirblock;
  BLOCK& data_block (dirblock->data_block [block_num]);
  extractUntil = -1;                        // This variable will hold the index of the last file in the solid block that has to be processed
  // Walk through all the files in this block
  for (curfile = dirblock->block_start(block_num); curfile < dirblock->block_end(block_num); curfile++) {
    if (command.accept_file (dirblock, curfile))           // If this file has to be processed
    {
      if (dir->size[curfile]==0) {   // then if it is a directory or an empty file - do it right away
        outfile_open (FIRST_PASS);
        outfile_close(); }
      else
        extractUntil = curfile;      // otherwise - remember that the block has to be decompressed at least up to this file
    }
  }
  if (extractUntil >= 0) {                       // If there was something to extract in this block - then let's extract it! :)
    infile = &dirblock->arcfile;                 //   The archive file
    infile->seek (archive_pos = data_block.pos); //   Start of the solid block's data in the archive
    bytes_left = data_block.compsize;            //   Size of the packed data in the solid block
    curfile = dirblock->block_start (block_num); // Index of the first file in this solid block
    outfile_open (SECOND_PASS);                  // Open the first output file
    int result = MultiDecompress (data_block.compressor, callback_func, NULL);
    CHECK (result!=FREEARC_ERRCODE_INVALID_COMPRESSOR, (s,"ERROR: unsupported compression method %s", data_block.compressor));
    CHECK (result>=0 || result==FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED, (s,"ERROR: archive data corrupted (decompression fails)"));
    outfile_close();                             // Close the last output file
  }
}

#ifdef FREEARC_INSTALLER
// Wipes entire directory with all its subdirs
void wipedir(TCHAR *dir)
{
    // List all entries in this directory
    CFILENAME dirstar  = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    CFILENAME fullname = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    _stprintf (dirstar, _T("%s%s*"), dir, _T(STR_PATH_DELIMITER));
    WIN32_FIND_DATA FindData[1];
    HANDLE h = FindFirstFileW (dirstar, FindData);
    if (h) do {
        // For every entry except for "." and ".., remove entire subdir (if it's a directory) or remove just file itself
        if (_tcscmp(FindData->cFileName,_T("."))  &&  _tcscmp(FindData->cFileName,_T("..")))
        {
            _stprintf (fullname, _T("%s%s%s"), dir, _T(STR_PATH_DELIMITER), FindData->cFileName);
            if (FindData->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                wipedir (fullname);
            else
                DeleteFile (fullname);
        }
    } while (FindNextFile(h,FindData));
    FindClose(h);
    RemoveDirectory (dir);
    free(fullname); free(dirstar);
}
#endif


/******************************************************************************
** Main program ***************************************************************
******************************************************************************/

// Reads the archive structure and, depending on the command being executed, calls
// ListFiles for each directory block or ExtractFiles for each solid block
void ProcessArchive (COMMAND &command)
{
  static ARCHIVE arcinfo (command.arcname);
  arcinfo.read_structure();                                           // Read the archive structure
  // Print the operation header on the screen and ask the user for permission to unpack the SFX
  if (!UI.AllowProcessing (command.cmd, command.silent, MYFILE(command.arcname).displayname(), &arcinfo.arcComment[0], arcinfo.arcComment.size, command.outpath)) {
    command.ok = FALSE;  return;
  }
  if (command.cmd!='t')  outfile.SetBaseDir (UI.GetOutDir());

  writtenBytes = 0;
  if (command.list_cmd())  ListHeader (command);
  else                     UI.BeginProgress (arcinfo.arcfile.size());
  iterate_array (i, arcinfo.control_blocks_descriptors) {             // Walk through all the service blocks in the archive...
    BLOCK& block_descriptor = arcinfo.control_blocks_descriptors[i];
    if (block_descriptor.type == DIR_BLOCK) {                         // ... and pick out the directory blocks among them
      DIRECTORY_BLOCK dirblock (arcinfo, block_descriptor);           // Read the directory block
      if (command.list_cmd())                                         // If this is a listing command
        ListFiles (&dirblock, command);                               //   then execute it
      else
        iterate_array (i, dirblock.data_block)                        //   otherwise - walk through all the solid blocks in the directory
          ExtractFiles (&dirblock, i, command);                       //     and run the testing/extraction procedure for each of them
    }
  }
  if (command.list_cmd())  ListFooter (command);
  else                     UI.EndProgress();

#ifdef FREEARC_INSTALLER
  // Run setup.exe after unpacking
  if (command.runme)
  {
      CFILENAME tmp  = (TCHAR*) malloc (MY_FILENAME_MAX * 4);
      CFILENAME tmp2 = (TCHAR*) malloc (MY_FILENAME_MAX * 4);

      // Execute command.runme in the directory command.outpath
      RunProgram (utf8_to_utf16 (command.runme, tmp), utf8_to_utf16 (command.outpath, tmp2), command.wipeoutdir);

      // Wipe outdir after installation was completed
      if (command.wipeoutdir)
          wipedir (utf8_to_utf16 (command.outpath, tmp));

      free(tmp); free(tmp2);
  }
#endif
}


int main (int argc, char *argv[])
{
  SetCompressionThreads (GetProcessorsCount());
  UI.DisplayHeader (HEADER1 NAME);
  COMMAND command (argc, argv);    // Parse the command
  if (command.ok)                  // If parsing succeeded and the command can be executed
    ProcessArchive (command);      //   Execute the parsed command
  printf ("\n");
  return command.ok? EXIT_SUCCESS : FREEARC_EXIT_ERROR;
}

