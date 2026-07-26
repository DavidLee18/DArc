class UI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
public:
  UI();
  ~UI();
  void DisplayHeader (char* header);
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressWrite (uint64 writtenBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
};

UI::UI()
{
}

UI::~UI()
{
}

void UI::DisplayHeader (char* header)
{
  printf ("%s", header);
}

void UI::BeginProgress (uint64 totalBytes)
{
}

bool UI::ProgressRead (uint64 readBytes)
{
  return TRUE;
}

bool UI::ProgressWrite (uint64 writtenBytes)
{
  return TRUE;
}

bool UI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
          operation, filename, filesize);
  return TRUE;
}

void UI::EndProgress()
{
  printf ("All OK");
}

FILENAME UI::GetOutDir()
{
  return outdir;
}

bool UI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  printf (". %s archive: %s\n",                       // Print the name of the archive being processed
    cmd=='l'||cmd=='v'? "Listing" : cmd=='t' ? "Testing" : "Extracting", drop_dirname(arcname));
  if (cmtsize>0)                                      // Print the archive comment
#ifdef FREEARC_WIN
{
    // Convert comment from UTF-8 to OEM encoding before printing
    char *oemname = (char*) malloc(cmtsize+1);
    strncpy (oemname, comment, cmtsize);
    oemname[cmtsize] = 0;
    utf8_to_oem (oemname, oemname);
    printf ("%s\n", oemname);
    free (oemname);
}
#else
    printf("%*.*s\n", cmtsize, cmtsize, comment);
#endif

#ifdef FREEARC_SFX
  // In an SFX we must ask the user for confirmation before starting extraction
  if (!silent)
  {
    char answer[256];
    printf ("Continue extraction (y/n)? ");
    // gets() is gone from C11 and from glibc, and could not be told the buffer
    // size in any case -- see AskOverwrite below.
    if (!fgets (answer, sizeof answer, stdin))  answer[0] = '\0';
    answer[strcspn(answer, "\r\n")] = '\0';
    if (! (strequ(answer,"y") || strequ(answer,"Y")))
    {
      printf ("Extraction aborted!\n");
      return FALSE;
    }
    printf("\n");
  }
#endif
  return TRUE;
}

char UI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  char help[] = "Valid answers: Y - yes, N - no, A - overwrite all, S - skip all, Q - quit\n";
  again: printf ("Overwrite %s (y/n/a/s/q) ? ", filename);
  // gets() was removed from C11 and from glibc; it also had no way to know the
  // buffer's size, which in a program that reads attacker-supplied archives is
  // not a stylistic point. fgets keeps the trailing newline, so it is trimmed
  // before the single-character check below.
  char answer[256];
  if (!fgets (answer, sizeof answer, stdin))  {printf ("Extraction aborted\n");  exit(1);}
  answer[strcspn(answer, "\r\n")] = '\0';
  *answer = tolower(*answer);
  if (strlen(answer)!=1 || !strchr("ynasq", *answer))  {printf (help);  goto again;}
  if (*answer=='q') {printf ("Extraction aborted\n");  exit(1);}
  return *answer;
}

