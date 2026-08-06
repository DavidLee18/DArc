# FreeArc version history

The upstream changelog DArc inherited, carried over verbatim from
`Documentation/History.txt`. It documents FreeArc up to 0.67 — the release this
fork descends from — and is kept because it is the only record of *why* several
format decisions were made. Nothing here describes DArc's own changes; for those
read the git history.

```text
Version 0.20 was rewritten from scratch in order to achieve the following goals:
- reliable error handling
- splitting complex tasks into separate interacting threads
- exhaustive commenting of the sources in Russian
- economical memory consumption
- greater flexibility of the program for creating volumes, storing when
    compression failed, multiprocessing, etc.
- change of the archive structure and of the file catalog
- creating the possibility of adding encryption, data recovery,
    storing file generations, chaining compression algorithms, etc.

0.20  15.01.05  Hugs-compatible version without compression.
0.21   5.02.05  PPMD, LZMA, LZP; -m1..-m6x, -t, -kb, CRC, restoration of time/date
0.22  19.02.05  GRZip, BCJ-x86, -js, complex -m decoder, full RE in filenames, last version for GHC 6.2.1
0.23  26.04.05  GHC 6.4, FAR plugin, -tl/-tk, arc.ini, %FREEARC, -cfg-, -o, -y, Russian names, economical read-ahead, HEADER BLOCK
0.24  16.12.05  Compression Library, Extractor, SFX; y/c/cw/k/s/-z/-k/-sfx; --Limit*Mem; Unicode, >4gb, 300 bytes/file
0.25   1.11.06  lzma 4.43, smart update of solid blocks

0.30  15.11.06  dict, ppmonstr; --display, --logfile
0.31  18.11.06  new LZP; new -m parser: m1x..m7, -m4b/3t, dict+lzp+ppmd/pmm, lzp+lzma; [Compression methods], --print-config
0.32  06.12.06  First public version (advertised with dict)
0.33  10.12.06  -dsgecpn; the documentation section "The arc.ini config file" was written
0.35  09.01.07  REP: replaced lzp in the compression of binary files
0.36  22.02.07  r/rr, -ac/-ao, m[f], -d[f], -t/-pt, -w, -lc/-ld and a heap of minor improvements
0.40  01.01.08  MM, tornado, delta, lzma -mt, ext.compressors, tempfile; encryption, unix version, http+recovery, -sc/far/tc, ^Break, warn/error/fail->logfile+stderr+exitCode, -cfg/-env, -n/-ta/../-sl/-sm


=== Change history of FreeArc 0.50 ================================================================

  Compression: filetype detection!!!
  GUI!!!
    prompts for archive comment, encryption/decryption passwords.

25.12.2007

  File Manager!!!
    INTERNET!
    deleting files
      the archive must be closed first
      confirmation prompt before deletion; check selected>[]
      remove the files from the screen too
    count and size of the selected files at the bottom
    going up and to the root
    enter in dialogs
    enter to go into an archive/directory
    use a single progress dialog for all operations and remove it from the screen when there is no work
    operation queue
    deleting files with Russian names from an archive (disable codepage conversion when handling commands we generated ourselves)
  GUI
    file overwrite prompt
    refresh everything except progressBarSetFraction once every 0.5 seconds
      (result - the GUI overhead on my duron-1200 dropped to ~7%)
  Compression: filetype detection
    Improved file type detection, problems remain only with Ruby
    Not used for files that are placed into non-standard groups in arc.groups ($iso, $precomp, etc.)
      a $jpg file will be detected as having type "", since there is no $jpg group in the default compression algorithm

8.01.2008

  File Manager
    no longer falls back after performing several operations
    testing/extraction of all selected archives
    extraction dialog
      "Extract a.cpp/files/5 files from a.arc/5 archives"
  Bugs
    -mrep:1200m - prints "memory used 1gb", but should print "1500mb" - rounding is far too coarse!
    the total memory amount of an algorithm chain must be an Integer
    Errors arising inside the (de)compression algorithms are at last reported clearly.
      "Arc.exe a a -di -lc- -ld- -m9b": "ERROR: runArchiveCreate:results undefined"
    rep:2047m (far too tight a limit on the dictionary size at the solid-block level)
    "-m=ppmd:1536m:" - crash because of the trailing colon!
    TRUE/FALSE may be defined by other libs
    lzma: allow any dictionary size that fits into 4gb of memory
    lzma:ultra=lzma:max
    -lcN% limits the algorithm not only by the amount of physical memory, but also by the amount of free virtual memory
      (in particular, by default no more than 3/4 of free virtual memory is used)
    In addition, the amounts of allocated memory are limited by the amount of free address space
      of the process minus 1mb
    rep - a 256 mb hash is used up to rep:1536m; the amount of memory used is reported exactly
    lzp - memory allocation errors are checked and the amount of memory used is computed/limited
      correctly
    -m3x: exclude delta for text/bmp

10.01.2008

  GUI: Overwrite dialog - full info about both files
  File Manager
    archives with Russian names, Russian directory names
    extraction dialog
      output directory selection and directory history
      frame around the overwrite options
      "append archive name to the extraction path" checkbox
      windowPresent to switch to main window after Extract window has been closed
    speed up navigation - update the data in the model instead of reassigning it.
    confirmation prompt when clicking the close button in the progress dialog
    we save history entries to disk; we write/read them on every operation (in order to
      provide crash protection / make the history global for all running copies of WinArc)
    when going up, place the cursor on the directory/archive we have just left
    Select/Unselect/Refresh
    combo box above the list for navigating the disk/archive structure
    the progress dialog is cleared before the next command is executed
    directories in the archive (140 seconds and 260 mb to open an archive with 350 thousand files)
    "additional options" with history in add/extract/... dialogs
    ArcInfo - information about the archive
    add/modify/join commands
    entering encrypted archives if the password/keyfile is configured in Decryption
    default arcname/outpath
    Settings dialog:
      localization: localization files in the arc.languages directory in UTF-8, import from 7-zip
      logfile
      associating WinArc with .arc files
  lzma
    round dictsize to the *nearest* power of two before computing hs
    the amount of memory used is *reported* correctly
    allocate the memory for the dictionary last
    allocate hash and son separately, and son first (normalize)
  Memory management: by default up to 75% of physical memory is used,
    but within the size of the largest contiguous free memory block minus 5 mb,
.   while decompression is limited to 1 gb of memory (-lc75% -ld1gb).
.   With -lc- -ld- all checks are disabled. That is why lzma/rep with maximum
      settings (lzma:255m, lzma:fast:511m, rep:2047m) are available only with "-lc- -ld-"
    Dict returns dmem=1mb, although that is not entirely true ;)
. RegExp: pattern matching sped up
. -s: by default solid blocks of unlimited size are created (previously it was 1 gb)
. The --archive-comment option lets you enter an archive comment directly on the command line
. ~/.FreeArc used by default for storing winarc.ini/winarc.history
  The l/v commands print '*' before a file name if it is encrypted
  The v command prints the compressed size of a file, l/v print the total compressed size of the files in the archive

28.01.2008

. Filetype detection:
    focused on identifying $text and $compressed files (for -m2..-m4)
    disabled in -m1 and when an explicit compression method is used (in the general case - when
       $text and $compressed are absent from the file type list - what would it be good for then? :)
    -ma+, -ma-, -ma0..9 - enable/disable/level of autodetection
  File Manager
    Compression/encryption profiles in Add&Settings dialogs
    Significantly improved Add dialog
    tooltips: N+1000
    Localization of names for standard profiles
  rr+, rr0.1%, rr0.01%

8.02.2008

. Taught it to understand the BackSpace key for going one level up
  Text compression in -m3 sped up by a factor of 1.7: -m3$text=dict:p+lzp:32m:24:h20+grzip:m3:l
  Improved file type detection (3 heuristics + recursion)
  lzp in -m5t..-m9t: dictionary enlarged
  tor:9-11 - cycled MF & large 2/3 hash updated at each step
  lz in filetype detection sped up (45mb/s)
  tor:2-3 - faster by 10% due to disabling of MM tables
. Lua scripting!
. Binary file compression in -m3 is 10% faster and 1% better (32 mb dictionary thanks to the ht4 match finder)
  Fixed: wfindfirst on empty drives
. -di+% to display memory statistics on screen (accordingly, -di+$# no longer print it)
. lzma by default = lzma:64m:ht4
. -m4 now uses lzma:64m:ht4:mc16 instead of rep:64m+lzma:8m:bt4 (similarly to the change in -m3)
. a :h parameter was added to lzma, allowing the hash size to be changed (for ht4, by default dict/2) or
    the hash header size (for hc4/bt4, by default dict*2). By changing this parameter you can speed up the search
    or, conversely, slightly reduce the memory requirements
  lzma:ht4 allows a dictionary of up to 1gb!
  fixed: LZMA: p->SetDictionary in parse means that we can change the dictionary size during decompression!

15.05.2008

  Added an icon for .arc files
  --When reading the archive directory it skips optional extra fields and complains about mandatory ones
  Changed the storage format of the archive comment (UTF-8 instead of UCS-4) with support for reading old comments
  GUI: combo box of messages and warnings at the bottom of the window
  lzma:*ht7 - ignore parameters starting with an asterisk
  CHECK -> printf/msgbox (utf8->utf8/utf16/oem)
. SFX!!!
    do not start the next algorithm until output from the previous one appears
    the s-/sSFX_MODULE/s sfx- command
    command line - -dp/-o+/-o-
    Russian file names in the archive (mkdir/BuildPathTo/exists/displayname with unicode names)
    RTF in the comment
  linux: lzma -mt, sfx/unarc (todo: files >4gb)
  filetype detection: try mm first for $wav/$bmp  (to do: header checking for 6*24/small files?)
  fast exit when updated archive is locked / any input archive is corrupt
  Fixed bug in Tornado: incompatibility with old -m1 method!
  Installers with and without GTK

3.06.2008

  Win32: memory is allocated with MEM_TOP_DOWN, which makes it possible to avoid the fragmentation problem
  REP: buffer allocation moved out of the loop in order to avoid memory fragmentation
       write in 8 mb chunks even when there are no matches
  GUI
    when archiving the directory "FreeArc 0.50" it suggests the archive name "FreeArc 0.50.arc"
    sorting of language files (important only for linux)
    display real name of archive tested by -t
    messages: found N files, sorting filelist, analyzed N files...
    the infobox shows the archive name without the directory
    the width of the Add dialog is limited; automatic enlargement of the main window is disabled
    remembering the size and position of the main window
    "Extract here" in the .arc context menu; "FreeArc archive" added to Explorer "New" menu
    Selecting Unicode files/directories through the Select dialog
    Printing Unicode archive names in the title of the progress indicator window
    The time remaining until the command finishes was added to the window title
    Time spent paused and in prompts (overwrite/password/comment) is not counted
  Unarc/SFX
    "All OK\n" on successful completion of unarc/the console sfx
    Printing Help on a parameter error in the Windows SFX
    full support for Unicode arcname/outpath/filenames
.   use freearc.sfx instead of arc.sfx by default
.   sfx/winsfx options a-la winrar
  Improved filetype detection: more chunks for large files; at least 92% of $text/$compressed to use it
  Installer: PATH updating, deleting of all FreeArc/Gtk files, the Theme Changer and the themes themselves are included;
    readme -> readme+whatsnew
  ^Break->terminate when not running in file-manager mode. Hangs removed - direct exit
  tempfile is removed from the description of the compression chain
  Fixed a bug in the Delta algorithm
  Decompression now uses -ld75% by default
  Fixed freearc.addon (for Total Commander) - added support for files with spaces and for sfx archives
  FAR Plugin: added "Convert to SFX" command
  Linux: no longer requires ~/.FreeArc to work; GUI: fixed archive navigation & Unicode filenames support (UTF-8 realpath)
  --Windows: config files are also looked for in the user directory "Application Data\FreeArc"
. WinArc->FreeArc

23.06.2008

  GUI
    The progress indicator window does not stretch on a long filename processed; ProgressWindowSize in freearc.ini
    For operations on existing archives (ch,d,x...) "--noarcext" is added to the command line
    "Abort operation?" - localization and pause_timing
    Progress indicator now shows current compressed/total compressed sizes
    Sorting by clicking on a column header
    Sorting by Modified now separates files and directories
    Saving the size/position of dialogs and the order/width/sorting of columns in the file manager
    Language selected via compact table
    "Change drive" button
    Single-click on the empty space at right clears selection, double-click selects all files
    FAR-like Shift+ Shift- Ctrl+ Ctrl- Alt+ Alt- keys for marking files
    Increased the number of commands in the menu, hot-key support implemented
    Archive comment editing in ArcInfo dialog
    You can start to search file by starting type its name; *? wildcards are also supported
    Shortcut keys for menu commands
    Windows: native Open/Save dialogs
    Windows: locale-specific date/time format
    Support for used-defined menu/toolbar (just copy freearc.menu.example to freearc.menu and edit it as you wish)
    Large icons in toolbar
    Setting to disable captions in toolbar buttons
    Filters (*.arc;*.exe) in Open Archive/Select output archive dialogs
    Uses long command names in menu and short names in toolbar
    "New" language simplifies adding new translations
    "Check for updates" commands runs in background
    "Watch for new versions via Internet" checkbox in Settings dialog automatically checks for updates every day
    When checking for updates, UserID/Version/language/OS/RAM/maxblock/cores reported to freearc.org
    UserID is random number saved to config file and backuped to registry
    Run any program/document just by double-clicking (or Enter) on it
    Added "Create" button to outdir select dialog
    Disabled move-to-top for options like Compression, Encryption... (flagged by "*Last" tag in freearc.history)
    Added Recover Archive command
    Stage indicators with percents for RR operations (add, check, repair) with immediate stopping when user aborted operation
    Modified default compression/RR options
    Added Info page to Settings dialog, now displaying only Max free memblock
    Saves Maximized window state
    Compression tab: show speeds for 3GHz Core2Duo
    Improved freearc.history, i.e. predefined sets of options
    Added icon for Arc.exe and FreeArc.exe
    Skin changer: more skins
    Made toolbar buttons non-homogeneous
    GTK updated to version 2.16
  Linux: "chmod +x" when creating an sfx archive, "chmod -x" with -sfx-
  --CHECK - in FreeArc it results in a longjmp call and the return of FREEARC_ERRCODE_GENERAL from the (de)compression procedure
. Encoding selection: -sct for the terminal; -sci for the logfile; -scf for file names on disk (under Linux)
. Windows: unicode (utf-16) cmdline
  Split into modules: Files => Charsets
  FAR/TC: copying/moving empty directories into/out of an archive; plugins fixed
  "-ms-": disable fast compression of already compressed files
  Initial CLS support (external compressors in cls-*.dll)
. -m1xx..-m4xx modes provides very fast decompression (require 1gb of free memory to decompress!)
  Tornado 0.5:
.   better compression in -m1; faster -m3xt; modified -m1xx..-m4xx definitions; :t :ah :al
    tor:7:c1 .. tor:11:c3; support for :l not power of 2
    checks at decoding in order to prevent segfaults; fixed one bug in decoder
  GRZip multithreading! Temporary: allow blocks up to 32 mb in order to improve -m2t performance
  Prints CPU time and Real time for operation
  Installer SFX (extracts into tempdir, runs setup.exe and then deletes extracted files)
  -m2b made faster on modern cpus; -m3b - fixed speed loss on multi-core cpus & 1% better compression
  "lt" command: technical archive listing
  100% GUI translation and translation of error/warning messages in CLI
  --language=LANGFILE option: localization for console version! If present in arc.ini, it should point to the same file as freearc.ini
  -m2/-m3/-m4 now use 96mb dictionary by default! *r modes are removed, bcj moved after rep
  arc.ini: large 45kb variant with support of External Compressors PowerPack
  (De)compression speed improved by 10% by using facompress.dll
  -rr0.1%, -rr0.01% options support (translated to -rr0*4kb, -rr0*64kb)
  Set arc.exe and freearc.exe icons to FreeArc.ico
  External compressors/tempfile method:
    Checks I/O for errors
    Copies data intact when compression program returned non-zero error code
    Raises error when decompression program returned non-zero error code
    Prints size of data being processed as 64-bit point-delimited value: "1.234.567 bytes" and "Error level=x" after operation
    Ability to use spaces in [External compressor] header:  [External compressor: ccm123, ccmx123, ccm125, ccmx125]
.   [External compressor: ccm, ccmx]
      packcmd=ppmonstr {-option } ...
      packcmd=ppmonstr {options} ...
      unpackcmd=thor e{option} ...
      default=-o10 -m48
  Fixed bugs:
    In Delta filter (leading to crashes)
    All encoding problems solved
    Showing "increase +RTS -Ksize" on multi-gigabyte files
.   Now it reports "Bad password" with errcode 21 when archive/file cannot be decrypted using supplied password
    Program was terminating when trying to detect filetype of locked file (such as pagefile.swp)
    -sfx --noarcext: did not change the archive extension
    Logfile: hide passwords used in cmdline/cfgfile
             strip too long arguments and too long argument list down to 100 chars/elements
    Fixed error handling (it was always printed "Program terminated" instead of real error message)
    Filetype detection: it was failing on pure English texts
    Fixed bug in SetWindowTitle
    Commands ch/k/c/rr/s shouldn't have additional arguments
    Fixed RR add/check/recover execution
    Testing archive with recovery record ("undefined UI::ref_ui_state")
    Computing estimated time and speed takes into account duration of last stage rather than whole operation
    Unarc crashed on errors instead of displaying error message
    Unarc didn't deleted partially decompressed files
  Fixed GUI bugs:
    First file in directory was selected after any operation if nothing was selected before
    ArcInfo on empty.arc or non-archive was terminating program
    DoubleClick/Enter on non-archives was terminated program, but now executes them
    Freezing on chdir "c:"
    Freezing when going into archive with encrypted directory  (entering archives with an encrypted directory)
    Freezing after archive operation was cancelled/returned an error
    Lack of scrolling for archive comment
    Freezing on selecting two files and pressing Up
    ArcInfo: fixed calculation of number of files and directories in archive
    All dialogs are now modal
    Report to user and exit when we cannot open initial archive/directory
    Ensure single-threaded access to freearc.history
    Freezing when trying to open encrypted archives (password dialog should work in any thread)
    Freezing on Up from http://....arc
    Run directly from archive shown empty "directory"
    Some files were not shown when directory names contained space
    Extraction of all files with the same name as one file extracted from root dir of archive
    Use stock icons for Yes/No/OK/Cancel/Close buttons
    All toolbar buttons had the same width
    Some files inside archive were not shown when directory names contained space ("1C/", "1C 2/")
    'Open Archive' button doesn't worked when you're currently in the root of any disk in filemanager
    When archive was converted to/from SFX, displayed archive name was not changed
    Encrypted.arc - Edit comment - Cancel -- resulted in program crash
    Removed hack that reversed column sort indicators due to bug in old GTK versions
    Linux: fixed warnings printed by Archive Open dialog


  aARC_VERSION = "0.51", aARCHIVE_VERSION = make4byte 0 0 5 1
=== To-do list ====================================================================================

  HCAR

  take whatsnew from the wiki

  do not close the program on Exit if archiving is in progress
  load icons for all commands from png/ico files


  -ap
    FAR: -apdir1 + dir1/dir2/file in filelist (rar-style?)
  packing/unpacking of empty directories
    Searching for files on disk
      1. check all non-regexp names to see whether they are directories,
           complaining about the ones not found when recursion is off (-r- or -r0);
           a directory name means archive the directory itself and everything inside it
      2. search recursively among the remaining names
           - all of them with -r
           - only regexps with -r0
           - none with -r-
      3. non-recursive search of non-regexp names - use stat
    dir/ -> dir/* (a-la RAR)
    Do not forget to change the FileManager accordingly (addCmdFiles)

    Selecting and excluding whole directories: Stats, Stats/, Stats/*, Stats\*, -xStats...
      copying empty directories into/from an archive; moving non-empty directories into an archive (the directory itself is not deleted)
        arc l a dirname dirname/*  =>  does not print the directory dirname
      excluding a whole directory from archiving works only in the form "-xhelp/*"
    -xDIRECTORY ?
    filetype detection: check for MM on large files
    Priorityio.doc
  bugs
    *Last in the history - gets localized right after the first use
    update GUI docs
    sfx - support and automatic use of the tempfile method
    dict bug: arc a aa cls.zip -mprecomp+6pt -t
      bcj+dict creates corrupted archives
    what happens if you pack onto a CD, i.e. the file is on a disk that cannot be written to, or in a folder where writing is forbidden
    arc a a d:\ - duplicate files
    problems remain: deleting empty directories from an archive (TC), copying empty directories from
      an archive (FAR), copying into/from directories inside an archive (-ap)
    separate & no files selected
    "arc a a ../file" - ".." is not removed from the name; likewise during extraction
    arc a dir/ - creates the archive dir/.arc; archive names ending in :/\ or containing : must be cut off
    *.* and makefile.* should include makefile
      gui: so should it select folders by *.* or not?
    updating an archive containing xxx: if xxx could not be opened from disk, the file will disappear from the archive entirely
    tta: float32 badly compressed (vs wavpack), int32 isn't compressed at all
    +RTS -S - processed by arc.exe itself too :)
    forbid all operations that lead to the creation of an invalid sfx
    after "no such file or directory" (selection from the history) - wrong name in the path field
    descript.ion for all files in package would be nice to see
  GUI
    use "arc:dir" instead of "arc/dir" so as not to have problems splitting the path into the archive and the directory inside it
    translation of tooltips:
      1004
      Englist text
      second line
      =
      Russian text
      second line
      (an empty line at the end)
    translation of error messages (errormsg) and internationalization of the cmdline version
  GRZip multithreading!
    ?fixing SyncQueue
    limiting memory
  -ilog a-la RAR (disable Lua support)
  arc.ini: there needs to be a way to specify non-solid mode for an external compressor



  GUI: center the progress indicator relative to the program window rather than the whole screen
       TC: when extracting FreeArc.exe, no-url\FreeArc.exe is extracted as well
         arc x a FreeArc.exe  -  rar extracts only one file (*.exe extracts all files)

  mm/tta/ppmd/lzp/dict: BigAlloc
  URL support: dynamic dlls in order to cutoff rep:1g errors
  testmem and correct method before starting single_compress (?? - compression threads running
    in parallel may run out of memory)
  for a complete implementation of multithreading we need to:
    make all 11 algorithms multithreaded. only 10 left to go :)
    make it possible for separate solid blocks to be compressed in parallel
    provide data buffering between compression algorithms (f.e. dict+lzp+grzip)

  NEW GUI:
    1.1 turn freearc into a dll
    1.2 implement in it the return of the list of files in the archive with an API yet to be chosen
    1.3 the (de)compression itself with all the callbacks
    2. you start working on the gui and in parallel I work on adding support for 7z.dll

  documentation update
    Filetype detection
        focused on identifying $text and $compressed files (for -m2..-m4)
        disabled in -m1 and when an explicit compression method is used (in the general case - when
           $text and $compressed are absent from the file type list - what would it be good for then? :)
        -ma+, -ma-, -ma0..9 - enable/disable/level of autodetection
    SFX
      use freearc.sfx instead of arc.sfx by default
      sfx/winsfx options a-la winrar
      the s-/sSFX_MODULE/s sfx- command
      command line - -dp/-o+/-o-
      Russian file names in the archive (mkdir/BuildPathTo/exists/displayname with unicode names)
      RTF in the comment
      Installer
    External compressor options ({options}...)
    CLS
    a description of how compression can be increased by raising or removing the -ld limit
    description of ht4

  Russian documentation (4gb)
  English documentation (future plans)
  linux screenshots


  File Manager
    displaying Tatar and other characters - Egor's recipe
    methodDescr - clear it in custom compr. mode
    directories in the archive
      refresh - re-read the archive
      closeFMArc before navigating and before refresh
    closing the progress dialog must not lead to closing the program
      +terminated -> fail in extract
      +shutdown checks terminated and issues Ctrl-Break
      +terminated -> fail in compress (terminate all decompression threads)
        quasiread each 256kb in dict/rep/delta/lzp
      progress indicator: max(write, quasiwrite), min(read, quasiread)

    problems
      Please, Bulat, can be PPMonstr's output limited to one row?
      several decryption passwords/keyfiles
      "all ok" after every command
        if you test an archiv, there is no message given back, there should be some kind of "Archive OK" at the end of the test
      when computing cmem/dmem use parseCmdline, or at least the full code from it
      when extracting/testing an archive without passwords they are still requested in the dialog
      the "ch" command must apply repacking/re-encryption only to the files specified in the cmdline
      tatarcha: the title Add 1 file, Extract 1 archive..
      add tempfile at the beginning of the compression method when there is not enough memory (ppmd:2gb repacking)
      Settings: Compression profile: it can be saved but not deleted
      FreeArc does not react to close from the taskbar when some window, for example Settings, is open.
      If a window is open in FreeArc (for example Settings), then clicking FreeArc in the taskbar minimizes the main FreeArc window, while the Settings window remains; on a second click the main window is not restored.
      ColumnOrder - remove the Russian translation
      Maximized window - the normal size is not remembered
      Manual implementation of Ctrl-XCV so that it works even in the Russian keyboard mode
      Figure out what changes any command will bring and display them in advance. As an example: after 'Convert to SFX' inside archive Path field isn't updated
      When keyfile is "", pressing "..." opens root dir/program dir rather than current directory
      encrypted - enter - cancel - the file gets launched (a system of explicit error typing is needed, with launching only when "this is not an archive")
      Dialogs do not inherit the "Always on top" property
      Removing the password from the headers:
        -hp-- - keep the directory encrypted
        -hp-  - remove encryption
        -hpxx - enable encryption
    an additional progressbar for big files, like in Winrar
    progress indicator - update it while copying solid blocks (archiveCopyData)
    DoubleClick->Run inside archive
    there is no drive list at the top level, there is no ".."
    ctrl-f4 - sort by extension
    option 'Do not show hidden files and folders' (like in Explorer)
    Performance problems
      Archive directory: faster removeDups, keepOnlyFirstOn, buildTree
      slow opening/closing of the Add dialog (repeated reading from / writing to the history)
      use arrays instead of lists of files for reasonable running time
    saving the -m -s -ds options in the archive
    in the main window, next to the "save" button, a "browse" button is just begging to be added
    select either with insert or with space (winrar clears the selection only of the files that were marked with the mouse)
    tooltips: show the tooltip not only on the label but also on the control it belongs to: combobox, label...
    tooltips in the progress dialog explaining the meaning of each number
    merge arc.english.txt and the locale so that English tooltips are shown when ours are missing
    Settings dialog
      "Restore standard profiles" button
      Date format selection
      Windows: [x] Associate FreeArc with .arc files
      [x] don't keep history
      Import 7-zip lang. file
    Recover, including "Original:"
    the ability to save something like a "project" as in Nero, i.e. a list of files + all settings
    View, Queue/Apply commands; d&d support
    freearc a/x --dialog - open the add/extract dialog (for Total Commander, for example)
    build the command at the bottom of the extract/compress/modify dialogs and execute it (so that the user can edit it)
    a "Detach operation" checkbox in all operation dialogs, launching a separate copy of WinArc to perform the operation!
    "Detach" FM button and appropriate syntax: "freearc c:\dir file1 file2 file3 /select file1"
    open the log file when the program starts
    extraction dialog
      [x] including subdirectories
      Selection tab: -ta/-tb/-tn/-to/-sl/-sm
    tree/arcinfo/... on the left
    unarc-based archive open (for arc.exe too) will make it possible to process large archives faster and using less memory
    ArcInfo: compression algorithms, created by FreeArc version xx, RR: "5*512 bytes",
    Cancel dialog: Cancel file/solid block/archive/operation/group of operations/All
    displaying/editing the operation queue in the progress dialog
    automatic refresh of the file list every second/when the directory is updated
    use the already opened archive to speed up view/test/extract operations
    linux: a different button order in the dialog (HButtonBox?)
    adding files
      compression settings
        dict/mem
        enable wav/bmp/text / rep/delta/exe... (all enabled by default)
      data protection
        test archive before operation [0..3]
        size of recovery record
        test archive after operation [0..3]
      place archive to the folder named as: current user, month, weekday, monthday
      include system/hidden files
    deleting files
      recursive deletion for directories (borrow removeDirectoryRecursive)

  lzma
    split son into two parts - first allocate the largest available block, then the remainder
    regulation of the hash size and sizeReserv
    CLZInWindow - reduce sizeReserv to dictSize/8 with BT and dict=81..95mb, 161..191mb, 321mb...
    lzma:32m:ht4 - if the file is small, then when reducing the dictionary do not reduce (and better yet
                   even increase) the hash table: 32m+16m -> 8m+32m (up to 4*dictsize)
    the fact that the hash is allocated first and then 1.25*dict was done for the old matchfinders,
      where it ensured allocation from larger blocks to smaller ones. I will rework it
  Compression
    -m3t = dict[:p?] + lzp:32m:32:h18:a + grzip:m3:l, where lzp:a is a grzip-friendly modification of the algorithm
        (lengths in the text rather than at the end) with multiplication for hashing
    grzip: bwt sorting from dark
    lzp: p=HTable[k] - do not perform it when c!=LZP_MATCH_FLAG
    lzp: speed up hash computation, skip 4-8 bytes at a time when nothing has been found for a long while (+tor)
    $bmp/$wav - group solid block by 64kb?
    to solve the -ld1gb problem - allocate memory in small blocks (during decompression in lzma, rep, ppmd, tor)
    tor:5 -> delta+tor:5:t0 ?  apparently it only makes sense on fast machines
  Compressor: it is best to use external compressors for the heavy algorithms. Even now you can take compressor.exe and use it that way. In the future, however,
    the compressor will be compatible in data format with the internal algorithms so that, for example, you will be able to pack with the external lzma using a 1 gig dictionary and unpack with the internal one
    naturally, there will be 32- and 64-bit versions, and they will be compiled with ICC, which will increase the speed by 10-20%
    fa will automatically figure out whether it has enough memory available to perform the operation or whether it is better to hand it over to an external program
    fa will learn to work with external compressors via stdin/stdout, which will make them as convenient to use as the internal algorithms - no writing of intermediate data to disk, an up-to-date progress indicator, etc.
  Bugs
    -p=... - "=" may be part of the password.
    clarify rep:SetCompressionMem; when limiting lzma/rep it is possible to use more precise
      dictionary values (and to reduce all the other buffers while simultaneously increasing the search depth)
    lzp:2200m - forbid dictionaries >=2gb; ppmd:2560 - compare against MAX_ORDER
    -ds:r - also pull together files with differing extensions
    translate error messages for errors in arc.ini/cmdline
    write the true name of arc/freearc.exe to the logfile and put parameters containing spaces in quotes
    checkingCtrlBreak in copy_data/eat_data
    rep:4gb == rep:0gb
    BigAlloc: smart alloc algorithm (alloc smallest block that's still enough for the data?)
    perform clearAll in uiStartArchive/uiDoneArchive
    pause the progress indicator when external programs are called
  SFX
    I can hardly wait for the GUI SFX module (for Windows) to get a silent-install switch with the ability
      to specify a default path, and also, as in WinRAR, the feature of launching an application after extraction
    Installer: in general it should be like in other installers - a check (test) is done before extraction,
      or rather, the check happens right after launch, and only then the install script is executed.
    knowing what the minimum memory required for decompression is is useful if more than 192mb
      or more than 3/4 of physical RAM is needed
    wopen: win95 compatibility?
    tempfile between algorithms that are too heavy?
  Features
    -ver (rar-compatible)
    use fiGroup for sort
    -idp (+i2 - print the names of all files being compressed)
    allow an external compressor to create a whole directory of files and compress them as ordinary files
    // - comments in listfiles
    "..." in listfiles - this is the usual way to write names containing spaces
    availPhys - use it to choose the size of the read-ahead buffer in the fast modes; and the number of solid blocks
      being compressed simultaneously
    768mb RAM -> rep:384mb:h128mb
    arc j 1 2 -apdir; j -d; j -ac
    arc x -tk1/2 (set to archive time/current time)
    On a write error (not enough space on disk?) show a prompt to the user with a
      Retry option, which will let him free up space before continuing
    Exclude list: I'm thinking on a text-box in "add with options" where file filters could get
      entered, as well as on a global exclude filter list and finally on a folder based
      configuration (text file or registry). The filter should allow wildcards and maybe directories
  Lua
    a predicate for filtering the files being compressed
  GUI
    an error message window; a warnings sub-window
    use processing time for speed/estimated time
    bytes - print how much has really been processed, without adding 10% for read
    printing the indicator can eat up all the CPU time
    Background - lower the priority, minimize to tray
    testing an archive with RR - crash! "ERROR: undefined CUI::ref_ui_state"
    checkbox "Close on exit"; "Remaining time", "Estimated endtime"
    the amount of RAM used by the current algorithm
    take into account that cbytes may lag behind b (a sneaky trick!)
    add the name of the file being read to UI_Read/num=0 (or set up a separate queue),
      which will allow processed files/current file to be displayed more accurately in the CUI and GUI
    support for -m5p (external compressors)
    It would also be nice to see:
      4. Changing the priority.
      5. Additional actions after packing finishes (Shut down\Reboot\Sleep\Standby\...)
      6. If the chain is executed sequentially, then separate statistics probably need to be displayed...
    And also:
      1. Interface:
      1.1. Adding files as in WinRK, i.e. the ability to drop them in, but in FreeArc with the ability to choose which method to pack with. Possibly split the files into groups - by dragging onto an icon or an area, for example txt\binary\..
      1.2. The ability to choose which interface to use, everyone has different preferences, some people want WinRAR, etc.
      2. Explorer integration - so that there is a way to choose the name of the context menu item - either at installation time or in the program settings.
  "-psome password" in arc.ini (options with spaces in quotes)
  '-ba' -> '-ra': searching for intact descriptors
  Archive protection and recovery (the 'r/rr' commands, the '-rr' option)
    if the RR is missing/faulty, download it from the specified URL and then recover using it
  Bugs:
    real-time progress indicator during decompression - 90% of the volume of the source data
      + 10% of the output + the number of files
    Russian letters in a password entered from the keyboard/in a dialog?
    getFileInfo: attr=0; this means it is impossible to store them in the archive
    "-sc": utf-32, bom in unParseFile, le/be recognition in parseFile, terminal/logfile
    unc names
    "arc a a c:* -ep3"; "arc a a -m1 \* -ep3"
  unixify long option names (keep old names for compatibility?)
  External: deletion of temporary files on ^Break
  dict: reduce the init time (1/15 sec) for small files?


0.41a:
  update the documentation and translate it into English
  Options in the compression method configuration:
    jpg=-s-
    m0=-ds
    m5=-s64m
    -s=8m..2g for -m1..m9, mm- $jpg too?
  4gb: http://msdn2.microsoft.com/en-us/library/aa366521.aspx
  ccm in pipe mode


0.41a:
  Encryption: ch - implement decryption+encryption when copying a block in ArcCreate
  the read speed when testing an archive is only 10mb/s (see Stats/read10mbs) 0.35->0.36, buffering?
  -m2: 5% writing to disk, 3% CRC speedup, 10% kadach MF, 7% huffman (-0.3% compression), 10% use repdist for MM detection

0.41a:
  External:
    create our own files in unique subdirectories of the TEMP directory
    checking the return code of external compressors?
  Encryption:
    -p- - decrypt (-p-- - leave as is) ?
    -p? -hp? - two separate passwords?
    moving a keyfile/password to the front of the list on successful decryption
    reuse previous salt values
    print a warning when the archive contains files compressed with a different password
    choose numIterations according to the machine's performance so that key generation takes 0.1 sec
  Unix:
    MidnightCommander VFS support
    GetMaxMemToAlloc(LargestMemoryBlock) / GetAvailablePhysicalMemory
    symlinks, file owner/group/permissions, syslog instead of logfile?, OS signals handling
    SetConsoleTitle (tigetstr/putp), setpriority (PRIO_PROCESS, 0, xxx) or nice (+-xxx)
  Windows
    support for saving/restoring attributes, access rights (VERY IMPORTANT), file times, symbolic links
  Archive protection and recovery (the 'r/rr' commands, the '-rr' option)
    split into parts with different periods
    add information about the location of the damaged data to the messages about archive failures
    the 'r' command could test the archive and print a list of damaged solid blocks
    If there is a per-sector CRC in the archive:
      1. Extract the archive skipping the corrupted solid blocks.
      2. Extract the corrupted solid blocks up to the damaged region.
    Manual recovery from the data downloaded with --save-bad-ranges is missing
    "--original?" - files/description/input from the keyboard
  FAR plugin: smarter Encrypted detection (:s:v), GetDecompressionMem - link all modules in FREEARC_INFORMATION_ONLY mode
  "rn" command, --sync=-u -as, -dp*, -si[name[+size]]/-so, -ssc[-], -ssw, -r0
  (De)archiving millions of files: a file-search thread and packing in batches of 100k files; extraction of one DIRBLOCK at a time
    read the file lists for -ac/-d from dirblocks
    store the file data in a ByteString - this will reduce the RAM requirements tenfold
  Progress indicator
    advance while the dict/lzp preprocessors and the multi-pass algorithms (dict, grzip) are working
      10%/n for each compressor in the list except the last one
    do not count directories/empty files, since they are compressed instantly
    quasiwrite4rep: -50kb read, 250kb quasiwrite
      read -50kb
      write 250kb
      read 0
      write -250kb
      write ....
      read 50kb - before the next read operation
  Use a dictionary of up to 48/96 mb for -m3/-m4, depending on the amount of free memory
     -lc = max(memory required for decompression, FreeMem())
     :mc - increase/decrease automatically when memory is increased/limited
     ppmd/pmm:o - increase/decrease automatically when memory is increased/limited
  Increase the read-ahead buffer for the fast compression modes (m1/m2/m3?); async write
  -{m3,m4,m5,m6}, -m{3,4,5,6}   -m1=... (a-la 7-zip)
  "arc command/option" to get help on this command/option
  [External compressor:ccm]
    Specifying the amount of memory for each set of default options, which will make it possible to choose a compression mode suitable for the given amount of RAM:
      default_options:256m=5
      default_options:384m/128m=6x
    mem=:m*mb
    New ideas:
      numopt = o
      numopts = --fastest --fast --normal --good --best
      memopt = m
      memopt:mb = m
      packcmd:stdio = ...
      packcmd:stdin = ...
      packcmd:stdout = ...
  "durilca e -m256 -o12 -t2" would become "durilca:256m:12:t2"
    ofr:1 => ofr:fastest, ofr:5 => ofr:best
  Compressor-in-a-dll: fac-lzma.dll, fac-ppmd.dll...
    on-demand autoloading from Internet with digital signature
  Open a hundred files, then read/write them all, then close them all and move on to the next batch!
    speed up the extraction of a large number of small files (add a separate thread that works with the files)
    --crconly 100 files/sec: Compressed 144.557 files. Compression time Total 1414.34 secs


Main improvements in 0.42
  Compression
    lpaq/ppmm
    utf8/16 to codepage switching converter (for text and binary files)
    bcj2 (in rep?), disasm
    bcj/bcj2 in REP, eliminate the causes of the compression loss when it is used (repeat overlaps a nearby string)
    REP: several output streams, built-in bcj/bcj2/disasm
      ? embed the exe preprocessor into REP (which has access to the true addresses of CALL instructions), which would make it possible not to do exe preprocessing on repeated fragments, as is done now
    bmp compression (CALIC/SLPRICE/JPEG-LS/BMF)
    xml-wrt & Grabowsky tricks in dict
    segmentation by the structure of *.exe, *.doc
    compress the output of tta/mm with logarithmic coding and hand the logarithms to grzip/ppmd/lzma
    automatic search over compression parameters (in particular, ppmd order)
    The compression ratio can be improved considerably if a file type recognition mechanism is built into the archiver.
      A binary - so we compress it this way, a web page - that way, a log - we change the parameters, and so on.
  use ICC to compile CompressionLib into a DLL (http://www.haskell.org/haskellwiki/FFI_cook_book)
  speed up dict, lzp, add our own MFs to lzma; dict for binary files?
  one-file preprocessors: exe, precomp, mm, jpg
  async -m1 mode
  Multi-volume: when the data being written goes beyond the volume size limit
    1. create a description of the solid block with the current file list
    2. build and compress in RAM the directory block with its descriptor (total length - N)
    3. replace the last N bytes in the archive with the directory
    4. open a new volume and flush into it the N bytes that did not fit into the previous one
    5. continue the packing process
    think through the interaction with the writing of the Recovery Record
    extraction of several volumes
  Warnings
    warnings get overwritten.
    remove the blank line before a warning, add spaces if there is a progress indicator
    line-by-line writing and the process number in the logfile
    after a prompt to the user there is no need to move to a new line once more
    arccomment - also a wrong line break
  lzma: fastest, fast, normal, max, ultra; dict:p - display on screen in the same form


Main improvements in 0.43
  -u=new.arc, -mt for -m2/-m3
  mm: YFrFb, YC0Cg and so on
  *.zip both in $compressed and in $precomp ?  "$precomp $compressed\n*.zip..."
  collecting statistics about files with unfamiliar extensions in order to choose the compression algorithm.
    mmdet for $wav/$bmp
  mmdet: check the redundancy of an rle+ari model instead of a pure order-0 one?
  Awe: automatic renaming of the files being extracted if the current
           directory already contains files with the same name (the -or switch in rar).
       7zip has the ability to create so-called "incremental backups", it would be great to build
           something similar into FreeArc. That is, new/updated files are written not into the "old" archive
           but into a new separate file.
       Incremental backups. FreeArc takes as parameters an archive and a set of files and creates
           a new archive with new / changed files and a reference to the previous archive for the
           rest. Something like DAR.
  an "output archive name" option
    a "virtual input archives" option - with 'u' only the files that are not in them are added
    take all input and output archives into account when excluding the archive itself from the archiving list (overwrite_f)
  Rename[Extracted/Existing] / AutoRename[Extracted/Existing][Unicode/Latin1] / Help
  Print arccomment on ALL operations (including 'r' and 'cw')
  add an "extended masks" option for using [] in file masks
  compressionLimitMemoryUsage and calcMem contain special cases for DICT and other algorithms,
    which may be wrong when they change and when computing the memory for decompression (compressionLimitMemoryUsage freearcGetDecompressionMem)
Error handling
  all file operations must contain "catch registerError"
  correct return codes in rep/dict/lzp/...
  handle CRC error detection adequately
  option "turn warnings into errors" - bail out on the very first warning
"m1$rgb = " means cancelling the subsequent "m1$rgb = ..." lines
compression methods help?
? m2xb: tor:l4:x:h16m:d8m:p2:s - unlike -m2b, a smaller buffer and a larger hash
Archive protection and recovery (the 'r/rr' commands, the '-rr' option)
  recovery of non-consecutive damaged sectors
    sectors/1.5 - look for a smaller prime number - sectors/4.5 - look again - ...
    1. Determine which of the corrupted sectors are recoverable
         using each recovery record (do not recover the same sector twice!)
    2. Recover them, correcting the other recovery records along the way
    3. Cross them off the list of corrupted ones and repeat the process until the list
       is empty or no more sectors can be recovered
  searching for recovery blocks
  reading/writing in 256 kb chunks
  sort out the double footer block
  huge archives - find more efficient solutions (several recovery blocks?)
  think through reliability issues on failures at the end of the archive even more thoroughly
    split the data into chunks of, say, 3.5 kb of recovery sectors + 500 bytes of CRC sectors + 12 bytes self-descriptor + CRC
    distribute the recovery records evenly across the archive (between solid blocks)
correct ordering when updating archives (key_func for 'c'/'i')
benchmarking command
sort out memory usage, refine LimitMem, LimitBlockSize (= the data block size) and similar things
  limitDict must limit the dictionary of the first method and +100 for the subsequent methods in the chain..
  do a preliminary tuning of the compression method and determine the read-ahead cache size
    after the list of files to be packed has been built. Collect in Compress.hs the code for tuning
    the compression methods/cache size from all four places - Cmdline/arcCreate/splitIntoSolidBlocks/createSolivBlock
  if only files of a single type are being archived, then remove all the other methods from the printed list of compression methods
-s64m should mean: the data is split into 64m chunks and the directories stay with them (will this make read-ahead impossible?)
Archive format extensions
  store in each service block (or in its descriptor?) the distance to the previous service block - this will greatly simplify archive recovery
  store lists of Ints in the format 0/1/2/4/8, where 0 is the current variable-length format (write the format before the list itself)
  antifiles and antidirectories
  additional attributes (unix/win), 3 timestamps with high precision
  split files (one chunk of a file in one solid block, the second in another, and so on)
  split solid blocks (for multi-volume archives)
  block descriptor - store its length at the beginning, this will simplify archive recovery
     and will make it possible in the future to extend it with new capabilities, in particular with a key
     for decrypting/verifying the AV of the block itself
  attribute name (String) + size + general attributes (COPY/OMIT/STOP+PERFILE/ONCE)
     + type (Number/String/Bytes)
     + representation type (0/1/2/4/8 for integers, S for strings, B for bits)
     + default value for COPY attributes
  extend the archive signature to 8 bytes, which must be at the very beginning and the very end of the archive
FreeArc 0.80 - rework of the compression methods:
  write "*" before those compression algorithm parameters that do not affect decompression: lzma:8m:*h32m:*ht4
  LZP: two output streams, sliding window, caching MF, multiply hashing, 2 entries?
    hash = lohi(word*123456789), where lohi adds together the high and low parts of a 64-bit word
    the high bits in the hash table store bits of the hash so as not to go to memory needlessly on a collision
    a circular buffer, output the "dictionary" before the data, process one megabyte at a time
    lzp for ppm* - a gradual reduction of MinLen as the match distance grows.
    preprocessing for ppmd - reduce :h?
    h13 => h15 in the final program?
  tempfile:8mb - buffer 8mb before starting to write to disk
  external:80%
  ppmd:r0 effectively means solid blocks inside the ppmd data. this should be exploited :)  -s8m?
  dict/lzp: use a minimum of memory during decompression
  -m3=-m3r: lzp:h13, dict:p - distribute the symbols used evenly
  rep:h32m (currently only rep:h23), automatically use hash=dict/2 when :h is absent
-m5$iso, -m.nrg.iso=m5$iso, -m.nrg.iso=$iso, -m.nrg.iso=lzma, -m$iso.nrg.iso, -m$iso:disk.iso
  per-file preprocessors, including external ones
  per-file preprocessing will make it possible to put exe and lib files into a single solid block
  different solid settings for different file types and compression levels?
Encrypt/Decrypt/Convert from zip Archive
'lt' - list technical
automatically delete the archive files after successful extraction
option to wipe files instead of deleting
compression
  -mb for lzp/dict and other preprocessors
  the solid block size must match the first algorithm in the chain if that is dict or lzp with a large block size
    in that case the amount of memory required for packing/unpacking should be computed as max rather than sum?
  the solid block size must be an upper bound (<=64mb) rather than a lower bound as it is now
joomla, IPB, smf - forumer
for block algorithms (including lzp preprocessing) it is useful to choose the first block so that it contains as few files as possible (to speed up reading it from disk)
splitting by file subtypes in splitBy (all examples are for an 8 mb solid block)
  1. the group really should denote a group number (for example, *hs - all in one group $$haskell)
  2. if a group has at least 2 mb - send it as a separate solid block
  3. if the group is larger - split it into solid blocks of ~6 mb, grouping directories at as high a level as possible.
  4. when reading the archive, sort the solid groups by the name of the first file in each of them
read_file: "reading" a file packed with a fake method - this is needed in order to update such archives
instead of decompressing a file, read it from disk if the files are considered identical (with -u, for example), thereby saving the decompression time
tor: 101..104  EOB/ari  huf/3 tables so as not to lose speed on +=10



to do:
  saving/restoring file attributes, selecting by them (and what should be done under Unix?)
  store NTFS named streams
  file comments, files.bbs
  archive optimization - copying with "-m0 --recompress", then with the desired compression options and sorting
    the files inside the archive. Likewise - adding to an archive with optimization
  Cmdline.hs - process the global config file first, then the personal one, then the FREEARC environment variable
    and finally the command line. Process options in order of their priority (from general to specific),
    and within a single priority - in order of appearance on the command line
  ByteStream to-do
  "arc a ..\{} -m{3 4 5}"
  an option "sort the file list in the order they are given on the command line/in listfiles"
  Intel dependencies:
    ByteStream
    LZP by Dima Shkarin - 32-bit only!
    REP
Compression improvements:
  Filters: jpeg, deflate decompression; exe-disasm
  instead of -ms: lzp - if the compression is at least 10%, then compress further with the normal algorithm, otherwise - storing
  the exe filter wastes time (and worsens compression) on non-executable files
  bcj2: analyze the statistics of the collected offsets before applying subtraction to them
  bcj: block-by-block (64kb) output with an extra flag byte - whether to apply bcj during decompression?
    all the offsets from the E8/E9 instructions can be collected at the end of the block
  lzp: split the block into small 64k blocks and decide whether to pack or not for each of them separately
  perl - looks like a suboptimal sort order
  import the new ppmd ?
  lzma: _flushPos - flush the data and update the statistics every 256k rather than every 8 mb :)
  dict: speed up compression, improve interaction with bwt/ppmd (use only the original symbols or a modified 16-bit bwt/ppmd) and use it in -m3
        an optimizing pass could solve the problem of compressing binary files
  tta: signature, non-diff modes
    compression parameters in header of *each* block (which would allow them to be changed from block to block)
    rice & arith encoding, do not reset the coding tables before the next block
Compression management improvements:
  collecting statistics about files with unfamiliar extensions in order to choose the compression algorithm.
    writing this information into the user's config file.
  Support for BCJ2-like algorithms (will improve LZP, REP, DICT, DELTA)
  Improvement of the solid block size selection algorithm taking into account large files at the boundary
    the conditions >=0.5bs, <=1.5bs - make them strict for both GroupBySize and GroupByBlockSize
  when splitting into solid blocks, take into account the "closeness" of files by type/subtype(C,Haskell,HTML...)/extension/the first three letters of the name/path
  mNxb/mNxt - use different lzma settings for texts and non-texts?
  selecting by groups and subgroups on the command line - "arc a a $text -x$$haskell"
  refactoring of comressionLib - separate the timing from the pure (de)compression
    introduce a universal function for querying/setting the parameters of a method
  DESIGN OF THE COMPRESSION STREAM:
    huftest              check the probability distribution of the symbols
      copy                 blocks with a uniform distribution - just copy
      multimedia           compress multimedia data with special compressors
      lzp                  process blocks with a text-like distribution with lzp
        lzma/ppmd?           service information.
        dict                 dictionary algorithm
          lzma/ppmd?           dictionary
          ppmd                 text
      exe-filter           process blocks with a non-text distribution with BCJ2 (E8/E9)
        copy                 service information.
        diff-filter          detection of tabular data
          copy (?)             service information.
          lzp                  lzp preprocessing
            lzma/ppmd?           service information.
            lzma                 main data
bugs:
  restoring summer/winter dates (extracting a summer archive in winter) leads to a one-hour shift (including in the Extractor)
  -s- does not work with a large number of files
problems:
  w95 file api support
  by default do not compress hidden/system files (only with the -?? option)
  -r0: "arc a archive dir dir\*.* -r0"
     r0: Enable recurse subdirectories only for wildcard names
minor unfinished items:
  Improve the use of hSetFileSize
  kill the decompression thread on failures in the main threads?
  -ag - convert the rar format into our internal one
  UTF8Z speed up: (de)serialization, joinDirFilename
  The 'm' command
    add a warning on a file deletion error?
    print the names of all deleted files with -i2?
    sort directories before deletion?
  ByteStream problems:
    Compressor==[String] - bad, since it is an instance of BufferData (restore write in archiveWriteDir)
      Compressor = String
    String support in the wildest form; UTF8Z.hs should be used
    separate the data types used in the program from the encoding used in Directory
    pos::Addr#, CPS style for readUnchecked (10.43->8.98 (String w/o reverse))

```
