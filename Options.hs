{-# LANGUAGE CPP #-}
---------------------------------------------------------------------------------------------------
---- Description of the commands and options supported by FreeArc.                             ----
---- Universal command-line parser.                                                            ----
---- (Lua scripting was removed; the luaLevel/luaEvent stubs remain as no-ops.)                ----
---------------------------------------------------------------------------------------------------
module Options where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Concurrent
import qualified GHC.Conc
import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List hiding (sortOn)
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression


-- |Description of the command being executed
data Command = Command {
    cmd_args                 :: ![String]           -- Full command text, split into words
  , cmd_additional_args      :: ![String]           -- Additional options read from the environment variable and the config file
  , cmd_name                 :: !String             -- Command name
  , cmd_arcspec              ::  String             -- Archive mask
  , cmd_arclist              ::  [FilePath]         --   Names of all archives found by this mask (possibly recursively)
  , cmd_arcname              ::  FilePath           --   Name of the archive being processed (a single command with wildcards in cmd_arcspec produces several commands, each with a concrete archive name)
  , cmd_archive_filter       :: (FileInfo -> Bool)  -- Predicate selecting files from existing archives
  , cmd_filespecs            :: ![String]           -- Specifications of the archives or files being added
  , cmd_added_arcnames       :: !(IO [FilePath])    --   Computation returning the names of the archives being added (the "j" command)
  , cmd_diskfiles            :: !(IO [FileInfo])    --   Computation returning the names of the files being added  (all other archive creation/update commands)
  , cmd_subcommand           :: !Bool               -- Subcommand? (for example, testing after archiving)
  , cmd_setup_command        :: !(IO ())            -- Actions to perform immediately before this command starts running (once for all archives)
                                                    -- Options:
  , opt_scan_subdirs         :: !Bool               --   search for files recursively?
  , opt_add_dir              :: !Bool               --   append the archive name to the name of the directory files are extracted into?
  , opt_add_exclude_path     :: !Int                --   strip the base directory name / keep the absolute path (when SEARCHING on disk for files to archive)
  , opt_dir_exclude_path     :: !Int                --   strip the base directory name / keep the absolute path (when reading the ARCHIVE DIRECTORY)
  , opt_arc_basedir          :: !String             --   base directory inside the archive
  , opt_disk_basedir         :: !String             --   base directory on disk
  , opt_group_dir            :: ![Grouping]         --   file grouping for the archive directory
  , opt_group_data           :: ![Grouping]         --   file grouping for the solid block
  , opt_data_compressor      :: !UserCompressor     --   compression methods for data
  , opt_dir_compressor       :: !Compressor         --   compression method for directory blocks
  , opt_autodetect           :: !Int                --   file type autodetection level (0..9)
  , opt_arccmt_file          :: !String             --   file the archive comment is read from (written to)
  , opt_arccmt_str           :: !String             --   .. or the comment itself, verbatim
  , opt_include_dirs         :: !(Maybe Bool)       --   include directories in processing? (Yes/No/Depends)
  , opt_indicator            :: !String             --   progress indicator type ("0" - none, "1" - default indicator, "2" - print the name of each processed file on its own line)
  , opt_display              :: !String             --   internal option describing which lines to print on screen
  , opt_overwrite            :: !(IORef String)     --   state of the file overwrite prompt ("a" - overwrite all, "s" - skip all, anything else - keep asking)
  , opt_sfx                  :: !String             --   name of the SFX module to attach to the archive ("-" - detach if already present, "--" - copy the existing one)
  , opt_keep_time            :: !Bool               --   preserve the archive mtime after updating its contents?
  , opt_time_to_last         :: !Bool               --   set the archive mtime to the mtime of the newest file in it?
  , opt_nodates              :: !Bool               --   don't store file timestamps in the archive (FreeArc 0.67, --nodates)
  , opt_create_in_workdir    :: !Bool               --   create the archive in the temporary directory, then move it (FreeArc 0.67)
  , opt_pause_before_exit    :: !String             --   pause before closing the window: on/off/on-warnings/on-error (FreeArc 0.67)
  , opt_queue                :: !Bool               --   serialize operations through an inter-process semaphore (FreeArc 0.67)
  , opt_volumes              :: ![FileSize]         --   volume sizes of a multi-volume archive (FreeArc 0.67)
  , opt_archive_type         :: !String             --   archive type (arc/zip/rar/...) (FreeArc 0.67, --type)
  , opt_shutdown             :: !Bool               --   shut down the computer once the operation completes (FreeArc 0.67, -ioff/--shutdown)
  , opt_arc_32bit_legacy     :: !Bool               --   read archives created by FreeArc x86 (masking Int/CTime down to 32 bits)
  , opt_keep_broken          :: !Bool               --   don't delete files extracted with errors?
  , opt_test                 :: !Bool               --   test the archive after compression?
  , opt_pretest              :: !Int                --   archive testing mode _before_ performing the operation (0 - none, 1 - recovery info only, 2 - recovery or full, 3 - full testing)
  , opt_lock_archive         :: !Bool               --   lock the archive being created against further changes?
  , opt_match_with           :: !(PackedFilePath -> FilePath)  -- when filtering, match masks against fpBasename or fpFullname
  , opt_append               :: !Bool               --   only append new files at the end of the archive?
  , opt_recompress           :: !Bool               --   force recompression of all files?
  , opt_keep_original        :: !Bool               --   don't recompress any file?
  , opt_noarcext             :: !Bool               --   don't add the default extension to the archive name?
  , opt_nodir                :: !Bool               --   don't write the archive directory into the archive (for benchmarks)?
  , opt_update_type          :: !Char               --   file update algorithm (a/f/u/s)
  , opt_x_include_dirs       :: !Bool               --   include directories in processing (for the listing/extraction commands)?
  , opt_no_nst_filters       :: !Bool               --   TRUE if the command has no options selecting files by name/size/time (-n/-s../-t..)
  , opt_file_filter          :: !(FileInfo -> Bool) --   predicate built from the options that selects files by attributes/size/time/name (everything except filespecs)
  , opt_sort_order           :: !String             --   sort order of files in the archive
  , opt_reorder              :: !Bool               --   reorder files after sorting (placing identical/similar files next to each other)?
  , opt_find_group           :: !(FileInfo -> Int)  --   function determining from FileInfo which group (out of arc.groups) this file belongs to
  , opt_groups_count         :: !Int                --   number of groups (`opt_find_group` returns results in the range 0..opt_groups_count-1)
  , opt_find_type            :: !(FileInfo -> Int)  --   function determining from FileInfo which data type (out of those listed in `opt_data_compressor`) this file belongs to
  , opt_types_count          :: !Int                --   number of file types (`opt_find_type` returns results in the range 0..opt_types_count-1)
  , opt_group2type           :: !(Int -> Int)       --   converts a group number from arc.groups into a file type number from opt_data_compressor
  , opt_logfile              :: !String             --   log file name or ""
  , opt_delete_files         :: !DelOptions         --   delete files/directories after successful archiving?
  , opt_workdir              :: !String             --   directory for temporary files or ""
  , opt_clear_archive_bit    :: !Bool               --   clear the Archive attribute on successfully packed files (and on files already present in the archive)
  , opt_select_archive_bit   :: !Bool               --   -ao: only files with the Archive bit set (Windows-only)
  , opt_language             :: !String             --   language/localization file
  , opt_recovery             :: !String             --   size of the recovery block (in percent, bytes or sectors)
  , opt_broken_archive       :: !String             --   process a damaged archive by fully scanning it in search of the blocks that are still intact
  , opt_original             :: !String             --   re-download the damaged parts of the archive from the given URL
  , opt_save_bad_ranges      :: !String             --   write the list of damaged archive parts to the given file so they can be re-downloaded
  , opt_cache                :: !Int                --   size of the read-ahead buffer.
  , opt_limit_compression_memory   :: !MemSize      --   memory limit for compression, bytes
  , opt_limit_decompression_memory :: !MemSize      --   memory limit for decompression, bytes

                                                    -- Encryption settings:
  , opt_encryption_algorithm :: !String             --   encryption algorithm.
  , opt_cook_passwords                              --   prepares the command for using encryption by asking the user for a password and reading the keyfile (this must not run before the command itself starts executing, so it cannot be done in parseCmdline)
                             :: !(Command -> (ParseDataFunc -> IO String, ParseDataFunc -> IO String, IO ()) -> IO Command)
  , opt_data_password        :: String              --   password used to encrypt the data (includes keyboard input and the contents of keyfiles). "" - no password needed
  , opt_headers_password     :: String              --   password used to encrypt the headers (ditto)
  , opt_decryption_info                             --   information used by the decryption key search procedure:
                             :: ( Bool              --     don't ask the user for a new password even if none of the known ones can decrypt the data?
                                , MVar [String]     --     list of "old passwords" we try to decrypt the data being extracted with
                                , [String]          --     contents of the keyfiles appended to the passwords
                                , IO String         --     ask_decryption_password
                                , IO ()             --     bad_decryption_password
                                )
  -- File read/write operations in the charset configured by the -sc option
  , opt_parseFile   :: !(Domain -> FilePath -> IO [String])      -- procedure parsing a file with the charset configured by -sc and OS-independent line splitting
  , opt_unParseFile :: !(Domain -> FilePath -> String -> IO ())  -- procedure writing a file with the charset configured by -sc
  , opt_parseData   :: !(Domain -> String -> String)             -- procedure parsing entered data with the charset configured by -sc
  , opt_unParseData :: !(Domain -> String -> String)             -- procedure unparsing data for output with the charset configured by -sc
  }

-- |Virtual option --debug
opt_debug cmd = cmd.$opt_display.$(`contains_one_of` "$#")

-- |Enable memory testing?
opt_testMalloc cmd = cmd.$opt_display.$(`contains_one_of` "%")

-- |The compressor actually used differs from the one recorded in the block header
-- by the "tempfile" calls inserted between overly memory-hungry algorithms
-- (memory is limited to the -lc value and to the size of the largest free memory block,
-- unless -lc- is given)
limit_compressor command compressor = do
  let memory_limit = opt_limit_compression_memory command
  if memory_limit==CompressionLib.aUNLIMITED_MEMORY
    then return compressor
    else do maxMem <- getMaxMemToAlloc
            return$ limitCompressionMemoryUsage (memory_limit `min` maxMem) compressor


-- |List of options supported by the program
optionsList = sortOn (\(OPTION a b _) -> (a|||"zzz",b))
   [OPTION "--"    ""                   "stop processing options"
   ,OPTION "cfg"   "config"            ("use configuration FILES (default: " ++ aCONFIG_FILE ++ ")")
   ,OPTION "env"   ""                  ("read default options from environment VAR (default: " ++ aCONFIG_ENV_VAR ++ ")")
   ,OPTION "r"     "recursive"          "recursively collect files"
   ,OPTION "f"     "freshen"            "freshen files"
   ,OPTION "u"     "update"             "update files"
   ,OPTION ""      "sync"               "synchronize archive and disk contents"
   ,OPTION "o"     "overwrite"          "existing files overwrite MODE (+/-/p)"
   ,OPTION "y"     "yes"                "answer Yes to all queries"
   ,OPTION "x"     "exclude"            "exclude FILESPECS from operation"
   ,OPTION "n"     "include"            "include only files matching FILESPECS"
   ,OPTION "ep"    "ExcludePath"        "Exclude/expand path MODE"
   ,OPTION "ap"    "arcpath"            "base DIR in archive"
   ,OPTION "dp"    "diskpath"           "base DIR on disk"
   ,OPTION "m"     "method"             "compression METHOD (-m0..-m9, -m1x..-m9x)"
   ,OPTION "dm"    "dirmethod"          "compression METHOD for archive directory"
   ,OPTION "ma"    ""                   "set filetype detection LEVEL (+/-/1..9)"
   ,OPTION "md"    "dictionary"         "set compression dictionary to N mbytes"
   ,OPTION "mm"    "multimedia"         "set multimedia compression to MODE"
   ,OPTION "ms"    "StoreCompressed"    "store already compressed files"
   ,OPTION "mt"    "MultiThreaded"      "number of compression THREADS"
   ,OPTION "mc"    ""                   "disable compression algorithms (-mcd-, -mc-rep...)"
   ,OPTION "mx"    ""                   "maximum internal compression mode"
   ,OPTION "max"   ""                   "maximum compression using external precomp, ecm, ppmonstr"
   ,OPTION "ds"    "sort"               "sort files in ORDER"                      -- to do: make this option an OptArg
   ,OPTION ""      "groups"             "name of groups FILE"                      -- to do: make this option an OptArg
   ,OPTION "s"     "solid"              "GROUPING for solid compression"           -- to do: make this option an OptArg
   ,OPTION "p"     "password"           "encrypt/decrypt compressed data using PASSWORD"
   ,OPTION "hp"    "HeadersPassword"    "encrypt/decrypt archive headers and data using PASSWORD"
   ,OPTION "ae"    "encryption"         "encryption ALGORITHM (aes, blowfish, serpent, twofish)"
   ,OPTION "kf"    "keyfile"            "encrypt/decrypt using KEYFILE"
   ,OPTION "op"    "OldPassword"        "old PASSWORD used only for decryption"
   ,OPTION "okf"   "OldKeyfile"         "old KEYFILE used only for decryption"
   ,OPTION "w"     "workdir"            "DIRECTORY for temporary files"
   ,OPTION ""      "create-in-workdir"  "create archive in workdir and then move to final location"
   ,OPTION "sc"    "charset"            "CHARSETS used for listfiles and comment files"
   ,OPTION ""      "language"           "load localisation from FILE"
   ,OPTION "tp"    "pretest"            "test archive before operation using MODE"
   ,OPTION "t"     "test"               "test archive after operation"
   ,OPTION "t"     "type"               "archive TYPE (arc/zip/rar/...)"
   ,OPTION "d"     "delete"             "delete files & dirs after successful archiving"
   ,OPTION "df"    "delfiles"           "delete only files after successful archiving"
   ,OPTION "kb"    "keepbroken"         "keep broken extracted files"
   ,OPTION "ba"    "BrokenArchive"      "deal with badly broken archive using MODE"
   ,OPTION "ac"    "ClearArchiveBit"    "clear Archive bit on files succesfully (de)archived"
   ,OPTION "ao"    "SelectArchiveBit"   "select only files with Archive bit set"
   ,OPTION "sm"    "SizeMore"           "select files larger than SIZE"
   ,OPTION "sl"    "SizeLess"           "select files smaller than SIZE"
   ,OPTION "tb"    "TimeBefore"         "select files modified before specified TIME"
   ,OPTION "ta"    "TimeAfter"          "select files modified after specified TIME"
   ,OPTION "tn"    "TimeNewer"          "select files newer than specified time PERIOD"
   ,OPTION "to"    "TimeOlder"          "select files older than specified time PERIOD"
   ,OPTION "k"     "lock"               "lock archive"
   ,OPTION "rr"    "recovery"           "add recovery information of specified SIZE to archive"
   ,OPTION "sfx"   ""                  ("add sfx MODULE (\""++aDEFAULT_SFX++"\" by default)")  -- to do: make this option an OptArg
   ,OPTION "z"     "arccmt"             "read archive comment from FILE or stdin"  -- to do: make this option an OptArg
   ,OPTION ""      "archive-comment"    "input archive COMMENT in cmdline"
   ,OPTION "i"     "indicator"          "select progress indicator TYPE (0/1/2)"   -- to do: make this option an OptArg
   ,OPTION "ad"    "adddir"             "add arcname to extraction path"
   ,OPTION "ag"    "autogenerate"       "autogenerate archive name with FMT"       -- to do: make this option an OptArg
   ,OPTION ""      "noarcext"           "don't add default extension to archive name"
   ,OPTION "tk"    "keeptime"           "keep original archive time"
   ,OPTION "tl"    "timetolast"         "set archive time to latest file"
   ,OPTION "fn"    "fullnames"          "match with full names"
   ,OPTION ""      "append"             "add new files to the end of archive"
   ,OPTION ""      "recompress"         "recompress archive contents"
   ,OPTION ""      "dirs"               "add empty dirs to archive"
   ,OPTION "ed"    "nodirs"             "don't add empty dirs to archive"
   ,OPTION ""      "nodates"            "don't store filetimes in archive"
   ,OPTION "ioff"  "shutdown"           "shutdown computer when operation completed"
   ,OPTION ""      "pause-before-exit"  "make a PAUSE just before closing program window"
   ,OPTION "v"     "volume"             "split archive to volumes each of SIZE bytes"
   ,OPTION ""      "queue"              "queue operations across multiple FreeArc copies"
   ,OPTION ""      "arc-32bit-legacy"   "read archives produced by 32-bit FreeArc/Arc.exe"
   ,OPTION ""      "cache"              "use N mbytes for read-ahead cache"
   ,OPTION "lc"    "LimitCompMem"       "limit memory usage for compression to N mbytes"
   ,OPTION "ld"    "LimitDecompMem"     "limit memory usage for decompression to N mbytes"
   ,OPTION ""      "nodir"              "don't write archive directories"
   ,OPTION ""      "nodata"             "don't store data in archive"
   ,OPTION ""      "crconly"            "save/check CRC, but don't store data"
   ,OPTION "di"    "display"           ("control AMOUNT of information displayed: ["++aDISPLAY_ALL++"]*")
   ,OPTION ""      "logfile"            "duplicate all information displayed to this FILE"
   ,OPTION ""      "print-config"       "display built-in definitions of compression methods"
   ,OPTION ""      "proxy"              "setups proxy(s) for URL access"
   ,OPTION ""      "bypass"             "setups proxy bypass list for URL access"
   ,OPTION ""      "original"           "redownload broken parts of archive from the URL"
   ,OPTION ""      "save-bad-ranges"    "save list of broken archive parts to the FILE"
   ]

-- |List of options that should be preferred when command-line parsing collisions arise
aPREFFERED_OPTIONS = words "method sfx charset SizeMore SizeLess overwrite shutdown type"

-- |Options from the previous list that have the highest priority :)
aSUPER_PREFFERED_OPTIONS = words "OldKeyfile"

-- |Hide passwords in the command line (before printing it to the log)
hidePasswords args = map f args1 ++ args2 where
  (args1,args2)  =  break (=="--") args
  f "-p-"                                   =  "-p-"
  f ('-':'p':_)                             =  "-p"
  f "-op-"                                  =  "-op-"
  f ('-':'o':'p':_)                         =  "-op"
  f "-hp-"                                  =  "-hp-"
  f ('-':'h':'p':_)                         =  "-hp"
  f "--OldPassword-"                        =  "--OldPassword-"
  f x | "--OldPassword" `isPrefixOf` x      =  "--OldPassword"
  f "--HeadersPassword-"                    =  "--HeadersPassword-"
  f x | "--HeadersPassword" `isPrefixOf` x  =  "--HeadersPassword"
  f "--password-"                           =  "--password-"
  f x | "--password" `isPrefixOf` x         =  "--password"
  f x = x


-- |Description of the commands supported by the program
commandsList = [
    "a        add files to archive"
  , "c        add comment to archive"
  , "ch       modify archive (recompress, encrypt and so on)"
  , "create   create new archive"
  , "cw       write archive comment to file"
  , "d        delete files from archive"
  , "e        extract files from archive ignoring pathnames"
  , "f        freshen archive"
  , "j        join archives"
  , "k        lock archive"
  , "l        list files in archive"
  , "lb       bare list of files in archive"
  , "lt       technical archive listing"
  , "m        move files and dirs to archive"
  , "mf       move files to archive"
  , "modify   modify archive using +/-/* actions"
  , "r        recover archive using recovery record"
  , "rr       add recovery record to archive"
  , "s        convert archive to SFX"
  , "t        test archive integrity"
  , "u        update files in archive"
  , "v        verbosely list files in archive"
  , "x        extract files from archive"
  ]

-- |List of the commands supported by the program
aLL_COMMANDS = map (head.words) commandsList

-- |List of the commands that simply copy the archive
is_COPYING_COMMAND ('r':'r':_) = True
is_COPYING_COMMAND ('s':_)     = True
is_COPYING_COMMAND x           = x `elem` words "c ch d j k"

-- |A command that MUST NOT have any arguments (besides the archive name)
is_CMD_WITHOUT_ARGS x  =  is_COPYING_COMMAND x  &&  (x `notElem` words "d j")

-- |Classification of all commands into four types: compression, extraction, testing and listing commands
data CmdType = ADD_CMD | EXTRACT_CMD | TEST_CMD | LIST_CMD | RECOVER_CMD  deriving (Eq)
cmdType "t"  = TEST_CMD
cmdType "e"  = EXTRACT_CMD
cmdType "x"  = EXTRACT_CMD
cmdType "cw" = EXTRACT_CMD
cmdType "l"  = LIST_CMD
cmdType "lb" = LIST_CMD
cmdType "lt" = LIST_CMD
cmdType "v"  = LIST_CMD
cmdType "r"  = RECOVER_CMD
cmdType  _   = ADD_CMD
{-# NOINLINE cmdType #-}

-- |Archiver version recorded in the HEADER BLOCK
aARCHIVE_VERSION = make4byte 0 0 5 1

{-# NOINLINE aARC_VERSION_WITH_DATE #-}
{-# NOINLINE aARC_HEADER_WITH_DATE #-}
{-# NOINLINE aARC_HEADER #-}
{-# NOINLINE aARC_VERSION #-}
{-# NOINLINE aARC_AUTHOR #-}
{-# NOINLINE aARC_EMAIL #-}
{-# NOINLINE aARC_WEBSITE #-}
-- |Short program name printed at startup
aARC_VERSION_WITH_DATE = aARC_VERSION ++ " ("++aARC_DATE++")"   -- aARC_VERSION
aARC_HEADER_WITH_DATE  = aARC_HEADER  ++ " ("++aARC_DATE++")"   -- aARC_HEADER
aARC_HEADER  = aARC_NAME++" "++aARC_VERSION++" "
aARC_VERSION = "2.1.0 (FA 0.67.1-compat)"              -- Wire format: 0.51; feature set: FA 0.67.1
aARC_DATE    = "2026"
aARC_NAME    = "DArc"
aARC_AUTHOR  = "Bulat Ziganshin"
aARC_EMAIL   = "Bulat.Ziganshin@gmail.com"
aARC_WEBSITE = "https://github.com/DavidLee18/DArc"

{-# NOINLINE aHELP #-}
-- |HELP printed when the program is invoked without parameters
aHELP = aARC_HEADER++" "++aARC_WEBSITE++"  "++aARC_DATE++"\n"++
        "A project created by DavidLee18 with the collaboration of YadeWira\n"++
        "Usage: Arc command [options...] archive [files... @listfiles...]\n" ++
        joinWith "\n  " ("Commands:":commandsList) ++ "\nOptions:\n" ++ optionsHelp

-- |Ways of grouping files for a solid block or for the archive directory
data Grouping = GroupNone                   -- each file separately
                                            -- grouping by:
              | GroupByExt                  --   identical extension
              | GroupBySize      FileSize   --   minimum data block size
              | GroupByBlockSize MemSize    --   maximum data block size (for block-oriented algorithms such as BWT and ST)
              | GroupByNumber    FileCount  --   number of files
              | GroupAll                    -- all files together

-- |Value of the -d[f] option: don't delete, delete files only, delete files and directories
data DelOptions = NO_DELETE | DEL_FILES | DEL_FILES_AND_DIRS  deriving (Eq)


---------------------------------------------------------------------------------------------------
-- DEFAULT VALUES ---------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Data compression method
#ifdef __MHS__
-- MicroHs: all numeric presets (0-9) now work, including dict+lzp+ppmd chains.
-- Default to "4" (same as GHC build) for best text compression.
aDEFAULT_COMPRESSOR = "4"
#else
aDEFAULT_COMPRESSOR = "4"
#endif

-- |Compression method for the archive directory
aDEFAULT_DIR_COMPRESSION = "lzma:bt4:1m"

-- |Solid block size (a single solid block for everything)
aDEFAULT_DATA_GROUPING  =  ""

-- |Grouping for directories
aDEFAULT_DIR_GROUPING  =  GroupByNumber (20*1000)

-- |Default data encryption algorithm
aDEFAULT_ENCRYPTION_ALGORITHM = "aes"

-- |If no file names are given on the command line - process everything, i.e. "*"
aDEFAULT_FILESPECS = [reANY_FILE]

-- |Extension of archive files
aDEFAULT_ARC_EXTENSION = ".arc"

-- |Extension of SFX archive files
#ifdef FREEARC_WIN
aDEFAULT_SFX_EXTENSION = ".exe"
#else
aDEFAULT_SFX_EXTENSION = ""
#endif

-- |Localization file
aLANG_FILE = "arc.language.txt"

-- |File describing the sort order of file names for "-og"
aDEFAULT_GROUPS_FILE = "arc.groups"

-- |Default SFX module
aDEFAULT_SFX = "freearc.sfx"

-- |Configuration file (holding the default options)
aCONFIG_FILE = "arc.ini"

-- |Environment variable containing the default options
aCONFIG_ENV_VAR = "FREEARC"

-- |Sort order used for solid compression (to improve the compression ratio)
aDEFAULT_SOLID_SORT_ORDER = "gerpn"

-- |Amount of information printed on screen - by default and when the "--display" option is used without a parameter.
-- By default "cmo" is not printed - additional options, compression mode and memory used
aDISPLAY_DEFAULT = "hanwrftske"
aDISPLAY_ALL     = "hoacmnwrfdtske"

-- arc.ini sections
compressionMethods = "[Compression methods]"
defaultOptions     = "[Default options]"
externalCompressor = "[External compressor:*]"

-- |Normalizing a section name to its canonical form
cleanupSectionName  =  strLower . filter (not.isSpace)

-- |Check whether this is a section heading
selectSectionHeadings  =  ("["==) . take 1 . trim


----------------------------------------------------------------------------------------------------
---- Universal command-line parser -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Option description - short name, long name, printed description
data Option = OPTION String String String

-- |Option type - short ones are prefixed with "-", long ones with "--"
data OptType  =  SHORT | LONG

-- |Presence of a parameter in the option: none/required/optional
data ParamType  =  ParamNo | ParamReq | ParamOpt

-- |Option "dictionary", holding them in a form convenient for command-line parsing
optionsDict  =  concatMap compileOption optionsList
  where compileOption (OPTION short long description)  =  compile short ++ compile ('-':long)
          where -- Add the description of the option named `name` to the list, if the name is non-empty
                compile name  =  case (name, paramName description) of
                    ("",  _      )  ->  []                                -- no name - no option :)
                    ("-", _      )  ->  []                                -- no name - no option :)
                    (_,   Nothing)  ->  [(name, long|||short, ParamNo )]  -- option without a parameter
                    (_,   Just _ )  ->  [(name, long|||short, ParamReq)]  -- option with a parameter

-- |Description of the options for the user.
optionsHelp  =  init$ unlines table
  where (ss,ls,ds)     = (unzip3 . map fmtOpt) optionsList
        table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
        paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
        sameLen xs     = flushLeft ((maximum . map length) xs) xs
        flushLeft n    = map (left_justify n)
          -- Returns the formatting of the "short option", the "long option", and their description
        fmtOpt (OPTION short long description)  =  (format short "" description, format ('-':long) "=" description, description)
          -- Returns the formatting of option `name`, accounting for whether it has a name and a parameter
        format name delim description  =  case (name, paramName description) of
                                            ("",   _         )  ->  ""
                                            ("-",  _         )  ->  ""
                                            ("--", _         )  ->  "--"
                                            (_,    Nothing   )  ->  "-"++name
                                            (_,    Just aWORD)  ->  "-"++name++delim++aWORD

-- |Returns the name of the option's parameter, extracting it from its description string.
paramName descr =
  case filter (all isUpper) (words descr)
    of []      -> Nothing      -- The description contains no UPPERCASED words
       [aWORD] -> Just aWORD   -- The description contains an UPPERCASED word denoting the option's parameter
       _       -> error$ "option description \""++descr++"\" contains more than one uppercased word"

-- |Command-line parsing, returning the list of options and the list of "free arguments"
parseOptions []          options freeArgs  =  return (reverse options, reverse freeArgs)
parseOptions ("--":args) options freeArgs  =  return (reverse options, reverse freeArgs ++ args)

parseOptions (('-':option):args) options freeArgs = do
  let check (prefix, _, ParamNo)  =  (option==prefix)
      check (prefix, _, _)        =  (startFrom prefix option /= Nothing)
  let accept (prefix, name, haveParam)  =  return (name, tryToSkip "=" (tryToSkip prefix option))
      unknown                           =  registerError$ CMDLINE_UNKNOWN_OPTION ('-':option)
      ambiguous variants                =  registerError$ CMDLINE_AMBIGUOUS_OPTION ('-':option) (map (('-':) . fst3) variants)
  newopt <- case (filter check optionsDict) of
              [opt] -> accept opt  -- accept the option
              []    -> unknown     -- unknown option.
              xs    -> -- On ambiguity, consult the list of preferred options
                       case (filter ((\x -> x `elem` (aPREFFERED_OPTIONS++aSUPER_PREFFERED_OPTIONS)) . snd3) xs) of
                         [opt] -> accept opt        -- accept the option
                         []    -> ambiguous xs      -- ambiguous option that is not in the preference list
                         xs    -> -- Repeat the trick! :)
                                  case (filter ((\x -> x `elem` aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                    [opt] -> accept opt        -- accept the option
                                    []    -> ambiguous xs      -- ambiguous option that is not in the preference list
                                    xs    -> ambiguous xs      -- still ambiguous even within the preference list!

  parseOptions args (newopt:options) freeArgs

parseOptions (arg:args) options freeArgs   =  parseOptions args options (arg:freeArgs)


-- |Return the list of values of the option named `flag`. Example call: findReqList opts "exclude"
findReqList ((name, param):flags) flag  | name==flag  =  param: findReqList flags flag
findReqList (_:flags) flag                            =  findReqList flags flag
findReqList [] flag                                   =  []

-- |Return the value of the option named `flag`, or the default `deflt` if it is absent
findReqArg options flag deflt  =  last (deflt : findReqList options flag)

-- |Return the value of an option with an optional parameter
findOptArg = findReqArg

-- |Return the value of the option named `flag`, or Nothing if it is absent
findMaybeArg options flag  =  case findReqList options flag
                                of [] -> Nothing
                                   xs -> Just (last xs)

-- |Return True if the option list contains an option named `flag`
findNoArg options flag  =  case findReqList options flag
                                of [] -> False
                                   _  -> True

-- |Return Just True if the option list contains an option named `flag1`,
--          Just False if the option list contains an option named `flag2`,
--          Nothing if neither one is present
findNoArgs options flag1 flag2  =  case filter (\(o,_) -> o==flag1||o==flag2) options
                                     of [] -> Nothing
                                        xs -> Just (fst (last xs) == flag1)

{-# NOINLINE optionsDict #-}
{-# NOINLINE optionsHelp #-}
{-# NOINLINE parseOptions #-}
{-# NOINLINE findReqList #-}
{-# NOINLINE findReqArg #-}
{-# NOINLINE findMaybeArg #-}
{-# NOINLINE findNoArg #-}
{-# NOINLINE findNoArgs #-}


---------------------------------------------------------------------------------------------------
---- (Lua scripting was removed; the luaLevel/luaEvent stubs remain as no-ops.)                ----
---------------------------------------------------------------------------------------------------

-- Lua scripting was REMOVED. It was an advisory event hook -- eight events
-- (ProgramStart/Done, CommandStart/Done, ArchiveStart/Done, Error, Warning)
-- dispatched to handlers registered by `arc.*.lua` config scripts, with every
-- exception swallowed so a script could not affect the archive. Nothing in the
-- format or the CLI depended on it, and it carried a vendored copy of Lua 5.1
-- (16,338 lines) that had to be built on every platform.
--
-- The stubs stay so the `luaLevel`/`luaEvent` call sites read unchanged; they
-- are the same no-ops the FREEARC_NO_LUA build always used.
type LuaState = ()
luaInit      = return ()
luaRun _ _ _ = return ()



-- |The global Lua instance
{-# NOINLINE lua_state #-}
lua_state :: MVar LuaState
lua_state = unsafePerformIO $ do
   lua <- luaInit
   errorHandlers   ++= [\msg -> luaEvent "Error"   [("message", msg)]]
   warningHandlers ++= [\msg -> luaEvent "Warning" [("message", msg)]]
   newMVar lua

-- |Run Lua event in the global Lua instance
luaEvent  =  liftMVar3 luaRun lua_state

-- |Perform Start/Done procedures of givel level
luaLevel level params action = do
  luaEvent (level++"Start") params
  ensureCtrlBreak "luaDone" (luaEvent (level++"Done") [""]) action


----------------------------------------------------------------------------------------------------
---- System information ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- macOS is grouped with Windows here rather than with Linux: the Linux branch
-- below reads /proc/meminfo, which does not exist on macOS, so it would report
-- zero physical memory and the -lc/-ld defaults derived from it would collapse
-- to zero. Environment.cpp queries sysctl there instead, so bind to it.
#if defined(FREEARC_WIN) || defined(FREEARC_MACOS)
-- |Number of physical processors/cores in the system. Determines number of heavy-computations thread runned
foreign import ccall unsafe "Environment.h GetProcessorsCount"
  getProcessorsCount :: CInt

-- |Size of physical computer memory in bytes
foreign import ccall unsafe "Environment.h GetPhysicalMemory"
  getPhysicalMemory :: CUInt

-- |Size of maximum memory block we can allocate in bytes
foreign import ccall unsafe "Environment.h GetMaxMemToAlloc"
  getMaxMemToAlloc :: IO CUInt

-- |Size of physical computer memory that is currently unused
foreign import ccall unsafe "Environment.h GetAvailablePhysicalMemory"
  getAvailablePhysicalMemory :: CUInt

-- |Prints detailed stats about memory available
foreign import ccall unsafe "Environment.h TestMalloc"
  testMalloc :: IO ()

#else

-- |Number of physical processors/cores in the system.
-- Uses GHC.Conc.getNumProcessors which queries the OS directly.
{-# NOINLINE getProcessorsCount #-}
getProcessorsCount :: CInt
getProcessorsCount  =  unsafePerformIO $ fmap fromIntegral GHC.Conc.getNumProcessors

-- |Size of physical computer memory in bytes (Linux: read /proc/meminfo)
{-# NOINLINE getPhysicalMemory #-}
getPhysicalMemory :: CUInt
getPhysicalMemory  =  unsafePerformIO $ readSysMemKB "MemTotal:"

-- |Size of physical computer memory that is currently unused (Linux: read /proc/meminfo)
{-# NOINLINE getAvailablePhysicalMemory #-}
getAvailablePhysicalMemory :: CUInt
getAvailablePhysicalMemory  =  unsafePerformIO $ readSysMemKB "MemAvailable:"

-- |Parse a memory value in kB from /proc/meminfo for the given field label
-- CUInt is 32 bits, so it tops out at just under 4 GB while /proc/meminfo
-- reports kB that multiply well past that on any modern machine. Narrowing
-- the product directly wrapped silently under GHC and threw "arithmetic
-- overflow" under MicroHs, which made every command abort during
-- command-line parsing -- Cmdline.hs forces this to compute the default
-- -lc/-ld limits before touching a single file.
--
-- Saturating at maxBound keeps the existing 32-bit interface (the Windows
-- branch of this #if binds a C function returning unsigned) and is the right
-- answer for the one thing the value feeds: a memory ceiling. A machine with
-- more than 4 GB is reported as having 4 GB and gets a correspondingly
-- conservative limit, which is a performance compromise rather than a
-- correctness one. Widening the type end to end, including Environment.h,
-- would remove the compromise.
readSysMemKB :: String -> IO CUInt
readSysMemKB label = do
  result <- Control.Exception.try (readFile "/proc/meminfo") :: IO (Either Control.Exception.SomeException String)
  case result of
    Left  _    -> return 0
    Right info ->
      case [ clampToCUInt (n * 1024)
           | l <- lines info
           , label `Data.List.isPrefixOf` l
           , w <- [words (drop (length label) l)]
           , not (null w)
           , let n = read (head w) :: Integer ] of
        (v:_) -> return v
        []    -> return 0

-- |Narrow to CUInt without overflowing: values above the 32-bit ceiling
-- saturate rather than wrapping or raising an arithmetic exception.
clampToCUInt :: Integer -> CUInt
clampToCUInt n
  | n <= 0        = 0
  | n >= maxCUInt = maxBound
  | otherwise     = fromIntegral n
  where maxCUInt = toInteger (maxBound :: CUInt)

-- |Size of maximum memory block we can allocate.
-- On 64-bit Unix there is effectively no single-allocation limit.
getMaxMemToAlloc :: IO CUInt
getMaxMemToAlloc  =  return maxBound

-- |Prints detailed stats about memory available (simplified Haskell version)
testMalloc :: IO ()
testMalloc  =  do
  let physMem  = getPhysicalMemory
      availMem = getAvailablePhysicalMemory
  putStrLn $ "Physical memory: " ++ show (physMem `div` (1024*1024)) ++ " mb"
  putStrLn $ "Available memory: " ++ show (availMem `div` (1024*1024)) ++ " mb"

#endif

