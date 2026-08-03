{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Main program module.                                                                       ----
---- Calls parseCmdline from the Cmdline module to parse the command line and executes each     ----
----   resulting command.                                                                       ----
---- If a command must process several archives, findArchives duplicates it                     ----
----   for each of them.                                                                        ----
---- Then each command boils down to performing one of the following tasks:                     ----
---- * modification of an archive  using  runArchiveCreate   from module ArcCreate   (commands a/f/m/u/j/d/ch/c/k/rr)
---- * extraction of an archive  -  runArchiveExtract  -         ArcExtract  (commands t/e/x)   ----
---- * listing of an archive     -  runArchiveList     -         ArcList     (commands l/v)     ----
---- * recovery of an archive    -  runArchiveRecovery -         ArcRecover  (command r)        ----
---- which are given arguments according to the specifics of the particular command being run.  ----
----                                                                                            ----
---- These procedures in turn refer, directly or indirectly, to the modules:                    ----
----   ArhiveFileList   - for working with the lists of files being archived                    ----
----   ArhiveDirectory  - for reading/writing the archive directory                             ----
----   ArhiveStructure  - for working with the archive structure                                ----
----   ByteStream       - for turning the archive directory into a byte sequence                ----
----   Compression      - for invoking the compression, decompression and CRC algorithms        ----
----   UI               - for keeping the user informed about the work in progress :)           ----
----   Errors           - for reporting errors that occur and writing to the log file           ----
----   FileInfo         - for finding files on disk and obtaining information about them        ----
----   Files            - for all operations on files on disk and on file names                 ----
----   Process          - for splitting the algorithm into parallel communicating processes     ----
----   Utils            - for all the remaining helper functions                                ----
----------------------------------------------------------------------------------------------------
module Arc where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
#ifdef __GLASGOW_HASKELL__
import GHC.Conc (setUncaughtExceptionHandler, getNumCapabilities, setNumCapabilities, getNumProcessors)
#endif
import Control.Monad
import Data.List
import System.Mem
import System.IO

import Utils
import Process
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UI
import ArcCreate
import ArcExtract
import ArcRecover
import Arc7z
import Foreign.C.String (CString, withCString)
import Foreign.C.Types  (CInt(..))

foreign import ccall unsafe "darc_queue_acquire" c_queue_acquire :: CString -> IO CInt
foreign import ccall unsafe "darc_queue_release" c_queue_release :: CInt   -> IO ()


-- |The program's main function
main         =  (doMain =<< myGetArgs) >> shutdown "" aEXIT_CODE_SUCCESS
-- |A duplicate main function for interactive debugging
arc cmdline  =  doMain (words cmdline)

-- |Turn the command line into a set of commands and execute them
doMain args  = do
#ifdef __GLASGOW_HASKELL__
  setUncaughtExceptionHandler handler
  nprocs <- getNumProcessors
  ncaps  <- getNumCapabilities
  when (ncaps < nprocs) $ setNumCapabilities nprocs
#endif
  setCtrlBreakHandler $ do          -- Set up ^Break handling
  ensureCtrlBreak "resetConsoleTitle" resetConsoleTitle $ do
  luaLevel "Program" [("command", args)] $ do
  uiStartProgram                    -- Open the UI
  commands <- parseCmdline args     -- Turn the command line into a list of commands to execute
  -- FreeArc 0.67 --queue: serialize with other arc processes via advisory lockfile
  queue_fd <- if any opt_queue commands
                then withCString "/tmp/darc.queue.lock" c_queue_acquire
                else return (-1)
  mapM_ run commands                -- Execute each of the resulting commands
  when (queue_fd >= 0) $ c_queue_release queue_fd
  uiDoneProgram                     -- Close the UI

 where
   handler (ex :: SomeException)  =
    registerError$ GENERAL_ERROR$
      maybe (show ex) (\(ErrorCall s) -> s) (fromException ex) : []


-- |Dispatches a command and arranges for it to be repeated for each matching archive
run command@Command
                { cmd_name            = cmd
                , cmd_setup_command   = setup_command
                , opt_scan_subdirs    = scan_subdirs
                } = do
  performGC       -- collect the garbage left over from processing the previous commands
  setup_command   -- apply the settings required before the command starts running
  luaLevel "Command" [("command", cmd)] $ do
  -- Route .7z archives to the system 7zz/7z binary.
  if is7zArchive (cmd_arcspec command)
    then run7z command
    else case cmd of
      "create" -> findArchives  False           runAdd     command
      "a"      -> findArchives  False           runAdd     command
      "f"      -> findArchives  False           runAdd     command
      "m"      -> findArchives  False           runAdd     command
      "mf"     -> findArchives  False           runAdd     command
      "u"      -> findArchives  False           runAdd     command
      "j"      -> findArchives  False           runJoin    command
      "cw"     -> findArchives  False           runCw      command
      "ch"     -> findArchives  scan_subdirs    runCopy    command
      "modify" -> findArchives  scan_subdirs    runModify  command
      's':_    -> findArchives  scan_subdirs    runCopy    command
      "c"      -> findArchives  scan_subdirs    runCopy    command
      "k"      -> findArchives  scan_subdirs    runCopy    command
      'r':'r':_-> findArchives  scan_subdirs    runCopy    command
      "r"      -> findArchives  scan_subdirs    runRecover command
      "d"      -> findArchives  scan_subdirs    runDelete  command
      "e"      -> findArchives  scan_subdirs    runExtract command
      "x"      -> findArchives  scan_subdirs    runExtract command
      "t"      -> findArchives  scan_subdirs    runTest    command
      "l"      -> findArchives  scan_subdirs    runList    command
      "lb"     -> findArchives  scan_subdirs    runList    command
      "lt"     -> findArchives  scan_subdirs    runList    command
      "v"      -> findArchives  scan_subdirs    runList    command
      _ -> registerError$ UNKNOWN_CMD cmd aLL_COMMANDS


-- |Finds the archives matching the wildcard arcspec and runs the given command on each of them
findArchives scan_subdirs   -- search for archives in subdirectories too?
              run_command    -- the procedure to run on each archive found
              command@Command {cmd_arcspec = arcspec} = do
  uiStartCommand command   -- Mark the start of the command's execution
  arclist <- if scan_subdirs || is_wildcard arcspec
               then find_files scan_subdirs arcspec >>== map diskName
               else return [arcspec]
  results <- foreach arclist $ \arcname -> do
    performGC   -- collect the garbage left over from processing the previous archives
    luaLevel "Archive" [("arcname", arcname)] $ do
    -- If the -ad option is given, append the archive name (without extension) to the base directory on disk
    let add_dir  =  opt_add_dir command  &&&  (</> takeBaseName arcname)
    run_command command { cmd_arcspec      = error "findArchives:cmd_arcspec undefined"  -- we won't need cmd_arcspec any more.
                        , cmd_arclist      = arclist
                        , cmd_arcname      = arcname
                        , opt_disk_basedir = add_dir (opt_disk_basedir command)
                        }
  uiDoneCommand command results   -- report the results of running the command over all the archives


-- |Commands that add to an archive: create, a, f, m, u
runAdd cmd = do
  msg <- i18n"0246 Found %1 files"
  let diskfiles =  find_and_filter_files (cmd_filespecs cmd) (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = opt_include_dirs     cmd
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = addFileFilter      cmd
                                , ff_group_f        = opt_find_group       cmd.$Just
                                , ff_arc_basedir    = opt_arc_basedir      cmd
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_diskfiles      = diskfiles     -- the files to be added from disk
                   , cmd_archive_filter = const True }  -- filter selecting files from the archives being opened


-- |Archive joining command: j
runJoin cmd@Command { cmd_filespecs = filespecs
                       , opt_noarcext  = noarcext
                       } = do
  msg <- i18n"0247 Found %1 archives"
  let arcspecs  =  map (addArcExtension noarcext) filespecs   -- append the default extension (".arc") to the names
      arcnames  =  map diskName ==<< find_and_filter_files arcspecs (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = Just False
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = addFileFilter      cmd
                                , ff_group_f        = Nothing
                                , ff_arc_basedir    = ""
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_added_arcnames = arcnames      -- additional input archives
                   , cmd_archive_filter = const True }  -- filter selecting files from the archives being opened


-- |Archive modification command: it accepts optional files from disk
-- like `runAdd`, but if there are no filespecs it simply re-encodes the existing archive.
runModify cmd | null (cmd_filespecs cmd) || cmd_filespecs cmd == aDEFAULT_FILESPECS
              = runArchiveAdd cmd{cmd_archive_filter = const True}
runModify cmd = runAdd cmd{cmd_archive_filter = const True}

-- |Commands that copy an archive while applying changes: ch, c, k. s, rr
runCopy    = runArchiveAdd                    . setArcFilter fullFileFilter
-- |Command that deletes from an archive: d
runDelete  = runArchiveAdd                    . setArcFilter ((not.) . fullFileFilter)
-- |Commands that extract from an archive: e, x
runExtract = runArchiveExtract pretestArchive . setArcFilter (test_dirs extractFileFilter)
-- |Archive testing command: t
runTest    = runArchiveExtract pretestArchive . setArcFilter (test_dirs fullFileFilter)
-- |Commands that list an archive: l, v
runList    = runArchiveList pretestArchive    . setArcFilter (test_dirs fullFileFilter)
-- |Command that writes the archive comment to a file: cw
runCw      = runCommentWrite
-- |Archive recovery command: r
runRecover = runArchiveRecovery

-- |Just shortcut
runArchiveAdd  =  runArchiveCreate pretestArchive writeRecoveryBlocks

{-# NOINLINE findArchives #-}
{-# NOINLINE runAdd #-}
{-# NOINLINE runModify #-}
{-# NOINLINE runJoin #-}
{-# NOINLINE runCopy #-}
{-# NOINLINE runDelete #-}
{-# NOINLINE runExtract #-}
{-# NOINLINE runTest #-}
{-# NOINLINE runList #-}


----------------------------------------------------------------------------------------------------
---- Criteria for selecting the files to be processed, for the various command types ---------------
----------------------------------------------------------------------------------------------------

-- |Set in cmd the predicate that selects the files to be processed from the archive
setArcFilter filter cmd  =  cmd {cmd_archive_filter = filter cmd}

-- |Select files according to the opt_file_filter filter, excluding
-- the archives processed by this command and the temporary files created while archiving
addFileFilter cmd      =  all_functions [opt_file_filter cmd, not . overwriteF cmd]

-- |Select files according to the fullFileFilter filter, excluding
-- the archives processed by this command and the temporary files created while archiving
extractFileFilter cmd  =  all_functions [fullFileFilter cmd, not . overwriteF cmd]

-- |Selects, among the files whose wildcards are given on the command line,
-- those matching the opt_file_filter filter
fullFileFilter cmd  =  all_functions
                           [  match_filespecs (opt_match_with cmd) (cmd_filespecs cmd) . fiFilteredName
                           ,  opt_file_filter cmd
                           ]

-- |Selects the archives being processed and the temporary files created while archiving,
-- as well as the files that could overwrite them during extraction
overwriteF cmd  =  in_arclist_or_temparc . fiDiskName
  where in_arclist_or_temparc filename =
            fpFullname filename `elem` cmd_arclist cmd
            || all_functions [(temparcPrefix `isPrefixOf`), (temparcSuffix `isSuffixOf`)]
                             (fpBasename filename)

-- |Add to the file selection filter `filter_f` the selection of directories according to the options of the command `cmd`
test_dirs filter_f cmd fi  =  if fiIsDir fi
                                then opt_x_include_dirs cmd
                                else filter_f cmd fi

