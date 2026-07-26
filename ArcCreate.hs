{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Creating and modifying archives.                                                           ----
---- This is where all archive creation and modification commands are handled:                  ----
----   create/a/f/m/u/ch/c/d/k/s/rr/j                                                           ----
---- The runArchiveCreate procedure builds the list of files to put into the output archive,    ----
----   then starts the processes that build the output archive structure, read input files,     ----
----   compress the data and write it to the output archive.                                    ----
---- These processes are described in ArcvProcessRead.hs and ArcvProcessCompress.hs             ----
----------------------------------------------------------------------------------------------------
module ArcCreate where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import System.Mem
import System.IO




import Utils
import Files
import Charsets            (i18n)
import Process
import Errors
import ByteStream
import FileInfo
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcExtract
import ArcvProcessRead
import ArcvProcessExtract
import ArcvProcessCompress
import Foreign.C.String (CString, withCString)
import Foreign.C.Types  (CInt(..))

foreign import ccall unsafe "darc_split_file" c_split_file :: CString -> CString -> CString -> IO CInt

pad3 :: Int -> String
pad3 n = let s = show n in replicate (3 - length s) '0' ++ s


-- |Generic archive creation/modification command
runArchiveCreate pretestArchive
                 writeRecoveryBlocks
                 command@Command {             -- data about the command being executed:
      cmd_name            = cmd                  --   the command name
    , cmd_arcname         = arcname              --   the main archive that is being updated
    , cmd_archive_filter  = archive_filter       --   predicate selecting which files from the archives are processed
    , cmd_added_arcnames  = find_added_arcnames  --   additional input archives
    , cmd_diskfiles       = find_diskfiles       --   files that should be added from disk
    , opt_arccmt_str      = arccmt_str           --   new archive comment, or
    , opt_arccmt_file     = arccmt_file          --   the file the new archive comment is read from
    , opt_data_compressor = compressor           --   compression algorithm
    } = do
  nodates_ref =: opt_nodates command  -- FreeArc 0.67: --nodates propagated to archiveWriteDir
  opt_testMalloc command &&& testMalloc  -- print the memory map
  -- for extreme memory savings: find_files |> buffer 100_000 |> write_to_archive

  -- Create the sfx archive with the EXE extension right away, unless we have to update an already existing archive
  arcname <- do archiveExists <- fileExist arcname
                if cmd=="create" || not archiveExists
                  then return$ cmdChangeSfxExt command arcname
                  else return arcname
  command <- return command {cmd_arcname = arcname}

  -- The "create" command always builds the archive from scratch
  when (cmd=="create")$  ignoreErrors$ fileRemove arcname
  -- Tell the user that archive processing has started and ask for the archiving password if necessary
  uiStartArchive command =<< limit_compressor command compressor   --   limit the compressor by the memory amount and by the -lc value
  command <- (command.$ opt_cook_passwords) command ask_passwords  --  prepare the passwords in the command for use
  debugLog "Started"

  -- Read the service information of the main (updated) archive, including its directories.
  -- Exit if the archive is locked, or contains recovery info and is damaged.
  -- If we are creating a new archive, substitute a "phantom" for the old one.
  let abort_on_locked_archive archive footer = do
          when (ftLocked footer) $
              registerError$ GENERAL_ERROR ["0310 can't modify archive locked with -k"]
          pretestArchive command archive footer
  --
  uiStage "0249 Reading archive directory"
  updatingArchive <- fileExist arcname
  main_archive    <- if updatingArchive
                       then archiveReadInfo command "" "" archive_filter abort_on_locked_archive arcname
                       else return phantomArc
  debugLogList "There are %1 files in archive being updated" (arcDirectory main_archive)

  -- Find the archives to be added on disk (for the "j" command) and read their service information.
  -- Exit if any of these archives contains recovery info and is damaged.
  uiStartScanning
  added_arcnames <- find_added_arcnames
  debugLogList "Found %1 archives to add" added_arcnames
  added_archives  <- foreach added_arcnames (archiveReadInfo command "" "" archive_filter (pretestArchive command))
  debugLogList "There are %1 files in archives to add" (concatMap arcDirectory added_archives)
  let input_archives = main_archive:added_archives      -- list of all input archives
      closeInputArchives = for input_archives arcClose  -- operation that closes all input archives

  -- Build the comment of the archive being created by combining the old ones or by asking the user
  arcComment <- getArcComment arccmt_str arccmt_file input_archives (opt_parseFile command)

  -- Find the files to be added on disk and sort their list
  uiStartScanning
  diskfiles <- find_diskfiles
  debugLogList "Found %1 files" diskfiles
  uiStage "0250 Sorting filelist"
  sorted_diskfiles <- (opt_reorder command &&& reorder) (sortFiles command diskfiles)
  debugLogList "Sorted %1 files" sorted_diskfiles
  uiStartScanning  -- clear the counter for the file content analysis stage

  -- Build the list of files that should end up in the output archive by merging
  -- the file list of the archive being updated, the file list of the archives added to it
  -- (by the "j" command), and the files from disk. These lists are first stripped of duplicates.
  files_to_archive <- joinLists main_archive added_archives sorted_diskfiles command
  debugLogList "Joined filelists, %1 files" files_to_archive

  if null files_to_archive                    -- If the output archive does not contain a single file
    then do registerWarning NOFILES           -- then tell the user about it
            closeInputArchives                --    close the input archives
            ignoreErrors$ fileRemove arcname  --    delete the archive if it existed before the operation (for example, in the case of the "arc d archive *" command)
            return (1,0,0,0)
    else do

  -- Wrapper that runs post-processing (-d[f], -ac) only if testing the created archive produced no warnings
  postProcessWrapper command $ \postProcess_processDir deleteFiles -> do

  -- Reference used to return the command results to the calling procedure
  results <- ref (error "runArchiveCreate:results undefined")

  -- Save the archive mtime for the -tk option
  old_arc_exist <- fileExist arcname
  arc_time <- if old_arc_exist  then getFileDateTime arcname  else return (error "runArchiveCreate:arc_time undefined")

  -- To implement the -tl option we must receive the lists of all files written to the archive and find the newest of them.
  --   To that end the `find_last_time` procedure is passed to create_archive_structure_PROCESS.
  --   It is fed the list of files written to the archive in chunks, and it tracks the newest of them.
  --   The archive will be stamped with that date once archiving has finished.
  last_time <- ref aMINIMAL_POSSIBLE_DATETIME
  let find_last_time dir  =  last_time .= (\time -> maximum$ time : map (fiTime . fwFileInfo) dir)
  let processDir dir      =  do when (opt_time_to_last command) (find_last_time dir)
                                postProcess_processDir dir  -- the post-processing wrapper must also receive the list of successfully archived files

  -- Tell the user that data compression has started
  uiStartProcessing (map cfFileInfo files_to_archive) 0 0
  performGC   -- Collect garbage to free as much memory as possible for the data compression algorithms

  -- First we write the contents of the archive being created into a temporary file and only then, if archiving succeeds, rename it
  tempfileWrapper arcname command deleteFiles pretestArchive $ \temp_arcname -> ensureCtrlBreak "closeInputArchives" closeInputArchives $ do   -- Close the input archives once archiving has finished
    bracketCtrlBreak "archiveClose:ArcCreate" (archiveCreateRW temp_arcname) archiveClose $ \archive -> do
      writeSFX (opt_sfx command) archive main_archive    -- Start creating the archive by writing the SFX module
      -- Creating an archive is a sequence of separate processes passing data to each other:
      --   the process that lays out the archive structure and reads the data to be compressed
      --   the process that compresses the data and writes it to the archive file
      -- Between them an unbounded queue is created (|>>>), which allows read-ahead of the data being compressed
      let read_files          =  createArchiveAtructureAndReadFilesProcess command archive main_archive files_to_archive processDir arcComment writeRecoveryBlocks results
          compress_AND_write  =  compress_AND_write_to_archive_PROCESS archive command
      backdoor <- newEmptyMVar   -- This channel is used to return information about the created archive blocks
      runP (read_files backdoor |>>> compress_AND_write backdoor)
      --debugLog "Archive written"

  when (opt_keep_time command && old_arc_exist) $ do   -- If the -tk option was used and this was an update of an existing archive
    setFileDateTime arcname arc_time                   --   then restore the archive mtime
  when (opt_time_to_last command) $ do                 -- If the -tl option was used
    setFileDateTime arcname =<< val last_time          --   then set the archive modification date&time to the date&time of the newest file in it
  renameArchiveAsSFX arcname command                   -- Rename the archive if an SFX module was added to it or removed from it
  val results                                          -- Return the command execution statistics


----------------------------------------------------------------------------------------------------
---- Using a temporary file when creating an archive -----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Prefix and suffix of the temporary file names being created
temparcPrefix = "$$temparc$$"
temparcSuffix = ".tmp"

-- |Run `action` with a temporary file name and then rename it
tempfileWrapper filename command deleteFiles pretestArchive action  =  find 0 >>= doit
  where -- Find a free name for the temporary file
        find n = do let tempname = (opt_workdir command ||| takeDirectory filename)
                                   </> (temparcPrefix++show n++temparcSuffix)
                    found <- fileExist tempname
                    case found of
                        True  | n==999    -> registerError$ GENERAL_ERROR ["0311 can't create temporary file"]
                              | otherwise -> find (n+1)
                        False             -> return tempname

        -- Run the action using the temporary file name, test it and then rename the final archive
        doit tempname = do old_file <- fileExist filename      -- Are we updating an existing archive?
                           handleCtrlBreak "fileRemove tempname" (ignoreErrors$ fileRemove tempname) $ do
                             -- Perform the archiving
                             action tempname
                             -- If the "-t" option was given, test the archive we have just created
                             when (opt_test command) $ test_archive tempname (opt_keep_broken command)
                           handleCtrlBreak "Keeping temporary archive" (condPrintLineLn "n"$ "Keeping temporary archive "++tempname) $ do
                             -- Delete the archived files if the -d option was used
                             deleteFiles
                             -- Replace the old archive with the new one
                             if old_file
                                 then fileRemove filename   -- It would be good to check that this is still the very same file
                                 else whenM (fileExist filename) $ do  -- If a file with the output archive name was created while archiving was running, report an error
                                          registerError$ GENERAL_ERROR ["0312 output archive already exists, keeping temporary file %1", tempname]
                             fileRename tempname filename
                                 `catch` (\(_::SomeException)-> do
                                                  condPrintLineLn "n"$ "Copying temporary archive "++tempname++" to "++filename
                                                  fileCopy tempname filename
                                                  fileRemove tempname)
                           -- If the "-t" and "-w" options were given, test the final archive once more
                           when (opt_test command && opt_workdir command/="") $ test_archive filename (opt_keep_broken command || opt_delete_files command /= NO_DELETE)
                           -- FreeArc 0.67 -v/--volume: split finished archive into volumes .001 .002 ...
                           case opt_volumes command of
                             (volsize:_) | volsize > 0 -> do
                               nvols <- withCString filename $ \sp ->
                                          withCString filename $ \dp ->
                                            withCString (show volsize) $ \sz ->
                                              c_split_file sp dp sz
                               if nvols > 0
                                 then do ignoreErrors (fileRemove filename)
                                         condPrintLineLn "n" $ "Split into " ++ show nvols ++ " volume(s): " ++ filename ++ ".001 .. ." ++ pad3 (fromIntegral nvols)
                                         condPrintLineLn "n" $ "To extract, reassemble with: cat "++filename++".* > "++filename
                                 else registerError$ GENERAL_ERROR ["0381 failed to split archive into volumes", filename]
                             _ -> return ()

        -- Test the archive and exit, deleting it, if any problems arose
        test_archive arcname keep_broken_archive = do
            w <- count_warnings $ testArchive command arcname pretestArchive
            -- Continue working only if there are no warnings
            when (w/=0) $ do
                unless keep_broken_archive (ignoreErrors$ fileRemove arcname)
                registerError$ GENERAL_ERROR$ if keep_broken_archive
                                                 then ["0313 archive broken, keeping temporary file %1", arcname]
                                                 else ["0314 archive broken, deleting"]


----------------------------------------------------------------------------------------------------
---- Post-processing performed only if archiving succeeded -----------------------------------------
----------------------------------------------------------------------------------------------------

-- |Post-processing performed only if archiving succeeded:
--    delete the successfully archived files if the -d[f] option is given
--    clear their Archive attribute if the -ac option is given
postProcessWrapper command archiving = do
  doFinally uiDoneArchive2 $ do
  (if opt_delete_files command /= NO_DELETE
      || opt_clear_archive_bit command then
     (do files2delete <- ref []
         dirs2delete <- ref []
         let processDir filelist0
               = do let filelist
                          = map fwFileInfo $ filter isFileOnDisk filelist0
                        (dirs, files) = partition fiIsDir filelist
                    evalList files `seq` (files2delete ++= files)
                    evalList dirs `seq` (dirs2delete ++= dirs)
             deleteFiles
               = when (opt_delete_files command /= NO_DELETE)
                   $ do condPrintLineLn "n" "Deleting successfully archived files"
                        files <- val files2delete
                        forM_ files
                          $ \ fi
                              -> whenM (checkThatFileWasNotChanged fi)
                                   $ do ignoreErrors . fileRemove . fpFullname . fiDiskName $ fi
                        when (opt_delete_files command == DEL_FILES_AND_DIRS)
                          $ do dirs <- val dirs2delete
                               for
                                 (reverse dirs) (ignoreErrors . dirRemove . fpFullname . fiDiskName)
         result <- archiving processDir deleteFiles
         when (opt_clear_archive_bit command)
           $ do condPrintLineLn
                  "n" "Clearing Archive attribute of successfully archived files"
                files <- val files2delete
                for files
                  $ \ fi
                      -> whenM (checkThatFileWasNotChanged fi)
                           $ do clearArchiveBit . fpFullname . fiDiskName $ fi
         return result)
  else
     archiving (\ dir -> return ()) (return ()))

-- |Check that the file has not changed since it was archived
checkThatFileWasNotChanged fi = fileWithStatus "checkThatFileWasNotChanged" (fpFullname . fiDiskName$ fi) $ \p_stat -> do
    size <- stat_size  p_stat
    time <- stat_mtime p_stat
    return (size==fiSize fi  &&  time==fiTime fi)


----------------------------------------------------------------------------------------------------
---- Helper operations -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Obtain the output archive comment from the file given by the -z option,
-- or by concatenating the comments of the input archives, and print it on screen
getArcComment arccmt_str arccmt_file input_archives parseFile = do
  -- Use the comment given on the command line, if any
  if arccmt_str>""  then do uiPrintArcComment arccmt_str
                            return arccmt_str
    else do
  let old_comment = joinWith "\n\n" $ deleteIf null $ map arcComment input_archives
  -- Depending on the value of the "-z" option:
  case arccmt_file of
  -- Enter the comment from stdin
    ""   -> uiInputArcComment old_comment
  -- Delete the old comment
    "-"  -> return ""
  -- Copy the existing comment (the default):
    "--" -> do uiPrintArcComment old_comment
               return old_comment
  -- Read the new comment from the given file:
    _    -> do newcmt <- parseFile 'c' arccmt_file >>== joinWith "\n"
               uiPrintArcComment newcmt
               return newcmt

-- |Write the SFX module at the start of the archive being created
writeSFX sfxname archive old_archive = do
  let oldArchive = arcArchive old_archive
      oldSFXSize = ftSFXSize (arcFooter old_archive)
  case sfxname of                                      -- Depending on the value of the "-sfx" option:
    "-"      -> return ()                              --   delete the old sfx module
    "--"     -> unless (arcPhantom old_archive) $ do   --   copy the sfx from the source archive (the default)
                  archiveCopyData oldArchive 0 oldSFXSize archive
    -- SFX support was removed with Unarc/: the modules WERE Unarc, compiled as
    -- a headless stub, so nothing builds arc.sfx or freearc.sfx any more. The
    -- option still parses (and "-"/"--" still work, since those only delete or
    -- copy a module already present in an existing archive), but naming a file
    -- can no longer succeed. Say so, rather than reporting a missing file and
    -- letting the user hunt for a module that is never going to exist.
    filename -> bracket (archiveOpen sfxname              --   read the sfx module from the given file
                          `catch` (\(e::SomeException) -> registerError$ GENERAL_ERROR ["0315 SFX modules were removed from DArc, so -sfx=%1 cannot be satisfied; self-extracting archives are no longer supported", sfxname]))
                        archiveClose
                        (\sfxfile -> do size <- archiveGetSize sfxfile
                                        archiveCopyData sfxfile 0 size archive)

-- |New archive name reflecting the fact that we added an SFX module to it or, conversely, removed one
cmdChangeSfxExt command  =  changeSfxExt (opt_noarcext command) (opt_sfx command)

changeSfxExt opt_noarcext opt_sfx arcname =
  case (opt_noarcext, opt_sfx) of
--  Disabled, because it prevented converting archives to SFX from inside the GUI
--  (True, _)     -> arcname                -- Do not change the extension if the --noarcext option is given
    (_   , "--")  -> arcname                --   or if the "-sfx" option is not given
                                            -- With "-sfx-" the extension is changed to ".arc"
    (_   , "-")   -> if takeExtension arcname == aDEFAULT_SFX_EXTENSION
                       then replaceExtension arcname aDEFAULT_ARC_EXTENSION
                       else arcname
                                            -- With "-sfx..." the extension is changed to ".exe"
    _             -> if takeExtension arcname == aDEFAULT_ARC_EXTENSION
                       then replaceExtension arcname aDEFAULT_SFX_EXTENSION
                       else arcname

-- |Rename the archive according to its SFX name
renameArchiveAsSFX arcname command = do
  let newname = cmdChangeSfxExt command arcname
  when (newname/=arcname) $ do
    condPrintLineLn "n"$ "Renaming "++arcname++" to "++newname
    fileRename arcname newname









-- |Test the archive we have just created, located in the file named `temp_arcname`
testArchive command temp_arcname pretestArchive = do
  let test_command = command{ cmd_name           = "t"           -- Testing
                            , cmd_arcname        = temp_arcname  -- in the created archive
                            , opt_arc_basedir    = ""            -- all the files
                            , opt_disk_basedir   = ""            -- ...
                            , cmd_archive_filter = const True    -- ...
                            , cmd_subcommand     = True          -- This is a subcommand (testing inside packing)
                            , opt_pretest        = 1             -- no need to run a pretest before testing, but the recovery info must be checked :)
                            }
  uiStartSubCommand command test_command
  results <- runArchiveExtract pretestArchive test_command
  uiDoneSubCommand command test_command [results]









