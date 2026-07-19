----------------------------------------------------------------------------------------------------
---- Implementation of the archive extraction and listing commands                              ----
----------------------------------------------------------------------------------------------------
module ArcExtract ( runArchiveExtract
                  , runArchiveList
                  , runCommentWrite
                  , formatDateTime
                  ) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.List
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Numeric
import System.IO
import System.IO.Unsafe
import System.Posix.Internals hiding (stat_mode)

import Process
import Utils
import Files
import FileInfo
import Charsets            (i18n)
import Errors
import Compression         (aINIT_CRC, updateCRC, finishCRC, join_compressor)
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessExtract

-- |Generic archive extraction command
runArchiveExtract pretestArchive
                  command@Command{ cmd_arcname        = arcname
                                 , cmd_archive_filter = archive_filter
                                 , opt_arc_basedir    = arc_basedir
                                 , opt_disk_basedir   = disk_basedir
                                 , opt_arccmt_file    = arccmt_file
                                 , opt_unParseFile    = unParseFile
                                 } = do
    -- Extreme memory saving: find_archives -> buffer 10_000 -> read_dir -> buffer 10_000 -> arcExtract
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []  -- tell the user that processing of the next archive has begun
  uiStage "0249 Reading archive directory"
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- prepare the passwords in the command for use
  let openArchive = archiveReadInfo command arc_basedir disk_basedir archive_filter (pretestArchive command)
  bracketCtrlBreak "arcClose:ArcExtract" (openArchive arcname) arcClose$ \archive -> do
    uiPrintArcComment (arcComment archive)            -- Print the comment
    when (arccmt_file/="-" && arccmt_file/="--") $    -- and write it to the file given by the -z option
      unParseFile 'c' arccmt_file (arcComment archive)
    arcExtract command archive
  uiDoneArchive  -- print the command execution statistics and return them to the caller

-- |Archive extraction
arcExtract command arcinfo = do
  -- The procedure used to process each file
  let process_file = case cmd_name command of
                       "t"  -> test_file
                       _    -> extractFile (fpFullname . fiDiskName) command
  -- Show in the UI the total size of the files to extract and the size of the already extracted archive directory
  uiStartProcessing (map cfFileInfo (arcDirectory arcinfo))  (arcDataBytes arcinfo)  (arcDataCBytes arcinfo)
  uiStartDirectory
  uiUnpackedBytes   (arcDirBytes  arcinfo)
  uiCompressedBytes (arcDirCBytes arcinfo)
  uiStartFiles 0
  -- Create the process that decompresses files and guarantee its correct shutdown
  bracket (runAsyncP$ decompress_PROCESS command (uiCompressedBytes.i))
          ( \decompress_pipe -> do sendP decompress_pipe Nothing; joinP decompress_pipe)
          $ \decompress_pipe -> do
  -- Extract every extractable file and complain about the ones that cannot be extracted
  let (filesToSkip, filesToExtract)  =  partition isCompressedFake (arcDirectory arcinfo)
  forM_ filesToExtract (process_file decompress_pipe)   -- runP$ enum_files |> decompress |> write_files
  unless (null filesToSkip)$  registerWarning$ SKIPPED_FAKE_FILES (length filesToSkip)

-- |Testing a single file from the archive
test_file decompress_pipe compressed_file = do
  uiStartFile (cfFileInfo compressed_file)
  runDecompress decompress_pipe compressed_file (\buf size -> return ())
  return ()

-- |Extracting a single file from the archive
extractFile filename_func command decompress_pipe compressed_file = do
  let fileinfo  = cfFileInfo compressed_file
      filename  = filename_func fileinfo
  if fiIsDir fileinfo
    then do uiStartFile fileinfo
            createDirectoryHierarchy filename
    else do
  -- Continue provided that this file is allowed to be extracted
  whenM (canBeExtracted command filename fileinfo)$ do
    uiStartFile fileinfo
    buildPathTo filename
    outfile  <- fileCreate filename
    let closeOutfile ok = do   -- Procedure run after the file is extracted or on exit via ^Break
          fileClose outfile                              -- to do: if fileSetSize is used, resize the file to match the number of bytes actually extracted
          if ok || opt_keep_broken command
            then do setFileDateTimeAttr filename fileinfo   -- Extracted successfully, or files extracted with errors must be kept too
                    when (opt_clear_archive_bit command) $ clearArchiveBit filename            -- Option -ac - clear the Archive attribute after extraction
            else fileRemove filename                     -- Delete the file that was extracted with errors
    do  --fileSetSize outfile (fiSize fileinfo)  -- A decent OS will then allocate disk space for the file in one contiguous chunk
        handleCtrlBreak "closeOutfile" (closeOutfile False) $ do
          ok <- runDecompress decompress_pipe compressed_file (fileWriteBuf outfile)
          closeOutfile ok


-- |This function decides whether a file may be extracted from the archive
-- The answer depends on 1) the options used (-u/-f/-sync)
--                  2) whether a previous file exists on disk
--                  3) which of the files is newer - the one on disk or the one in the archive
--                  4) the values of the "-o" and "y" options
--                  5) the user's answer to the overwrite prompt
--
canBeExtracted cmd filename arcfile = do
  diskfile_exist <- fileExist filename
  if not diskfile_exist                         -- If the file does not exist on disk
    then return (opt_update_type cmd /= 'f')    -- then the file may be extracted in every case except '-f'
    else do
  fileWithStatus "getFileInfo" filename $ \p_stat -> do
  diskFileIsDir  <-  stat_mode  p_stat  >>==  s_isdir
  diskFileTime   <-  stat_mtime p_stat
  diskFileSize   <-  if diskFileIsDir then return 0
                                      else stat_size p_stat
  let arcfile_newer  =  fiTime arcfile > diskFileTime   -- is the file in the archive newer than the one on disk?
  let overwrite = case opt_update_type cmd of
                    'f' -> arcfile_newer
                    'u' -> arcfile_newer
                    's' -> error "--sync can't be used on extract"
                    'a' -> True
  if not overwrite  then return False  else do
  askOverwrite filename diskFileSize diskFileTime arcfile (opt_overwrite cmd) arcfile_newer


{-# NOINLINE runDecompress #-}
-- |Extracting a file from the archive with CRC checking
runDecompress decompress_pipe compressed_file write_data = do
  crc <- ref aINIT_CRC                        -- Initialize the CRC value
  let writer buf len = do
        uiUnpackedBytes  (i len)              -- Inform the user about extraction progress
        uiUpdateProgressIndicator (i len)     -- -.-
        crc          .<- updateCRC buf len    -- Update the CRC with the buffer contents
        write_data       buf len              -- Write the data to the file
        send_backP       decompress_pipe ()   -- And return the used buffer
  decompress_file decompress_pipe compressed_file writer
  acrc  <-  val crc >>== finishCRC            -- Compute the final CRC value
  when (cfCRC compressed_file /= acrc) $ registerWarning$ BAD_CRC (fpFullname$ fiStoredName$ cfFileInfo compressed_file)
  return (cfCRC compressed_file == acrc)      -- Return True if everything is OK


----------------------------------------------------------------------------------------------------
---- Writing the archive comment to a file (the "cw" command)                                   ----
----------------------------------------------------------------------------------------------------

-- |Implementation of the "cw" command - writing the archive comment to a file
runCommentWrite command@Command{ cmd_filespecs   = filespecs
                               , cmd_arcname     = arcname
                               , opt_unParseFile = unParseFile
                               } = do
  doFinally uiDoneArchive2 $ do
  when (length filespecs /= 1) $
    registerError$ CMDLINE_SYNTAX "cw archive outfile"
  let [outfile] = filespecs
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- prepare the passwords in the command for use
  printLineLn$ "Writing archive comment of "++arcname++" to "++outfile
  bracket (archiveReadFooter command arcname) (archiveClose.fst) $ \(_,footer) -> unParseFile 'c' outfile (ftComment footer)
  return (0,0,0,0)


----------------------------------------------------------------------------------------------------
---- Printing the archive listing:                                                              ----
----     - for the user (the "l" command)                                                       ----
----     - for building file lists (the "lb" command)                                           ----
----     - for other programs (the "v" command)                                                 ----
---------------------------------------------------------------------------------------------------

-- |Generic archive listing command
runArchiveList pretestArchive
               command@Command{ cmd_arclist        = arclist
                              , cmd_arcname        = arcname
                              , opt_arc_basedir    = arc_basedir
                              , cmd_archive_filter = archive_filter
                              } = do
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- prepare the passwords in the command for use
  bracket (archiveReadInfo command arc_basedir "" archive_filter (pretestArchive command) arcname) arcClose $
      archiveList command (null$ tail arclist)

-- |Archive listing
archiveList command@Command{ cmd_name = cmd, cmd_arcname = arcname }
            show_empty
            arc@ArchiveInfo{ arcDirectory = directory } = do
  let files = length directory
      bytes = sum$ map (fiSize . cfFileInfo) directory
  when (files>0 || show_empty) $ do
    doFinally uiDoneArchive2 $ do
    uiStartArchive command [] -- Tell the user that processing of the next archive has begun
    let list line1 line2 list_func linelast = do
                uiPrintArcComment (arcComment arc)
                myPutStrLn line1
                myPutStrLn line2
                compsize <- list_func
                myPutStrLn linelast
                myPutStr$   show3 files ++ " files, " ++ show3 bytes ++ " bytes, " ++ show3 compsize ++ " compressed"
    case cmd of
      "l" -> list "Date/time                  Size Filename"
                  "----------------------------------------"
                  (myMapM terseList directory)
                  "----------------------------------------"

      "v" -> list "Date/time              Attr            Size          Packed      CRC Filename"
                  "-----------------------------------------------------------------------------"
                  (myMapM verboseList directory)
                  "-----------------------------------------------------------------------------"

      "lb"-> myPutStr$ joinWith "\n"$ map filename directory

      "lt"-> list "              Pos            Size      Compressed   Files Method"
                  "-----------------------------------------------------------------------------"
                  (do mapM_ dataBlockList (arcDataBlocks arc)
                      return (sum$ map blCompSize (arcDataBlocks arc)))
                  "-----------------------------------------------------------------------------"
  return (1, files, bytes, -1)


-- |File name
filename = fpFullname . fiStoredName . cfFileInfo

-- |Adds solid block compressed size information to the listing commands
myMapM f = go 0 True undefined
 where
  go total first lastSolidBlock [] = return total
  go total first lastSolidBlock (file:rest) = do
    let solidBlock = cfArcBlock file
    let compsize = if first  ||  blPos solidBlock /= blPos lastSolidBlock
                     then blCompSize solidBlock
                     else 0
    f file compsize
    (go $! total+compsize) False solidBlock rest


-- |Single-line terse listing of a file
terseList direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        formatDateTime (fiTime fi)
           ++ " " ++ right_justify 11 (if fiIsDir fi then "-dir-" else show3$ fiSize fi)
                  ++ (if cfIsEncrypted direntry  then "*"  else " ")
                  ++ filename direntry

-- |Single-line verbose listing of a file
verboseList direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        formatDateTime (fiTime fi)
           ++ " " ++ (if fiIsDir fi  then ".D....."  else ".......")
           ++ " " ++ right_justify 15 (show$ fiSize fi)
           ++ " " ++ right_justify 15 (show compsize)
           ++ " " ++ left_fill  '0' 8 (showHex (cfCRC direntry) "")
                  ++ (if cfIsEncrypted direntry  then "*"  else " ")
                  ++ filename direntry

{-
-- |Multi-line technical listing of a file
technical_list direntry = do
  let fi = (cfFileInfo direntry)
  timestr <- formatDateTime (fiTime fi)
  myPutStrLn$ ""
  myPutStrLn$ "Filename: "  ++ (fpFullname$ fiStoredName fi)
  myPutStrLn$ "Size: "      ++ (show$ fiSize fi)
  myPutStrLn$ "Date/time: " ++ timestr
  myPutStrLn$ "CRC: "       ++ showHex (cfCRC direntry) ""
  myPutStrLn$ "Type: "      ++ if (fiIsDir fi) then "directory" else "file"
-}

-- |Solid block description
dataBlockList bl = myPutStrLn$        (if blIsEncrypted bl  then "*"  else " ")
         ++ " " ++ right_justify 15 (show3$ blPos      bl)
         ++ " " ++ right_justify 15 (show3$ blOrigSize bl)
         ++ " " ++ right_justify 15 (show3$ blCompSize bl)
         ++ " " ++ right_justify  7 (show3$ blFiles    bl)
         ++ " " ++ join_compressor (blCompressor bl)

