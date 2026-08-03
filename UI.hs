{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Collection and display of program run statistics (amount of data processed, speed, etc.) ------
----------------------------------------------------------------------------------------------------
module UI (module UI, module UIBase, module CUI) where
import CUI

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Data.IORef
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.IO.Unsafe
import System.Time

import Utils
import Errors
import Charsets
import Files
import FileInfo
import Compression (encode_method, showMem, getCompressionMem, getDecompressionMem)
import qualified ByteStream
import Options
import UIBase


-- |Note the start of program execution
uiStartProgram = do
  guiStartProgram

-- |Note the start of command execution
uiStartCommand command = do
  ref_command =: command
  display_option' =: opt_display command
  refStartArchiveTime =:: getClockTime
  -- Open the log file and write the command being executed into it. Long comments/file lists/file names
  -- should not end up in the log file, so we truncate the list and every string in it to 100 elements
  openLogFile (opt_logfile command)
  curdir <- getCurrentDirectory
  printLog (curdir++">arc "++unwords(map (takeSome 100 "...")$ hidePasswords$ takeSome 100 ["..."]$ cmd_args command)++"\n")
  -- Print the archiver version and the additional options in use
  let addArgs = cmd_additional_args command
  once putHeader$ condPrintLine "h" aARC_HEADER
  condPrintLine "o" (addArgs &&& "Using additional options: "++unwords(hidePasswords addArgs)++"\n")
  myFlushStdout

-- |Note the start of subcommand execution
uiStartSubCommand command subCommand = do
  ref_command =: subCommand
  uiArcname   =: cmd_arcname command
  display_option' =: opt_display subCommand

-- |Note the start of processing of the next archive
uiStartArchive command@Command {
                 opt_data_compressor = compressor
               , opt_cache           = cache
               }
               method = do
  -- Remember the archive processing start time and the command being executed
  refStartArchiveTime =:: getClockTime
  ref_command =: command
  display_option' =: opt_display command
  perform_shutdown =: opt_shutdown command  -- FreeArc 0.67 --shutdown/-ioff
  pause_before_exit_mode =: opt_pause_before_exit command  -- FreeArc 0.67 --pause-before-exit
  ByteStream.legacy32bitRead =: opt_arc_32bit_legacy command  -- --arc-32bit-legacy: read 32-bit FreeArc archives
  uiMessage =: ""

  -- The rest of the procedure need not run if this is a subcommand (for example, testing after archiving)
  if cmd_subcommand command
    then do condPrintLineNeedSeparator "" "\n"
    else do

  -- Print a message like "Testing archive ..."
  let cmd     = cmd_name    command
      arcname = cmd_arcname command
  uiArcname =: arcname
  exist <- fileExist arcname
  condPrintLine "a"  $ (msgStart cmd exist) ++ arcname
  condPrintLine "c"  $ (method &&& " using "++encode_method method)
  condPrintLine "ac" $ "\n"
  when (cmdType cmd == ADD_CMD) $ do
      condPrintLineLn "m" $
          "Memory for compression "++showMem (getCompressionMem   method)
          ++", decompression "     ++showMem (getDecompressionMem method)
          ++", cache "             ++showMem cache

-- |Note the start of data compression or decompression
uiStartProcessing filelist archive_total_bytes archive_total_compressed = do
  refArchiveProcessingTime =: 0
  command <- val ref_command
  let cmd = cmd_name command
      total_files' = i$ length filelist
      total_bytes' = sum (map fiSize filelist)
      ui_state = UI_State {
          total_files     = total_files'
        , total_bytes     = total_bytes'
        , archive_total_bytes      = archive_total_bytes
        , archive_total_compressed = archive_total_compressed
        , datatype        = error "internal CUI error: datatype not initialized"
        , uiFileinfo      = Nothing
        , files           = 0
        , bytes           = 0
        , cbytes          = 0
        , dirs            = 0
        , dir_bytes       = 0
        , dir_cbytes      = 0
        , fake_files      = 0
        , fake_bytes      = 0
        , fake_cbytes     = 0
        , algorithmsCount = error "internal CUI error: algorithmsCount not initialized"
        , rw_ops          = error "internal CUI error: rw_ops not initialized"
        , r_bytes         = 0
        , rnum_bytes      = 0
        }
  ref_ui_state =: ui_state
  printLine$ msgDo cmd ++ show_files3 total_files' ++ ", "
                       ++ show_bytes3 total_bytes'
  -- Printing this "separator" lets us overwrite the on-screen line holding the current statistics
  printLineNeedSeparator $ "\r"++replicate 75 ' '++"\r"
  when (opt_indicator command == "1") $ do
    myPutStr$ ". Processed "
  -- The complex progress indicator also accounts for the percentage of processed files, which makes it move more smoothly
  -- Data and files are mixed assuming: compression speed 1mb/s, file open time 10 msec
  let current bytes = do ui_state <- val ref_ui_state
                         return$ bytes + (bytes_per_sec `div` 100)*i (files ui_state)
      total = do ui_state <- val ref_ui_state
                 return$ total_bytes ui_state + (bytes_per_sec `div` 100)*i (total_files ui_state)
  uiStartProgressIndicator INDICATOR_FULL command current total
  myFlushStdout


-- |Note the current stage of the process
uiStage msg = do
  syncUI $ do
  uiMessage =:: i18n msg

-- |Reset the counter of scanned files
uiStartScanning = do
  files_scanned =: 0

-- |Called while scanning the disk; files is the list of files found in the current directory.
-- The body only ever ran under the GUI, which has been removed, so this is now
-- the no-op the console build always saw.
uiScanning msg files = return ()

-- |Note the start of file compression/decompression
uiStartFiles count = do
  syncUI $ do
  modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
    ui_state { datatype        = File
             , algorithmsCount = count
             , rw_ops          = (replicate count [] :: [[UI_RW FileSize]])
             , r_bytes         = (0 :: FileSize)
             , rnum_bytes      = (0 :: FileSize)
             }

-- |Note the start of compression/decompression of the archive directory
uiStartDirectory = do
  syncUI $ do
  modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
    ui_state { datatype        = Dir
             , dirs            = dirs ui_state + 1
             , algorithmsCount = 0 }

-- |Note the start of compression/decompression of the archive control data
uiStartControlData = do
  syncUI $ do
  modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
    ui_state { datatype        = CData
             , algorithmsCount = 0 }

-- |Note the start of compression/decompression of a file
uiStartFile fileinfo = do
  syncUI $ do
    uiMessage =: (fpFullname . fiStoredName) fileinfo  ++  (fiIsDir fileinfo &&& "/")
    modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
      ui_state { datatype   = File
               , uiFileinfo = Just fileinfo
               , files      = files ui_state + 1}
  guiStartFile

-- |Adjust total_bytes in ui_state
uiCorrectTotal files bytes = do
  when (files/=0 || bytes/=0) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
      ui_state { total_files = total_files ui_state + files
               , total_bytes = total_bytes ui_state + bytes }

-- |Note the simulated processing of the files in the supplied list
uiFakeFiles filelist compsize = do
  let origsize  =  sum (map fiSize filelist)
  syncUI $ do
    modifyIORef ref_ui_state $ \(ui_state :: UI_State) ->
      ui_state { datatype    = File
               , files       = (files       ui_state) + (i$ length filelist)
               , fake_files  = (fake_files  ui_state) + (i$ length filelist)
               , fake_bytes  = (fake_bytes  ui_state) + origsize
               , fake_cbytes = (fake_cbytes ui_state) + compsize
               }
  uiUnpackedBytes           origsize
  uiCompressedBytes         compsize
  uiUpdateProgressIndicator origsize

-- |Note that this many bytes of compressed data have been processed (no matter whether it is
-- the result of compression, the input for decompression, or simply compressed data copied
-- from the old archive into the new one without any recompression)
uiCompressedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     cbytes =     cbytes ui_state + len }
      Dir   ->  ui_state { dir_cbytes = dir_cbytes ui_state + len }
      CData ->  ui_state

-- |Note that this many bytes of unpacked data have been processed (even if in reality
-- nobody ever saw those bytes, since they were copied between archives still compressed)
uiUnpackedBytes len = do
  syncUI $ do
  modifyIORef ref_ui_state $ \ui_state ->
    case (datatype ui_state) of
      File  ->  ui_state {     bytes =     bytes ui_state + len }
      Dir   ->  ui_state { dir_bytes = dir_bytes ui_state + len }
      CData ->  ui_state

-- |Note the start of compression or decompression of a solid block
uiStartDeCompression deCompression = do
  x <- getCPUTime
  newMVar (x,deCompression,[])

-- |Add to the list the running time of one of the algorithms in the chain
-- (the compressor/decompressor running time measured in the C thread)
uiDeCompressionTime times t =  do
  modifyMVar_ times (\(x,y,ts) -> return (x, y, ts++[t]))

-- |Compression/decompression of the solid block is finished - sum up the running time of all threads,
-- or use wall clock time if at least one of the returned times == -1
uiFinishDeCompression times = do
  (timeStarted, deCompression, results) <- takeMVar times
  timeFinished <- getCPUTime
  let deCompressionTimes  =  map snd3 results
  refArchiveProcessingTime +=  {-if (all (>=0) deCompressionTimes)      -- Commented out until all compression methods (lzma, grzip) will include timing for all threads
                                 then sum deCompressionTimes
                                 else-} i(timeFinished - timeStarted) / 1e12
  let total_times = if (all (>=0) deCompressionTimes)
                                 then " ("++showFFloat (Just 3) (sum deCompressionTimes) ""++" seconds)"
                                 else ""
  when (results>[]) $ do
    debugLog0$ "  Solid block "++deCompression++" results"++total_times
    for results $ \(method,time,size) -> do
        debugLog0$ "    "++method++": "++show3 size++" bytes in "++showFFloat (Just 3) time ""++" seconds"

-- |Processing of the current archive is finished -> print the statistics and return them to the caller
uiDoneArchive = do
  command <- val ref_command
  ui_state@UI_State { total_files   = total_files
                      , total_bytes   = total_bytes
                      , files         = files
                      , bytes         = bytes
                      , cbytes        = cbytes
                      , dirs          = dirs
                      , dir_bytes     = dir_bytes
                      , dir_cbytes    = dir_cbytes
                      , fake_files    = fake_files
                      , fake_bytes    = fake_bytes
                      , fake_cbytes   = fake_cbytes }  <-  val ref_ui_state
  let cmd = cmd_name command
  uiMessage =: ""
  uiDoneProgressIndicator
  when (opt_indicator command=="2" && files-fake_files>0) $ do
    myPutStrLn ""
    printLineNeedSeparator ""  -- the separator before printing the following lines is no longer needed

  -- Compression statistics (not printed for subcommands, since the main command has already printed exactly the same statistics)
  unless (cmd_subcommand command) $ do
    condPrintLineLn "f" $ left_justify 75 $    -- without the extra spaces the previous line may not be fully overwritten
      msgDone cmd ++ show_files3 files ++ ", " ++ show_ratio cmd bytes cbytes
    -- Print statistics for the archive directory only if it is large enough
    when (dir_bytes>10^4) $ do
      condPrintLine   "d" $ "Directory " ++ (dirs>1 &&& "has " ++ show3 dirs ++ " chunks, ")
      condPrintLineLn "d" $                 show_ratio cmd dir_bytes dir_cbytes

  -- Information about the running time and the compression/decompression speed
  secs <- val refArchiveProcessingTime   -- time spent directly on compression/decompression
  real_secs <- return_real_secs          -- total time spent executing the command on the current archive
  condPrintLine                     "t" $ msgStat cmd ++ "time: "++(secs>0 &&& "cpu " ++ showTime secs ++ ", ")
  condPrintLine                     "t" $ "real " ++ showTime real_secs
  when (real_secs>=0.01) $ condPrintLine "t" $ ". Speed " ++ showSpeed (bytes-fake_bytes) real_secs

  condPrintLineNeedSeparator "rdt" "\n"
  myFlushStdout
  resetConsoleTitle
  return (1,files,bytes,cbytes)

-- |Called after all auxiliary operations (adding recovery info, testing)
uiDoneArchive2 = do
  command <- val ref_command
  unless (cmd_subcommand command) $ do
    condPrintLineNeedSeparator "" "\n\n"

-- |Subcommand execution finished
uiDoneSubCommand command subCommand results = do
  ref_command =: command
  display_option' =: opt_display command

-- |Command execution finished, print the summary statistics over all processed archives
uiDoneCommand Command{cmd_name=cmd} totals = do
  let sum4 (a0,b0,c0,d0) (a,b,c,d)   =  (a0+a,b0+b,c0+c,d0+d)
      (counts, files, bytes, cbytes) =  foldl sum4 (0,0,0,0) totals
  when (counts>1) $ do
    condPrintLine "s" $ "Total: "++show_archives3 counts++", "
                                 ++show_files3    files ++", "
                                 ++if (cbytes>=0)
                                     then show_ratio cmd bytes cbytes
                                     else show_bytes3 bytes
    condPrintLineNeedSeparator "s" "\n\n\n"

-- |Finish program execution
uiDoneProgram = do
  condPrintLineNeedSeparator "" "\n"


{-# NOINLINE uiStartProgram #-}
{-# NOINLINE uiStartArchive #-}
{-# NOINLINE uiStartProcessing #-}
{-# NOINLINE uiStartFile #-}
{-# NOINLINE uiCorrectTotal #-}
{-# NOINLINE uiUnpackedBytes #-}
{-# NOINLINE uiCompressedBytes #-}
{-# NOINLINE uiDoneArchive #-}
{-# NOINLINE uiDoneCommand #-}


----------------------------------------------------------------------------------------------------
---- Queue of r/w operations from which the progress indicator is computed during compression ------
----------------------------------------------------------------------------------------------------

-- Add a read/write operation to the head of the list, merging together operations of the same type
add_Read  a (UI_Write 0:UI_Read  0:ops) = (UI_Read a:ops)  -- get rid of the useless r0+w0 pair
add_Read  a (UI_Read  b:ops) = (UI_Read (a+b):ops)
add_Read  a             ops  = (UI_Read  a   :ops)

add_Write a (UI_Write b:ops) = (UI_Write(a+b):ops)
add_Write a             ops  = (UI_Write a   :ops)

-- |Algorithm number num in the chain promises to write bytes bytes corresponding to the last block of read
-- data (this "write promise" operation lets us keep the progress indicator accurate)
uiQuasiWriteData num bytes = do
  -- The implementation is arranged so that bytes of written data are attributed to the last data read,
  -- while the total size of written data does not change one bit ;)
  uiWriteData num bytes
  uiReadData  num 0
  uiWriteData num (-bytes)

-- |Algorithm number num in the chain wrote bytes bytes
uiWriteData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- Record a read operation in the list of I/O operations
  when (num>=1 && num<count) $ do
    syncUI $ do
    ui_state@UI_State {rw_ops=rw_ops0}  <-  val ref_ui_state
    let rw_ops = updateAt num (add_Write bytes) rw_ops0
    return $! length (take 4 (rw_ops!!num))   -- strictify operations list!
    ref_ui_state =: ui_state {rw_ops=rw_ops}

-- |Algorithm number num in the chain read bytes bytes
uiReadData num bytes = do
  UI_State {algorithmsCount=count, datatype=datatype} <- val ref_ui_state
  when (datatype == File) $ do
  -- Record a write operation in the list of I/O operations
  when (num>=1 && num<count) $ do
    syncUI $ do
    modifyIORef ref_ui_state $ \ui_state@UI_State {rw_ops=rw_ops} ->
      ui_state {rw_ops = updateAt num (add_Read bytes) rw_ops}

  -- Update the progress indicator if this is the last compression algorithm in the chain
  when (num>=1 && num==count) $ do
    unpBytes <- syncUI $ do
      -- State before these bytes were processed
      ui_state@UI_State {r_bytes=r_bytes0, rnum_bytes=rnum_bytes0, rw_ops=rw_ops0}  <-  val ref_ui_state
      -- bytes bytes are added to the input of algorithm num,
      -- compute the number of bytes at the input of the first algorithm if this block does not look like just a header (bytes>16)
      let rnum_bytes = rnum_bytes0+bytes
          (r_bytes, rw_ops) = if bytes>16
                                 then calc num (reverse rw_ops0) [] rnum_bytes
                                 else (r_bytes0, rw_ops0)
      ref_ui_state =: ui_state {r_bytes=r_bytes, rnum_bytes=rnum_bytes, rw_ops=rw_ops}
      --for rw_ops $ \x -> print (reverse x)
      --print (rnum_bytes0, bytes, r_bytes0, r_bytes-r_bytes0)
      -- Return the number of bytes at the input of the first algorithm, relative to its previous value
      return (r_bytes-r_bytes0)
    uiUpdateProgressIndicator ((unpBytes*9) `div` 10)
  when (num==1) $ do  -- 90% for the last algorithm in the chain and 10% for the first one (to smooth the output for external compression and so on)
    uiUpdateProgressIndicator (bytes `div` 10)

 where
  -- Recursively convert bytes bytes at the input of algorithm num into the number of bytes at the input of algorithm 1
  -- While at it, update the operation queue, summing up the operations preceding the current point of interest
  calc 1   _                new_ops bytes = (bytes, []:new_ops)
  calc num (old_op:old_ops) new_ops bytes =
    -- Convert bytes bytes at the output of algorithm num-1 into bytes at its input
    let (new_bytes, new_op) = go 0 bytes (0,0) (smart_reverse old_op)
    in calc (num-1) old_ops (reverse new_op:new_ops) new_bytes

  -- Reverse oplist, or simply replace it with two operations if it has more than 1000 elements
  -- (which most likely means these are the operations before a tempfile)
  smart_reverse oplist
      | length oplist < 1000  =  reverse oplist
      | otherwise             =  [UI_Read r, UI_Write w]  where (r,w) = go oplist
                                                                go (UI_Read  r:ops) = mapFst (+r) (go ops)
                                                                go (UI_Write w:ops) = mapSnd (+w) (go ops)
                                                                go []               = (0,0)

  -- Converts written bytes (restW) into read bytes (totalR) according to the sequence of I/O operations
  go totalR restW (rsum,wsum) ops@(UI_Read r:UI_Write w:rest_ops)
       -- If the next chunk of compressed data is larger than our remainder, add it to totalR and move on
       | w<restW    =  go (totalR+r) (restW-w) (rsum+r,wsum+w) rest_ops
       -- Otherwise split it proportionally (r/w * restW) and add that to totalR
       | otherwise  =  (totalR + ((r*restW) `div` max w 1), UI_Read rsum:UI_Write wsum:ops)
  -- All other cases
  go totalR _ (rsum,wsum) ops  =  (totalR, UI_Read rsum:UI_Write wsum:ops)


----------------------------------------------------------------------------------------------------
---- Progress indicator updating -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Initialize the progress indicator
uiStartProgressIndicator indType command bytes' total' = do
  bytes <- bytes' 0;  total <- total'
  arcname <- val uiArcname
  let cmd        =  cmd_name command
      direction  =  if (cmdType cmd == ADD_CMD)  then " => "  else " <= "
      indicator  =  select_indicator command total
  aProgressIndicatorState =: (indicator, indType, arcname, direction, 0, bytes', total')
  indicator_start_real_secs =:: return_real_secs
  uiResumeProgressIndicator

-- |Display the progress indicator on screen and in the window title (what percentage of the data is already processed)
uiUpdateProgressIndicator add_b =
  when (add_b/=0) $ do
    -- A little trick: this function is called BEFORE any data is actually
    -- processed. We assume that the previous data has already been processed by this moment and
    -- report it as such. The new data is merely added to the counter and does not affect
    -- the statistics displayed RIGHT NOW. Such are the quirks around here :)
    syncUI $ do
    (indicator, indType, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
    aProgressIndicatorState =: (indicator, indType, arcname, direction, b+add_b, bytes', total')

-- |Finish displaying the progress indicator
uiDoneProgressIndicator = do
  uiSuspendProgressIndicator
  aProgressIndicatorState =: (NoIndicator, undefined, undefined, undefined, undefined, undefined, undefined)

-- |Wrap command execution in opening and closing the progress indicator
uiWithProgressIndicator command arcsize action = do
  uiStartProgressIndicator INDICATOR_PERCENTS command return (return arcsize)
  ensureCtrlBreak "uiDoneProgressIndicator" uiDoneProgressIndicator action

{-# NOINLINE uiUpdateProgressIndicator #-}

