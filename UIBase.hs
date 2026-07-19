{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Informing the user about the program's progress (CUI - Console User Interface). ---------------
----------------------------------------------------------------------------------------------------
module UIBase where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char
import Data.IORef
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.IO.Unsafe
import System.Time
#ifdef FREEARC_UNIX
import System.Posix.IO
import System.Posix.Terminal
#endif

import Utils
import Errors
import Files
import FileInfo
import Options


-- |This holds all the information about the command and its execution needed to display
-- the progress indicator and print the final statistics
data UI_State = UI_State {
    total_files     :: !FileCount   -- Number of files it has to process
  , total_bytes     :: !FileSize    -- Total size of those files (uncompressed)
  , archive_total_bytes      :: !FileSize    -- Total size of the files in the archive - set only for extraction commands
  , archive_total_compressed :: !FileSize    -- Total size of the files in the archive (compressed)
  , datatype        ::  DataType    -- The part of the archive currently being processed: file/directory/control data
  , uiFileinfo      :: !(Maybe FileInfo)  -- The file currently being processed (if any)
  -- Depending on which part of the archive is being processed, the statistics are credited
  -- either to the files account:
  ,    files        :: !FileCount   -- Number of files already processed
  ,    bytes        :: !FileSize    -- Amount of data already processed, uncompressed
  ,    cbytes       :: !FileSize    -- Amount of data already processed, compressed
  -- or to the directories account (control data is not counted):
  ,    dirs         :: !FileCount   -- Number of directory blocks and other control blocks created
  ,    dir_bytes    :: !FileSize    -- Amount of data already processed, uncompressed
  ,    dir_cbytes   :: !FileSize    -- Amount of data already processed, compressed
  -- In addition, we remember what part of this data was in fact not compressed at all (useful for computing the real compression speed):
  ,    fake_files   :: !FileCount   -- Number of files already processed
  ,    fake_bytes   :: !FileSize    -- Amount of data already processed, uncompressed
  ,    fake_cbytes  :: !FileSize    -- Amount of data already processed, compressed
  -- Information about the current solid block
  ,    algorithmsCount :: Int       -- Number of algorithms in the chain
  ,    rw_ops       :: [[UI_RW FileSize]] -- Sequence of read/write operations, split per individual algorithm
  ,    r_bytes      :: !FileSize     -- Amount of data already processed at the input of the first compression algorithm
  ,    rnum_bytes   :: !FileSize     -- Amount of data already processed at the input of the last compression algorithm
  }

-- |The part of the archive currently being processed: file/directory/control data
data DataType = File | Dir | CData   deriving Eq

-- |Read and write operations in the operations list
data UI_RW a = UI_Read a | UI_Write a

-- |Indicator type - percentages only, or also files/...
data IndicatorType = INDICATOR_PERCENTS | INDICATOR_FULL   deriving Eq


-- The command currently being executed
ref_command               =  unsafePerformIO$ newIORef$ error "undefined UI::ref_command"
-- The archive being processed (differs from command.$cmd_arcname when testing a temporary archive after compression)
uiArcname                 =  unsafePerformIO$ newIORef$ error "undefined UI::uiArcname"
refStartArchiveTime       =  unsafePerformIO$ newIORef$ error "undefined UI::refStartArchiveTime"
refStartPauseTime         =  unsafePerformIO$ newIORef$ error "undefined UI::refStartPauseTime"
refArchiveProcessingTime  =  unsafePerformIO$ newIORef$ error "undefined UI::refArchiveProcessingTime"  :: IORef Double
ref_ui_state              =  unsafePerformIO$ newIORef$ error "undefined UI::ref_ui_state"
putHeader                 =  unsafePerformIO$ init_once
-- The current stage of command execution, or the file name from uiFileinfo
uiMessage                 =  unsafePerformIO$ newIORef$ ""
-- |Counter of scanned files
files_scanned             =  unsafePerformIO$ newIORef$ (0::Integer)

-- |Global variable holding the progress indicator state
aProgressIndicatorState    =  unsafePerformIO$ newIORef$ error "undefined UI::aProgressIndicatorState"
aProgressIndicatorEnabled  =  unsafePerformIO$ newIORef$ False
-- |Start time of the current indicator's countdown
indicator_start_real_secs  =  unsafePerformIO$ newIORef$ (0::Double)

-- |Synchronization of access to the UI
syncUI = withMVar mvarSyncUI . const;  mvarSyncUI = unsafePerformIO$ newMVar "mvarSyncUI"


-- |Thread that watches the indicator and prints its updated values from time to time
indicatorThread secs output =
  backgroundThread secs $ do
    whenM (val aProgressIndicatorEnabled) $ do
      operationTerminated' <- val operationTerminated
      (indicator, indType, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
      when (indicator /= NoIndicator  &&  not operationTerminated') $ do
        bytes <- bytes' b;  total <- total'
        -- Ratio of the amount of processed data to the total amount
        let processed = total>0 &&& (fromIntegral bytes / fromIntegral total :: Double)
        secs <- return_real_secs
        sec0 <- val indicator_start_real_secs
        let remains  = if processed>0.001  then " "++showHMS(sec0+(secs-sec0)/processed-secs)  else ""
            winTitle = "{"++trimLeft p++remains++"}" ++ direction ++ takeFileName arcname
            p        = percents indicator bytes total
        output indicator indType winTitle b bytes total processed p

-- |Run action in the background every secs seconds
backgroundThread secs action =
  forkIO $ do
    foreverM $ do
      threadDelay (round$ secs*1000000)
      syncUI $ do
        action

{-# NOINLINE indicatorThread #-}
{-# NOINLINE backgroundThread #-}

----------------------------------------------------------------------------------------------------
---- Progress indicator ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Progress indicator types (silent, percents, tenths of a percent)
data Indicator = NoIndicator | ShortIndicator | LongIndicator   deriving (Eq)

bytes_per_sec = 1*mb  -- Typical (de)compression speed

-- |Choose the progress indicator based on the testimony of witnesses :)
select_indicator command total_bytes  =  case (opt_indicator command)
  of "0"                                    ->  NoIndicator      -- the "-i" option - disable the indicator!
     _ | i total_bytes < bytes_per_sec*100  ->  ShortIndicator   -- percent indicator if the total data size is under 100 mb (in that case more than one percent of the data is processed per second)
       | otherwise                          ->  LongIndicator    -- tenths-of-a-percent indicator if there is more than 100 mb of data

-- |Render the progress indicator with the chosen precision
percents NoIndicator    current total  =  ""
percents ShortIndicator current total  =  right_justify 3 (ratio2 current total) ++ "%"
percents LongIndicator  current total  =  right_justify 5 (ratio3 current total) ++ "%"

-- |Make room for the progress indicator
open_percents     =  flip replicate ' '  . indicator_len
-- |Move back by as many characters as the progress indicator occupies
back_percents     =  flip replicate '\b' . indicator_len
-- |Print spaces over the progress indicator that was displayed
clear_percents i  =  back_percents i ++ open_percents i

-- |Size of the progress indicator in characters
indicator_len NoIndicator    = 0
indicator_len ShortIndicator = 4
indicator_len LongIndicator  = 6

-- |Format percent ratio with 2 digits
ratio2 count 0     =  "0"
ratio2 count total =  show$ count*100 `div` total

-- |Format percent ratio with 2+1 digits
ratio3 count 0     =  "0.0"
ratio3 count total =  case (show$ count*1000 `div` total) of
                        [digit]  -> "0." ++ [digit]
                        digits   -> init digits ++ ['.', last digits]

-- |Print a number separating thousands, millions, etc.: "1.234.567"
show3 :: (Show a) => a -> [Char]
show3 = reverse . xxx . reverse . show
          where xxx (a:b:c:d:e) = a:b:c:'.': xxx (d:e)
                xxx a = a

{-# NOINLINE ratio2 #-}
{-# NOINLINE ratio3 #-}
{-# NOINLINE show3 #-}


----------------------------------------------------------------------------------------------------
---- Helper functions for formatting numbers/strings and working with time -------------------------
----------------------------------------------------------------------------------------------------

-- |Difference between two times in seconds - relies on the details of the internal representation!!!
diffTimes (TOD sa pa) (TOD sb pb)  =  i(sa - sb) + (i(pa-pb) / 1e12)

-- |Add seconds to a time value
addTime (TOD sa pa) secs  = TOD (sa+sb+sc) pc
  where
    sb = i$ floor secs
    pb = round$ (secs - fromIntegral sb)*1e12
    (sc,pc) = (pa+pb) `divMod` (10^12)

-- |Return the time in Unix format (seconds since goodness knows when)
getUnixTime = do
  (TOD seconds picoseconds) <- getClockTime
  return seconds

-- |Print the size of the original and compressed data, and the compression ratio
show_ratio cmd bytes cbytes =
  ""        ++ show3       (if (cmdType cmd == ADD_CMD) then bytes else cbytes) ++
   " => "   ++ show_bytes3 (if (cmdType cmd == ADD_CMD) then cbytes else bytes) ++ ". " ++
   "Ratio " ++ ratio3 cbytes bytes ++ "%"

-- |Return a string describing the given time
showTime secs  =  showFFloat (Just 2) secs " secs"

-- |Return a string describing the given speed
showSpeed bytes secs  =  show3(round$ i bytes/1000/secs) ++ " kB/s"

-- |Format a time as H:MM:SS
showHMS secs  =  show hour++":"++left_fill '0' 2 (show min)++":"++left_fill '0' 2 (show sec)
  where
    s = round secs
    sec = (s `mod` 60)
    min = (s `div` 60) `mod` 60
    hour= (s `div` 3600)



-- |Record the time when a certain point in the program was reached (purely for internal benchmarks)
debugLog label = do
  condPrintLine   "$" $  label   -- evaluate label and print its value
  real_secs <- return_real_secs
  condPrintLineLn "$" $  ": " ++ showTime real_secs

-- |Print information about the list if it contains at least two elements
debugLogList label list = do
  drop 1 list &&& debugLog (format label (show3$ length list))

-- |Append a line to the program's debug output
debugLog0 = condPrintLineLn "$"

-- |Time actually elapsed since the command started running on the current archive
return_real_secs = do
  start_time    <- val refStartArchiveTime
  current_time  <- getClockTime
  return$ diffTimes current_time start_time

-- Subtract the time spent paused from the real command execution time
pause_real_secs = do
  refStartPauseTime =:: getClockTime

resume_real_secs = do
  start_time    <- val refStartPauseTime
  current_time  <- getClockTime
  let pause = diffTimes current_time start_time :: Double
  refStartArchiveTime .= (`addTime` pause)

pauseTiming = bracket_ pause_real_secs resume_real_secs

{-# NOINLINE diffTimes #-}
{-# NOINLINE show_ratio #-}
{-# NOINLINE debugLog #-}


----------------------------------------------------------------------------------------------------
---- Choosing the messages corresponding to the command being executed -----------------------------
----------------------------------------------------------------------------------------------------

msgStart cmd arcExist =
                case (cmdType cmd, arcExist) of
                  (ADD_CMD,     False)  ->  "Creating archive: "
                  (ADD_CMD,     True)   ->  "Updating archive: "
                  (LIST_CMD,    _)      ->  "Listing archive: "
                  (TEST_CMD,    _)      ->  "Testing archive: "
                  (EXTRACT_CMD, _)      ->  "Extracting archive: "
                  (RECOVER_CMD, _)      ->  "Recovering archive: "

msgDo cmd    =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressing "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extracting "

msgFile      =  ("  " ++) . msgDo

msgDone cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressed "
                  TEST_CMD    -> "Tested "
                  EXTRACT_CMD -> "Extracted "

msgStat cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compression "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extraction "

-- |Print "file" or "files", depending on the count
show_files3 1 = "1 file"
show_files3 n = show3 n ++ " files"

-- |Print "archive" or "archives", depending on the count
show_archives3 1 = "1 archive"
show_archives3 n = show3 n ++ " archives"

-- |Print "byte" or "bytes", depending on the count
show_bytes3 1 = "1 byte"
show_bytes3 n = show3 n ++ " bytes"


{-
  UI structure:
  - a single process that receives information from compression/decompression and defines the structure
      of the interaction with the UI:
        ui_PROCESS pipe = do
          (StartCommand cmd) <- receiveP pipe
            (StartArchive cmd) <- receiveP pipe
              (StartFile fi fi) <- receiveP pipe
                (UnpackedData n) <- receiveP pipe
                (CompressedData n) <- receiveP pipe
            (EndArchive) <- receiveP pipe
          (EndCommand) <- receiveP pipe
         (EndProgram) <- receiveP pipe
      This process writes the current UI state into a SampleVar
-}
