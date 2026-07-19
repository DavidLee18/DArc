{-# LANGUAGE CPP #-}
---------------------------------------------------------------------------------------------------
---- Registration of errors/warnings and printing of their messages. ------------------------------
---------------------------------------------------------------------------------------------------
module Errors where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import Data.IORef
import System.Exit
import System.IO
import System.IO.Unsafe
#if defined(FREEARC_WIN) && !defined(__MHS__)
import GHC.ConsoleHandler
#else
import System.Posix.Signals
import Foreign.C (CInt(..))
#endif

import CompressionLib   (compressionLib_cleanup)
import Utils
import Files
import Charsets

-- |Program return codes
aEXIT_CODE_SUCCESS      = 0
aEXIT_CODE_WARNINGS     = 1
aEXIT_CODE_FATAL_ERROR  = 2
aEXIT_CODE_BAD_PASSWORD = 21
aEXIT_CODE_USER_BREAK   = 255

-- |All possible error and warning types
data ErrorType  = GENERAL_ERROR                 [String]
                | CMDLINE_GENERAL               [String]
                | CMDLINE_SYNTAX                String
                | CMDLINE_INCOMPATIBLE_OPTIONS  String String
                | CMDLINE_NO_COMMAND            [String]
                | CMDLINE_NO_ARCSPEC            [String]
                | CMDLINE_NO_FILENAMES          [String]
                | UNKNOWN_CMD                   String [String]
                | CMDLINE_UNKNOWN_OPTION        String
                | CMDLINE_AMBIGUOUS_OPTION      String [String]
                | CMDLINE_BAD_OPTION_FORMAT     String
                | INVALID_OPTION_VALUE          String String [String]
                | CANT_READ_DIRECTORY           String
                | CANT_GET_FILEINFO             String
                | CANT_OPEN_FILE                String
                | UNSUPPORTED_METHOD            String
                | DATA_ERROR                    String
                | DATA_ERROR_ENCRYPTED          String
                | BAD_CRC                       String
                | BAD_CRC_ENCRYPTED             String
                | UNKNOWN_ERROR                 String
                | BAD_CFG_SECTION               String [String]
                | OP_TERMINATED
                | TERMINATED
                | NOFILES
                | SKIPPED_FAKE_FILES            Int
                | BROKEN_ARCHIVE                FilePath [String]
                | INTERNAL_ERROR                String
                | COMPRESSION_ERROR             [String]
                | BAD_PASSWORD                  FilePath FilePath
  deriving (Eq)

--foreign import "&errCounter" :: Ptr Int
{-
data SqliteException = SqliteException Int String
  deriving (Typeable)

catchSqlite :: IO a -> (SqliteException -> IO a) -> IO a
catchSqlite = catchDyn

throwSqlite :: SqliteException -> a
throwSqlite = throwDyn
-}

---------------------------------------------------------------------------------------------------
---- Handling of Ctrl-Break, Close and similar external events ------------------------------------
---------------------------------------------------------------------------------------------------

setCtrlBreakHandler action = do
  --myThread <- myThreadId
  -- On exit or when an exception occurs, restore the previous event handler
#if defined(FREEARC_WIN) && !defined(__MHS__)
  bracket (installHandler$ Catch onBreak) (installHandler) $  \oldHandler -> do
    action
#else
  let catchSignals a  =  installHandler sigINT (CatchOnce$ onBreak undefined) Nothing
  bracket (catchSignals (CatchOnce$ onBreak (error "onBreak"))) (catchSignals) $  \oldHandler -> do
    action
#endif

-- |Call fail if the abnormal-termination flag is set
failOnTerminated = do
  whenM (val operationTerminated) $ do
    fail ""

-- |Handling Ctrl-Break and pressing Cancel comes down to running the finalizers and
-- setting a special flag that is checked by the callbacks invoked from C
onBreak event = terminateOperation
terminateOperation = do
  isFM <- val fileManagerMode
  registerError$ iif isFM OP_TERMINATED TERMINATED

-- |Forcibly terminates the program with the given exitCode, printing the message msg
shutdown msg exitCode = do
  w <- val warnings
  -- Make cleanup unless this is a second call (after pause)
  unlessM (val programFinished) $ do
    programFinished =: True
    separator' =: ("","\n")
    log_separator' =: "\n"

    fin <- val finalizers
    for fin $ \(name,id,action) -> do
      ignoreErrors$ action
    compressionLib_cleanup

    unlessM (val fileManagerMode) $ do
      case w of
        0 -> when (exitCode==aEXIT_CODE_SUCCESS) $ condPrintLineLn "k" "All OK"
        _ -> condPrintLineLn "n"$ "There were "++show w++" warning(s)"
      ignoreErrors (msg &&& condPrintLineLn "n" msg)
      condPrintLineLn "e" ""
#if !defined(FREEARC_GUI)
    putStrLn ""
#endif

    ignoreErrors$ closeLogFile
    ignoreErrors$ hFlush stdout
    ignoreErrors$ hFlush stderr

    -- FreeArc 0.67 --shutdown: turn off computer when done
    whenM (val perform_shutdown) $ ignoreErrors powerOffComputer

    -- FreeArc 0.67 --pause-before-exit: optionally pause for the user
    pbe <- val pause_before_exit_mode
    let should_pause = case pbe of
          "on"          -> True
          "on-warnings" -> w > 0 || exitCode /= aEXIT_CODE_SUCCESS
          "on-error"    -> exitCode /= aEXIT_CODE_SUCCESS
          _             -> False
    when should_pause $ ignoreErrors $ do
      putStr "Press Enter to exit..."
      hFlush stdout
      _ <- getLine
      return ()

  -- And finally - exit program!
  exit (exitCode  |||  (w &&& aEXIT_CODE_WARNINGS))
#if 0
  -- A more correct way to terminate the program, but unfortunately arc.exe sometimes hangs with it
  exitWith$ case () of
   _ | exitCode>0 -> ExitFailure exitCode
     | w>0        -> ExitFailure aEXIT_CODE_WARNINGS
     | otherwise  -> ExitSuccess
#endif
  return undefined

-- |"handle" that also runs "onException" on ^Break
handleCtrlBreak name onException action = do
  failOnTerminated
  id <- newId
  handle (\(e :: SomeException) -> do onException; throwIO e) $ do
    bracket_ (addFinalizer name id onException)
             (removeFinalizer id)
             (action)

-- |"bracket" that also runs "close" on ^Break
bracketCtrlBreak name init close action = do
  failOnTerminated
  id <- newId
  bracket (do x<-init; addFinalizer name id (close x); return x)
          (\x -> do removeFinalizer      id; close x)
          action

-- |bracketCtrlBreak that runs fail when init returns Nothing
bracketCtrlBreakMaybe name init fail close action = do
  bracketCtrlBreak name (do x<-init; when (isNothing x) fail; return x)
                        (`whenJust_` close)
                        (`whenJust`  action)

-- |Run the close action once action has finished
ensureCtrlBreak name close action  =  bracketCtrlBreak name (return ()) (\_->close) (\_->action)

-- Add/remove a finalizer to/from the list
addFinalizer name id action  =  finalizers .= ((name,id,action):)
removeFinalizer id           =  finalizers .= filter ((/=id) . snd3)
newId                        =  do curId+=1; id<-val curId; return id

-- |Unique number
curId :: IORef Int
curId = unsafePerformIO (ref 0)
{-# NOINLINE curId #-}

-- |List of actions to be performed before the program terminates abnormally
finalizers :: IORef [(String, Int, IO ())]
finalizers = unsafePerformIO (ref [])
{-# NOINLINE finalizers #-}

-- |Flag indicating that we are in the middle of aborting the current operation
operationTerminated = unsafePerformIO (ref False)
{-# NOINLINE operationTerminated #-}

-- |Prevents finalization from running a second time after the pause
programFinished = unsafePerformIO (ref False)
{-# NOINLINE programFinished #-}

-- |FreeArc 0.67 --shutdown: turn off the computer after the operation finishes
perform_shutdown = unsafePerformIO (ref False)
{-# NOINLINE perform_shutdown #-}

pause_before_exit_mode = unsafePerformIO (ref "off")
{-# NOINLINE pause_before_exit_mode #-}

foreign import ccall unsafe "PowerOffComputer"
  powerOffComputer :: IO ()

-- |File manager mode: registerError is handled differently here - we wait for all compression and decompression threads to finish
fileManagerMode = unsafePerformIO (ref False)
{-# NOINLINE fileManagerMode #-}


---------------------------------------------------------------------------------------------------
---- Message texts for the various error types. A good resource for internationalization ----------
---------------------------------------------------------------------------------------------------

errormsg (GENERAL_ERROR msgs) =
  i18fmt msgs

errormsg (BROKEN_ARCHIVE arcname msgs) = do
  msg <- i18fmt msgs
  i18fmt ["0341 %1 isn't archive or this archive is corrupt: %2. Please recover it using 'r' command or use -tp- option to ignore Recovery Record", arcname, msg]

errormsg (INTERNAL_ERROR msg) =
  return$ "FreeArc internal error: "++msg

errormsg (COMPRESSION_ERROR msgs) =
  i18fmt msgs

errormsg (CMDLINE_GENERAL msgs) =
  i18fmt msgs

errormsg (CMDLINE_SYNTAX syntax) =
  i18fmt ["0318 command syntax is \"%1\"", syntax]

errormsg (CMDLINE_INCOMPATIBLE_OPTIONS option1 option2) =
  i18fmt ["0319 options %1 and %2 can't be used together", option1, option2]

errormsg (UNKNOWN_CMD cmd known_cmds) =
  i18fmt ["0320 unknown command \"%1\". Supported commands are: %2", cmd, joinWith ", " known_cmds]

errormsg (CMDLINE_UNKNOWN_OPTION option) =
  i18fmt ["0321 unknown option \"%1\"", option]

errormsg (CMDLINE_AMBIGUOUS_OPTION option variants) = do
  or <- i18n"0323 or"
  i18fmt ["0322 ambiguous option \"%1\" - is that %2?", option, enumerate or variants]

errormsg (CMDLINE_BAD_OPTION_FORMAT option) =
  i18fmt ["0325 option \"%1\" have illegal format", option]

errormsg (INVALID_OPTION_VALUE fullname shortname valid_values) = do
  or <- i18n"0323 or"
  let spelling | shortname>"" = (('-':shortname)++)
               | otherwise    = (("--"++fullname++"=")++)
  i18fmt ["0326 %1 option must be one of: %2", fullname, enumerate or (map spelling valid_values)]

errormsg (CMDLINE_NO_COMMAND args) =
  i18fmt ["0327 no command name in command: %1", unwords args]

errormsg (CMDLINE_NO_ARCSPEC args) =
  i18fmt ["0328 no archive name in command: %1", unwords args]

errormsg (CMDLINE_NO_FILENAMES args) =
  i18fmt ["0329 no filenames in command: %1", unwords args]

errormsg (CANT_READ_DIRECTORY dir) =
  i18fmt ["0330 can't read directory \"%1\"", dir]

errormsg (CANT_GET_FILEINFO filename) =
  i18fmt ["0331 can't get info about file \"%1\"", filename]

errormsg (CANT_OPEN_FILE filename) =
  i18fmt ["0332 can't open file \"%1\"", filename]

errormsg (UNSUPPORTED_METHOD filename) =
  i18fmt ["0472 Unsupported compression method for \"%1\".", filename]

errormsg (DATA_ERROR filename) =
  i18fmt ["0473 Data error in \"%1\". File is broken.", filename]

errormsg (DATA_ERROR_ENCRYPTED filename) =
  i18fmt ["0474 Data error in encrypted file \"%1\". Wrong password?", filename]

errormsg (BAD_CRC filename) =
  i18fmt ["0475 CRC failed in \"%1\". File is broken.", filename]

errormsg (BAD_CRC_ENCRYPTED filename) =
  i18fmt ["0476 CRC failed in encrypted file \"%1\". Wrong password?", filename]

errormsg (UNKNOWN_ERROR filename) =
  i18fmt ["0477 Unknown error", filename]

errormsg (BAD_CFG_SECTION cfgfile section) =
  i18fmt ["0334 bad section %1 in %2", head section, cfgfile]

errormsg (OP_TERMINATED) =
  i18fmt ["0455 Operation terminated by user!"]

errormsg (TERMINATED) =
  i18fmt ["0456 Program terminated by user!"]

errormsg (NOFILES) =
  i18fmt ["0337 no files, erasing empty archive"]

errormsg (SKIPPED_FAKE_FILES n) =
  i18fmt ["0338 skipped %1 fake files", show n]

errormsg (BAD_PASSWORD archive "") =
  i18fmt ["0339 bad password for archive %1", archive]

errormsg (BAD_PASSWORD archive file) =
  i18fmt ["0340 bad password for %1 in archive %2", file, archive]


-- |Enumerate a list of values
enumerate s list  =  joinWith2 ", " (" "++s++" ") (map quote list)

{-# NOINLINE errormsg #-}


----------------------------------------------------------------------------------------------------
---- Exit codes for the various errors -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

errcode TERMINATED     = aEXIT_CODE_USER_BREAK
errcode BAD_PASSWORD{} = aEXIT_CODE_BAD_PASSWORD
errcode _              = aEXIT_CODE_FATAL_ERROR


----------------------------------------------------------------------------------------------------
---- Screen input/output in the encoding specified by the -sct option ------------------------------
----------------------------------------------------------------------------------------------------

#ifdef FREEARC_GUI
myPutStr      = doNothing
myPutStrLn    = doNothing
myFlushStdout = doNothing0
#else
myGetLine     = getLine >>= terminal2str
myPutStr      = putStr   =<<. str2terminal
myPutStrLn    = putStrLn =<<. str2terminal
myFlushStdout = hFlush stdout
#endif


----------------------------------------------------------------------------------------------------
---- Logfile handling and control of the amount of screen output per the --display option ----------
----------------------------------------------------------------------------------------------------

-- Print the given string, separating it if necessary from the previous command/processed archive
-- In addition, the first letter of the printed string is lowercased
-- if it is printed immediately after the program banner
printLine = printLineC ""
printLineC c str = do
  (oldc,separator) <- val separator'
  let makeLower (x:y:zs) | isLower y  =  toLower x:y:zs
      makeLower xs                    =  xs
  let handle "w" = stderr
      handle _   = stdout
#ifndef FREEARC_GUI
  hPutStr (handle oldc) =<< str2terminal separator
  hPutStr (handle c)    =<< str2terminal ((oldc=="h" &&& makeLower) str)
  hFlush  (handle c)
#endif
  separator' =: (c,"")

-- |Print a string followed by a line separator
printLineLn str = do
  printLine str
  printLineNeedSeparator "\n"

-- Separate the following output with the given string. We don't print this string right away,
-- because there may well be no following output at all :)))
printLineNeedSeparator str = do
  separator' =: ("",str)

-- Write a string to the logfile.
-- Print it on screen provided that its output is not disabled by the --display option
condPrintLine c line = do
  if c=="G" then val loggingHandlers >>= mapM_ ($line) else do
  display_option <- val display_option'
  when (c `notElem` words "$ !" || (display_option `contains` '#')) $ do
      printLog line
  when (display_option `contains_one_of` c) $ do
      printLineC c line

-- |Print a string followed by a line separator
condPrintLineLn c line = do
  condPrintLine c line
  condPrintLineNeedSeparator c "\n"

-- Separate the following output with the given string, provided output of class c is enabled
condPrintLineNeedSeparator c str = do
  display_option <- val display_option'
  when (c `notElem` words "$ !" || (display_option `contains` '#')) $ do
      log_separator' =: str
  when (c=="" || (display_option `contains_one_of` c)) $ do
      separator' =: (c,str)

-- Open the logfile
openLogFile logfilename = do
  closeLogFile  -- close the previous one, if there was any
  logfile <- case logfilename of
                 ""  -> return Nothing
                 log -> fileAppendText log >>== Just
  logfile' =: logfile

-- Write a string to the logfile
printLog line = do
  separator <- val log_separator'
  whenJustM_ (val logfile') $ \log -> do
      fileWrite log =<< str2logfile (separator ++ line); fileFlush log
      log_separator' =: ""

-- Close the logfile
closeLogFile = do
  whenJustM_ (val logfile') fileClose
  logfile' =: Nothing

-- Variable holding the logfile Handle
logfile'        = unsafePerformIO$ newIORef Nothing
-- Variables used to prettify the printed output
separator'      = unsafePerformIO$ newIORef ("","") :: IORef (String,String)
log_separator'  = unsafePerformIO$ newIORef "\n"    :: IORef String
display_option' = unsafePerformIO$ newIORef ""      :: IORef String
-- Handler for messages sent to the log
loggingHandlers = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]

{-# NOINLINE printLine #-}
{-# NOINLINE printLineNeedSeparator #-}
{-# NOINLINE condPrintLine #-}
{-# NOINLINE condPrintLineNeedSeparator #-}
{-# NOINLINE separator' #-}
{-# NOINLINE log_separator' #-}
{-# NOINLINE display_option' #-}

----------------------------------------------------------------------------------------------------
---- Printing of error and warning messages
----------------------------------------------------------------------------------------------------

-- |Write an error message to the logfile and abort the program with that message
registerError err = do
  unless (err `elem` [TERMINATED,OP_TERMINATED]) $ do
    val errcodeHandler >>= ($err)
  msg <- errormsg err
  msg <- if err `elem` [TERMINATED,OP_TERMINATED]
           then return msg
           else i18fmt ["0316 ERROR: %1", msg]
  val errorHandlers >>= mapM_ (\h -> h msg `catch` (\(_::SomeException) -> return ()))
  -- In file manager mode we have to wait for all compression threads to finish,
  -- otherwise we simply abort the program
  unlessM (val fileManagerMode) $ do
    shutdown msg (errcode err)
      `catch` \(_::SomeException) -> exit (errcode err)
  operationTerminated =: True
  fail ""

-- |Write a warning to the logfile and print it on screen
registerWarning warn = do
  warnings += 1
  msg <- errormsg warn
  msg <- i18fmt ["0317 WARNING: %1", msg]
  val warningHandlers >>= mapM_ ($msg)
  condPrintLineLn "w" msg

-- |Run an operation and return the number of warnings it produced
count_warnings action = do
  w0 <- val warnings
  action
  w  <- val warnings
  return (w-w0)

-- |Counter of errors that occurred while the program was running
warnings = unsafePerformIO$ newIORef 0 :: IORef Int
-- |Number of warnings before the current main operation started
warningsBefore = unsafePerformIO$ newIORef 0 :: IORef Int

-- Depending on the mode, register either an error or a warning
registerThreadError err = do
  isFM <- val fileManagerMode
  (iif isFM registerWarning registerError) err

-- Actions performed when an error/warning occurs (registered in other parts of the program)
errcodeHandler  = unsafePerformIO$ newIORef doNothing :: IORef (ErrorType -> IO ())
errorHandlers   = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]
warningHandlers = unsafePerformIO$ newIORef [] :: IORef [String -> IO ()]

{-# NOINLINE registerError #-}
{-# NOINLINE registerWarning #-}
{-# NOINLINE warnings #-}
{-# NOINLINE warningsBefore #-}
{-# NOINLINE errcodeHandler #-}
{-# NOINLINE errorHandlers #-}
{-# NOINLINE warningHandlers #-}

----------------------------------------------------------------------------------------------------
---- File operations
----------------------------------------------------------------------------------------------------

-- |Return Nothing and print an error message if the file could not be opened
tryOpen filename = (fileOpen filename >>== Just)
                     `catch` (\(e::IOError) -> do registerWarning$ CANT_OPEN_FILE filename; return Nothing)

-- |Copy a file
fileCopy srcname dstname = do
  bracketCtrlBreak "fileClose1:fileCopy" (fileOpen srcname) (fileClose) $ \srcfile -> do
    handleCtrlBreak "fileRemove1:fileCopy" (ignoreErrors$ fileRemove dstname) $ do
      bracketCtrlBreak "fileClose2:fileCopy" (fileCreate dstname) (fileClose) $ \dstfile -> do
        size <- fileGetSize srcfile
        fileCopyBytes srcfile size dstfile


----------------------------------------------------------------------------------------------------
----- External functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Stop program execution.
-- On Windows the C exit() is used to avoid a known hang with exitWith in arc.exe.
-- On Unix we use POSIX _exit() which terminates immediately without running atexit
-- handlers (unlike exit()).  Using exit() would invoke GHC's hs_exit() atexit handler
-- which sends ThreadKilled to all threads and waits for them; but threads blocked
-- inside 'withMVar' (masked mode) defer async exceptions, causing a deadlock.
-- exitWith throws ExitCode, which also escapes setUncaughtExceptionHandler.
#if defined(FREEARC_WIN)
foreign import ccall unsafe "stdlib.h exit"
  exit :: Int -> IO ()
#else
foreign import ccall unsafe "unistd.h _exit"
  c_posix_exit :: CInt -> IO ()
exit :: Int -> IO ()
exit n = c_posix_exit (fromIntegral n)
#endif

