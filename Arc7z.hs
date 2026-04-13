{-# LANGUAGE CPP            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------------------------------------------
-- .7z support for DArc.
--
-- Read-only operations (list / extract / test) are handled natively by the
-- 7zip C SDK vendored under Compression/7z/sdk and linked as libdarc7z (see
-- C_7z.c). Creation and in-place update fall back to the system '7zz' (or
-- '7z') binary, since writing the .7z container would pull in the p7zip C++
-- encoder — out of scope for the lite integration.
----------------------------------------------------------------------------------------------------
module Arc7z (is7zArchive, run7z) where

import Data.Char (toLower)
import Data.List (isSuffixOf)
import Foreign.C.String
import Foreign.C.Types
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.IO
import System.Process (system)

import Options
import Errors

foreign import ccall unsafe "darc_7z_list"
  c_darc_7z_list    :: CString -> IO CInt
foreign import ccall unsafe "darc_7z_extract"
  c_darc_7z_extract :: CString -> CString -> IO CInt
foreign import ccall unsafe "darc_7z_test"
  c_darc_7z_test    :: CString -> IO CInt

is7zArchive :: String -> Bool
is7zArchive path = ".7z" `isSuffixOf` map toLower path

run7z :: Command -> IO ()
run7z cmd = do
  let arc       = cmd_arcspec cmd
      filespecs = cmd_filespecs cmd
      name      = cmd_name cmd
      outdir    = opt_disk_basedir cmd
  case name of
    "l" -> runNative  arc  (\p -> c_darc_7z_list p)
    "lb" -> runNative arc  (\p -> c_darc_7z_list p)
    "lt" -> runNative arc  (\p -> c_darc_7z_list p)
    "v" -> runNative  arc  (\p -> c_darc_7z_list p)
    "t" -> runNative  arc  (\p -> c_darc_7z_test p)
    "x" -> runExtract arc  outdir
    "e" -> runExtract arc  (if null outdir then "." else outdir)
    "a" -> runCreate  arc filespecs
    "create" -> runCreate arc filespecs
    "f" -> runUpdate  arc filespecs
    "u" -> runUpdate  arc filespecs
    "d" -> runDelete  arc filespecs
    c   -> registerError $ GENERAL_ERROR
      ["command '" ++ c ++ "' is not supported for .7z archives (use .arc instead)"]

-- Native SDK call for list/test.
runNative :: String -> (CString -> IO CInt) -> IO ()
runNative arc act = do
  rc <- withCString arc act
  case fromIntegral rc of
    0 -> return ()
    n -> registerError $ GENERAL_ERROR
           ["7z decoder returned SRes=" ++ show n ++ " for " ++ arc]

runExtract :: String -> String -> IO ()
runExtract arc outdir = do
  let dest = if null outdir then "." else outdir
  createDirectoryIfMissing True dest
  rc <- withCString arc $ \parc ->
        withCString dest $ \pdir ->
          c_darc_7z_extract parc pdir
  case fromIntegral rc of
    0 -> return ()
    n -> registerError $ GENERAL_ERROR
           ["7z decoder returned SRes=" ++ show n ++ " for " ++ arc]

-- For create/update/delete we still shell out — no encoder linked.
runCreate, runUpdate, runDelete :: String -> [String] -> IO ()
runCreate = shellOut "a"
runUpdate = shellOut "u"
runDelete = shellOut "d"

shellOut :: String -> String -> [String] -> IO ()
shellOut op arc filespecs = do
  mbin <- find7zBinary
  case mbin of
    Nothing -> registerError $ GENERAL_ERROR
      ["creating/updating .7z archives requires the '7zz' (or '7z') binary in PATH; install p7zip-full"]
    Just bin -> do
      let shellCmd = unwords (bin : op : map shellQuote (arc : filespecs))
      hFlush stdout
      rc <- system shellCmd
      case rc of
        ExitSuccess   -> return ()
        ExitFailure n -> registerError $ GENERAL_ERROR
          ["7zz exited with code " ++ show n ++ " for archive " ++ arc]

find7zBinary :: IO (Maybe String)
find7zBinary = do
  rc1 <- system "command -v 7zz >/dev/null 2>&1"
  case rc1 of
    ExitSuccess -> return (Just "7zz")
    _ -> do
      rc2 <- system "command -v 7z >/dev/null 2>&1"
      case rc2 of
        ExitSuccess -> return (Just "7z")
        _           -> return Nothing

shellQuote :: String -> String
shellQuote s = '\'' : concatMap esc s ++ "'"
  where esc '\'' = "'\\''"
        esc c    = [c]
