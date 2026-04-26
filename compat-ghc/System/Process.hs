-- MicroHs shim for System.Process.
module System.Process
  ( callCommand
  , system
  , readProcess
  , runInteractiveCommand
  , waitForProcess
  , ProcessHandle
  , StdStream(..)
  , CreateProcess(..)
  , proc
  , createProcess
  ) where

import System.Process       (callCommand, system, readProcess)
import System.Exit          (ExitCode(..))
import System.IO            (Handle, hClose, stdin, stderr)
import System.IO.StringHandle (stringToHandle)

-- | Opaque process handle.
newtype ProcessHandle = ProcessHandle ExitCode

data StdStream = Inherit | UseHandle Handle | CreatePipe | NoStream

data CreateProcess = CreateProcess
  { cmdspec :: String
  , std_in  :: StdStream
  , std_out :: StdStream
  , std_err :: StdStream
  }

proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess (unwords (cmd : args)) Inherit Inherit Inherit

-- | Run a shell command and return handles for (stdin, stdout, stderr, ph).
-- stdout contains the captured output; stderr is discarded.
runInteractiveCommand :: String -> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommand cmd = do
  result <- readProcess "/bin/sh" ["-c", cmd] ""
  hOut <- stringToHandle result
  return (stdin, hOut, stderr, ProcessHandle ExitSuccess)

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess (ProcessHandle code) = return code

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  rc <- system (cmdspec cp)
  return (Nothing, Nothing, Nothing, ProcessHandle rc)
