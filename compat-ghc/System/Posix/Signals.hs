-- Minimal shim for System.Posix.Signals targeting MicroHs on Linux.
-- Uses a C-level volatile flag set by the signal handler; a polling
-- thread calls the registered Haskell action when the flag fires.
module System.Posix.Signals
  ( Signal
  , Handler(..)
  , installHandler
  , sigINT, sigTERM, sigHUP, sigPIPE
  ) where

import Control.Concurrent  (forkIO, threadDelay)
import Control.Monad       (when, forever)
import Data.IORef
import Foreign.C.Types     (CInt(..))
import System.IO.Unsafe    (unsafePerformIO)

type Signal = CInt

data Handler
  = Default
  | Ignore
  | Catch   (IO ())
  | CatchOnce (IO ())

sigINT  :: Signal; sigINT  = 2
sigTERM :: Signal; sigTERM = 15
sigHUP  :: Signal; sigHUP  = 1
sigPIPE :: Signal; sigPIPE = 13

-- C helper: install a C-level handler that sets an atomic flag,
-- and query / clear that flag.
foreign import ccall "darc_install_sigint" c_install_sigint :: IO ()
foreign import ccall "darc_clear_sigint"  c_clear_sigint   :: IO ()
foreign import ccall "darc_check_sigint"  c_check_sigint   :: IO CInt

-- Global IORef holding the current Haskell action for SIGINT.
{-# NOINLINE sigintActionRef #-}
sigintActionRef :: IORef (IO ())
sigintActionRef = unsafePerformIO $ do
  ref <- newIORef (return ())
  c_install_sigint
  _ <- forkIO $ forever $ do
    threadDelay 100000  -- poll every 100 ms
    fired <- c_check_sigint
    when (fired /= 0) $ do
      c_clear_sigint
      action <- readIORef ref
      action
  return ref

installHandler :: Signal -> Handler -> Maybe a -> IO Handler
installHandler sig handler _mask = do
  case handler of
    Catch   action -> writeIORef sigintActionRef action
    CatchOnce action -> writeIORef sigintActionRef $ do
                          writeIORef sigintActionRef (return ())
                          action
    Default -> writeIORef sigintActionRef (return ())
    Ignore  -> writeIORef sigintActionRef (return ())
  -- Force initialisation of the polling thread
  _ <- return sigintActionRef
  return Default
