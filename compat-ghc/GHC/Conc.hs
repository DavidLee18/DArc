-- MicroHs shim for GHC.Conc.
module GHC.Conc
  ( setUncaughtExceptionHandler
  , getNumCapabilities
  , setNumCapabilities
  , getNumProcessors
  ) where

import Control.Exception (SomeException)
import Foreign.C.Types   (CInt(..))

-- MicroHs runs single-threaded (no RTS capabilities concept).
getNumCapabilities :: IO Int
getNumCapabilities = return 1

setNumCapabilities :: Int -> IO ()
setNumCapabilities _ = return ()

-- Query the number of online processors via sysconf(_SC_NPROCESSORS_ONLN).
foreign import ccall "darc_get_nprocs" c_get_nprocs :: IO CInt

getNumProcessors :: IO Int
getNumProcessors = fmap fromIntegral c_get_nprocs

-- MicroHs has its own top-level exception handling; this is a no-op shim.
setUncaughtExceptionHandler :: (SomeException -> IO ()) -> IO ()
setUncaughtExceptionHandler _ = return ()
