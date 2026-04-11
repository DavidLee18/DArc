-- MicroHs shim: adds fillBytes on top of Foreign.Marshal.Utils.
module Foreign.Marshal.Utils
  ( with, new, fromBool, toBool
  , maybeNew, maybeWith, maybePeek, withMany
  , copyBytes, moveBytes
  , fillBytes
  ) where

import Foreign.Marshal.Utils (with, new, fromBool, toBool,
                               maybeNew, maybeWith, maybePeek, withMany,
                               copyBytes, moveBytes)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Ptr     (Ptr, castPtr)
import Data.Word       (Word8)

foreign import ccall "memset" c_memset :: Ptr () -> CInt -> CSize -> IO (Ptr ())

fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes ptr val n = c_memset (castPtr ptr) (fromIntegral val) (fromIntegral n) >> return ()
