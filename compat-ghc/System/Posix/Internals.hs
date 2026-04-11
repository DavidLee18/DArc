-- MicroHs shim for System.Posix.Internals.
-- Provides the low-level stat/open-flags interface used by DArc.
module System.Posix.Internals
  ( -- * File status (stat)
    CStat
  , sizeof_stat
  , c_stat
  , st_mode
  , st_size
  , st_mtime
  , stat_mode
  , s_isdir
  , s_isreg
    -- * File descriptor type
  , FD
    -- * Open flags
  , o_RDONLY, o_WRONLY, o_RDWR
  , o_CREAT, o_TRUNC, o_APPEND
  , o_NONBLOCK, o_NOCTTY
    -- * Seek constants
  , sEEK_SET, sEEK_CUR, sEEK_END
    -- * Re-export CFilePath helpers (Unix path)
  , CFilePath
  , withCFilePath
  , peekCFilePath
  ) where

import Data.Bits         ((.&.))
import Foreign.C.String  (CString, withCString, peekCString)
import Foreign.C.Types   (CInt(..), CSize(..), CLong(..), CTime(..))
import Foreign.Ptr       (Ptr)
import System.Posix.Types (Fd(..), FileMode, CMode(..))

-- | Opaque C struct stat.
data CStat

-- | Size of struct stat on this platform (queried from C).
foreign import ccall "darc_sizeof_stat" sizeof_stat :: Int

-- | Call POSIX stat(2).
foreign import ccall "stat" c_stat :: CString -> Ptr CStat -> IO CInt

-- | Read st_mode field from a struct stat.
foreign import ccall "darc_st_mode" st_mode :: Ptr CStat -> IO CMode

-- | Read st_size field from a struct stat.
foreign import ccall "darc_st_size"  st_size  :: Ptr CStat -> IO CLong

-- | Read st_mtime field from a struct stat.
foreign import ccall "darc_st_mtime" st_mtime_raw :: Ptr CStat -> IO CLong
st_mtime :: Ptr CStat -> IO CTime
st_mtime p = fmap (CTime . fromIntegral) (st_mtime_raw p)

-- | Alias used in DArc source.
stat_mode :: Ptr CStat -> IO CMode
stat_mode = st_mode

-- | S_ISDIR test.
s_isdir :: CMode -> Bool
s_isdir m = (m .&. 0o170000) == 0o040000

-- | S_ISREG test.
s_isreg :: CMode -> Bool
s_isreg m = (m .&. 0o170000) == 0o100000

-- | File descriptor type (CInt on POSIX).
type FD = CInt

-- Open flags (standard POSIX values on Linux).
o_RDONLY, o_WRONLY, o_RDWR   :: CInt
o_RDONLY  = 0
o_WRONLY  = 1
o_RDWR    = 2

o_CREAT, o_TRUNC, o_APPEND   :: CInt
o_CREAT   = 0o100
o_TRUNC   = 0o1000
o_APPEND  = 0o2000

o_NONBLOCK, o_NOCTTY          :: CInt
o_NONBLOCK = 0o4000
o_NOCTTY   = 0o400

-- Seek constants.
sEEK_SET, sEEK_CUR, sEEK_END :: CInt
sEEK_SET = 0
sEEK_CUR = 1
sEEK_END = 2

-- On Unix, CFilePath is just CString.
type CFilePath = CString

withCFilePath :: String -> (CFilePath -> IO a) -> IO a
withCFilePath = withCString

peekCFilePath :: CFilePath -> IO String
peekCFilePath = peekCString
