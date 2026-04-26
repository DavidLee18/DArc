-- MicroHs shim for System.Posix.IO.
module System.Posix.IO
  ( openFd
  , closeFd
  , fdRead
  , fdWrite
  , stdInput
  , stdOutput
  , stdError
  , OpenMode(..)
  , OpenFileFlags(..)
  , defaultFileFlags
  , noctty
  , nonBlock
  , append
  ) where

import Foreign.C.String  (withCStringLen, peekCStringLen, withCString)
import Foreign.C.Types   (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr       (Ptr, castPtr, nullPtr)
import System.Posix.Types (Fd(..), FileMode, ByteCount)
import System.Posix.Internals (o_RDONLY, o_WRONLY, o_RDWR, o_CREAT, o_APPEND,
                                o_NONBLOCK, o_NOCTTY, o_TRUNC)
import Data.Bits ((.|.))

data OpenMode = ReadOnly | WriteOnly | ReadWrite

data OpenFileFlags = OpenFileFlags
  { append    :: Bool
  , exclusive :: Bool
  , noctty    :: Bool
  , nonBlock  :: Bool
  , trunc     :: Bool
  }

defaultFileFlags :: OpenFileFlags
defaultFileFlags = OpenFileFlags
  { append   = False
  , exclusive = False
  , noctty   = False
  , nonBlock = False
  , trunc    = False
  }

stdInput, stdOutput, stdError :: Fd
stdInput  = Fd 0
stdOutput = Fd 1
stdError  = Fd 2

foreign import ccall "open"  c_open  :: Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall "close" c_close :: CInt -> IO CInt
foreign import ccall "read"  c_read  :: CInt -> Ptr () -> CSize -> IO CInt
foreign import ccall "write" c_write :: CInt -> Ptr () -> CSize -> IO CInt

openFd :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
openFd path mode mFileMode flags = withCString path $ \cs -> do
  let modeFlag = case mode of
                   ReadOnly  -> o_RDONLY
                   WriteOnly -> o_WRONLY
                   ReadWrite -> o_RDWR
      extraFlags = (if append    flags then o_APPEND   else 0)
               .|. (if noctty    flags then o_NOCTTY   else 0)
               .|. (if nonBlock  flags then o_NONBLOCK else 0)
               .|. (if trunc     flags then o_TRUNC    else 0)
               .|. (case mFileMode of { Just _ -> o_CREAT; Nothing -> 0 })
      cmode = case mFileMode of { Just m -> fromIntegral m; Nothing -> 0o666 }
  fd <- c_open (castPtr cs) (modeFlag .|. extraFlags) cmode
  if fd < 0
    then ioError (userError ("openFd: " ++ path))
    else return (Fd fd)

closeFd :: Fd -> IO ()
closeFd (Fd fd) = c_close fd >> return ()

fdRead :: Fd -> ByteCount -> IO (String, ByteCount)
fdRead (Fd fd) n = allocaBytes (fromIntegral n) $ \buf -> do
  r <- c_read fd buf (fromIntegral n)
  if r < 0
    then ioError (userError "fdRead failed")
    else do
      s <- peekCStringLen (castPtr buf, fromIntegral r)
      return (s, fromIntegral r)

fdWrite :: Fd -> String -> IO ByteCount
fdWrite (Fd fd) str = withCStringLen str $ \(buf, len) -> do
  r <- c_write fd (castPtr buf) (fromIntegral len)
  if r < 0
    then ioError (userError "fdWrite failed")
    else return (fromIntegral r)
