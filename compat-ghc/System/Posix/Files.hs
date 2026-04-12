-- MicroHs shim for System.Posix.Files.
module System.Posix.Files
  ( FileStatus
  , getFileStatus
  , fileExist
  , isDirectory
  , isRegularFile
  , isSymbolicLink
  , isBlockDevice
  , isCharacterDevice
  , isNamedPipe
  , isSocket
  , fileMode
  , fileSize
  , modificationTime
  , accessTime
  , setFileMode
  , setFileTimes
  -- File mode bit helpers
  , ownerReadMode, ownerWriteMode, ownerExecuteMode
  , groupReadMode, groupWriteMode, groupExecuteMode
  , otherReadMode, otherWriteMode, otherExecuteMode
  , unionFileModes, intersectFileModes
  ) where

import Data.Bits          ((.&.), (.|.), complement)
import Foreign.C.String   (CString, withCString)
import Foreign.C.Types    (CInt(..), CLong(..))
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Ptr        (Ptr)
import Foreign.Storable   (peek)
import System.IO.Unsafe   (unsafePerformIO)
import Foreign.C.Types    (CTime(..))
import System.Posix.Types (FileMode, EpochTime, FileOffset, Fd(..), CMode(..))
import System.Posix.Internals (CStat, sizeof_stat, c_stat, st_mode)

newtype FileStatus = FileStatus [Int]  -- opaque placeholder

-- | Query file metadata. Returned as a FileStatus wrapping the raw stat struct.
foreign import ccall "darc_getfilestatus" c_getfilestatus
  :: Ptr CStat -> IO CInt  -- just calls stat() already exposed as c_stat

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = allocaBytes sizeof_stat $ \p -> do
  withCString path $ \cs -> c_stat cs p >>= \r ->
    if r /= 0
      then ioError (userError ("getFileStatus: " ++ path))
      else do
        m <- st_mode p
        sz <- alloca $ \pOut -> do { darc_st_size_w p pOut; peek pOut }
        mt <- alloca $ \pOut -> do { darc_st_mtime_w p pOut; peek pOut }
        return (FileStatus [fromIntegral m, fromIntegral sz, fromIntegral mt])

fileExist :: FilePath -> IO Bool
fileExist path = allocaBytes sizeof_stat $ \p ->
  withCString path $ \cs -> do
    r <- c_stat cs p
    return (r == 0)

isDirectory :: FileStatus -> Bool
isDirectory (FileStatus (m:_)) = (m .&. 0o170000) == 0o040000
isDirectory _ = False

isRegularFile :: FileStatus -> Bool
isRegularFile (FileStatus (m:_)) = (m .&. 0o170000) == 0o100000
isRegularFile _ = False

isSymbolicLink :: FileStatus -> Bool
isSymbolicLink (FileStatus (m:_)) = (m .&. 0o170000) == 0o120000
isSymbolicLink _ = False

isBlockDevice :: FileStatus -> Bool
isBlockDevice (FileStatus (m:_)) = (m .&. 0o170000) == 0o060000
isBlockDevice _ = False

isCharacterDevice :: FileStatus -> Bool
isCharacterDevice (FileStatus (m:_)) = (m .&. 0o170000) == 0o020000
isCharacterDevice _ = False

isNamedPipe :: FileStatus -> Bool
isNamedPipe (FileStatus (m:_)) = (m .&. 0o170000) == 0o010000
isNamedPipe _ = False

isSocket :: FileStatus -> Bool
isSocket (FileStatus (m:_)) = (m .&. 0o170000) == 0o140000
isSocket _ = False

fileMode :: FileStatus -> FileMode
fileMode (FileStatus (m:_)) = fromIntegral m
fileMode _ = 0

fileSize :: FileStatus -> FileOffset
fileSize (FileStatus (_:sz:_)) = fromIntegral sz
fileSize _ = 0

modificationTime :: FileStatus -> EpochTime
modificationTime (FileStatus (_:_:mt:_)) = fromIntegral mt
modificationTime _ = 0

-- | Access time (same as modification time in this simplified shim).
accessTime :: FileStatus -> EpochTime
accessTime = modificationTime

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes path atime mtime = withCString path $ \cs -> do
  let unCTime (CTime n) = fromIntegral n
  r <- c_utimes cs (unCTime atime) (unCTime mtime)
  if r /= 0
    then ioError (userError ("setFileTimes: " ++ path))
    else return ()

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode path mode = withCString path $ \cs -> do
  r <- c_chmod cs (fromIntegral mode)
  if r /= 0
    then ioError (userError ("setFileMode: " ++ path))
    else return ()

-- File mode bits (standard POSIX octal values)
ownerReadMode, ownerWriteMode, ownerExecuteMode :: FileMode
ownerReadMode    = CMode 0o400
ownerWriteMode   = CMode 0o200
ownerExecuteMode = CMode 0o100

groupReadMode, groupWriteMode, groupExecuteMode :: FileMode
groupReadMode    = CMode 0o040
groupWriteMode   = CMode 0o020
groupExecuteMode = CMode 0o010

otherReadMode, otherWriteMode, otherExecuteMode :: FileMode
otherReadMode    = CMode 0o004
otherWriteMode   = CMode 0o002
otherExecuteMode = CMode 0o001

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes (CMode a) (CMode b) = CMode (a .|. b)

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes (CMode a) (CMode b) = CMode (a .&. b)

foreign import ccall "chmod"       c_chmod  :: CString -> CMode  -> IO CInt
foreign import ccall "darc_utimes" c_utimes :: CString -> CLong -> CLong -> IO CInt
-- MicroHs truncates FFI return values to 32 bits; use _w variants with pointer.
foreign import ccall "darc_st_size_w"  darc_st_size_w  :: Ptr CStat -> Ptr CLong -> IO ()
foreign import ccall "darc_st_mtime_w" darc_st_mtime_w :: Ptr CStat -> Ptr CLong -> IO ()
