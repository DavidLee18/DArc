{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Encoding data structures as a byte stream, with buffered writing/reading ----------------------
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  ByteStream
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC/Hugs on x86 processors
--
--  This module is like 'Binary' module from NHC - it supports writing data
--  structures to binary files or memory buffers and reading them back.
--
--  This module features:
--  * Compatibility with last versions of GHC and Hugs
--  * Lightning speed, especially for large strings and lists of Ints/Words
--      (i have seen a 10mb/s speed on my 1.2 ghz machine)
--  * Flexibility of input/output - data may be hold in files, memory buffers,
--      or reading/writing may be performed via callbacks
--
--  This module currently DON'T supports:
--  * Haskell'98 compatibility (because it uses "too complex" class scheme)
--  * Compatibility with MSB (most significant byte first) processors,
--      including Power PC, Motorola and Sparc
--  * Compatibility with processors, which require aligning of Ints on word
--      boundaries
--  * Bit-oriented compression (instead it uses byte-oriented compression,
--      which is faster and simplier)
--  * Writing strings which contains null chars
--  * Tell/Seek-like operations on streams and "freezing" streams
--  * Reading input streams via fixed-size buffer (buffering at this time
--      supported only for output streams, input streams must be placed
--      in one memory buffer containing all the data. MOREOVER, YOU MUST
--      ALLOCATE BUFFER WITH 8 ADDITIONAL BYTES AFTER END OF REAL DATA.
--      It's because Integer demarshalling can pre-read whole 9 bytes
--      even for values which use only 1 byte)
--
--  Example of simple usage you can see in the last section of this file,
--  and examples of defining functions to read/write values of some type -
--  in two preceding sections of file. If you need more explanations -
--  please write me.
--
-----------------------------------------------------------------------------

module ByteStream where

import Prelude hiding (read,readList)
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Char
import Data.IORef
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import System.IO  hiding (openFile)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Base (unsafeChr)

import Files
import Utils

aTypicalBuffer = 64*1024

----------------------------------------------------------------------------------------------------
---- Output buffer for fast writing of structured data ---------------------------------------------
----------------------------------------------------------------------------------------------------

data OutStream = OutStream
  { ref_buf     :: IORef (Ptr CChar)    -- the in-memory buffer currently in use
  , ref_size    :: IORef Int            -- its size in bytes
  , ref_pos     :: IORef Int            -- current write position in the buffer
  , functions   :: ( RecvBuf              -- functions providing the link to the outside world
                   , SendBuf              --   (see the description of create)
                   , Cleanup )
  }

type RecvBuf = IO (Ptr CChar, Int)
type SendBuf = Ptr CChar -> Int -> Int -> IO ()
type Cleanup = IO ()


-- |Write the output data to the file `filename`, buffering it in a buffer of `size` bytes
createFile filename size = do
  file <- fileCreate filename
  createBuffered size (fileWriteBuf file) (fileClose file)

-- |Create an output stream, allocating a buffer of `size` bytes for it.
-- The data accumulated in the buffer is flushed out via the `writer` function
createBuffered size writer closer = do
  buf <- mallocBytes size
  let buf' = castPtr buf
      sendBuf b sz = writer b
  create (return (buf',size)) sendBuf (free buf >> closer)

-- |Create an output stream, allocating a buffer of `size` bytes for it.
-- The data accumulated in the buffer is flushed out via the `writer` function
createMemBuf buf size = do
  create (return (buf,size)) (\buf size len -> fail "createMemBuf: Buffer overflow") (return ())

-- |Create a general-purpose output stream using the following functions:
-- receiveBuf : IO (buf,size)                - obtain the next buffer for our own use
-- sendBuf    : buf -> size -> len -> IO ()  - send the buffer out with `len` bytes of data
-- cleanup    : IO ()                        - cleanup on shutdown
create receiveBuf sendBuf cleanup = do
  (buf, size) <- receiveBuf   -- get the very first buffer of our life right away
  ref_buf  <- ref buf
  ref_size <- ref size
  ref_pos  <- ref 0
  return (OutStream ref_buf ref_size ref_pos (receiveBuf, sendBuf, cleanup))

-- |Get the next buffer for writing data
receiveBuffer (OutStream ref_buf ref_size ref_pos (receiveBuf, _, _)) = do
  (buf, size) <- receiveBuf
  ref_buf  =: buf
  ref_size =: size
  ref_pos  =: 0

-- |Make sure the buffer still has room to write `bytes` bytes.
-- If not - hand this buffer off to the resellers and get a new, nice and clean one, which is sure to have enough room!
ensureFreeSpaceInOutStream buffer@(OutStream _ ref_size ref_pos _) bytes = do
  size <- val ref_size
  pos  <- val ref_pos
  when (pos+bytes>size-1) $ do
    sendBuffer buffer
    receiveBuffer buffer
    size <- val ref_size
    pos  <- val ref_pos
    when (pos+bytes>size-1) $
      fail$ "OutStream: needs "++show bytes++" bytes, but entire new buffer contains only "++show size++" bytes"

-- |Send the accumulated buffer contents through the output function and stop using it
sendBuffer (OutStream ref_buf ref_size ref_pos (_, sendBuf, _)) = do
  modifyIORefIO ref_buf $ \buf -> do
    size <- val ref_size
    pos  <- val ref_pos
    sendBuf buf size pos
    return (error "OutStream::buf undefined")

-- |Send the accumulated buffer contents and close the stream
closeOut buffer@(OutStream _ _ _ (_, _, cleanup)) = do
  sendBuffer buffer
  cleanup

-- |All-in-one operation: creates an output stream, writes a value into it and closes the stream.
-- If you need to write several values - collect them into a tuple
writeAll :: (BufferData a) =>  RecvBuf -> SendBuf -> Cleanup -> a -> IO ()
writeAll receiveBuf sendBuf cleanup x =
  bracket (create receiveBuf sendBuf cleanup) closeOut
    (\buf -> write buf x)

-- |All-in-one operation: writes a value to a file and closes it.
-- If you need to write several values - collect them into a tuple
writeFile :: (BufferData a) => FilePath -> a -> IO ()
writeFile filename x =
  bracket (createFile filename aTypicalBuffer) closeOut
    (\buf -> write buf x)

{-# NOINLINE createFile #-}
{-# NOINLINE createBuffered #-}
{-# NOINLINE create #-}
{-# NOINLINE receiveBuffer #-}
{-# NOINLINE ensureFreeSpaceInOutStream #-}
{-# NOINLINE sendBuffer #-}
{-# NOINLINE closeOut #-}
{-# NOINLINE writeAll #-}


----------------------------------------------------------------------------------------------------
---- Input buffer for fast reading of structured data ----------------------------------------------
----------------------------------------------------------------------------------------------------

data InStream = InStream
  { iref_buf     :: IORef (Ptr CChar)   -- the in-memory buffer currently in use
  , iref_size    :: IORef Int           -- its size in bytes
  , iref_pos     :: IORef Int           -- current write position in the buffer
  , ifunctions   :: ( RecvBuf             -- functions providing the link to the outside world
                    , SendBuf             --   (see the description of open)
                    , Cleanup )
  }

-- |to do: Decode data from a file, reading it through a buffer of `size` bytes
-- At the moment the file is read into memory in its entirety,
-- which is caused by the lack of support for moving on to the next buffer
openFile filename _size = do
  file     <- fileOpen filename
  filesize <- fileGetSize file   -- temporary solution
  let size  = 8 + i filesize     -- ditto
  buf      <- mallocBytes size
  let receiveBuf = do len <- fileReadBuf file buf size;  return (castPtr buf, len)
      sendBuf buf size len  =  return ()
  open receiveBuf sendBuf (free buf >> fileClose file)

-- |Decode the data contained in buffer `buf` of length `size`
openMemory buf size = do
  ref_bytes_read <- ref 0   -- how many bytes of the buffer have already been processed
  let   -- receiveBuf returns (buf,size) without the part of the data that has already been processed
      receiveBuf = do bytes_read <- val ref_bytes_read
                      return (buf+:bytes_read, size-bytes_read)
        -- sendBuf records that another `len` bytes have been processed
      sendBuf buf size len  =  ref_bytes_read += len
   -- Use the general-purpose `open`; when an attempt is made to move on to the next buffer, simply return
   -- the remainder of the data in `buf`
  open receiveBuf sendBuf (return ())

-- |to do: Create a general-purpose input stream using the following functions:
-- receiveBuf : IO (buf,size)                - get a buffer `buf` holding `size` bytes of data
-- sendBuf    : buf -> size -> len -> IO ()  - release the received buffer, from which `len` bytes were read
-- cleanup    : IO ()                        - cleanup on shutdown
open receiveBuf sendBuf cleanup = do
  (buf, size) <- receiveBuf   -- get the very first buffer of our life right away
  ref_buf  <- ref buf
  ref_size <- ref size
  ref_pos  <- ref 0
  return (InStream ref_buf ref_size ref_pos (receiveBuf, sendBuf, cleanup))

-- |Close the input stream and run the `cleanup` procedure
closeIn (InStream _ _ _ (_, _, cleanup)) = do
  cleanup

-- |Returns the read pointer to the beginning of the current buffer
rewindMemory buffer@(InStream _ _ pos _) = do
  pos =: 0

-- |Skips the given number of bytes
skipBytes buffer@(InStream _ _ pos _) bytes = do
  pos += bytes

-- |Checks that we have reached the end of the current buffer
isEOFMemory buffer@(InStream _ size' pos' _) = do
  size <- val size'
  pos  <- val pos'
  return (pos==size)

-- |All-in-one operation: creates an input stream, reads a value and closes the stream.
-- If you need to read several values - collect them into a tuple
readMemory :: (BufferData a) =>  Ptr CChar -> Int -> IO a
readMemory buf size = do
  bracket (openMemory buf size) closeIn read

-- |All-in-one operation: opens a file, reads a value and closes the file.
-- If you need to read several values - collect them into a tuple
readFile filename = do
  bracket (openFile filename aTypicalBuffer) closeIn read

{-# NOINLINE openFile #-}
{-# NOINLINE openMemory #-}
{-# NOINLINE open #-}
{-# NOINLINE closeIn #-}
{-# NOINLINE readMemory #-}



----------------------------------------------------------------------------------------------------
---- Writing a memory block ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

writeBuf :: OutStream -> Ptr a -> Int -> IO ()
writeBuf buffer@(OutStream ref_buf ref_size ref_pos _) dataptr datasize = do
  when (datasize>0) $ do
    ensureFreeSpaceInOutStream buffer 1
    buf  <- val ref_buf
    size <- val ref_size
    pos  <- val ref_pos
    let len = min datasize (size-pos)
    copyBytes (buf+:pos) dataptr len
    ref_pos =: pos+len
    writeBuf buffer (dataptr+:len) (datasize-len)


----------------------------------------------------------------------------------------------------
---- Type classes for which buffer reading/writing is implemented ----------------------------------
----------------------------------------------------------------------------------------------------

-- |Elements of this class can be written to an output buffer and read from an input one
class BufferData a where
  -- |Write a single value to the output buffer
  write :: OutStream -> a -> IO ()

  -- |Write a whole list of values to the buffer - the default implementation does this slowly and sadly :)
  writeList :: OutStream -> [a] -> IO ()
  writeList buffer xs = mapM_ (write buffer) xs

  -- |Read a single value from the input buffer
  read :: InStream -> IO a

  -- |Read a whole list of values from the input buffer - and the default implementation is a bit sluggish too :)
  readList :: InStream -> Int -> IO [a]
  readList buffer length  =  replicateM length (read buffer)

  {-# NOINLINE readList #-}
  {-# NOINLINE writeList #-}


-- Appoint the procedures of the FastBufferData class to the posts of the BufferData class procedures :)
#ifndef __MHS__
instance {-# OVERLAPPABLE #-} (FastBufferData a) => BufferData a where
  write     = writeFast
  writeList = writeListFast
  read      = readFast
  readList  = readListFast
#endif


-- |Elements of this class can be written to an output buffer and read from an input one VERY FAST
class FastBufferData a where
  -- To do so they must supply the following information:
  --   The maximum number of bytes a single value can occupy (1 for CChar, 4 for Int32 etc.)
  maxSizeOf :: a -> Int
  --   A procedure that writes value `x` into buffer `buf` at position `pos`, and returns
  --   the position in the buffer after the written data (for types occupying a fixed number of bytes,
  --   this will simply be "pos+maxSizeOf x")
  writeUnchecked :: Ptr CChar -> a -> Int -> IO Int
  --   A procedure that reads a value from buffer `buf` at position `pos`, returns that value,
  --   and updates the position in the buffer
  readUnchecked :: Ptr CChar -> Int -> IO (a, Int)

  -- |Write a single value to the buffer - and be quick about it
  writeFast :: OutStream -> a -> IO ()
  writeFast buffer@(OutStream ref_buf _ ref_pos _) x = do
    ensureFreeSpaceInOutStream buffer (maxSizeOf x)   -- check that there is enough room in the buffer
    buf <- val ref_buf
    modifyIORefIO ref_pos (writeUnchecked buf x)   -- write the data into the buffer and update the value of ref_pos

  -- |Write a whole list into the buffer, quick-quick!
  writeListFast :: OutStream -> [a] -> IO ()
  writeListFast buffer@(OutStream ref_buf _ ref_pos _)   list = do
    let aSIZE = 100
    -- Check that there is enough room in the buffer for `aSIZE` values of this type
    ensureFreeSpaceInOutStream buffer (aSIZE * maxSizeOf (head list))
    buf <- val ref_buf
    pos <- val ref_pos

    -- The procedure "go list pos n" writes without any checks at all, starting at position `pos`,
    -- the data from list `list`, but no more than `n` values. If the list turns out to be
    -- longer - the `writeListFast` procedure is called again, which will check
    -- that there is room in the buffer for another 100 values, and will carry on writing the list from the
    -- point at which we stopped
    --
    let --go :: (FastBufferData a) => [a] -> Int -> Int -> IO ()
        go []     pos _  = ref_pos =: pos  -- We are done! All that is left is to write the new position in the buffer!
        go list   pos 0  = do ref_pos =: pos             -- Write the new position in the buffer
                              writeListFast buffer list  -- ... and call the function recursively for the rest of the list
        go (x:xs) pos n  = do new_pos <- writeUnchecked buf x pos    -- write the next element
                              go xs new_pos (n-1)                    -- ... and move on to the next one
    go list pos aSIZE  -- write the list into memory without checks, but no more than `aSIZE` values


  -- |Fast reading of a single value from the input buffer
  readFast :: InStream -> IO a
  readFast buffer@(InStream buf _ pos _) = do
    abuf <- val buf
    apos <- val pos
    (x, new_pos) <- readUnchecked abuf apos
    pos =: new_pos
    return x

  -- |Fast reading of a whole list from the input buffer
  readListFast :: InStream -> Int -> IO [a]
  readListFast buffer@(InStream buf _ pos _) length = do
    abuf <- val buf
    apos <- val pos
    let --go :: (FastBufferData a) => Int -> Int -> [a] -> IO [a]
        go apos 0 xs = do pos =: apos
                          return (reverse xs)
        go apos n xs = do (x, new_pos) <- readUnchecked abuf apos
                          go new_pos (n-1) (x:xs)
    go apos length []


  {-# NOINLINE readFast #-}
  {-# NOINLINE writeFast #-}
  {-# NOINLINE readListFast #-}
  {-# NOINLINE writeListFast #-}



----------------------------------------------------------------------------------------------------
---- Implementations for simple data types ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Any instance of the Storable class automatically becomes an instance of the FastBufferData class:
-- we know how many bytes data of such a type occupies, and how to write it to / read it from memory
-- |FreeArc/Arc.exe 0.67 32-bit archives write Int and CTime using the native Storable
-- stride — 4 bytes on x86 (not 8). DArc x64 defaults to a fixed 8-byte layout. When
-- --arc-32bit-legacy is set, the Int/CTime reader consumes 4 bytes with stride 4 so
-- directories produced by Arc.exe 0.67 can be decoded. Set via --arc-32bit-legacy.
legacy32bitRead :: IORef Bool
legacy32bitRead = unsafePerformIO (newIORef False)
{-# NOINLINE legacy32bitRead #-}

#ifndef __MHS__
instance (Storable a) => FastBufferData a where
  maxSizeOf = sizeOf
  writeUnchecked buf x pos = do
    pokeByteOff buf pos x
    return (pos + sizeOf x)
  readUnchecked buf pos = do
    x <- peekByteOff buf pos
    return (x, pos + sizeOf x)
#else
-- MicroHs: explicit FastBufferData instances for storable types
instance FastBufferData Word8 where
  maxSizeOf _ = 1
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 1) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 1) }
instance FastBufferData Word16 where
  maxSizeOf _ = 2
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 2) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 2) }
instance FastBufferData Word32 where
  maxSizeOf _ = 4
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 4) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 4) }
instance FastBufferData Word64 where
  maxSizeOf _ = 8
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 8) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 8) }
readIntCompat :: Ptr CChar -> Int -> IO (Int, Int)
readIntCompat buf pos = do
  legacy <- readIORef legacy32bitRead
  if legacy
    then do (x :: Int32) <- peekByteOff buf pos; return (fromIntegral x, pos + 4)
    else do (x :: Int64) <- peekByteOff buf pos; return (fromIntegral x, pos + 8)

instance FastBufferData Int where
  -- Int is serialized as a 64-bit little-endian value for cross-platform
  -- compatibility. Native Int width varies (4 on Win32, 8 on x64), so raw
  -- poke/peek would produce different on-disk layouts.
  maxSizeOf _ = 8
  writeUnchecked buf x pos = do { pokeByteOff buf pos (fromIntegral x :: Int64); return (pos + 8) }
  readUnchecked  buf pos   = readIntCompat buf pos
instance FastBufferData Int32 where
  maxSizeOf _ = 4
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 4) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 4) }
instance FastBufferData Int64 where
  maxSizeOf _ = 8
  writeUnchecked buf x pos = do { pokeByteOff buf pos x; return (pos + 8) }
  readUnchecked  buf pos   = do { x <- peekByteOff buf pos; return (x, pos + 8) }
#endif

-- Characters are written in UTF-8
instance {-# OVERLAPPING #-} BufferData Char where
  write buf c  =  writeList buf (toUTF8List [c])
  read  buffer@(InStream buf _ pos _) = do
    buf' <- val buf
    pos' <- val pos
    unpackCharUtf8 buf' pos' pos


-- A string is written as an ordinary list of characters, but with a zero character at the end (C style)
instance {-# OVERLAPPING #-} BufferData String where
  write buf str  =  writeList buf (toUTF8List str)  >>  write buf (0::Word8)
  read  buffer@(InStream buf _ pos _) = do
    buf' <- val buf
    pos' <- val pos
    unpackCStringUtf8 buf' pos' pos


-- Arbitrary-precision integers are encoded by a method that is an improvement on the one used in 7-zip.
-- With it, values up to 2^64 require a variable number of bytes to write: from 1 to 9
instance FastBufferData Integer where
  maxSizeOf x = 9   -- maximum: 1 byte made of ones and 8 bytes of data
  writeUnchecked buf x pos = do
    let write1  x  =  writeUnchecked buf (x::Word8)
        write4  x  =  writeUnchecked buf (x::Word32)
        write_8 x  =  writeUnchecked buf (x::Word64)
    -- 4 or 8 bytes are written to memory at once, but the position pointer is advanced only by the required
    -- number of bytes. The number of low-order one-bits in the first byte written determines how many
    -- additional bytes must be read in order to obtain the whole number
    -- This implementation is designed only for machines where the least significant byte comes first in memory!!!
    -- Besides that, it is optimized for 32-bit machines; on 64-bit ones this code will be suboptimal
    case () of
     _ | x<0       ->  fail$ "Sorry, FastBufferData.Integer.writeUnchecked don't support negative values like this: "++show x
       | x<128     ->  do write4  (i x*  2+  0) pos; return (pos+1)
       | x<128^2   ->  do write4  (i x*  4+  1) pos; return (pos+2)
       | x<128^3   ->  do write4  (i x*  8+  3) pos; return (pos+3)
       | x<128^4   ->  do write4  (i x* 16+  7) pos; return (pos+4)
       | x<128^5   ->  do write_8 (i x* 32+ 15) pos; return (pos+5)
       | x<128^6   ->  do write_8 (i x* 64+ 31) pos; return (pos+6)
       | x<128^7   ->  do write_8 (i x*128+ 63) pos; return (pos+7)
       | x<128^8   ->  do write_8 (i x*256+127) pos; return (pos+8)
       | x<256^8   ->  do write1 255 pos  >>=  write_8 (i x); return (pos+9)
       | otherwise ->  fail$ "Sorry, FastBufferData.Integer.writeUnchecked don't support numbers larger than 256^8, like this: "++show x

  readUnchecked buf pos = do
    -- 4 bytes are read from memory at once, but only the low-order bytes of them are used, the rest are masked off
    (x::Word32,_)  <-  readUnchecked buf pos
    case () of
     _ | x .&.  1 ==   0  ->  return (i$ (x `mod` 256^1) `shiftR` 1, pos+1)
       | x .&.  3 ==   1  ->  return (i$ (x `mod` 256^2) `shiftR` 2, pos+2)
       | x .&.  7 ==   3  ->  return (i$ (x `mod` 256^3) `shiftR` 3, pos+3)
       | x .&. 15 ==   7  ->  return (i$  x              `shiftR` 4, pos+4)
       | otherwise -> do
          -- If the value occupies more than 4 bytes, then read 8 bytes from memory and again mask off the high-order ones
          (x::Word64,_)  <-  readUnchecked buf pos
          case () of
           _ | x .&. 31 ==  15  ->  return (i$ (x `mod` 256^5) `shiftR` 5, pos+5)
             | x .&. 63 ==  31  ->  return (i$ (x `mod` 256^6) `shiftR` 6, pos+6)
             | x .&.127 ==  63  ->  return (i$ (x `mod` 256^7) `shiftR` 7, pos+7)
             | x .&.255 == 127  ->  return (i$  x              `shiftR` 8, pos+8)
             | otherwise        ->  do
                 -- And the last variant - a byte made of one-bits plus 8 bytes of the value proper
                 (x::Word64, _) <- readUnchecked buf (pos+1); return (i x, pos+9)


-- Boolean values are written individually as values of type Word8 (i.e. each one
-- consumes a whole byte), while when a list is written they are grouped eight values per byte
instance FastBufferData Bool where
  maxSizeOf x = maxSizeOf (undefined :: Word8)
  writeUnchecked buf x   =  writeUnchecked buf (toWord8 x)
  readUnchecked buf pos  =  do (x, new_pos) <- readUnchecked buf pos; return (fromWord8 x, new_pos)
{-
  writeListFast buffer  =  writeListFast buffer . makeBytes
   where
    makeBytes (a:b:c:d:e:f:g:h:xs) = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2+n g)*2+n h) : makeBytes xs
    makeBytes [a,b,c,d,e,f,g]      = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2+n g)*2) : []
    makeBytes [a,b,c,d,e,f]        = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2)*2) : []
    makeBytes [a,b,c,d,e]          = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2)*2)*2) : []
    makeBytes [a,b,c,d]            = (((((((n a*2+n b)*2+n c)*2+n d)*2)*2)*2)*2) : []
    makeBytes [a,b,c]              = (((((((n a*2+n b)*2+n c)*2)*2)*2)*2)*2) : []
    makeBytes [a,b]                = (((((((n a*2+n b)*2)*2)*2)*2)*2)*2) : []
    makeBytes [a]                  = (((((((n a*2)*2)*2)*2)*2)*2)*2) : []
    makeBytes []                   = []
    n = toWord8
-}



----------------------------------------------------------------------------------------------------
---- Implementations for compound data types -------------------------------------------------------
----------------------------------------------------------------------------------------------------

#ifdef __MHS__
-- MicroHs doesn't support overlapping instances, so we provide explicit BufferData
-- instances for the concrete types that GHC handles via (FastBufferData a) => BufferData a.
instance BufferData Word8 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Word16 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Word32 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Word64 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Int where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Int32 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Int64 where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Bool where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance BufferData Integer where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
-- C numeric types (via FastBufferData):
instance FastBufferData CUInt where
  maxSizeOf _ = 4
  writeUnchecked buf (CUInt x) pos = do { pokeByteOff buf pos (fromIntegral x :: Word32); return (pos + 4) }
  readUnchecked  buf pos   = do { (x :: Word32) <- peekByteOff buf pos; return (CUInt (fromIntegral x), pos + 4) }
instance BufferData CUInt where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance FastBufferData CInt where
  maxSizeOf _ = 4
  writeUnchecked buf (CInt x) pos = do { pokeByteOff buf pos (fromIntegral x :: Int32); return (pos + 4) }
  readUnchecked  buf pos   = do { (x :: Int32) <- peekByteOff buf pos; return (CInt (fromIntegral x), pos + 4) }
instance BufferData CInt where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance FastBufferData CSize where
  maxSizeOf _ = 8
  writeUnchecked buf (CSize x) pos = do { pokeByteOff buf pos (fromIntegral x :: Word64); return (pos + 8) }
  readUnchecked  buf pos   = do { (x :: Word64) <- peekByteOff buf pos; return (CSize (fromIntegral x), pos + 8) }
instance BufferData CSize where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
instance FastBufferData CTime where
  maxSizeOf _ = 8
  writeUnchecked buf (CTime x) pos = do { pokeByteOff buf pos (fromIntegral x :: Int64); return (pos + 8) }
  readUnchecked  buf pos   = do { (x, pos') <- readIntCompat buf pos; return (CTime (fromIntegral x), pos') }
instance BufferData CTime where
  write     = writeFast; writeList = writeListFast
  read      = readFast;  readList  = readListFast
#endif

-- |Functions that allow values of any integral type to be written in a variable-length format
writeInteger buf  =  write buf . toInteger
readInteger  buf  =  fromInteger <$> read buf

{-
-- |Write a list of non-negative values bounded above by max'
-- For the efficiency of the writing process and better compressibility of the representation, all values are
-- represented by 1/2/4/8 bytes
writeBoundList buf max =
  case () of
    _ | toInteger max <= toInteger (maxBound::Word8)   ->  writeList buf . map toWord8
      | toInteger max <= toInteger (maxBound::Word16)  ->  writeList buf . map toWord16
      | toInteger max <= toInteger (maxBound::Word32)  ->  writeList buf . map toWord32
      | toInteger max <= toInteger (maxBound::Word64)  ->  writeList buf . map toWord64
      | otherwise                                      ->  writeList buf

-- |Read a list of non-negative values bounded above by max'
-- For the efficiency of the writing process and better compressibility of the representation, all values are
-- represented by 1/2/4/8 bytes
readBoundList buf max n =
  case () of
    _ | toInteger max <= toInteger (maxBound::Word8)   ->  readList buf n >>= return.map fromWord8
      | toInteger max <= toInteger (maxBound::Word16)  ->  readList buf n >>= return.map fromWord16
      | toInteger max <= toInteger (maxBound::Word32)  ->  readList buf n >>= return.map fromWord32
      | toInteger max <= toInteger (maxBound::Word64)  ->  readList buf n >>= return.map fromWord64
      | otherwise                                      ->  readList buf n
-}

instance {-# OVERLAPPABLE #-} (BufferData a) => BufferData [a]  where
  write buf list  =  writeInteger buf (length list)  >>  mapM_ (write buf) list
  read  buf       =  readInteger  buf                >>=  readList  buf

instance (BufferData a, BufferData b) => BufferData (a,b)  where
  write buf (a,b) = write buf a  >>  write buf b
  read  buf       = do a <- read buf; b <- read buf; return (a,b)

instance (BufferData a, BufferData b, BufferData c) => BufferData (a,b,c)  where
  write buf (a,b,c) = write buf ((a,b),c)
  read  buf         = do ((a,b),c) <- read buf; return (a,b,c)

instance (BufferData a, BufferData b, BufferData c, BufferData d) => BufferData (a,b,c,d)  where
  write buf (a,b,c,d) = write buf ((a,b),c,d)
  read  buf           = do ((a,b),c,d) <- read buf; return (a,b,c,d)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e) => BufferData (a,b,c,d,e)  where
  write buf (a,b,c,d,e) = write buf ((a,b),c,d,e)
  read  buf             = do ((a,b),c,d,e) <- read buf; return (a,b,c,d,e)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f) => BufferData (a,b,c,d,e,f)  where
  write buf (a,b,c,d,e,f) = write buf ((a,b),c,d,e,f)
  read  buf               = do ((a,b),c,d,e,f) <- read buf; return (a,b,c,d,e,f)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f, BufferData g) => BufferData (a,b,c,d,e,f,g)  where
  write buf (a,b,c,d,e,f,g) = write buf ((a,b),c,d,e,f,g)
  read  buf                 = do ((a,b),c,d,e,f,g) <- read buf; return (a,b,c,d,e,f,g)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f, BufferData g, BufferData h) => BufferData (a,b,c,d,e,f,g,h)  where
  write buf (a,b,c,d,e,f,g,h) = write buf ((a,b),c,d,e,f,g,h)
  read  buf                   = do ((a,b),c,d,e,f,g,h) <- read buf; return (a,b,c,d,e,f,g,h)

instance (BufferData a) => BufferData (Maybe a)  where
  write buf (Just  a) = write buf (True,a)
  write buf Nothing = write buf False
  read buf = do x <- read buf; if x  then Just <$> read buf  else return Nothing

instance (BufferData a, BufferData b) => BufferData (Either a b)  where
  write buf (Left  a) = write buf (True, a)
  write buf (Right b) = write buf (False,b)
  read buf = do x <- read buf; if x  then Left <$> read buf  else Right <$> read buf

{- An attempt to make a universal class for reading/writing data
class DerivedBufferData a where
  toTuple   :: BufferData b => a -> b
  fromTuple :: BufferData b => b -> a

instance DerivedBufferData a => BufferData a where
  write buf a  =  write buf (toTuple a)
  read  buf    =  do a <- read buf; return (fromTuple a)

instance (BufferData a, BufferData b) => DerivedBufferData (Either a b)  where
  toTuple   (Left  a)  = (True,  a)
  toTuple   (Right b)  = (False, b)
  fromTuple (True,  a) = (Left   a)
  fromTuple (False, b) = (Right  b)

instance (Enum a) => BufferData a  where
  write buf a = writeInteger buf (fromEnum a)
  read  buf   = readInteger  buf >>= return.toEnum
-}

instance (FastBufferData a, FastBufferData b) => FastBufferData (a,b)  where
  maxSizeOf (a,b)               =  maxSizeOf a + maxSizeOf b
  writeUnchecked buf (a,b) pos  =  writeUnchecked buf a pos  >>=  writeUnchecked buf b
  readUnchecked buf pos         =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    return ((a,b), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}

instance (FastBufferData a, FastBufferData b, FastBufferData c) => FastBufferData (a,b,c)  where
  maxSizeOf (a,b,c)               =  maxSizeOf a + maxSizeOf b + maxSizeOf c
  writeUnchecked buf (a,b,c) pos  =
    writeUnchecked buf a pos
      >>=  writeUnchecked buf b
      >>=  writeUnchecked buf c
  readUnchecked buf pos           =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    (c, pos) <- readUnchecked buf pos
    return ((a,b,c), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}

instance (FastBufferData a, FastBufferData b, FastBufferData c, FastBufferData d) => FastBufferData (a,b,c,d)  where
  maxSizeOf (a,b,c,d)               =  maxSizeOf a + maxSizeOf b + maxSizeOf c + maxSizeOf d
  writeUnchecked buf (a,b,c,d) pos  =
    writeUnchecked buf a pos
      >>=  writeUnchecked buf b
      >>=  writeUnchecked buf c
      >>=  writeUnchecked buf d
  readUnchecked buf pos             =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    (c, pos) <- readUnchecked buf pos
    (d, pos) <- readUnchecked buf pos
    return ((a,b,c,d), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}



----------------------------------------------------------------------------------------------------
---- Auxiliary functions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Functions converting to the given types
toWord8  x = toEnum (fromEnum x) :: Word8
toWord16 x = toEnum (fromEnum x) :: Word16
toWord32 x = toEnum (fromEnum x) :: Word32
toWord64 x = i x                 :: Word64

-- |Functions converting from the given types
fromWord8  (x::Word8 ) = toEnum (fromEnum x)
fromWord16 (x::Word16) = toEnum (fromEnum x)
fromWord32 (x::Word32) = toEnum (fromEnum x)
fromWord64 (x::Word64) = i x








toUTF8List :: String -> [Word8]
toUTF8List a | a `seq` False = undefined
toUTF8List [] = []
toUTF8List (x:xs)
  | ord x<=0x007f = fromIntegral (ord x):
                    toUTF8List xs
  | ord x<=0x07ff = fromIntegral (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    toUTF8List xs
  | ord x<=0xffff = fromIntegral (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    toUTF8List xs
  | otherwise     = fromIntegral (0xF0 .|. (ord x `shiftR` 18)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)) :
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)) :
                    toUTF8List xs


-- | Convert UTF-8 to Unicode.
fromUTF8 :: [Word8] -> String
fromUTF8 xs = fromUTF' (map fromIntegral xs) where
    fromUTF' [] = []
    fromUTF' all@(x:xs)
        | x<=0x7F = chr x:fromUTF' xs
        | x<=0xBF = err
        | x<=0xDF = twoBytes all
        | x<=0xEF = threeBytes all
        | otherwise   = fourBytes all
    twoBytes (x1:x2:xs) = chr  (((x1 .&. 0x1F) `shift` 6) .|.
                                  (x2 .&. 0x3F)):fromUTF' xs
    twoBytes _ = error "fromUTF8: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr (((x1 .&. 0x0F) `shift` 12) .|.
                                     ((x2 .&. 0x3F) `shift` 6) .|.
                                      (x3 .&. 0x3F)):fromUTF' xs
    threeBytes _ = error "fromUTF8: illegal three byte sequence"

    fourBytes (x1:x2:x3:x4:xs) = chr (((x1 .&. 0x0F) `shift` 18) .|.
                                       ((x2 .&. 0x3F) `shift` 12) .|.
                                       ((x3 .&. 0x3F) `shift` 6) .|.
                                        (x4 .&. 0x3F)):fromUTF' xs
    fourBytes _ = error "fromUTF8: illegal four byte sequence"

    err = error "fromUTF8: illegal UTF-8 character"


-- |Convert UTF8-encoded byte array to Char
unpackCharUtf8 a b c | a `seq` b `seq` c `seq` False = undefined
unpackCharUtf8 buf pos ref_pos = do
      let addr = castPtr buf :: Ptr Word8
      ch0 <- fromIntegral <$> peekElemOff addr pos
      case () of
         _ | ch0 <= 0x7F -> do
                ref_pos =: pos+1
                return $! unsafeChr (fromIntegral ch0)
           | ch0 <= 0xDF -> do
                ref_pos =: pos+2
                ch1 <- fromIntegral <$> peekElemOff addr (pos+1)
                return $! unsafeChr (((ch0 - 0xC0) `shiftL` 6) +
                                       (ch1 - 0x80))
           | ch0 <= 0xEF -> do
                ref_pos =: pos+3
                ch1 <- fromIntegral <$> peekElemOff addr (pos+1)
                ch2 <- fromIntegral <$> peekElemOff addr (pos+2)
                return $! unsafeChr (((ch0 - 0xE0) `shiftL` 12) +
                                      ((ch1 - 0x80) `shiftL` 6) +
                                       (ch2 - 0x80))
           | otherwise -> do
                ref_pos =: pos+4
                ch1 <- fromIntegral <$> peekElemOff addr (pos+1)
                ch2 <- fromIntegral <$> peekElemOff addr (pos+2)
                ch3 <- fromIntegral <$> peekElemOff addr (pos+3)
                return $! unsafeChr (((ch0 - 0xF0) `shiftL` 18) +
                                      ((ch1 - 0x80) `shiftL` 12) +
                                      ((ch2 - 0x80) `shiftL` 6) +
                                       (ch3 - 0x80))


-- |Convert UTF8-encoded byte array to String
--unpackCStringUtf8 :: Ptr Word8 -> Int -> IO String
unpackCStringUtf8 a b c | a `seq` b `seq` c `seq` False = undefined
unpackCStringUtf8 buf pos ref_pos = do
  unpack pos
  where
    addr = castPtr buf :: Ptr Word8
    unpack nh = do
      ch0 <- fromIntegral <$> peekElemOff addr nh
      case () of
         _ | ch0 == 0 -> do
                ref_pos =: nh + 1
                return []
           | ch0 <= 0x7F -> do
                chs <- unpack (nh + 1)
                return $! (unsafeChr (fromIntegral ch0) : chs)
           | ch0 <= 0xDF -> do
                ch1 <- fromIntegral <$> peekElemOff addr (nh+1)
                chs <- unpack (nh + 2)
                return $! (unsafeChr (((ch0 - 0xC0) `shiftL` 6) +
                                       (ch1 - 0x80)) : chs)
           | ch0 <= 0xEF -> do
                ch1 <- fromIntegral <$> peekElemOff addr (nh+1)
                ch2 <- fromIntegral <$> peekElemOff addr (nh+2)
                chs <- unpack (nh + 3)
                return $! (unsafeChr (((ch0 - 0xE0) `shiftL` 12) +
                                      ((ch1 - 0x80) `shiftL` 6) +
                                       (ch2 - 0x80)) : chs)
           | otherwise -> do
                ch1 <- fromIntegral <$> peekElemOff addr (nh+1)
                ch2 <- fromIntegral <$> peekElemOff addr (nh+2)
                ch3 <- fromIntegral <$> peekElemOff addr (nh+3)
                chs <- unpack (nh + 4)
                return $! (unsafeChr (((ch0 - 0xF0) `shiftL` 18) +
                                      ((ch1 - 0x80) `shiftL` 12) +
                                      ((ch2 - 0x80) `shiftL` 6) +
                                       (ch3 - 0x80)) : chs)


----------------------------------------------------------------------------------------------------
---- Example of simple usage of in/out byte streams ------------------------------------------------
----------------------------------------------------------------------------------------------------
{-
test = do
  -- Writing and reading memory buffer as one operation
  --to do: (buf,bufsize)  <-  ByteStream.writeMemory (sign, block_type, crc)
  (sign::Word32, block_type::Int16, crc::Word64)  <-  ByteStream.readMemory buf bufsize

  -- Writing and reading file as one operation
  ByteStream.writeFile "test" [1..1000::Integer]
  (restored::[Integer]) <- ByteStream.readFile "test"

  -- Writing and reading file, divided to low-level operations
  stream <- ByteStream.createFile "test" 5000
  ByteStream.write stream  "asdfr"
  ByteStream.write stream  "12345"
  ByteStream.write stream  [10,20..500::Int]
  ByteStream.write stream  ([10,20..500] ++ [103*10^3, 106*10^6, 109*10^9, 112*10^12, 115*10^15::Integer])
  ByteStream.write stream  (concat$ replicate 100 [True,False,True])
  ByteStream.closeOut stream

  stream <- ByteStream.openFile "test" 5000
  (x::String)    <- ByteStream.read stream
  (y::String)    <- ByteStream.read stream
  (a::[Int])     <- ByteStream.read stream
  (b::[Integer]) <- ByteStream.read stream
  (c::[Bool])    <- ByteStream.read stream
  ByteStream.closeIn stream
  print [x,y]
  print a
  print b
  print c
-}
--Checklist:
--1. +receive buffers via the function receiveBuf = receiveP pipe
--2. +default write for Storable - checks for free space and does pokeByteOff elem
--3. +fast writing of strings
--4. +use writeUnchecked
--5. +finish the Integer encoding
--6. +rename WriteList to WriteListWithoutLength
--7. +simplify the function names for qualified import and make read/writeList=writeLength+writeListWithoutLength
--8. correctly move from one buffer to another
--9. read more than 100 elements in a list
--10. restore the [Bool] encoding
--11. rework string reading to get rid of reverse (put the intermediate data on the stack - without tail recursion)
--12. organize FastBufferData reading without returning a tuple - make pos either IORef Int, or FastMutInt, or IORef (Ptr CChar)


















































