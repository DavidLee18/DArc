----------------------------------------------------------------------------------------------------
---- Compression, decompression and CRC calculation.                                            ----
---- Data types CompressionMethod, Compressor, UserCompressor - compression method description. ----
---- Interface to the C routines that do all the real work.                                     ----
----------------------------------------------------------------------------------------------------
module Compression (module Compression, CompressionLib.decompressMem) where

import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Ptr
import System.IO.Unsafe

import qualified CompressionLib
import Utils
import Errors
import Files
import qualified ByteStream


-- |A compression method or preprocessor and its parameters
type CompressionMethod  =  CompressionLib.Method

-- "Compression" methods supported directly rather than through CompressionLib
aSTORING              = "storing"
aFAKE_COMPRESSION     = "fake"
aCRC_ONLY_COMPRESSION = "crc"

-- |Fake (non-decompressible) compression methods.
isFakeMethod             =  any_function [(==aFAKE_COMPRESSION), (==aCRC_ONLY_COMPRESSION)] . method_name
-- |The LZP compression method.
isLZP_Method     method  =  method_name method == "lzp"
-- |The Tornado compression method.
isTornado_Method method  =  method_name method == "tor"
-- |The DICT compression method.
isDICT_Method    method  =  method_name method == "dict"
-- |The TTA compression method.
isTTA_Method     method  =  method_name method == "tta"
-- |The MM compression method.
isMM_Method      method  =  method_name method == "mm"
-- |The JPG compression method.
isJPG_Method     method  =  method_name method == "jpg"
-- |The GRZip compression method.
isGRZIP_Method   method  =  method_name method == "grzip"
-- |A very fast compression method (>10 mb/s on a 1GHz processor)
isVeryFastMethod         =  CompressionLib.compressionIs "VeryFast?"
-- |A fast decompression method
isFastDecMethod          =  not . any_function [(=="ppmd"), (=="ppmm"), (=="pmm"), isEXTERNAL_Method] . method_name
-- |A compression method carried out by an external program
isEXTERNAL_Method        =  CompressionLib.compressionIs "external?"
-- |An encryption method.
isEncryption             =  CompressionLib.compressionIs "encryption?"
-- |Non-solid method — each block is compressed independently (0.67).
isNonSolidMethod         =  CompressionLib.compressionIs "nosolid?"
-- |Memory barrier for a chain of compression methods (0.67): the method splits memory accounting into independent clusters.
isMemoryBarrier_Compression    =  any_function [isEXTERNAL_Method, CompressionLib.compressionIs "MemoryBarrierCompression?"]
isMemoryBarrier_Decompression  =  any_function [isEXTERNAL_Method, CompressionLib.compressionIs "MemoryBarrierDecompression?"]


-- |The sequence of compression algorithms used to process the data
type Compressor = [CompressionMethod]

-- |The "storing" method (-m0)
aNO_COMPRESSION = [aSTORING] :: Compressor

-- |Very fast compression for already compressed files
aCOMPRESSED_METHOD = split_compressor "tor:8m:c3"

-- |This is a fake compressor if it holds exactly one compression method and that method is fake
isFakeCompressor (method:xs)  =  isFakeMethod method  &&  null xs

-- |This is a fake compressor if it holds exactly one compression method and that method is "fake"
isReallyFakeCompressor (method:xs)  =  method_name method == aFAKE_COMPRESSION  &&  null xs

-- |This is an LZP compressor if it holds exactly one compression method and that method is LZP
isLZP_Compressor (method:xs)  =  isLZP_Method method  &&  null xs

-- |This is a very fast compressor if it holds exactly one, very fast compression method.
isVeryFastCompressor (method:xs)  =  isVeryFastMethod method  &&  null xs

-- |This is a fast decompressor if it includes only fast decompression methods
isFastDecompressor = all isFastDecMethod


-- |The choice of compressor depending on the type of data being processed.
-- The first element of the list is unnamed and describes the compressor used
-- by default (for files of all other types not explicitly described in the list)
type UserCompressor = [(String,Compressor)]  -- an association list like "$text->m3t, $exe->m3x, $compressed->m0"

getCompressors :: UserCompressor -> [Compressor]
getCompressors = map snd

getMainCompressor :: UserCompressor -> Compressor
getMainCompressor = snd . head

-- |This is the Storing method if it holds only the single aNO_COMPRESSION compressor for files of all types
isStoring ((_,compressor):xs)  =  compressor==aNO_COMPRESSION  &&  null xs

-- |This is fake compression if it holds only a single fake compressor for files of all types
isFakeCompression ((_,compressor):xs)  =  isFakeCompressor compressor  &&  null xs

-- |This is LZP compression if it holds only a single LZP compressor for files of all types
isLZP_Compression ((_,compressor):xs)  =  isLZP_Compressor compressor  &&  null xs

-- |This is very fast compression if it uses only very fast compressors for files of all types
isVeryFastCompression = all (isVeryFastCompressor . snd)

-- |This is fast decompression if it uses only fast decompressors for files of all types
isFastDecompression = all (isFastDecompressor . snd)

-- |Find the compressor best suited to data of type `ftype`.
-- If no compressor for files of this type is described in the list, return the compressor
-- used by default, stored in the first element of the list
findCompressor ftype list  =  lookup ftype list  `defaultVal`  snd (head list)

-- |For writing information about the compression algorithms used into the archive directory.
instance ByteStream.BufferData Compressor where
  write buf x  =  ByteStream.write buf (join_compressor x)
  read  buf    =  ByteStream.read  buf  >>==  split_compressor


----------------------------------------------------------------------------------------------------
----- Operations on compression algorithms                                                     -----
----------------------------------------------------------------------------------------------------

class Compression a where
  getCompressionMem              :: a -> Integer
  getDecompressionMem            :: a -> Integer
  getBlockSize                   :: a -> MemSize
  getDictionary                  :: a -> MemSize
  setDictionary                  :: MemSize -> a -> a
  limitCompressionMem            :: MemSize -> a -> a
  limitDecompressionMem          :: MemSize -> a -> a
  limitDictionary                :: MemSize -> a -> a
  limitCompressionMemoryUsage    :: MemSize -> a -> a
  limitDecompressionMemoryUsage  :: MemSize -> a -> a

-- |Turn a CompressionLib function that modifies a Method into a function that modifies a CompressionMethod
liftSetter action  method | aSTORING ==  method   =  method
liftSetter action  method | isFakeMethod method   =  method
liftSetter action  method                         =  action method

-- |Turn a CompressionLib function that queries a Method into a function that queries a CompressionMethod
liftGetter action  method | aSTORING ==  method   =  0
liftGetter action  method | isFakeMethod method   =  0
liftGetter action  method                         =  action method

instance Compression CompressionMethod where
  getCompressionMem              = i . liftGetter   CompressionLib.getCompressionMem
  getDecompressionMem            = i . liftGetter   CompressionLib.getDecompressionMem
  getBlockSize                   =  liftGetter   CompressionLib.getBlockSize
  getDictionary                  =  liftGetter   CompressionLib.getDictionary
  setDictionary                  =  liftSetter . CompressionLib.setDictionary
  limitCompressionMem            =  liftSetter . CompressionLib.limitCompressionMem
  limitDecompressionMem          =  liftSetter . CompressionLib.limitDecompressionMem
  limitDictionary                =  liftSetter . CompressionLib.limitDictionary
  limitCompressionMemoryUsage    =  limitCompressionMem
  limitDecompressionMemoryUsage  =  const id

instance Compression Compressor where
  getCompressionMem              =  calcMem getCompressionMem
  getDecompressionMem            =  calcMem getDecompressionMem
  getBlockSize                   =  maximum . map getBlockSize
  getDictionary                  =  maximum . map getDictionary
  setDictionary                  =  mapLast . setDictionary
  limitCompressionMem            =  map . limitCompressionMem
  limitDecompressionMem          =  map . limitDecompressionMem
  limitDictionary                =  compressionLimitDictionary
  limitCompressionMemoryUsage    =  compressionLimitMemoryUsage
  limitDecompressionMemoryUsage  =  genericLimitMemoryUsage getDecompressionMem

instance Compression UserCompressor where
  -- Determine the maximum memory usage / block size in the given UserCompressor
  getCompressionMem              =  maximum . map (getCompressionMem   . snd)
  getDecompressionMem            =  maximum . map (getDecompressionMem . snd)
  getBlockSize                   =  maximum . map (getBlockSize        . snd)
  getDictionary                  =  maximum . map (getDictionary       . snd)
  -- Set the dictionary / Limit the memory used during compression/decompression
  -- for all the methods making up the UserCompressor at once
  setDictionary                  =  mapSnds . setDictionary
  limitCompressionMem            =  mapSnds . limitCompressionMem
  limitDecompressionMem          =  mapSnds . limitDecompressionMem
  limitDictionary                =  mapSnds . limitDictionary
  limitCompressionMemoryUsage    =  mapSnds . limitCompressionMemoryUsage
  limitDecompressionMemoryUsage  =  mapSnds . limitDecompressionMemoryUsage


-- |The minimum amount of memory required for compression/decompression
compressorGetShrinkedCompressionMem    = maximum . map (compressionGetShrinkedCompressionMem . snd)
compressorGetShrinkedDecompressionMem  = maximum . map (compressionGetShrinkedDecompressionMem . snd)
compressionGetShrinkedCompressionMem    = maximum . map getCompressionMem
compressionGetShrinkedDecompressionMem  = maximum . map getDecompressionMem

-- |Limit the dictionaries for a chain of algorithms, stopping after the first algorithm
-- that can significantly inflate the data (such as precomp). There are no such algorithms
-- among the internal ones, but we treat every external one as suspect :)
compressionLimitDictionary mem (x:xs) =  new_x : (not(isEXTERNAL_Method new_x)  &&&  compressionLimitDictionary mem) xs
                                             where new_x = limitDictionary mem x
compressionLimitDictionary mem []     =  []

-- |Reduces the memory requirements of each algorithm in the chain down to mem
-- and then inserts tempfile calls between them if necessary
compressionLimitMemoryUsage mem  =  genericLimitMemoryUsage getCompressionMem mem . map (limitCompressionMem mem)

-- |Inserts tempfile calls between compression algorithms, splitting them into "clusters"
-- that fit into memory_limit+5% ("small" algorithms must not start new clusters).
-- For dict/dict+lzp a special memory accounting is used (blocksize*2 for both, blocksize/2 at the output),
-- while external compressors reset the memory usage to zero
genericLimitMemoryUsage getMem memory_limit = go (0::Double) ""
  where go _   _    []      =  []
        go mem prev (x:xs) | isEXTERNAL_Method x          =  x: go 0            x xs
                           | mem+newMem < memlimit*1.05   =  x: go (mem+newMem) x xs
                           | otherwise                    =  "tempfile":x: go newMem x xs

           where newMem | mem==0 && isDICT_Method x             =  realToFrac (getBlockSize x) / 2
                        | isDICT_Method prev && isLZP_Method x  =  0
                        | otherwise                             =  realToFrac$ getMem x
                 memlimit = realToFrac memory_limit

-- |Compute the memory requirements of a chain of compression algorithms, taking into account their split
-- into clusters by compressionIs "external?" and the special accounting for dict/dict+lzp
calcMem getMem  = maximum . map getMemSum . splitOn isEXTERNAL_Method
  where getMemSum (x:y:xs) | isDICT_Method x && isLZP_Method y  =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)   | isDICT_Method x                    =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)                                        =  i(getMem x) + getMemSum xs
        getMemSum []                                            =  0::Integer

-- |Removes every mention of "tempfile" from the compression algorithm spec.
compressionDeleteTempCompressors = filter (/="tempfile")


----------------------------------------------------------------------------------------------------
----- (De)compression of data stream                                                           -----
----------------------------------------------------------------------------------------------------

{-
compress   method callback      - compress the data
decompress method callback      - decompress the data

  method :: CompressionMethod - the compression algorithm
  callback "read" buf size - read the input data into the buffer `buf` of length `size`
                             Returns 0   - end of data
                                        <0  - abort the (de)compression (error, or no more data needed)
                                        >0  - the number of bytes read
  callback "write" buf size - write the output data
                              Returns <0  - abort the (de)compression (error, or no more data needed)
                                         >=0 - all ok
                              By the time this function returns, the data must be "consumed", because
                                the (de)compressor may start writing new data to the same place
The input and output buffers are allocated and freed by the (de)compressor
-}

-- |Compression procedures for the various compression algorithms.
freearcCompress   num method | aSTORING ==  method =  copy_data
freearcCompress   num method | isFakeMethod method =  eat_data
freearcCompress   num method                       =  checkingCtrlBreak num (CompressionLib.compress method)

-- |Decompression procedures for the various compression algorithms.
freearcDecompress num method | aSTORING ==  method =  copy_data
freearcDecompress num method | isFakeMethod method =  impossible_to_decompress   -- these kinds of compressed data cannot be decompressed
freearcDecompress num method                       =  checkingCtrlBreak num (CompressionLib.decompress method)

-- |Since Haskell code called from C cannot receive exceptions,
-- we add explicit checks to the read/write procedures
checkingCtrlBreak num action callback = do
  let checked_callback what buf size auxdata = do
        operationTerminated' <- val operationTerminated
        if operationTerminated'
          then return CompressionLib.aFREEARC_ERRCODE_OPERATION_TERMINATED   -- foreverM doNothing0
          else callback what buf size
  --
  res <- checked_callback "read" nullPtr 0 undefined   -- this call lets us postpone starting the next compression/decompression algorithm in the chain until the previous one returns at least some data (and if it is a block-based algorithm - until it has processed the whole block)
  if res<0  then return res
            else action (checked_callback)

-- |Copying the data without compression (-m0)
copy_data callback = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do  -- we use `alloca` so the allocated buffer is freed automatically on exit
    let go ptr = do
          len <- callback "read" ptr ((buf+:aHUGE_BUFFER_SIZE)-:ptr)
          if (len>0)
            then do let newptr = ptr+:len
                    if newptr < buf+:aHUGE_BUFFER_SIZE
                       then go newptr
                       else do result <- callback "write" buf (newptr-:buf)
                               if (result>=0)
                                 then go buf
                                 else return (result)  -- Return a negative number if an error occurred / no more data is needed
            else do if (len==0 && ptr>buf)
                      then do result <-  callback "write" buf (ptr-:buf)
                              return (if result>0 then 0 else result)
                      else return len  -- Return 0 if the data ran out, and a negative number if an error occurred / no more data is needed
    go buf -- return the result

-- |We read everything, write nothing, and the CRC is computed elsewhere ;)
eat_data callback = do
  allocaBytes aBUFFER_SIZE $ \buf -> do  -- we use `alloca` so the allocated buffer is freed automatically on exit
    let go = do
          len <- callback "read" buf aBUFFER_SIZE
          if (len>0)
            then go
            else return len   -- Return 0 if the data ran out, and a negative number if an error occurred / no more data is needed
    go  -- return the result

impossible_to_decompress callback = do
  return CompressionLib.aFREEARC_ERRCODE_GENERAL   -- return an error straight away, since this algorithm (FAKE/CRC_ONLY) cannot be decompressed

{-# NOINLINE checkingCtrlBreak               #-}
{-# NOINLINE copy_data                       #-}
{-# NOINLINE eat_data                        #-}


----------------------------------------------------------------------------------------------------
----- CRC calculation ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |The CRC of a file
type CRC  = CUInt
aINIT_CRC = 0xffffffff  :: CRC

-- |CRC-32 polynomial lookup table (IEEE 802.3)
{-# NOINLINE crcTable #-}
crcTable :: UArray Int Word32
crcTable = UA.listArray (0, 255) $ map buildEntry [0..255]
  where
    buildEntry :: Int -> Word32
    buildEntry i = foldl step (fromIntegral i) ([0..7] :: [Int])
    step c _ = if c .&. 1 /= 0
                 then (c `shiftR` 1) `xor` 0xEDB88320
                 else  c `shiftR` 1

-- |Update a running CRC-32 over a memory buffer. Delegates to the C
-- UpdateCRC from Environment.cpp, which processes a whole buffer per call
-- rather than iterating byte-by-byte in Haskell — critical under MicroHs
-- where combinator reduction makes per-byte loops ~7µs/byte.
foreign import ccall "UpdateCRC" c_UpdateCRC :: Ptr () -> CUInt -> CUInt -> IO CUInt

{-# NOINLINE updateCRC #-}
updateCRC :: (Integral n) => Ptr a -> n -> CRC -> IO CRC
updateCRC addr len startCRC =
  c_UpdateCRC (castPtr addr) (fromIntegral len) (fromIntegral startCRC)
  >>= return . fromIntegral

finishCRC = xor aINIT_CRC

-- |Compute the CRC of the data in a buffer (accepts any Integral length)
calcCRC :: Integral n => Ptr a -> n -> IO CRC
calcCRC addr len  =  updateCRC addr len aINIT_CRC  >>==  finishCRC

-- |Compute the CRC of a non-unicode string (characters with codes 0..255)
crc32 str  =  unsafePerformIO$ withCStringLen str (uncurry calcCRC)


-------------------------------------------------------------------------------------------------------------
-- Encode/decode compression method for parsing options/printing info about selected compression method -----
-------------------------------------------------------------------------------------------------------------

-- |Parse command-line option that represents compression method.
-- Decode the textual representation of a compression method, turning it into an association list
-- "file type -> compression method". The first element of this list describes the default compression method
decode_method configuredMethodSubsts str =
    str                       -- "3/$obj=2b/$iso=ecm+3b"
    .$ subst list             -- "3b/3t/$obj=2b/$iso=ecm+3b"
    .$ split_to_methods       -- [("","exe+3b"), ("$obj","3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ keepOnlyLastOn fst     -- [("","exe+3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ filter (not.null.snd)  -- "-m$bmp=" means forbidding the use of a special algorithm for the $bmp group
    .$ mapSnds (subst2 list)  -- [("",["exe","lzma"]), ("$text",["ppmd"]), ("$obj",["lzma"]), ("$iso",["ecm","lzma"])]

    where list = prepareSubsts (concatMap reorder [configuredMethodSubsts, builtinMethodSubsts])   -- user substitutions first, then the built-in ones, to give the former priority
          reorder list = a++b  where (a,b) = partition (notElem '#') list                          -- within those groups: first the lines without #, then those with # (specific substitutions first, then general ones)

-- List-driven substitution for a compression method (the generic notation covering files of all types)
subst list method  =  joinWith "/" (main_methods:group_methods++user_methods)
  where -- From a spec like -m3/$obj=2b we take only the first part, up to the slash, for expansion
        main:user_methods = split '/' method
        -- Expansion of the main compression methods, such as 3x = 3xb/3xt
        main_methods = case (lookup main list) of
            Just x  -> subst list x   -- On success, repeat recursively
            Nothing -> main           -- No more substitutions
        -- Find in the substitution list the extra compression methods for individual groups, such as 3x$iso = ecm+exe+3xb
        group_methods = list .$ keepOnlyFirstOn fst                      -- drop duplicate definitions (not very efficient to do it right here, but it is at the point of use)
                             .$ mapMaybe (startFrom main . join2 "=")    -- keep only the definitions starting with 3x, stripping that 3x
                             .$ filter (("$"==) . take 1)                  -- and of those, only the ones starting with $

-- List-driven substitution for a compression algorithm (the sequence of compressors for one specific file type)
subst2 list  =  concatMap f . split_compressor
    where f method = let (head,params)  =  break (==':') method
                     in case (lookup head list) of
                          Just new_head -> subst2 list (new_head++params)
                          Nothing       -> [decode_one_method method]

-- |Decode an explicitly specified compression method.
decode_one_method method | isFakeMethod method = method
                         | otherwise           = CompressionLib.canonizeCompressionMethod method

-- Turns a long string describing the compression methods for different file types
-- into an association array of (file type, compression method)
split_to_methods method = case (split '/' method) of
    [_]                 ->  [("",method)]   -- one method for files of all types
    x : xs@(('$':_):_) ->  ("",x) : map (split2 '=') xs   -- m1/$type=m2...
    b : t : xs          ->  [("","exe+"++b), ("$obj",b), ("$text",t)] ++ map (split2 '=') xs   -- m1/m2/$type=m3...

-- Prepare the substitution list for use with lookup
prepareSubsts x = x
    -- Remove empty lines, spaces and comments
    .$ map (filter (not . isSpace) . fst . split2 ';') .$ filter (not . null)
    -- Replace each line containing # with 9 lines where # runs over the values 1 to 9
    .$ concatMap (\s -> if s `contains` '#'  then map (\d->replace '#' d s) ['1'..'9']  else [s])
    -- Convert the list of "a=b" strings into a list suitable for lookup
    .$ map (split2 '=')

-- Built-in compression method definitions, in the same format as used in arc.ini
builtinMethodSubsts = [
      ";High-level method definitions"
    , "x  = 9            ;highest compression mode using only internal algorithms"
    , "ax = 9p           ;highest compression mode involving external compressors"
    , "0  = storing"
    , "1  = 1b  / $exe=exe+1b"
    , "1x = 1"
    , "#  = #rep+exe+#xb / $obj=#b / $text=#t"
    , "#x = #xb/#xt"
    , ""
    , ";Text files compression with slow decompression"
    , "1t  = 1b"
    , "2t  = grzip:m4:8m:32:h15"
    , "3t  = dict:p: 64m:85% + lzp: 64m: 24:h20        :92% + grzip:m3:8m:l"
    , "4t  = dict:p: 64m:80% + lzp: 64m: 65:d1m:s16:h20:90% + ppmd:8:96m"
    , "5t  = dict:p: 64m:80% + lzp: 80m:105:d1m:s32:h22:92% + ppmd:12:192m"
    , "6t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "7t  = dict:p:128m:80% + lzp:320m:185:d1m:s32:h24:92% + ppmd:20:768m"
    , "8t  = dict:p:128m:80% + lzp:640m:225:d1m:s32:h25:92% + ppmd:24:1536m"
    , "9t  = dict:p:128m:80% + lzp:800m:235:d1m:s32:h26:92% + ppmd:25:2047m"
    , ""
    , ";Binary files compression with slow and/or memory-expensive decompression"
    , "1b  = 1xb"
    , "#b  = #rep+#bx"
    , "2rep  = rep:  96m"
    , "3rep  = rep:  96m"
    , "4rep  = rep:  96m"
    , "5rep  = rep: 128m"
    , "6rep  = rep: 256m"
    , "7rep  = rep: 512m"
    , "8rep  = rep:1024m"
    , "9rep  = rep:2047m"
    , ""
    , ";Text files compression with fast decompression"
    , "1xt = 1xb"
    , "2xt = 2xb"
    , "3xt = dict:  64m:80% + tor:7:96m:h64m"
    , "4xt = dict:  64m:75% + 4binary"
    , "#xt = dict: 128m:75% + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1xb = 4x4:tor:3"
    , "2xb = 4x4:tor:6"
    , "#xb = delta + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1binary = tor:3"
    , "2binary = tor:6"
    , "3binary = 4x4:b8m:lzma:8m:h64m:fast:mc8"
    , "4binary = 4x4:b16m:lzma:16m:h64m:normal:mc16"
    , "5binary = 4x4:b16m:lzma:16m:max"
    , "6binary = 4x4:b32m:lzma:32m:max"
    , "7binary = 4x4:b64m:lzma:64m:max"
    , "8binary = 4x4:b128m:lzma:128m:max"
    , "9binary = 4x4:b254m:lzma:254m:max"
    , ""
    , ";Synonyms"
    , "bcj = exe"
    , "#bx = #xb"
    , "#tx = #xt"
    , "x#  = #x"    -- accept options like "-mx7" to mimic 7-zip
    , ""
    , ";Compression modes involving external PPMONSTR.EXE"
    , "#p  = #rep+exe+#xb / $obj=#pb / $text=#pt"
    , "5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0"
    , "6pt = dict:p: 64m:80% + lzp: 64m:64:h22:85% + pmm:16:384m:r1"
    , "7pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:20:768m:r1"
    , "8pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:24:1536m:r1"
    , "9pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:25:2047m:r1"
    , "#pt = #t"
    , "#pb = #b"
    , ""
    , "#q  = #qb/#qt"
    , "5qt = dict:p:64m:80% + lzp:64m:64:d1m:24:h22:85% + pmm:10:160m:r1"
    , "5qb = rep: 128m      + delta                     + pmm:16:160m:r1"
    , "6qb = rep: 256m      + delta                     + pmm:20:384m:r1"
    , "7qb = rep: 512m      + delta                     + pmm:22:768m:r1"
    , "8qb = rep:1024m      + delta                     + pmm:24:1536m:r1"
    , "9qb = rep:2047m      + delta                     + pmm:25:2047m:r1"
    , "#qt = #pt"
    , "#qb = #pb"
    , ""
    , ";Sound wave files are compressed best with TTA"
    , "wav     = tta      ;best compression"
    , "wavfast = tta:m1   ;faster compression and decompression"
    , "1$wav  = wavfast"
    , "2$wav  = wavfast"
    , "#$wav  = wav"
    , "#x$wav = wavfast"
    , "#p$wav = wav"
    , ""
    , ";Bitmap graphic files are compressed best with GRZip"
    , "bmp        = mm    + grzip:m1:l:a  ;best compression"
    , "bmpfast    = mm    + grzip:m4:l:a  ;faster compression"
    , "bmpfastest = mm:d1 + tor:3:t0      ;fastest one"
    , "1$bmp  = bmpfastest"
    , "2$bmp  = bmpfastest"
    , "3$bmp  = bmpfast"
    , "#$bmp  = bmp"
    , "1x$bmp = bmpfastest"
    , "2x$bmp = bmpfastest"
    , "#x$bmp = mm+#binary"
    , "#p$bmp = bmp"
    , ""
    , ";Quick & dirty compression for data already compressed"
    , "4$compressed   = rep:96m + tor:c3"
    , "3$compressed   = rep:96m + tor:3"
    , "2$compressed   = rep:96m + tor:3"
    , "4x$compressed  = tor:8m:c3"
    , "3x$compressed  = rep:8m  + tor:3"
    , "2x$compressed  = rep:8m  + tor:3"
    ]

-- |Is this a multimedia file type?
isMMType x  =  x `elem` words "$wav $bmp"

-- |In a sense the inverse operation - guessing the file type from its compressor
typeByCompressor c  =  case (map method_name c) of
  xs | xs `contains` "tta"        -> "$wav"
     | xs `contains` "mm"         -> "$bmp"
     | xs `contains` "grzip"      -> "$text"
     | xs `contains` "ppmd"       -> "$text"
     | xs `contains` "pmm"        -> "$text"
     | xs `contains` "dict"       -> "$text"
     | xs == aNO_COMPRESSION      -> "$compressed"
     | xs == ["rep","tor"]        -> "$compressed"
     | xs `contains` "ecm"        -> "$iso"
     | xs `contains` "precomp"    -> "$precomp"
     | xs == ["precomp","rep"]    -> "$jpgsolid"
     | xs `contains` "jpg"        -> "$jpg"
     | xs `contains` "exe"        -> "$binary"
     | xs `contains` "lzma"       -> "$obj"
     | xs `contains` "tor"        -> "$obj"
     | otherwise                  -> "default"

-- |The list of all file types detected this way
typesByCompressor = words "$wav $bmp $text $compressed $iso $precomp $jpgsolid $jpg $obj $binary $exe"


-- |Human-readable description of compression method
encode_method uc  =  joinWith ", " (map encode_one_method uc)
encode_one_method (group,compressor)  =  between group " => " (join_compressor compressor)
join_compressor   =  joinWith "+"

-- |Opposite to join_compressor (used to read compression method from archive file)
split_compressor  =  split '+'

-- |Process the algorithms in a compressor with the imperative operation process
process_algorithms process compressor = do
    return (split_compressor compressor)
       >>=  mapM process
       >>== join_compressor

-- |Split a compression method into its header and the individual parameters
split_method = split ':'

-- |The name of a compression method.
method_name = head . split_method

-- |A string telling the user how much memory is being used
showMem 0      = "0b"
showMem mem    = showM [(gb,"gb"),(mb,"mb"),(kb,"kb"),(b,"b"),error"showMem"] mem

showMemory 0   = "0 bytes"
showMemory mem = showM [(gb," gbytes"),(mb," mbytes"),(kb," kbytes"),(b," bytes"),error"showMemory"] mem

showM xs@( (val,str) : ~(nextval,_) : _) mem =
  if mem `mod` val==0 || mem `div` nextval>=4096
    then show((mem+val`div` 2) `div` val)++str
    else showM (tail xs) mem

-- |Round the memory amount up so that it becomes readable
roundMemUp mem | mem>=4096*kb = mem `roundUp` mb
               | otherwise    = mem `roundUp` kb

{-# NOINLINE builtinMethodSubsts #-}
{-# NOINLINE decode_method #-}
{-# NOINLINE showMem #-}

