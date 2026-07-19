----------------------------------------------------------------------------------------------------
---- Process for decompressing input archives.                                                  ----
---- Called from ArcExtract.hs and ArcCreate.hs (when updating and merging archives).           ----
----------------------------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module ArcvProcessExtract where

import Prelude hiding (catch)
import Control.Exception
import Control.Concurrent (MVar, Chan)
import Control.Monad
import Data.Int
import Data.IORef
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc (mallocBytes, free, alloca)
import Foreign.Marshal.Utils
import Foreign.Storable

import Utils
import Errors
import Process
import FileInfo
import Compression
import CompressionLib (aFREEARC_OK, aFREEARC_ERRCODE_OPERATION_TERMINATED, aFREEARC_ERRCODE_GENERAL, aFREEARC_ERRCODE_NOT_IMPLEMENTED, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED, compressionErrorMessage, compressMem)
import Encryption
import Options
import UI
import ArhiveStructure
import ArhiveDirectory

{-# NOINLINE decompressFile #-}
-- |Extract a file from the archive using the given decompressor process
-- and write the unpacked data via the `writer` function
decompressFile decompress_pipe compressed_file writer = do
  -- Don't try to unpack directories/empty files and files without data, since waiting for 0 bytes to arrive is a truly zen occupation ;)
  when (fiSize (cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_while (receiveP decompress_pipe) ((>=0) . snd) (uncurry writer)
    failOnTerminated

{-# NOINLINE decompressProcess #-}
-- |Process that extracts files from archives
decompressProcess command count_cbytes pipe = do
  cmd <- receiveP pipe
  case cmd of
    Nothing     -> return ()
    Just cfile' -> do
      cfile <- ref cfile'
      state <- ref (error "Decompression state is not initialized!")
      repeat_until $ do
        decompressBlock command cfile state count_cbytes pipe
        operationTerminated' <- val operationTerminated
        when operationTerminated' $ do
          sendP pipe (error "Decompression terminated", aFREEARC_ERRCODE_OPERATION_TERMINATED)
        (x,_,_) <- val state
        return (x == aStopDecompressThread || operationTerminated')


{-# NOINLINE decompressBlock #-}
-- |Decompress a single solid block
decompressBlock command cfile state count_cbytes pipe = mdo
  cfile' <- (val cfile :: IO FileToCompress)
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
      compressor  =  blCompressor block .$ limitDecompressionMemoryUsage (opt_limit_decompression_memory command)
      startPos  | compressor==aNO_COMPRESSION  =  pos  -- for -m0 we start reading directly at the required position in the block
                | otherwise                    =  (0 :: Integer)
  (state :: IORef (Integer, Integer, Integer)) =: (startPos, pos, size)
  archiveBlockSeek block startPos
  let compSize = blCompSize block - startPos
  bytesLeft <- ref compSize

  let reader buf size  =  do aBytesLeft <- val bytesLeft
                             let bytes   = min size (fromIntegral aBytesLeft :: Int)
                             len        <- archiveBlockReadBuf block buf bytes
                             bytesLeft  -= i len
                             count_cbytes  len
                             return len

  let writer (DataBuf buf len)  =  decompressStep cfile state pipe buf len
      writer  NoMoreData        =  return (0 :: Int)

  -- Add the key into the decryption algorithm record
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (cmd_arcname command) (cfile'.$cfFileInfo.$storedName)

  -- Build the decompression pipeline: the last method of the chain reads first, the first one decodes last
  -- Bind `times` before let so decompressa is not in the same mdo rec-group,
  -- allowing GHC to generalise the Pipe element type over PipeElement.
  (times :: MVar (Integer, String, [(String, Double, Integer)])) <- uiStartDeCompression "decompression"  -- Add the key into the decryption algorithm record
#ifdef __MHS__
  -- MicroHs: C-side pipeline decompression.
  -- Collects compressed data into a C growing buffer, then decompresses
  -- the entire chain using streaming Decompress() in C.
  result <- ref (0 :: Int)
  let methods    = map fromJust keyed_compressor
      decompOrder = reverse methods
      origHint = blOrigSize block
  -- Phase 1: Collect compressed data into C buffer (use large reads to minimize MHS iterations)
  darc_pipeline_init (64 * 1024 * 1024)
  let readChunkSize = 8 * 1024 * 1024 :: Int  -- 8MB per read to reduce pipe overhead
      collectLoop = do
        chunk <- mallocBytes readChunkSize
        n <- reader chunk readChunkSize
        if n <= (0 :: Int)
          then free chunk
          else do darc_pipeline_append (castPtr chunk) (fromIntegral n)
                  free chunk
                  collectLoop
  collectLoop
  -- Phase 2: Decompress through method chain entirely in C
  let decompLoop [] = return True
      decompLoop (m:ms) = do
        r <- withCString m $ \cm -> alloca $ \pResult -> do
               darc_pipeline_decompress_step_w cm (fromIntegral origHint) pResult
               peek pResult
        if r >= (0 :: CLong)
          then decompLoop ms
          else do registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage (fromIntegral r), m]
                  operationTerminated =: True
                  darc_pipeline_free
                  return False
  ok <- decompLoop decompOrder
  -- Phase 3: Feed decompressed result to writer
  when ok $ alloca $ \pBuf -> alloca $ \pSize -> do
    darc_pipeline_get_buf_w pBuf pSize
    outBuf <- peek pBuf
    outSize <- fmap fromIntegral (peek pSize :: IO CLong)
    when (outSize > (0 :: Int)) $ do
      r <- writer (DataBuf (castPtr outBuf) outSize)
      writeIORef result r
    free outBuf
#else
  let
      decompress1 p = deCompressProcess1 freearcDecompress reader times p 0
      decompressN p = deCompressProcess  freearcDecompress times         p 0
      -- Decompression pipeline: methods are applied in reverse of compression order.
      -- For chain [p1,p2,...,pN] (p1 first to compress), pN must decompress first.
      -- [p1,p2]: decompress1 p2 reads from archive, decompressN p1 reads p2 output.
      -- N-stage: last ps = outermost; tail (reverse ps) are middle stages in order.
      decompressa [p]     = decompress1 p
      decompressa [p1,p2] = decompress1 p2 |> decompressN p1
      decompressa (p1:ps) = decompress1 (last ps) |> foldl1 (|>) (map decompressN (tail (reverse ps))) |> decompressN p1

  result <- ref (0 :: Int)   -- number of bytes written by the last call to writer
  runFuncP (decompressa (map fromJust keyed_compressor)) (fail "decompressBlock::runFuncP" :: IO CompressionData) doNothing ((writer :: CompressionData -> IO Int) .>>= writeIORef result) (val result)
#endif
  uiFinishDeCompression times                    -- account for the net operation time in the UI


{-# NOINLINE deCompressProcess #-}
-- |Helper process that moves data from the input stream's buffers
-- into the input buffers of the compression/decompression routine
--   comprMethod - the compression method string with parameters, like "ppmd:o10:m48m"
--   num - the number of the process within the chain of compression processes
deCompressProcess de_compress times comprMethod num pipe = do
  -- Information about the leftover data received from the previous process but not yet handed over for compression/decompression
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", (0 :: Int))
  let
    -- Copy data from srcbuf into dstbuf and return the amount of data copied
    copyData (prevlen :: Int) dstbuf (dstlen :: Int) buf0 srcbuf (srclen :: Int) = do
      let len = srclen `min` dstlen    -- determine how much data we can read
      copyBytes dstbuf srcbuf len
      uiReadData num (i len)
      remains =: Just (buf0, srcbuf+:len, srclen-len)
      case () of
       _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- return the buffer size, since all its data has already been passed to the packer/unpacker
                             read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- read the next instruction
         | len==dstlen -> return (prevlen+len)                                   -- the buffer is full enough
         | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)    -- fill the rest of the buffer with the contents of the following files

    -- Get the next instruction from the input data stream and process it
    processNextInstruction (prevlen :: Int) (dstbuf :: Ptr CChar) (dstlen :: Int) = do
      instr <- receiveP pipe
      case instr of
        DataBuf srcbuf srclen  ->  copyData prevlen dstbuf dstlen srcbuf srcbuf srclen
        NoMoreData             ->  do remains =: Nothing;  return prevlen

    -- The input data "reading" procedure. It is important that the first call with dstlen=0 does not return until at least one byte of data has arrived from the previous process
    read_data (prevlen :: Int)  -- how much data has already been read
              (dstbuf :: Ptr CChar)   -- the buffer where the input data should be placed
              (dstlen :: Int)   -- Add the key into the decryption algorithm record
              = do     -- -> the procedure must return the number of bytes read, or 0 if the data is exhausted
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                                       -- If there is still data received from the previous process
         | srclen>(0 :: Int)  ->  copyData prevlen dstbuf dstlen buf0 srcbuf srclen --  then pass it to the packer/unpacker
         | otherwise ->  processNextInstruction prevlen dstbuf dstlen      --  otherwise get new data
        Nothing      ->  return prevlen                                    -- This solid block has ended, there is no more data

  -- The input reading procedure of the packing/unpacking process (called only once, unlike the recursive read_data)
  let reader dstbuf dstlen  =  read_data (0 :: Int) dstbuf dstlen

#ifdef __MHS__
  -- MicroHs: for real compression methods (not storing/fake), use C-side pipeline
  -- to avoid re-entrancy in ffe_eval and MHS combinator reduction overhead.
  if comprMethod == aSTORING || isFakeMethod comprMethod
    then deCompressProcess1 de_compress reader times comprMethod num pipe
    else do
      -- Collect input into C buffer (use large reads to minimize MHS iterations)
      darc_pipeline_init (64 * 1024 * 1024)
      let readChunkSize = 8 * 1024 * 1024 :: Int  -- 8MB per read
          collectLoop = do
            chunk <- mallocBytes readChunkSize
            n <- reader chunk readChunkSize
            if n <= (0 :: Int)
              then free chunk
              else do darc_pipeline_append (castPtr chunk) (fromIntegral n)
                      free chunk
                      collectLoop
      collectLoop
      -- Decompress in C
      ret <- withCString comprMethod $ \cm -> alloca $ \pResult -> do
               darc_pipeline_decompress_step_w cm 0 pResult
               peek pResult
      let retI = fromIntegral (ret :: CLong) :: Int
      uiDeCompressionTime times (comprMethod, (0.0 :: Double), i retI)
      if ret >= (0 :: CLong)
        then alloca $ \pBuf -> alloca $ \pSize -> do
          darc_pipeline_get_buf_w pBuf pSize
          outBuf <- peek pBuf
          outSize <- fmap fromIntegral (peek pSize :: IO CLong)
          uiWriteData num (i (outSize :: Int))
          sendP pipe (DataBuf (castPtr outBuf) outSize)
          (_ :: Int) <- receive_backP pipe
          free outBuf
        else do
          darc_pipeline_free
          registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage retI, comprMethod]
          operationTerminated =: True
#else
  deCompressProcess1 de_compress reader times comprMethod num pipe
#endif


{-# NOINLINE deCompressProcess1 #-}
-- |de_compress_PROCESS with a parameterizable reading function (it can read data directly
-- from the archive for the first process in the decompression chain)
deCompressProcess1 de_compress reader times comprMethod num pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let -- The input reading procedure of the packing/unpacking process
      callback "read" buf size = reader buf size
      -- Output data writing routine
      callback "write" buf size = do total' += i size
                                     uiWriteData num (i size)
                                     resendData pipe (DataBuf buf size)
      -- A "quasi-write" merely signals how much data will be written as a result of compressing
      -- the data already read. The value is passed via int64* ptr
      callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return (aFREEARC_OK :: Int)
      -- Information about the net execution time of compression/decompression
      callback "time" ptr (0 :: Int) = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                          time' =: t
                                          return (aFREEARC_OK :: Int)
      -- Other (unsupported) callbacks
      callback _ _ _ = return (aFREEARC_ERRCODE_NOT_IMPLEMENTED :: Int)

  -- THE COMPRESSION OR DECOMPRESSION PROPER
  result <- de_compress num comprMethod callback
  -- Statistics
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  -- Exit with a message if an error occurred
  unlessM (val operationTerminated) $ do
    unless (result `elem` [aFREEARC_OK, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED]) $ do
      registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage result, comprMethod]
      operationTerminated =: True
  -- Tell the previous process that the data is no longer needed, and the next one that there is no more data
  send_backP  pipe (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)
  resendData pipe NoMoreData
  return ()


-- |Handling of the next portion of unpacked data (the writer for the decompressor).
-- The state (held in the reference state) contains:
--   1) block_pos - the current position within the data block
--   2) pos       - the position at which the file (or its remaining part) starts
--   3) size      - the size of the file (or of its remaining part)
-- Accordingly, having received from the decompressor the data at address buf of length len, we must:
--   1) skip the data at the start of the buffer that precedes the file being extracted (if any)
--   2) pass on the data belonging to this file (if any)
--   3) update the state - the position within the block advances by the size of the received buffer,
--        while the position and size of the file's remaining data advance by the amount of data passed on
--   4) if the file has been fully extracted - the receiving side must be notified about it
-- and get the next decompression command
--   5) if the next file to extract turns out to be in another block, or in an already passed part
--        of the current block - the decompression of this block must be interrupted so that decompress_block
--        moves on to unpacking what is needed (it reads this data from cfile)
--
decompressStep (cfile :: IORef FileToCompress) (state :: IORef (Integer, Integer, Integer)) pipe buf len = do
  (block_pos, pos, size) <- (val state :: IO (Integer, Integer, Integer))
  if block_pos<(0 :: Integer)   -- it seems the decompressor did not notice that we want to move on to another data block
    then return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- never mind, we'll wait until it comes to its senses. alternative: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- skip the data of previous files at the start of the buffer
      data_start = buf +: skip_bytes             -- start of the data belonging to the file being extracted
      data_size  = min size (i len-skip_bytes)   -- number of bytes belonging to the file being extracted
      block_end  = block_pos+i len               -- position in the solid block corresponding to the end of the received buffer
  when (data_size>(0 :: Integer)) $ do    -- if the buffer contains data belonging to the file being extracted
    sendP pipe (data_start, i data_size)  -- then send this data over the channel to the consumer
    receive_backP pipe                    -- get confirmation that the data has been consumed
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- if the file has not been fully extracted yet
    then return len     -- then continue decompressing the block
    else do             -- otherwise move on to the next extraction task
  sendP pipe (error "End of decompressed data", (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int))
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- This message means that no more files are required from the decompression thread and it should be terminated
      state =: (aStopDecompressThread, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- if the new file lies in another block, or in this one but earlier
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- or we are unpacking a block compressed with -m0 and we have the option of skipping some files
        then do state =: (-1 :: Integer, error "undefined state.pos", error "undefined state.size")
                return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- a sign that the decompression of this block must be finished
        else do state =: (block_pos, pos, size)            -- examine the passed buffer once more,
                decompressStep cfile state pipe buf len   -- now in the context of extracting the new file

-- |Signal requesting termination of the decompression thread
aStopDecompressThread = -99


-- |Structure used to pass data to the next packing/unpacking process
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

{-# NOINLINE resendData #-}
-- |Procedure passing the output data of the packer/unpacker to the next procedure in the chain
resendData pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- return the number of consumed bytes reported by the consumer process
resendData pipe x@NoMoreData  =  sendP pipe x  >>  return 0


#ifdef __MHS__
{-# NOINLINE collectInputMHS #-}
-- |Collect all input data into a single malloc'd buffer by calling reader repeatedly.
-- Used for buffer-to-buffer compression/decompression to avoid ffe_eval re-entrancy in MicroHs.
collectInputMHS :: (Ptr CChar -> Int -> IO Int) -> IO (Ptr CChar, Int)
collectInputMHS reader = go [] (0 :: Int)
  where
    chunkSize = 65536 :: Int
    go chunks total = do
      chunk <- mallocBytes chunkSize
      n <- reader chunk chunkSize
      if n <= (0 :: Int)
        then do
          free chunk
          buf <- mallocBytes (max total (1 :: Int))
          fillBuf buf (0 :: Int) (reverse chunks)
          mapM_ (free . fst) chunks
          return (buf, total)
        else go ((chunk, n) : chunks) (total + n)
    fillBuf _ _ [] = return ()
    fillBuf buf off ((src, len) : rest) = do
      copyBytes (buf `plusPtr` off) src len
      fillBuf buf (off + len) rest

aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL :: Int
aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL = -4
#endif


-- Compatibility aliases (old underscore-style names)
decompress_PROCESS = decompressProcess
decompress_file    = decompressFile
de_compress_PROCESS = deCompressProcess
resend_data         = resendData

#ifdef __MHS__
-- C-side pipeline FFI (Environment.cpp)
foreign import ccall "darc_pipeline_init"              darc_pipeline_init :: CLong -> IO ()
foreign import ccall "darc_pipeline_append"            darc_pipeline_append :: Ptr () -> CLong -> IO ()
foreign import ccall "darc_pipeline_decompress_step_w" darc_pipeline_decompress_step_w :: CString -> CLong -> Ptr CLong -> IO ()
foreign import ccall "darc_pipeline_get_buf_w"         darc_pipeline_get_buf_w :: Ptr (Ptr ()) -> Ptr CLong -> IO ()
foreign import ccall "darc_pipeline_free"              darc_pipeline_free :: IO ()
#endif
