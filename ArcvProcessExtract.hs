----------------------------------------------------------------------------------------------------
---- ������� ���������� ������� �������.                                                        ----
---- ���������� �� ArcExtract.hs � ArcCreate.hs (��� ���������� � ������� �������).             ----
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
-- |���������� ����� �� ������ � �������������� ����������� �������� �������������
-- � ������� ������������� ������ � ������� ������� `writer`
decompressFile decompress_pipe compressed_file writer = do
  -- �� �������� ����������� ��������/������ ����� � ����� ��� ������, ��������� ������� ��������� 0 ������ - �������� �������� ������� ;)
  when (fiSize (cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_while (receiveP decompress_pipe) ((>=0) . snd) (uncurry writer)
    failOnTerminated

{-# NOINLINE decompressProcess #-}
-- |�������, ��������������� ����� �� �������
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
-- |����������� ���� �����-����
decompressBlock command cfile state count_cbytes pipe = mdo
  cfile' <- (val cfile :: IO FileToCompress)
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
      compressor  =  blCompressor block .$ limitDecompressionMemoryUsage (opt_limit_decompression_memory command)
      startPos  | compressor==aNO_COMPRESSION  =  pos  -- ��� -m0 �������� ������ �������� � ������ ������� � �����
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

  -- �������� ���� � ������ ��������� ������������
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (cmd_arcname command) (cfile'.$cfFileInfo.$storedName)

  -- Создадим конвейер декомпрессии: последний метод цепочки читает первым, первый декодирует последним
  -- Bind `times` before let so decompressa is not in the same mdo rec-group,
  -- allowing GHC to generalise the Pipe element type over PipeElement.
  (times :: MVar (Integer, String, [(String, Double, Integer)])) <- uiStartDeCompression "decompression"  -- ������� ��������� ��� ����� ������� ����������
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

  result <- ref (0 :: Int)   -- ���������� ����, ���������� � ��������� ������ writer
  runFuncP (decompressa (map fromJust keyed_compressor)) (fail "decompressBlock::runFuncP" :: IO CompressionData) doNothing ((writer :: CompressionData -> IO Int) .>>= writeIORef result) (val result)
#endif
  uiFinishDeCompression times                    -- ������ � UI ������ ����� ��������


{-# NOINLINE deCompressProcess #-}
-- |��������������� ������� �������������� ������ �� ������� �������� ������
-- �� ������� ������ ��������� ��������/����������
--   comprMethod - ������ ������ ������ � �����������, ���� "ppmd:o10:m48m"
--   num - ����� �������� � ������� ��������� ��������
deCompressProcess de_compress times comprMethod num pipe = do
  -- ���������� �� ������� ������, ���������� �� ����������� ��������, �� ��� �� ������������ �� ��������/����������
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", (0 :: Int))
  let
    -- ����������� ������ �� srcbuf � dstbuf � ���������� ������ ������������� ������
    copyData (prevlen :: Int) dstbuf (dstlen :: Int) buf0 srcbuf (srclen :: Int) = do
      let len = srclen `min` dstlen    -- ���������� - ������� ������ �� ����� ���������
      copyBytes dstbuf srcbuf len
      uiReadData num (i len)
      remains =: Just (buf0, srcbuf+:len, srclen-len)
      case () of
       _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- ���������� ������ ������, ��������� ��� ������ �� ���� ��� �������� ����������/������������
                             read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- ��������� ��������� ����������
         | len==dstlen -> return (prevlen+len)                                   -- ����� ���������� ��������
         | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)    -- �������� ������� ������ ���������� ��������� ������

    -- �������� ��������� ���������� �� ������ ������� ������ � ���������� �
    processNextInstruction (prevlen :: Int) (dstbuf :: Ptr CChar) (dstlen :: Int) = do
      instr <- receiveP pipe
      case instr of
        DataBuf srcbuf srclen  ->  copyData prevlen dstbuf dstlen srcbuf srcbuf srclen
        NoMoreData             ->  do remains =: Nothing;  return prevlen

    -- ��������� "������" ������� ������. �����, ����� ������ ����� � dstlen=0 �� ��������� ���������� ���� �� �������� ���� �� ���� ���� ������ �� ����������� ��������
    read_data (prevlen :: Int)  -- ������� ������ ��� ���������
              (dstbuf :: Ptr CChar)   -- �����, ���� ����� ��������� ������� ������
              (dstlen :: Int)   -- ������ ������
              = do     -- -> ��������� ������ ���������� ���������� ����������� ���� ��� 0, ���� ������ �����������
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                                       -- ���� ��� ���� ������, ���������� �� ����������� ��������
         | srclen>(0 :: Int)  ->  copyData prevlen dstbuf dstlen buf0 srcbuf srclen --  �� �������� �� ����������/������������
         | otherwise ->  processNextInstruction prevlen dstbuf dstlen      --  ����� �������� �����
        Nothing      ->  return prevlen                                    -- ���� solid-���� ����������, ������ ������ ���

  -- ��������� ������ ������� ������ �������� ��������/���������� (���������� ���� �������, � ������� �� ����������� read_data)
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
-- |deCompressProcess � ��������������� �������� ������ (����� ������ ������ ��������
-- �� ������ ��� ������� �������� � ������� ����������)
deCompressProcess1 de_compress reader times comprMethod num pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let -- ��������� ������ ������� ������ �������� ��������/����������
      callback "read" buf size = reader buf size
      -- ��������� ������ �������� ������
      callback "write" buf size = do total' += i size
                                     uiWriteData num (i size)
                                     resendData pipe (DataBuf buf size)
      -- "�����������" ������ ������������� ������� ������ ����� �������� � ���������� ������
      -- ��� ����������� ������. �������� ��������� ����� int64* ptr
      callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return (aFREEARC_OK :: Int)
      -- ���������� � ������ ������� ���������� ��������/����������
      callback "time" ptr (0 :: Int) = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                          time' =: t
                                          return (aFREEARC_OK :: Int)
      -- ������ (����������������) callbacks
      callback _ _ _ = return (aFREEARC_ERRCODE_NOT_IMPLEMENTED :: Int)

  -- ���������� �������� ��� ����������
  result <- de_compress num comprMethod callback
  -- ����������
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  -- ������ � ����������, ���� ��������� ������
  unlessM (val operationTerminated) $ do
    unless (result `elem` [aFREEARC_OK, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED]) $ do
      registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage result, comprMethod]
      operationTerminated =: True
  -- ������� ����������� ��������, ��� ������ ������ �� �����, � ���������� - ��� ������ ������ ���
  send_backP  pipe (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)
  resendData pipe NoMoreData
  return ()


-- |��������� ��������� ������ ������������� ������ (writer ��� ������������).
-- ��������� (�������� �� ������ state) ��������:
--   1) block_pos - ������� ������� � ����� ������
--   2) pos       - �������, � ������� ���������� ���� (��� ��� ���������� �����)
--   3) size      - ������ ����� (��� ��� ���������� �����)
-- ��������������, ������� �� ������������ ������ �� ������ buf ������ len, �� ������:
--   1) ���������� � ������ ������ ������, �������������� ���������������� ����� (���� ����)
--   2) �������� �� ����� ������, ����������� � ����� ����� (���� ����)
--   3) �������� ��������� - ������� � ����� ���������� �� ������ ����������� ������,
--        � ������� � ������ ���������� ������ ����� - �� ������ ���������� �� ����� ������
--   4) ���� ���� ���������� ��������� - ���� ��������� �� ���� ����������� �������
--        � �������� ��������� ������� �� ����������
--   5) ���� ��������� ��������������� ���� �������� � ������ ����� ��� � ��� ��������� �����
--        �������� ����� - ���� �������� ���������� ����� ����� � ���, ����� decompressBlock
--        ������� � ���������� ����, ��� ����� (�� ������ ��� ������ �� cfile)
--
decompressStep (cfile :: IORef FileToCompress) (state :: IORef (Integer, Integer, Integer)) pipe buf len = do
  (block_pos, pos, size) <- (val state :: IO (Integer, Integer, Integer))
  if block_pos<(0 :: Integer)   -- ������, ��� ����������� �� ������� ��������, ��� �� ����� ������� � ������� ����� ������
    then return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- ������, ��������, ���� �� �����������. ������������: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- ���������� ������ ���������� ������ � ������ ������
      data_start = buf +: skip_bytes             -- ������ ������, ������������� ���������������� �����
      data_size  = min size (i len-skip_bytes)   -- ���-�� ����, ������������� ���������������� �����
      block_end  = block_pos+i len               -- ������� � �����-�����, ��������������� ����� ����������� ������
  when (data_size>(0 :: Integer)) $ do    -- ���� � ������ ������� ������, ������������� ���������������� �����
    sendP pipe (data_start, i data_size)  -- �� ������� ��� ������ �� ������ ����� �����������
    receive_backP pipe                    -- �������� ������������� ����, ��� ������ ���� ������������
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- ���� ���� ��� �� ���������� ���������
    then return len     -- �� ���������� ���������� �����
    else do             -- ����� ��������� � ���������� ������� �� ����������
  sendP pipe (error "End of decompressed data", (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int))
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- ��� ��������� ��������, ��� ������ ������� ������ �� ����� ���������� �� ��������� � �� ������ ���� ��������
      state =: (aStopDecompressThread, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- ���� ����� ���� ��������� � ������ ����� ��� � ����, �� ������
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- ��� �� ������������� ����, ������ � -m0, � � ��� ���� ����������� ���������� ����� ������
        then do state =: (-1 :: Integer, error "undefined state.pos", error "undefined state.size")
                return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- ������� ����, ��� ����� ��������� ���������� ����� �����
        else do state =: (block_pos, pos, size)            -- ����� ���������� ���������� �����,
                decompressStep cfile state pipe buf len   -- ��� � ��������� ���������� ������ �����

-- |������, ��������� ���������� ������ ����� ����������
aStopDecompressThread = -99


-- |���������, ������������ ��� �������� ������ ���������� �������� ��������/����������
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

{-# NOINLINE resendData #-}
-- |��������� �������� �������� ������ ����������/������������ ��������� ��������� � �������
resendData pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- ���������� ���������� ����������� ����, ������������ �� ��������-�����������
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
