----------------------------------------------------------------------------------------------------
---- Процесс распаковки входных архивов.                                                        ----
---- Вызывается из ArcExtract.hs и ArcCreate.hs (при обновлении и слиянии архивов).             ----
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
-- |Распаковка файла из архива с использованием переданного процесса декомпрессора
-- и записью распакованных данных с помощью функции `writer`
decompressFile decompress_pipe compressed_file writer = do
  -- Не пытаться распаковать каталоги/пустые файлы и файлы без данных, поскольку ожидать получение 0 байтов - воистину дзенское занятие ;)
  when (fiSize (cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_while (receiveP decompress_pipe) ((>=0) . snd) (uncurry writer)
    failOnTerminated

{-# NOINLINE decompressProcess #-}
-- |Процесс, распаковывающий файлы из архивов
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
-- |Распаковать один солид-блок
decompressBlock command cfile state count_cbytes pipe = mdo
  cfile' <- (val cfile :: IO FileToCompress)
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
      compressor  =  blCompressor block .$ limitDecompressionMemoryUsage (opt_limit_decompression_memory command)
      startPos  | compressor==aNO_COMPRESSION  =  pos  -- для -m0 начинаем чтение напрямую с нужной позиции в блоке
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

  -- Добавить ключ в запись алгоритма дешифрования
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (cmd_arcname command) (cfile'.$cfFileInfo.$storedName)

  -- Создадим конвейер декомпрессии: последний метод цепочки читает первым, первый декодирует последним
  -- Bind `times` before let so decompressa is not in the same mdo rec-group,
  -- allowing GHC to generalise the Pipe element type over PipeElement.
  (times :: MVar (Integer, String, [(String, Double, Integer)])) <- uiStartDeCompression "decompression"  -- Добавить ключ в запись алгоритма дешифрования
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

  result <- ref (0 :: Int)   -- количество байт, записанных в последнем вызове writer
  runFuncP (decompressa (map fromJust keyed_compressor)) (fail "decompressBlock::runFuncP" :: IO CompressionData) doNothing ((writer :: CompressionData -> IO Int) .>>= writeIORef result) (val result)
#endif
  uiFinishDeCompression times                    -- учесть в UI чистое время операции


{-# NOINLINE deCompressProcess #-}
-- |Распаковка файла из архива с использованием переданного процесса декомпрессора
-- Превратим список методов сжатия/шифрования в конвейер процессов распаковки
--   comprMethod - строка метода сжатия с параметрами, типа "ppmd:o10:m48m"
--   num - номер процесса в цепочке процессов упаковки
deCompressProcess de_compress times comprMethod num pipe = do
  -- Информация об остатке данных, полученных из предыдущего процесса, но ещё не отправленных на упаковку/распаковку
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", (0 :: Int))
  let
    -- Скопировать данные из srcbuf в dstbuf и возвратить размер скопированных данных
    copyData (prevlen :: Int) dstbuf (dstlen :: Int) buf0 srcbuf (srclen :: Int) = do
      let len = srclen `min` dstlen    -- определить - сколько данных мы можем прочитать
      copyBytes dstbuf srcbuf len
      uiReadData num (i len)
      remains =: Just (buf0, srcbuf+:len, srclen-len)
      case () of
       _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- возвратить размер буфера, поскольку все данные из него уже переданы упаковщику/распаковщику
                             read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- прочитать следующую инструкцию
         | len==dstlen -> return (prevlen+len)                                   -- буфер достаточно заполнен
         | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)    -- заполним остаток буфера содержимым следующих файлов

    -- Добавить ключ в запись алгоритма дешифрования
    processNextInstruction (prevlen :: Int) (dstbuf :: Ptr CChar) (dstlen :: Int) = do
      instr <- receiveP pipe
      case instr of
        DataBuf srcbuf srclen  ->  copyData prevlen dstbuf dstlen srcbuf srcbuf srclen
        NoMoreData             ->  do remains =: Nothing;  return prevlen

    -- Процедура "чтения" входных данных. Важно, чтобы первый вызов с dstlen=0 не возвращал управление пока не поступит хотя бы один байт данных от предыдущего процесса
    read_data (prevlen :: Int)  -- сколько данных уже прочитано
              (dstbuf :: Ptr CChar)   -- буфер, куда нужно поместить входные данные
              (dstlen :: Int)   -- Добавить ключ в запись алгоритма дешифрования
              = do     -- -> процедура должна возвратить количество прочитанных байт или 0, если данные закончились
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                                       -- Если ещё есть данные, полученные из предыдущего процесса
         | srclen>(0 :: Int)  ->  copyData prevlen dstbuf dstlen buf0 srcbuf srclen --  то передать их упаковщику/распаковщику
         | otherwise ->  processNextInstruction prevlen dstbuf dstlen      --  иначе получить новые
        Nothing      ->  return prevlen                                    -- Этот solid-блок закончился, данных больше нет

  -- Процедура чтения входных данных процесса упаковки/распаковки (вызывается лишь однажды, в отличие от рекурсивной read_data)
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
-- |de_compress_PROCESS с параметризуемой функцией чтения (может читать данные напрямую
-- из архива для первого процесса в цепочке распаковки)
deCompressProcess1 de_compress reader times comprMethod num pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let -- Процедура чтения входных данных процесса упаковки/распаковки
      callback "read" buf size = reader buf size
      -- Добавить ключ в запись алгоритма дешифрования
      callback "write" buf size = do total' += i size
                                     uiWriteData num (i size)
                                     resendData pipe (DataBuf buf size)
      -- "Квазизапись" просто сигнализирует сколько данных будет записано в результате сжатия
      -- уже прочитанных данных. Значение передаётся через int64* ptr
      callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return (aFREEARC_OK :: Int)
      -- Превратим список методов сжатия/шифрования в конвейер процессов распаковки
      callback "time" ptr (0 :: Int) = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                          time' =: t
                                          return (aFREEARC_OK :: Int)
      -- Прочие (неподдерживаемые) callbacks
      callback _ _ _ = return (aFREEARC_ERRCODE_NOT_IMPLEMENTED :: Int)

  -- Добавить ключ в запись алгоритма дешифрования
  result <- de_compress num comprMethod callback
  -- Добавить ключ в запись алгоритма дешифрования
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  -- Выйдем с сообщением, если произошла ошибка
  unlessM (val operationTerminated) $ do
    unless (result `elem` [aFREEARC_OK, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED]) $ do
      registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage result, comprMethod]
      operationTerminated =: True
  -- Сообщим предыдущему процессу, что данные больше не нужны, а следующему - что данных больше нет
  send_backP  pipe (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)
  resendData pipe NoMoreData
  return ()


-- |Обработка очередной порции распакованных данных (writer для распаковщика).
-- Состояние (хранимое по ссылке state) содержит:
--   1) block_pos - текущую позицию в блоке данных
--   2) pos       - позицию, с которой начинается файл (или его оставшаяся часть)
--   3) size      - размер файла (или его оставшейся части)
-- Соответственно, получив от распаковщика данные по адресу buf длиной len, мы должны:
--   1) пропустить в начале буфера данные, предшествующие распаковываемому файлу (если есть)
--   2) передать на выход данные, относящиеся к этому файлу (если есть)
--   3) обновить состояние - позиция в блоке изменилась на размер полученного буфера,
--        а позиция и размер оставшихся данных файла - на размер переданных на выход данных
--   4) если файл распакован полностью - надо известить об этом принимающую сторону
-- Добавить ключ в запись алгоритма дешифрования
--   5) если следующий распаковываемый файл оказался в другом блоке или в уже прошедшей части
--        текущего блока - надо прервать распаковку этого блока с тем, чтобы decompress_block
--        перешёл к распаковке того, что нужно (он читает эти данные из cfile)
--
decompressStep (cfile :: IORef FileToCompress) (state :: IORef (Integer, Integer, Integer)) pipe buf len = do
  (block_pos, pos, size) <- (val state :: IO (Integer, Integer, Integer))
  if block_pos<(0 :: Integer)   -- похоже, что распаковщик не обратил внимание, что мы хотим перейти к другому блоку данных
    then return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- ничего, потерпим, пока он образумится. альтернатива: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- пропустить данные предыдущих файлов в начале буфера
      data_start = buf +: skip_bytes             -- начало данных, принадлежащих распаковываемому файлу
      data_size  = min size (i len-skip_bytes)   -- кол-во байт, принадлежащих распаковываемому файлу
      block_end  = block_pos+i len               -- позиция в солид-блоке, соответствующая концу полученного буфера
  when (data_size>(0 :: Integer)) $ do    -- если в буфере нашлись данные, принадлежащие распаковываемому файлу
    sendP pipe (data_start, i data_size)  -- то выслать эти данные по каналу связи потребителю
    receive_backP pipe                    -- получить подтверждение того, что данные были использованы
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- если файл ещё не распакован полностью
    then return len     -- то продолжаем распаковку блока
    else do             -- иначе переходим к следующему заданию на распаковку
  sendP pipe (error "End of decompressed data", (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int))
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- Это сообщение означает, что больше никаких файлов от треда распаковки не требуется и он должен быть завершён
      state =: (aStopDecompressThread, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- если новый файл находится в другом блоке или в этом, но раньше
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- или мы распаковываем блок, сжатый с -m0, и у нас есть возможность пропустить часть файлов
        then do state =: (-1 :: Integer, error "undefined state.pos", error "undefined state.size")
                return (aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED :: Int)   -- признак того, что нужно завершить распаковку этого блока
        else do state =: (block_pos, pos, size)            -- снова рассмотрим переданный буфер,
                decompressStep cfile state pipe buf len   -- уже в контексте распаковки нового файла

-- |Процесс, распаковывающий файлы из архивов
aStopDecompressThread = -99


-- |Структура, используемая для передачи данных следующему процессу упаковки/распаковки
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

{-# NOINLINE resendData #-}
-- |Процедура передачи выходных данных упаковщика/распаковщика следующей процедуре в цепочке
resendData pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- возвратить количество потреблённых байт, возвращаемое из процесса-потребителя
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
