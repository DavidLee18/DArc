----------------------------------------------------------------------------------------------------
---- Процесс упаковки данных и служебной информации архива, и записи упакованных данных в архив.----
---- Вызывается из ArcCreate.hs                                                                 ----
----------------------------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module ArcvProcessCompress where

import Prelude hiding (catch)
import Control.Concurrent (MVar, Chan)
import Control.Monad
import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc (mallocBytes, free, alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek)
import CompressionLib (compressMem, aFREEARC_OK, compressionErrorMessage)

import Utils
import Files
import Errors
import Process
import FileInfo
import Compression
import Encryption
import Options           (opt_data_password, opt_headers_password, opt_encryption_algorithm)
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessExtract
import ArcvProcessRead

-- |Процесс упаковки данных и служебной информации архива, и записи упакованных данных в архив.
-- Также возвращает через backdoor служебную информацию о блоках, созданных при записи архива
compressAndWriteToArchiveProcess archive command backdoor pipe = do

  -- Процедура отображения в UI входных данных
  let display (FileStart fi)               =  uiStartFile      fi
      display (DataChunk buf len)          =  uiUnpackedBytes  (i len)
      display (CorrectTotals files bytes)  =  uiCorrectTotal   files bytes
      display (FakeFiles cfiles)           =  uiFakeFiles      (map cfFileInfo cfiles) 0
      display _                            =  return ()

  -- Процедура записи упакованных данных в архив
  let write_to_archive (DataBuf buf len) =  do uiCompressedBytes  (i len)
                                               archiveWriteBuf    archive buf len
                                               return len
      write_to_archive  NoMoreData       =  return 0

  -- Процедура копирования целиком солид-блока из входного архива в выходной без переупаковки
  let copy_block = do
        CopySolidBlock files <- receiveP pipe
        let block       = cfArcBlock (head files)
        uiFakeFiles       (map cfFileInfo files)  (blCompSize block)
        archiveCopyData   (blArchive block) (blPos block) (blCompSize block) archive
        DataEnd <- receiveP pipe
        return ()

  repeat_while (receiveP pipe) notTheEnd $ \case
    DebugLog str -> debugLog str   -- Напечатать отладочное сообщение
    DebugLog0 str -> debugLog0 str
    CompressData block_type compressor real_compressor just_copy -> do
        case block_type of             -- Сообщим UI какого типа данные сейчас будут паковаться
            DATA_BLOCK  ->  uiStartFiles (length real_compressor)
            DIR_BLOCK   ->  uiStartDirectory
            _           ->  uiStartControlData
        result <- ref 0   -- количество байт, записанных в последнем вызове write_to_archive

        -- Подсчёт CRC (только для служебных блоков) и количества байт в неупакованных данных блока
        crc      <- ref aINIT_CRC
        origsize <- ref 0
        let update_crc (DataChunk buf len) =  do when (block_type/=DATA_BLOCK) $ do
                                                     crc .<- updateCRC buf len
                                                 origsize += i len
            update_crc _                   =  return ()

        -- Выясним, нужно ли шифрование для этого блока
        let useEncryption = password>""
            password = case block_type of
                         DATA_BLOCK     -> opt_data_password command
                         DIR_BLOCK      -> opt_headers_password command
                         FOOTER_BLOCK   -> opt_headers_password command
                         DESCR_BLOCK    -> ""
                         HEADER_BLOCK   -> ""
                         RECOVERY_BLOCK -> ""
                         _              -> error$ "Unexpected block type "++show (fromEnum block_type)++" in compressAndWriteToArchiveProcess"
            algorithm = command.$ opt_encryption_algorithm

        -- Если для этого блока нужно использовать шифрование, то добавить алгоритм шифрования
        -- к цепочке методов сжатия. В реально вызываемый алгоритм шифрования передаётся key и initVector,
        -- а в архиве запоминаются salt и checkCode, необходимый для быстрой проверки пароля
        (add_real_encryption, add_encryption_info) <- if useEncryption
                                                         then generateEncryption algorithm password   -- not thread-safe due to use of PRNG!
                                                         else return (id,id)

        -- Bind `times` before let so compressa is not in the same mdo rec-group
        (times :: MVar (Integer, String, [(String, Double, Int)])) <- uiStartDeCompression "compression"              -- создать структуру для учёта времени упаковки

        -- Процесс упаковки одним алгоритмом
        -- Последовательность процессов упаковки, соответствующая последовательности алгоритмов `real_compressor`
        let real_crypted_compressor = add_real_encryption real_compressor
#ifdef __MHS__
        -- MicroHs: C-side pipeline compression.
        -- Accumulates data in a C growing buffer (fast memcpy), then compresses
        -- the entire chain using streaming Compress() in C — no Haskell iteration
        -- for the actual compression, avoiding MHS combinator reduction overhead.
        let mhs_compress_block = do
              -- Phase 1: Collect data into C-managed buffer
              darc_pipeline_init (64 * 1024 * 1024)
              let collectLoop = do
                    x <- receiveP pipe
                    display x
                    update_crc x
                    case x of
                      DataChunk buf len -> do
                        darc_pipeline_append (castPtr buf) (fromIntegral len)
                        send_backP pipe (buf, len)
                        collectLoop
                      DataEnd -> return ()
                      _ -> collectLoop
              collectLoop
              -- Phase 2: Compress through method chain entirely in C
              let compressLoop [] = return True
                  compressLoop (m:ms) = do
                    r <- withCString m $ \cm -> alloca $ \pResult -> do
                           darc_pipeline_compress_step_w cm pResult
                           peek pResult
                    if r >= (0 :: CLong)
                      then compressLoop ms
                      else do registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage (fromIntegral r), m]
                              operationTerminated =: True
                              darc_pipeline_free
                              return False
              ok <- compressLoop real_crypted_compressor
              -- Phase 3: Write result to archive
              when ok $ alloca $ \pBuf -> alloca $ \pSize -> do
                darc_pipeline_get_buf_w pBuf pSize
                outBuf <- peek pBuf
                outSize <- fmap fromIntegral (peek pSize :: IO CLong)
                when (outSize > (0 :: Int)) $ do
                  r <- write_to_archive (DataBuf (castPtr outBuf) outSize)
                  writeIORef result r
                free outBuf
        let compress_f = if just_copy then copy_block else mhs_compress_block
#else
        let compressa :: Pipe (PairFunc Instruction) (PairFunc (Ptr CChar, Int)) (PairFunc CompressionData) (PairFunc Int) -> IO ()
            compressa = case real_crypted_compressor of
                          [m]  -> storingProcess |> de_compress_PROCESS freearcCompress times m 1
                          ms   -> storingProcess
                                   |> foldl1 (|>) [ de_compress_PROCESS freearcCompress times m n
                                                  | (m, n) <- zip (init ms) [1..] ]
                                   |> de_compress_PROCESS freearcCompress times (last ms) (length ms)
        -- Процедура упаковки, вызывающая процесс упаковки со всеми необходимыми процедурами для получения/отправки данных
        let compress_block  =  runFuncP compressa (do x<-receiveP pipe; display x; update_crc x; return x)
                                                  (send_backP pipe)
                                                  (write_to_archive .>>= writeIORef result)
                                                  (val result)
        -- Выбрать между процедурой упаковки и процедурой копирования целиком солид-блока из входного архива
        let compress_f  =  if just_copy  then copy_block  else compress_block
#endif

        -- Упаковать один солид-блок
        pos_begin <- archiveGetPos archive
        compress_f                                             -- упаковать данные
        ; uiFinishDeCompression times `on` block_type==DATA_BLOCK  -- учесть в UI чистое время операции
        ; uiUpdateProgressIndicator 0                              -- отметить, что прочитанные данные уже обработаны
        pos_end   <- archiveGetPos archive

        -- Возвратить в первый процесс информацию о только что созданном блоке
        -- вместе со списком содержащихся в нём файлов
        (Directory dir)  <-  receiveP pipe   -- Получим от первого процесса список файлов в блоке
        crc'             <-  val crc >>== finishCRC     -- Вычислим окончательное значение CRC
        origsize'        <-  val origsize
        putP backdoor (ArchiveBlock {
                           blArchive     = archive
                         , blType        = block_type
                         , blCompressor  = compressor .$(not just_copy &&& add_encryption_info) .$compressionDeleteTempCompressors
                         , blPos         = pos_begin
                         , blOrigSize    = origsize'
                         , blCompSize    = pos_end-pos_begin
                         , blCRC         = crc'
                         , blFiles       = error "undefined ArchiveBlock::blFiles"
                         , blIsEncrypted = error "undefined ArchiveBlock::blIsEncrypted"
                       }, dir)


{-# NOINLINE storingProcess #-}
-- |Вспомогательный процесс, перекодирующий поток Instruction в поток CompressionData
storingProcess pipe = do
  let send (DataChunk buf len)  =  do failOnTerminated
                                      resend_data pipe (DataBuf buf len)
                                      send_backP pipe (buf,len)
      send  DataEnd             =  void (resend_data pipe NoMoreData)
      send x                   =  return ()

  -- По окончании сообщим следующему процессу, что данных больше нет
  ensureCtrlBreak "send DataEnd" (send DataEnd)$ do
    -- Цикл перекодирования инструкций
    repeat_while (receiveP pipe) notDataEnd send


-- Compatibility alias
compress_AND_write_to_archive_PROCESS = compressAndWriteToArchiveProcess

#ifdef __MHS__
-- C-side pipeline FFI (Environment.cpp)
foreign import ccall "darc_pipeline_init"              darc_pipeline_init :: CLong -> IO ()
foreign import ccall "darc_pipeline_append"            darc_pipeline_append :: Ptr () -> CLong -> IO ()
foreign import ccall "darc_pipeline_compress_step_w"   darc_pipeline_compress_step_w :: CString -> Ptr CLong -> IO ()
foreign import ccall "darc_pipeline_get_buf_w"         darc_pipeline_get_buf_w :: Ptr (Ptr ()) -> Ptr CLong -> IO ()
foreign import ccall "darc_pipeline_free"              darc_pipeline_free :: IO ()
#endif
