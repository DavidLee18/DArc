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
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Utils (copyBytes)
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
        -- MicroHs: buffer-to-buffer compression, bypassing callback-based pipeline.
        -- Collects all input DataChunks, then applies the method chain using compressMem.
        let mhs_compress_block = do
              (inBuf, (inSize :: Int)) <- mhsCollectFromPipe
              let tryCompSz p inB (inS :: Int) (outSz :: Int) = do
                    (outB :: Ptr CChar) <- mallocBytes (max outSz (1 :: Int))
                    r <- compressMem p inB inS outB outSz
                    if r == aFREEARC_ERRCODE_OUTBLOCK_TOO_SMALL
                      then do free outB; tryCompSz p inB inS (outSz * 2)
                      else return (outB, r)
                  goChain [] inB (inS :: Int) = return (inB, inS)
                  goChain (p:ps) inB (inS :: Int) = do
                    let outSz = max (inS + inS) (65536 :: Int)
                    (outB, (r :: Int)) <- tryCompSz p inB inS outSz
                    free inB
                    if r >= (aFREEARC_OK :: Int)
                      then goChain ps outB r
                      else do registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage r, p]
                              operationTerminated =: True
                              return (outB, r)
              (outBuf, (outSize :: Int)) <- goChain real_crypted_compressor inBuf inSize
              when (outSize >= (aFREEARC_OK :: Int)) $ do
                r <- write_to_archive (DataBuf outBuf outSize)
                writeIORef result r
              free outBuf
            -- Reads DataChunk messages from pipe, copying data and acking each buffer.
            mhsCollectFromPipe = mhsGo [] (0 :: Int)
            mhsGo chunks total = do
              x <- receiveP pipe
              display x
              update_crc x
              case x of
                DataChunk buf len -> do
                  (chunk :: Ptr CChar) <- mallocBytes (max len (1 :: Int))
                  copyBytes chunk buf len
                  send_backP pipe (buf, len)  -- return producer's buffer to the pool
                  mhsGo ((chunk, len) : chunks) (total + len)
                DataEnd -> mhsPack chunks total
                _ -> mhsGo chunks total
            mhsPack chunks total = do
              buf <- mallocBytes (max total (1 :: Int))
              mhsFill buf (0 :: Int) (reverse chunks)
              return (buf, total)
            mhsFill _ _ [] = return ()
            mhsFill buf off ((chunk, n) : rest) = do
              copyBytes (buf `plusPtr` off) chunk n
              free chunk
              mhsFill buf (off + n) rest
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
