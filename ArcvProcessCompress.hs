----------------------------------------------------------------------------------------------------
---- Process compressing archive data and service info, and writing packed data to the archive. ----
---- Called from ArcCreate.hs                                                                   ----
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
import Foreign.Marshal.Array (withArray, allocaArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peek, peekElemOff)
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

-- |Process that compresses archive data and service information and writes the packed data to the archive.
-- Also returns, via the backdoor, service information about the blocks created while writing the archive
compressAndWriteToArchiveProcess archive command backdoor pipe = do

  -- Procedure that displays the incoming data in the UI
  let display (FileStart fi)               =  uiStartFile      fi
      display (DataChunk buf len)          =  uiUnpackedBytes  (i len)
      display (CorrectTotals files bytes)  =  uiCorrectTotal   files bytes
      display (FakeFiles cfiles)           =  uiFakeFiles      (map cfFileInfo cfiles) 0
      display _                            =  return ()

  -- Procedure that writes the compressed data into the archive
  let write_to_archive (DataBuf buf len) =  do uiCompressedBytes  (i len)
                                               archiveWriteBuf    archive buf len
                                               return len
      write_to_archive  NoMoreData       =  return 0

  -- Procedure that copies a whole solid block from the input archive to the output one without recompressing
  let copy_block = do
        CopySolidBlock files <- receiveP pipe
        let block       = cfArcBlock (head files)
        uiFakeFiles       (map cfFileInfo files)  (blCompSize block)
        archiveCopyData   (blArchive block) (blPos block) (blCompSize block) archive
        DataEnd <- receiveP pipe
        return ()

  repeat_while (receiveP pipe) notTheEnd $ \case
    DebugLog str -> debugLog str   -- Print a debug message
    DebugLog0 str -> debugLog0 str
    CompressData block_type compressor real_compressor just_copy -> do
        case block_type of             -- Tell the UI what kind of data is about to be compressed
            DATA_BLOCK  ->  uiStartFiles (length real_compressor)
            DIR_BLOCK   ->  uiStartDirectory
            _           ->  uiStartControlData
        result <- ref 0   -- number of bytes written by the last call to write_to_archive

        -- Compute the CRC (for service blocks only) and the number of bytes in the block's uncompressed data
        crc      <- ref aINIT_CRC
        origsize <- ref 0
        let update_crc (DataChunk buf len) =  do when (block_type/=DATA_BLOCK) $ do
                                                     crc .<- updateCRC buf len
                                                 origsize += i len
            update_crc _                   =  return ()

        -- Determine whether this block needs encryption
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

        -- If this block must be encrypted, append the encryption algorithm
        -- to the chain of compression methods. The actually invoked encryption algorithm receives key and initVector,
        -- while the archive stores salt and checkCode, which is needed for a quick password check
        (add_real_encryption, add_encryption_info) <- if useEncryption
                                                         then generateEncryption algorithm password   -- not thread-safe due to use of PRNG!
                                                         else return (id,id)

        -- Bind `times` before let so compressa is not in the same mdo rec-group
        (times :: MVar (Integer, String, [(String, Double, Integer)])) <- uiStartDeCompression "compression"              -- create the structure that accounts for compression time

        -- Compression process for a single algorithm
        -- A sequence of compression processes matching the sequence of algorithms in `real_compressor`
        let real_crypted_compressor = add_real_encryption real_compressor
#ifdef __MHS__
        -- Per-file CRCs computed by C hot path, to patch into Directory entries
        fileCRCs <- newIORef ([] :: [CRC])
        -- MicroHs: C hot path for DATA_BLOCK with disk files.
        -- Reads files, compresses, and writes to archive entirely in C,
        -- bypassing ALL Haskell per-byte/per-chunk iteration overhead.
        let mhs_compress_block = do
              x <- receiveP pipe
              case x of
                CompressFiles paths fis -> do
                  -- Full C hot path: read+CRC+compress+write in one C call
                  uiUnpackedBytes (i (sum (map fiSize fis)))
                  let numFiles = length paths
                  cstrPaths <- mapM newCString paths
                  cstrMethods <- mapM newCString real_crypted_compressor
                  withArray cstrPaths $ \pathArr ->
                    withArray cstrMethods $ \methodArr ->
                    alloca $ \pCompSize ->
                    alloca $ \pOrigSize ->
                    alloca $ \pBlockCrc ->
                    alloca $ \pResult ->
                    alloca $ \pFailedIdx ->
                    allocaArray numFiles $ \crcArr -> do
                      withArchiveBFILE archive $ \bfile -> do
                        darc_compress_solid_block_w
                          pathArr (fromIntegral numFiles)
                          bfile
                          methodArr (fromIntegral (length real_crypted_compressor))
                          pCompSize crcArr pOrigSize pBlockCrc pResult pFailedIdx
                      rc <- peek pResult
                      if rc < (0 :: CInt)
                        then do failedIdx <- peek pFailedIdx
                                let idx = fromIntegral (failedIdx :: CInt)
                                if idx >= 0 && idx < numFiles
                                  then registerThreadError$ CANT_OPEN_FILE (paths !! idx)
                                  else registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage (fromIntegral rc), head real_crypted_compressor]
                                operationTerminated =: True
                        else do compSize <- peek pCompSize
                                origSize' <- peek pOrigSize
                                blockCrc <- peek pBlockCrc
                                writeIORef result (fromIntegral (compSize :: CLong))
                                origsize =: fromIntegral (origSize' :: CLong)
                                uiCompressedBytes (fromIntegral (compSize :: CLong))
                                when (block_type/=DATA_BLOCK) $ do
                                  crc =: fromIntegral (blockCrc :: CUInt)
                                -- Read per-file CRCs from C array
                                crcs <- mapM (\j -> peekElemOff crcArr j) [0..numFiles-1]
                                writeIORef fileCRCs crcs
                  mapM_ free cstrPaths
                  mapM_ free cstrMethods
                  -- Drain DataEnd from pipe
                  DataEnd <- receiveP pipe
                  return ()
                _ -> do
                  -- Fallback: old pipeline path for non-CompressFiles instructions
                  display x
                  update_crc x
                  darc_pipeline_init (64 * 1024 * 1024)
                  let collectFirst = case x of
                        DataChunk buf len -> do
                          darc_pipeline_append (castPtr buf) (fromIntegral len)
                          send_backP pipe (buf, len)
                        _ -> return ()
                  collectFirst
                  let collectLoop = do
                        y <- receiveP pipe
                        display y
                        update_crc y
                        case y of
                          DataChunk buf len -> do
                            darc_pipeline_append (castPtr buf) (fromIntegral len)
                            send_backP pipe (buf, len)
                            collectLoop
                          DataEnd -> return ()
                          _ -> collectLoop
                  collectLoop
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
        -- Compression procedure that runs the compression process with all the procedures needed to receive/send data
        let compress_block  =  runFuncP compressa (do x<-receiveP pipe; display x; update_crc x; return x)
                                                  (send_backP pipe)
                                                  (write_to_archive .>>= writeIORef result)
                                                  (val result)
        -- Choose between the compression procedure and copying the whole solid block from the input archive
        let compress_f  =  if just_copy  then copy_block  else compress_block
#endif

        -- Compress one solid block
        pos_begin <- archiveGetPos archive
        compress_f                                             -- compress the data
        ; uiFinishDeCompression times `on` block_type==DATA_BLOCK  -- account the net operation time in the UI
        ; uiUpdateProgressIndicator 0                              -- mark the data that was read as already processed
        pos_end   <- archiveGetPos archive

        -- Return to the first process the information about the block just created
        -- together with the list of files it contains
        (Directory dir0)  <-  receiveP pipe   -- Get the list of files in the block from the first process
#ifdef __MHS__
        -- Patch per-file CRCs from C hot path into directory entries
        crcs <- readIORef fileCRCs
        let dir = if null crcs then dir0
                  else patchDirCRCs dir0 crcs
            patchDirCRCs [] _          = []
            patchDirCRCs fws []        = fws
            patchDirCRCs (fw:fws) ccs@(c:cs)
              | fiIsDir (fwFileInfo fw)  = fw : patchDirCRCs fws ccs
              | otherwise                = fw{fwCRC = c} : patchDirCRCs fws cs
#else
        let dir = dir0
#endif
        crc'             <-  val crc >>== finishCRC     -- Compute the final CRC value
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
-- |Helper process that re-encodes a stream of Instruction into a stream of CompressionData
storingProcess pipe = do
  let send (DataChunk buf len)  =  do failOnTerminated
                                      resend_data pipe (DataBuf buf len)
                                      send_backP pipe (buf,len)
      send  DataEnd             =  void (resend_data pipe NoMoreData)
      send x                   =  return ()

  -- When done, tell the next process that there is no more data
  ensureCtrlBreak "send DataEnd" (send DataEnd)$ do
    -- Instruction re-encoding loop
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
-- Full solid-block C hot path
foreign import ccall "darc_compress_solid_block_w"     darc_compress_solid_block_w ::
    Ptr CString -> CInt -> Ptr () ->
    Ptr CString -> CInt ->
    Ptr CLong -> Ptr CUInt -> Ptr CLong -> Ptr CUInt ->
    Ptr CInt -> Ptr CInt -> IO ()
#endif
