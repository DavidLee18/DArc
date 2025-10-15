----------------------------------------------------------------------------------------------------
---- Process that builds the archive structure and reads the data to be packed.                    ----
---- Called from ArcCreate.hs                                                                    ----
----------------------------------------------------------------------------------------------------
module ArcvProcessRead where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils

import Utils
import Files
import Process
import Errors
import FileInfo
import Compression
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcvProcessExtract


-- |Instructions sent by the input-reading process to the packing process
data Instruction
  =   DebugLog  String                        --   Output a debug message with a timestamp
  |   DebugLog0 String                        --   Output a debug message without timestamp
  |   CompressData BlockType Compressor Compressor Bool
                                              --   Start of an archive block
  |   FileStart FileInfo                      --   Start of the next file
  |   DataChunk (Ptr CChar) Int               --   Next chunk of data to be packed
  |   CorrectTotals FileCount FileSize        --   Adjust Total Files/Bytes shown in the UI
  |   FakeFiles [FileToCompress]              --   Adjust the list of already-packed files for the UI
  |   CopySolidBlock [CompressedFile]         --   Copy an entire solid block from an existing archive
  |   DataEnd                                 --   End of the archive block
  |   Directory [FileWithCRC]                 --   Request for administrative data about the last created archive block
  |   TheEnd                                  --   Archive creation complete

-- |Predicates used in loops: run until end of archive block, run until end of archive
notDataEnd DataEnd = False
notDataEnd _       = True
notTheEnd  TheEnd  = False
notTheEnd  _       = True


-- |Process that builds the archive structure - it splits files
--   into separate archive volumes, directory blocks inside the archive and solid blocks.
-- This same process also gathers input data for packing - it reads files from disk and
--   decompresses data from input archives.
createArchiveAtructureAndReadFilesProcess command archive oldarc files processDir arcComment writeRecoveryBlocks results backdoor pipe = do
  initPos <- archiveGetPos archive
  -- On error, set a flag to interrupt c_compress() operation
  handleCtrlBreak "operationTerminated =: True" (operationTerminated =: True) $ do
  -- Create a process for decompressing files from input archives and ensure it terminates correctly
  bracket (runAsyncP$ decompress_PROCESS command doNothing)
          ( \decompress_pipe -> do sendP decompress_pipe Nothing; joinP decompress_pipe)
          $ \decompress_pipe -> do
  -- Create a cache for lookahead reading of files to be archived
  withPool $ \pool -> do
  bufOps <- makeFileCache (opt_cache command) pool pipe
  -- Parameters for writeControlBlock
  let params = (command,bufOps,pipe,backdoor)

  -- Write the header block (HEADER_BLOCK) at the start of the archive
  header_block  <-  writeControlBlock HEADER_BLOCK aNO_COMPRESSION params $ do
                      archiveWriteHeaderBlock bufOps


                              (createDirBlock archive processDir decompress_pipe params)

  -- Write the final block (FOOTER_BLOCK) containing the index of control blocks and the archive comment
  let write_footer_block blocks arcRecovery = do
          footerPos <- archiveGetPos archive
          writeControlBlock FOOTER_BLOCK (dirCompressor command) params $ do
            let lock_archive = opt_lock_archive command   -- Should the created archive be locked from modifications?
            archiveWriteFooterBlock blocks lock_archive arcComment arcRecovery footerPos bufOps
          return ()
  write_footer_block (header_block:directory_blocks) ""

  -- Print command execution statistics and save them for returning to the caller
  uiDoneArchive  >>=  writeIORef results

  -- If writing RECOVERY information is enabled - write RECOVERY blocks and repeat the FOOTER block
  (recovery_blocks,recovery) <- writeRecoveryBlocks archive oldarc initPos command params bufOps
  unless (null recovery_blocks) $ do
    write_footer_block (header_block:directory_blocks++recovery_blocks) recovery

  -- Notify the archive writer process that archive creation is complete
  sendP pipe TheEnd


-- |Записать в архив переданные файлы и dir-блок с их описанием
createDirBlock archive processDir decompress_pipe params@(command,bufOps,pipe,backdoor) files = do
  -- Split files into solid blocks and process each sublist separately. For debugging: mapM (print . map (fpFullname . fiDiskName . cfFileInfo)) (splitToSolidBlocks files)
  solidBlocks <- foreach (splitToSolidBlocks command files)
                         (createSolidBlock command processDir bufOps pipe decompress_pipe)
  -- Get from the write_to_archive process information about created solid blocks and the files they contain.
  -- Executing this forces completion of packing of all previously sent data and writing the packed data to the archive...
  blocks_info  <-  replicateM (length solidBlocks) (getP backdoor)
  -- ... after which we can be sure that the current position in the archive is where the directory block will start
  dirPos <- archiveGetPos archive
  -- Записать блок каталога и возвратить информацию о нём для формирования каталога каталогов
  writeControlBlock DIR_BLOCK (dirCompressor command) params $ do
    archiveWriteDir blocks_info dirPos bufOps


-- |Create a solid block containing data from the provided files
createSolidBlock command processDir bufOps pipe decompress_pipe (orig_compressor,files) = do
  let -- Choose the compression algorithm for this solid block
      -- and shrink dictionaries of its algorithms if they are larger than the data size of the block
      -- (+1% + 512 because delta-type filters may increase data size):
      compressor | copy_solid_block = cfCompressor (head files)
                 | otherwise        = orig_compressor.$limitDictionary (clipToMaxMemSize$ roundMemUp$ totalBytes+(totalBytes `div` 100)+512)
      -- Total size of files in the solid block
      totalBytes = sum$ map (fiSize.cfFileInfo) files
      -- True if this is a whole solid block from an input archive that can be copied without changes
      copy_solid_block = not (opt_recompress command)  &&  isWholeSolidBlock files
  -- Ограничить компрессор объёмом свободной памяти и значением -lc
  real_compressor <- limit_compressor command compressor
  opt_testMalloc command &&& testMalloc

  -- Compress the solid block and send the list of files placed into it to the next process
  unless (null files) $ do
    printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor
    writeBlock pipe DATA_BLOCK compressor real_compressor copy_solid_block $ do
      dir <- -- If the solid block is being transferred whole from archive to archive, skip unnecessary recompression
             if copy_solid_block then do
               sendP pipe (CopySolidBlock files)
               return$ map fileWithCRC files
             -- If --nodata is used, skip reading input files
             else if isReallyFakeCompressor compressor then do
               sendP pipe (FakeFiles files)
               return$ map fileWithCRC files
             -- Normal file reading for all other (more typical) cases
             else do
               mapMaybeM (readFile command pipe bufOps decompress_pipe) files
      processDir dir   -- let the procedure passed from above inspect the list of archived files (used to implement options -tl, -ac, -d[f])
      return dir


-- |Print debug information
printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor = do
  --print (clipToMaxInt totalBytes, compressor)
  --print$ map (diskName.cfFileInfo) files   -- debugging tool :)
  when (opt_debug command) $ do
    sendP pipe$ DebugLog$  "Compressing "++show_files3 (length files)++" of "++show_bytes3 totalBytes
    sendP pipe$ DebugLog0$ if copy_solid_block then "  Copying "++join_compressor compressor  else "  Using "++join_compressor real_compressor
  unless copy_solid_block $ do
      sendP pipe$ DebugLog0$ "  Memory for compression "++showMem (getCompressionMem   real_compressor)
                                    ++", decompression "++showMem (getDecompressionMem real_compressor)


---------------------------------------------------------------------------------------------------
---- Procedure to read data of the file being packed -------------------------------------------------
---------------------------------------------------------------------------------------------------

{-# NOINLINE readFile #-}
-- If this is a directory, skip reading data
readFile command pipe _ _ file  | fi<-cfFileInfo file, fiIsDir fi = do
  sendP pipe (FileStart fi)
  return$ Just$ fileWithCRC file

-- If this is a file on disk, read it in parts and send read chunks for packing
readFile _ pipe (receiveBuf, sendBuf) _ (DiskFile old_fi) = do
  -- Operation to inform the next process about change in file count/size that it should send to the UI
  let correctTotals files bytes  =  when (files/=0 || bytes/=0) (sendP pipe (CorrectTotals files bytes)) >> return Nothing
  -- Check possibility to open the file - it may be locked or the file might have been deleted in the meantime :)
  mfile <- tryOpen (diskName old_fi)
  case mfile of
    Nothing -> correctTotals (-1) (-fiSize old_fi)
    Just file -> ensureCtrlBreak "fileClose:readFile" (fileClose file) $ do  -- Ensure file is closed
      -- Re-read file information in case it changed
      mfi <- rereadFileInfo old_fi file
      case mfi of
        Nothing -> correctTotals (-1) (-fiSize old_fi)
        Just fi -> do
          correctTotals 0 (fiSize fi - fiSize old_fi) -- Adjust UI totals if the file size has changed
          sendP pipe (FileStart fi)                   -- Inform the user about the start of packing the file
          let readFile crc bytes = do                 -- Read the file in a loop, sending read chunks for packing:
                (buf, size) <- receiveBuf                 -- Get a free buffer from the buffer queue
                len         <- fileReadBuf file buf size  -- Read the next portion of data from file into it
                newcrc      <- updateCRC buf len crc      -- Update CRC with buffer contents
                sendBuf        buf size len               -- Send the data to the packing process
                if len>0
                  then readFile newcrc $! bytes+i len    -- Update the count of read bytes
                  else return (finishCRC newcrc, bytes)  -- Exit the loop if the file ended
          (crc,bytesRead) <- readFile aINIT_CRC 0     -- Read the file, obtaining its CRC and size
          correctTotals 0 (bytesRead - fiSize fi)     -- Adjust UI totals if the actual read size differs from getFileInfo
          return$ Just$ FileWithCRC crc FILE_ON_DISK fi{fiSize=bytesRead}

-- If this is a file from an existing archive, decompress it and send decompressed chunks for packing
readFile _ pipe (receiveBuf, sendBuf) decompress_pipe compressed_file = do
  crc  <-  ref aINIT_CRC                       -- Initialize CRC value
  -- The operation of "writing" decompressed data by copying them into our own buffers
  -- and sending these buffers for further processing
  let writer inbuf 0 = send_backP decompress_pipe ()  -- сообщим распаковщику, что теперь буфер свободен
      writer inbuf insize = do
        (buf, size) <- receiveBuf              -- get a free buffer from the buffer queue
        let len  = min insize size             -- determine how many bytes we can process
        crc    .<- updateCRC inbuf len         -- update CRC with buffer contents
        copyBytes  buf inbuf len               -- copy data into the acquired buffer
        sendBuf    buf size len                -- send them to the next process in the pipeline
        writer     (inbuf+:len) (insize-len)   -- process remaining data, if any
  let fi  =  cfFileInfo compressed_file
  sendP pipe (FileStart fi)                    -- Inform the user about beginning re-packing of the file
  decompress_file decompress_pipe compressed_file writer   -- Decompress the file in a separate thread
  crc'  <-  val crc >>== finishCRC            -- Compute the final CRC value
  if cfCRC compressed_file == crc'            -- If CRC matches
    then return$ Just$ fileWithCRC compressed_file  -- then return file info
    else registerError$ BAD_CRC$ diskName fi        -- otherwise register an error


---------------------------------------------------------------------------------------------------
---- Auxiliary definitions ------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- |Create a cache for lookahead reading and return procedures receiveBuf and sendBuf
-- to get a free buffer from the cache and to release used buffers respectively
makeFileCache cache_size pool pipe = do
  -- Size of buffers the whole cache will be split into
  let bufsize | cache_size>=aLARGE_BUFFER_SIZE*16  =  aLARGE_BUFFER_SIZE
              | otherwise                          =  aBUFFER_SIZE
  -- Allocate memory for the cache and start memoryAllocator on the allocated block
  heap                     <-  pooledMallocBytes pool cache_size
  (getBlock, shrinkBlock)  <-  memoryAllocator   heap cache_size bufsize 256 (receive_backP pipe)
  let -- Операция получения свободного буфера
      receiveBuf            =  do buf <- getBlock
                                  failOnTerminated
                                  return (buf, bufsize)
      -- Operation to obtain a free buffer
      -- Operation to send a filled buffer to the next process
      sendBuf buf size len  =  do shrinkBlock buf len
                                  failOnTerminated
                                  when (len>0)$  do sendP pipe (DataChunk buf len)
  return (receiveBuf, sendBuf)

{-# NOINLINE writeBlock #-}
-- |Записать в архив блок данных/служебный/дескриптор блока
writeBlock pipe blockType compressor real_compressor just_copy action = do
  sendP pipe (CompressData blockType compressor real_compressor just_copy)
  directory <- action
  sendP pipe  DataEnd
  sendP pipe (Directory directory)

{-# NOINLINE writeControlBlock #-}
-- Записать в архив служебный блок вместе с его дескриптором и возвратить информацию об этом блоке
writeControlBlock blockType compressor (command,bufOps,pipe,backdoor) action = do
    if opt_nodir command   -- Опция "--nodir" отключает запись в архив всех служебных блоков - остаются только сами сжатые данные
    then return (error "Attempt to use value returned by writeControlBlock when \"--nodir\"")
    else do
      writeBlock pipe blockType compressor compressor False $ do  -- запишем в архив блок каталога
        action
        return []
      (thisBlock, [])  <-  getP backdoor                      -- получим его дескриптор
      writeBlock pipe DESCR_BLOCK aNO_COMPRESSION aNO_COMPRESSION False $ do  -- запишем этот дескриптор в архив
        archiveWriteBlockDescriptor thisBlock bufOps
        return []
      (_, [])  <-  getP backdoor                              -- оприходуем ненужный дескриптор дескриптора
    return thisBlock                                        -- возвратим дескриптор блока каталога

