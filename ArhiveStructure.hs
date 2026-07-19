----------------------------------------------------------------------------------------------------
---- Working with the archive file structure.                                                 ------
---- An archive is a sequence of blocks, each of which can be:                                ------
----   * a data block                                                                         ------
----   * an archive directory block                                                           ------
----   * another service block (recovery record, general archive info, etc.)                  ------
---- After each service block (that is, any block except data blocks), a                      ------
----   descriptor is written that allows this block to be located and read even if            ------
----   the archive contents are damaged.                                                      ------
---- This module contains procedures for:                                                     ------
----   * writing and reading archive service blocks                                           ------
----   * writing, finding and reading service block descriptors                               ------
----------------------------------------------------------------------------------------------------
module ArhiveStructure where

import Prelude hiding (catch)
import Control.Monad
import Data.Word
import Data.Maybe
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Storable

import qualified Data.Foldable
import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import Compression
import Encryption
import Options

-- |Signature used to search for service block descriptors
aSIGNATURE = make4byte 65 114 67 1 :: Word32

-- |Signature written at the very beginning of the archive - it identifies the file as an archive, + archiver version
aARCHIVE_SIGNATURE = (aSIGNATURE, aARCHIVE_VERSION)

-- |How many bytes at the end of the archive to scan when searching for the signature that starts the last block's descriptor
aSCAN_MAX = 4096

-- Tags of optional fields
aTAG_END = 0::Integer   -- ^tag marking the end of the optional fields


----------------------------------------------------------------------------------------------------
---- Writing/reading/finding an archive service block descriptor -----------------------------------
----------------------------------------------------------------------------------------------------

-- |Write into the archive the descriptor of service block `block` and the CRC of that descriptor
archiveWriteBlockDescriptor block (receiveBuf,sendBuf) = do
  crc  <- ref aINIT_CRC
  let sendBuf_UpdatingCRC buf size len  =  do crc .<- updateCRC buf len;  sendBuf buf size len
  ByteStream.writeAll receiveBuf sendBuf_UpdatingCRC (return ())
    -- Layout of a directory block and of any other control block - first the compressed data, then its descriptor:
    --   signature, block type, compressor used, unpacked and packed sizes, CRC of the unpacked data
    (aSIGNATURE, blType block, blCompressor block, blOrigSize block, blCompSize block, blCRC block)
  acrc <- val crc
  -- The CRC of the descriptor itself is written after the descriptor
  ByteStream.writeAll receiveBuf sendBuf (return ())
    (finishCRC acrc)

-- |Read a service block descriptor from the buffer and decode it, taking into account
-- that inside the archive this descriptor is located at offset `arcpos`
archiveReadBlockDescriptor archive arcpos buf bufsize = do
  -- First check the CRC of the descriptor itself
  right_crc  <- peekByteOff buf (bufsize - sizeOf (undefined::CRC))
  descriptor_crc <- calcCRC buf (bufsize - sizeOf (undefined::CRC))
  if descriptor_crc/=right_crc
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0354 block descriptor at pos %1 is corrupted", show arcpos]
    else do
  -- Decode the contents of the descriptor:
  -- signature, block type, compressor used, unpacked and packed sizes, CRC of the unpacked data
  (sign, block_type, compressor, origsize, compsize, crc)  <-  ByteStream.readMemory buf bufsize
  let pos   =  blDecodePosRelativeTo arcpos compsize  -- position of the start of the block in the archive
      block =  ArchiveBlock archive block_type compressor pos origsize compsize crc undefined (enc compressor)
  if sign/=aSIGNATURE || pos<0
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0355 %1 is corrupted", block_name block]
    else return$ Right block

{-# NOINLINE archiveWriteBlockDescriptor #-}
{-# NOINLINE archiveReadBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- Scanning a corrupted archive in search of surviving blocks ------------------------------------
----------------------------------------------------------------------------------------------------

-- |Scan the archive in search of surviving blocks.
-- Builds a list of them and returns a pseudo FOOTER BLOCK containing all the blocks found.
-- The procedure reads the archive from the end in 8 Mb chunks, looks for block descriptors in these chunks
-- and then verifies that both the descriptor and the block itself are intact.
findBlocksInBrokenArchive arcname = do
  archive <- archiveOpen arcname
  arcsize <- archiveGetSize archive
  -- Allocate a buffer of 8 Mb + two times 4 Kb to simplify the descriptor search implementation
  allocaBytes (aHUGE_BUFFER_SIZE+2*aSCAN_MAX) $ \buf -> do
  blocks <- withList $ scanArchiveSearchingDescriptors archive buf arcsize
  if null blocks
    then registerError$ BROKEN_ARCHIVE arcname ["0356 archive directory not found"]
    else do
  -- Return the normal FOOTER_BLOCK if it was found in the archive
  --if blType (head blocks) == FOOTER_BLOCK
  --  then return (archive, (head blocks) {ftBlocks=reverse blocks})
  --  else do
  let pseudo_footer = FooterBlock
        { ftBlocks   = reverse blocks
        , ftLocked   = False
        , ftComment  = ""
        , ftRecovery = ""
        , ftSFXSize  = minimum (map blPos blocks)   -- SFX size = position of HEADER_BLOCK in the archive
        }
  return (archive, pseudo_footer)

-- Find in the archive the control blocks whose descriptors start
-- at addresses <pos, and add these descriptors to the list found
scanArchiveSearchingDescriptors archive buf pos found =
  when (pos>0) $ do
    -- To speed up reading of the archive we round the required address down to an 8 Mb boundary
    -- and search for a descriptor from that boundary base_pos up to position pos-1
    let base_pos = (pos-1) `roundDown` aHUGE_BUFFER_SIZE
    archiveSeek archive base_pos
    -- We read 4 Kb more than needed so that we can check a
    -- descriptor starting at the very end of the buffer (a descriptor is certainly shorter than 4 Kb)
    len <- archiveReadBuf archive buf (i$ pos-base_pos+aSCAN_MAX)
    memset (buf+:len) 0 aSCAN_MAX  -- just in case, pad the data read with 4096 zero bytes
    -- scanMem returns the position in the archive after which all descriptors have already been found
    newpos <- scanMem archive base_pos found buf (i (pos-base_pos) `min` len)
    -- Call the function recursively to search for descriptors in the previous chunk
    scanArchiveSearchingDescriptors archive buf newpos found

-- Scan buffer buf in search of descriptors starting within the first len bytes of that buffer
scanMem archive base_pos found buf len = do
  pos' <- ref base_pos
  whenRightM_ (archiveFindBlockDescriptor archive base_pos buf (len+aSCAN_MAX) len) $ \block -> do
    -- The descriptor of block `block` was found. This means further search can be limited
    -- to its position, and if it is a directory block - to the position of the first data block in it
    pos' =: blPos block
    -- Maybe this should not be done, since the archive may contain "shuffled sectors"
    -- and accordingly, where the data of this block is supposed to be there may well be
    -- completely different blocks. On the other hand, that data may simply contain another archive
    -- in uncompressed form, and then we would include its blocks in the block list of our archive :)
    -- As an option - the "thoroughness of the search" could be controlled by a switch
    -- when (blType block == DIR_BLOCK) $ do
    --   data_blocks <- archiveReadDir_OnlyBlocks
    --   pos' =: minpos data_blocks
    found <<= block
  -- If the buffer still contains unscanned data, continue searching in it,
  -- otherwise - move on to the previous chunk of the archive
  pos <- val pos'
  if pos > base_pos
    then scanMem archive base_pos found buf (i$ pos-base_pos)
    else return pos

-- |Try to find the (last) block descriptor in the given buffer.
-- The total amount of data in the block is size, but we are only interested in descriptors
-- starting within the first len bytes of the block.
archiveFindBlockDescriptor archive base_pos buf size len =
  go ((size-sizeOf aSIGNATURE) `max` (len-1)) defaultError
    where
  go pos err | pos<0     = return$ Left err
             | otherwise = do
       x <- peekByteOff buf pos
       if x==aSIGNATURE
         then do -- A descriptor signature was found at address pos, check whether it is a real descriptor
                 res <- archiveReadBlockDescriptor
                          archive            -- the archive file
                          (base_pos+i pos)   -- position of the decoded descriptor in the archive file
                          (buf+:pos)         -- memory address of the decoded descriptor
                          (size-pos)         -- maximum possible size of this descriptor
                 case res of
                   Left  err -> go (pos-1) err
                   Right _   -> return res
         else go (pos-1) err
  -- Error message returned if no descriptor at all was found in the block
  defaultError = BROKEN_ARCHIVE (archiveName archive) ["0357 archive signature not found at the end of archive"]

{-# NOINLINE findBlocksInBrokenArchive #-}
{-# NOINLINE scanArchiveSearchingDescriptors #-}
{-# NOINLINE scanMem #-}
{-# NOINLINE archiveFindBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- Writing the archive header block (HEADER_BLOCK) -----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Write the archive signature into the archive header block (HEADER_BLOCK)
archiveWriteHeaderBlock (receiveBuf,sendBuf) = do
  ByteStream.writeAll receiveBuf sendBuf (return ()) aARCHIVE_SIGNATURE


----------------------------------------------------------------------------------------------------
---- Writing and reading the RECOVERY block --------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Write a RECOVERY block
archiveWriteRecoveryBlock :: (ByteStream.BufferData a) =>  Maybe a -> Ptr CChar -> Int -> (ByteStream.RecvBuf, ByteStream.SendBuf) -> IO ()
archiveWriteRecoveryBlock moreinfo buf size (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  Data.Foldable.forM_ moreinfo (ByteStream.write stream)
  ByteStream.writeBuf stream buf size
  ByteStream.closeOut stream


----------------------------------------------------------------------------------------------------
---- Writing and reading the archive footer block (FOOTER_BLOCK) -------------------------------------
----------------------------------------------------------------------------------------------------

-- |Information contained in the FOOTER BLOCK
data FooterBlock = FooterBlock
       { ftBlocks     :: ![ArchiveBlock]     -- list of blocks in the archive (excluding data blocks)
       , ftLocked     :: !Bool               -- is the archive locked against modification?
       , ftComment    :: !String             -- archive comment
       , ftRecovery   :: !String             -- recovery info setting
       , ftSFXSize    :: !FileSize           -- size of the SFX module preceding the archive proper (computed as the amount of data preceding the first archive block)
       }

-- |Write the FOOTER_BLOCK
archiveWriteFooterBlock control_blocks arcLocked arcComment (arcRecovery::String) arcpos (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let utf8comment  =  ByteStream.toUTF8List arcComment
  ByteStream.write        stream (map (blockToTuple arcpos) control_blocks)   -- write the descriptions of the control blocks,
  ByteStream.write        stream arcLocked                                    -- ... the flag marking the archive as locked against modification
  ByteStream.writeInteger stream 0                                            -- ... the archive comment in the old format - absent
  ByteStream.write        stream arcRecovery                                  -- ... the amount of recovery information
  ByteStream.writeInteger stream (length utf8comment)                         -- ... the archive comment (encoded as a list, since this encodes the length explicitly and the comment may therefore contain zero characters)
  ByteStream.writeList    stream utf8comment                                  --     -.-
  ByteStream.closeOut     stream

-- |Read the information from the FOOTER_BLOCK
archiveReadFooterBlock footer@ArchiveBlock {
                                   blArchive  = archive
                                 , blType     = block_type
                                 , blPos      = pos
                                 , blOrigSize = origsize
                               }
                       decryption_info = do
  when (block_type/=FOOTER_BLOCK) $
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0358 last block of archive is not footer block"]
  withPool $ \pool -> do   -- use a memory pool so that the allocated buffers are freed automatically on exit
    (buf,size) <- archiveBlockReadAll pool decryption_info footer  -- put the unpacked data of the block into a buffer
    stream <- ByteStream.openMemory buf size
    control_blocks <- ByteStream.read stream      -- read the descriptions of the control blocks,
    locked         <- ByteStream.read stream      -- ... the flag marking the archive as locked against modification
    oldComment     <- ByteStream.readInteger stream >>= ByteStream.readList stream >>== map (toEnum.i :: Word32 -> Char)  -- ... and the archive comment (read as a list, since this encodes the length explicitly and the comment may therefore contain zero characters)
    isEOF          <- ByteStream.isEOFMemory stream  -- Older versions of the program did not write information about the recovery record
    recovery       <- not isEOF &&& ByteStream.read stream  -- ... the setting for the RECOVERY information added to the archive
    isEOF          <- ByteStream.isEOFMemory stream
    comment        <- not isEOF &&& (ByteStream.readInteger stream >>= ByteStream.readList stream >>== ByteStream.fromUTF8)  -- ... and the archive comment (read as a list, since this encodes the length explicitly and the comment may therefore contain zero characters)
    ByteStream.closeIn stream
    let blocks = map (tupleToBlock archive pos) control_blocks   -- build ArchiveBlock structures from the data read
    return FooterBlock
             { ftBlocks   = blocks++[footer]
             , ftLocked   = locked
             , ftComment  = comment ||| oldComment
             , ftRecovery = recovery
             , ftSFXSize  = minimum (map blPos blocks)   -- SFX size = position of HEADER_BLOCK in the archive
             }

{-# NOINLINE archiveWriteFooterBlock #-}
{-# NOINLINE archiveReadFooterBlock #-}


----------------------------------------------------------------------------------------------------
---- Archive block (data block, directory or service block) ----------------------------------------
----------------------------------------------------------------------------------------------------

-- |Archive block
data ArchiveBlock = ArchiveBlock
       { blArchive     :: !Archive      -- the archive this block belongs to
       , blType        :: !BlockType    -- block type
       , blCompressor  :: !Compressor   -- compression method
       , blPos         :: !FileSize     -- position of the block in the archive file
       , blOrigSize    :: !FileSize     -- size of the block when unpacked
       , blCompSize    :: !FileSize     -- size of the block when packed
       , blCRC         :: !CRC          -- CRC of the unpacked data (service blocks only)
       , blFiles       ::  Int          -- number of files (data blocks only)
       , blIsEncrypted ::  Bool         -- is the block encrypted?
       }

instance Eq ArchiveBlock where
  (==)  =  map2eq$ map2 (blPos, archiveName . blArchive)

-- |Helper function computing the blIsEncrypted field from blCompressor
enc = any isEncryption

-- |For storing information about archive blocks in the archive. The block position is written
-- relative to `arcpos` - the archive position of the block in which this information is saved
blockToTuple              arcpos (ArchiveBlock _ t c p o s crc f e) = (t,c,arcpos-p,o,s,crc)
tupleToBlock     archive arcpos (t,c,p,o,s,crc) = ArchiveBlock archive t c (arcpos-p) o s crc undefined (enc c)
tupleToDataBlock archive arcpos   (c,p,o,s,f)   = ArchiveBlock archive DATA_BLOCK c (arcpos-p) o s 0 f (enc c)

-- Separate functions for (de)coding a block position relative to another place in the archive
blEncodePosRelativeTo arcpos arcblock  =  arcpos - blPos arcblock
blDecodePosRelativeTo arcpos offset    =  arcpos - offset

-- |Block description
block_name block  =  (case blType block of
                          DESCR_BLOCK    -> "block descriptor"
                          HEADER_BLOCK   -> "header block"
                          DATA_BLOCK     -> "data block"
                          DIR_BLOCK      -> "directory block"
                          FOOTER_BLOCK   -> "footer block"
                          RECOVERY_BLOCK -> "recovery block"
                          _              -> "block of unknown type"
                      ) ++ " at pos "++ show (blPos block)

-- |Archive block type (add new values only at the end, since they are written into the archive!)
data BlockType = DESCR_BLOCK       -- ^Tag of an archive block descriptor  (written after each service block, that is all blocks except DATABLOCK)
               | HEADER_BLOCK      -- ^Tag of the archive header block     (used as the signature of an archive file)
               | DATA_BLOCK        -- ^Tag of a data block
               | DIR_BLOCK         -- ^Tag of a directory block
               | FOOTER_BLOCK      -- ^Tag of the archive footer block     (containing the list of blocks in the archive)
               | RECOVERY_BLOCK    -- ^Tag of a block holding recovery info
               | UNKNOWN_BLOCK     -- Actually unused variants standing for all block types not supported by this version of the program
               | UNKNOWN_BLOCK2
               | UNKNOWN_BLOCK3
     deriving (Eq,Enum)

instance ByteStream.BufferData BlockType  where
  write buf = ByteStream.writeInteger buf . fromEnum
  read  buf = ByteStream.readInteger  buf >>== toEnum

-- Operations on archive blocks
archiveBlockSeek    block pos       =  archiveSeek    (blArchive block) (blPos block + pos)
archiveBlockRead block = archiveRead    (blArchive block)
archiveBlockReadBuf block = archiveReadBuf (blArchive block)

-- |Allocate a buffer, read the block contents into it and check the CRC
archiveBlockReadAll pool
                    decryption_info
                    block@ArchiveBlock {
                              blArchive     = archive
                            , blType        = block_type
                            , blCompressor  = compressor
                            , blPos         = pos
                            , blCRC         = right_crc
                          } = do
  let origsize = i$ blOrigSize block
      compsize = i$ blCompSize block
  (origbuf, decompressed_size)  <-  decompressInMemory pool compressor decryption_info archive pos compsize origsize
  crc <- calcCRC origbuf origsize
  when (crc/=right_crc || decompressed_size/=origsize) $ do
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0359 %1 failed decompression", block_name block]
  return (origbuf, origsize)

-- |Allocate a buffer and read the block contents into it. Does not check the CRC and does not unpack the data!
archiveBlockReadUnchecked pool block = do
  when (blCompressor block/=aNO_COMPRESSION) $ do
    registerError$ BROKEN_ARCHIVE (archiveName$ blArchive block) ["0360 %1 should be uncompressed", block_name block]
  archiveMallocReadBuf pool (blArchive block) (blPos block) (i$ blOrigSize block)

-- |Allocate a buffer and read data from the archive into it
archiveMallocReadBuf pool archive pos size = do
  buf         <- pooledMallocBytes pool (size+8)  -- +8 - because of shortcomings in ByteStream :(
  archiveSeek    archive pos
  archiveReadBuf archive buf size
  return buf

-- |Decompress in memory a block that may be packed by several algorithms and encrypted on top of that
decompressInMemory mainPool compressor decryption_info archive pos compsize origsize = do
  withPool $ \tempPool -> do
  let process srcbuf srcsize [] = return (srcbuf, srcsize)
      process srcbuf srcsize (algorithm:algorithms) = do
        let (dstsize, pool) = if null algorithms
                                then (origsize, mainPool)
                                else (max compsize origsize*2+100*kb, tempPool)
        dstbuf <- pooledMallocBytes pool (dstsize+8)  -- +8 - because of shortcomings in ByteStream :(
        decompressed_size <- decompressMem algorithm srcbuf srcsize dstbuf dstsize
        pooledReallocBytes tempPool srcbuf 0
        process dstbuf decompressed_size algorithms
  --
  if compressor==aNO_COMPRESSION
    then do compbuf <- archiveMallocReadBuf mainPool archive pos (compsize+8)
            return (compbuf, compsize)
    else do
  -- Supply the encryption algorithms with the keys needed for decryption
  keyed_compressor <- generateDecryption compressor decryption_info
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (archiveName archive) ""
  -- Read the original block from the archive
  compbuf <- archiveMallocReadBuf tempPool archive pos compsize
  process compbuf compsize (reverse (map fromJust keyed_compressor))


{-# NOINLINE archiveBlockReadAll #-}
{-# NOINLINE archiveMallocReadBuf #-}

