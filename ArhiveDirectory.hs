----------------------------------------------------------------------------------------------------
---- ������ � ����������� ������.                                                             ------
---- ���� ������ �������� ��������� ���:                                                      ------
----   * ������ ��������� �������� ������ (�.�. ��������� � ������ ��������� ������)          ------
----   * ������ � ������ ��������� ������                                                     ------
----------------------------------------------------------------------------------------------------
module ArhiveDirectory where

import Prelude hiding (catch)
import Control.Monad
-- import Data.HashTable as Hash
import Data.List
import Foreign.Marshal.Pool
import System.Mem

-- import GHC.PArr

import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import Compression      (CRC, isFakeCompressor)
import UI               (debugLog)
import Options
import ArhiveStructure

----------------------------------------------------------------------------------------------------
---- ������ ��������� �������� ������ --------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |��� ����������� ���������� � ������� ������
data ArchiveInfo = ArchiveInfo
         { arcArchive    :: Archive           -- �������� ���� ������
         , arcFooter     :: FooterBlock       -- FOOTER BLOCK ������
         , arcDirectory  :: [CompressedFile]  -- �����, ������������ � ������
         , arcDataBlocks :: [ArchiveBlock]    -- ������ �����-������
         , arcDirBytes   :: FileSize          -- ������ ��������� ������ � ������������� ����
         , arcDirCBytes  :: FileSize          -- ������ ��������� ������ � ����������� ����
         , arcDataBytes  :: FileSize          -- ������ ������ � ������������� ����
         , arcDataCBytes :: FileSize          -- ������ ������ � ����������� ����
         , arcPhantom    :: Bool              -- True, ���� ������ �� ����� ���� ��� (������������ ��� main_archive)
         }

-- ���������, ���������� ������ � ��������
arcGetPos  = archiveGetPos . arcArchive
arcSeek    = archiveSeek   . arcArchive
arcComment = ftComment . arcFooter

-- |���������, �������������� �����, ����������� ��� ���������� � ��������� ���������
-- (������� ������� ������, �������� ������� �������)
phantomArc  =  (dirlessArchive (error "phantomArc:arcArchive") (FooterBlock [] False "" "" 0)) {arcPhantom = True}

-- |����� ��� �������� ������ - ������������ ������ ��� ������ writeSFX �� runArchiveRecovery
dirlessArchive archive footer = ArchiveInfo archive footer [] [] (error "emptyArchive:arcDirBytes") (error "emptyArchive:arcDirCBytes") (error "emptyArchive:arcDataBytes") (error "emptyArchive:arcDataCBytes") False

-- |������� �������� ����, ���� ������ ��� �� ��������� �����
arcClose arc  =  unless (arcPhantom arc) $  do archiveClose (arcArchive arc)


{-# NOINLINE archiveReadInfo #-}
-- |��������� ������� ������
archiveReadInfo command               -- ����������� ������� �� ����� � �������
                arc_basedir           -- ������� ������� ������ ������ ("" ��� ������ ����������)
                disk_basedir          -- ������� ������� �� ����� ("" ��� ������ ����������/��������)
                filter_f              -- �������� ��� ���������� ������ ������ � ������
                processFooterInfo     -- ���������, ����������� �� ������ �� FOOTER_BLOCK
                arcname = do          -- ��� �����, ����������� �����
  -- ��������� FOOTER_BLOCK � ��������� �� ��� ���������� ���������
  (archive,footer) <- if opt_broken_archive command /= "-"
                         then findBlocksInBrokenArchive arcname
                         else archiveReadFooter command arcname
  processFooterInfo archive footer

  -- ��������� ���������� ������ ��������, ��������� � FOOTER_BLOCK
  let dir_blocks  =  filter ((DIR_BLOCK==).blType) (ftBlocks footer)
  files  <-  foreach dir_blocks $ \block -> do
    withPool $ \pool -> do
      (buf,size) <- archiveBlockReadAll pool (opt_decryption_info command) block
      archiveReadDir arc_basedir disk_basedir (opt_dir_exclude_path command) archive (blPos block) filter_f (return (buf,size))

  let data_blocks = concatMap fst files
      directory   = concatMap snd files

  -- ������� � arcinfo ���������� � ������ ������ � ������
  return ArchiveInfo { arcArchive    = archive
                     , arcFooter     = footer
                     , arcDirectory  = directory
                     , arcDataBlocks = data_blocks
                     , arcDirBytes   = sum (map blOrigSize dir_blocks)
                     , arcDirCBytes  = sum (map blCompSize dir_blocks)
                     , arcDataBytes  = sum (map blOrigSize data_blocks)
                     , arcDataCBytes = sum (map blCompSize data_blocks)
                     , arcPhantom    = False
                     }


{-# NOINLINE archiveReadFooter #-}
-- |��������� ��������� ���� ������
archiveReadFooter command               -- ����������� ������� �� ����� � �������
                  arcname = do          -- ��� �����, ����������� �����
  archive <- archiveOpen arcname
  arcsize <- archiveGetSize archive
  let scan_bytes = min aScanMax arcsize  -- ��������� 4096 ���� � ����� ������, ���� ������� ������� :)

  withPool $ \pool -> do
    -- ��������� 4096 ���� � ����� ������, ������� ������ ��������� ���������� FOOTER_BLOCK'�
    buf <- archiveMallocReadBuf pool archive (arcsize-scan_bytes) (i scan_bytes)
    -- ����� � ���������� ��������� ���������� ������ (��� ������ ���� ���������� FOOTER_BLOCK'�)
    res <- archiveFindBlockDescriptor archive (arcsize-scan_bytes) buf (i scan_bytes) (i scan_bytes)
    case res of
      Left  msg -> registerError msg
      Right footer_descriptor -> do
              -- ��������� FOOTER_BLOCK, ����������� ���� ������������, ������� � ����� � ���������� ��� ����������
              footer <- archiveReadFooterBlock footer_descriptor (opt_decryption_info command)
              return (archive,footer)


----------------------------------------------------------------------------------------------------
---- ������ ����� �������� -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveWriteDir #-}
-- |������������ `dirdata` � ������� ���������� ������ �� ���������� ��������� � ������� `sendBuf`
archiveWriteDir dirdata     -- ������ ��� (block :: ArchiveBlock, directory :: [FileWithCRC])
                arcpos      -- ������� � ������, ��� ���������� ���� �������
                (receiveBuf -- "(buf,size) <- receiveBuf" �������� ��� ������ ��������� ����� �������� `size`
                ,sendBuf)   -- "sendBuf buf size len" �������� �������������� � ������ ������ �� �����
                = do
  debugLog "\n  Writing directory"
  let blocks      = map fst dirdata            :: [ArchiveBlock]  -- ������ �����-������, �������� � ������ �������
      crcfilelist = concatMap snd dirdata      :: [FileWithCRC]   -- ������������ ������ ������ - � ��� �������, � ����� ��� ����������� � ������!
      filelist    = map fwFileInfo crcfilelist :: [FileInfo]      -- ���������� � ����� ������

  -- 0. C������� �������� �����, ������������ ��� ������� � ������� ����� ������� `receiveBuf` � `sendBuf`
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let write         :: (ByteStream.BufferData a) =>  a -> IO ()   -- shortcuts ��� ������� ������ � �����
      write          =  ByteStream.write          stream
      writeLength    =  ByteStream.writeInteger   stream . length
      writeList     :: (ByteStream.BufferData a) =>  [a] -> IO ()
      writeList      =  ByteStream.writeList      stream
      writeIntegers  =  mapM_ (ByteStream.writeInteger stream)
      writeTagged     tag x   =  write tag >> write x     -- ������ � ������ - ��� ������������ �����
      writeTaggedList tag xs  =  write tag >> writeList xs

  -- 1. ���������� �������� ������ ������ � ���-�� ������ � ������ �� ���
  writeLength dirdata               -- ���-�� ������.    ��� ������� ����� ������������:
  mapM_ (writeLength.snd) dirdata                        -- ���-�� ������
  writeList$ map blCompressor blocks   -- ����� ������
  writeList$ map (blEncodePosRelativeTo arcpos) blocks   -- ������������� ������� ����� � ����� ������
  writeList$ map blCompSize blocks   -- ������ ����� � ����������� ����

  -- 2. ������� � ����� ������ ��� ���������
    -- ������� ������ ��� ��������� � ������ ���������, ��������������� ������ � filelist
  (n, dirnames, dir_numbers)  <-  enumDirectories filelist
  debugLog$ "  Found "++show n++" directory names"
  writeLength dirnames  -- ��������, ��� ������ �������� � Compressor==[String]
  writeList   dirnames

  -- 3. ���������� �������� ������ ���������� ���� � CompressedFile/FileInfo
    -- to do: �������� RLE-����������� �����?
  writeList$ map (fpBasename.fiStoredName)  filelist     -- ����� ������
  writeIntegers                             dir_numbers  -- ������ ���������
  writeList$ map fiSize                     filelist     -- ������� ������
  writeList$ map fiTime                     filelist     -- ������� ��������
  writeList$ map fiIsDir                    filelist     -- �������� ��������
  -- cfArcBlock � cfPos ���������� ������, ���� ���������� �� ���� ���� �����
  writeList$ map fwCRC                      crcfilelist  -- CRC

  -- 4. ������������ ����, �������������� ������ ������, � ����� - ��� ��������� ������������ �����
  write aTagEnd  -- ���� ������������ ����� ���, ��� ������� ������ ����� �������� ��� �� ���������

  -- 5. �����! :)
  ByteStream.closeOut stream
  -- ��� �������� � ������ Arc.exe!!! - when (length filelist >= 10000) performGC  -- ������ �����, ���� ���� �������� ���������� ����� ������
  debugLog "  Directory written"


-- �������� �� ������ ������ - ������ ���������� ��������� + ����� �������� ��� ������� ����� � ������
enumDirectories filelist = do
  -- ��� ������� Stored ����� ����� �� ���� ��� � ��� �� ��������� � ���-������� `table`.
  -- ���� ��� �������, �� �� �������� �� ���-������� ����� ����� ��������,
  -- � ���� ��� - ��������� ��� ��� � ���-������� � ��������� ���������� �������, �������
  -- ��������� ����� ���������� n, � ��������� ��� �������� � ������ `dirnames`.
  -- ����� �������, ���-������� `table` ���������� ����� ��������� � �� ������
  -- � ����������� ������ ���� ��������� `dirnames`.
  table <- Hash.new (==) fpHash                     -- ���������� �������� � �� ������

  -- ���������� ��� ������ ������ ���������� ���������� ��� ���������, �� ������ ������,
  -- � ����� �������� ��� ������� ����� (��������, [0,1,0,0,2] ��� a\1 b\1 a\2 a\3 c\1)
  let go []              dirnames dir_numbers n = return (n, reverse dirnames, reverse dir_numbers)
      go (fileinfo:rest) dirnames dir_numbers n = do
        let storedName  =  fiStoredName fileinfo    -- ���, ��������������� ��� ���������� � ������
            dirname     =  fpParent storedName      -- �������, � �������� ����������� ����
        x <- Hash.lookup table dirname              -- ���� �� ��� � ���� ���� �������?
        case x of                                   -- ���� ���, ��
          Nothing -> do Hash.insert table dirname n -- ������� � ��� ����� ��������
                        -- �������� ��� �������� � ������ ��� ���������,
                        -- ����� �������� � ������ ������� �������� ��� ������� �����,
                        -- � ���������������� ������� ���������
                        go rest (fpDirectory storedName:dirnames) (n:dir_numbers) $! n+1
          Just x  -> do go rest dirnames (x:dir_numbers) n
  --
  go filelist [] [] (0::FileCount)


----------------------------------------------------------------------------------------------------
---- ������ ����� �������� -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveReadDir #-}
-- |��������� �������, ���������� �������� `archiveWriteDir`
archiveReadDir arc_basedir   -- ������� ������� � ������
               disk_basedir  -- ������� ������� �� �����
               ep            -- ��������� �������� �� ���/��������� ���������� ����
               archive       -- ���� ������
               arcpos        -- ������� � ������, ��� ���������� ���� �������
               filter_f      -- �������� ���������� ������
               receiveBuf    -- "(buf,size) <- receiveBuf" �������� ��� ������ ��������� ����� �������� `size`
               = do
  debugLog "  Decoding directory"

  -- 0. C������� ������� �����, ������������ ��� ������� � ������� ����� ������� `receiveBuf`
  stream <- ByteStream.open receiveBuf (\a b c->return ()) (return ())
  let read         :: (ByteStream.BufferData a) =>  IO a   -- shortcuts ��� ������� ������ �� ������
      read           = ByteStream.read stream
      readList     :: (ByteStream.BufferData a) =>  Int -> IO [a]
      readList       = ByteStream.readList stream
      readInteger    = ByteStream.readInteger stream
      readLength     = readInteger
      readIntegers n = replicateM n readInteger

  -- 1. ��������� �������� ������ ������
  num_of_blocks <- readLength                     -- ���-�� ������
  -- ��� ������� ����� ���������:
  num_of_files  <- readIntegers num_of_blocks     -- ���-�� ������
  blCompressors <- readList     num_of_blocks     -- ����� ������
  blOffsets     <- readList     num_of_blocks     -- ������������� ������� ����� � ����� ������
  blCompSizes   <- readList     num_of_blocks     -- ������ ����� � ����������� ����

  -- 2. ��������� ����� ���������
  total_dirs    <-  readLength                    -- ������� ����� ��� ��������� ��������� � ���� ���������� ������
  storedName    <-  readList total_dirs >>== toP  -- ������ ��� ���������

  -- 3. ��������� ������ ������ ��� ������� ���� � CompressedFile/FileInfo
  let total_files = sum num_of_files              -- ��������� ���-�� ������ � ��������
  names         <- readList     total_files       -- ����� ������ (��� ����� ��������)
  dir_numbers   <- readIntegers total_files       -- ����� �������� ��� ������� �� ������
  sizes         <- readList     total_files       -- ������� ������
  times         <- readList     total_files       -- ����� ����������� ������
  dir_flags     <- readList     total_files       -- ��������� ����� "��� �������?"
  crcs          <- readList     total_files       -- CRC ������

  -- 4. �������������� ����, �������������� ������ ������, � ����� - ��� ��������� �������������� �����
{-repeat_while (read) (/=aTagEnd) $ \tag -> do
    (isMandatory::Bool) <- read
    when isMandatory $ do
      registerError$ GENERAL_ERROR ("can't skip mandatory field TAG="++show tag++" in archive directory")
    readInteger >>= ByteStream.skipBytes stream   -- ���������� ������ ����� ����
    return ()
-}
  -- 5. �����! :)
  ByteStream.closeIn stream
  debugLog "  Directory decoded"

  ------------------------------------------------------------------------------------------------
  -- ������ �������� ������� �� ����������� ������ -----------------------------------------------
  ------------------------------------------------------------------------------------------------
  -- �������, ���������� ���������� � ���������
  let drop_arc_basedir  = if arc_basedir>""  then drop (length arc_basedir + 1)  else id
      make_disk_name    = case ep of         -- ���������� ��� � ������ � ��� �� �����
                            0 -> const ""    --   ������� "e"  -> ������������ ������ ������� ���
                            3 -> id          --   ����� -ep3   -> ������������ ������ ���
                            _ -> stripRoot   --   �� ��������� -> �������� "d:\" �����
      -- �������, ������������ ����� �������� � ��� Filtered/Disk name (������ ��� Stored name �������� ����� ��� ������)
      filteredName      = fmap drop_arc_basedir                    storedName
      diskName          = fmap ((disk_basedir </>) . make_disk_name) filteredName
      -- �������, ������������ ����� �������� � ��������� PackedFilePath
      storedInfo        = fmap packParentDirPath storedName
      filteredInfo      = fmap packParentDirPath filteredName
      diskInfo          = fmap packParentDirPath diskName
      -- ��� ������� �������� - ��������� ����: ���������� �� ��� ��� � �������� �������� ("-ap")
      dirIncludedArray  = fmap (arc_basedir `isParentDirOf`) storedName
      dirIncluded       = if arc_basedir==""  then const True  else (dirIncludedArray!:)

  -- ������ �������� Maybe FileInfo (Nothing ��� ��� ������, ������� �� �����������
  -- �������� �������� ("-ap") ��� �� �������� ����� �������� ���������� ������)
  let make_fi dir name size time dir_flag =
        if dirIncluded dir && filter_f fileinfo  then Just fileinfo  else Nothing

        where fileinfo = FileInfo { fiFilteredName  =  if arc_basedir>""           then fiFilteredName  else fiStoredName
                                  , fiDiskName      =  if disk_basedir>"" || ep/=3 then fiDiskName      else fiFilteredName
                                  , fiStoredName    =  fiStoredName
                                  , fiSize          =  size
                                  , fiTime          =  time
                                  , fiAttr          =  0
                                  , fiIsDir         =  dir_flag
                                  , fiGroup         =  fiUndefinedGroup
                                  }
              fiStoredName    =  packFilePathPacked2 stored   (fpPackedFullname stored)   name
              fiFilteredName  =  packFilePathPacked2 filtered (fpPackedFullname filtered) name
              fiDiskName      =  packFilePathPacked2 disk     (fpPackedFullname disk)     name
              stored   = storedInfo  !:dir
              filtered = filteredInfo!:dir
              disk     = diskInfo    !:dir

  -- �������� ��������� FileInfo �� ��������� �����, ����������� �� ������
  let fileinfos = zipWith5 make_fi dir_numbers names sizes times dir_flags

  -- �������������� ����������� ������ ������.
  -- ������� �������� ������ ���� ������ �� ���������, ����������� � ��������� ������.
  -- ��� �������� ��� ��������� ��������� ����� ������ � ������ �� ������
  let filesizes = splitByLens num_of_files sizes
  let blocks    = map (tupleToDataBlock archive arcpos) $
                    zip5 blCompressors
                         blOffsets
                         (map sum filesizes)
                         blCompSizes
                         num_of_files

  -- ��������� ������ �� ����������� ������ ������, ����� ������� �� ��� ����� :)
  let arcblocks = concat [ replicate files_in_block blockDescriptor
                           | files_in_block  <- num_of_files  -- ���-�� ������ � ��������� ����� ������
                           | blockDescriptor <- blocks        -- ���������� ���������� �����
                         ]

  -- ������� ����� � ����� ����� ��������� ����� ���������� ������ � ���� �����.
  -- filesizes - ������ ������� ���� ������, ����������� � ������� �����.
  -- ��� ����, ����� �������� �� ���� ������� ����� ������ �����, �� ������ �������
  -- "����������� �����". ��������� [0] � ������ ������� ������ �������,
  -- ����� �������� ������� ����� �������, � �� ����� ��� :)
  -- ����� ������, ����  num_of_files = [1..4]
  --                  �  sizes = [1..10]
  --               ��  filesizes = [[1],[2,3],[4,5,6],[7,8, 9,10]]
  --                �  positions = [ 0,  0,2,  0,4,9,  0,7,15,24]
  let positions = concatMap scanningSum filesizes
      scanningSum [] = []
      scanningSum xs = 0 : scanl1 (+) (init xs)

  -- ������ � ��� ������ ��� ���������� ��� �������� ������ ������, ������������ � ���� ��������
  let files = [ CompressedFile fileinfo arcblock pos crc
              | (Just fileinfo, arcblock, pos, crc)  <-  zip4 fileinfos arcblocks positions crcs
              ]

  return $! evalList files               -- �������� ��������� ������ ������ � ����������� ���������
  when (total_files >= 10000) performGC  -- ������ �����, ���� ���� �������� ���������� ����� ������
  debugLog "  Directory built"

  return (blocks, files)

--  let f CompressedFile{cfFileInfo=FileInfo{fiFilteredName=PackedFilePath{fpParent=PackedFilePath{fpParent=RootDir}}}} = True
--      f _ = False


----------------------------------------------------------------------------------------------------
---- ������������� ���� (��� � �����, ��� �� ��� ������������� ������) -----------------------------
----------------------------------------------------------------------------------------------------

-- |File to compress: either file on disk or compressed file in existing archive
data FileToCompress
  = DiskFile
      { cfFileInfo           :: !FileInfo
      }
  | CompressedFile
      { cfFileInfo           :: !FileInfo
      , cfArcBlock           ::  ArchiveBlock   -- Archive datablock which contains file data
      , cfPos                ::  FileSize       -- Starting byte of file data in datablock
      , cfCRC :: {-# UNPACK #-} !CRC            -- File's CRC
      }

-- |Assign type synonym because variant label can't be used in another types declarations
type CompressedFile = FileToCompress


-- |�������� ����, ��� ������������� ���� - �� ��� ������������� ������, � �� � �����
isCompressedFile CompressedFile{} = True
isCompressedFile DiskFile{}       = False

-- |�������� ������, �������������� ��� ������� (�������) �����
cfCompressor = blCompressor.cfArcBlock

-- |��� ������ ����, ������������ �������� ����� ����������?
isCompressedFake file  =  isCompressedFile file  &&  isFakeCompressor (cfCompressor file)

-- |��� ���������������� ����?
cfIsEncrypted = blIsEncrypted . cfArcBlock

-- |���������� ��� ����� �� ������, ���� ��� �� ����������� - ��������� �� �����
cfType command file | group/=fiUndefinedGroup  =  opt_group2type command group
                    | otherwise                =  opt_find_type command fi
                                                    where fi    = cfFileInfo file
                                                          group = fiGroup fi


----------------------------------------------------------------------------------------------------
---- ���� � ��� CRC - ������������ ��� �������� ����������� �������� -------------------------------
----------------------------------------------------------------------------------------------------

-- |File and it's CRC
data FileWithCRC = FileWithCRC { fwCRC  :: {-# UNPACK #-} !CRC
                               , fwType :: {-# UNPACK #-} !FileType
                               , fwFileInfo            :: !FileInfo
                               }

data FileType = FILE_ON_DISK | FILE_IN_ARCHIVE  deriving (Eq)

-- |�������� ����, ��� ����������� ���� - �� ��������� ������, � �� � �����
isFileOnDisk fw  =  fwType fw == FILE_ON_DISK

-- |Convert FileToCompress to FileWithCRC
fileWithCRC (DiskFile       fi)          = FileWithCRC 0   FILE_ON_DISK    fi
fileWithCRC (CompressedFile fi _ _ crc)  = FileWithCRC crc FILE_IN_ARCHIVE fi

