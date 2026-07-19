----------------------------------------------------------------------------------------------------
---- Working with the archive directory.                                                      ------
---- This module contains procedures for:                                                     ------
----   * reading the input archive structure (i.e. directories and other service blocks)      ------
----   * writing and reading archive directories                                              ------
----------------------------------------------------------------------------------------------------
module ArhiveDirectory where

import Prelude hiding (catch)
import Control.Monad
import qualified HashTable as Hash
import Data.List
import Foreign.Marshal.Pool
import System.Mem

-- import GHC.PArr

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import Compression      (CRC, Compressor, isFakeCompressor)
import UI               (debugLog)
import Options
import ArhiveStructure

-- |The --nodates flag: don't store file mtimes in the archive (FreeArc 0.67).
-- Set before compression begins, in ArcCreate.
nodates_ref :: IORef Bool
nodates_ref = unsafePerformIO (newIORef False)
{-# NOINLINE nodates_ref #-}

----------------------------------------------------------------------------------------------------
---- Reading the input archive structure -----------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |All the information needed about the input archive
data ArchiveInfo = ArchiveInfo
         { arcArchive    :: Archive           -- the open archive file
         , arcFooter     :: FooterBlock       -- the archive's FOOTER BLOCK
         , arcDirectory  :: [CompressedFile]  -- the files contained in the archive
         , arcDataBlocks :: [ArchiveBlock]    -- list of solid blocks
         , arcDirBytes   :: FileSize          -- size of the service blocks when unpacked
         , arcDirCBytes  :: FileSize          -- size of the service blocks when packed
         , arcDataBytes  :: FileSize          -- size of the data when unpacked
         , arcDataCBytes :: FileSize          -- size of the data when packed
         , arcPhantom    :: Bool              -- True if the archive doesn't actually exist (used for main_archive)
         }

-- Procedures that simplify working with archives
arcGetPos  = archiveGetPos . arcArchive
arcSeek    = archiveSeek   . arcArchive
arcComment = ftComment . arcFooter

-- |A phantom, non-existent archive, needed for use in certain operations
-- (merging file lists, closing input archives)
phantomArc  =  (dirlessArchive (error "phantomArc:arcArchive") (FooterBlock [] False "" "" 0)) {arcPhantom = True}

-- |An archive without a file directory - used only to call writeSFX from runArchiveRecovery
dirlessArchive archive footer = ArchiveInfo archive footer [] [] (error "emptyArchive:arcDirBytes") (error "emptyArchive:arcDirCBytes") (error "emptyArchive:arcDataBytes") (error "emptyArchive:arcDataCBytes") False

-- |Close the archive file, unless this is a phantom archive
arcClose arc  =  unless (arcPhantom arc) $  do archiveClose (arcArchive arc)


{-# NOINLINE archiveReadInfo #-}
-- |Read the archive directory
archiveReadInfo command               -- the command being executed, with all its options
                arc_basedir           -- base directory inside the archive ("" for add commands)
                disk_basedir          -- base directory on disk ("" for add/list commands)
                filter_f              -- predicate for filtering the list of files in the archive
                processFooterInfo     -- procedure executed on the data from FOOTER_BLOCK
                arcname = do          -- name of the file containing the archive
  -- Read FOOTER_BLOCK and run the supplied procedure on it
  (archive,footer) <- if opt_broken_archive command /= "-"
                         then findBlocksInBrokenArchive arcname
                         else archiveReadFooter command arcname
  processFooterInfo archive footer

  -- Read the contents of the directory blocks described in FOOTER_BLOCK
  let dir_blocks  =  filter ((DIR_BLOCK ==) . blType) (ftBlocks footer)
  files  <-  foreach dir_blocks $ \block -> do
    withPool $ \pool -> do
      (buf,size) <- archiveBlockReadAll pool (opt_decryption_info command) block
      archiveReadDir arc_basedir disk_basedir (opt_dir_exclude_path command) archive (blPos block) filter_f (return (buf,size))

  let data_blocks = concatMap fst files
      directory   = concatMap snd files

  -- Add information about the archive's file list to arcinfo
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
-- |Read the archive footer block
archiveReadFooter command               -- the command being executed, with all its options
                  arcname = do          -- name of the file containing the archive
  archive <- archiveOpen arcname
  arcsize <- archiveGetSize archive
  let scan_bytes = min aSCAN_MAX arcsize  -- scan the last 4096 bytes of the archive, if there are that many :)

  withPool $ \pool -> do
    -- Read the last 4096 bytes of the archive, which should contain the FOOTER_BLOCK descriptor
    buf <- archiveMallocReadBuf pool archive (arcsize-scan_bytes) (i scan_bytes)
    -- Find and decode the last archive descriptor (it must be the FOOTER_BLOCK descriptor)
    res <- archiveFindBlockDescriptor archive (arcsize-scan_bytes) buf (i scan_bytes) (i scan_bytes)
    case res of
      Left  msg -> registerError msg
      Right footer_descriptor -> do
              -- Read the FOOTER_BLOCK described by this descriptor entirely into a buffer and decode its contents
              footer <- archiveReadFooterBlock footer_descriptor (opt_decryption_info command)
              return (archive,footer)


----------------------------------------------------------------------------------------------------
---- Writing a directory block ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveWriteDir #-}
-- |Encode `dirdata` and send the resulting data on for further processing via `sendBuf`
archiveWriteDir dirdata     -- list of pairs (block :: ArchiveBlock, directory :: [FileWithCRC])
                arcpos      -- position in the archive where this directory starts
                (receiveBuf -- "(buf,size) <- receiveBuf" obtains the next buffer of size `size` to work with
                ,sendBuf)   -- "sendBuf buf size len" sends the data prepared in the buffer to the output
                = do
  debugLog "\n  Writing directory"
  let blocks      :: [ArchiveBlock]
      blocks       = map fst dirdata            -- list of solid blocks that ended up in this directory
      crcfilelist  :: [FileWithCRC]
      crcfilelist  = concatMap snd dirdata      -- combined list of files - in the order in which they are laid out in the blocks!
      filelist     :: [FileInfo]
      filelist     = map fwFileInfo crcfilelist -- information about the files themselves

  -- 0. Create the output buffer that uses `receiveBuf` and `sendBuf` to communicate with the outside world
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let write         :: (ByteStream.BufferData a) =>  a -> IO ()   -- shortcuts for the buffer-writing functions
      write          =  ByteStream.write          stream
      writeLength    :: [a] -> IO ()
      writeLength xs =  ByteStream.writeInteger   stream (length xs)
      writeList     :: (ByteStream.BufferData a) =>  [a] -> IO ()
      writeList      =  ByteStream.writeList      stream
      writeIntegers  =  mapM_ (ByteStream.writeInteger stream)
      writeTagged     tag x   =  write tag >> write x     -- tagged writing - for optional fields
      writeTaggedList tag xs  =  write tag >> writeList xs

  -- 1. Encode the descriptions of the archive blocks and the number of files in each of them
  writeLength dirdata               -- number of blocks. For each block the following is written:
  mapM_ (writeLength . snd) dirdata                        -- number of files
  let compressors   = map blCompressor blocks  :: [Compressor]
      encodedPositions = map (blEncodePosRelativeTo arcpos) blocks
      compSizes        = map blCompSize blocks  :: [FileSize]
  writeList compressors   -- compression method
  writeList encodedPositions   -- the open archive file
  writeList compSizes   -- file sizes

  -- 2. Write the list of directory names to the archive
    -- Obtain the list of directory names and the directory numbers corresponding to the files in filelist
  (n, dirnames, dir_numbers)  <-  enumDirectories filelist
  debugLog$ "  Found "++show n++" directory names"
  writeLength dirnames  -- temporary, to work around the problem with Compressor==[String]
  -- Always write directory names with '/' separator for cross-OS interop (matches FA 0.67).
  writeList   (map unixifyPath dirnames)

  -- 3. Encode each remaining field of CompressedFile/FileInfo separately
    -- to do: add RLE encoding of fields?
  writeList$ map (fpBasename . fiStoredName)  filelist     -- file names
  writeIntegers                             dir_numbers  -- directory numbers
  writeList$ map fiSize                     filelist     -- file sizes
  nodates <- val nodates_ref
  writeList$ map (if nodates then const aMINIMAL_POSSIBLE_DATETIME else fiTime) filelist     -- the open archive file
  writeList$ map fiIsDir                    filelist     -- directory flags
  -- cfArcBlock and cfPos are encoded implicitly, by sorting on these two fields
  writeList$ map fwCRC                      crcfilelist  -- CRC

  -- 4. Optional fields, prefixed by their tags, with the optional-fields terminator tag at the end
  write aTAG_END  -- there are no optional fields yet, so all we have to do is write their terminator tag right away

  -- 5. That's all! :)
  ByteStream.closeOut stream
  -- This makes Arc.exe crash!!! - when (length filelist >= 10000) performGC  -- Collect garbage if the block contains a fair number of files
  debugLog "  Directory written"


-- Building, from a list of files, a list of unique directories + the directory number for each file in the list
enumDirectories filelist = do
  -- For each Stored file name we look up a name with the same directory in the hash table `table`.
  -- If it is found, we get that directory's number out of the hash table,
  -- and if not - we insert this name into the hash table with the next sequential number, which are
  -- generated via the variable n, and append the directory name to the list `dirnames`.
  -- Thus the hash table `table` maps directory names to their numbers
  -- in the list of all directories `dirnames` being built.
  table <- Hash.new (==) fpHash                     -- maps directories to their numbers

  -- Return, for a list of files, the number of unique directory names, their full list,
  -- and the directory number for each file (e.g. [0,1,0,0,2] for a\1 b\1 a\2 a\3 c\1)
  let go []              dirnames dir_numbers n = return (n, reverse dirnames, reverse dir_numbers)
      go (fileinfo:rest) dirnames dir_numbers n = do
        let storedName  =  fiStoredName fileinfo    -- the name intended to be stored in the archive
            dirname     =  fpParent storedName      -- the directory the file belongs to
        x <- Hash.lookup table dirname              -- is this directory already in the hash?
        case x of                                   -- If not, then
          Nothing -> do Hash.insert table dirname n -- put the directory number into the hash
                        -- Add the directory name to the list of directory names,
                        -- the directory number to the list of directory numbers for each file,
                        -- and increment the directory counter
                        go rest (fpDirectory storedName:dirnames) (n:dir_numbers) $! n+1
          Just x  -> do go rest dirnames (x:dir_numbers) n
  --
  go filelist [] [] (0::FileCount)


----------------------------------------------------------------------------------------------------
---- Reading a directory block----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE archiveReadDir #-}
-- |Read a directory written by the function `archiveWriteDir`
archiveReadDir arc_basedir   -- base directory in the archive
               disk_basedir  -- base directory on disk
               ep            -- exclude directories from names/allow absolute paths
               archive       -- the archive file
               arcpos        -- position in the archive where this directory starts
               filter_f      -- predicate for filtering files
               receiveBuf    -- "(buf,size) <- receiveBuf" obtains the next buffer of size `size` to work with
               = do
  debugLog "  Decoding directory"

  -- 0. Create the input buffer that uses the function `receiveBuf` to communicate with the outside world
  stream <- ByteStream.open receiveBuf (\a b c->return ()) (return ())
  let read         :: (ByteStream.BufferData a) =>  IO a   -- shortcuts for the buffer-reading functions
      read           = ByteStream.read stream
      readList     :: (ByteStream.BufferData a) =>  Int -> IO [a]
      readList       = ByteStream.readList stream
      readInteger    = ByteStream.readInteger stream
      readLength     = readInteger
      readIntegers n = replicateM n readInteger

  -- 1. Read the descriptions of the archive blocks
  num_of_blocks <- readLength                     -- number of blocks
  -- For each block we read:
  num_of_files  <- readIntegers num_of_blocks     -- number of files
  blCompressors <- readList     num_of_blocks     -- compression method
  blOffsets     <- readList     num_of_blocks     -- the block's relative position in the archive file
  blCompSizes   <- readList     num_of_blocks     -- the block's packed size

  -- 2. Read the directory names
  total_dirs    <-  readLength                    -- How many directory names in total are stored in this archive directory
  -- Sanitize directory names: strip ".."/"." (prevent path traversal on extraction),
  -- and convert separators to the current OS convention. Matches FA 0.67.
  storedName    <-  readList total_dirs >>== map (remove_unsafe_dirs . make_OS_native_path) >>== toP

  -- 3. Read the data lists for each field in CompressedFile/FileInfo
  let total_files = sum num_of_files              -- total number of files in the directory
  names         <- readList     total_files       -- File names (without the directory name)
  dir_numbers   <- readIntegers total_files       -- Directory number for each of the files
  sizes         <- readList     total_files       -- File sizes
  times         <- readList     total_files       -- File modification times
  dir_flags     <- readList     total_files       -- Boolean "is this a directory?" flags
  crcs          <- readList     total_files       -- File CRCs

  -- 4. Optional fields, prefixed by their tags, with the optional-fields terminator tag at the end
{-repeat_while (read) (/=aTAG_END) $ \tag -> do
    (isMandatory::Bool) <- read
    when isMandatory $ do
      registerError$ GENERAL_ERROR ("can't skip mandatory field TAG="++show tag++" in archive directory")
    readInteger >>= ByteStream.skipBytes stream   -- skip this field's data
    return ()
-}
  -- 5. That's all! :)
  ByteStream.closeIn stream
  debugLog "  Directory decoded"

  ------------------------------------------------------------------------------------------------
  -- Now build the directory from the data we read -----------------------------------------------
  ------------------------------------------------------------------------------------------------
  -- Arrays holding information about the directories
  let drop_arc_basedir  = if arc_basedir>""  then drop (length arc_basedir + 1)  else id
      make_disk_name    = case ep of         -- Turns a name in the archive into a name on disk
                            0 -> const ""    --   the "e" command -> use only the base name
                            3 -> id          --   the -ep3 option -> use the full name
                            _ -> stripRoot   --   by default      -> strip the "d:\" part
      -- Arrays mapping a directory number to its Filtered/Disk name (the array for Stored name is built right away while reading)
      filteredName      = fmap drop_arc_basedir                    storedName
      diskName          = fmap ((disk_basedir </>) . make_disk_name) filteredName
      -- Arrays mapping a directory number to a PackedFilePath structure
      storedInfo        = fmap packParentDirPath storedName
      filteredInfo      = fmap packParentDirPath filteredName
      diskInfo          = fmap packParentDirPath diskName
      -- For each directory - a boolean flag: does its name start with the base directory ("-ap")
      dirIncludedArray  = fmap (arc_basedir `isParentDirOf`) storedName
      dirIncluded       = if arc_basedir==""  then const True  else (dirIncludedArray!:)

  -- List of Maybe FileInfo structures (Nothing for those files that do not belong to
  -- the base directory ("-ap") or do not pass the file filtering predicate)
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

  -- Assemble FileInfo structures from the individual fields read from the archive
  let fileinfos = zipWith5 make_fi dir_numbers names sizes times dir_flags

  -- Reconstruct the data block descriptors.
  -- First split the list of file lengths into sublists belonging to the individual blocks.
  -- This will let us compute the total size of the files in each of the blocks
  let filesizes = splitByLens num_of_files sizes
  let blocks    = map (tupleToDataBlock archive arcpos) $
                    zip5 blCompressors
                         blOffsets
                         (map sum filesizes)
                         blCompSizes
                         num_of_files

  -- Replicate the references to the data block descriptors so there are enough for all files :)
  let arcblocks = concat [ replicate files_in_block blockDescriptor
                           | (files_in_block, blockDescriptor) <- zip num_of_files blocks
                         ]

  -- A file's position in the block equals the total length of the preceding files in that block.
  -- filesizes - a list of lists of file lengths belonging to each block.
  -- To get a file's position inside the block from it, we simply compute
  -- a "scanning sum". We prepend [0] to each list of positions,
  -- so as to get the positions BEFORE the files rather than after them :)
  -- In short, if  num_of_files = [1..4]
  --           and sizes = [1..10]
  --           then filesizes = [[1],[2,3],[4,5,6],[7,8, 9,10]]
  --           and  positions = [ 0,  0,2,  0,4,9,  0,7,15,24]
  let positions = concatMap scanningSum filesizes
      scanningSum [] = []
      scanningSum xs = 0 : scanl1 (+) (init xs)

  -- Now we have all the components ready to build the list of files contained in this directory
  let files = [ CompressedFile fileinfo arcblock pos crc
              | (Just fileinfo, arcblock, pos, crc)  <-  zip4 fileinfos arcblocks positions crcs
              ]

  return $! evalList files               -- Force the created file list into an evaluated state
  when (total_files >= 10000) performGC  -- Collect garbage if the block contains a fair number of files
  debugLog "  Directory built"

  return (blocks, files)

--  let f CompressedFile{cfFileInfo=FileInfo{fiFilteredName=PackedFilePath{fpParent=PackedFilePath{fpParent=RootDir}}}} = True
--      f _ = False


----------------------------------------------------------------------------------------------------
---- File to compress (either from disk or from an already existing archive) -----------------------
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


-- |Check that the file to compress comes from an already existing archive rather than from disk
isCompressedFile CompressedFile{} = True
isCompressedFile DiskFile{}       = False

-- |The compression algorithm used for this (compressed) file
cfCompressor = blCompressor . cfArcBlock

-- |Is this a compressed file that uses a fake compression method?
isCompressedFake file  =  isCompressedFile file  &&  isFakeCompressor (cfCompressor file)

-- |Is this a password-protected file?
cfIsEncrypted = blIsEncrypted . cfArcBlock

-- |Determine the file type from its group; if the group isn't set - compute it from the name
cfType command file | group/=fiUndefinedGroup  =  opt_group2type command group
                    | otherwise                =  opt_find_type command fi
                                                    where fi    = cfFileInfo file
                                                          group = fiGroup fi


----------------------------------------------------------------------------------------------------
---- A file and its CRC - used to pass compression results -----------------------------------------
----------------------------------------------------------------------------------------------------

-- |File and it's CRC
data FileWithCRC = FileWithCRC { fwCRC  :: {-# UNPACK #-} !CRC
                               , fwType :: {-# UNPACK #-} !FileType
                               , fwFileInfo            :: !FileInfo
                               }

data FileType = FILE_ON_DISK | FILE_IN_ARCHIVE  deriving (Eq)

-- |Check that the compressed file comes from the source archive rather than from disk
isFileOnDisk fw  =  fwType fw == FILE_ON_DISK

-- |Convert FileToCompress to FileWithCRC
fileWithCRC (DiskFile       fi)          = FileWithCRC 0   FILE_ON_DISK    fi
fileWithCRC (CompressedFile fi _ _ crc)  = FileWithCRC crc FILE_IN_ARCHIVE fi

