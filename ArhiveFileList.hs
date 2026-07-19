--

module ArhiveFileList where

import qualified HashTable as Hash
import Data.Ix
import Data.List hiding (sortOn)
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc

import Utils
import Files
import Charsets         (i18n)
import Errors
import FileInfo
import Compression
import Options
import UI               (debugLog0, uiScanning, uiCorrectTotal)
import ArhiveStructure
import ArhiveDirectory

--

-- |Sort `filelist` according to order given in `command`
sortFiles Command{ opt_sort_order    = sort_order     -- sort order as a string, e.g. "gen" - sort by group, extension and name
                  , opt_find_group    = find_group     -- the "FileInfo -> group number" function
                  , opt_groups_count  = groups_count   -- number of groups (`find_group` returns results in the range 0..groups_count-1)
                  }
           filelist = dirs ++ sortBy sort_order files  -- Sort only the files, gathering the directories at the head of the list
  where
    (dirs,files)  =  partition fiSpecialFile filelist
    sortBy sortOrder =
        case sortOrder of
          ""     -> id
          "n"    -> sortOn' (fpPackedBasename . fiStoredName)
          "s"    -> sortOn' s_key
          "es"   -> sortOn' es_key
          "en"   -> sortOn' en_key
          "ep"   -> sortOn' ep_key
          "epn"  -> sortOn' epn_key
          'e':xs -> concatMap (sortBy xs) . sort_and_groupOn' e_key
          'g':xs -> concatMap (sortBy xs) . partitionList groups_count find_group   -- split the files into groups and sort each group by the remaining criteria
          'r':xs -> (unsafePerformIO . reorder) . sortBy xs
          'c':xs -> (\(small,large) -> sortBy xs small ++ sortBy "s" large)
                          . partition (\fi -> fiSize fi < i (128*kb))
          _ | sortOrder `contains` 'i'  ->  intellectual_sort sortOrder
          _      -> sortOn' (keyFunc sortOrder find_group)

    s_key   fi =  fiSize fi                                                   where filename = fiStoredName fi
    e_key   fi =  fpLCExtension filename                                      where filename = fiStoredName fi
    n_key   fi =  fpPackedBasename filename                                   where filename = fiStoredName fi
    ns_key  fi = (fpPackedBasename filename, fiSize fi)                       where filename = fiStoredName fi
    np_key  fi = (fpPackedBasename filename, fpPackedDirectory filename)      where filename = fiStoredName fi
    es_key  fi = (fpLCExtension filename, fiSize fi)                          where filename = fiStoredName fi
    en_key  fi = (fpLCExtension filename, fpPackedBasename  filename)         where filename = fiStoredName fi
    ep_key  fi = (fpLCExtension filename, fpPackedDirectory filename)         where filename = fiStoredName fi
    epn_key fi = (fpLCExtension filename, fpPackedDirectory filename, fpPackedBasename filename)   where filename = fiStoredName fi

    -- Intelligent sorting of the file list (-ds=gepin/geipn)
    intellectual_sort sortOrder =
    -- 1. sort and group by "gep"
    -- 2. files with the same extension are grouped by the first three letters of the name
    -- 3a. those groups that contain only a single file - merge them together and sort by "s" ("ps", since files from the same directory are better kept together?)
    -- 3b. the remaining groups are sorted internally by "ns"
    -- 4. output the groups in order of increasing group size? of average file size in the group?
      concatMap isort . sort_and_groupOn' (keyFunc o1 find_group)
      where (o1,'i':o2) =  break (=='i') sortOrder
            isort group = -- Sort the list of files having the same extension
              let groups = groupOn three (sortOn (keyFunc o2 find_group) group)  -- group by the first 3 letters of the name
                             where three = take 3 . filenameLower . fpBasename . fiStoredName
                  (singles, full_groups)  =  partition (null . tail) groups  -- split into groups consisting of one single file, and "real groups" :)
                  list1  =  sortOn' s_key (concat singles)  -- the list of single files in the right order
              in list1 ++ concat full_groups

-- |Map `sort_order` to function returning ordering key
keyFunc sort_order find_group  =  map_functions (map key sort_order)
  where
        key 'p' = OrderPackedStr . fpPackedDirectory . fiStoredName
        key 'n' = OrderPackedStr . fpPackedBasename  . fiStoredName
        key 'e' = OrderFilePath  . fpLCExtension     . fiStoredName
        key 's' = OrderFileSize                      . fiSize
        key 't' = OrderFileTime                      . fiTime
        key 'g' = OrderGroup                         . find_group
        key 'c' = key 's'
        key 'i' = key 's'
        key 'r' = OrderGroup                         . const 1

-- |Data structure describing a sort key
data SortOrder =   OrderFilePath  !FilePath
                 | OrderPackedStr !MyPackedString
                 | OrderFileSize  !FileSize
                 | OrderFileTime  !FileTime
                 | OrderGroup     Int               deriving (Eq, Ord)

--

-- |Reorder the files so that identical or similar ones end up next to each other, namely -
-- those having the same extension and size (presumably the same file under different names),
-- or the same name and close sizes (presumably different versions of the same file)
reorder files = do
    -- Check that the filenames are equal and the sizes differ by no more than a factor of two (or are exactly equal for small files)
    let near_size (name1,size1) (name2,size2)  =  name1==name2 &&
               (if size1 <= i (16*kb)
                 then size1==size2
                 else size1.$inRange (size2 `div` 2, size2*2))
    -- The first hash is by matching extension and size, the second by matching name and closeness of size
    hash1 <- Hash.new (==)      (\(ext,size)  -> i$ filenameHash size ext)
    hash2 <- Hash.new near_size (\(name,size) ->    filenameHash 0 (myUnpackStr name))
    -- Put every file into the hash and find for it, if possible, a similar one among those already processed.
    let renumber (num,file) = do
            if fiSize file <= 1024
              then return (num, file)
              else do
            -- Find a file similar to the current one, preferably in the first hash table
            let key1 = (fpLCExtension$ fiStoredName file, fiSize file)
                key2 = (fpPackedBasename$ fiStoredName file, fiSize file)
            newnum <- (if fiSize file <= i (16*kb)
                         then return Nothing
                         else Hash.lookup hash1 key1) `defaultValM`
                      (Hash.lookup hash2 key2  `defaultValM` return num)
            -- The number of the similar file found (or the file's own number, if nothing similar
            -- could be found) becomes its "group number". Insert the file into both hash tables
            -- under this number, so that subsequent files similar to it can get the same
            -- group number, and return this number - it will be used during sorting
            -- to gather similar files together
            Hash.insert hash1 key1 newnum
            Hash.insert hash2 key2 newnum
            return (newnum, file)
    -- numbered_files - a list of (number, file) pairs where similar files got equal numbers
    numbered_files <- mapM renumber (zip [0..] files)
    -- Sort the files by group number/size/name/path
    let ordering (num,file) = (num, fiSize file, fpPackedBasename filename, fpPackedDirectory filename)   where filename = fiStoredName file
    return $ map snd $ sortOn ordering numbered_files

--

-- |Construct the list of files that should end up in the archive being created,
-- out of the lists of files in the input archive, in the additional archives and on disk,
-- first removing the duplicates from these lists.
-- The way the lists are merged depends on the archive update algorithm `update_type` (a/f/u/s)
--
joinLists main_archive added_archives added_diskfiles
           Command {            -- data about the command being executed:
               opt_update_type = update_type    -- archive update algorithm (a/f/u/s)
             , opt_append      = append         -- add new files only at the end of the archive?
             , opt_sort_order  = sort_order     -- sort order as a string, e.g. "gen" - sort by group, extension and name
             , opt_find_group  = find_group     -- the "FileInfo -> group number" function
             } = do

  -- The algorithm:
  -- 1. Create two hashes containing all the files in the archive and on disk, respectively.
  --    While building the hash tables we also get rid of duplicates (by StoredName) in the lists
  -- 2. If one of the lists is empty - simply return the other one
  -- 3. Walk through the list of files in the archive, find the corresponding file on disk (if any),
  --    and choose from these two files the one that should go into the output archive
  -- 4. Walk through the list of files from disk and add to the output archive those of them
  --    that were not present in the input archive

  let -- The list of files in the main archive, the one being updated
      main_list   =  arcDirectory main_archive
      -- The list of files added to the archive, including files from the additional input archives and files from disk
      added_list  =  concatMap arcDirectory added_archives  ++  map DiskFile added_diskfiles

  -- DEBUGGING
  let typ (DiskFile _)             = "Disk: "
      typ (CompressedFile {}) = "Archive: "
      name  fi = fpFullname (fiStoredName fi)
      names fi = fpFullname (fiDiskName fi)++" "++fpFullname (fiFilteredName fi)++" "++fpFullname (fiStoredName fi)
  --print$ map (names.cfFileInfo) arcdir
  --print$ map (names.cfFileInfo) diskfiles
  -- DEBUGGING

  -- 1. Create two hashes: one with the files from the archive being updated, and one with the files being added,
  --    and at the same time exclude duplicate files from the source lists
  let keyFunc  =  fiStoredName . cfFileInfo  -- the key these hash tables are indexed by
  (main_list,  main_hash )  <-  removeDuplicates main_list  keyFunc (==) fpHash
  (added_list, added_hash)  <-  removeDuplicates added_list keyFunc (==) fpHash

  -- 2. If one of the lists is empty - simply return the other one
  case () of
   _ | null main_list   -> return added_list  -- if one of the lists is empty, we simply return the other one
     | null added_list  -> return (if update_type=='s' then [] else main_list)  -- almost the same
     | otherwise        -> do
    -- Otherwise we have to merge these lists:

    -- 3. Walk through the files in the archive being updated and replace them, if needed, with the files being added
    let newer_file arcfile diskfile =   -- Return whichever of the files is newer
          if fiTime (cfFileInfo arcfile) >= fiTime (cfFileInfo diskfile)
          then arcfile   -- the file in the archive is newer or the same, so we take it
          else diskfile  -- the file on disk is newer, so we take it

    let sync_file arcfile diskfile =   -- Return the archive file if it doesn't differ from the disk one
          if fiTime (cfFileInfo arcfile) == fiTime (cfFileInfo diskfile)
          then arcfile   -- the file in the archive is the same, so we take it
          else diskfile  -- the file on disk is newer or older, so we take it

    let select_file arcfile = do
          diskfile <- Hash.lookup added_hash (keyFunc arcfile)
          case (diskfile, update_type) of
            (Nothing, 's')        ->  return Nothing        -- There is no file with this name on disk: mode "
            (Nothing,  _ )        ->  return$ Just arcfile   --   the other modes keep the already existing file in the archive
            (Just diskfile, 'a')  ->  return$ Just diskfile  -- The file exists both in the archive and on disk:  mode "a" always takes the file from disk
            (Just diskfile, 's')  ->  return$ Just (sync_file  arcfile diskfile)  -- mode "
            (Just diskfile,  _ )  ->  return$ Just (newer_file arcfile diskfile)  -- the other modes take the newer of the two files

    list1 <- mapMaybeM select_file main_list  -- choose between the file from the input archive and the file from disk

    -- 4. Add those files from the second list that were not present in the archive being updated at all
    let new_files_only diskfile = do
          -- Return true if this file was not present in the original archive
          isNothing ==<< Hash.lookup main_hash (keyFunc diskfile)

    list2 <- case update_type of
                 -- Mode "f": don't take the files that were absent from the input archive
               'f' -> return []
                 -- Take those files from disk that were not present in the input archive
               _   -> Utils.filterM new_files_only added_list

    -- DEBUGGING
    --print$ map (\f -> typ f ++ (fpFullname.fiStoredName.cfFileInfo) f) list1
    --print$ map (fpFullname.fiStoredName.cfFileInfo) list2
    -- DEBUGGING

    -- Merge the list of files in the archive with the list of files on disk
    let mergeFunction = case () of     -- Add the new files at the end of the archive in two cases:
          _ | append         -> (++)   --    when the " option is set
            | sort_order=="" -> (++)   --    when the sort key is empty ("-ds")
            | otherwise      -> mergeFilelists sort_order find_group  -- otherwise use a full-featured merge
    return$ mergeFunction list1 list2

-- |Joining the lists (files from the archive + files from disk)
mergeFilelists sort_order find_group filelist1 filelist2  =  dirs ++ files
  where -- Merge the files in the order defined by the -ds option, and the directories in "path+name" order
    (dirs1,files1)  =  partition (fiSpecialFile . cfFileInfo) filelist1
    (dirs2,files2)  =  partition (fiSpecialFile . cfFileInfo) filelist2
    dirs  = merge (map2cmp (keyFunc "pn"       find_group . cfFileInfo)) dirs1  dirs2
    files = merge (map2cmp (keyFunc sort_order find_group . cfFileInfo)) files1 files2

-- | Build a duplicate-free hash table out of the list `originalList`,
-- cleaning the duplicates out of the list itself at the same time
--
removeDuplicates originalList keyFunc eqFunc hashFunc = do
  table <- Hash.new eqFunc hashFunc

  -- Insert the element `value` into the hash `table` only if it doesn't yet hold an element with the same key.
  -- Of duplicate values in the list and the hash the first one is kept, so `update` cannot be used
  -- (to do: `reverse` would make it possible to use `update`)
  let insert_no_dups value = do
        let key  =  keyFunc value
        found <- Hash.lookup table key
        case found of
          Nothing       ->  do Hash.insert table key value
                               return True
          Just oldfile  ->  return False

  list <- Utils.filterM insert_no_dups originalList
  return (list,table)

{-# NOINLINE splitBy #-}
{-# NOINLINE removeDuplicates #-}

--

-- |Compressor for the archive's service blocks (except HEADER_BLOCK, which we don't compress)
dirCompressor  =  opt_dir_compressor

-- |Split the file list into parts, each of which should get its own directory block
splitToDirBlocks command  =  splitBy (opt_group_dir command) True

-- |The most suitable compressor for the files in the list (determined by the first file in the list)
-- (special files such as directories are not compressed, and files compressed with fake algorithms
-- are left unchanged)
dataCompressor _       []                                = aNO_COMPRESSION
dataCompressor command (file:_) | fiSpecialFile fi       = aNO_COMPRESSION
                                 | isCompressedFake file  = cfCompressor file
                                 | otherwise              = snd (types_of_compressors !! find_type fi)
  where fi = cfFileInfo file
        types_of_compressors = opt_data_compressor command
        find_type = opt_find_type command

-- |Split the file list into solid blocks, taking file types into account (splitByType)
-- and the solid-compression grouping specified in opt_group_data (splitOneType).
-- In addition, for block algorithms (bwt, lzp) the solid block size
-- is limited by the block size of the compression algorithm.
-- Finally, directories and files compressed with fake compressors (nodata/crconly)
-- are not split into separate solid blocks.
splitToSolidBlocks command filelist  =  (dirs &&& [(aNO_COMPRESSION,dirs)])
                                     ++ map (keyval (cfCompressor . head)) (groupOn cfArcBlock solidBlocksToKeep)
                                     ++ concatMap splitOneType (splitByType filesToSplit)
  where
    -- Directories go into a separate block
    (dirs,files)  =  partition (fiSpecialFile . cfFileInfo) filelist
    -- In the archive copying commands and when
    (solidBlocksToKeep, filesToSplit) | opt_keep_original command = partition isCompressedFile files
                                      | otherwise                 = ([],files)

    -- Split the list by file type ($binary, $text...).
    -- Files already compressed with fake algorithms must keep their former type (i.e. compression algorithm)
    splitByType filelist  =  map concatSnds groups
      where
        (fake,normal) = partition isCompressedFake filelist
        normalGroups  = mapFsts (snd.(opt_data_compressor command!!)) $ splitFileTypes command normal
        -- Fake files are split into groups by the compression algorithms used.
        fakeGroups    = map (keyval (cfCompressor . head)) $ sort_and_groupOn cfCompressor fake
        -- ... and these groups are joined with the groups of normal files that are to be compressed with the same algorithms
        groups        = sort_and_groupOn fst (fakeGroups++normalGroups)

    -- Split into solid blocks the list of files compressed with the given algorithm
    splitOneType (compressor,files) =
        -- For fake compressors or -m0 there is no point in splitting the block into parts
        if isFakeCompressor compressor || compressor==aNO_COMPRESSION
        then [(compressor,files)]
        else files.$ splitBy (opt_group_data command .$ addBlockSizeCrit) (opt_recompress command)
                  .$ map (compressor,)
      where
        -- For algorithm chains starting with TTA/MM/JPG, disable solid compression
        -- For algorithm chains starting with DICT - limit the solid block to the DICT block size
        -- For the other block algorithms (grzip, lzp) alone - to the block size of the compression algorithm.
        addBlockSizeCrit = case compressor of
            algorithm:_ | makeNonSolid  algorithm     ->  const [GroupNone]
            algorithm:_ | isDICT_Method algorithm     ->  ([GroupByBlockSize $ getBlockSize algorithm]++)
            [algorithm] | getBlockSize algorithm > 0  ->  ([GroupByBlockSize $ getBlockSize algorithm]++)
            _                                         ->  id

-- |Is it necessary to place the files compressed with this multimedia algorithm
-- into separate solid blocks? An exception can be made only in the case
-- where the algorithm contains the string "*8" and does not contain the string ":o", that is,
-- all the data is compressed as one continuous byte sequence which
-- there is no need to split into separate blocks
makeNonSolid m = any_function [isTTA_Method, isMM_Method, isJPG_Method] m
                 && (not (m `substr` "*8")  ||  (m `substr` ":o"))

--

-- |Split the file list into groups according to the given criteria (for data/directory blocks)
-- Each successive group is taken as the shortest segment satisfying at least one of the given criteria,
-- and when updating solid archives everything possible is done to avoid recompressing those solid blocks that stayed intact after the insertion of new files
-- crits - the group splitting criteria, recomress=True - ignore the boundaries of the old solid blocks
splitBy []    _          files = [files]  -- If there are no criteria for splitting into sublists - pass all the files as one common list
splitBy crits recompress files = splitByLen computeLen files where
  -- Return the number of files from the head of files that should go into the next solid block
  computeLen files = case () of
     _ | recompress     -> newLen   -- when
       | Just n<-oldLen -> n        -- copy the existing solid block of n files
       | Just n<-oldPos -> if n<=newLen
                             then n        -- make a new solid block reaching exactly up to the start of the next old one
                             else minLen
       | otherwise      -> newLen   -- fitting to the old solid block did not succeed
    where
      newLen = minimum$ map (`splitLen`    files) crits   -- length of the new solid block, computed according to the solid block splitting criteria
      minLen = minimum$ map (`splitLenMin` files) crits   -- minimum allowed solid block length (with the criteria reduced by a factor of 4)
      maxLen = minimum$ map (`splitLenMax` files) crits   -- ...
      oldLen = solidBlockLen files                        -- length of the existing solid block the given file list starts with, or Nothing
      oldPos = findSolidBlock minLen maxLen files         -- look, among the files that are to be included in the new solid block, for the start of an old solid block

-- |Length of the minimal group (initial segment of the file list) satisfying the given criterion
splitLen  GroupNone              = const 1
splitLen  GroupByExt             = length . head . groupOn (fpLCExtension . fiFilteredName . cfFileInfo)
splitLen (GroupBySize      size) = (1+)      . groupLen (fiSize . cfFileInfo) (+) (<i size)
splitLen (GroupByBlockSize size) = atLeast 1 . groupLen (fiSize . cfFileInfo) (+) (<special (i size))
splitLen (GroupByNumber       n) = atLeast 1 . const n
splitLen  GroupAll               = const maxBound

-- |Length of the minimal file group allowed for the given criterion (half the nominal one, a third for block compressors)
splitLenMin (GroupBySize      size) = splitLen (GroupBySize      (size `div` 2))
splitLenMin (GroupByBlockSize size) = splitLen (GroupByBlockSize (size `div` 3))
splitLenMin (GroupByNumber       n) = splitLen (GroupByNumber    (n    `div` 2))
splitLenMin x                       = splitLen x

-- |Length of the maximal file group allowed for the given criterion (1.5 times the nominal one, except for block compressors)
splitLenMax (GroupBySize      size) = splitLen (GroupBySize      (size+(size `div` 2)))
splitLenMax (GroupByBlockSize size) = splitLen (GroupByBlockSize size)
splitLenMax (GroupByNumber       n) = splitLen (GroupByNumber    (n   +(n    `div` 2)))
splitLenMax x                       = splitLen x

-- |Temporary: a special transformation to raise the -m2t compression speed on multi-core machines
special size | size>8*mb = size
             | otherwise = 4*size

--

-- |Check that the given file list is the complete list of files in a solid block
isWholeSolidBlock files@(CompressedFile {cfArcBlock=solidBlock, cfPos=pos}:_) =
  pos == 0                            &&    -- If the first file in the list is the start of a solid block (pos = the number of the first byte of this file within the solid block)
  blFiles solidBlock == length files  &&    --   the list has the same length as the solid block the first file in the list belongs to,
  all        isCompressedFile  files  &&    --   consists only of compressed files,
  isEqOn     cfArcBlock        files  &&    --   belonging to one and the same block
  isSortedOn cfPos             files        --   and sorted by their position within the solid block

isWholeSolidBlock _ = False

-- |Length of the initial segment of the file list containing files from one solid block (more precisely, exactly one whole solid block)
solidBlockLen []    = Nothing
solidBlockLen files = let n = blFiles (cfArcBlock (head files)) in   -- the number of files in the solid block that 'head files' belongs to
                      if isCompressedFile (head files)           -- The given list starts with a compressed file
                         && isWholeSolidBlock (take n files)     --   and is a complete solid block
                      then Just n
                      else Nothing

-- |Find a solid block starting in one of the first min..max files of the list,
-- and return the index of its first file
findSolidBlock min max = fmap (+min)                       -- compensate for the 'drop min'
                       . findIndex (isJust . solidBlockLen)
                       . take (max-min)
                       . tails
                       . drop min

--
{-
+don't forget about the already-packed files
+BUG: binary a . c -> "?" is returned and it gets packed as text
  "?" means neither text nor compressed, so $text/$compressed must be reset to "?",
    the rest keep their type.
    several different types - split into groups or (if they are too small anyway) pack as binary
+$compressed -> $incompressible in mmdet*2, #$incompressible=rep+tor
+new algorithm:
   0. detect distinguishes only text, compressed and the rest; therefore
   1. $text (and $compressed) should be conditionally excluded from arc . groups,
         so that these types are detected only for text+text+text or compressed+compressed+compressed
   2. when the types disagree (text+default+default) use the default type
+problems solved
   some . lib = text+default+default, as a result of which it is compressed as $binary instead of $obj
   $text arc ["default","default","default"]
+$compressed = [rep+]tor in -m2x..-m4  (absent in -m5/-m5x and above)
+turned off if the compressor list has no special algorithms for $text/$compressed
+Doesn't work: -mx: choose lzma/ppmd based on the actual compression.
-ma - control over autodetection. -ma+, -ma-. -ma9 = the most thorough autodetection
+check the beginning+middle+end of the file! .doc?
+split the data into large chunks, make more probes (2mb,5) and when they disagree recursively split the group into smaller parts
+Ruby & Dev-Cpp(*.map) - compression restored; C:\Base\Doc\Java\tutorial - twice as fast thanks to $compressed
+check files from the $wav/$bmp groups for MM
+files >64kb - detect them in chunks of 32-64 kb; take more chunks to determine the true nature of the file
+files with a binary + text part - allow up to 8% of binary part
+should be 1*default + 1*$text => default (count up to 20%)

1. detect text-binary files (do texts have a smooth context transition?)
4. identical files should be cut down with the help of lzma/rep/lzp
5. failures on *.rgb, texts with tables, so/lib without repdist
tune the probe parameters depending on the compression ratio.
utf-8: check detection and compression on Russian/English texts
replace "sort_and_groupOn fst" with something better (like partitionList) so that file type detection happens as packing proceeds
  [ filter "binary" xs, filter "text" xs, filter "!binary!text" xs]
files like readme.* and makefile.* may be joined together.. (split by extension only $default)
a type mismatch often means that files of different types were accidentally lumped together
files without extensions probably need to be analyzed more thoroughly
graphics? in skype . exe it is detected as text :(
zero-size files shouldn't turn up here?
binary, text, compressed - in order to overlap binary compression and text files reading
  compress the groups of large files first..
-}

aGroupSize = 2*mb       -- the overall file list is split into chunks of this size; for each chunk we first try to determine the common type
aCHUNKS = 5              -- number of probes made to determine the file type
aChunkSize = 64*kb      -- size of each probe

splitFileTypes command  -- Determine the file types from arc.groups
  | quick_and_dirty = deleteIf (null . snd) . zip [0..] . partitionList (opt_types_count command) (cfType command)
                      -- Split into groups by extension with max. 2 mb per group and determine the file types by content
  | otherwise       = unsafePerformIO . concatMapM groupType . splitBy [GroupByExt, GroupByBlockSize aGroupSize] True
--splitFileTypes = map (unsafePerformIO.groupType) . splitBy [GroupByExt, GroupByBlockSize (500*kb)] True
 where                           -- todo: add splitting by groups

  -- If autodetection won't help this algorithm
  quick_and_dirty  =  detect_level <= 1                                    -- Autodetection is disabled
                      || not (types `contains_one_of` detectable_types)    -- The fiddler is not needed :D
    where (types,compressors) = unzip (opt_data_compressor command)

  -- Thoroughness level of autodetection (by default it matches the compression level)
  detect_level = opt_autodetect command .$i

  -- Determine the type of a single (most likely fairly large) file
  groupType [file] = do
    let defaultType = getDefaultType file             -- file type according to arc.groups
        filesize = fiSize$ cfFileInfo file            -- file size
        chunks = filesize `div` aChunkSize + 1       -- how many blocks the file *can* be split into
        n = if chunks < aCHUNKS  then chunks          -- number of blocks to be checked
                                 else sqrt (i$ aCHUNKS * chunks) .$round
        blocksize = min aChunkSize (filesize `div` n)  -- size of the blocks being checked
        step = (filesize-n*blocksize) `div` n    -- gaps between the checked blocks (their size is chosen so as to spread n blocks of blocksize bytes evenly over the file)
    fmap maybeToList $ whenJustM (check defaultType (i blocksize) (take (i n) [0, blocksize+step ..]) file) $ \dataTypes -> do
      let typ = chooseType dataTypes defaultType
    --debugLog0$ show$ (fpBasename.fiDiskName.cfFileInfo) file
      debugLog0$ "  "++fst (opt_data_compressor command!!typ)++" "++(fpBasename . fiDiskName . cfFileInfo) file++"("++show n++") "++show dataTypes; myFlushStdout
      msg <- i18n "0248 Analyzed %1 files"
      uiScanning msg [file]
      return (typ,[file])

  -- Determine the type of a (presumably homogeneous) group of files
  groupType files@(file:_) = do
    let defaultType = getDefaultType file
        -- The list of file groups plus one file from each group that will be tested
        (fileGroups, filesToTry)
           -- If the list is small enough - treat every file as a separate group
           | len<=aCHUNKS  =  (map (:[]) files, files)
           -- Otherwise - split the file list into aCHUNKS parts equal in total file size
           -- and pick the largest file in each group for testing
           | otherwise     =  (files.$ splitByLen (splitLen$ GroupBySize$ totallen `div` aCHUNKS)
                              ,fileGroups.$ map (maxOn (fiSize . cfFileInfo)))
        -- The length of the list and the total size of the files in it
        len = length files
        totallen = sum$ map (fiSize . cfFileInfo) files
    -- Determine the types of the selected files and the "aggregate" type (if a file cannot be opened, its group gets the "default" type)
    dataTypes <- concatMapM (check defaultType aChunkSize [0] .>>== fromMaybe ["default"]) filesToTry
    let typ = chooseType dataTypes defaultType
    --debugLog0$ show$ map (fpBasename.fiDiskName.cfFileInfo) files
    debugLog0$ "  "++(if isAll (==) dataTypes  then fst$ opt_data_compressor command!!typ  else "?")++" "++show (map (fpBasename . fiDiskName . cfFileInfo) filesToTry)++" "++show dataTypes; myFlushStdout
    -- If the file types came out differently, let every subgroup determine its own type
    if not (isAll (==) dataTypes)
      then concatMapM groupType fileGroups
      else do msg <- i18n "0248 Analyzed %1 files"
              uiScanning msg files
              return [(typ,files)]

  -- The file type ($bmp/$obj/...) determined from arc.groups. Files whose types we are able
  -- to detect automatically ($text/$compressed) get the $binary type by default
  getDefaultType file  =  if typ=="" || typ `elem` detectable_types  then "$binary"  else typ
    where typ  =  fst (opt_data_compressor command!!cfType command file)

  -- Determine the data type from the probe results and the type from arc.groups
  chooseType dataTypes defaultType =
         (best `elemIndex` map fst (opt_data_compressor command)) `defaultVal` 0
    where best = bestType dataTypes .$changeTo [("default", defaultType)]

  -- Determine the correct data type from several probes
  -- (if [almost] all of them gave the same result - return it, otherwise - "default")
  bestType dataTypes@(_:_)
    | x>[]  &&  isAll (==) x
      && (lenx==total ||                          -- if the whole list consists of x
          total>=aCHUNKS && lenx==total-1  ||     -- or the whole list minus one element and the list is large enough
          lenx*12 >= total*11                     -- or 92% of the list
         )           =  head x         where x     = filter (/="default") dataTypes
                                             lenx  = length x
                                             total = length dataTypes
  bestType _         =  "default"

  -- Check chunks of blocksize bytes in the file file, starting at the positions positions
  -- (for files copied from the input archive, the file type is instead guessed from the compressor used)
  check defaultType blocksize positions file
    | isCompressedFile file  =  if defaultType `elem` typesByCompressor
                                  then return$ Just [(typeByCompressor . blCompressor . cfArcBlock) file]
                                  else return$ Just [defaultType]

    | otherwise = do let filename = (fpFullname . fiDiskName . cfFileInfo) file
                         onFail   = uiCorrectTotal (-1) (-fiSize (cfFileInfo file))
                     bracketCtrlBreakMaybe "fileClose:splitFileTypes" (tryOpen filename) onFail fileClose $ \f -> do
                     -- First of all, check the file for MM
                     mm <- detectMM file f defaultType
                     if mm then return [defaultType]  else do
                     -- Second - the check for $text/$compressed
                     foreach positions (detectType f blocksize)

  -- Ask detect_datatype() for the list of data types it is able to recognize
  detectable_types = words $ unsafePerformIO $ do
    allocaBytes 1000 $ \c_filetype -> do
    detect_datatype nullPtr 0 c_filetype
    peekCString c_filetype

  -- Read from the file f the data starting at position pos and of length size,
  -- and determine its type by calling the C function detect_datatype()
  detectType f size pos = do
    withChunk f pos size $ \buf len -> do
    allocaBytes 100 $ \c_filetype -> do
    detect_datatype buf len c_filetype
    peekCString c_filetype

  -- Check for an MM file
  detectMM file f defaultType =
    if not (isMMType defaultType)  then return False  else do
      let filesize = clipToMaxInt$ fiSize$ cfFileInfo file                 -- file size, clipped to 2 gb
      isMmHeader <- withChunk f 0 (1024 `min` filesize) $ \buf len -> do   -- read 1 kb from the start of the file
        detect_mm_header detect_level buf len >>== (/=0)                   --   check whether it contains the header of a guaranteed MM file
      if isMmHeader then return True else do                               -- if the header is found we return True, otherwise
      bytes <- detect_mm_bytes detect_level filesize                       -- how many bytes to check
      withChunk f ((filesize-bytes) `div` 2) bytes $ \buf len -> do        -- read a chunk of the recommended length from the middle of the file
        detect_mm detect_level buf len >>== (/=0)                          --   and check it for MM-type data

  -- Read from the file f at position pos data of length size and run action on it
  withChunk f pos size action = do
    allocaBytes (i size) $ \buf -> do
    fileSeek f (i pos)
    len <- fileReadBuf f buf (i size)
    action buf (i len)

foreign import ccall safe "Compression/MM/C_MM.h detect_datatype"
  detect_datatype :: Ptr CChar -> CInt -> Ptr CChar -> IO ()

foreign import ccall safe "Compression/MM/C_MM.h detect_mm_bytes"
  detect_mm_bytes :: CInt -> CInt -> IO CInt

foreign import ccall safe "Compression/MM/C_MM.h detect_mm"
  detect_mm :: CInt -> Ptr CChar -> CInt -> IO CInt

foreign import ccall safe "Compression/MM/C_MM.h detect_mm_header"
  detect_mm_header :: CInt -> Ptr CChar -> CInt -> IO CInt
