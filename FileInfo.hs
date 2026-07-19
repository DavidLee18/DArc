{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Obtaining and storing file information, searching for files on disk.                       ----
----------------------------------------------------------------------------------------------------
module FileInfo where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import qualified Data.Map.Strict as Hash
import Data.Int
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import System.IO.Unsafe
import System.Posix.Internals hiding (stat_mode)

import Utils
import Process
import Files
import Errors
#ifdef FREEARC_PACKED_STRINGS
import UTF8Z
#endif
#if defined(FREEARC_WIN) && !defined(__MHS__)
import Win32Files
import System.Win32.File
#endif


----------------------------------------------------------------------------------------------------
---- Compact representation of a file name ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Storing file names in a compact form, providing fast access
-- to the directory name, the file name without the directory, and the file extension
data PackedFilePath = PackedFilePath
  { fpPackedDirectory       :: !MyPackedString     -- Directory name
  , fpPackedBasename        :: !MyPackedString     -- File name without the directory, but with the extension
  , fpLCExtension           :: !String             -- Extension, converted to lower case
  , fpHash   :: {-# UNPACK #-} !Int32              -- Hash of the file name
  , fpParent                :: !PackedFilePath     -- The PackedFilePath structure of the parent directory
  }
  | RootDir

instance Eq PackedFilePath where
  (==)  =  map2eq$ map3 (fpHash,fpPackedBasename,fpPackedDirectory)

#ifdef FREEARC_PACKED_STRINGS
-- Using packed strings halves memory consumption
type MyPackedString = PackedString
myPackStr           = packString
myUnpackStr         = unpackPS

-- |Replaces repeated occurrences of the same extension with one shared string
packext ext = unsafePerformIO$ do
  m <- readIORef extsHash
  case Hash.lookup ext m of
    Nothing      -> do writeIORef extsHash (Hash.insert ext ext m)
                       return ext
    Just oldext  -> return oldext

extsHash = unsafePerformIO$ newIORef (Hash.empty :: Hash.Map String String)

#else
type MyPackedString = String
myPackStr           = id
myUnpackStr         = id
packext             = id
#endif

fpDirectory  =  myUnpackStr . fpPackedDirectory
fpBasename   =  myUnpackStr . fpPackedBasename

-- |Virtual field: the full file name, including directory and extension
fpFullname fp  =  fpDirectory fp </> fpBasename fp

-- |Fast computation of the packed full name
fpPackedFullname fp  =  if fpPackedDirectory fp == myPackStr ""
                          then fpPackedBasename fp
                          else myPackStr (fpFullname fp)


-- |Build the packed representation from a file name
packFilePath parent fullname  =  packFilePath2 parent dir name
  where (dir,name) = splitDirFilename fullname

-- |Build the packed representation from a directory name and a file name without the directory
packFilePath2       parent dir        name  =  packFilePathPacked2 parent (myPackStr dir) name
packFilePathPacked2 parent packed_dir name  =  packFilePathPacked3 parent packed_dir name (packext$ filenameLower$ getFileSuffix name)

-- |Build the packed representation from a directory name, a file name without the directory, and an extension.
packFilePath3 parent dir name lcext              =  packFilePathPacked3 parent (myPackStr dir) name lcext
packFilePathPacked3 parent packed_dir name lcext =
  PackedFilePath { fpPackedDirectory    =  packed_dir
                 , fpPackedBasename     =  myPackStr name
                 , fpLCExtension        =  lcext
                 , fpHash               =  filenameHash (fpHash parent) name
                 , fpParent             =  parent
                 }

-- |Create the structure for the base directory used when searching for files
packParentDirPath dir  =
  PackedFilePath { fpPackedDirectory    =  myPackStr ""   -- To avoid wasting time,
                 , fpPackedBasename     =  myPackStr dir  -- we put the whole directory name into Basename
                 , fpLCExtension        =  ""
                 , fpHash               =  filenameHash 0 (filter (not . isPathSeparator) dir)
                 , fpParent             =  RootDir
                 }

-- |Hash of the full file name (without directory separators!).
-- To speed up its computation it uses `dirhash` - the hash of the directory name containing the file,
-- and `basename` - the file name without the directory name
-- The accumulator is masked to 25 bits before each multiply. A hash of this
-- shape relies on wraparound, and h*37 leaves Int32 within a handful of
-- characters. GHC wraps silently, but MicroHs traps integer overflow and
-- raises "arithmetic overflow", so under mhs this aborted every command that
-- touched a filename -- which is every archive operation. Masking keeps the
-- largest intermediate at 0x1FFFFFF*37 + 255 = 1241514202, comfortably inside
-- Int32, so no wraparound is needed and both compilers agree.
--
-- Changing the mixing changes the hash values, which is safe here: fpHash is
-- never written to an archive. Its only uses are in-memory duplicate removal
-- (ArhiveFileList) and directory-numbering hash tables (ArhiveDirectory).
filenameHash {-dirhash basename-}  =  foldl (\h c -> (h .&. 0x1FFFFFF)*37 + i(ord c))

{-# INLINE filenameHash #-}


----------------------------------------------------------------------------------------------------
---- Matching file names against regular expressions -----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Match a file name against the wildcard `filespec`.
-- The wildcards "*", "*.ext" and a bare file name without a directory are handled specially
match_FP getName filespec =
  if filespec==reANY_FILE  then const True  else
    case (splitFilename3 filespec) of
      ("", "*", ext) -> match  (filenameLower ext)      . fpLCExtension
      ("", _,   _  ) -> match  (filenameLower filespec) . filenameLower . getName
      _              -> match  (filenameLower filespec) . filenameLower . fpFullname

-- |Does the file path `filepath` match at least one of the wildcards `filespecs`?
match_filespecs getName {-filespecs filepath-}  =  anyf . map (match_FP getName)

-- |The wildcard that matches any file name
reANY_FILE = "*"


----------------------------------------------------------------------------------------------------
---- File information ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Data types for..
type FileCount = Int              -- the number of files
type FileSize  = Integer          -- the size of a file or a read/write position within it
aFILESIZE_MIN  = -(2^63)          -- a very small value of type FileSize
type FileTime  = CTime            -- the creation/modification/access time of a file
type FileAttr  = FileAttributes   -- the DOS file attributes
type FileGroup = Int              -- the group number in arc.groups

-- |Structure holding all the file information we need
data FileInfo = FileInfo
  { fiFilteredName         :: !PackedFilePath  -- The file name matched against the ones given on the command line
  , fiDiskName             :: !PackedFilePath  -- The "external" file name - for reading/writing files on disk
  , fiStoredName           :: !PackedFilePath  -- The "internal" file name - the one stored in the archive directory
  , fiSize  :: {-# UNPACK #-} !FileSize        -- File size (0 for directories)
  , fiTime  :: {-# UNPACK #-} !FileTime        -- File creation date/time
  , fiAttr  :: {-# UNPACK #-} !FileAttr        -- DOS file attributes
  , fiIsDir :: {-# UNPACK #-} !Bool            -- Is it a directory?
  , fiGroup :: {-# UNPACK #-} !FileGroup       -- Group number in arc.groups
  }

-- |Convert a FileInfo into the file name on disk
diskName     = fpFullname . fiDiskName
storedName   = fpFullname . fiStoredName
filteredName = fpFullname . fiFilteredName

-- |Convert a FileInfo into the base file name
baseName     = fpBasename . fiStoredName

-- |Special files (directories, symlinks and the like) don't need to be compressed
fiSpecialFile = fiIsDir

-- |The group number used where no group number is actually needed.
fiUndefinedGroup = -1

-- |Create a FileInfo structure for a directory with the given name
createParentDirFileInfo fiFilteredName fiDiskName fiStoredName =
  FileInfo { fiFilteredName  =  packParentDirPath fiFilteredName
           , fiDiskName      =  packParentDirPath fiDiskName
           , fiStoredName    =  packParentDirPath fiStoredName
           , fiSize          =  0
           , fiTime          =  aMINIMAL_POSSIBLE_DATETIME
           , fiAttr          =  0
           , fiIsDir         =  True
           , fiGroup         =  fiUndefinedGroup
           }

-- |Re-read the file information after opening it (in case the file has changed in the meantime).
--  Returns incorrect fiAttr (under Unix) and fiGroup
rereadFileInfo fi file = do
  getFileInfo (fiFilteredName fi) (fiDiskName fi) (fiStoredName fi)

-- |Create a FileInfo structure with information about the given file.
--  Returns incorrect fiAttr (under Unix) and fiGroup
getFileInfo fiFilteredName fiDiskName fiStoredName  =
    let filename = fpFullname fiDiskName in do
    fileWithStatus "getFileInfo" filename $ \p_stat -> do
      fiIsDir  <-  stat_mode  p_stat  >>==  s_isdir
      fiTime   <-  stat_mtime p_stat
      fiSize   <-  if fiIsDir then return 0
                              else stat_size p_stat
      return$ Just$ FileInfo fiFilteredName fiDiskName fiStoredName fiSize fiTime 0 fiIsDir fiUndefinedGroup
  `catch`
    \(e::SomeException) -> do
             registerWarning$ CANT_GET_FILEINFO filename
             return Nothing  -- On an error from stat we return Nothing

-- |Restore date/time/attrs saved in FileInfo structure
setFileDateTimeAttr filename fileinfo  =  setFileDateTime filename (fiTime fileinfo)

{-# NOINLINE getFileInfo #-}


----------------------------------------------------------------------------------------------------
---- The process of searching for files on disk ----------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Settings for the process of searching for files on disk
data FindFiles = FindFiles
    { ff_disk_eq_filtered   :: Bool
    , ff_stored_eq_filtered :: Bool
    , ff_recursive          :: Bool
    , ff_parent_or_root     :: FileInfo -> FileInfo
    , ff_accept_f           :: FileInfo -> Bool
    , ff_process_f          :: [FileInfo] -> IO ()
    }


-- |Return the FileInfo of the files and directories (excluding "." and "..") located in the `parent` directory
getDirectoryContents_FileInfo ff parent{-parent FileInfo structure-} = do
  let -- Full disk name of the parent directory
      diskDirName = fpFullname$ fiDiskName parent
      -- Packed strings with the disk, filtered and stored names of the parent directory
      -- These names may coincide when -ap/-dp are absent, which lets us save memory in those cases
      packedDisk  = myPackStr diskDirName
      packedFiltered = if ff.$ff_disk_eq_filtered
                          then packedDisk
                          else myPackStr$ fpFullname$ fiFilteredName parent
      packedStored   = if ff.$ff_stored_eq_filtered
                          then packedFiltered
                          else myPackStr$ fpFullname$ fiStoredName   parent_or_root
      -- Choose parent or root as the parent record (the latter only with -ep0)
      parent_or_root = (ff.$ff_parent_or_root) parent

      -- Call the function f, passing it the filtered, disk and stored name objects
      make_names f name = f (packFilePathPacked3 (fiFilteredName parent)          packedFiltered  name lcext)
                            (packFilePathPacked3 (fiDiskName     parent)          packedDisk      name lcext)
                            (packFilePathPacked3 (fiStoredName   parent_or_root)  packedStored    name lcext)
                          where lcext  =  packext$ filenameLower$ getFileSuffix name

#if !defined(FREEARC_WIN) || defined(__MHS__)
  (dirList (diskDirName|||".")) .$handleFindErrors diskDirName  -- Get the list of files in the directory, handling directory read errors,
    >>== filter exclude_special_names                           -- Exclude "." and ".." from the list
    >>= (mapMaybeM $! make_names getFileInfo)                   -- Turn the file names into FileInfo structures and drop the files that `stat` choked on
#else
  withList $ \list -> do
    handleFindErrors diskDirName $ do
      wfindfiles (diskDirName </> reANY_FILE) $ \find -> do
        name <- w_find_name find
        when (exclude_special_names name) $ do
          fiAttr  <- w_find_attrib     find
          fiSize  <- w_find_size       find
          fiTime  <- w_find_time_write find
          fiIsDir <- w_find_isDir      find
          (list <<=) $! make_names FileInfo name fiSize fiTime fiAttr fiIsDir fiUndefinedGroup
#endif


-- |Add an exception handler invoked on errors while listing the files in a directory
handleFindErrors dir =
  handle $ \(e::IOError) -> do
    -- The error message is not printed for "/System Volume Information" directories
    d <- myCanonicalizePath dir
    unless (stripRoot d `strLowerEq` "System Volume Information") $ do
      registerWarning$ CANT_READ_DIRECTORY dir
    return defaultValue

-- |Build the list of files in `dir` satisfying `accept_f` and send the result to `process_f`.
-- If recursive==True, repeat these actions recursively in every subdirectory found
findFiles_FileInfo dir ff@FindFiles{ff_accept_f=accept_f, ff_process_f=process_f, ff_recursive=recursive} = do
  if recursive  then recursiveM processDir dir  else do processDir dir; return ()
    where processDir dir = do
            dirContents  <-  getDirectoryContents_FileInfo ff dir
            process_f `unlessNull` (filter accept_f dirContents)   -- Process the filtered files if the list is non-empty
            return                 (filter fiIsDir  dirContents)   -- Return the list of subdirectories for recursive processing

{-# NOINLINE getDirectoryContents_FileInfo #-}
{-# NOINLINE findFiles_FileInfo #-}


----------------------------------------------------------------------------------------------------
---- Finding and processing files that match the given criteria ------------------------------------
----------------------------------------------------------------------------------------------------

-- |Criteria for searching for files on disk
data FileFind = FileFind
    { ff_ep             :: !Int
    , ff_scan_subdirs   :: !Bool
    , ff_include_dirs   :: !(Maybe Bool)
    , ff_no_nst_filters :: !Bool
    , ff_filter_f       :: !(FileInfo -> Bool)
    , ff_group_f        :: !(Maybe (FileInfo -> FileGroup))
    , ff_arc_basedir    :: !String
    , ff_disk_basedir   :: !String
    }

-- |Find [recursively] all files matching the wildcard `filespec` and return their list
find_files scan_subdirs filespec  =  find_and_filter_files [filespec] doNothing $
    FileFind { ff_ep             = -1
             , ff_scan_subdirs   = scan_subdirs
             , ff_include_dirs   = Just False
             , ff_no_nst_filters = True
             , ff_filter_f       = const True
             , ff_group_f        = Nothing
             , ff_arc_basedir    = ""
             , ff_disk_basedir   = ""
             }

-- |Build the list of all files and subdirectories in a directory
dir_list directory  =  find_and_filter_files [directory </> reANY_FILE] doNothing $
    FileFind { ff_ep             = 0
             , ff_scan_subdirs   = False
             , ff_include_dirs   = Just True
             , ff_no_nst_filters = True
             , ff_filter_f       = const True
             , ff_group_f        = Nothing
             , ff_arc_basedir    = ""
             , ff_disk_basedir   = ""
             }


-- |Find all files satisfying the selection criteria `ff`,
-- and return their list
find_and_filter_files filespecs process_f ff = do
  concat ==<< withList (\list -> do  -- Concatenate the file lists found in each subdirectory
    find_filter_and_process_files filespecs ff $ \files -> do
      process_f files
      list <<= files)

-- |Find all files satisfying the selection criteria `ff`,
-- and send their list in chunks to the process's output channel
find_and_filter_files_PROCESS filespecs ff pipe = do
  find_filter_and_process_files filespecs ff (sendP pipe)
  sendP pipe []  -- the "that's all, folks!" signal :)


-- |Find [recursively] all files described by the wildcards `filespecs` and the selection criterion `filter_f`,
-- and apply the operation `process_f` to each list of files found in a single directory
find_filter_and_process_files filespecs ff@FileFind{ ff_ep=ep, ff_scan_subdirs=scan_subdirs, ff_include_dirs=include_dirs, ff_filter_f=filter_f, ff_group_f=group_f, ff_arc_basedir=arc_basedir, ff_disk_basedir=disk_basedir, ff_no_nst_filters=no_nst_filters} process_f

  -- Group the wildcards by directory name and process each of those groups separately
  = do curdir  <-  getCurrentDirectory >>== translatePath
{-
       -- Searching for files the way RAR does
       let doit f = do
             let re = isRegExp f
             isdir <- isDirExists f
             if not re && isdir  then findRecursively f  else do
             if not re && -r-    then getStat f `catch` "WARNING: file %s not found"
             else                     find (re || !-r-) f
-}
       -- Replace directory names dir with the two wildcards "dir dir/" to cover the directory itself and all files in it
       filespecs1 <- foreach filespecs $ \filespec -> do
         isDir <- case hasTrailingPathSeparator filespec of
                    True  -> return True
                    False -> dirExist filespec
         when isDir $ do
           find_files_in_one_dir curdir True [dropTrailingPathSeparator filespec]
         return$ (isDir &&& addTrailingPathSeparator) filespec
       --
       mapM_ (find_files_in_one_dir curdir False) $ sort_and_groupOn (filenameLower . takeDirectory) filespecs1 where

    -- Process a group of wildcards belonging to the same directory
    find_files_in_one_dir curdir addDir filespecs = do
      findFiles_FileInfo root FindFiles{ff_process_f=process_f . map_group_f, ff_recursive=recursive, ff_disk_eq_filtered=disk_eq_filtered, ff_stored_eq_filtered=stored_eq_filtered, ff_parent_or_root=parent_or_root, ff_accept_f=accept_f}

      where dirname  =  takeDirectory (head filespecs)  -- The directory common to all the wildcards
            masks    =  map takeFileName filespecs      -- The wildcards without that directory name
            root     =  createParentDirFileInfo         -- Base FileInfo for this search:
                            dirname                     --   base directory for filtering files
                            diskdir                     --   base directory on disk
                            arcdir                      --   base directory inside the archive

            -- Base directory on disk
            diskdir           =  disk_basedir </> dirname
            -- Do the file names on disk and on the command line coincide?
            disk_eq_filtered  =  diskdir==dirname
            -- Full path to the base directory on disk, for -ep2/-ep3
            full_dirname      =  curdir </> diskdir

            -- Base directory inside the archive
            arcdir  =  arc_basedir </> case ep of
               0 -> ""                        -- -ep:  exclude any paths from names
               1 -> ""                        -- -ep1: exclude base dir from names
               2 -> full_dirname.$stripRoot   -- -ep2: full absolute path without "d:\"
               3 -> full_dirname              -- -ep3: full absolute path with "d:\"
               _ -> dirname.$stripRoot        -- Default: full relative path
            -- Chooses the parent or root directory depending on the -ep option
            parent_or_root      =  if ep==0  then const root  else id
            -- Do the file names inside the archive and on the command line coincide?
            stored_eq_filtered  =  arcdir==dirname && ep/=0

            -- Is one of the names given as "dir/"?
            dir_slash    =  dirname>"" && masks `contains` ""
            -- Scan subdirectories if the "-r" option is given or one of the names is given as "dir/"
            recursive    =  scan_subdirs || dir_slash
            -- Include all files/directories in the list if one of the names is given as "dir/" or "*" or "dir/*"
            include_all  =  dir_slash || masks `contains` reANY_FILE
            -- Predicate determining which files and directories will be included in the list being built:
            --   for directories this depends on the --[no]dirs options, by default - provided "[dir/]* -r" || "dir/" and no file selection filters -n/-s../-t..
            --   for files, we check that they satisfy the `filter_f` predicate and one of the wildcards
            accept_f fi | fiIsDir fi  =  include_dirs `defaultVal` (addDir && baseName fi `elem` masks  ||  no_nst_filters && recursive && include_all)
                        | otherwise   =  filter_f fi && (include_all || match_filespecs fpBasename masks (fiFilteredName fi))
            -- Sets the fiGroup group numbers in [FileInfo] using the function passed in group_f
            map_group_f = case group_f of
                            Nothing -> id
                            Just f  -> map (\x -> x {fiGroup = f x})

{-# NOINLINE find_files #-}
{-# NOINLINE find_and_filter_files #-}
{-# NOINLINE find_and_filter_files_PROCESS #-}
{-# NOINLINE find_filter_and_process_files #-}
