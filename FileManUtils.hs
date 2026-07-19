----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: utility functions                                               ------
----------------------------------------------------------------------------------------------------
module FileManUtils where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Options
import UIBase
import UI
import ArhiveDirectory
import ArcExtract

----------------------------------------------------------------------------------------------------
---- Current file manager state --------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Current file manager state: the list of selected files, the overall file list and other information
data FM_State = FM_State { fm_window       :: Window
                         , fm_view         :: TreeView
                         , fm_model        :: New.ListStore FileData
                         , fm_selection    :: TreeSelection
                         , fm_statusLabel  :: Label
                         , fm_messageCombo :: (New.ComboBox, IORef Int)
                         , fm_filelist     :: [FileData]
                         , fm_history_file :: MVar FilePath
                         , fm_history      :: Maybe [String]
                         , fm_onChdir      :: [IO()]
                         , fm_sort_order   :: String
                         , subfm           :: SubFM_State
                         }

-- |Current file manager state: information about the displayed archive or disk directory
data SubFM_State = FM_Archive   { subfm_archive  :: ArchiveInfo
                                , subfm_arcname  :: FilePath
                                , subfm_arcdir   :: FilePath
                                , subfm_filetree :: FileTree FileData
                                }
                 | FM_Directory { subfm_dir      :: FilePath
                                }

-- |True if the FM is currently showing an archive
isFM_Archive (FM_State {subfm=FM_Archive{}}) = True
isFM_Archive _                               = False

fm_archive = subfm_archive.subfm
fm_arcname = subfm_arcname.subfm
fm_arcdir  = subfm_arcdir .subfm
fm_dir     = subfm_dir    .subfm

-- |The current archive + directory inside it, or a directory on disk
fm_current fm | isFM_Archive fm = fm_arcname fm </> fm_arcdir fm
              | otherwise       = fm_dir     fm

-- |The current directory shown in the FM, or the directory containing the current archive
fm_curdir fm | isFM_Archive fm = fm_arcname fm .$takeDirectory
             | otherwise       = fm_dir     fm

-- |Change the name of the archive opened in the FM
fm_changeArcname arcname fm@(FM_State {subfm=subfm@FM_Archive{}}) =
                         fm {subfm = subfm {subfm_arcname=arcname}}


----------------------------------------------------------------------------------------------------
---- Operations on directory/file names ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Current location in the file manager: a directory inside an archive or on disk
data PathInfo path  =  ArcPath path path | DiskPath path | Not_Exists  deriving (Eq,Show)

isArcPath ArcPath{} = True
isArcPath _         = False

-- |Parses the current location in the FM into a PathInfo structure
splitArcPath fm' fullname = do
  fm <- val fm'
  -- Compare fullname with the name of the archive opened in fm (arcname)
  -- If arcname is a prefix of fullname, split fullname into the archive name arcname and the directory inside it
  let arcname = isFM_Archive fm.$bool "!^%^@!%" (fm_arcname fm)
  if arcname `isParentDirOf` fullname
    then return$ ArcPath arcname (fullname `dropParentDir` arcname)
    else do
  -- Check whether a directory with such a name exists (or "", to avoid looping)
  d <- not(isURL fullname) &&& io(dirExist fullname)
  if d || fullname=="" then return$ DiskPath fullname
    else do
  -- Check whether a file with such a name exists
  f <- io(fileExist fullname)
  if f then return$ ArcPath fullname ""
    else do
  -- Repeat all the checks after cutting the last name component off fullname
  res <- splitArcPath fm' (takeDirectory fullname)
  -- If the result is a directory inside an archive, append the cut-off component to the directory name
  -- Otherwise the original fullname referred to a file that doesn't exist at all
  case res of
    ArcPath  dir name | isURL(takeDirectory fullname) == isURL fullname  -- Check that we didn't cut the URL down to the bone :D
                      -> return$ ArcPath dir (name </> takeFileName fullname)
    _                 -> return$ Not_Exists


-- |Convert a path written relative to the FM's current disk directory into an absolute one
fmCanonicalizeDiskPath fm' relname = do
  let name  =  unquote (trimRight relname)
  if (name=="")  then return ""  else do
  fm <- val fm'
  io$ myCanonicalizePath$ fm_curdir fm </> name

-- |Convert a path written relative to the current position in the FM into an absolute one
fmCanonicalizePath fm' relname = do
  fm <- val fm'
  case () of
   _ | isURL relname                              ->  return relname
     | isAbsolute relname                         ->  myCanonicalizePath relname
     | isURL (fm_current fm) || isFM_Archive fm   ->  return$ urlNormalize (fm_current fm) relname    -- Use our own Normalize for navigation inside archives and over URLs
     | otherwise                                  ->  myCanonicalizePath (fm_current fm </> relname)

-- |Normalize a path written relative to some URL
urlNormalize url relname =  dropTrailingPathSeparator$ concat$ reverse$ remove$ reverse$ splitPath (url++[pathSeparator]) ++ splitPath relname
  where remove (".":xs)    = remove xs
        remove ("./":xs)    = remove xs
        remove (".\\":xs)    = remove xs
        remove ("..":x:xs) = remove xs
        remove ("../":x:xs) = remove xs
        remove ("..\\":x:xs) = remove xs
        remove (x:xs)      = x : remove xs
        remove []          = []


----------------------------------------------------------------------------------------------------
---- FileData and FileTree -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Structure holding all the information we need about a file
data FileData = FileData
  { fdPackedDirectory       :: !MyPackedString   -- Directory name
  , fdPackedBasename        :: !MyPackedString   -- File name without the directory, but with the extension
  , fdSize  :: {-# UNPACK #-}  !FileSize         -- File size (0 for directories)
  , fdTime  :: {-# UNPACK #-}  !FileTime         -- File creation date/time
  , fdIsDir :: {-# UNPACK #-}  !Bool             -- Is this a directory?
  }

fiToFileData fi = FileData { fdPackedDirectory = fpPackedDirectory (fiStoredName fi)
                           , fdPackedBasename  = fpPackedBasename  (fiStoredName fi)
                           , fdSize            = fiSize  fi
                           , fdTime            = fiTime  fi
                           , fdIsDir           = fiIsDir fi }

fdDirectory  =  myUnpackStr.fdPackedDirectory
fdBasename   =  myUnpackStr.fdPackedBasename

-- |Virtual field: the full file name, including the directory and extension
fdFullname fd  =  fdDirectory fd </> fdBasename fd

-- |File name. Must be fdFullname to support the "flat listing" mode of archives/file trees
fmname = fdBasename

-- |Returns an artificial directory with the base name name
fdArtificialDir name = FileData { fdPackedDirectory = myPackStr ""
                                , fdPackedBasename  = myPackStr name
                                , fdSize            = 0
                                , fdTime            = aMINIMAL_POSSIBLE_DATETIME
                                , fdIsDir           = True }



-- |File tree. Contains the list of files at this level plus named subtrees
--                        files   dirname subtree
data FileTree a = FileTree [a]  [(String, FileTree a)]

-- |Returns the number of directories in the tree
ftDirs  (FileTree files subdirs) = length (removeDups (subdirs.$map fst  ++  files.$filter fdIsDir .$map fdBasename))
                                 + sum (map (ftDirs.snd) subdirs)

-- |Returns the number of files in the tree
ftFiles (FileTree files subdirs) = length (filter (not.fdIsDir) files)  +  sum (map (ftFiles.snd) subdirs)

-- |Returns the list of files in the given directory,
-- using the artificial mapping to generate pseudo-files from the names of nested directories
ftFilesIn dir artificial = f (splitDirectories dir)
 where
  f (path0:path_rest) (FileTree _     subdirs) = lookup path0 subdirs.$ maybe [] (f path_rest)
  f []                (FileTree files subdirs) = (files++map (artificial.fst) subdirs)
                                                  .$ keepOnlyFirstOn (filenameLower.fmname)

-- |Turns a list of files into a tree
buildTree x = x
  .$splitt 0                                  -- Split into groups by directory, starting from level 0
splitt n x = x
  .$sort_and_groupOn (dirPart n)              -- Sort/group by the directory name of the current level
  .$partition ((=="") . dirPart n.head)         -- Separate the group with the files located directly in this directory
  .$(\(root,other) -> FileTree (concat root)  -- The remaining groups are processed recursively at level (n+1)
                               (map2s (dirPart n.head, splitt (n+1)) other))

-- Name of the n-th directory component
dirPart n = (!!n).(++[""]) . splitDirectories.fdDirectory

io=id
