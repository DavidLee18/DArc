{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Операции с именами файлов, манипуляции с файлами на диске, ввод/вывод.                     ----
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  Files
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Files (module Files, module FilePath) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Int
import Data.IORef
import Data.List
import Data.Word
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
#ifndef __MHS__
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr     (castPtr)
#else
import Foreign.C.Types (CSize(..))
import Foreign.Ptr     (castPtr)
foreign import ccall "memset" c_memset_files :: Ptr () -> Int -> CSize -> IO (Ptr ())
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes ptr val n = c_memset_files (castPtr ptr) (fromIntegral val) (fromIntegral n) >> return ()
#endif
#if defined(FREEARC_WIN)
import System.Posix.Internals hiding (CFilePath, FD, sEEK_SET, sEEK_CUR, sEEK_END)
#else
import System.Posix.Internals hiding (CFilePath)
#endif
import System.Posix.Types
import System.IO
import System.IO.Error hiding (catch)
import System.IO.Unsafe
import System.Locale
import System.Time
import System.Process
import System.Directory
#ifdef __MHS__
import System.IO.Internal  (BFILE, withHandleAny)
import System.IO.StringHandle (stringToHandle)
import Foreign.ForeignPtr  (ForeignPtr, withForeignPtr)
#endif

import Utils
import FilePath
#ifdef __MHS__
import System.Environment (lookupEnv)
getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
  mHome <- lookupEnv "HOME"
  let home = maybe "/root" id mHome
  return (home ++ "/." ++ appName)
#endif
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32
#else
import System.Posix.Files hiding (fileExist)
#endif

-- |Размер одного буфера, используемый в различных операциях
#ifdef __MHS__
-- MicroHs: use much larger buffers to reduce pipe iteration count.
-- Each pipe iteration costs ~0.45s in MHS combinator reduction,
-- so minimizing chunk count is critical for large file performance.
aBUFFER_SIZE = 8*mb
#else
aBUFFER_SIZE = 64*kb
#endif

-- |Количество байт, которые должны читаться/записываться за один раз в быстрых методах и при распаковке асимметричных алгоритмов
#ifdef __MHS__
aLARGE_BUFFER_SIZE = 64*mb
#else
aLARGE_BUFFER_SIZE = 256*kb
#endif

-- |Количество байт, которые должны читаться/записываться за один раз в очень быстрых методах (storing, tornado и тому подобное)
-- Этот объём минимизирует потери на disk seek operations - при условии, что одновременно не происходит в/в в другом потоке ;)
aHUGE_BUFFER_SIZE = 8*mb


----------------------------------------------------------------------------------------------------
---- Filename manipulations ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |True, если file находится в каталоге `dir`, одном из его подкаталогов, или совпадает с ним
dir `isParentDirOf` file =
  case (startFrom dir file) of
    Just ""    -> True
    Just (x:_) -> isPathSeparator x
    Nothing    -> False

-- |Имя файла за минусом каталога dir
file `dropParentDir` dir =
  case (startFrom dir file) of
    Just ""    -> ""
    Just (x:xs) | isPathSeparator x -> xs
    _          -> error "Utils::dropParentDir: dir isn't prefix of file"


#if defined(FREEARC_WIN)
-- |Для case-insensitive файловых систем
filenameLower = strLower
#else
-- |Для case-sensitive файловых систем
filenameLower = id
#endif

-- |Return False for special filenames like "." and ".." - used to filtering results of getDirContents
exclude_special_names s  =  (s/=".")  &&  (s/="..")

-- |Remove "." and ".." components from the path. Prevents path traversal
-- attacks (e.g. archive containing "../../etc/passwd") on extraction.
remove_unsafe_dirs :: FilePath -> FilePath
remove_unsafe_dirs path  =  path .$ splitDirectories .$ reverse .$ filter (/=".") .$ process .$ reverse .$ joinPath
  where
    process ("..":_:xs) = process xs
    process [".."]      = []
    process (x:xs)      = x : process xs
    process []          = []

-- Strip "drive:/" at the beginning of absolute filename
stripRoot = dropDrive

-- |Replace all '\' with '/'
translatePath = map (\c -> if isPathSeparator c  then '/'  else c)

-- |Filename extension, "dir/name.ext" -> "ext"
getFileSuffix = snd . splitFilenameSuffix

splitFilenameSuffix str  =  (name, drop 1 ext)
                               where (name, ext) = splitExtension str

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitDirFilename :: String -> (String,String)
splitDirFilename str  =  case splitFileName str of
                           x@([d,':',s], name) -> x   -- оставляем ("c:\", name)
                           (dir, name)         -> (dropTrailingPathSeparator dir, name)

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", "ext")
splitFilename3 :: String -> (String,String,String)
splitFilename3 str
   = let (dir, rest) = splitDirFilename str
         (name, ext) = splitFilenameSuffix rest
     in  (dir, name, ext)

-- | Modify the base name.
updateBaseName :: (String->String) -> FilePath -> FilePath
updateBaseName f pth  =  dir </> f name <.> ext
    where
          (dir, name, ext) = splitFilename3 pth


----------------------------------------------------------------------------------------------------
---- Поиск конфиг-файлов программы и SFX модулей ---------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Найти конфиг-файл с заданным именем или возвратить ""
findFile = findName fileExist
findDir  = findName dirExist
findName exist possibleFilePlaces cfgfilename = do
  found <- possibleFilePlaces cfgfilename >>= Utils.filterM exist
  case found of
    x:xs -> return x
    []   -> return ""

-- |Найти конфиг-файл с заданным именем или возвратить имя для создания нового файла
findOrCreateFile possibleFilePlaces cfgfilename = do
  variants <- possibleFilePlaces cfgfilename
  found    <- Utils.filterM fileExist variants
  case found of
    x:xs -> return x
    []   -> return (head variants)


#if defined(FREEARC_WIN)
-- Под Windows все дополнительные файлы по умолчанию лежат в одном каталоге с программой
libraryFilePlaces = configFilePlaces
configFilePlaces filename  =  do -- dir1 <- getAppUserDataDirectory "FreeArc"
                                 exe  <- getExeName
                                 return [-- dir1              </> filename,
                                         takeDirectory exe </> filename]

-- |Имя исполняемого файла программы
getExeName = do
  allocaBytes (long_path_size*4) $ \pOutPath -> do
    c_GetExeName pOutPath (fromIntegral long_path_size*2) >>= peekCWString

foreign import ccall unsafe "Environment.h GetExeName"
  c_GetExeName :: CWFilePath -> CInt -> IO CWFilePath

#else
-- |Места для поиска конфиг-файлов
configFilePlaces  filename  =  do
                                  dir1 <- getAppUserDataDirectory "FreeArc"
                                  return [dir1   </> filename
                                         ,"/etc/FreeArc" </> filename]

-- |Места для поиска sfx-модулей
libraryFilePlaces filename  =  return ["/usr/lib/FreeArc"       </> filename
                                      ,"/usr/local/lib/FreeArc" </> filename]
#endif


----------------------------------------------------------------------------------------------------
---- Запуск внешних программ и работа с Windows registry -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Запустить команду через shell и возвратить её stdout
#ifdef __MHS__
runProgram cmd = readProcess "/bin/sh" ["-c", cmd] ""
#else
runProgram cmd = do
    (_, stdout, stderr, ph) <- runInteractiveCommand cmd
    forkIO (hGetContents stderr >>= evaluate.length >> return ())
    result <- hGetContents stdout
    evaluate (length result)
    waitForProcess ph
    return result
#endif

-- |Execute file `filename` in the directory `curdir` optionally waiting until it finished
#ifdef __MHS__
runFile filename curdir wait_finish = do
  let cmd = "cd " ++ show curdir ++ " && ./" ++ filename
  rc <- system cmd
  return ()
#else
runFile filename curdir wait_finish = do
  let p = (proc ("./" ++ filename) []) { cwd = Just curdir }
  (_, _, _, ph) <- createProcess p
  when wait_finish $ waitForProcess ph >> return ()
#endif


#if defined(FREEARC_WIN)
-- |Создать HKEY и прочитать из Registry значение типа REG_SZ
registryGetStr root branch key =
  bracket (regCreateKey root branch) regCloseKey
    (\hk -> registryGetStringValue hk key)

-- |Создать HKEY и записать в Registry значение типа REG_SZ
registrySetStr root branch key val =
  bracket (regCreateKey root branch) regCloseKey
    (\hk -> registrySetStringValue hk key val)

-- |Прочитать из Registry значение типа REG_SZ
registryGetStringValue :: HKEY -> String -> IO (Maybe String)
registryGetStringValue hk key = do
#if __GLASGOW_HASKELL__ >= 900
  (regQueryValue hk (Just key) >>== Just)
#else
  (regQueryValue hk key >>== Just)
#endif
    `catch` (\(e::SomeException) -> return Nothing)

-- |Записать в Registry значение типа REG_SZ
registrySetStringValue :: HKEY -> String -> String -> IO ()
registrySetStringValue hk key val =
  withTString val $ \v ->
  regSetValueEx hk key rEG_SZ v (length val*2)
#endif


#if defined(FREEARC_WIN)
-- |OS-specific thread id
foreign import stdcall unsafe "windows.h GetCurrentThreadId"
  getOsThreadId :: IO DWORD
#else
-- |OS-specific thread id
foreign import ccall unsafe "pthread.h pthread_self"
  getOsThreadId :: IO Int
#endif


----------------------------------------------------------------------------------------------------
---- Операции с неоткрытыми файлами и каталогами ---------------------------------------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)
-- |Список дисков в системе с их типами
getDrives = getLogicalDrives >>== unfoldr (\n -> Just (n `mod` 2, n `div` 2))
                             >>== zipWith (\c n -> n>0 &&& [c:":"]) ['A'..'Z']
                             >>== concat
                             >>=  mapM (\d -> do t <- withCString d c_GetDriveType; return (d++"\t"++(driveTypes!!i t)))

driveTypes = ["", ""]++split ',' "Removable,Fixed,Network,CD/DVD,Ramdisk"

foreign import stdcall unsafe "windows.h GetDriveTypeA"
  c_GetDriveType :: LPCSTR -> IO CInt
#endif


-- |Create a hierarchy of directories
createDirectoryHierarchy :: FilePath -> IO ()
createDirectoryHierarchy dir = do
  let d = stripRoot dir
  when (d/= "" && exclude_special_names d) $ do
    unlessM (dirExist dir) $ do
      createDirectoryHierarchy (takeDirectory dir)
      dirCreate dir

-- |Создать недостающие каталоги на пути к файлу
buildPathTo filename  =  createDirectoryHierarchy (takeDirectory filename)

-- |Return current directory
getCurrentDirectory = myCanonicalizePath "."

-- | Given path referring to a file or directory, returns a
-- canonicalized path, with the intent that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path. Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fpath | isURL fpath = return fpath
                         | otherwise   =
#if defined(FREEARC_WIN)
  withCFilePath fpath $ \pInPath ->
  allocaBytes (long_path_size*4) $ \pOutPath ->
  alloca $ \ppFilePart ->
    do c_DArcGetFullPathName pInPath (fromIntegral long_path_size*2) pOutPath ppFilePart
       peekCFilePath pOutPath >>== dropTrailingPathSeparator

foreign import stdcall unsafe "GetFullPathNameW"
            c_DArcGetFullPathName :: CWString
                              -> CInt
                              -> CWString
                              -> Ptr CWString
                              -> IO CInt
#elif defined(__MHS__)
  withCString fpath $ \cs ->
    allocaBytes long_path_size $ \out -> do
      r <- darc_realpath cs out
      if r == 0
        then return (dropTrailingPathSeparator fpath)
        else peekCString out >>== dropTrailingPathSeparator
#else
  -- Use Haskell's canonicalizePath as a pure-Haskell replacement for C realpath
  fmap dropTrailingPathSeparator (canonicalizePath fpath)
#endif

-- |Максимальная длина имени файла (MY_FILENAME_MAX from Common.h is 4096)
long_path_size :: Int
long_path_size  =  4096


#if defined(FREEARC_WIN)
-- |Clear file's Archive bit
clearArchiveBit filename = do
    attr <- getFileAttributes filename
    when (attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0) $ do
        setFileAttributes filename (attr - fILE_ATTRIBUTE_ARCHIVE)
#else
clearArchiveBit _ = return ()
#endif


-- |Минимальное datetime, которое только может быть у файла. Соответствует 1 января 1970 г.
aMINIMAL_POSSIBLE_DATETIME = 0 :: CTime

-- |Get file's date/time
getFileDateTime filename  =  fileWithStatus "getFileDateTime" filename stat_mtime

-- |Set file's date/time
#if defined(FREEARC_WIN)
setFileDateTime filename datetime  =  withCFilePath filename (`c_SetFileDateTime` datetime)

foreign import ccall unsafe "Environment.h SetFileDateTime"
   c_SetFileDateTime :: CFilePath -> CTime -> IO ()
#else
setFileDateTime filename datetime = do
  status <- getFileStatus filename
  let atime = accessTime status
  setFileTimes filename atime datetime
#endif

-- |Пребразование CTime в ClockTime. Используется информация о внутреннем представлении ClockTime в GHC!!!
convert_CTime_to_ClockTime ctime = TOD (realToInteger ctime) 0
  where realToInteger = round . realToFrac :: Real a => a -> Integer

-- |Пребразование ClockTime в CTime
convert_ClockTime_to_CTime (TOD secs _) = i secs

-- |Текстовое представление времени
showtime format t = formatCalendarTime defaultTimeLocale format (unsafePerformIO (toCalendarTime t))

-- |Отформатировать CTime в строку с форматом "%Y-%m-%d %H:%M:%S"
formatDateTime t  =  unsafePerformIO $ do
  ct <- toCalendarTime (convert_CTime_to_ClockTime t)
  return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ct


#if defined(FREEARC_UNIX)
executeModes         =  [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
removeFileModes a b  =  a `intersectFileModes` (complement b)
#endif


----------------------------------------------------------------------------------------------------
---- Операции с открытыми файлами ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--withMVar  mvar action     =  bracket (takeMVar mvar) (putMVar mvar) action
liftMVar1  action mvar     =  withMVar mvar action
liftMVar2  action mvar x   =  withMVar mvar (\a -> action a x)
liftMVar3  action mvar x y =  withMVar mvar (\a -> action a x y)
returnMVar action          =  action >>= newMVar

-- |Архивный файл, заворачивается в MVar для реализации параллельного доступа из разных тредов ко входным архивам
data Archive = Archive { archiveName :: FilePath
                       , archiveFile :: MVar File
                       }
foreign import ccall unsafe "darc_volfile_open" c_volfile_open :: CString -> IO Int

archiveOpen     name = do
  let base | ".001" `isSuffixOf` name = take (length name - 4) name
           | otherwise                = name
  exists    <- fileExist base
  vol1Ex    <- fileExist (base ++ ".001")
  file <- if exists || not vol1Ex
            then fileOpen name
            else do slot <- withCString base c_volfile_open
                    if slot < 0 then fileOpen name
                                else return (VolFile slot)
  mv <- newMVar file
  let realname = if exists || not vol1Ex then name else base
  return (Archive realname mv)
archiveCreate   name = do file <- fileCreate name >>= newMVar; return (Archive name file)
archiveCreateRW name = do file <- fileCreateRW name >>= newMVar; return (Archive name file)
archiveGetPos        = liftMVar1 fileGetPos   . archiveFile
archiveGetSize       = liftMVar1 fileGetSize  . archiveFile
archiveSeek          = liftMVar2 fileSeek     . archiveFile
archiveRead          = liftMVar2 fileRead     . archiveFile
archiveReadBuf       = liftMVar3 fileReadBuf  . archiveFile
archiveWrite         = liftMVar2 fileWrite    . archiveFile
archiveWriteBuf      = liftMVar3 fileWriteBuf . archiveFile
archiveClose         = liftMVar1 fileClose    . archiveFile

-- |Скопировать данные из одного архива в другой и затем восстановить позицию в исходном архиве
archiveCopyData srcarc pos size dstarc = do
  withMVar (archiveFile srcarc) $ \srcfile ->
    withMVar (archiveFile dstarc) $ \dstfile -> do
      restorePos <- fileGetPos srcfile
      fileSeek      srcfile pos
      fileCopyBytes srcfile size dstfile
      fileSeek      srcfile restorePos

-- |При работе с одним физическим диском (наиболее частый вариант)
-- нет смысла выполнять несколько I/O операций параллельно,
-- поэтому мы их все проводим через "угольное ушко" одной-единственной MVar
oneIOAtTime = unsafePerformIO$ newMVar "oneIOAtTime value"
fileReadBuf  file buf size = withMVar oneIOAtTime $ \_ -> fileReadBufSimple file buf size
fileWriteBuf file buf size = withMVar oneIOAtTime $ \_ -> fileWriteBufSimple file buf size


----------------------------------------------------------------------------------------------------
---- URL access ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type VolHandle = Int
data File = FileOnDisk FileOnDisk | URL URL | VolFile VolHandle

fileOpen           = choose0 fOpen           url_open
fileCreate         = choose0 fCreate         (\_ -> err "url_create")
fileCreateRW       = choose0 fCreateRW       (\_ -> err "url_create_rw")
fileAppendText     = choose0 fAppendText     (\_ -> err "url_append_text")
fileGetPos         = choose3 fGetPos         (url_pos  .>>==i) (volfile_pos  .>>==i)
fileGetSize        = choose3 fGetSize        (url_size .>>==i) (volfile_size .>>==i)
fileSeek           = choose3 fSeek           (\f p -> url_seek f (i p)) (\f p -> volfile_seek f (i p))
fileReadBufSimple  = choose3 fReadBufSimple  url_read volfile_read
fileWriteBufSimple = choose3 fWriteBufSimple (\_ _ _ -> err "url_write")   (\_ _ _ -> err "volfile_write")
fileFlush          = choose3 fFlush          (\_     -> err "url_flush")   (\_     -> return ())
fileClose          = choose3 fClose          url_close volfile_close

-- |Проверяет существование файла/URL
fileExist name | isURL name = do url <- withCString name url_open
                                 url_close url
                                 return (url/=nullPtr)
               | otherwise  = fExist name

-- |Проверяет, является ли имя url
isURL name = "://" `isInfixOf` name

{-# NOINLINE choose0 #-}
choose0 onfile onurl name | isURL name = do url <- withCString name onurl
                                            when (url==nullPtr) $ do
                                              fail$ "Can't open url "++name   --registerError$ CANT_OPEN_FILE name
                                            return (URL url)
                          | otherwise  = onfile name >>== FileOnDisk

choose _ onurl  (URL        url)   = onurl  url
choose onfile _ (FileOnDisk file)  = onfile file
choose _ _      (VolFile    _)     = err "volfile op via choose"

choose3 _ onurl _      (URL     url)    = onurl  url
choose3 onfile _ _     (FileOnDisk f)   = onfile f
choose3 _ _ onvol      (VolFile v)      = onvol  v

foreign import ccall safe "darc_volfile_pos_out"  c_volfile_pos_out  :: VolHandle -> Ptr Int64 -> IO ()
foreign import ccall safe "darc_volfile_size_out" c_volfile_size_out :: VolHandle -> Ptr Int64 -> IO ()
foreign import ccall safe "darc_volfile_seek"     volfile_seek       :: VolHandle -> Int -> IO ()
foreign import ccall safe "darc_volfile_read"     volfile_read_raw   :: VolHandle -> Ptr () -> Int -> IO Int
foreign import ccall safe "darc_volfile_close"    volfile_close      :: VolHandle -> IO ()
volfile_read :: VolHandle -> Ptr a -> Int -> IO Int
volfile_read h buf n = volfile_read_raw h (castPtr buf) n
volfile_size :: VolHandle -> IO Int
volfile_size h = alloca $ \p -> c_volfile_size_out h p >> peek p >>= return . fromIntegral
volfile_pos :: VolHandle -> IO Int
volfile_pos h = alloca $ \p -> c_volfile_pos_out h p >> peek p >>= return . fromIntegral

{-# NOINLINE err #-}
err s  =  fail$ s++" isn't implemented"    --registerError$ GENERAL_ERROR ["0343 %1 isn't implemented", s]


type URL = Ptr ()
foreign import ccall safe "URL.h url_setup_proxy"         url_setup_proxy         :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h url_setup_bypass_list"   url_setup_bypass_list   :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h url_open"   url_open   :: Ptr CChar -> IO URL
#ifndef __MHS__
foreign import ccall safe "URL.h url_pos"    url_pos    :: URL -> IO Int64
foreign import ccall safe "URL.h url_size"   url_size   :: URL -> IO Int64
foreign import ccall safe "URL.h url_seek"   url_seek   :: URL -> Int64 -> IO ()
#else
-- MicroHs doesn't support Int64 in FFI; use Int (= 64-bit on 64-bit platforms)
foreign import ccall safe "URL.h url_pos"    url_pos    :: URL -> IO Int
foreign import ccall safe "URL.h url_size"   url_size   :: URL -> IO Int
foreign import ccall safe "URL.h url_seek"   url_seek   :: URL -> Int -> IO ()
#endif
#ifndef __MHS__
foreign import ccall safe "URL.h url_read"   url_read   :: URL -> Ptr a -> Int -> IO Int
#else
foreign import ccall safe "URL.h url_read"   url_read_raw :: URL -> Ptr () -> Int -> IO Int
url_read :: URL -> Ptr a -> Int -> IO Int
url_read url buf n = url_read_raw url (castPtr buf) n
#endif
foreign import ccall safe "URL.h url_close"  url_close  :: URL -> IO ()


----------------------------------------------------------------------------------------------------
---- Под Windows мне пришлось реализовать библиотеку в/в самому для поддержки файлов >4Gb и Unicode имён файлов
----------------------------------------------------------------------------------------------------
#if defined(FREEARC_WIN)

type FileOnDisk      = FD
type CFilePath       = CWFilePath
type FileAttributes  = FileAttributeOrFlag
withCFilePath        = withCWFilePath
peekCFilePath        = peekCWString
fOpen       name     = wopen name (read_flags  .|. o_BINARY) 0o666
fCreate     name     = wopen name (write_flags .|. o_BINARY .|. o_TRUNC) 0o666
fCreateRW   name     = wopen name (rw_flags    .|. o_BINARY .|. o_TRUNC) 0o666
fAppendText name     = wopen name (append_flags) 0o666
fGetPos              = wtell
fGetSize             = wfilelength
fSeek   file pos     = wseek file pos sEEK_SET
fReadBufSimple       = wread
fWriteBufSimple      = wwrite
fFlush  file         = return ()
fClose               = wclose
fExist               = wDoesFileExist
fileRemove           = wunlink
fileRename           = wrename
fileWithStatus       = wWithFileStatus
fileStdin            = 0
stat_mode            = wst_mode
stat_size            = wst_size
stat_mtime           = wst_mtime
dirCreate            = wmkdir
dirExist             = wDoesDirectoryExist
dirRemove            = wrmdir
dirList dir          = dirWildcardList (dir </> "*")
dirWildcardList wc   = withList $ \list -> do
                         wfindfiles wc $ \find -> do
                           name <- w_find_name find
                           list <<= name

#else

type FileOnDisk      = Handle
type CFilePath       = CString
type FileAttributes  = Int
withCFilePath s a    = (`withCString` a) =<< str2filesystem s
peekCFilePath ptr    = peekCString ptr >>= filesystem2str
fOpen                = (`openBinaryFile` ReadMode     ) =<<. str2filesystem
fCreate              = (`openBinaryFile` WriteMode    ) =<<. str2filesystem
fCreateRW            = (`openBinaryFile` ReadWriteMode) =<<. str2filesystem
fAppendText          = (`openFile`       AppendMode   ) =<<. str2filesystem
fGetPos              = hTell
fGetSize             = hFileSize
fSeek                = (`hSeek` AbsoluteSeek)
#ifndef __MHS__
fReadBufSimple       = hGetBuf
fWriteBufSimple      = hPutBuf
#endif
fFlush               = hFlush
fClose               = hClose
fExist               = doesFileExist =<<. str2filesystem
fileGetStatus        = getFileStatus =<<. str2filesystem
fileSetMode name mode= (`setFileMode` mode) =<< str2filesystem name
fileRemove name      = removeFile    =<<  str2filesystem name
fileRename a b       = do a1 <- str2filesystem a; b1 <- str2filesystem b; renameFile a1 b1
#ifdef __MHS__
renameFile :: FilePath -> FilePath -> IO ()
renameFile old new = withCString old $ \cs1 -> withCString new $ \cs2 -> do
  r <- darc_rename cs1 cs2
  if r /= 0 then ioError (userError ("renameFile: " ++ old)) else return ()
foreign import ccall "darc_realpath" darc_realpath :: CString -> CString -> IO Int
foreign import ccall "rename" darc_rename :: CString -> CString -> IO Int
removeDirectory :: FilePath -> IO ()
removeDirectory path = withCString path $ \cs -> do
  r <- darc_rmdir cs
  if r /= 0 then ioError (userError ("removeDirectory: " ++ path)) else return ()
foreign import ccall "rmdir" darc_rmdir :: CString -> IO Int
#endif
fileSetSize          = hSetFileSize
fileStdin            = stdin

#ifdef __MHS__
foreign import ccall "darc_bfile_seek"      darc_bfile_seek      :: Ptr () -> CLong -> Int -> IO Int
foreign import ccall "darc_bfile_truncate"  darc_bfile_truncate  :: Ptr () -> CLong -> IO Int
-- MicroHs truncates FFI return values to 32 bits, so use _w variants
-- that write 64-bit results via pointer instead of returning them.
foreign import ccall "darc_bfile_tell_w"    darc_bfile_tell_w    :: Ptr () -> Ptr CLong -> IO ()
foreign import ccall "darc_bfile_size_w"    darc_bfile_size_w    :: Ptr () -> Ptr CLong -> IO ()
foreign import ccall "darc_bfile_read_w"    darc_bfile_read_w    :: Ptr () -> Ptr () -> CLong -> Ptr CLong -> IO ()
foreign import ccall "darc_bfile_write_w"   darc_bfile_write_w   :: Ptr () -> Ptr () -> CLong -> Ptr CLong -> IO ()

fReadBufSimple :: Handle -> Ptr a -> Int -> IO Int
fReadBufSimple h buf size = withHandleAny h $ \bf ->
  alloca $ \pOut -> do
    darc_bfile_read_w (castPtr bf) (castPtr buf) (fromIntegral size) pOut
    fmap fromIntegral (peek pOut)

fWriteBufSimple :: Handle -> Ptr a -> Int -> IO ()
fWriteBufSimple h buf size = withHandleAny h $ \bf ->
  alloca $ \pOut -> do
    darc_bfile_write_w (castPtr bf) (castPtr buf) (fromIntegral size) pOut
    return ()

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h mode pos = withHandleAny h $ \bf -> do
  let whence = case mode of
                 AbsoluteSeek -> 0
                 RelativeSeek -> 1
                 SeekFromEnd  -> 2
  _ <- darc_bfile_seek (castPtr bf) (fromIntegral pos) whence
  return ()

hTell :: Handle -> IO Integer
hTell h = withHandleAny h $ \bf ->
  alloca $ \pOut -> do
    darc_bfile_tell_w (castPtr bf) pOut
    fmap fromIntegral (peek pOut)

hFileSize :: Handle -> IO Integer
hFileSize h = withHandleAny h $ \bf ->
  alloca $ \pOut -> do
    darc_bfile_size_w (castPtr bf) pOut
    fmap fromIntegral (peek pOut)

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize h sz = withHandleAny h $ \bf -> do
  _ <- darc_bfile_truncate (castPtr bf) (fromIntegral sz)
  return ()

-- |Get raw BFILE* pointer from an Archive, holding the MVar lock during the action
withArchiveBFILE :: Archive -> (Ptr () -> IO a) -> IO a
withArchiveBFILE arc action = withMVar (archiveFile arc) $ \file -> case file of
    FileOnDisk h -> withHandleAny h $ \bf -> action (castPtr bf)
    _            -> error "withArchiveBFILE: not a disk file"
#endif
stat_mode            = st_mode
stat_size            = st_size  .>>== i
stat_mtime           = st_mtime
dirCreate            = createDirectory     =<<. str2filesystem
dirExist             = doesDirectoryExist  =<<. str2filesystem
dirRemove            = removeDirectory     =<<. str2filesystem
dirList dir          = str2filesystem dir >>= getDirectoryContents >>= mapM filesystem2str
dirWildcardList wc   = dirList (takeDirectory wc)  >>==  filter (match$ takeFileName wc)

-- kidnapped from System.Directory :)))
fileWithStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
fileWithStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
	f p

#endif

fileRead      file size = allocaBytes size $ \buf -> do fileReadBuf file buf size; peekCStringLen (castPtr buf,size)
fileWrite     file str  = withCStringLen str $ \(buf,size) -> fileWriteBuf file (castPtr buf) size
fileGetBinary name      = bracket (fileOpen   name) fileClose (\file -> fileGetSize file >>= fileRead file . i)
filePutBinary name str  = bracket (fileCreate name) fileClose (`fileWrite` str)

-- |Скопировать заданное количество байт из одного открытого файла в другой
fileCopyBytes srcfile size dstfile = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do        -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    doChunks size aHUGE_BUFFER_SIZE $ \bytes -> do  -- Скопировать size байт кусками по aHUGE_BUFFER_SIZE
      bytes <- fileReadBuf srcfile buf bytes        -- Проверим, что прочитано ровно столько байт, сколько затребовано
      fileWriteBuf dstfile buf bytes

-- |True, если существует файл или каталог с заданным именем
fileOrDirExist f  =  mapM ($f) [fileExist, dirExist] >>== or


---------------------------------------------------------------------------------------------------
---- Глобальные настройки перекодировки для использования в глубоко вложенных функциях ------------
---------------------------------------------------------------------------------------------------

-- |Translate filename from filesystem to internal encoding
filesystem2str'   = unsafePerformIO$ newIORef$ id   -- 'id' means that inifiles can't have non-English names
filesystem2str s  = val filesystem2str' >>== ($s)
-- |Translate filename from internal to filesystem encoding
str2filesystem'   = unsafePerformIO$ newIORef$ id
str2filesystem s  = val str2filesystem' >>== ($s)


---------------------------------------------------------------------------------------------------
---- Utility functions ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Fill memory block with a byte value.
-- Pure Haskell replacement for C memset using Foreign.Marshal.Utils.fillBytes.
memset :: Ptr a -> Int -> CSize -> IO ()
memset ptr val size = fillBytes ptr (fromIntegral val :: Word8) (fromIntegral size :: Int)

-- |XOR two memory blocks: dest[i] ^= src[i] for i in [0..size-1]
-- Pure Haskell replacement for the C memxor from Environment.cpp.
-- Processes 8 bytes at a time using Word64 for performance, then handles any remainder byte-by-byte.
memxor :: Ptr a -> Ptr a -> Int -> IO ()
memxor dest src size = do
  let w64s = size `quot` 8
      rem8 = size `rem`  8
  goW64 0 w64s
  goBytes (w64s * 8) rem8
  where
    goW64 !k !lim
      | k >= lim  = return ()
      | otherwise = do
          let off = k * 8
          s <- peek (castPtr src  `plusPtr` off :: Ptr Word64)
          d <- peek (castPtr dest `plusPtr` off :: Ptr Word64)
          poke (castPtr dest `plusPtr` off :: Ptr Word64) (d `xor` s)
          goW64 (k + 1) lim
    goBytes !off !n
      | n <= 0    = return ()
      | otherwise = do
          s <- peek (castPtr src  `plusPtr` off :: Ptr Word8)
          d <- peek (castPtr dest `plusPtr` off :: Ptr Word8)
          poke (castPtr dest `plusPtr` off :: Ptr Word8) (d `xor` s)
          goBytes (off + 1) (n - 1)

