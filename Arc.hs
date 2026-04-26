{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Основной модуль программы.                                                                 ----
---- Вызывает parseCmdline из модуля Cmdline для разбора командной строки и выполняет каждую    ----
----   полученную команду.                                                                      ----
---- Если команда должна обработать несколько архивов, то findArchives дублирует её            ----
----   для каждого из них.                                                                      ----
---- Затем каждая команда сводится к выполнению одной из следующих задач:                       ----
---- * изменение архива  с помощью  runArchiveCreate   из модуля ArcCreate   (команды a/f/m/u/j/d/ch/c/k/rr)
---- * распаковка архива         -  runArchiveExtract  -         ArcExtract  (команды t/e/x)    ----
---- * получение листинга архива -  runArchiveList     -         ArcList     (команды l/v)      ----
---- * восстановление архива     -  runArchiveRecovery -         ArcRecover  (команда r)        ----
---- которым передаются аргументы в соответствии со спецификой конкретной выполняемой команды.  ----
----                                                                                            ----
---- Эти процедуры в свою очередь прямо или косвенно обращаются к модулям:                      ----
----   ArhiveFileList   - для работы со списками архивируемых файлов                            ----
----   ArhiveDirectory  - для чтения/записи оглавления архива                                   ----
----   ArhiveStructure  - для работы со структурой архива                                       ----
----   ByteStream       - для превращения каталога архива в последовательность байтов           ----
----   Compression      - для вызова алгоритмов упаковки, распаковки и вычисления CRC           ----
----   UI               - для информирования пользователя о ходе выполняемых работ :)           ----
----   Errors           - для сигнализации о возникших ошибках и записи в логфайл               ----
----   FileInfo         - для поиска файлов на диске и получения информации о них               ----
----   Files            - для всех операций с файлами на диске и именами файлов                 ----
----   Process          - для разделения алгоритма на параллельные взаимодействующие процессы   ----
----   Utils            - для всех остальных вспомогательных функций                            ----
----------------------------------------------------------------------------------------------------
module Arc where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
#ifdef __GLASGOW_HASKELL__
import GHC.Conc (setUncaughtExceptionHandler, getNumCapabilities, setNumCapabilities, getNumProcessors)
#endif
import Control.Monad
import Data.List
import System.Mem
import System.IO

import Utils
import Process
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UI
import ArcCreate
import ArcExtract
import ArcRecover
import Arc7z
#ifdef FREEARC_GUI
import FileManager
#endif

import Foreign.C.String (CString, withCString)
import Foreign.C.Types  (CInt(..))

foreign import ccall unsafe "darc_queue_acquire" c_queue_acquire :: CString -> IO CInt
foreign import ccall unsafe "darc_queue_release" c_queue_release :: CInt   -> IO ()


-- |Главная функция программы
main         =  (doMain =<< myGetArgs) >> shutdown "" aEXIT_CODE_SUCCESS
-- |Дублирующая главная функция для интерактивной отладки
arc cmdline  =  doMain (words cmdline)

-- |Превратить командную строку в набор команд и выполнить их
doMain args  = do
#ifdef FREEARC_GUI
  bg $ do                           -- выполняем в новом треде, не являющемся bound thread
#endif
#ifdef __GLASGOW_HASKELL__
  setUncaughtExceptionHandler handler
  nprocs <- getNumProcessors
  ncaps  <- getNumCapabilities
  when (ncaps < nprocs) $ setNumCapabilities nprocs
#endif
  setCtrlBreakHandler $ do          -- Организуем обработку ^Break
  ensureCtrlBreak "resetConsoleTitle" resetConsoleTitle $ do
  luaLevel "Program" [("command", args)] $ do
#ifdef FREEARC_GUI
  if length args < 2                -- При вызове программы без аргументов или с одним аргументом (именем каталога/архива)
    then myGUI run args             --   запускаем полноценный Archive Manager
    else do                         --   а иначе - просто отрабатываем команды (де)архивации
#endif
  uiStartProgram                    -- Открыть UI
  commands <- parseCmdline args     -- Превратить командную строку в список команд на выполнение
  -- FreeArc 0.67 --queue: serialize with other arc processes via advisory lockfile
  queue_fd <- if any opt_queue commands
                then withCString "/tmp/darc.queue.lock" c_queue_acquire
                else return (-1)
  mapM_ run commands                -- Выполнить каждую полученную команду
  when (queue_fd >= 0) $ c_queue_release queue_fd
  uiDoneProgram                     -- Закрыть UI

 where
   handler (ex :: SomeException)  =
#ifdef FREEARC_GUI
    mapM_ doNothing $
#else
    registerError$ GENERAL_ERROR$
#endif
      maybe (show ex) (\(ErrorCall s) -> s) (fromException ex) : []


-- |Диспетчеризует команду и организует её повторение для каждого подходящего архива
run command@Command
                { cmd_name            = cmd
                , cmd_setup_command   = setup_command
                , opt_scan_subdirs    = scan_subdirs
                } = do
  performGC       -- почистить мусор после обработки предыдущих команд
  setup_command   -- выполнить настройки, необходимые перед началом выполнения команды
  luaLevel "Command" [("command", cmd)] $ do
  -- Route .7z archives to the system 7zz/7z binary.
  if is7zArchive (cmd_arcspec command)
    then run7z command
    else case cmd of
      "create" -> findArchives  False           runAdd     command
      "a"      -> findArchives  False           runAdd     command
      "f"      -> findArchives  False           runAdd     command
      "m"      -> findArchives  False           runAdd     command
      "mf"     -> findArchives  False           runAdd     command
      "u"      -> findArchives  False           runAdd     command
      "j"      -> findArchives  False           runJoin    command
      "cw"     -> findArchives  False           runCw      command
      "ch"     -> findArchives  scan_subdirs    runCopy    command
      "modify" -> findArchives  scan_subdirs    runModify  command
      's':_    -> findArchives  scan_subdirs    runCopy    command
      "c"      -> findArchives  scan_subdirs    runCopy    command
      "k"      -> findArchives  scan_subdirs    runCopy    command
      'r':'r':_-> findArchives  scan_subdirs    runCopy    command
      "r"      -> findArchives  scan_subdirs    runRecover command
      "d"      -> findArchives  scan_subdirs    runDelete  command
      "e"      -> findArchives  scan_subdirs    runExtract command
      "x"      -> findArchives  scan_subdirs    runExtract command
      "t"      -> findArchives  scan_subdirs    runTest    command
      "l"      -> findArchives  scan_subdirs    runList    command
      "lb"     -> findArchives  scan_subdirs    runList    command
      "lt"     -> findArchives  scan_subdirs    runList    command
      "v"      -> findArchives  scan_subdirs    runList    command
      _ -> registerError$ UNKNOWN_CMD cmd aLL_COMMANDS


-- |Ищет архивы, подходящие под маску arcspec, и выполняет заданную команду на каждом из них
findArchives scan_subdirs   -- искать архивы и в подкаталогах?
              run_command    -- процедура, которую нужно запустить на каждом найденном архиве
              command@Command {cmd_arcspec = arcspec} = do
  uiStartCommand command   -- Отметим начало выполнения команды
  arclist <- if scan_subdirs || is_wildcard arcspec
               then find_files scan_subdirs arcspec >>== map diskName
               else return [arcspec]
  results <- foreach arclist $ \arcname -> do
    performGC   -- почистить мусор после обработки предыдущих архивов
    luaLevel "Archive" [("arcname", arcname)] $ do
    -- Если указана опция -ad, то добавить к базовому каталогу на диске имя архива (без расширения)
    let add_dir  =  opt_add_dir command  &&&  (</> takeBaseName arcname)
    run_command command { cmd_arcspec      = error "findArchives:cmd_arcspec undefined"  -- cmd_arcspec нам больше не понадобится.
                        , cmd_arclist      = arclist
                        , cmd_arcname      = arcname
                        , opt_disk_basedir = add_dir (opt_disk_basedir command)
                        }
  uiDoneCommand command results   -- доложить о результатах выполнения команды над всеми архивами


-- |Команды добавления в архив: create, a, f, m, u
runAdd cmd = do
  msg <- i18n"0246 Found %1 files"
  let diskfiles =  find_and_filter_files (cmd_filespecs cmd) (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = opt_include_dirs     cmd
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = addFileFilter      cmd
                                , ff_group_f        = opt_find_group       cmd.$Just
                                , ff_arc_basedir    = opt_arc_basedir      cmd
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_diskfiles      = diskfiles     -- файлы, которые нужно добавить с диска
                   , cmd_archive_filter = const True }  -- фильтр отбора файлов из открываемых архивов


-- |Команда слияния архивов: j
runJoin cmd@Command { cmd_filespecs = filespecs
                       , opt_noarcext  = noarcext
                       } = do
  msg <- i18n"0247 Found %1 archives"
  let arcspecs  =  map (addArcExtension noarcext) filespecs   -- добавим к именам расширение по умолчанию (".arc")
      arcnames  =  map diskName ==<< find_and_filter_files arcspecs (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = Just False
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = addFileFilter      cmd
                                , ff_group_f        = Nothing
                                , ff_arc_basedir    = ""
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_added_arcnames = arcnames      -- дополнительные входные архивы
                   , cmd_archive_filter = const True }  -- фильтр отбора файлов из открываемых архивов


-- |Команда модификации архива: принимает archivos de disco opcionales
-- como `runAdd`, pero si no hay filespecs simplemente re-encode el archivo existente.
runModify cmd | null (cmd_filespecs cmd) || cmd_filespecs cmd == aDEFAULT_FILESPECS
              = runArchiveAdd cmd{cmd_archive_filter = const True}
runModify cmd = runAdd cmd{cmd_archive_filter = const True}

-- |Команды копирования архива с внесением изменений: ch, c, k. s, rr
runCopy    = runArchiveAdd                    . setArcFilter fullFileFilter
-- |Команда удаления из архива: d
runDelete  = runArchiveAdd                    . setArcFilter ((not.) . fullFileFilter)
-- |Команды извлечения из архива: e, x
runExtract = runArchiveExtract pretestArchive . setArcFilter (test_dirs extractFileFilter)
-- |Команда тестирования архива: t
runTest    = runArchiveExtract pretestArchive . setArcFilter (test_dirs fullFileFilter)
-- |Команды получения листинга архива: l, v
runList    = runArchiveList pretestArchive    . setArcFilter (test_dirs fullFileFilter)
-- |Команда записи архивного комментария в файл: cw
runCw      = runCommentWrite
-- |Команда восстановления архива: r
runRecover = runArchiveRecovery

-- |Just shortcut
runArchiveAdd  =  runArchiveCreate pretestArchive writeRecoveryBlocks

{-# NOINLINE findArchives #-}
{-# NOINLINE runAdd #-}
{-# NOINLINE runModify #-}
{-# NOINLINE runJoin #-}
{-# NOINLINE runCopy #-}
{-# NOINLINE runDelete #-}
{-# NOINLINE runExtract #-}
{-# NOINLINE runTest #-}
{-# NOINLINE runList #-}


----------------------------------------------------------------------------------------------------
---- Критерии отбора файлов, подлежащих обработке, для различных типов команд ----------------------
----------------------------------------------------------------------------------------------------

-- |Установить в cmd предикат выбора из архива обрабатываемых файлов
setArcFilter filter cmd  =  cmd {cmd_archive_filter = filter cmd}

-- |Отобрать файлы в соответствии с фильтром opt_file_filter, за исключением
-- обрабатываемых этой командой архивов и временных файлов, создаваемых при архивации
addFileFilter cmd      =  all_functions [opt_file_filter cmd, not . overwriteF cmd]

-- |Отобрать файлы в соответствии с фильтром fullFileFilter, за исключением
-- обрабатываемых этой командой архивов и временных файлов, создаваемых при архивации
extractFileFilter cmd  =  all_functions [fullFileFilter cmd, not . overwriteF cmd]

-- |Отбирает среди файлов, маски которых указаны в командной строке,
-- соответствующие фильтру opt_file_filter
fullFileFilter cmd  =  all_functions
                           [  match_filespecs (opt_match_with cmd) (cmd_filespecs cmd) . fiFilteredName
                           ,  opt_file_filter cmd
                           ]

-- |Отбирает обрабатываемые архивы и временные файлы, создаваемые при архивации,
-- а также файлы, которые могут их перезаписать при распаковке
overwriteF cmd  =  in_arclist_or_temparc . fiDiskName
  where in_arclist_or_temparc filename =
            fpFullname filename `elem` cmd_arclist cmd
            || all_functions [(temparcPrefix `isPrefixOf`), (temparcSuffix `isSuffixOf`)]
                             (fpBasename filename)

-- |Добавить в фильтр отбора файлов `filter_f` отбор каталогов в соответствии с опциями команды `cmd`
test_dirs filter_f cmd fi  =  if fiIsDir fi
                                then opt_x_include_dirs cmd
                                else filter_f cmd fi

