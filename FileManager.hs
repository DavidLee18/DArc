{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager                                                                  ------
----------------------------------------------------------------------------------------------------
module FileManager where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Numeric hiding (readInt)
import System.IO.Unsafe
import System.Cmd
import System.Process
#if defined(FREEARC_WIN)
import System.Win32
import Foreign.Ptr
#endif

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets            (i18n, i18ns)
import Compression
import Encryption
import Options
import Cmdline
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import FileManPanel
import FileManUtils
import FileManDialogs
import FileManDialogAdd

----------------------------------------------------------------------------------------------------
---- Main program menu and the toolbar below it ----------------------------------------------------
----------------------------------------------------------------------------------------------------
--      File: New Archive, Open Archive, New SFX, Change Drive, Select All, Select Group, Deselect Group, Invert Selection
--      Commands (or Actions): Add, Extract, Test, ArcInfo, View, Delete, Rename
--      Tools: Wizard (if there ever is one), Protect, Comment, Convert to EXE, Encrypt, Add Recovery record, Repair
--      Options: Configuration, Save settings, Load settings, View log, Clear log
--      Help: Help itself, Goto Homepage (and/or Check for update), About

uiDef =
  "<ui>"++
  "  <menubar>"++
  "    <menu name=\"File\"     action=\"FileAction\">"++
  "      <menuitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Select all\"         action=\"SelectAllAction\" />"++
  "      <menuitem name=\"Select\"             action=\"SelectAction\" />"++
  "      <menuitem name=\"Unselect\"           action=\"UnselectAction\" />"++
  "      <menuitem name=\"Invert selection\"   action=\"InvertSelectionAction\" />"++
  "      <menuitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "      <separator/>"++
  "      <placeholder name=\"FileMenuAdditions\" />"++
  "      <menuitem name=\"Exit\"               action=\"ExitAction\"/>"++
  "    </menu>"++
  "    <menu name=\"Commands\" action=\"CommandsAction\">"++
  "      <menuitem name=\"Add\"                action=\"AddAction\" />"++
  "      <menuitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <menuitem name=\"Test\"               action=\"TestAction\" />"++
  "      <menuitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <menuitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "    </menu>"++
  "    <menu name=\"Tools\"    action=\"ToolsAction\">"++
  "      <menuitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <menuitem name=\"Comment\"            action=\"CommentAction\" />"++
  "      <menuitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <menuitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Encrypt\"            action=\"EncryptAction\" />"++
  "      <menuitem name=\"Protect\"            action=\"ProtectAction\" />"++
  "      <menuitem name=\"Repair\"             action=\"RepairAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"Modify\"             action=\"ModifyAction\" />"++
  "      <menuitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "    </menu>"++
  "    <menu name=\"Options\"  action=\"OptionsAction\">"++
  "      <menuitem name=\"Settings\"           action=\"SettingsAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"ViewLog\"            action=\"ViewLogAction\" />"++
  "      <menuitem name=\"ClearLog\"           action=\"ClearLogAction\" />"++
  "    </menu>"++
  "    <menu name=\"Help\"     action=\"HelpAction\">"++
  "      <menuitem name=\"MainHelp\"           action=\"MainHelpAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"CmdlineHelp\"        action=\"CmdlineHelpAction\" />"++
  "      <menuitem name=\"OpenHomepage\"       action=\"OpenHomepageAction\" />"++
  "      <menuitem name=\"OpenForum\"          action=\"OpenForumAction\" />"++
  "      <menuitem name=\"OpenWiki\"           action=\"OpenWikiAction\" />"++
  "      <menuitem name=\"CheckForUpdate\"     action=\"CheckForUpdateAction\" />"++
  "      <separator/>"++
  "      <menuitem name=\"About\"              action=\"AboutAction\" />"++
  "    </menu>"++
  "  </menubar>"++
  "  <toolbar>"++
  "    <placeholder name=\"FileToolItems\">"++
  "      <toolitem name=\"OpenArchive\"        action=\"OpenArchiveAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Add\"                action=\"AddAction\" />"++
  "      <toolitem name=\"Extract\"            action=\"ExtractAction\" />"++
  "      <toolitem name=\"Test\"               action=\"TestAction\" />"++
  "      <toolitem name=\"ArcInfo\"            action=\"ArcInfoAction\" />"++
  "      <toolitem name=\"Delete\"             action=\"DeleteAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Lock\"               action=\"LockAction\" />"++
  "      <toolitem name=\"Recompress\"         action=\"RecompressAction\" />"++
  "      <toolitem name=\"Convert to SFX\"     action=\"ConvertToSFXAction\" />"++
  "      <toolitem name=\"Join archives\"      action=\"JoinArchivesAction\" />"++
  "      <separator/>"++
  "      <toolitem name=\"Refresh\"            action=\"RefreshAction\" />"++
  "    </placeholder>"++
  "  </toolbar>"++
  "</ui>"


----------------------------------------------------------------------------------------------------
---- File manager visual layer ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

myGUI run args = do
  fileManagerMode =: True
  startGUI $ do
  io$ parseCmdline ["l", "a"]   -- initialization: display, log file
  -- List of key->action associations
  onKeyActions <- newList
  let onKey = curry (onKeyActions <<=)
  -- Create the progress indicator window and load settings/localization
  (windowProgress, clearStats) <- runIndicators
  -- Main menu
  standardGroup <- actionGroupNew "standard"
  let action name  =  (concat$ map (mapHead toUpper)$ words$ drop 5 name)++"Action"   -- "9999 the name" -> "TheNameAction"
  let names = split ',' "0050 File,0258 Commands,0259 Tools,0260 Options,0261 Help"
  for names $ \name -> do
    label <- i18n name
    actionGroupAddAction standardGroup  =<<  actionNew (action name) label Nothing Nothing
  -- Menus and toolbars
  let anew name comment icon accel = do
        [i18name,i18comment] <- i18ns [name,comment]
        action <- actionNew (action name) i18comment (Just i18comment) icon
        action `set` [actionShortLabel := i18name]
        actionGroupAddActionWithAccel standardGroup action (Just accel)
        accel `onKey` actionActivate action
        return action
  --
  openAct     <- anew "0262 Open archive"     "0265 Open archive"                       (Just stockOpen)            "<Alt>O"
  selectAllAct<- anew "0263 Select all"       "0290 Select all files"                   (Just stockSelectAll)       "<Ctrl>A"
  selectAct   <- anew "0037 Select"           "0047 Select files"                       (Just stockAdd)             "KP_Add"
  unselectAct <- anew "0038 Unselect"         "0048 Unselect files"                     (Just stockRemove)          "KP_Subtract"
  invertSelAct<- anew "0264 Invert selection" "0291 Invert selection"                   (Nothing)                   "KP_Multiply"
  refreshAct  <- anew "0039 Refresh"          "0049 Reread archive/directory"           (Just stockRefresh)         "F5"
  exitAct     <- anew "0036 Exit"             "0046 Quit application"                   (Just stockQuit)            "<Alt>Q"

  addAct      <- anew "0030 Add"              "0040 Add files to archive(s)"            (Just stockMediaRecord)     "<Alt>A"
  extractAct  <- anew "0035 Extract"          "0045 Extract files from archive(s)"      (Just stockMediaPlay)       "<Alt>E"
  testAct     <- anew "0034 Test"             "0044 Test files in archive(s)"           (Just stockSpellCheck)      "<Alt>T"
  arcinfoAct  <- anew "0086 ArcInfo"          "0087 Information about archive"          (Just stockInfo)            "<Alt>I"
  deleteAct   <- anew "0033 Delete"           "0043 Delete files (from archive)"        (Just stockDelete)          "Delete"

  lockAct     <- anew "0266 Lock"             "0267 Lock archive from further changes"  (Just stockDialogAuthentication) "<Alt>L"
  commentAct  <- anew "0268 Comment"          "0269 Edit archive comment"               (Just stockEdit)            "<Alt>C"
  recompressAct<-anew "0293 Recompress"       "0294 Recompress files in archive"        (Just stockGotoBottom)      "<Alt>R"
  toSfxAct    <- anew "0270 Convert to SFX"   "0271 Convert archive to SFX"             (Just stockConvert)         "<Alt>S"
  encryptAct  <- anew "0272 Encrypt"          "0273 Encrypt archive contents"           (Nothing)                   ""
  addRrAct    <- anew "0274 Protect"          "0275 Add Recovery record to archive"     (Nothing)                   "<Alt>P"
  repairAct   <- anew "0379 Repair"           "0380 Repair damaged archive"             (Nothing)                   ""
  modifyAct   <- anew "0031 Modify"           "0041 Modify archive(s)"                  (Just stockEdit)            "<Alt>M"
  joinAct     <- anew "0032 Join archives"    "0042 Join archives together"             (Just stockCopy)            "<Alt>J"

  settingsAct <- anew "0064 Settings"         "0065 Edit program settings"              (Just stockPreferences)     ""
  viewLogAct  <- anew "0276 View log"         "0277 View logfile"                       (Nothing)                   ""
  clearLogAct <- anew "0278 Clear log"        "0279 Clear logfile"                      (Nothing)                   ""

  helpAct     <- anew "0280 Main help"        "0281 Help on using FreeArc"              (Just stockHelp)            "F1"
  helpCmdAct  <- anew "0282 Cmdline help"     "0283 Help on FreeArc command line"       (Just stockHelp)            ""
  homepageAct <- anew "0284 Open Homepage"    "0285 Open program site"                  (Just stockHome)            ""
  openForumAct<- anew "0373 Open forum"       "0374 Open program forum"                 (Nothing)                   ""
  openWikiAct <- anew "0375 Open wiki"        "0376 Open program wiki"                  (Nothing)                   ""
  whatsnewAct <- anew "0286 Check for update" "0287 Check for new program version"      (Just stockDialogInfo)      ""
  aboutAct    <- anew "0288 About"            "0289 About"                              (Just stockAbout)           ""

  menufile <- findFile configFilePlaces aMENU_FILE
  uiData   <- if menufile>""  then fileGetBinary menufile  else return uiDef

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiData
  uiManagerInsertActionGroup ui standardGroup 0

  window <- windowNew
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
  (Just toolBar) <- uiManagerGetWidget ui "/ui/toolbar"

  (listUI, listView, listModel, listSelection, columns, onColumnTitleClicked) <- createFilePanel
  statusLabel  <- labelNew Nothing
  miscSetAlignment statusLabel 0 0.5
  messageCombo <- New.comboBoxNewText
  statusbar    <- statusbarNew
  ctx <- statusbarGetContextId statusbar ""
  statusbarPush statusbar ctx "    "
  widgetSetSizeRequest messageCombo 30 (-1)
  lowBox <- vBoxNew False 0
  boxPackStart lowBox statusLabel  PackNatural 2
  boxPackStart lowBox messageCombo PackGrow    2
  --boxPackStart lowBox statusbar    PackNatural 0

  -- Create a variable holding the current file manager state
  fm' <- newFM window listView listModel listSelection statusLabel messageCombo
  fmStackMsg fm' "              "

  -- Fine-tune the toolbar
  let toolbar = castToToolbar toolBar
  toolbarCaptions <- fmGetHistoryBool fm' "ToolbarCaptions" True
  toolbar `set` [toolbarStyle := if toolbarCaptions then ToolbarBoth else ToolbarIcons]
  toolbar `toolbarSetIconSize` iconSizeLargeToolbar
  n <- toolbarGetNItems toolbar
  for [0..n-1] $ \i -> do
    Just button <- toolbarGetNthItem toolbar i
    toolItemSetHomogeneous button False

  -- Navigation bar
  naviBar  <- hBoxNew False 0
  upButton <- button "0006   Up  "
  curdir   <- fmEntryWithHistory fm' "dir/arcname" (const$ return True) (fmCanonicalizePath fm')
  saveDirButton <- button "0007   Save  "
  boxPackStart naviBar (widget upButton)       PackNatural 0
#if defined(FREEARC_WIN)
  -- Drive selection menu
  driveButton <- button "C:"
  driveMenu   <- makePopupMenu (chdir fm'.(++"\\") . head.words) =<< getDrives
  driveButton `onClick` (widgetShowAll driveMenu >> menuPopup driveMenu Nothing)
  boxPackStart naviBar (widget driveButton)    PackNatural 0
  -- Update the drive button caption when switching to another drive
  fm' `fmOnChdir` do
    fm <- val fm'
    let drive = takeDrive (fm_current fm)
    setTitle driveButton (take 2 drive)  `on` drive
#endif
  boxPackStart naviBar (widget curdir)         PackGrow    0
  boxPackStart naviBar (widget saveDirButton)  PackNatural 0

  -- The file manager window as a whole
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  boxPackStart vBox menuBar   PackNatural 0
  boxPackStart vBox toolBar   PackNatural 0
  boxPackStart vBox naviBar   PackNatural 0
  boxPackStart vBox listUI    PackGrow    0
  boxPackStart vBox lowBox    PackNatural 0

  containerAdd window vBox


  -- List of actions performed when the file manager window is closed
  onExit <- newList
  window `onDestroy` do
    sequence_ =<< listVal onExit
    mainQuit

  -- List of key->action associations
  listView `onKeyPress` \event -> do
    x <- lookup (eventKey event) `fmap` listVal onKeyActions
    case x of
      Just action -> do action; return True
      Nothing     -> return False


----------------------------------------------------------------------------------------------------
---- Saving/restoring the size and position of the main window and its columns ---------------------
----------------------------------------------------------------------------------------------------

  window `windowSetPosition` WinPosCenter
  --windowSetGeometryHints window (Just window) (Just (1,1)) (Just (32000,32000)) Nothing Nothing Nothing
  --widgetSetSizeRequest window 700 500
  --window `windowSetGravity` GravityStatic
  --window `windowSetPosition` WinPosNone
  --windowSetDefaultSize window 200 100

  -- At startup, restore the saved window size
  restoreSizePos fm' window "MainWindow" "-10000 -10000 720 500"

  -- Save the main window size and position after it has been moved
  window `onConfigure` \e -> do
    saveSizePos fm' window "MainWindow"
    return False

  -- Remember whether the window was maximized
  window `onWindowState` \e -> do
    let isMax x = case x of
                    WindowStateMaximized -> True
                    _                    -> False
    saveMaximized fm' "MainWindow" (any isMax (eventWindowState e))
    return False


  -- When the program closes, save the column order and widths
  onExit <<= do
    colnames  <-  New.treeViewGetColumns listView  >>=  mapM New.treeViewColumnGetTitle
    fmReplaceHistory fm' "ColumnOrder" (unwords$ catMaybes colnames)
    for columns $ \(name,col1) -> do
      w <- New.treeViewColumnGetWidth col1
      fmReplaceHistory fm' (name++"ColumnWidth") (show w)

  -- At startup, restore the saved column order and widths
  order <- (reverse.words) `fmap` fmGetHistory1 fm' "ColumnOrder" ""
  for order $ \colname -> do
    whenJust (lookup colname columns) $
      New.treeViewMoveColumnFirst listView
  for columns $ \(name,col1) -> do
    w <- readInt  `fmap`  fmGetHistory1 fm' (name++"ColumnWidth") "150"
    New.treeViewColumnSetFixedWidth col1 w


----------------------------------------------------------------------------------------------------
---- File manager navigation layer -----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--  for [upButton,saveDirButton] (`buttonSetFocusOnClick` False)

  -- Display errors/warnings at the bottom of the FreeArc window
  showErrors' <- ref True
  errorHandlers   ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']
  warningHandlers ++= [whenM (val showErrors') . postGUIAsync . fmStackMsg fm']

  -- Disables error message output for the duration of action
  let hideErrors action  =  bracket (showErrors' <=> False)  (showErrors' =: )  (\_ -> action)

  -- Catching and handling errors
  errorMsg <- ref ""
  errorHandlers ++= [(errorMsg =:)]
  let withErrorHandler onError = handle$ \e->do operationTerminated =: False
                                                fmErrorMsg fm' =<< val errorMsg
                                                sequence_ onError
  -- On error, report it to the user
  let msgboxOnError = withErrorHandler []
  -- On error, report it to the user and terminate the program
  let terminateOnError = withErrorHandler [shutdown "" aEXIT_CODE_FATAL_ERROR]


  -- Enter the given directory/archive or run a command
  let select filename = do
        fm <- val fm'
        handle (\e -> (operationTerminated =: False) >> runFile filename (fm_curdir fm) False) $ do    -- if entering fails, just run the file :)
          hideErrors $ do
            chdir fm' filename
            New.treeViewScrollToPoint (fm_view fm) 0 0
            --New.treeViewSetCursor (fm_view fm) [0] Nothing

  -- Go to the parent directory
  let goParentDir = do
        fm <- val fm'
        unless (isFM_Archive fm  &&  isURL(fm_arcname fm)  &&  fm_arcdir fm=="") $ do  -- Disallow Up from an archive located on the internet
        chdir fm' ".."
        -- Select the directory/archive we have just left
        fmSetCursor fm' (takeFileName$ fm_current fm)

  -- Write the current directory into the history
  let saveCurdirToHistory = do
        fm <- val fm'
        fmAddHistory fm' (isFM_Archive fm.$bool "dir" "arcname") =<< fmCanonicalizePath fm' =<< val curdir


  -- Pressing Enter on a list row opens the selected archive/directory
  listView `New.onRowActivated` \path column -> do
    fm <- val fm'
    file <- fmFileAt fm' path
    unless (isFM_Archive fm  &&  not(fdIsDir file)) $ do  -- Run command don't yet work directly from archives
    select (fmname file)

  -- A single-click on the empty space to the right/below deselects all files,
  -- a double-click in the same place selects all files
  listView `onButtonPress` \e -> do
    path <- New.treeViewGetPathAtPos listView (round$ eventX e, round$ eventY e)
    coltitle <- case path of
                  Just (_,column,_) -> New.treeViewColumnGetTitle column >>== fromMaybe ""
                  _                 -> return ""
    -- An empty coltitle means the click was outside the file list
    coltitle=="" &&& e.$eventButton==LeftButton &&&
      ((if e.$eventClick==SingleClick  then fmUnselectAll  else fmSelectAll) fm'  >>  return True)

  -- When switching to another directory/archive, show its name in the entry field
  fm' `fmOnChdir` do
    fm <- val fm'
    curdir =: fm_current fm
    -- Save archive names into the history
    isFM_Archive fm  &&&  fm_arcdir fm==""  &&&  saveCurdirToHistory
    -- Reread the history from disk
    rereadHistory curdir

  -- When switching to another directory/archive, show its name in the window title
  fm' `fmOnChdir` do
    fm <- val fm'
    let title | isFM_Archive fm  =  takeFileName (fm_arcname fm) </> fm_arcdir fm
              | otherwise        =  takeFileName (fm_dir fm)  |||  fm_dir fm
    set (fm_window fm) [windowTitle := title++" - "++aARC_NAME]

  -- Go to the parent directory via the Up button or BackSpace in the file list
  upButton  `onClick` goParentDir
  "BackSpace" `onKey` goParentDir

  -- Save the selected archive/directory into the history
  saveDirButton `onClick` do
    saveCurdirToHistory

  -- Open another directory or archive (Enter in the entry field)
  entry curdir `onEntryActivate` do
    saveCurdirToHistory
    select =<< val curdir

  -- Open another directory or archive (choosing from the history)
  widget curdir `New.onChanged` do
    whenJustM_ (New.comboBoxGetActive$ widget curdir) $ \_ -> do
    saveCurdirToHistory
    select =<< val curdir

  -- Run action on the files matching makeRE applied to the filename under the cursor
  let byFile action makeRE = do
        filename <- fmGetCursor fm'
        action fm' ((match$ makeRE filename) . fdBasename)

  -- Shift/Ctrl/Alt-Plus/Minus keys with the same operations as in FAR
  "<Shift>KP_Add"      `onKey` fmSelectAll   fm'
  "<Shift>KP_Subtract" `onKey` fmUnselectAll fm'
  "<Ctrl>KP_Add"       `onKey` byFile fmSelectFilenames   (("*" ++) . takeExtension)
  "<Ctrl>KP_Subtract"  `onKey` byFile fmUnselectFilenames (("*" ++) . takeExtension)
  "<Alt>KP_Add"        `onKey` byFile fmSelectFilenames   ((++".*") . dropExtension)
  "<Alt>KP_Subtract"   `onKey` byFile fmUnselectFilenames ((++".*") . dropExtension)


  -- Clicking a column header in the file list sorts by that column
  --   (clicking again sorts in reverse order)
  onColumnTitleClicked =: \column -> do
    fmModifySortOrder fm' (showSortOrder columns) (calcNewSortOrder column)
    refreshCommand fm'
    fmSaveSortOrder  fm' =<< fmGetSortOrder fm'  -- write the sort order into the config

  -- Sort the files by the saved criterion
  fmSetSortOrder fm' (showSortOrder columns) =<< fmRestoreSortOrder fm'


----------------------------------------------------------------------------------------------------
---- File menu -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Open an archive
  openAct `onActionActivate` do
    fm <- val fm'
    let curfile  =  if isFM_Archive fm  then fm_arcname fm  else fm_dir fm </> "."
    chooseFile window FileChooserActionOpen "0305 Open archive" aARCFILE_FILTER (return curfile) $ \filename -> do
      msgboxOnError $
        chdir fm' filename

  -- Select/unselect files by user-supplied mask
  let byDialog method msg = do
        whenJustM_ (fmInputString fm' "mask" msg (const$ return True) return) $ \mask -> do
          method fm' ((match mask) . fdBasename)
  selectAct   `onActionActivate`  byDialog fmSelectFilenames   "0008 Select files"
  unselectAct `onActionActivate`  byDialog fmUnselectFilenames "0009 Unselect files"

  -- Select all files
  selectAllAct `onActionActivate` do
    fmSelectAll fm'

  -- Invert the selection
  invertSelAct `onActionActivate` do
    fmInvertSelection fm'

  -- Refresh the file list with up-to-date data
  refreshAct `onActionActivate` do
    refreshCommand fm'

  -- Quit the program
  exitAct `onActionActivate`
    mainQuit


----------------------------------------------------------------------------------------------------
---- Console command execution engine inside the FM GUI --------------------------------------------
----------------------------------------------------------------------------------------------------

  -- While running operations, don't exit on exceptions; print their messages to the log file instead
  let myHandleErrors action x  =  do operationTerminated =: False
                                     action x `catch` handler
                                     operationTerminated =: False
        where handler ex = do
                errmsg <- case ex of
                   Deadlock    -> i18n"0011 No threads to run: infinite loop or deadlock?"
                   ErrorCall s -> return s
                   other       -> return$ show ex
                with' (val log_separator') (log_separator'=:) $ \_ -> do
                  log_separator' =: ""
                  io$ condPrintLineLn "w" errmsg
                return ()

  -- Run an archiver command
  let runWithMsg ([formatStart,formatSuccess,formatFail],msgArgs,cmd) = do
        -- Message about the start of command execution
        msgStart <- i18n formatStart
        postGUIAsync$ fmStackMsg fm' (formatn msgStart msgArgs)
        --postGUIAsync$ fmStackMsg fm' (unwords cmd)
        -- Execute and count the number of errors
        w <- count_warnings (parseCmdline cmd >>= mapM_ run)
        -- Message about successful completion or the number of errors encountered
        msgFinish <- i18n (if w==0  then formatSuccess  else formatFail)
        postGUIAsync$ fmStackMsg fm' (formatn msgFinish (msgArgs++[show w]))

  -- Commands executed by various buttons
  cmdChan <- newChan
  forkIO $ do
    foreverM $ do
      commands <- readChan cmdChan
      postGUIAsync$ do clearStats; widgetShowAll windowProgress
      mapM_ (myHandleErrors runWithMsg) commands
      whenM (isEmptyChan cmdChan)$ postGUIAsync$ do widgetHide windowProgress; refreshCommand fm'
      --uiDoneProgram
  let exec = writeChan cmdChan


----------------------------------------------------------------------------------------------------
---- Commands menu ---------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Compress data
  addAct `onActionActivate` do
    addDialog fm' exec "a" NoMode

  -- Extract archive(s)
  extractAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "x"
    rereadHistory curdir

  -- Test archive(s)
  testAct `onActionActivate` do
    archiveOperation fm' $
      extractDialog fm' exec "t"

  -- Information about the archive
  arcinfoAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec NoMode

  -- Delete files (from the archive)
  deleteAct `onActionActivate` do
    fm <- val fm'
    files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
    if null files  then fmErrorMsg fm' "0012 There are no files selected!" else do
    msg <- i18n$ case files of [_] | isFM_Archive fm -> "0160 Delete %1 from archive?"
                                   | otherwise       -> "0161 Delete %1?"
                               _   | isFM_Archive fm -> "0019 Delete %2 file(s) from archive?"
                                   | otherwise       -> "0020 Delete %2 file(s)?"
    whenM (askOkCancel window (formatn msg [head files, show3$ length files])) $ do
      fmDeleteSelected fm'
      if isFM_Archive fm
        -- Erase files from the archive
        then do closeFMArc fm'
                let arcname = fm_arcname fm
                exec [(["0228 Deleting from %1",
                        "0229 FILES SUCCESFULLY DELETED FROM %1",
                        "0230 %2 WARNINGS WHILE DELETING FROM %1"],
                       [takeFileName arcname],
                       ["d", "--noarcext", "--", arcname]++files)]
        -- Delete files on disk
        else io$ mapM_ (ignoreErrors.fileRemove.(fm_dir fm </>)) files


----------------------------------------------------------------------------------------------------
---- Tools menu ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Protect the archive from writing
  lockAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0299 Lock archive(s)?"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec [(["0300 Locking archive %1",
                  "0301 SUCCESFULLY LOCKED ARCHIVE %1",
                  "0302 %2 WARNINGS WHILE LOCKING ARCHIVE %1"],
                 [takeFileName arcname],
                 ["ch", "--noarcext", "-k", "--", arcname])]

  -- Edit the archive comment
  commentAct `onActionActivate` do
    msgboxOnError $
      archiveOperation fm' $
        arcinfoDialog fm' exec CommentMode

  -- Convert the archive to SFX
  recompressAct `onActionActivate` do
    addDialog fm' exec "ch" RecompressMode

  -- Convert the archive to SFX
  toSfxAct `onActionActivate` do
    addDialog fm' exec "ch" MakeSFXMode

  -- Encrypt the archive
  encryptAct `onActionActivate` do
    addDialog fm' exec "ch" EncryptionMode

  -- Add a recovery record to the archive
  addRrAct `onActionActivate` do
    addDialog fm' exec "ch" ProtectionMode

  -- Repair a damaged archive
  repairAct `onActionActivate` do
    multiArchiveOperation fm' $ \archives -> do
      let msg = "0381 Repair archive(s)? Repaired archive(s) will be placed into files named fixed.*"
      whenM (askOkCancel window (formatn msg [head archives, show3$ length archives])) $ do
        closeFMArc fm'
        for archives $ \arcname -> do
          exec [(["0382 Repairing archive %1",
                  "0383 SUCCESFULLY REPAIRED ARCHIVE %1",
                  "0384 %2 WARNINGS WHILE REPAIRING ARCHIVE %1"],
                 [takeFileName arcname],
                 ["r", "--noarcext", "--", arcname])]

  -- Modify archives
  modifyAct `onActionActivate` do
    addDialog fm' exec "ch" NoMode

  -- Join archives
  joinAct `onActionActivate` do
    addDialog fm' exec "j" NoMode


----------------------------------------------------------------------------------------------------
---- Options menu ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Program settings window
  settingsAct `onActionActivate` do
    settingsDialog fm'

  -- Operations on the log file
  let withLogfile action = do
        logfileHist <- fmGetHistory fm' "logfile"
        case logfileHist of
          logfile:_ | logfile>""  ->  action logfile
          _                       ->  fmErrorMsg fm' "0303 No log file specified in Settings dialog!"

  -- View the log file
  viewLogAct `onActionActivate` do
    withLogfile runViewCommand

  -- Clear the log file
  clearLogAct `onActionActivate` do
    withLogfile $ \logfile -> do
      msg <- i18n"0304 Clear logfile %1?"
      whenM (askOkCancel window (format msg logfile)) $ do
        filePutBinary logfile ""


----------------------------------------------------------------------------------------------------
---- Help menu -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

  -- Home/news page for the current locale
  homeURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0254 "
  newsURL  <- ((aARC_WEBSITE ++ "/") ++) ==<< i18n"0255 News.aspx"
  forumURL <- ("http://apps.sourceforge.net/phpbb/freearc/" ++) ==<< i18n"0371 viewforum.php?f=3"
  wikiURL  <- ("http://freearc.wiki.sourceforge.net/"       ++) ==<< i18n"0372 "

  -- Open a URL
  let openWebsite url  =  runFile url "." False

  -- Open a help file
  let openHelp helpfile = do
        doc  <- i18n helpfile
        file <- findFile libraryFilePlaces ("../Documentation" </> doc)
        case file of
          "" -> return ()
          _  -> openWebsite file

  -- Read the ID generated for this computer
  let getUserID = do
#ifndef FREEARC_WIN
        -- Emulate a non-functional Windows Registry on other operating systems
        let registryGetStr root branch key       = return Nothing
            registrySetStr root branch key value = return ()
            hKEY_LOCAL_MACHINE                   = ()
#endif
        -- First look for it in the ini file
        userid <- fmGetHistory1 fm' "UserID" ""
        if userid/=""  then return (Just userid)  else do
        -- If that failed, read the previous installation's key from the Windows Registry...
        userid <- do userid <- registryGetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID"
                     case userid of
                       Just userid -> return userid
                                      -- ... or, as a last resort, generate a new one
                       Nothing     -> generateRandomBytes 8 >>== encode16
        -- And write it everywhere
        registrySetStr hKEY_LOCAL_MACHINE "SOFTWARE\\FreeArc" "UserID" userid
        fmReplaceHistory fm' "UserID" userid
        -- Return it only if writing to the ini file succeeded
        userid1 <- fmGetHistory1 fm' "UserID" ""
        return (if userid==userid1  then Just userid  else Nothing)

  -- Returns True once a day
  let daily = do
        last <- fmGetHistory1 fm' "LastCheck" ""
        now  <- getUnixTime
        let day = round$ 24.37*60*60
        if  last>""  &&  (now - readI last < day)  then return False  else do
        fmReplaceHistory fm' "LastCheck" (show now)
        now1 <- fmGetHistory1 fm' "LastCheck" ""
        return (show now==now1)

  -- Size of maximum memory block we can allocate in bytes
  maxBlock <- getMaxMemToAlloc

  -- Registers program usage and checks for news
  --  (manual=True - manual invocation from the menu, False - daily automatic check)
  let checkNews manual = do
        fmStackMsg fm' "0295 Checking for updates..."
        forkIO_ $ do
          -- Report program usage
          whenJustM_ getUserID $ \userid -> do
#ifdef FREEARC_WIN
            si <- getSystemInfo; let ramLimit = showMem (si.$siMaximumApplicationAddress.$ptrToWordPtr.$toInteger `roundTo` (4*mb))
#endif
            language <- i18n"0000 English"
            let url = aARC_WEBSITE ++ "/CheckNews.aspx?user=" ++ userid ++ "&version=" ++ urlEncode aARC_VERSION
                                   ++ "&OS%20family=" ++ iif isWindows "Windows" "Unix"
                                   ++ "&RAM=" ++ showMem (toInteger getPhysicalMemory `roundTo` (4*mb))
#ifdef FREEARC_WIN
                                   ++ "&address%20space=" ++ ramLimit
#endif
                                   ++ "&largest%20memory%20block=" ++ showMem (maxBlock `roundDown` (100*mb))
                                   ++ "&number%20of%20cores=" ++ show getProcessorsCount
                                   ++ "&language=" ++ urlEncode language
            --gui$ fmStackMsg fm' url
            ignoreErrors (fileGetBinary url >> return ())
          -- Check the news page
          handleErrors
            -- Runs when the news page is unavailable
            (gui $ do
                msg <- i18n"0296 Cannot open %1. Do you want to check the page with browser?"
                whenM (askOkCancel window (format msg newsURL)) $ do
                  openWebsite newsURL)
            -- Attempt to read the news page
            (fileGetBinary newsURL >>== (`showHex` "") . crc32) $ \new_crc -> do
          -- The news page was read successfully
          old_crc <- fmGetHistory1 fm' "news_crc" ""
          gui $ do
          fmStackMsg fm' ""
          if (new_crc == old_crc) then do
             msg <- i18n"0297 Nothing new at %1"
             manual &&& fmInfoMsg fm' (format msg newsURL)
           else do
             fmReplaceHistory fm' "news_crc" new_crc
             msg <- i18n"0298 Found new information at %1! Open the page with browser?"
             whenM (askOkCancel window (format msg newsURL)) $ do
               openWebsite newsURL

  -- Check for news twice an hour
  forkIO_ $ do
    whenM (fmGetHistoryBool fm' "CheckNews" True) $ do
      foreverM $ do
        whenM daily $ do
          checkNews False
        threadDelay (30*60*1000000::Int)  -- 60 minutes is too much for Int!


  -- Help on using the GUI
  helpAct `onActionActivate` do
    openHelp "0256 FreeArc-GUI-Eng.htm"

  -- Help on using the command line
  helpCmdAct `onActionActivate` do
    openHelp "0257 FreeArc036-eng.htm"

  -- Program home page
  homepageAct `onActionActivate` do
    openWebsite homeURL

  -- Program home page
  openForumAct `onActionActivate` do
    openWebsite forumURL

  -- Program home page
  openWikiAct `onActionActivate` do
    openWebsite wikiURL

  -- Check for updates on the web site
  whatsnewAct `onActionActivate` do
    checkNews True

  -- About dialog
  aboutAct `onActionActivate` do
    bracketCtrlBreak "aboutDialogDestroy" aboutDialogNew widgetDestroy $ \dialog -> do
    dialog `set` [windowTransientFor   := window
                 ,aboutDialogName      := aARC_NAME
                 ,aboutDialogVersion   := aARC_VERSION_WITH_DATE
                 ,aboutDialogCopyright := "(c) "++aARC_EMAIL
                 ,aboutDialogComments  := "High-performance archiver"
                 ,aboutDialogWebsite   := homeURL
                 ,aboutDialogAuthors   := ["Igor Pavlov (author of LZMA and EXE filter)"
                                          ,"Dmitry Shkarin (author of PPMd)"
                                          ,"Ilya Grebnov (author of GRZipII and LZP filter)"
                                          ,"Alexander Djourik and Pavel Zhilin (authors of TTA)"
                                          ,"Dmitry Subbotin (author of Carryless rangecoder)"
                                          ,"Joachim Henke (coauthor of Tornado)"
                                          ,"Mark Shevchenko (author of GUI SFX and web site)"
                                          ,aARC_EMAIL++" (author of remaining parts)"
                 ]]
    dialogRun dialog
    return ()

  -- Enable URL support in the About dialog
  aboutDialogSetUrlHook openWebsite


  -- Initialize the file manager state with the directory/archive given on the command line (if absent, with the current directory)
  terminateOnError $
    chdir fm' (head (args++["."]))
  fmStatusBarTotals fm'

  return window

