----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: file manager panel                                              ------
----------------------------------------------------------------------------------------------------
module FileManPanel where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UIBase
import UI
import ArhiveDirectory
import ArcExtract
import FileManUtils


-- |Encryption and decryption passwords
encryptionPassword  =  unsafePerformIO$ newIORef$ ""
decryptionPassword  =  unsafePerformIO$ newIORef$ ""

----------------------------------------------------------------------------------------------------
---- File manager operations -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Create a variable holding the file manager state
newFM window view model selection statusLabel messageCombo = do
  historyFile <- findOrCreateFile configFilePlaces aHISTORY_FILE >>= mvar
  curdir <- io$ getCurrentDirectory
  counterCombo <- ref 0  -- number of last message + 1 in combobox
  fm' <- mvar FM_State { fm_window       = window
                       , fm_view         = view
                       , fm_model        = model
                       , fm_selection    = selection
                       , fm_statusLabel  = statusLabel
                       , fm_messageCombo = (messageCombo, counterCombo)
                       , fm_filelist     = error "undefined FM_State::fm_filelist"
                       , fm_history_file = historyFile
                       , fm_history      = Nothing
                       , fm_onChdir      = []
                       , fm_sort_order   = ""
                       , subfm           = FM_Directory {subfm_dir=curdir}}
  selection `New.onSelectionChanged` fmStatusBarTotals fm'
  return fm'

-- |Open an archive and return it as a file manager state object
newFMArc fm' arcname arcdir = do
  xpwd'     <- val decryptionPassword
  xkeyfile' <- fmGetHistory1 fm' "keyfile" ""
  [command] <- io$ parseCmdline$ ["l", arcname]++(xpwd' &&& ["-op"++xpwd'])
                                               ++(xkeyfile' &&& ["--OldKeyfile="++xkeyfile'])
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- prepare the passwords in the command for use
  archive <- io$ archiveReadInfo command "" "" (const True) doNothing2 arcname
  let filetree = buildTree$ map (fiToFileData.cfFileInfo)$ arcDirectory archive
  io$ arcClose archive
  return$ FM_Archive archive arcname arcdir filetree

-- |Close the archive file so that other operations can modify it
closeFMArc fm' = do
  return ()
  --fm <- val fm'
  --io$ arcClose (fm_archive fm)
  --fm' .= \fm -> fm {subfm = (subfm fm) {subfm_archive = phantomArc}}

-- Change into the archive/directory filename
chdir fm' filename' = do
  fm <- val fm'
  filename <- fmCanonicalizePath fm' filename'
  res <- splitArcPath fm' filename
  msg <- i18n"0071 %1: no such file or directory!"
  if res==Not_Exists  then fmErrorMsg fm' (format msg filename)  else do
  (files, sub) <- case res of
    -- List of files in a directory on disk
    DiskPath dir -> do filelist <- io$ dir_list dir
                       return (map fiToFileData filelist, FM_Directory dir)
    -- List of files in an archive
    ArcPath arcname arcdir -> do
                       arc <- if isFM_Archive fm && arcname==fm_arcname fm
                                then return ((fm.$subfm) {subfm_arcdir=arcdir})
                                else newFMArc fm' arcname arcdir
                       return (arc.$subfm_filetree.$ftFilesIn arcdir fdArtificialDir, arc)
  -- Store the current directory/archive in fm and display the new file list
  fm' =: fm {subfm = sub}
  fmSetFilelist fm' (files.$ sortOnColumn (fm_sort_order fm))
  -- Refresh the status bar and run all the other programmed actions.
  sequence_ (fm_onChdir fm)
  widgetGrabFocus (fm_view fm)

-- Reflect a change of the archive name
fmChangeArcname fm' newname = do
  fm' .= fm_changeArcname newname
  fm <- val fm'
  sequence_ (fm_onChdir fm)

-- |Add action to the list of operations performed when changing to another directory/archive
fmOnChdir fm' action = do
  fm' .= \fm -> fm {fm_onChdir = action : fm_onChdir fm}

-- |Show in the message line the total size of the files and how many of them are selected
fmStatusBarTotals fm' = do
  fm <- val fm'
  selected <- getSelectionFileInfo fm'
  [sel,total] <- i18ns ["0022 Selected %1 bytes in %2 file(s)", "0023 Total %1 bytes in %2 file(s)"]
  let format msg files  =  formatn msg [show3$ sum$ map fdSize files,  show3$ length files]
  fmStatusBarMsg fm' $ (selected &&& (format sel   selected++"     "))
                                 ++   format total (fm_filelist fm)

-- |Print a message in the message line
fmStatusBarMsg fm' msg = do
  fm <- val fm'
  labelSetText (fm_statusLabel fm) msg
  return ()

-- |Add a message to the pop-up message list
fmStackMsg fm' msg = do
  fm <- val fm'
  let (box,n')  =  fm_messageCombo fm
  n <- val n';  n' += 1
  New.comboBoxAppendText box =<< i18n msg
  New.comboBoxSetActive  box n
  return ()

-- |Name of the file located at the given path
fmFilenameAt fm' path  =  fmname `fmap` fmFileAt fm' path

-- |The file located at the given path
fmFileAt fm' path = do
  fm <- val fm'
  let fullList = fm_filelist fm
  return$ fullList!!head path

-- |Return the file under the cursor
fmGetCursor fm' = do
  fm <- val fm'
  let fullList  = fm_filelist  fm
  (cursor,_) <- New.treeViewGetCursor (fm_view fm)
  case cursor of
    [i] -> return (fdBasename$ fullList!!i)
    _   -> return ""

-- |Place the cursor on the given file
fmSetCursor fm' filename = do
  fm <- val fm'
  whenJustM_ (fmFindCursor fm' filename)
             (\cursor -> New.treeViewSetCursor (fm_view fm) cursor Nothing)

-- |Return the cursor for the file with the given name
fmFindCursor fm' filename = do
  fm <- val fm'
  let fullList  =  fm_filelist  fm
  return (fmap (:[])$  findIndex ((filename==) . fmname) fullList)

-- |Display a new file list
fmSetFilelist fm' files = do
  fm <- val fm'
  fm' =: fm {fm_filelist = files}
  changeList (fm_model fm) (fm_selection fm) files

-- |Show an error message
fmErrorMsg fm' msg = do
  fm <- val fm'
  msgBox (fm_window fm) MessageError msg

-- |Show an informational message
fmInfoMsg fm' msg = do
  fm <- val fm'
  msgBox (fm_window fm) MessageInfo msg


----------------------------------------------------------------------------------------------------
---- File selection --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Select/deselect the files satisfying the given predicate
fmSelectFilenames   = fmSelUnselFilenames New.treeSelectionSelectPath
fmUnselectFilenames = fmSelUnselFilenames New.treeSelectionUnselectPath
fmSelUnselFilenames selectOrUnselect fm' filter_p = do
  fm <- val fm'
  let fullList  = fm_filelist  fm
  let selection = fm_selection fm
  for (findIndices filter_p fullList)
      (selectOrUnselect selection.(:[]))

-- |Select/deselect all files
fmSelectAll   fm' = New.treeSelectionSelectAll   . fm_selection =<< val fm'
fmUnselectAll fm' = New.treeSelectionUnselectAll . fm_selection =<< val fm'

-- |Invert the selection
fmInvertSelection fm' = do
  fm <- val fm'
  let files     = length$ fm_filelist fm
  let selection = fm_selection fm
  for [0..files-1] $ \i -> do
    selected <- New.treeSelectionPathIsSelected selection [i]
    (if selected  then New.treeSelectionUnselectPath  else New.treeSelectionSelectPath) selection [i]

-- |List of names of the selected files + directory names under the mapping mapDirName
getSelection fm' mapDirName = do
  let mapFilenames fd | fdIsDir fd = mapDirName$ fmname fd
                      | otherwise  = [fmname fd]
  getSelectionFileInfo fm' >>== concatMap mapFilenames

-- |List of FileInfo for the selected files
getSelectionFileInfo fm' = do
  fm <- val fm'
  let fullList = fm_filelist fm
  getSelectionRows fm' >>== map (fullList!!)

-- |List of indices of the selected files
getSelectionRows fm' = do
  fm <- val fm'
  let selection = fm_selection fm
  New.treeSelectionGetSelectedRows selection >>== map head

-- |Delete the selected files from the model
fmDeleteSelected fm' = do
  rows <- getSelectionRows fm'
  fm <- val fm'
  fmSetFilelist fm' (fm_filelist fm `deleteElems` rows)     -- O(n^2)!


----------------------------------------------------------------------------------------------------
---- Sorting the file list -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Return the current sort order
fmGetSortOrder fm'  =  fm_sort_order `fmap` val fm'
-- |Set the sort order
fmSetSortOrder fm' showSortOrder  =  fmModifySortOrder fm' showSortOrder . const
-- |Modify the sort order in fm' and show the sort indicator above the corresponding column
fmModifySortOrder fm' showSortOrder f_order = do
  fm <- val fm'
  let sort_order = f_order (fm_sort_order fm)
  fm' =: fm {fm_sort_order = sort_order}
  -- Update the sort indicator
  let (column, order)  =  break1 isUpper sort_order
  showSortOrder column (if order == "Asc"  then SortAscending  else SortDescending)

-- |Save the sort order into the history
fmSaveSortOrder    fm'  =  fmReplaceHistory fm' "SortOrder"
-- |Restore the sort order from the history
fmRestoreSortOrder fm'  =  fmGetHistory1    fm' "SortOrder" "NameAsc"

-- | (ClickedColumnName, OldSortOrder) -> NewSortOrder
calcNewSortOrder "Name"     "NameAsc"      = "NameDesc"
calcNewSortOrder "Name"     _              = "NameAsc"
calcNewSortOrder "Size"     "SizeDesc"     = "SizeAsc"
calcNewSortOrder "Size"     _              = "SizeDesc"
calcNewSortOrder "Modified" "ModifiedDesc" = "ModifiedAsc"
calcNewSortOrder "Modified" _              = "ModifiedDesc"

-- |Choosing the sort function by column name
sortOnColumn "NameAsc"       =  sortOn (\fd -> (not$ fdIsDir fd, strLower$ fmname fd))
sortOnColumn "NameDesc"      =  sortOn (\fd -> (     fdIsDir fd, strLower$ fmname fd))  >>> reverse
--
sortOnColumn "SizeAsc"       =  sortOn (\fd -> if fdIsDir fd  then -1             else  fdSize fd)
sortOnColumn "SizeDesc"      =  sortOn (\fd -> if fdIsDir fd  then aFILESIZE_MIN  else -fdSize fd)
--
sortOnColumn "ModifiedAsc"   =  sortOn (\fd -> (not$ fdIsDir fd,  fdTime fd))
sortOnColumn "ModifiedDesc"  =  sortOn (\fd -> (not$ fdIsDir fd, -fdTime fd))
--
sortOnColumn _               =  id


----------------------------------------------------------------------------------------------------
---- Operations on the history file ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Add a value to the history list (removing previous identical lines)
fmAddHistory fm' tags text     =   fmModifyHistory fm' tags text (\tag line -> (line==))
-- |Replace a value in the history list (removing previous values with this tag)
fmReplaceHistory fm' tags text  =  fmModifyHistory fm' tags text (\tag line -> (tag==) . fst.split2 '=')
-- |Add/replace a value in the history list
fmModifyHistory fm' tags text deleteCond = ignoreErrors $ do
  fm <- val fm'
  -- Put the new element at the head of the list and get rid of duplicate values
  let newItem  =  join2 "=" (mainTag, text)
      mainTag  =  head (split '/' tags)
  withMVar (fm_history_file fm) $ \history_file -> do
    modifyConfigFile history_file ((newItem:) . deleteIf (deleteCond mainTag newItem))

-- |Remove a tag from the history list
fmDeleteTagFromHistory fm' tag = ignoreErrors $ do
  fm <- val fm'
  withMVar (fm_history_file fm) $ \history_file -> do
    modifyConfigFile history_file (deleteIf ((tag==) . fst.split2 '='))

-- |Extract the history list for the given tag/tags
fmGetHistory1 fm' tags deflt = do x <- fmGetHistory fm' tags; return (head (x++[deflt]))
fmGetHistory  fm' tags       = handle (\_ -> return []) $ do
  fm <- val fm'
  hist <- fmGetConfigFile fm'
  hist.$ map (split2 '=')                           -- split each line into tag+value
      .$ filter ((split '/' tags `contains`) . fst)   -- keep the lines whose tag is in the tags list
      .$ map snd                                    -- keep only the values.
      .$ map (splitCmt "")                          -- split each value into description+options
      .$ mapM (\x -> case x of                      -- localize the description and merge them back
                       ("",b) -> return b
                       (a ,b) -> do a <- i18n a; return$ join2 ": " (a,b))

-- Reading/writing a boolean value in the history
fmGetHistoryBool     fm' tag deflt  =  fmGetHistory1 fm' tag (bool2str deflt)  >>==  (==bool2str True)
fmReplaceHistoryBool fm' tag x      =  fmReplaceHistory fm' tag (bool2str x)
bool2str True  = "1"
bool2str False = "0"


-- |Get the contents of the history file
fmGetConfigFile fm' = do
  fm <- val fm'
  case fm_history fm of
    Nothing      -> withMVar (fm_history_file fm) readConfigFile
    Just history -> return history

-- |For the duration of this bracket the history file contents are read from the fm_history field
fmCacheConfigFile fm' =
  bracket_ (do history <- fmGetConfigFile fm'
               fm' .= \fm -> fm {fm_history = Just history})
           (do fm' .= \fm -> fm {fm_history = Nothing})

-- |Save the window size and position into the history
saveSizePos fm' window name = do
    (x,y) <- windowGetPosition window
    (w,h) <- widgetGetSize     window
    fmReplaceHistory fm' (name++"Coord") (unwords$ map show [x,y,w,h])

-- |Remember whether the window was maximized
saveMaximized fm' name = fmReplaceHistoryBool fm' (name++"Maximized")

-- |Restore the window size and position from the history
restoreSizePos fm' window name deflt = do
    coord <- fmGetHistory1 fm' (name++"Coord") deflt
    let a  = coord.$split ' '
    when (length(a)==4  &&  all isSignedInt a) $ do  -- check that a consists of exactly 4 numbers
      let [x,y,w,h] = map readSignedInt a
      windowMove   window x y  `on` x/= -10000
      windowResize window w h  `on` w/= -10000
    whenM (fmGetHistoryBool fm' (name++"Maximized") False) $ do
      windowMaximize window


----------------------------------------------------------------------------------------------------
---- Helper definitions ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Selects one of several variants by index
opt `select` variants  =  words (split ',' variants !! opt)
-- Converts the setting text into a list of options, first stripping the comment at its start
cvt1 opt  =  map (opt++) . (||| [""]) . words . clear
-- The same, except the option name is added only to words that don't start with "-"
cvt  opt  =  map (\w -> (w!~"-?*" &&& opt)++w) . (||| [""]) . words . clear
-- Removes a comment of the form "*: " at the start of the line
clear     =  trim . snd . splitCmt ""
-- |Splits a value into description+options
splitCmt xs ""           = ("", reverse xs)
splitCmt xs ":"          = (reverse xs, "")
splitCmt xs (':':' ':ws) = (reverse xs, ws)
splitCmt xs (w:ws)       = splitCmt (w:xs) ws


----------------------------------------------------------------------------------------------------
---- GUI controls that interact with the FM --------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |A combined widget representing an input line with persistent history
data EntryWithHistory = EntryWithHistory
  { ehGtkWidget   :: GtkWidget ComboBoxEntry String
  , entry         :: Entry
  }

instance GtkWidgetClass EntryWithHistory ComboBoxEntry String where
  widget        = widget        . ehGtkWidget
  getTitle      = getTitle      . ehGtkWidget
  setTitle      = setTitle      . ehGtkWidget
  getValue      = getValue      . ehGtkWidget
  setValue      = setValue      . ehGtkWidget
  setOnUpdate   = setOnUpdate   . ehGtkWidget
  onClick       = onClick       . ehGtkWidget
  saveHistory   = saveHistory   . ehGtkWidget
  rereadHistory = rereadHistory . ehGtkWidget


{-# NOINLINE fmEntryWithHistory #-}
-- |Create a combo box with history under the tag tag;
-- before storing it in the history, pass the entered text through the process operation
fmEntryWithHistory fm' tag filter_p process = do
  -- Create GUI controls
  comboBox <- New.comboBoxEntryNewText
  Just entry <- binGetChild comboBox >>== fmap castToEntry
  set entry [entryActivatesDefault := True]
  -- Define callbacks
  last <- fmGetHistory fm' (tag++"Last")
  let fixedOrder  =  (last>[])   -- True - keep order of dropdown "menu" elements fixed
  history' <- mvar []
  let readHistory = do
        history' .<- \oldHistory -> do
          replicateM_ (1+length oldHistory) (New.comboBoxRemoveText comboBox 0)
          history <- fmGetHistory fm' tag >>= Utils.filterM filter_p
          for history (New.comboBoxAppendText comboBox)
          return history
  let getText = do
        val entry >>= process
  let setText text = do
        entry =: text
  let saveHistory = do
        text <- getText
        history <- val history'
        when fixedOrder $ do
          fmReplaceHistory fm' (tag++"Last") text
        unless (fixedOrder && (text `elem` history)) $ do
          New.comboBoxPrependText comboBox text
          fmAddHistory fm' tag text
  readHistory
  -- Set the text in the input field
  case last of
    last:_ -> entry =: last
    []     -> do history <- val history'
                 when (history > []) $ do
                   New.comboBoxSetActive comboBox 0
  --
  return EntryWithHistory
           {                           entry           = entry
           , ehGtkWidget = gtkWidget { gwWidget        = comboBox
                                     , gwGetValue      = getText
                                     , gwSetValue      = setText
                                     , gwSetOnUpdate   = \action -> New.onChanged comboBox action >> return ()
                                     , gwSaveHistory   = saveHistory
                                     , gwRereadHistory = readHistory
                                     }
           }


{-# NOINLINE fmLabeledEntryWithHistory #-}
-- |String input with history under the tag tag and a label on the left
fmLabeledEntryWithHistory fm' tag title = do
  hbox  <- hBoxNew False 0
  title <- label title
  inputStr <- fmEntryWithHistory fm' tag (const$ return True) (return)
  boxPackStart  hbox  (widget title)     PackNatural 0
  boxPackStart  hbox  (widget inputStr)  PackGrow    5
  return (hbox, inputStr)


{-# NOINLINE fmCheckedEntryWithHistory #-}
-- |String input with history under the tag tag and a checkbox on the left
fmCheckedEntryWithHistory fm' tag title = do
  hbox  <- hBoxNew False 0
  checkBox <- checkBox title
  inputStr <- fmEntryWithHistory fm' tag (const$ return True) (return)
  boxPackStart  hbox  (widget checkBox)  PackNatural 0
  boxPackStart  hbox  (widget inputStr)  PackGrow    5
  --checkBox `onToggled` do
  --  on <- val checkBox
  --  (if on then widgetShow else widgetHide) (widget inputStr)
  return (hbox, checkBox, inputStr)


{-# NOINLINE fmFileBox #-}
-- |File/directory name input with history under the tag tag and disk browsing via an invoked dialog
fmFileBox fm' dialog tag dialogType makeControl dialogTitle filters filter_p process = do
  hbox     <- hBoxNew False 0
  control  <- makeControl
  filename <- fmEntryWithHistory fm' tag filter_p process
  chooserButton <- button "9999 ..."
  chooserButton `onClick` do
    chooseFile dialog dialogType dialogTitle filters (val filename) (filename =:)
  boxPackStart  hbox  (widget control)        PackNatural 0
  boxPackStart  hbox  (widget filename)       PackGrow    5
  boxPackStart  hbox  (widget chooserButton)  PackNatural 0
  return (hbox, control, filename)

{-# NOINLINE fmInputString #-}
-- |Ask the user for a string (with input history)
fmInputString fm' tag title filter_p process = do
  fm <- val fm'
  -- Create a dialog with the standard OK/Cancel buttons
  fmDialog fm' title $ \(dialog,okButton) -> do
    x <- fmEntryWithHistory fm' tag filter_p process

    upbox <- dialogGetUpper dialog
    --boxPackStart  upbox label    PackGrow 0
    boxPackStart  upbox (widget x) PackGrow 0
    widgetShowAll upbox

    choice <- dialogRun dialog
    case choice of
      ResponseOk -> do saveHistory x; val x >>== Just
      _          -> return Nothing


{-# NOINLINE fmCheckButtonWithHistory #-}
-- |Create a checkbox with history under the tag tag
fmCheckButtonWithHistory fm' tag deflt title = do
  control <- checkBox title
  let rereadHistory = do
        control =:: fmGetHistoryBool fm' tag deflt
  let saveHistory = do
        fmReplaceHistoryBool fm' tag =<< val control
  rereadHistory
  return$ control
           { gwSaveHistory   = saveHistory
           , gwRereadHistory = rereadHistory
           }

{-# NOINLINE fmDialog #-}
-- |A dialog with the standard OK/Cancel buttons
fmDialog fm' title action = do
  fm <- val fm'
  title <- i18n title
  bracketCtrlBreak "fmDialog" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle          := title,
                windowTransientFor   := fm_window fm,
                containerBorderWidth := 0]
    addStdButton dialog ResponseOk      >>= \okButton -> do
    addStdButton dialog ResponseCancel
    dialogSetDefaultResponse dialog ResponseOk
    tooltips =:: tooltipsNew
    action (dialog,okButton)

{-# NOINLINE fmDialogRun #-}
-- |Run a dialog, saving its position and size into the history
fmDialogRun fm' dialog name = do
    inside (restoreSizePos fm' dialog name "")
           (saveSizePos    fm' dialog name)
      (dialogRun dialog)


----------------------------------------------------------------------------------------------------
---- List of files in the archive ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

createFilePanel = do
  -- Scrolled window where this list will be put
  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
  -- Create a new ListView
  view  <- New.treeViewNew
  set view [ {-New.treeViewSearchColumn := 0, -} New.treeViewRulesHint := True]
  New.treeViewSetHeadersVisible view True
  -- Create and install the model
  model <- New.listStoreNew []
  set view [New.treeViewModel := model]
  -- Create the columns used to display it.
  let columnTitles = ["0015 Name", "0016 Size", "0017 Modified", "0018 DIRECTORY"]
      n = map (drop 5) columnTitles
  s <- i18ns columnTitles
  onColumnTitleClicked <- ref doNothing
  columns <- fmap (dropEnd 1) $ sequence [
     addColumn view model onColumnTitleClicked (n!!0) (s!!0) fmname                                                       []
    ,addColumn view model onColumnTitleClicked (n!!1) (s!!1) (\fd -> if (fdIsDir fd) then (s!!3) else (show3$ fdSize fd)) [cellXAlign := 1]
    ,addColumn view model onColumnTitleClicked (n!!2) (s!!2) (guiFormatDateTime.fdTime)                                   []
    ,addColumn view model onColumnTitleClicked ("")   ("")   (const "")                                                   [] ]
  -- Enable searching by the first column
  New.treeViewSetEnableSearch view True
  New.treeViewSetSearchColumn view 0
  New.treeViewSetSearchEqualFunc view $ \col str iter -> do
    (i:_) <- New.treeModelGetPath model iter
    row <- New.listStoreGetValue model i
    return (strLower(fmname row) ~= strLower(str)++"*")
  -- Enable multiple selection
  selection <- New.treeViewGetSelection view
  set selection [New.treeSelectionMode := SelectionMultiple]
  -- Pack list into scrolled window and return window
  containerAdd scrwin view
  return (scrwin, view, model, selection, columns, onColumnTitleClicked)

-- |Set a new list of displayed files
changeList model selection filelist = do
  New.treeSelectionUnselectAll selection
  -- Delete the old data from the model and fill it with the new data
  New.listStoreClear model
  for filelist (New.listStoreAppend model)

-- |Add to view a column displaying field, with the header title
addColumn view model onColumnTitleClicked colname title field attrs = do
  col1 <- New.treeViewColumnNew
  New.treeViewColumnSetTitle col1 title
  renderer1 <- New.cellRendererTextNew
  New.cellLayoutPackStart col1 renderer1 False
  -- Attempts to make the name field grow automatically when the program window is enlarged
  -- (bool New.cellLayoutPackStart New.cellLayoutPackEnd expand) col1 renderer1 expand
  -- set col1 [New.treeViewColumnSizing := TreeViewColumnAutosize] `on` expand
  -- set col1 [New.treeViewColumnSizing := TreeViewColumnFixed] `on` not expand
  -- cellLayoutSetAttributes  [New.cellEditable := True, New.cellEllipsize := EllipsizeEnd]
  when (colname/="") $ do
    set col1 [ New.treeViewColumnResizable   := True
             , New.treeViewColumnSizing      := TreeViewColumnFixed
             , New.treeViewColumnClickable   := True
             , New.treeViewColumnReorderable := True ]
  -- Invoke the callback when the column header is clicked
  col1 `New.onColClicked` do
    val onColumnTitleClicked >>= ($colname)
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [New.cellText := field row] ++ attrs
  New.treeViewAppendColumn view col1
  return (colname,col1)

-- |Show the sort indicator above the column colname in the direction order
showSortOrder columns colname order = do
  for (map snd columns) (`New.treeViewColumnSetSortIndicator` False)
  let Just col1  =  colname `lookup` columns
  New.treeViewColumnSetSortIndicator col1 True
  New.treeViewColumnSetSortOrder     col1 order

