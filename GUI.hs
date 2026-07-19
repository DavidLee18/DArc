{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Notifying the user about the program's progress.                                         ------
----------------------------------------------------------------------------------------------------
module GUI where

import Prelude    hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char  hiding (Control)
import Data.IORef
import Data.List
import Data.Maybe
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.Time

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import Charsets
import FileInfo
import Options
import UIBase

-- |Program settings file
aINI_FILE = "freearc.ini"

-- |File describing the menu and toolbar
aMENU_FILE = "freearc.menu"

-- Tags in the INI file
aINITAG_LANGUAGE = "language"
aINITAG_PROGRESS = "ProgressWindowSize"

-- |Localization directory
aLANG_DIR = "arc.languages"

-- |Name of the config file storing the heart-rending histories of opened archives
aHISTORY_FILE = "freearc.history"

-- |Name of the file holding the program icon
aICON_FILE = "FreeArc.ico"

-- |Filters for choosing an archive
aARCFILE_FILTER = ["0307 FreeArc archives (*.arc)", "0308 Archives and SFXes (*.arc;*.exe)"]


----------------------------------------------------------------------------------------------------
---- Displaying the progress indicator -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Initializes Gtk and creates the program's initial window
startGUI action = runInBoundThread $ do
  unsafeInitGUIForThreadedRTS
  guiThread =:: getOsThreadId
  action >>= widgetShowAll
  mainGUI

-- |Variable holding the GUI thread number
guiThread  =  unsafePerformIO$ newIORef$ error "undefined GUI::guiThread"

-- |Initialization of the GUI part of the program
guiStartProgram = forkIO$ startGUI (fmap fst runIndicators)

{-# NOINLINE runIndicators #-}
-- |Creates the progress indicator window and starts a thread that updates it periodically.
runIndicators = do
  -- INI file
  inifile  <- findFile configFilePlaces aINI_FILE
  settings <- inifile  &&&  readConfigFile inifile >>== map (split2 '=')
  -- Localization.
  langDir  <- findDir libraryFilePlaces aLANG_DIR
  setLocale$ langDir </> (settings.$lookup aINITAG_LANGUAGE `defaultVal` aLANG_FILE)

  -- The progress indicator window itself
  window <- windowNew
  vbox   <- vBoxNew False 10
  set window [windowWindowPosition := WinPosCenter,
              containerBorderWidth := 10, containerChild := vbox]

  -- Size of the progress indicator window
  let sz = settings.$lookup aINITAG_PROGRESS `defaultVal` "350 200"
  let (w,h) = sz.$ split2 ' '
  windowResize window (readInt w) (readInt h)

  -- Split the window vertically
  (statsBox, updateStats, clearStats) <- createStats
  curFileLabel <- labelNew Nothing
  curFileBox   <- hBoxNew True 0
  boxPackStart curFileBox curFileLabel PackGrow 2
  widgetSetSizeRequest curFileLabel 30 (-1)
  progressBar  <- progressBarNew
  buttonBox    <- hBoxNew True 10
  boxPackStart vbox statsBox     PackNatural 0
  boxPackStart vbox curFileBox   PackNatural 0
  boxPackStart vbox progressBar  PackNatural 0
  boxPackEnd   vbox buttonBox    PackNatural 0
  miscSetAlignment curFileLabel 0 0    -- left-align the current file name
  progressBarSetText progressBar " "   -- a non-empty text is needed to set the correct progressBar height

  -- Fill the lower part of the window with buttons
  --buttonNew window stockClose ResponseClose
  backgroundButton <- buttonNewWithMnemonic       =<< i18n"0052   _Background  "
  pauseButton      <- toggleButtonNewWithMnemonic =<< i18n"0053   _Pause  "
  cancelButton     <- buttonNewWithMnemonic       =<< i18n"0081   _Cancel  "
  boxPackStart buttonBox backgroundButton PackNatural 0
  boxPackStart buttonBox pauseButton      PackNatural 0
  boxPackEnd   buttonBox cancelButton     PackNatural 0

  -- Event handlers (window closing / button presses)
  let askProgramClose = do
        active <- val pauseButton
        terminationRequested <-
            (if active then id else syncUI) $ do
               pauseTiming $ do
                 askYesNo window "0251 Abort operation?"
        when terminationRequested $ do
          pauseButton =: False
          ignoreErrors$ terminateOperation

  window `onDelete` \e -> do
    askProgramClose
    return True

  cancelButton `onClicked` do
    askProgramClose

  pauseButton `onToggled` do
    active <- val pauseButton
    if active then do takeMVar mvarSyncUI
                      pause_real_secs
                      buttonSetLabel pauseButton =<< i18n"0054   _Continue  "
              else do putMVar mvarSyncUI "mvarSyncUI"
                      resume_real_secs
                      buttonSetLabel pauseButton =<< i18n"0053   _Pause  "

  backgroundButton `onClicked` do
    windowIconify window

  -- Update the window title, the statistics and the progress indicator label every 0.5 seconds
  i' <- ref 0   -- while the progress indicator itself is updated every 0.1 seconds
  indicatorThread 0.1 $ \indicator indType title b bytes total processed p -> postGUIAsync$ do
    i <- val i'; i' += 1; let once_a_halfsecond = (i `mod` 5 == 0)
    -- Window title
    set window [windowTitle := title]                              `on` once_a_halfsecond
    -- Statistics
    updateStats indType b total processed                          `on` once_a_halfsecond
    -- Progress bar and its label
    progressBarSetFraction progressBar processed                   `on` True
    progressBarSetText     progressBar p                           `on` once_a_halfsecond
  backgroundThread 0.5 $ postGUIAsync$ do
    -- Name of the current file, or the stage of the command being executed
    uiMessage' <- val uiMessage
    labelSetText curFileLabel uiMessage'

  -- Clears all the fields holding information about the current archive
  let clearAll = do
        set window [windowTitle := " "]
        clearStats
        labelSetText curFileLabel ""
        progressBarSetFraction progressBar 0
        progressBarSetText     progressBar " "

  -- Off we go!
  widgetGrabFocus pauseButton
  return (window, clearAll)


-- |Creation of the fields for displaying statistics
createStats = do
  textBox <- tableNew 4 6 False
  labels' <- ref []

  -- Create the fields for displaying the current statistics and draw the labels for them
  let newLabel2 x y s = do label1 <- labelNewWithMnemonic =<< i18n s
                           tableAttach textBox label1 (x+0) (x+1) y (y+1) [Expand, Fill] [Expand, Fill] 0 0
                           --set label1 [labelWidthChars := 25]
                           miscSetAlignment label1 0 0

                           label2 <- labelNew Nothing
                           tableAttach textBox label2 (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 10 0
                           set label2 [labelSelectable := True]
                           miscSetAlignment label2 1 0
                           labels' ++= [label2]
                           return [label1,label2]
      -- Returns only the value field
      newLabel x y s  =    newLabel2 x y s >>== (!!1)

  newLabel 2 0 "     "        -- make space between left and right columns
  filesLabel      <- newLabel 0 0 "0056 Files"
  totalFilesLabel <- newLabel 3 0 "0057 Total files"
  bytesLabel      <- newLabel 0 1 "0058 Bytes"
  totalBytesLabel <- newLabel 3 1 "0059 Total bytes"
  ratioLabel      <- newLabel 0 3 "0060 Ratio"
  speedLabel      <- newLabel 3 3 "0061 Speed"
  timesLabel      <- newLabel 0 4 "0062 Time"
  totalTimesLabel <- newLabel 3 4 "0063 Total time"

  compressed      @ [_,      compressedLabel] <- newLabel2 0 2 "0252 Compressed"
  totalCompressed @ [_, totalCompressedLabel] <- newLabel2 3 2 "0253 Total compressed"
  last_cmd' <- ref ""

  -- Procedure that displays the current statistics (indType==INDICATOR_FULL - a full indicator, otherwise only percentages, e.g. for RR operations)
  let updateStats indType b total_b (processed :: Double) = do
        ~UI_State { total_files = total_files
                  , total_bytes = total_bytes
                  , files       = files
                  , cbytes      = cbytes
                  , archive_total_bytes      = archive_total_bytes
                  , archive_total_compressed = archive_total_compressed
                  }  <-  val ref_ui_state
        total_bytes <- return (if indType==INDICATOR_FULL  then total_bytes  else total_b)
        -- Total time since the start of the operation, and the moment when the display of the current progress indicator began
        secs <- return_real_secs
        sec0 <- val indicator_start_real_secs

        -- For add commands a Compressed/Total compressed line is displayed; for the rest it is hidden
        cmd <- val ref_command >>== cmd_name
        let total_compressed
              | cmdType cmd == ADD_CMD              =  "~"++show3 (total_bytes*cbytes `div` b)
              | archive_total_bytes == total_bytes  =       show3 (archive_total_compressed)
              | archive_total_bytes == 0            =       show3 (0)
              | otherwise                           =  "~"++show3 (archive_total_compressed*total_bytes `div` archive_total_bytes)

        labelSetMarkup filesLabel$           bold$ show3 files                                  `on` indType==INDICATOR_FULL
        labelSetMarkup bytesLabel$           bold$ show3 b
        labelSetMarkup compressedLabel$      bold$ show3 cbytes                                 `on` indType==INDICATOR_FULL
        labelSetMarkup totalFilesLabel$      bold$ show3 total_files                            `on` indType==INDICATOR_FULL
        labelSetMarkup totalBytesLabel$      bold$ show3 total_bytes
        labelSetMarkup timesLabel$           bold$ showHMS secs
        when (processed>0.001 && b>0 && secs-sec0>0.001) $ do
        labelSetMarkup totalCompressedLabel$ bold$ total_compressed                             `on` indType==INDICATOR_FULL
        labelSetMarkup ratioLabel$           bold$ ratio2 cbytes b++"%"                         `on` indType==INDICATOR_FULL
        labelSetMarkup totalTimesLabel$      bold$ "~"++showHMS (sec0 + (secs-sec0)/processed)
        labelSetMarkup speedLabel$           bold$ showSpeed b (secs-sec0)

  -- Procedure that clears the current statistics
  let clearStats  =  val labels' >>= mapM_ (`labelSetMarkup` "     ")
  --
  return (textBox, updateStats, clearStats)


-- |Called at the start of processing a file
guiStartFile = doNothing0

-- |Suspend the display of the progress indicator and erase its traces
uiSuspendProgressIndicator = do
  aProgressIndicatorEnabled =: False

-- |Resume the display of the progress indicator and print its current value
uiResumeProgressIndicator = do
  aProgressIndicatorEnabled =: True

-- |Suspend the indicator (if it is running) for the duration of an operation
uiPauseProgressIndicator action =
  bracket (do x <- val aProgressIndicatorEnabled
              aProgressIndicatorEnabled =: False
              return x)
          (\x -> aProgressIndicatorEnabled =: x)
          (\x -> action)

-- |Reset console title
resetConsoleTitle = return ()

-- |Pause progress indicator & timing while dialog runs
myDialogRun dialog  =  uiPauseProgressIndicator$ pauseTiming$ dialogRun dialog


----------------------------------------------------------------------------------------------------
---- User queries ("Overwrite file?" and so on) ----------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE askOverwrite #-}
-- |Query about overwriting a file
askOverwrite filename diskFileSize diskFileTime arcfile ref_answer answer_on_u = do
  (title:file:question) <- i18ns ["0078 Confirm File Replace",
                                  "0165 %1\n%2 bytes\nmodified on %3",
                                  "0162 Destination folder already contains processed file.",
                                  "",
                                  "",
                                  "",
                                  "0163 Would you like to replace the existing file",
                                  "",
                                  "%1",
                                  "",
                                  "",
                                  "0164 with this one?",
                                  "",
                                  "%2"]
  let f1 = formatn file [filename,           show3$ diskFileSize,   formatDateTime$ diskFileTime]
      f2 = formatn file [storedName arcfile, show3$ fiSize arcfile, formatDateTime$ fiTime arcfile]
  ask (format title filename) (formatn (joinWith "\n" question) [f1,f2]) ref_answer answer_on_u

-- |General mechanism for issuing queries to the user
ask title question ref_answer answer_on_u =  do
  old_answer <- val ref_answer
  new_answer <- case old_answer of
                  "a" -> return old_answer
                  "u" -> return old_answer
                  "s" -> return old_answer
                  _   -> ask_user title question
  ref_answer =: new_answer
  case new_answer of
    "u" -> return answer_on_u
    _   -> return (new_answer `elem` ["y","a"])

-- |The actual interaction with the user happens here
ask_user title question  =  gui $ do
  -- Create the dialog
  bracketCtrlBreak "ask_user" (messageDialogNew Nothing [] MessageQuestion ButtonsNone question) widgetDestroy $ \dialog -> do
  set dialog [windowTitle          := title,
              windowWindowPosition := WinPosCenter]
{-
  -- Query to the user
  upbox <- dialogGetUpper dialog
  label <- labelNew$ Just$ question++"?"
  boxPackStart  upbox label PackGrow 0
  widgetShowAll upbox
-}
  -- Buttons for all the possible answers
  hbox <- dialogGetActionArea dialog
  buttonBox <- tableNew 3 3 True
  boxPackStart hbox buttonBox PackGrow 0
  id' <- ref 1
  for (zip [0..] buttons) $ \(y,line) -> do
    for (zip [0..] (split '/' line)) $ \(x,text) -> do
      when (text>"") $ do
      text <- i18n text
      button <- buttonNewWithMnemonic ("  "++text++"  ")
      tableAttachDefaults buttonBox button x (x+1) y (y+1)
      id <- val id'; id' += 1
      dialogAddActionWidget dialog button (ResponseUser id)
  widgetShowAll hbox

  -- Get the answer as a letter: y/n/a/...
  (ResponseUser id) <- myDialogRun dialog
  let answer = (split '/' valid_answers) !! (id-1)
  when (answer=="q") $ do
    terminateOperation
  return answer


-- The answers returned by ask_user and the corresponding button labels, line by line
valid_answers = "y/n/q/a/s/u"
buttons       = ["0079 _Yes/0080 _No/0081 _Cancel"
                ,"0082 Yes to _All/0083 No to A_ll/0084 _Update all"]


----------------------------------------------------------------------------------------------------
---- Password prompting ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Prompt for a password when encrypting/decrypting. Invisible input is used.
-- When encrypting, the password must be entered twice - to guard against typing mistakes
ask_passwords = ( ask_password_dialog "0076 Enter encryption password" 2
                , ask_password_dialog "0077 Enter decryption password" 1
                , doNothing0   -- called when the password is wrong
                )

-- |Password prompt dialog.
ask_password_dialog title' amount opt_parseData = gui $ do
  -- Create a dialog with the standard OK/Cancel buttons
  bracketCtrlBreak "ask_password_dialog" dialogNew widgetDestroy $ \dialog -> do
  title <- i18n title'
  set dialog [windowTitle          := title,
              windowWindowPosition := WinPosCenter]
  okButton <- addStdButton dialog ResponseOk
  addStdButton dialog ResponseCancel

  -- Creates a table with fields for entering one or two passwords
  (pwdTable, [pwd1,pwd2]) <- pwdBox amount
  for [pwd1,pwd2] (`onEntryActivate` buttonClicked okButton)

  -- The OK button fires only if both entered passwords are the same
  onClicked okButton $ do
    p1 <- val pwd1
    p2 <- val pwd2
    when (p1>"" && p1==p2) $ do
      dialogResponse dialog ResponseOk

  -- Add spaces around the table and put it on the form
  set pwdTable [containerBorderWidth := 10]
  upbox <- dialogGetUpper dialog
  boxPackStart  upbox pwdTable PackGrow 0
  widgetShowAll upbox

  choice <- myDialogRun dialog
  if choice==ResponseOk
    then val pwd1
    else terminateOperation >> return ""


{-# NOINLINE ask_passwords #-}

-- |Creates a table with fields for entering one or two passwords
pwdBox amount = do
  pwdTable <- tableNew 2 amount False
  tableSetColSpacings pwdTable 0
  let newField y s = do -- Labels in the left column
                        label <- labelNewWithMnemonic =<< i18n s
                        tableAttach pwdTable label 0 1 (y-1) y [Fill] [Expand, Fill] 5 0
                        miscSetAlignment label 0 0.5
                        -- Password entry fields in the right column
                        pwd <- entryNew
                        set pwd [entryVisibility := False, entryActivatesDefault := True]
                        tableAttach pwdTable pwd 1 2 (y-1) y [Expand, Shrink, Fill] [Expand, Fill] 5 0
                        return pwd
  pwd1 <- newField 1 "0074 Enter password:"
  pwd2 <- if amount==2  then newField 2 "0075 Reenter password:"  else return pwd1
  return (pwdTable, [pwd1,pwd2])


----------------------------------------------------------------------------------------------------
---- Reading/writing archive comments  -------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-# NOINLINE uiPrintArcComment #-}
uiPrintArcComment = doNothing

{-# NOINLINE uiInputArcComment #-}
uiInputArcComment old_comment = gui$ do
  bracketCtrlBreak "uiInputArcComment" dialogNew widgetDestroy $ \dialog -> do
  title <- i18n"0073 Enter archive comment"
  set dialog [windowTitle := title,
              windowDefaultHeight := 200, windowDefaultWidth := 400,
              windowWindowPosition := WinPosCenter]
  addStdButton dialog ResponseOk
  addStdButton dialog ResponseCancel

  commentTextView <- newTextViewWithText old_comment
  upbox <- dialogGetUpper dialog
  boxPackStart upbox commentTextView PackGrow 10
  widgetShowAll upbox

  choice <- myDialogRun dialog
  if choice==ResponseOk
    then textViewGetText commentTextView
    else terminateOperation >> return ""


----------------------------------------------------------------------------------------------------
---- Library ---------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Perform an operation in the GUI thread
gui action = do
  gui <- val guiThread
  my  <- getOsThreadId
  if my==gui  then action else do
  x <- ref Nothing
  y <- postGUISync (action `catch` (\e -> do x=:Just e; return undefined))
  whenJustM (val x) throwIO
  return y

-- |Global variable holding the tooltips of the controls on the current form
tooltips :: IORef Tooltips = unsafePerformIO$ ref$ error "undefined GUI::tooltips"
tooltip w s = do s <- i18n s; t <- val tooltips; tooltipsSetTip t w s ""

-- |Create a control, giving it a localized label and tooltip
i18t title create = do
  (label, t) <- i18n' title
  control <- create label
  tooltip control t  `on`  t/=""
  return control

-- |This instance allows to get/set checkbox state using standard =:/val interface
instance Variable RadioButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set checkbox state using standard =:/val interface
instance Variable ToggleButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set checkbox state using standard =:/val interface
instance Variable CheckButton Bool where
  new  = undefined
  val  = toggleButtonGetActive
  (=:) = toggleButtonSetActive

-- |This instance allows to get/set entry state using standard =:/val interface
instance Variable Entry String where
  new  = undefined
  val  = entryGetText
  (=:) = entrySetText

-- |This instance allows to get/set expander state using standard =:/val interface
instance Variable Expander Bool where
  new  = undefined
  val  = expanderGetExpanded
  (=:) = expanderSetExpanded

-- |This instance allows to get/set expander state using standard =:/val interface
instance Variable TextView String where
  new  = undefined
  val  = textViewGetText
  (=:) = textViewSetText

-- |This instance allows to get/set value displayed by widget using standard =:/val interface
instance GtkWidgetClass w gw a => Variable w a where
  new  = undefined
  val  = getValue
  (=:) = setValue

-- |Universal interface to arbitrary GTK widget `w` that controls value of type `a`
class GtkWidgetClass w gw a | w->gw, w->a where
  widget        :: w -> gw                 -- ^The GTK widget by itself
  getTitle      :: w -> IO String          -- ^Read current widget's title
  setTitle      :: w -> String -> IO ()    -- ^Set current widget's title
  getValue      :: w -> IO a               -- ^Read current widget's value
  setValue      :: w -> a -> IO ()         -- ^Set current widget's value
  setOnUpdate   :: w -> (IO ()) -> IO ()   -- ^Called when user changes widget's value
  onClick       :: w -> (IO ()) -> IO ()   -- ^Called when user clicks button
  saveHistory   :: w -> IO ()
  rereadHistory :: w -> IO ()

data GtkWidget gw a = GtkWidget
 {gwWidget        :: gw
 ,gwGetTitle      :: IO String
 ,gwSetTitle      :: String -> IO ()
 ,gwGetValue      :: IO a
 ,gwSetValue      :: a -> IO ()
 ,gwSetOnUpdate   :: (IO ()) -> IO ()
 ,gwOnClick       :: (IO ()) -> IO ()
 ,gwSaveHistory   :: IO ()
 ,gwRereadHistory :: IO ()
 }

instance GtkWidgetClass (GtkWidget gw a) gw a where
  widget        = gwWidget
  getTitle      = gwGetTitle
  setTitle      = gwSetTitle
  getValue      = gwGetValue
  setValue      = gwSetValue
  setOnUpdate   = gwSetOnUpdate
  onClick       = gwOnClick
  saveHistory   = gwSaveHistory
  rereadHistory = gwRereadHistory

-- |An empty GtkWidget
gtkWidget = GtkWidget { gwWidget        = undefined
                      , gwGetTitle      = undefined
                      , gwSetTitle      = undefined
                      , gwGetValue      = undefined
                      , gwSetValue      = undefined
                      , gwSetOnUpdate   = undefined
                      , gwOnClick       = undefined
                      , gwSaveHistory   = undefined
                      , gwRereadHistory = undefined
                      }

-- Use bold Pango Markup for the given text
bold text = "<b>"++text++"</b>"


{-# NOINLINE eventKey #-}
-- |Returns the full name of a key, for example <Alt><Ctrl>M
eventKey (Key {eventKeyName = name, eventModifier = modifier}) =
  let mshow Shift   = "<Shift>"
      mshow Control = "<Ctrl>"
      mshow Alt     = "<Alt>"
      mshow _       = "<_>"
  --
  in concat ((sort$ map mshow modifier)++[mapHead toUpper name])


{-# NOINLINE addStdButton #-}
-- |Add a standard button with a standard icon to a dialog
addStdButton dialog responseId = do
  let (emsg,item) = case responseId of
                      ResponseYes    -> ("0079 _Yes",    stockYes         )
                      ResponseNo     -> ("0080 _No",     stockNo          )
                      ResponseOk     -> ("0362 _OK",     stockOk          )
                      ResponseCancel -> ("0081 _Cancel", stockCancel      )
                      ResponseClose  -> ("0364 _Close",  stockClose       )
                      _              -> ("???",          stockMissingImage)
  msg <- i18n emsg
  button <- dialogAddButton dialog msg responseId
  image  <- imageNewFromStock item iconSizeButton
  buttonSetImage button image
  return button


{-# NOINLINE debugMsg #-}
-- |Dialog with a debug message
debugMsg msg = do
  bracketCtrlBreak "debugMsg" (messageDialogNew (Nothing) [] MessageError ButtonsClose msg) widgetDestroy $ \dialog -> do
  dialogRun dialog
  return ()

-- |Dialog with an informational message
msgBox window dialogType msg  =  askConfirmation [ResponseClose] window msg  >>  return ()

-- |Ask the user to confirm an operation
askOkCancel = askConfirmation [ResponseOk,  ResponseCancel]
askYesNo    = askConfirmation [ResponseYes, ResponseNo]
{-# NOINLINE askConfirmation #-}
askConfirmation buttons window msg = do
  -- Create a dialog with a single Close button
  bracketCtrlBreak "askConfirmation" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle        := aARC_NAME,
                windowTransientFor := window,
                containerBorderWidth := 10]
    mapM_ (addStdButton dialog) buttons
    -- Print the message in it
    label <- labelNew.Just =<< i18n msg
    upbox <- dialogGetUpper dialog
    label `set` [labelWrap := True]
    boxPackStart  upbox label PackGrow 20
    widgetShowAll upbox
    -- And launch it
    dialogRun dialog >>== (==buttons!!0)

{-# NOINLINE inputString #-}
-- |Ask the user for a string
inputString window msg = do
  -- Create a dialog with the standard OK/Cancel buttons
  bracketCtrlBreak "inputString" dialogNew widgetDestroy $ \dialog -> do
    set dialog [windowTitle        := msg,
                windowTransientFor := window]
    addStdButton dialog ResponseOk      >>= \okButton -> do
    addStdButton dialog ResponseCancel

    --label    <- labelNew$ Just msg
    entry <- entryNew
    entry `onEntryActivate` buttonClicked okButton

    upbox <- dialogGetUpper dialog
    --boxPackStart  upbox label    PackGrow 0
    boxPackStart  upbox entry PackGrow 0
    widgetShowAll upbox

    choice <- dialogRun dialog
    case choice of
      ResponseOk -> val entry >>== Just
      _          -> return Nothing


{-# NOINLINE boxed #-}
-- |Create a control and place it into an hbox
boxed makeControl title = do
  hbox    <- hBoxNew False 0
  control <- makeControl .$i18t title
  boxPackStart  hbox  control  PackNatural 0
  return (hbox, control)


{-# NOINLINE label #-}
-- |Label
label title   =  do (hbox, _) <- boxed labelNewWithMnemonic title
                    return gtkWidget {gwWidget = hbox}


{-# NOINLINE button #-}
-- |Button
button title  =  do
  (hbox, control) <- boxed buttonNewWithMnemonic title
  return gtkWidget { gwWidget   = hbox
                   , gwOnClick  = \action -> onClicked control action >> return ()
                   , gwSetTitle = buttonSetLabel control
                   , gwGetTitle = buttonGetLabel control
                   }


{-# NOINLINE checkBox #-}
-- |Checkbox
checkBox title = do
  (hbox, control) <- boxed checkButtonNewWithMnemonic title
  return gtkWidget { gwWidget      = hbox
                   , gwGetValue    = val control
                   , gwSetValue    = (control=:)
                   , gwSetOnUpdate = \action -> onToggled control action >> return ()
                   }


{-# NOINLINE comboBox #-}
-- |Creates a combo box containing the given set of alternatives
comboBox title labels = do
  hbox  <- hBoxNew False 0
  label <- labelNewWithMnemonic .$i18t title
  combo <- New.comboBoxNewText
  for labels (\l -> New.comboBoxAppendText combo =<< i18n l)
  boxPackStart  hbox  label  PackNatural 5
  boxPackStart  hbox  combo  PackNatural 5
  return gtkWidget { gwWidget      = hbox
                   , gwGetValue    = New.comboBoxGetActive combo >>== fromMaybe 0
                   , gwSetValue    = New.comboBoxSetActive combo
                   }


{-# NOINLINE simpleComboBox #-}
-- |Creates a combo box containing the given set of alternatives
simpleComboBox labels = do
  combo <- New.comboBoxNewText
  for labels (New.comboBoxAppendText combo)
  return combo

{-# NOINLINE makePopupMenu #-}
-- |Creates a popup menu
makePopupMenu action labels = do
  m <- menuNew
  mapM_ (mkitem m) labels
  return m
    where
        mkitem menu label =
            do i <- menuItemNewWithLabel label
               menuShellAppend menu i
               i `onActivateLeaf` (action label)



{-# NOINLINE radioFrame #-}
-- |Creates a frame containing a set of radio buttons and returns that frame
--  plus a procedure for reading the currently selected button
radioFrame title (label1:labels) = do
  -- Create the radio buttons, joining them into a single group
  radio1 <- radioButtonNewWithMnemonic .$i18t label1
  radios <- mapM (\title -> radioButtonNewWithMnemonicFromWidget radio1 .$i18t title) labels
  let buttons = radio1:radios
  -- Pack them vertically and make them run the event stored in the onChanged variable
  vbox <- vBoxNew False 0
  onChanged <- ref doNothing0
  for buttons $ \button -> do boxPackStart vbox button PackNatural 0
                              button `onToggled` do
                                whenM (val button) $ do
                                  val onChanged >>= id
  -- Create a frame around the buttons
  frame <- i18t title $ \title -> do
             frame <- frameNew
             set frame [frameLabel := title.$ deleteIf (=='_'), containerChild := vbox]
             return frame
  return gtkWidget { gwWidget      = frame
                   , gwGetValue    = foreach buttons val >>== fromJust.elemIndex True
                   , gwSetValue    = \i -> (buttons!!i) =: True
                   , gwSetOnUpdate = (onChanged=:)
                   }


{-# NOINLINE twoColumnTable #-}
-- |Two-column table displaying the given labels+data
twoColumnTable dataset = do
  (table, setLabels) <- emptyTwoColumnTable$ map fst dataset
  zipWithM_ ($) setLabels (map snd dataset)
  return table

{-# NOINLINE emptyTwoColumnTable #-}
-- |Two-column table: takes a list of labels for the left column
-- and returns a list of setLabels operations for putting data into the second column
emptyTwoColumnTable dataset = do
  table <- tableNew (length dataset) 2 False
  -- Create the fields for displaying the current statistics and draw the labels for them
  setLabels <- foreach (zip [0..] dataset) $ \(y,s) -> do
      -- First column
      label <- labelNewWithMnemonic =<< i18n s;  let x=0
      tableAttach table label (x+0) (x+1) y (y+1) [Expand, Fill] [Expand, Fill] 0 0
      miscSetAlignment label 0 0     --set label [labelWidthChars := 25]
      -- Second column
      label <- labelNew Nothing
      tableAttach table label (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 10 0
      set label [labelSelectable := True]
      miscSetAlignment label 1 0
      -- Return the operation that sets the text of the second label (the one meant for displaying data)
      return$ \text -> labelSetMarkup label$ bold$ text
  return (table, setLabels)

{-# NOINLINE scrollableTextView #-}
-- |Scrollable TextView
scrollableTextView s attributes = do
  control <- newTextViewWithText s
  set control attributes
  -- Scrolled window where the TextView will be placed
  scrwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
  containerAdd scrwin control
  return gtkWidget { gwWidget   = scrwin
                   , gwGetValue = val control
                   , gwSetValue = (control=:)
                   }

-- |Creates a new TextView object with the given text
newTextViewWithText s = do
  textView <- textViewNew
  textViewSetText textView s
  return textView

-- |Sets the text displayed in the TextView
textViewSetText textView s = do
  buffer <- textViewGetBuffer textView
  textBufferSetText buffer s

-- |Reads the text displayed in the TextView
textViewGetText textView = do
  buffer <- textViewGetBuffer      textView
  start  <- textBufferGetStartIter buffer
  end    <- textBufferGetEndIter   buffer
  textBufferGetText buffer start end False


----------------------------------------------------------------------------------------------------
---- File selection --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)

{-# NOINLINE chooseFile #-}
-- |File selection via a dialog
chooseFile parentWindow dialogType dialogTitle filters getFilename setFilename = do
  title <- i18n dialogTitle
  filename <- getFilename >>== windosifyPath
  -- The filter string consists of (name,patterns) pairs separated by NULL chars, plus an extra NULL char at the end
  filterStr <- prepareFilters filters >>== map (join2 "\0") >>== joinWith "\0" >>== (++"\0")
  withCFilePath title            $ \c_prompt   -> do
  withCFilePath filename         $ \c_filename -> do
  withCFilePath filterStr        $ \c_filters  -> do
  allocaBytes (long_path_size*4) $ \c_outpath  -> do
    result <- case dialogType of
                FileChooserActionSelectFolder  ->  c_BrowseForFolder c_prompt c_filename c_outpath
                _                              ->  c_BrowseForFile   c_prompt c_filters c_filename c_outpath
    when (result/=0) $ do
       setFilename =<< peekCFilePath c_outpath

foreign import ccall safe "Environment.h BrowseForFolder"  c_BrowseForFolder :: CFilePath -> CFilePath -> CFilePath -> IO CInt
foreign import ccall safe "Environment.h BrowseForFile"    c_BrowseForFile   :: CFilePath -> CFilePath -> CFilePath -> CFilePath -> IO CInt


guiFormatDateTime t = unsafePerformIO $ do
  allocaBytes 1000 $ \buf -> do
  c_GuiFormatDateTime t buf 1000 nullPtr nullPtr
  peekCString buf

foreign import ccall safe "Environment.h GuiFormatDateTime"
  c_GuiFormatDateTime :: CTime -> CString -> CInt -> CString -> CString -> IO ()

#else

{-# NOINLINE chooseFile #-}
-- |File selection via a dialog
chooseFile parentWindow dialogType dialogTitle filters getFilename setFilename = do
  title <- i18n dialogTitle
  filename <- getFilename
  [select,cancel] <- i18ns ["0363 _Select", "0081 _Cancel"]
  bracketCtrlBreak "chooseFile" (fileChooserDialogNew (Just title) (Just$ castToWindow parentWindow) dialogType [(select,ResponseOk), (cancel,ResponseCancel)]) widgetDestroy $ \chooserDialog -> do
    fileChooserSetFilename chooserDialog (unicode2utf8 filename)
    case dialogType of
      FileChooserActionSave -> fileChooserSetCurrentName chooserDialog (takeFileName filename)
      _                     -> fileChooserSetFilename    chooserDialog (unicode2utf8 filename++"/non-existing-file") >> return ()
    prepareFilters filters >>= addFilters chooserDialog
    choice <- dialogRun chooserDialog
    when (choice==ResponseOk) $ do
      whenJustM_ (fileChooserGetFilename chooserDialog) $ \filename -> do
        setFilename (utf8_to_unicode filename)

{-# NOINLINE addFilters #-}
-- |Set the filters for file selection
addFilters chooserDialog filters = do
  for filters $ \(text, patterns) -> do
    filt <- fileFilterNew
    fileFilterSetName filt text
    for (patterns.$ split ';')  (fileFilterAddPattern filt)
    fileChooserAddFilter chooserDialog filt

guiFormatDateTime = formatDateTime

#endif


-- |Prepare the filters for use in the dialog
prepareFilters filters = do
  foreach (filters &&& filters++["0309 All files (*)"]) $ \element -> do
    text <- i18n element
    let patterns = text .$words .$last .$drop 1 .$dropEnd 1
    return (text, patterns)

