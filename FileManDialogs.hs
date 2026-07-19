{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: Extract/ArcInfo/Settings dialogs                                ------
----------------------------------------------------------------------------------------------------
module FileManDialogs where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Cmd
#if defined(FREEARC_WIN)
import System.Win32
#endif

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets
import Compression
import Encryption
import Options
import UIBase
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import FileManPanel
import FileManUtils

----------------------------------------------------------------------------------------------------
---- File extraction dialog ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

extractDialog fm' exec cmd arcnames arcdir files = do
  fm <- val fm'
  title <- i18n$ case (cmd, files, arcnames) of
                   ("t", [],     [])        -> "0157 Test all archives"
                   ("t", [],     [arcname]) -> "0152 Test %3"
                   ("t", [file], [arcname]) -> "0153 Test %1 from %3"
                   ("t", files,  [arcname]) -> "0154 Test %2 files from %3"
                   ("t", files,  arcnames)  -> "0155 Test %4 archives"
                   (_,   [],     [])        -> "0158 Extract all archives"
                   (_,   [],     [arcname]) -> "0024 Extract files from %3"
                   (_,   [file], [arcname]) -> "0025 Extract %1 from %3"
                   (_,   files,  [arcname]) -> "0026 Extract %2 files from %3"
                   (_,   files,  arcnames)  -> "0027 Extract files from %4 archives"
  let wintitle  =  formatn title [head files, show3$ length files, takeFileName$ head arcnames, show3$ length arcnames]
  -- Create a dialog with the standard OK/Cancel buttons
  fmDialog fm' wintitle $ \(dialog,okButton) -> do
    upbox <- dialogGetUpper dialog
    overwrite <- radioFrame "0005 Overwrite mode"
                            [ "0001 Ask before overwrite",
                              "0002 Overwrite without prompt",
                              "0003 Update old files",
                              "0051 Skip existing files" ]
    ; boxPackStart upbox (widget overwrite) PackNatural 5         `on` cmd/="t"

    ; outFrame <- frameNew
    ; boxPackStart upbox outFrame           PackNatural 5         `on` cmd/="t"
    ;   vbox <- vBoxNew False 0
    ;   set outFrame [containerChild := vbox, containerBorderWidth := 5]
    (hbox, _, dir) <- fmFileBox fm' dialog
                                "dir" FileChooserActionSelectFolder
                         (label "0004 Output directory:")
                                "0021 Select output directory"
                                []
                                (const$ return True)
                                (fmCanonicalizeDiskPath fm')
    ; boxPackStart vbox hbox                  PackNatural 0
    addDirButton <- checkBox "0014 Append archive name to the output directory"
    ; boxPackStart vbox (widget addDirButton) PackNatural 0

    (decryption, decryptionOnOK) <- decryptionBox fm' dialog   -- Decryption settings
    ; boxPackStart upbox decryption           PackNatural 5

    (hbox, options, optionsStr)  <- fmCheckedEntryWithHistory fm' "xoptions" "0072 Additional options:"
    ; boxPackStart upbox hbox                 PackNatural 5


    -- Set the output directory to its default value
    case arcnames of
      [arcname] -> dir =: takeBaseName arcname
      _         -> do dir=:"."; addDirButton=:True


    widgetShowAll upbox
    choice <- fmDialogRun fm' dialog (if cmd/="t" then "ExtractDialog" else "TestDialog")
    windowPresent (fm_window fm)
    when (choice==ResponseOk) $ do
      overwriteOption    <- val overwrite
      dir'               <- val dir;         saveHistory dir
      isAddDir           <- val addDirButton
      decryptionOptions  <- decryptionOnOK
      logfile'           <- fmGetHistory1 fm' "logfile" ""
      optionsEnabled     <- val options
      ; optionsStr'        <- val optionsStr;  saveHistory optionsStr  `on`  optionsEnabled
      let msgs = case cmd of
                  "t" -> ["0231 Testing %1",
                          "0232 SUCCESFULLY TESTED %1",
                          "0233 %2 WARNINGS WHILE TESTING %1"]
                  _   -> ["0234 Extracting files from %1",
                          "0235 FILES SUCCESFULLY EXTRACTED FROM %1",
                          "0236 %2 WARNINGS WHILE EXTRACTING FILES FROM %1"]
      exec$ (arcnames ||| ["*"]) .$map (\arcname ->
            (msgs, [takeFileName arcname],
             [cmd]++
             (cmd/="t" &&& (
               ["-dp"++clear dir']++
               (isAddDir &&& ["-ad"])++
               (arcdir &&& files &&& ["-ap"++clear arcdir])++
               (overwriteOption  `select`  ",-o+,-u -o+,-o-")))++
             decryptionOptions++
             (logfile'         &&&  ["--logfile="++clear logfile'])++
             ["--fullnames"]++
             ["--noarcext"]++
             (optionsEnabled   &&&  words (clear optionsStr'))++
             ["--", clear arcname]++files))


----------------------------------------------------------------------------------------------------
---- Archive information dialog --------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

arcinfoDialog fm' exec mode arcnames arcdir files = do
  handle (\e -> fmErrorMsg fm' "0013 There are no archives selected!") $ do
  fm <- val fm'
  let arcname = head arcnames
  fm_arc <- case () of _ | isFM_Archive fm -> return (subfm fm)
                         | otherwise       -> with' (newFMArc fm' arcname "") (return) closeFMArc
  let archive = subfm_archive fm_arc
  title <- i18n"0085 All about %1"
  let wintitle  =  format title (takeFileName arcname)
  -- Create a dialog with the standard OK/Cancel buttons
  fmDialog fm' wintitle $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ Main tab ------------------------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n

    let filelist    = map (cfFileInfo)$ arcDirectory archive
        footer      = arcFooter archive
        dataBlocks  = arcDataBlocks archive        -- list of solid blocks
        numOfBlocks = length dataBlocks
        empty       = "-"
    ;   yes        <- i18n"0101 Yes" >>== replaceAll "_" ""

    let origsize = sum$ map blOrigSize dataBlocks  -- total size of the files when unpacked
        compsize = sum$ map blCompSize dataBlocks  -- total size of the files when packed
        getCompressors = partition isEncryption.blCompressor  -- split the encryption and compression algorithms for the block
        (encryptors, compressors) = unzip$ map getCompressors dataBlocks  -- list of the encryption and compression algorithms.
        header_encryptors = deleteIf null$ map (fst.getCompressors) (ftBlocks footer)  -- encryption algorithms of the service blocks
        all_encryptors = deleteIf null encryptors ++ header_encryptors   -- and now all of them together :)
        ciphers = joinWith "\n"$ removeDups$ map (join_compressor.map method_name) all_encryptors   -- names of the encryption algorithms.
        formatMem s  =  x++" "++y  where (x,y) = span isDigit$ showMem s

    let -- Maximum dictionaries of the main and auxiliary algorithms
        dicts = compressors.$ map (splitAt 1.reverse.map getDictionary)  -- [([mainDict],[auxDict1,auxDict2..])...]
                           .$ map (\([x],ys) -> (x, maximum(0:ys)))      -- [(mainDict,maxAuxDict)...]
                           .$ ((0,0):) .$ sort .$ last                   -- Pick the row with the largest main and auxiliary dictionary
        dictionaries  =  case dicts of
                           (0,0)                     -> empty
                           (maxMainDict, 0)          -> formatMem maxMainDict
                           (maxMainDict, maxAuxDict) -> showMem maxAuxDict++" + "++showMem maxMainDict

    pack 10 $twoColumnTable [("0173 Directories:",      show3$ ftDirs$  subfm_filetree fm_arc)
                            ,("0088 Files:",            show3$ ftFiles$ subfm_filetree fm_arc)
                            ,("0089 Total bytes:",      show3$ origsize)
                            ,("0090 Compressed bytes:", show3$ compsize)
                            ,("0091 Ratio:",            ratio3 compsize origsize++"%")]

    pack  0 $twoColumnTable [("0104 Directory blocks:", show3$ length$ filter ((DIR_BLOCK==) . blType) (ftBlocks footer))
                            ,("0092 Solid blocks:",     show3$ numOfBlocks)
                            ,("0093 Avg. blocksize:",   formatMem$ origsize `div` i(max numOfBlocks 1))]

    pack 10 $twoColumnTable [("0099 Compression memory:",    formatMem$ maximum$ 0: map compressionGetShrinkedCompressionMem   compressors)
                            ,("0100 Decompression memory:",  formatMem$ maximum$ 0: map compressionGetShrinkedDecompressionMem compressors)
                            ,("0105 Dictionary:",            dictionaries)]

    pack  0 $twoColumnTable [("0094 Archive locked:",    ftLocked footer   &&& yes ||| empty)
                            ,("0098 Archive comment:",   ftComment footer  &&& yes ||| empty)
                            ,("0095 Recovery info:",     ftRecovery footer ||| empty)
                            ,("0096 SFX size:",          ftSFXSize footer .$show3 .$changeTo [("0", empty)])
                            ,("0156 Headers encrypted:", header_encryptors &&& yes ||| empty)]

    table <- twoColumnTable [("0097 Encryption algorithms:",  ciphers ||| empty)]
    boxPackStart vbox table PackNatural (ciphers &&& 10)


------ Archive comment tab -------------------------------------------------------------------------
    vbox <- newPage "0199 Comment"

    comment <- scrollableTextView (ftComment footer) []
    boxPackStart vbox (widget comment) PackGrow 0

    widgetShowAll dialog
    notebookSetCurrentPage nb 1    `on` mode==CommentMode
    choice <- fmDialogRun fm' dialog "ArcInfoDialog"
    windowPresent (fm_window fm)
    when (choice==ResponseOk) $ do
      newComment <- val comment
      when (newComment /= ftComment footer) $ do
        let msgs = ["0237 Modifying %1",
                    "0238 SUCCESFULLY MODIFIED %1",
                    "0239 %2 WARNINGS WHILE MODIFYING %1"]
        let cmd  = ["ch"
                   ,"--noarcext"
                   ,newComment &&& ("--archive-comment="++newComment)
                               ||| "-z-"
                   ,"--"
                   ,arcname]
        --
        exec [(msgs, [takeFileName arcname], cmd)]


----------------------------------------------------------------------------------------------------
---- Program settings dialog -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

settingsDialog fm' = do
  fm <- val fm'
  fmDialog fm' "0067 Settings" $ \(dialog,okButton) -> do
    (nb,newPage) <- startNotebook dialog
------ Main tab ------------------------------------------------------------------------------
    vbox <- newPage "0174 Main";  let pack x = boxPackStart vbox x PackNatural 1
    aboutLabel         <- labelNewWithMnemonic aARC_HEADER_WITH_DATE
    langLabel          <- label "0068 Language:"
    langComboBox       <- New.comboBoxNewText
    editLangButton     <- button "0069 Edit"
    convertLangButton  <- button "0070 Import"
    -- Logfile
    (logfileBox, _, logfile) <- fmFileBox fm' dialog
                                          "logfile" FileChooserActionSave
                                   (label "0166 Logfile:")
                                          "0167 Select logfile"
                                          []
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
    ; viewLogfileButton <- button "0292 View"
    -- Miscellaneous
    toolbarTextButton   <- fmCheckButtonWithHistory fm' "ToolbarCaptions" True "0361 Add captions to toolbar buttons"
    checkNewsButton     <- fmCheckButtonWithHistory fm' "CheckNews"       True "0370 Watch for new versions via Internet"
    registerButton      <- button "0172 Associate FreeArc with .arc files"
    notes               <- label . joinWith "\n" =<<
      i18ns["0168 You should restart FreeArc in order for a language settings to take effect.",
            "0169 Passwords need to be entered again after restart."]

-----------------------------------------------------------------------------------------------
    -- Information about the current localization language
    langTable <- tableNew 2 2 False
    let dataset = [("0170 Full name:", "0000 English"), ("0171 Copyright:", "0159 ")]
    labels <- foreach [0..1] $ \y -> do
      -- First column
      label1 <- labelNew Nothing;  let x=0
      tableAttach langTable label1 (x+0) (x+1) y (y+1) [Fill] [Fill] 5 5
      miscSetAlignment label1 0 0
      -- Second column
      label2 <- labelNew Nothing
      tableAttach langTable label2 (x+1) (x+2) y (y+1) [Expand, Fill] [Expand, Fill] 5 5
      set label2 [labelSelectable := True]
      miscSetAlignment label2 0 0
      return (label1, label2)
    --
    let showLang i18n = do
          for (zip labels dataset) $ \((l1,l2),(s1,s2)) -> do
            labelSetTextWithMnemonic l1      =<< i18n s1
            labelSetMarkup           l2.bold =<< i18n s2
    --
    showLang i18n

    -- Current settings
    inifile  <- io$ findFile configFilePlaces aINI_FILE
    settings <- inifile  &&&  io(readConfigFile inifile) >>== map (split2 '=')
    let langFile =  settings.$lookup aINITAG_LANGUAGE `defaultVal` ""

    -- Fill the language list with the file names in the arc.languages directory and select the active language
    langDir   <- io$ findDir libraryFilePlaces aLANG_DIR
    langFiles <- langDir &&& (io(dir_list langDir) >>== map baseName >>== sort >>== filter (match "arc.*.txt"))
    -- Display the languages in 5 columns, sorted by column
    let cols = 5
    ;   langComboBox `New.comboBoxSetWrapWidth` cols
    let rows = (length langFiles) `divRoundUp` cols;  add = rows*cols - length langFiles
        sortOnColumn x  =  r*cols+c  where (c,r) = x `divMod` rows  -- recompute from column-wise positions into row-wise ones
    ;   langFiles <- return$ map snd $ sort $ zip (map sortOnColumn [0..]) (langFiles ++ replicate add "")
    --
    for langFiles (New.comboBoxAppendText langComboBox . mapHead toUpper . replace '_' ' ' . dropEnd 4 . drop 4)
    whenJust_ (elemIndex (takeFileName langFile) langFiles)
              (New.comboBoxSetActive langComboBox)

    -- Determine the localization file corresponding to the language selected in the combobox
    let getCurrentLangFile = do
          lang <- New.comboBoxGetActive langComboBox
          case lang of
            Just lang -> myCanonicalizePath (langDir </> (langFiles !! lang))
            Nothing   -> return ""

    -- When a different localization language is selected, display information about it
    langComboBox `New.onChanged` do
      whenJustM_ (New.comboBoxGetActive langComboBox) $ \_ -> do
        langFile   <- getCurrentLangFile
        localeInfo <- parseLocaleFile langFile
        showLang (i18n_general (return localeInfo) .>>== fst)

    -- Editing the current localization file/logfile
    editLangButton    `onClick` (runEditCommand =<< getCurrentLangFile)
    viewLogfileButton `onClick` (runViewCommand =<< val logfile)

#if defined(FREEARC_WIN)
    registerButton `onClick` do
      exe <- getExeName                                -- Name of FreeArc.exe file
      let ico   =  exe `replaceExtension` ".ico"       -- Name of FreeArc.ico file
          empty =  exe `replaceFileName` "empty.arc"   -- Name of empty archive file
          register = registrySetStr hKEY_CLASSES_ROOT
          regCmd name msg cmdline = do
              register ("FreeArc.arc\\shell\\"++name) "" msg
              register ("FreeArc.arc\\shell\\"++name++"\\command") "" ("\""++exe++"\" "++cmdline++" --noarcext -- \"%1\"")
      register ".arc" "" "FreeArc.arc"
      register ".arc\\ShellNew" "FileName" empty
      register "FreeArc.arc" "" "FreeArc archive"
      register "FreeArc.arc\\DefaultIcon" "" (ico++",0")
      register "FreeArc.arc\\shell" "" "open"
      register "FreeArc.arc\\shell\\open\\command" "" ("\""++exe++"\" \"%1\"")
      regCmd   "extract-folder" "Extract to new folder" "x -ad"
      regCmd   "extract-here"   "Extract here"          "x"
      regCmd   "test"           "Test"                  "t"
      -- todo: extract to ...;    *: add to archive; add to ...; choice of the caption and of the user's profile/options
      return ()
#endif

    ; langFrame <- frameNew
    ;   vbox1 <- vBoxNew False 0
    ;   set langFrame [containerChild := vbox1, containerBorderWidth := 5]
    ;     langbox <- hBoxNew False 0
    boxPackStart langbox    (widget  langLabel)          PackNatural 0
    boxPackStart langbox             langComboBox        PackGrow    5
    boxPackStart langbox    (widget  editLangButton)     PackNatural 5
    --boxPackStart langbox    (widget  convertLangButton)  PackNatural 5
    boxPackStart vbox1               langbox             PackNatural 5
    boxPackStart vbox1               langTable           PackNatural 5
    boxPackStart logfileBox (widget  viewLogfileButton)  PackNatural 5
    boxPackStart vbox                aboutLabel          PackNatural 5
    boxPackStart vbox                langFrame           PackNatural 5
    boxPackStart vbox                logfileBox          PackNatural 5
    boxPackStart vbox       (widget  toolbarTextButton)  PackNatural 5
    boxPackStart vbox       (widget  checkNewsButton)    PackNatural 5
    boxPackStart vbox       (widget  registerButton)     PackNatural 5   `on` isWindows
    boxPackStart vbox       (widget  notes)              PackNatural 5

------ Compression tab ------------------------------------------------------------------------
    (_, saveCompressionHistories) <- compressionPage fm' =<< newPage "0106 Compression"

------ Encryption tab -------------------------------------------------------------------------
    (_, saveEncryptionHistories)  <-  encryptionPage fm' dialog okButton =<< newPage "0119 Encryption"

------ System information tab -----------------------------------------------------------------
    vbox <- newPage "0388 Info";  let pack n makeControl = do control <- makeControl
                                                              boxPackStart vbox control PackNatural n

    maxBlock <- getMaxMemToAlloc
    pack 10 $twoColumnTable [("0389 Largest contiguous free memblock:", showMem (maxBlock `roundDown` mb))]


-----------------------------------------------------------------------------------------------
    widgetShowAll dialog
    choice <- fmDialogRun fm' dialog "SettingsDialog"
    windowPresent (fm_window fm)
    when (choice==ResponseOk) $ do
      -- Save the settings to the INI file, the passwords - in global variables, the keyfile - in the history
      langFile <- getCurrentLangFile
      inifile  <- io$ findOrCreateFile configFilePlaces aINI_FILE
      io$ buildPathTo inifile
      io$ saveConfigFile inifile$ map (join2 "=") [(aINITAG_LANGUAGE, takeFileName langFile)]
      logfile' <- val logfile;  saveHistory logfile
      saveHistory `mapM_` [toolbarTextButton, checkNewsButton]
      saveCompressionHistories
      saveEncryptionHistories ""
      return ()


----------------------------------------------------------------------------------------------------
---- Compression tab -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

compressionPage fm' vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  -- Compression algorithm.
  (hbox, cmethod) <- fmLabeledEntryWithHistory fm' "compression" "0175 Compression profile:";  pack hbox
  ; save <- button "0178 Save";  boxPackStart hbox (widget save) PackNatural 5

  -- Compression algorithm settings.
  hbox <- hBoxNew False 0;  pack hbox
  ; vbox1 <- vBoxNew False 0
  ;   method <- radioFrame "0107 Compression level" levels
  ;     boxPackStart vbox1 (widget method)  PackNatural 0
  ;   xMethod <- checkBox "0113 Fast, low-memory decompression"
  ;     boxPackStart vbox1 (widget xMethod) PackNatural 0
  ;   autodetect <- checkBox "0176 Filetype auto-detection"
  ;     boxPackStart vbox1 (widget autodetect) PackNatural 1
  ;   boxPackStart hbox vbox1 PackNatural 5
  ; methodText <- labelNew Nothing
  ;   boxPackStart hbox methodText PackGrow 0

  -- Solid block size settings
  vbox1 <- vBoxNew False 0;  let pack1 x = boxPackStart vbox1 x PackNatural 1
  ; (hbox, solidBytesOn, solidBytes) <- fmCheckedEntryWithHistory fm' "bytes" "0138 Bytes, no more than:";  pack1 hbox
  ; (hbox, solidFilesOn, solidFiles) <- fmCheckedEntryWithHistory fm' "files" "0139 Files, no more than:";  pack1 hbox
  ; solidByExtension                 <- checkBox                              "0140 Split by extension" ;  pack1 (widget solidByExtension)
  solidBlocksFrame <- frameNew;  pack solidBlocksFrame;  s <- i18n"0177 Limit solid blocks"
  set solidBlocksFrame [containerChild := vbox1, frameLabel := s, containerBorderWidth := 5]

  -- Field initialization
  let m=4; x=False
  method  =: (6-m) .$ clipTo 0 5
  xMethod =: x
  autodetect =: True

  -- Publish the description of the initially selected compression method and update it whenever the settings change
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))
  let getSimpleMethod = do
        m <- val method
        x <- val xMethod
        return$ show(if m==0 then 9 else 6-m)++(x.$bool "" "x")
  let describeMethod = do
        m <- val method
        x <- val xMethod
        methodName <- getSimpleMethod
        let compressor = methodName.$ decode_method []
                                   .$ limitCompressionMem   (parsePhysMem "75%")
                                   .$ limitDecompressionMem (1*gb)
            cmem = compressor.$ compressorGetShrinkedCompressionMem
            dmem = compressor.$ compressorGetShrinkedDecompressionMem
        let level  =         "      ccm      uharc     7-zip        rar        ace      zip"
            cspeed = x.$bool "    3mb/s      3mb/s     4mb/s     10mb/s     20mb/s   50mb/s" --m9,m5..m1
                             "  2.5mb/s    2.5mb/s     4mb/s      8mb/s     15mb/s   50mb/s" --m9x,m5x..m1x
            dspeed = x.$bool " 4-40mb/s   4-40mb/s  4-40mb/s     25mb/s     40mb/s  100mb/s" --m9,m5..m1
                             "   40mb/s     40mb/s    40mb/s     40mb/s     60mb/s  100mb/s" --m9x,m5x..m1x
        labelSetMarkup methodText . deleteIf (=='_') . unlines =<< mapM i18fmt
            [ ["0114 Compression level: %1",               bold((words level!!m).$replace '_' ' ')]
            , ["0115 Compression speed: %1, memory: %2",   bold(words cspeed!!m), bold(showMem cmem)]
            , ["0116 Decompression speed: %1, memory: %2", bold(words dspeed!!m), bold(showMem dmem)]
            , [""]
            , ["0390 All speeds were measured on 3GHz Core2Duo"]]
        w1 <- i18n (levels!!m)
        w2 <- i18n "0226 (fast, low-memory decompression)"
        autodetect'   <- val autodetect
        solidBytesOn' <- val solidBytesOn
        solidBytes'   <- val solidBytes
        solidFilesOn' <- val solidFilesOn
        solidFiles'   <- val solidFiles
        solidByExtension' <- val solidByExtension
        let s = (solidBytesOn'     &&&  solidBytes')++
                (solidFilesOn'     &&& (solidFiles'++"f"))++
                (solidByExtension' &&&  "e")
        cmethod =: w1++(x&&&" "++w2)++": "++"-m"++(methodName.$changeTo [("9","x")])++(not autodetect' &&& " -ma-")++(s &&& " -s"++s)
  --
  describeMethod
  describeMethod .$ setOnUpdate xMethod
  describeMethod .$ setOnUpdate method
  describeMethod .$ setOnUpdate autodetect
  describeMethod .$ setOnUpdate solidBytesOn
  describeMethod .$ setOnUpdate solidBytes
  describeMethod .$ setOnUpdate solidFilesOn
  describeMethod .$ setOnUpdate solidFiles
  describeMethod .$ setOnUpdate solidByExtension

  -- Saving the history of the string fields and handling the Save button press
  let saveHistories = do
        whenM (val solidBytesOn) $ do saveHistory solidBytes
        whenM (val solidFilesOn) $ do saveHistory solidFiles
  save `onClick` do saveHistories; saveHistory cmethod

  -- Return the method for attaching a reaction to a compression setting change, and the procedure executed when OK is pressed
  return (\act -> setOnUpdate cmethod (val cmethod >>= act), saveHistories)

{-
    let simpleMethod = initSetting "simpleMethod" `defaultVal` "4"
        m =  case (take 1 simpleMethod) of [d] | isDigit d -> digitToInt d
                                           _               -> 4
        x =  drop 1 simpleMethod == "x"

      ; solidBytes'        <- val solidBytes;  solidBytes' !~ "*b"   &&&   solidBytes =: solidBytes'++"b"
      simpleMethod' <- getSimpleMethod
-}

-- |Compression level names
levels = [ "0108 Maximum",
           "0109 High",
           "0110 Normal",
           "0111 Fast",
           "0112 Very fast",
           "0127 HDD-speed"]


----------------------------------------------------------------------------------------------------
---- Encryption tab ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

encryptionPage fm' dialog okButton vbox = do
  let pack x = boxPackStart vbox x PackNatural 1
  (hbox, pwds)  <-  pwdBox 2;  pack hbox   -- Creates a table with fields for entering two passwords

  -- Encryption frame.
  vbox1 <- vBoxNew False 0
  frame <- frameNew;  s <- i18n"0119 Encryption"
  set frame [containerChild := vbox1, frameLabel := s, containerBorderWidth := 5]
  let pack1 x = boxPackStart vbox1 x PackNatural 1
  boxPackStart vbox frame        PackNatural 10

  -- Encryption algorithm.
  (hbox, method) <- fmLabeledEntryWithHistory fm' "encryption" "0179 Encryption profile:";  pack1 hbox
  ; save <- button "0180 Save";  boxPackStart hbox (widget save) PackNatural 0

  -- Encryption settings.
  encryptHeaders <- checkBox "0120 Encrypt archive directory";  pack1 (widget encryptHeaders)
  usePwd <- checkBox "0181 Use password";  pack1 (widget usePwd)
  (hbox, keyfileOn, keyfile) <- fmFileBox fm' dialog
                                          "akeyfile" FileChooserActionOpen
                                (checkBox "0123 Keyfile:")
                                          "0124 Select keyfile"
                                          []
                                          (const$ return True)
                                          (fmCanonicalizeDiskPath fm')
  ; createKeyfile <- button "0125 Create"
  ; boxPackStart hbox (widget createKeyfile) PackNatural 0;  pack1 hbox
  (hbox, encAlg) <- fmLabeledEntryWithHistory fm' "encryptor" "0121 Encryption algorithm:";  pack1 hbox

  -- Decryption settings
  (decryption, decryptionOnOK) <- decryptionBox fm' dialog
  ; boxPackStart vbox decryption        PackNatural 10

  -- Allow OK to be pressed only if the two entered passwords are the same
  let [pwd1,pwd2] = pwds
  for pwds $ flip afterKeyRelease $ \e -> do
    [pwd1', pwd2'] <- mapM val pwds
    okButton `widgetSetSensitivity` (pwd1'==pwd2')
    return False

  -- Create a new keyfile by writing cryptographically random data into the file specified by the user
  createKeyfile `onClick` do
    title <- i18n "0126 Create new keyfile"
    bracketCtrlBreak "createKeyfile" (fileChooserDialogNew (Just title) (Just$ castToWindow dialog) FileChooserActionSave [("OK",ResponseOk), ("Cancel",ResponseCancel)]) widgetDestroy $ \chooserDialog -> do
      fileChooserSetFilename    chooserDialog =<< (fmCanonicalizeDiskPath fm' "new.key" >>== unicode2utf8)
      fileChooserSetCurrentName chooserDialog "new.key"
      fileChooserSetFilename    chooserDialog =<< (fmCanonicalizeDiskPath fm' "new.key" >>== unicode2utf8)
      fileChooserSetDoOverwriteConfirmation chooserDialog True
      choice <- dialogRun chooserDialog
      windowPresent dialog
      when (choice==ResponseOk) $ do
        whenJustM_ (fileChooserGetFilename chooserDialog) $ \fn8 -> do
          let filename = utf8_to_unicode fn8
          filePutBinary filename =<< generateRandomBytes 1024
          keyfile   =: filename
          keyfileOn =: True

  -- Initialization: read the passwords from the global variables
  pwd1 =:: val encryptionPassword
  pwd2 =:: val encryptionPassword

  -- Saving the history of the string fields and handling the Save button press
  let saveHistories = do
        whenM (val keyfileOn) $ do saveHistory keyfile
        saveHistory encAlg
  save `onClick` do saveHistories; saveHistory method

  -- Actions performed when OK is pressed. Returns the options that need to be added to the command line
  let onOK encryption = do
        saveHistories
        pwd' <- val pwd1;  encryptionPassword =: pwd'
        decryptionOptions <- decryptionOnOK
        return$ decryptionOptions ++ ((words encryption `contains` "-p?") &&& pwd' &&& ["-p"++pwd'])

  -- Builds the encryption profile; called whenever any option in this frame changes
  let makeProfile = do
        usePwd'         <- val usePwd
        keyfileOn'      <- val keyfileOn
        keyfile'        <- val keyfile
        encAlg'         <- val encAlg
        encryptHeaders' <- val encryptHeaders
        method =: unwords( (encryptHeaders' &&& ["-hp"])++
                           (usePwd'         &&& ["-p?"])++
                                                ["--encryption="++clear encAlg']++
                           (keyfileOn'      &&& ["--keyfile="   ++clear keyfile']))
  --
  makeProfile
  makeProfile .$ setOnUpdate usePwd
  makeProfile .$ setOnUpdate keyfileOn
  makeProfile .$ setOnUpdate keyfile
  makeProfile .$ setOnUpdate encAlg
  makeProfile .$ setOnUpdate encryptHeaders

  -- Return the method for attaching a reaction to encryption setting changes, and the procedure executed when OK is pressed
  return (\act -> setOnUpdate method (val method >>= act), onOK)


----------------------------------------------------------------------------------------------------
---- Decryption frame ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

decryptionBox fm' dialog = do
  vbox <- vBoxNew False 0
  decryptionFrame <- frameNew;  s <- i18n"0144 Decryption"
  set decryptionFrame [containerChild := vbox, frameLabel := s, containerBorderWidth := 5]

  lbl <- label "0074 Enter password:"
  pwd <- entryNew   --newTextViewWithText
  ; set pwd [entryVisibility := False, entryActivatesDefault := True]
  (keyfileBox, _, keyfile) <- fmFileBox fm' dialog
                                        "keyfile" FileChooserActionOpen
                                 (label "0123 Keyfile:")
                                        "0124 Select keyfile"
                                        []
                                        (const$ return True)
                                        (fmCanonicalizeDiskPath fm')
  hbox <- hBoxNew False 0
  ; boxPackStart hbox (widget lbl) PackNatural 0
  ; boxPackStart hbox pwd          PackGrow    5
  boxPackStart vbox hbox       PackNatural 0
  boxPackStart vbox keyfileBox PackNatural 5
  -- Read the passwords from the global variables
  pwd =:: val decryptionPassword
  -- Action performed when OK is pressed. Returns the options that need to be added to the command line
  let onOK = do
        pwd'     <- val pwd;      decryptionPassword =: pwd'
        keyfile' <- val keyfile;  saveHistory keyfile
        return$ (pwd'      &&&  ["-op"++pwd'])++
                (keyfile'  &&&  ["--OldKeyfile="++clear keyfile'])

  return (decryptionFrame, onOK)


----------------------------------------------------------------------------------------------------
---- Auxiliary definitions -------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Perform an operation on the selected files in an archive / on all files in the selected archives
archiveOperation fm' action = do
  fm <- val fm'
  files <- getSelection fm' (if isFM_Archive fm  then xCmdFiles  else const [])
  if isFM_Archive fm
    then action [fm_arcname fm] (fm_arcdir fm) files
    else do fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames "" []

-- |Performs an operation that only needs to be given the archive names
multiArchiveOperation fm' action = do
  fm <- val fm'
  if isFM_Archive fm
    then action [fm_arcname fm]
    else do files <- getSelection fm' (const [])
            fullnames <- mapM (fmCanonicalizePath fm') files
            action fullnames

-- |Refresh the contents of the file-manager panel with up-to-date data
refreshCommand fm' = do
  fm <- val fm'
  curfile <- fmGetCursor fm'
  selected <- getSelection fm' (:[])
  -- Refresh the contents of the directory/archive and restore the current file and the list of marked ones
  chdir fm' (fm_current fm)
  when (selected>[]) $ do
    fmSetCursor fm' curfile
  fmUnselectFilenames fm' (const True)
  fmSelectFilenames   fm' ((`elem` selected) . fmname)

-- |View a file
runViewCommand           = runEditCommand

-- |Edit a file
runEditCommand filename  = run (iif isWindows "notepad" "gedit") [filename]
  where run cmd params = forkIO (rawSystem cmd params >> return ()) >> return ()
  -- edit filename | isWindows && takeExtension filename == "txt"  =  todo: direct shell open command

-- |Determines how directory names are substituted into commands
addCmdFiles dirname =  [dirname++"/"]
xCmdFiles   dirname =  [dirname++"/*"]

-- Put all the controls into a nice notebook and obtain the procedure for creating new pages in it
startNotebook dialog = do
  upbox <- dialogGetUpper dialog
  nb <- notebookNew;  boxPackStart upbox nb PackGrow 0
  let newPage name = do hbox <- hBoxNew False 0; notebookAppendPage nb hbox =<< i18n name
                        vbox <- vBoxNew False 0; boxPackStart hbox vbox PackGrow 5
                        return vbox
  return (nb,newPage)

-- |Dialog modes
data DialogMode = EncryptionMode | ProtectionMode | RecompressMode | CommentMode | MakeSFXMode | NoMode  deriving Eq

