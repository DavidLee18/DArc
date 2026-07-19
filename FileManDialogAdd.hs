{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- FreeArc archive manager: Add dialog                                                      ------
----------------------------------------------------------------------------------------------------
module FileManDialogAdd where

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
import System.Time

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

import Utils
import Errors
import Files
import FileInfo
import Charsets            (i18n)
import Compression
import Encryption
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcExtract
import ArcCreate
import FileManPanel
import FileManUtils
import FileManDialogs

----------------------------------------------------------------------------------------------------
---- Dialog for compressing files and modifying/joining archives -----------------------------------
----------------------------------------------------------------------------------------------------

addDialog fm' exec cmd mode = do
  --start_time  <- getClockTime
  fm <- val fm'
  if isFM_Archive fm && cmd=="a"  then fmErrorMsg fm' "0133 You can't compress files directly from archive!" else do
  if isFM_Archive fm && cmd=="j"  then fmErrorMsg fm' "0145 You can't join archives directly from archive!" else do
  files <- if isFM_Archive fm then return [fm_arcname fm]
                              else getSelection fm' addCmdFiles  -- todo: j/ch when Selection includes directories
  title <- i18n$ case (cmd,files) of
                   ("a" , []    ) -> "0136 Add all files to archive"
                   ("a" , [file]) -> "0134 Add %1 to archive"
                   ("a" , _     ) -> "0135 Add %2 files to archive"
                   ("ch", []    ) -> "0146 Modify all archives"
                   ("ch", [file]) -> "0147 Modify %1"
                   ("ch", _     ) -> "0148 Modify %2 archives"
                   ("j" , []    ) -> "0149 Join all archives"
                   ("j" , [file]) -> "0150 Join %1 with another archive"
                   ("j" , _     ) -> "0151 Join %2 archives"
  let wintitle  =  formatn title [head files, show3$ length files]
  -- Create the dialog with the standard OK/Cancel buttons
  fmDialog fm' wintitle $ \(dialog,okButton) -> do
    fmCacheConfigFile fm' $ do
    (nb,newPage) <- startNotebook dialog

------ Main tab ------------------------------------------------------------------------------
    vbox <- newPage "0182 Main";  let pack x = boxPackStart vbox x PackNatural 1
    ------ Archive and directory inside it ------------------------------------------------------------
    (hbox, _, arcname) <- fmOutputArchiveFileBox fm' dialog;  pack hbox  `on`  cmd/="ch"
    (hbox,    arcpath) <- fmLabeledEntryWithHistory fm' "arcpath" "0141 Base directory inside archive:";  pack hbox  `on`  cmd=="a"
    ep                 <- fmExcludePaths;  pack (widget ep)          `on`  cmd=="a"
    updateMode         <- fmUpdateMode;    pack (widget updateMode)  `on`  cmd=="a"
    ------ Compression/Encryption/Protection ----------------------------------------------------------------------
    (hbox, compression, compressionMethod) <- fmCheckedEntryWithHistory fm' "compression" "0183 Compression:";  pack hbox
    (hbox, encryption,  encryptionMethod)  <- fmCheckedEntryWithHistory fm' "encryption"  "0184 Encryption:" ;  pack hbox
    (hbox, protection,  protectionMethod)  <- fmCheckedEntryWithHistory fm' "protection"  "0185 Protection:" ;  pack hbox
    (hbox, comment,     commentFile)       <- fmCheckedEntryWithHistory fm' "comment"     "0186 Comment:"    ;  pack hbox
    (hbox, makeSFX,     sfxFile)           <- fmCheckedEntryWithHistory fm' "sfx"         "0227 Make EXE:"   ;  pack hbox
    -- The rest
    testAfter   <- checkBox "0128 Test archive after operation";        pack (widget testAfter)
    deleteFiles <- checkBox "0122 Delete files successfully archived";  pack (widget deleteFiles)  `on`  cmd=="a"
    lock        <- checkBox "0187 Finalize archive";                    pack (widget lock)
    (hbox, options, optionsStr) <- fmCheckedEntryWithHistory fm' "options" "0072 Additional options:";  pack hbox


------ Archive options tab --------------------------------------------------------------------------
    vbox <- newPage "0200 Archive";  let pack x = boxPackStart vbox x PackNatural 1
    separate <- checkBox "0201 Compress each marked file/directory into separate archive";  pack (widget separate)  `on`  cmd=="a"
    (hbox, ag, agTemplate) <- fmCheckedEntryWithHistory fm' "ag"  "0202 Add to archive name:";  pack hbox  `on`  cmd/="ch"
    archiveTimeMode <- comboBox "0203 Set archive time to:"
                                [ "0204 Current system time"
                                , "0205 Original archive time"
                                , "0206 Latest file time" ];  pack (widget archiveTimeMode)

    create <- checkBox "0207 Delete previous archive contents";  pack (widget create)  `on`  cmd/="ch"
    (hbox, sort, sortOrder)  <- fmCheckedEntryWithHistory fm' "sort" "0208 Order of files in archive:";  pack hbox  `on`  cmd=="a"
    recompressMode <- comboBox "0209 Recompression mode:"
                               [ "0210 Quickly append new files"
                               , "0211 Smart recompression of solid blocks (default)"
                               , "0212 Recompress all files"
                               , "0213 Store only fileinfo"
                               , "0214 Store only fileinfo & crcs"
                               , "0215 No archive headers" ];  pack (widget recompressMode)
    backupMode <- comboBox "0216 Backup mode:"
                               [ "0217 No (default)"
                               , "0218 Full: clear \"Archive\" attribute of files succesfully archived"
                               , "0219 Differential: select only files with \"Archive\" attribute set"
                               , "0220 Incremental: select by \"Archive\" attribute & clear it after compression" ];  pack (widget backupMode)  `on`  cmd/="ch"


------ File selection tab --------------------------------------------------------------------------
    vbox <- newPage "0221 Files";  let pack x = boxPackStart vbox x PackNatural 1
    (hbox, include, includeMasks) <- fmCheckedEntryWithHistory fm' "include" "0222 Include only files:";  pack hbox
    (hbox, exclude, excludeMasks) <- fmCheckedEntryWithHistory fm' "exclude" "0223 Exclude files:";  pack hbox
    (hbox, larger,  largerSize)   <- fmCheckedEntryWithHistory fm' "larger"  "0224 Include only files larger than:";  pack hbox
    (hbox, smaller, smallerSize)  <- fmCheckedEntryWithHistory fm' "smaller" "0225 Include only files smaller than:";  pack hbox
    --times: -tn/to/ta/tb

------ Compression tab ------------------------------------------------------------------------
    (onCompressionChanged, saveCompressionHistories)  <-  compressionPage fm' =<< newPage "0106 Compression"
    onCompressionChanged (compressionMethod =:)

------ Encryption tab -----------------------------------------------------------------------------
    (onEncryptionChanged, encryptionOnOk)  <-  encryptionPage fm' dialog okButton =<< newPage "0119 Encryption"
    onEncryptionChanged (encryptionMethod =:)

------ Archive comment tab -------------------------------------------------------------------------------------
    vbox <- newPage "0199 Comment";  let pack x = boxPackStart vbox x PackGrow 1
    commentText <- scrollableTextView "" [];  pack (widget commentText)


------ Field initialization -------------------------------------------------------------------------
    compression     =: mode==RecompressMode || cmd=="a"
    encryption      =: mode==EncryptionMode
    protection      =: mode==ProtectionMode
    comment         =: mode==CommentMode
    makeSFX         =: mode==MakeSFXMode
    ep              =: 2
    updateMode      =: 0
    archiveTimeMode =: 0
    recompressMode  =: 1
    backupMode      =: 0

    -- The default name of the archive being created depends on the names of the files being archived / archives being joined
    let arcnameBase = case files of
          [file] -> let base = dropTrailingPathSeparator file
                    in if base==file  then dropExtension file  -- one file      - strip the extension
                                      else base                -- one directory - strip the trailing slash
          _      -> takeFileName (fm_curdir fm)                -- many files    - use the current directory name
    arcname =: if isFM_Archive fm then fm_arcname fm
                                  else (arcnameBase ||| "archive") ++ aDEFAULT_ARC_EXTENSION
    arcpath =: ""


------ Reading field values and saving them to the history ----------------------------------------
    widgetShowAll dialog
    --current_time  <- getClockTime;  debugMsg (show$ diffTimes current_time start_time)
    choice <- fmDialogRun fm' dialog "AddDialog"
    windowPresent (fm_window fm)
    when (choice==ResponseOk) $ do
      -- Main settings
      arcname' <- val arcname;  saveHistory arcname   `on`  cmd/="ch"
      arcpath' <- val arcpath;  saveHistory arcpath   `on`  cmd=="a"
      -- If the "archive name" actually points to a directory inside an archive, let's not embarrass ourselves :)
      x <- splitArcPath fm' arcname'
      (arcname', arcpath') <- return$ case x of
          ArcPath arc path -> (arc, path </> arcpath')
          _                -> (arcname', arcpath')
      ep'          <- val ep
      updateMode'  <- val updateMode
      testAfter'   <- val testAfter
      deleteFiles' <- val deleteFiles
      optionsEnabled     <- val options
      ; optionsStr'        <- val optionsStr;         saveHistory optionsStr        `on` optionsEnabled
      compressionEnabled <- val compression
      ; compressionMethod' <- val compressionMethod;  saveHistory compressionMethod `on` compressionEnabled
      encryptionEnabled  <- val encryption
      ; encryptionMethod'  <- val encryptionMethod;   saveHistory encryptionMethod  `on` encryptionEnabled
      protectionEnabled  <- val protection
      ; protectionMethod'  <- val protectionMethod;   saveHistory protectionMethod  `on` protectionEnabled
      commentEnabled     <- val comment
      ; commentFile'       <- val commentFile;        saveHistory commentFile       `on` commentEnabled
      ; commentText'       <- val commentText
      sfxEnabled  <- val makeSFX
      ; sfxFile'  <- val sfxFile;   saveHistory sfxFile  `on` sfxEnabled
      -- Archive settings
      separate'  <- val separate
      agEnabled  <- val ag
      ; agTemplate' <- val agTemplate;      saveHistory agTemplate   `on` agEnabled
      archiveTimeMode' <- val archiveTimeMode
      lock'      <- val lock
      create'    <- val create
      sortEnabled  <- val sort
      ; sortOrder' <- val sortOrder;        saveHistory sortOrder    `on` sortEnabled
      recompressMode' <- val recompressMode
      backupMode'     <- val backupMode
      -- File selection settings
      includeEnabled  <- val include
      ; includeMasks' <- val includeMasks;  saveHistory includeMasks `on` includeEnabled
      excludeEnabled  <- val exclude
      ; excludeMasks' <- val excludeMasks;  saveHistory excludeMasks `on` excludeEnabled
      largerEnabled   <- val larger
      ; largerSize'   <- val largerSize;    saveHistory largerSize   `on` largerEnabled
      smallerEnabled  <- val smaller
      ; smallerSize'  <- val smallerSize;   saveHistory smallerSize  `on` smallerEnabled
      -- Compression/encryption/decryption settings
      saveCompressionHistories
      encryptionOptions <- encryptionOnOk (encryptionEnabled &&& encryptionMethod')
      -- Global settings
      logfile'        <- fmGetHistory1 fm' "logfile" ""
{-
      -- Remember the settings in the history
      fmAddHistory fm' "acmd"$ joinWith "," [ "simpleMethod="  ++simpleMethod'
                                            , "akeyfile="      ++keyfile'
                                            , "xkeyfile="      ++xkeyfile'
                                            , "encryptHeaders="++show encryptHeaders'
                                            , "testAfter="     ++show testAfter']
-}
      -- Reflect the archive name change
      when sfxEnabled $ do
        when (isFM_Archive fm) $ do
        let newname' = changeSfxExt True (clear sfxFile') arcname'
        when (newname'/=arcname') $ do
          fmChangeArcname fm' newname'

------ Building the command(s) to execute ---------------------------------------------------------
      let msgs = case cmd of
                  "ch"-> ["0237 Modifying %1",
                          "0238 SUCCESFULLY MODIFIED %1",
                          "0239 %2 WARNINGS WHILE MODIFYING %1"]
                  "j" -> ["0240 Joining archives to %1",
                          "0241 SUCCESFULLY JOINED ARCHIVES TO %1",
                          "0242 %2 WARNINGS WHILE JOINING ARCHIVES TO %1"]
                  _   -> ["0243 Adding to %1",
                          "0244 FILES SUCCESFULLY ADDED TO %1",
                          "0245 %2 WARNINGS WHILE ADDING TO %1"]
      let command archive filelist =
           (msgs, [takeFileName archive],
            [if create' then "create" else cmd]++
            -- Main page settings
            (compressionEnabled &&&  cvt "-m"   compressionMethod')++
            (encryptionEnabled  &&&  cvt "-ae=" encryptionMethod')++encryptionOptions++
            (protectionEnabled  &&&  cvt "-rr"  protectionMethod')++
            (commentEnabled     &&&  [((clear commentFile' !~ "-z*" &&& "--archive-comment=")++) (clear commentFile' ||| commentText')])++
            (sfxEnabled         &&&  cvt "-sfx" sfxFile')++
            (testAfter'         &&&  ["-t"])++
            (deleteFiles'       &&&  ["-d"])++
            (null files         &&&  ["-r"])++
            (arcpath'           &&&  ["-ap"++clear arcpath'])++
            (ep'            `select`  "-ep,-ep1,,-ep2,-ep3")++
            (updateMode'    `select`  ",-u,-f,--sync")++
            -- Archive settings
            (lock'                &&&   ["-k"])++
            (agEnabled            &&&   ["-ag"++clear agTemplate'])++
            (sortEnabled          &&&   ["-ds"++clear sortOrder'])++
            (archiveTimeMode' `select`  ",-tk,-tl")++
            (backupMode'      `select`  ",-ac,-ao,-ac -ao")++
            (recompressMode'  `select`  "--append,,--recompress,--nodata,--crconly,--nodir")++
            (cmd/="a" &&& (compressionEnabled || encryptionEnabled)  &&&  ["--recompress"])++
            -- File selection settings
            (includeEnabled   &&&  cvt1 "-n" includeMasks')++
            (excludeEnabled   &&&  cvt1 "-x" excludeMasks')++
            (largerEnabled    &&&  ["-sm"++clear largerSize'])++
            (smallerEnabled   &&&  ["-sl"++clear smallerSize'])++
            -- Other
            ["-dp"++fm_curdir fm]++
            (logfile'         &&&  ["--logfile="++clear logfile'])++
            (cmd=="ch"        &&&  ["--noarcext"])++
            (optionsEnabled   &&&  words (clear optionsStr'))++
            ["--", clear archive]++filelist)
      --
      exec$ if cmd=="ch" then map (\archive -> command (fm_curdir fm </> archive) []) (files ||| ["*"])
       else if separate' then files.$map (\file -> command (fm_curdir fm </> dropTrailingPathSeparator file++aDEFAULT_ARC_EXTENSION) [file])
                         else [command arcname' files]


----------------------------------------------------------------------------------------------------
---- Helper definitions ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Field for choosing the output archive name
fmOutputArchiveFileBox fm' dialog =
  fmFileBox fm' dialog
            "arcname" FileChooserActionSave
     (label "0131 Output archive:")
            "0132 Select output archive"
            aARCFILE_FILTER
            (const$ return True)
            (fmCanonicalizeDiskPath fm')

-- |Field for choosing the -ep option
fmExcludePaths =
  comboBox "0188 Store file paths:"
           [ "0189 No"
           , "0190 Relative to compressed dir"
           , "0191 Relative to curdir (default)"
           , "0192 Absolute (relative to root dir)"
           , "0193 Full (including drive letter)" ]

-- |Field for choosing the update mode.
fmUpdateMode =
  comboBox "0194 Update mode:"
           [ "0195 Add and replace files (default)"
           , "0196 Add and update files"
           , "0197 Fresh existing files"
           , "0198 Synchronize archive with disk contents" ]

