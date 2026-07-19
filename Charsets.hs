{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Support for various charsets and localization of the program interface                     ----
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  Charsets
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Charsets where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Array
import Data.Char
import Data.IORef
import Data.List
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.Posix.Internals
import System.Posix.Types
import System.IO
import System.IO.Error hiding (catch)
import System.IO.Unsafe
-- import System.Locale
-- import System.Time
import System.Process
import System.Directory
import System.Environment
import Utils
import Files


---------------------------------------------------------------------------------------------------
---- Global recoding settings for use in deeply nested functions                       ------------
---------------------------------------------------------------------------------------------------

-- DWORD: 32-bit unsigned int; equivalent to System.Win32.Types.DWORD on Windows
type DWORD = CUInt

-- |Translate string from internal to terminal encoding
{-# NOINLINE str2terminal' #-}
str2terminal'     = unsafePerformIO$ newIORef$ unParseData (domainTranslation aCharsetDefaults 't')
str2terminal s    = val str2terminal' >>== ($ s)
-- |Translate string from terminal to internal encoding
{-# NOINLINE terminal2str' #-}
terminal2str'     = unsafePerformIO$ newIORef$ parseData (domainTranslation aCharsetDefaults 't')
terminal2str s    = val terminal2str' >>== ($ s)
-- |Translate string from cmdline to internal encoding
{-# NOINLINE cmdline2str' #-}
cmdline2str'      = unsafePerformIO$ newIORef$ parseData (domainTranslation aCharsetDefaults 'p')
cmdline2str s     = val cmdline2str' >>== ($ s)
-- |Translate string from internal to logfile encoding
{-# NOINLINE str2logfile' #-}
str2logfile'      = unsafePerformIO$ newIORef$ unParseData (domainTranslation aCharsetDefaults 'i')
str2logfile s     = val str2logfile' >>== ($ s)

-- |Operation that sets the global recoding settings for use in deeply nested functions
setGlobalCharsets charsets = do
  str2filesystem' =: unParseData (domainTranslation charsets 'f')
  str2terminal'   =: unParseData (domainTranslation charsets 't')
  str2logfile'    =: unParseData (domainTranslation charsets 'i')
  terminal2str'   =: parseData (domainTranslation charsets 't')
  filesystem2str' =: parseData (domainTranslation charsets 'f')
  cmdline2str'    =: parseData (domainTranslation charsets 'p')


-- Obtaining the command line















myGetArgs = getArgs >>= mapM cmdline2str



---------------------------------------------------------------------------------------------------
---- Parser for the -sc/--charset option         --------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Type of a function translating input data of a given type into Unicode
type ParseDataFunc  =  Domain -> String -> String

-- |Process the list of --charset/-sc options, returning the charset table
-- and the file read/write procedures that take it into account
parseCharsetOption optionsList = (charsets
                                   ,parseFile   . domainTranslation charsets
                                   ,unParseFile . domainTranslation charsets
                                   ,parseData   . domainTranslation charsets
                                   ,unParseData . domainTranslation charsets)
  where
    -- Charset table
    charsets = foldl f aCharsetDefaults optionsList
    -- Handler function for the --charset options
    f value "--"      =  aCharsetDefaults      -- -sc-- means restore the default values
    f value ('s':cs)  =  _7zToRAR value "l" cs  -- -scs... sets the charset for listfiles
    f value ('l':cs)  =  _7zToRAR value "l" cs  -- -scl... does the same
    f value ('c':cs)  =  _7zToRAR value "c" cs  -- -scs... sets the charset for comment files
    f value ('f':cs)  =  _7zToRAR value "f" cs  -- -scf... sets the charset for the filesystem
    f value ('d':cs)  =  _7zToRAR value "d" cs  -- -scd... sets the charset for the archive directory
    f value ('t':cs)  =  _7zToRAR value "t" cs  -- -sct... sets the charset for the terminal (console)
    f value ('p':cs)  =  _7zToRAR value "p" cs  -- -scp... sets the charset for command line parameters
    f value ('i':cs)  =  _7zToRAR value "i" cs  -- -sci... sets the charset for ini files (arc.ini/arc.groups)
    f value (x:cs)    =  foldl Utils.update value [(c,x) | c<-cs|||"cl"]  -- set to `x` those list elements that are enumerated in cs (by default 'c' and 'l')
    -- Helper functions converting the 7zip option format into the RAR one
    _7zToRAR value typ cs  =  f value (g (strLower cs):typ)
    g "utf-8"  = '8';  g "win"  = 'a'
    g "utf8"   = '8';  g "ansi" = 'a'
    g "utf-16" = 'u';  g "dos"  = 'o'
    g "utf16"  = 'u';  g "oem"  = 'o'


-- File reading procedure that translates its charset and splits it into separate lines
parseFile encoding file  =  fileGetBinary file >>== parseData encoding >>== linesCRLF

-- Procedure translating input data from encoding into Unicode
parseData encoding  =  aTRANSLATE_INPUT (charsetTranslation encoding)

-- File writing procedure that translates the data into the encoding charset
unParseFile encoding file  =  filePutBinary file . unParseData encoding

-- Procedure translating output data from Unicode into encoding
unParseData encoding  =  aTRANSLATE_OUTPUT (charsetTranslation encoding)

-- |Split into lines a file that uses any end-of-line representation (CR, LF, CR+LF)
linesCRLF = recursive oneline  -- oneline "abc\n..." = ("abc","...")
              where oneline ('\r':'\n':s)  =  ("",'\xFEFF':s)
                    oneline ('\r':s)       =  ("",'\xFEFF':s)
                    oneline ('\n':s)       =  ("",'\xFEFF':s)
                    oneline ('\xFEFF':s)   =  oneline s
                    oneline (c:s)          =  (c:s0,s1)  where (s0,s1) = oneline s
                    oneline ""             =  ("","")


-- We assume that all GUI config files are stored in UTF-8
readConfigFile          = parseFile   '8'
saveConfigFile   file   = unParseFile '8' file . joinWith "\n"
modifyConfigFile file f = handle (\(e::SomeException)->return []) (readConfigFile file) >>== f >>= saveConfigFile file


---------------------------------------------------------------------------------------------------
---- Support for various charsets on input/output   -----------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Returns the charset used in domainCharsets for data of type domain
domainTranslation domainCharsets domain =
  lookup domain domainCharsets `defaultVal` error ("Unknown charset domain "++quote [domain])

-- |Translation of data given in the charset encoding
charsetTranslation charset =
  lookup charset aCHARSETS `defaultVal` error ("Unknown charset "++quote [charset])

-- |Translation of data from the domain area (listfiles, config files, comment files...),
-- using the charset specified for it in domainCharsets
translation domainCharsets domain =
  charsetTranslation $ domainTranslation domainCharsets domain

-- Types used to represent domain and charset
type Domain  = Char
type Charset = Char

-- |Each charset is represented by pair of functions: input translation (byte sequence into Unicode String) and output translation
data TRANSLATION = TRANSLATION {aTRANSLATE_INPUT, aTRANSLATE_OUTPUT :: String->String}

-- |Character sets and functions to translate texts from/to these charsets
aCHARSETS = [ ('0', TRANSLATION id               id)
            , ('8', TRANSLATION utf8_to_unicode  unicode2utf8)
            , ('u', TRANSLATION utf16_to_unicode unicode2utf16)
            ] ++ aLocalCharsets


















-- |Windows-specific charsets
aLocalCharsets = [ ('o', TRANSLATION oem2unicode  unicode2oem)
                  , ('a', TRANSLATION ansi2unicode unicode2ansi)
                  ]

-- |Default charsets for various domains
#if defined(FREEARC_WIN) && !defined(__MHS__)
aCharsetDefaults = [ ('f','u')  -- filenames in filesystem: UTF-16 (Windows uses wide-char API)
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','o')  -- filelists: OEM
                    , ('c','o')  -- comment files: OEM
                    , ('t','o')  -- terminal: OEM
                    , ('p','a')  -- program arguments: ANSI
                    , ('i','o')  -- ini/group files: OEM
                    ]
#else
aCharsetDefaults = [ ('f','8')  -- filenames in filesystem: UTF-8 (Linux/Unix uses UTF-8 paths)
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','8')  -- filelists: UTF-8
                    , ('c','8')  -- comment files: UTF-8
                    , ('t','8')  -- terminal: UTF-8
                    , ('p','8')  -- program arguments: UTF-8
                    , ('i','8')  -- ini/group files: UTF-8
                    ]
#endif

---------------------------------------------------------------------------------------------------
---- Windows-specific codecs ----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Convert the Windows character codes for \r and \n into a human-readable form
iHateWindows = replace (chr 9834) '\r' . replace (chr 9689) '\n'

#if defined(FREEARC_WIN) && !defined(__MHS__)
-- |Translate string from Unicode to OEM encoding
unicode2oem s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from OEM encoding to Unicode
oem2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

-- |Translate string from Unicode to ANSI encoding
unicode2ansi s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               c_OemToAnsiBuff cstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from ANSI encoding to Unicode
ansi2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_AnsiToOemBuff cstr cstr (i len)
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

foreign import stdcall unsafe "winuser.h CharToOemBuffW"
  c_WideToOemBuff :: CWString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffW"
  c_OemToWideBuff :: CString -> CWString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffA"
  c_OemToAnsiBuff :: CString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h CharToOemBuffA"
  c_AnsiToOemBuff :: CString -> CString -> DWORD -> IO Bool
#else
-- Non-Windows stubs: on Unix, strings are already in Unicode (UTF-8)
unicode2oem  = id
oem2unicode  = id
unicode2ansi = id
ansi2unicode = id
#endif




---------------------------------------------------------------------------------------------------
---- UTF-8, UTF-16 codecs -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Translate string from UTF-16 encoding to Unicode
utf16_to_unicode = tryToSkip [chr 0xFEFF] . map chr . fromUTF16 . map ord
 where
  fromUTF16 (c1:c2:c3:c4:wcs)
    | 0xd8<=c2 && c2<=0xdb  &&  0xdc<=c4 && c4<=0xdf =
      ((c1+c2*256 - 0xd800)*0x400 + (c3+c4*256 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c1:c2:wcs) = c1+c2*256 : fromUTF16 wcs
  fromUTF16 _ = []  -- discard any trailing incomplete code unit

-- |Translate string from Unicode to UTF-16 encoding
unicode2utf16 = map chr . foldr (utf16Char . ord) []
 where
  utf16Char c wcs
    | c < 0x10000 = c `mod` 256 : c `div` 256 : wcs
    | otherwise   = let c' = c - 0x10000 in
                    ((c' `div` 0x400) .&. 0xFF) :
                    (c' `div` 0x40000 + 0xd8) :
                    (c' .&. 0xFF) :
                    (((c' `mod` 0x400) `div` 256) + 0xdc) : wcs

-- |Translate string from UTF-8 encoding to Unicode
utf8_to_unicode s =
  if all isAscii s
    then s
    else (tryToSkip [chr 0xFEFF] . fromUTF' . map ord) s  where
            fromUTF' [] = []
            fromUTF' all@(x:xs)
                | x<=0x7F = chr x : fromUTF' xs
                | x<=0xBF = err
                | x<=0xDF = twoBytes all
                | x<=0xEF = threeBytes all
                | x<=0xFF = fourBytes all
                | otherwise = err
            twoBytes (x1:x2:xs) = chr  (((x1 .&. 0x1F) `shift` 6) .|.
                                          (x2 .&. 0x3F)):fromUTF' xs
            twoBytes _ = error "fromUTF: illegal two byte sequence"

            threeBytes (x1:x2:x3:xs) = chr (((x1 .&. 0x0F) `shift` 12) .|.
                                             ((x2 .&. 0x3F) `shift` 6) .|.
                                              (x3 .&. 0x3F)):fromUTF' xs
            threeBytes _ = error "fromUTF: illegal three byte sequence"

            fourBytes (x1:x2:x3:x4:xs) = chr (((x1 .&. 0x0F) `shift` 18) .|.
                                               ((x2 .&. 0x3F) `shift` 12) .|.
                                               ((x3 .&. 0x3F) `shift` 6) .|.
                                                (x4 .&. 0x3F)):fromUTF' xs
            fourBytes _ = error "fromUTF: illegal four byte sequence"

            err = error "fromUTF: illegal UTF-8 character"

-- |Translate string from Unicode to UTF-8 encoding
unicode2utf8 s =
  if all isAscii s
    then s
    else go s
      where go [] = []
            go (x:xs) | ord x<=0x007f = chr (ord x) : go xs
                      | ord x<=0x07ff = chr (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | ord x<=0xffff = chr (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | otherwise     = chr (0xF0 .|. ( ord x `shiftR` 18)) :
                                        chr (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)) :
                                        chr (0x80 .|. ( ord x .&. 0x3F)) :
                                        go xs


---------------------------------------------------------------------------------------------------
---- Internalization ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

{-# NOINLINE locale #-}
-- |Localization: mapping from an index to a localized string
locale :: IORef (Array Int (Maybe String))
locale = unsafePerformIO $ ref$ array (0,-1) []

{-# NOINLINE setLocale #-}
-- |Set the localization from a file
setLocale "--"       = return ()
setLocale localeFile = do
  localeInfo <- parseLocaleFile localeFile
  locale =: localeInfo

-- |Translates a string / list of strings into the local language
i18ns = mapM i18n
i18n  = i18n' .>>== fst
i18n' = i18n_general (val locale)

{-# NOINLINE i18fmt #-}
-- |Format a list of strings, using the first one as the template requiring localization
-- and the rest as its arguments
i18fmt (x:xs)  =  i18n x >>== (`formatn` xs)


{-# NOINLINE parseLocaleFile #-}
-- |Read the list of localization strings from a file
parseLocaleFile localeFile = do
  -- Read the localization file or return an empty stub
  localeInfo <- readConfigFile localeFile `catch` (\(e::SomeException) -> return ["0000=English"])
  -- Select the lines starting with "dddd" and build an array from them: dddd -> the text after the '=' sign
  -- If the text after '=' is enclosed in double quotes - strip them
  -- '&' characters are replaced with '_' (7-zip and Gtk differ in their accelerators)
  -- \" is replaced by a plain ", and the sequence "\\n" by the \n character itself
  return$ localeInfo .$ filter   (\s -> length s > 4  &&  s `contains` '=')
                     .$ filter   (all isDigit . take 4)
                     .$ map      (split2 '=')
                     .$ deleteIf (("??"==) . snd)
                     .$ mapFsts  (readInt . take 4)
                     .$ mapSnds  (\s -> s.$ (s.$match "\"*\"" &&& (reverse . drop 1 . reverse . drop 1)))
                     .$ mapSnds  (replace '&' '_' . replaceAll "\\\"" "\"" . replaceAll "\\n" "\n")
                     .$ populateArray Nothing Just

{-# NOINLINE i18n_general #-}
-- |Returns the localized label text and its tooltip
i18n_general getLocale text = do
  -- If the text starts with "dddd ", return the localization string numbered dddd instead of it
  -- If no such string is found - return the given text without the "dddd " prefix
  -- In addition, texts of the form "  *  " are localized into a similar form
  case splitAt 4 text of
    (d,' ':engText) | all isDigit d -> do
         let f = (engText.$match "  *  ")  &&&  (("  "++).(++"  "))
         arr <- getLocale
         let n = readInt d
             g i def = if i.$inRange (bounds arr)
                         then fmap f (arr!i) `defaultVal` def
                         else def
         return (g n engText, g (n+1000) "")
    _ -> return (text, "")











