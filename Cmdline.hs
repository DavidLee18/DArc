{-# LANGUAGE CPP #-}
---------------------------------------------------------------------------------------------------
---- Turning the command line into a set of commands/options to execute.                       ----
---------------------------------------------------------------------------------------------------
module Cmdline where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression
import Options

-- |Parses the command line and returns the list of commands it specifies as Command structures.
-- Each command contains the command name, the archive spec, the list of file specs and the options.
-- Commands are separated by " ; ", for example "a archive -r ; t archive ; x archive"
parseCmdline cmdline  =  (`mapMaybeM` split ";" cmdline) $ \args -> do
  -- Set display_option to its default value, since there may be no other chance to do so.
  display_option' =: aDISPLAY_DEFAULT
  let options = takeWhile (/="--") $ filter (match "-*") args
  -- If the command line contains nothing but options - print help/configuration and exit
  if args==options then do
      putStr $ if options `contains` "--print-config"
                 then unlines ("":";You can insert these lines into ARC.INI":compressionMethods:builtinMethodSubsts)
                 else aHELP
      return Nothing
    else do

  -- Read the options from the FREEARC environment variable or the one given by the -env option
  (o0, _) <- parseOptions options [] []
  let no_configs = findReqList o0 "config" `contains` "-"
  env_options <- case findReqArg o0 "env" "--" of
                    "--" | no_configs -> return ""  -- The -cfg- option on the command line disables the use of BOTH arc.ini AND %FREEARC
                         | otherwise  -> getEnv aCONFIG_ENV_VAR  `catch`  (\(e::SomeException) -> return "")
                    "-"               -> return ""
                    env               -> getEnv env

  -- Read the config file arc.ini, or the one given by the -cfg option
  (o1, _)  <- parseOptions (words env_options++options) [] []   -- the -cfg option may be given on the command line or in the environment variable
  cfgfile <- case findReqArg o1 "config" "--" of
               "--" -> findFile configFilePlaces aCONFIG_FILE
               "-"  -> return ""
               cfg  -> return cfg
  -- Process the --charset/-sc option to determine the encoding used to read the config file
  let (_, parseFile1, _, _, _)  =  parseCharsetOption (findReqList o1 "charset")
  -- Read the options from the config file, if there is one, dropping empty lines and comments
  config  <-  cfgfile  &&&  parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]

  -- These definitions turn the contents of the config file into a set of sections,
  -- whose contents can be queried with the configSection function.
  -- For example, configSection "[Compression methods]" is the list of lines in the "[Compression methods]" section
  let configSections = map makeSection $ makeGroups selectSectionHeadings config
      makeSection (x:xs) = (cleanupSectionName x, xs)
      configSection name = lookup (cleanupSectionName name) configSections `defaultVal` []
      -- Decode the compression method/extra algorithms using the settings from the "[Compression methods]" section
      decode_compression_method = decode_method (configSection compressionMethods)
      decode_methods s = ("0/"++s).$decode_compression_method.$lastElems (length (elemIndices '/' s) + 1)

  -- And these definitions let you fetch an element with a given name from a section,
  -- including the cases where the left-hand side of the definition contains several words,
  -- which are given one and the same definition,
  -- and where the definition is repeated (in that case all the lines must be merged).
  -- Example:
  --   a create j = -m4x -ms
  --   a = --display
  -- In this case (configElement section "a") returns "-m4x -ms --display"
  let sectionElement name = unwords . map snd
                              . concatMap (filter (strLowerEq name . fst) . (\line -> let (a,b)  =  split2 '=' line
                                                                                      in  map (,trim b) (words$ trim a)))
      configElement section element  =  configSection section .$ sectionElement element

  -- If the first significant line of the config file is not a section header, then
  -- it describes options common to all commands
  let config_1st_line  =  case head1 config of
                              '[' : _  -> ""    -- this is a section heading
                              str      -> str

  -- The command name: "a", "create" and so on. The default options for this command, given in the config file
  let cmd = head1$ filter (not . match "-*") args
      default_cmd_options = configElement defaultOptions cmd

  -- Prepend the default options for all commands to the command line,
  -- the default options for this command and the contents of the environment variable
  let additional_args  =  concatMap words [config_1st_line, default_cmd_options, env_options]

  -- Parse the command line, obtaining a set of options and a list of "free arguments"
  (o, freeArgs)  <-  parseOptions (additional_args++args) [] []
  -- Report an error if there are fewer than two "free arguments" - the command or archive name is missing
  case freeArgs of
    []     ->  registerError$ CMDLINE_NO_COMMAND args
    [cmd]  ->  registerError$ CMDLINE_NO_ARCSPEC args
    _ -> return ()
  let (cmd:pure_arcspec:pure_filespecs) = freeArgs

                               -- Arguments:  the option name and the default value
  let grouping              =  findReqArg   o "solid" aDEFAULT_DATA_GROUPING .$ parseSolidOption
      group_dir             =  fst3 grouping
      group_data            =  snd3 grouping
      defaultDirCompressor  =  thd3 grouping ||| aDEFAULT_DIR_COMPRESSION
      orig_dir_compressor   =  findReqArg   o "dirmethod"  defaultDirCompressor .$ decode_compression_method
      compression_options   =  findReqList  o "method"
      orig_sort_order       =  findMaybeArg o "sort"
      yes                   =  findNoArg    o "yes"
      autogenerate_arcname  =  findOptArg   o "autogenerate"  "--" ||| "%Y%m%d%H%M%S"
      indicator             =  findOptArg   o "indicator"     "1"  ||| "0"   -- -i1 by default; -i is equivalent to -i0
      recovery              =  findOptArg   o "recovery"      (if take 2 cmd=="rr"  then drop 2 cmd  else "--")   -- the "rr..." command is equivalent to "ch -rr..."
                                                              .$  changeTo [("0.1%","0*4kb"), ("0.01%","0*64kb")]
      orig_workdir          =  findOptArg   o "workdir" "--"   ||| "%TEMP"
      pretest               =  findOptArg   o "pretest"       "1" .$  changeTo [("-","0"), ("+","2"), ("","2")]
      broken_archive        =  findReqArg   o "BrokenArchive" "-"  ||| "0"
      language              =  findReqArg   o "language"      "--"
      noarcext              =  findNoArg    o "noarcext"
      crconly               =  findNoArg    o "crconly"
      nodata                =  findNoArg    o "nodata"
      url_proxy             =  findOptArg   o "proxy"         "--"
      url_bypass            =  findOptArg   o "bypass"        ""
      exclude_path          =  findOptArg   o "ExcludePath"   "--"

      add_exclude_path  =  exclude_path .$ changeTo [("--", "9"), ("", "0")] .$ readInt
      dir_exclude_path
        | cmd=="e" = 0
        | cmdType cmd==EXTRACT_CMD = add_exclude_path
        | otherwise = 3

  -- List of actions to perform immediately before the command starts executing
  setup_command <- newList
  setup_command <<= (url_setup_proxy      .$ withCString (replace ',' ' ' url_proxy))
  setup_command <<= (url_setup_bypass_list.$ withCString (replace ',' ' ' url_bypass))

  -- Load the localization file
  setup_command <<= setLocale language
  setLocale language

  -- Manually split apart the -o/-op options
  let (op, o_rest) = partition is_op_option (findReqList o "overwrite")
      op_opt       = map  (tryToSkip "p") op
      overwrite    = last ("p":o_rest)
      is_op_option ('p':_:_) = True
      is_op_option _         = False

  -- Check that the options take one of the allowed values
  testOption "overwrite"     "o"  overwrite      (words "+ - p")
  testOption "indicator"     "i"  indicator      (words "0 1 2")
  testOption "pretest"       "tp" pretest        (words "0 1 2 3")
  testOption "BrokenArchive" "ba" broken_archive (words "- 0 1")
  testOption "ExcludePath"   "ep" exclude_path   ("" : words "1 2 3 --")

  -- Determine the name of the SFX module that will be prepended to the archive
  let sfxname  =  findOptArg o "sfx" (if take 1 cmd=="s"  then drop 1 cmd  else "--")   -- the "s..." command is equivalent to "ch -sfx..."
                    ||| aDEFAULT_SFX  -- with an empty parameter, use the default SFX module (arc.sfx from the standard directory)
  sfx <- if sfxname `notElem` words "- --" && takeFileName sfxname == sfxname
           then findFile libraryFilePlaces sfxname   -- use the module with the given name from the standard directory
           else return sfxname
  -- This is the check the user actually reaches: -sfx is resolved here, at
  -- parse time, and writeSFX's own error is downstream of it -- it only fires
  -- if a module is found and then turns out to be unreadable.
  when (sfx=="") $
    registerError$ GENERAL_ERROR ["0342 SFX module %1 is not found", sfxname]

  -- Append a date/time stamp to the archive base name if the -ag option is given
  current_time <- getClockTime
  let add_ag  =  case autogenerate_arcname of
                   "--" -> id
                   _    -> updateBaseName (++ showtime autogenerate_arcname current_time)

  -- Append the default extension to the archive name if it has no other extension and --noarcext is not used
  let arcspec  =  addArcExtension noarcext$ add_ag pure_arcspec

  -- Process the list of --charset/-sc options, returning the encoding table
  -- and the file read/write procedures that take it into account
  let (charsets, parseFile, unParseFile, parseData, unParseData)  =  parseCharsetOption (findReqList o "charset")
  setGlobalCharsets charsets
  setup_command <<= setGlobalCharsets charsets

  -- Manually process the list of --display options
  let orig_display = foldl f aDISPLAY_DEFAULT (findReqList o "display")
      -- Handler function for the --display options
      f value ""       =  aDISPLAY_ALL     -- -di without parameters means enable output of all information
      f value "--"     =  aDISPLAY_DEFAULT -- -di-- means restore the default value
      f value ('+':x)  =  nub (value++x)   -- -di+x means add x to the flags
      f value ('-':x)  =  nub value \\ x   -- -di-x means remove x from the flags
      f value x        =  nub x            -- otherwise just copy the parameter into the option value

  -- For the "lb" command, completely disable printing extra information on screen,
  -- for the other listing commands, force the archive name to be printed
  let display = case () of
                  _ | cmd=="lb"              ->  ""
                    | cmdType cmd==LIST_CMD  ->  orig_display++"a"
                    | otherwise              ->  orig_display
  -- Set display_option, since we may need it when printing a warning about the contents of an external compressor section
  display_option' =: display
  -- Restore display_option before the command starts running, since it may have been changed while parsing/running other commands
  setup_command <<= (display_option' =: display)

  -- Register the external compressor descriptions from the [External compressor:...] sections
  let externalSections = filter (matchExternalCompressor.head) $ makeGroups selectSectionHeadings config
      matchExternalCompressor s = (head externalCompressor          ==    head s) && (init (tail externalCompressor) `match` init (tail s)) && (last externalCompressor          ==    last s)
  let registerExternalCompressors makeWarnings = do
        CompressionLib.clearExternalCompressorsTable
        for externalSections $ \section -> do
          result <- CompressionLib.addExternalCompressor (unlines section)
          when (result/=1 && makeWarnings) $ do
            registerWarning (BAD_CFG_SECTION cfgfile section)
  -- Register them now for command-line parsing and re-register them at execution time for the actual compression.
  registerExternalCompressors True
  setup_command <<= registerExternalCompressors False

---------------------------------------------------------------------------------------------------
-- COMPRESSION ALGORITHM SELECTION ----------------------------------------------------------------
  -- Memory-size parser that understands notations like "75%" (of the RAM size)
  -- The memory size is rounded to a multiple of 4 mb, to avoid odd values caused by various Shadow BIOS options
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))

  -- Parser for the -md option
  let parseDict dictionary  =  case dictionary of
          [c]       | isAlpha c     ->  Just$ 2^(16 + ord c - ord 'a')   -- the option is given as a single letter, -mda..-mdz
          s@(c:_)   | isDigit c     ->  Just$ parsePhysMem s             -- the option starts with a digit: -md8, -md8m, -md10%
          _                 ->  Nothing                          -- otherwise this is not the -md option but an -m option starting with -md...

  -- Loop that manually processes the various options starting with "-m"
  method <- ref "";    methods <- ref "";  mc' <- newList;  dict <- ref 0;
  mm'    <- ref "--";  threads <- ref 0 ;  ma' <- ref "--"
  forM_ compression_options $ \option ->
    case option of
      -- The -mc option lets you quickly disable individual compression algorithms (-mcd-, -mc-rep)
      'c':rest  | anyf [beginWith "-", endWith "-"] rest
                    ->  mc' <<= rest.$tryToSkip "-".$tryToSkipAtEnd "-"
                                    .$changeTo [("d","delta"), ("e","exe"),  ("l","lzp")
                                               ,("r","rep"),   ("z","dict")
                                               ,("a","$wav"),  ("c","$bmp"), ("t","$text")
                                               ]
      -- The -md option sets the dictionary size, just like in good old RAR :)
      'd':rest  | Just md <- parseDict rest ->  dict =: md
      -- The -mm option selects the multimedia compression mode.
      'm':rest  | mmflag <- rest.$tryToSkip "=",
                  mmflag `elem` ["","--","+","-","max","fast"]  ->  mm' =: mmflag
      -- The -ms option selects a fast compression method for already-compressed files
      "s"  ->  methods ++= "/$compressed="++join_compressor aCOMPRESSED_METHOD
      "s-" ->  mc' <<= "$compressed"
      -- The -ma option selects the file type autodetection mode
      'a':rest  | maflag <- rest.$tryToSkip "=".$changeTo [("+","--"), ("","--"), ("-","0")],
                  maflag `elem` ("--" : map show [0..9])  ->  ma' =: maflag
      -- The -mt option enables/disables multithreading and sets the number of threads
      't':rest  | n <- rest.$tryToSkip "=".$changeTo [("-","1"), ("+","0"), ("","0"), ("--","0")],
                  all isDigit n  ->  threads =: readInt n
      -- The -m$type=method options set the compression algorithms for individual file types
      '$':_ -> case break (`elem` "=:.") option of
                 (_type, '=':method) -> methods ++= '/':option                      -- -m$type=method: archive files of this type with the given compressor
                 -- (_type, ':':names)  -> types  ++= split ':' names               -- -m$type:name1:name2: add the given masks to the file list of this type
                 -- (_type, ',':exts)   -> types  ++= map ("*."++) $ split '.' exts -- -m$type.ext1.ext2: add extensions to the type's list
                 _ -> registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
      -- All other options starting with -m0= or just -m set the main compression method.
      m  ->  method =: m.$tryToSkip "0="
  -- Read the final values of the variables
  dictionary  <- val dict       -- dictionary size (-md)
  cthreads    <- val threads    -- number of compression threads (-mt)
  mainMethod  <- val method     -- main compression method.
  userMethods <- val methods    -- extra methods for specific file types (-m$/-ms)
  mm          <- val mm'        -- multimedia compression
  mc          <- listVal mc'    -- list of compression algorithms that must be disabled
  ma          <- val ma'        -- file type autodetection mode

  -- Compression level, 0..9
  let clevel = case mainMethod of
                 [d]     | isDigit d -> digitToInt d
                 [d,'p'] | isDigit d -> digitToInt d
                 [d,'x'] | isDigit d -> digitToInt d
                 ['x',d] | isDigit d -> digitToInt d
                 "mx"                -> 9
                 "max"               -> 9
                 _                   -> 4  -- default compression level
  -- Autodetect level, 0..9
  let ma_opt = case ma of "--" -> clevel
                          _    -> readInt ma

  -- Before the command starts, tell the compression library how many threads it should use
  setup_command <<= CompressionLib.setCompressionThreads (cthreads ||| i getProcessorsCount)   -- By default, use number of threads equal to amount of available processors/cores

  -- Memory limits for compression/decompression
  let climit = parseLimit "75%"$ findReqArg o "LimitCompMem"   "--"
      dlimit = parseLimit d_def$ findReqArg o "LimitDecompMem" "--"
      d_def  = if cmdType cmd == ADD_CMD  then "1gb"  else "75%"
      parseLimit deflt x = case x of
        "--" -> parsePhysMem deflt  -- By default: limit memory usage to 75% of physical RAM when compressing, and to 1gb when decompressing
        "-"  -> CompressionLib.aUNLIMITED_MEMORY   -- Do not limit memory usage
        s    -> parsePhysMem s      -- Limit memory usage to the given amount

  -- Control of multimedia compression
  let multimedia mm = case mm of
        "-"    -> filter ((`notElem` words "$wav $bmp") . fst)    -- remove the $wav and $bmp groups from the list of compression methods.
        "fast" -> (++decode_methods "$wav=wavfast/$bmp=bmpfast") . multimedia "-"
        "max"  -> (++decode_methods "$wav=wav/$bmp=bmp")         . multimedia "-"
        "+"    -> \m -> case () of
                          _ | m.$isFastDecompression  -> m.$multimedia "fast"
                            | otherwise               -> m.$multimedia "max"
        ""     -> multimedia "+"
        "--"   -> id

  -- Removal of the given compression algorithm.
  let method_change mc x = case mc of
        '$':_  -> -- remove the group mc (for example "$bmp") from the list of compression methods.
                  x.$ filter ((/=mc) . fst)
        _      -> -- remove the groups whose last compression algorithm is mc (for example -mc-tta removes the groups whose compression chains end with the tta algorithm)
                  x.$ (\(x:xs) -> x:(xs.$ filter ((/=mc) . method_name.last1.snd)))   -- We leave the main compression group (the head of the list) alone
                  -- remove the mc algorithm from the remaining compression chains.
                   .$ map (mapSnd$ filter ((/=mc) . method_name))

  -- If the "--nodata" option is given, simulate data compression.
  -- If the "--crconly" option is given, only compute the CRC of the archived files.
  -- Otherwise process the selected main and additional compression algorithms,
  -- configuring multimedia compression and the dictionary size, removing the disabled algorithms,
  -- and limiting memory consumption
  let dataCompressor
        | nodata = [("", [aFAKE_COMPRESSION])]
        | crconly = [("", [aCRC_ONLY_COMPRESSION])]
        | otherwise = ((mainMethod ||| aDEFAULT_COMPRESSOR) ++ userMethods)
                               .$ decode_compression_method
                               .$ multimedia mm
                               .$ applyAll (map method_change mc)
                               .$ setDictionary dictionary
                               .$ limitCompressionMem   climit
                               .$ limitDecompressionMem dlimit

  -- Limit directory compression to the last method in the main chain and to the available memory
  let dirCompressor = orig_dir_compressor.$ limitCompressionMem   climit
                                          .$ limitDecompressionMem dlimit
                                          .$ getMainCompressor
                                          .$ reverse .$ take 1

  -- Max. block size of the block compressors in use, or 0
  let maxBlockSize = getBlockSize dataCompressor
  -- Memory required by the compression algorithm.
  let compressionMem = getCompressionMem dataCompressor

  -- Compute how much memory to use for the file read-ahead buffer.
  -- If the cache size is not set explicitly by the --cache option, we use from 1 mb to 16 mb,
  -- trying to keep the program's total memory consumption from exceeding
  -- half of the physical RAM (not counting the memory needed to decompress data
  -- in the archives being updated). Of course, when memory-intensive tasks run in parallel
  -- (and in particular, several copies of FreeArc at once) this tactic is not very good.
  -- It would be better to look at the amount of *free* physical RAM when the program starts
  let minCache  =  1*mb                             -- Min. cache size  - 1  mb
      maxCache  =  (16*mb) `atLeast` maxBlockSize   -- Max. cache size - 16 mb or the block size for block algorithms (lzp/grzip/dict)
      availMem  =  if i (parsePhysMem "50%") >= compressionMem      -- "Free memory" = 50% of RAM minus the memory required for compression.
                       then parsePhysMem "50%" - i compressionMem
                       else 0
      cache     =  clipToMaxInt $ atLeast aBUFFER_SIZE $  -- The cache must hold at least one buffer
                       case findReqArg o "cache" "--" of
                           "--" -> availMem.$clipTo minCache maxCache
                           "-"  -> aBUFFER_SIZE
                           s    -> parsePhysMem s

  -- Automatically enable the --recompress option for archive-copying commands
  -- if the -m../--nodata/--crconly options are given
  let recompress = findNoArg o "recompress"
                   || (is_COPYING_COMMAND cmd  &&  (mainMethod>"" || nodata || crconly))
  -- Do not recompress the existing solid blocks in the archive with --append
  -- or in archive-copying commands, unless --recompress is given explicitly
  let keep_original = findNoArg o "append"
                      || (is_COPYING_COMMAND cmd  &&  not recompress)

---------------------------------------------------------------------------------------------------
-- PREDICATES FOR DETERMINING THE GROUP NUMBER (find_group) AND FILE TYPE (find_type) -------------
  -- Determine which group list file (such as arc.groups) will be used.
  actual_group_file <- case findReqArg o "groups" "--" of
      "--" -> findFile configFilePlaces aDEFAULT_GROUPS_FILE  -- use the default groups file (arc.groups from the directory where the program lives)
      "-"  -> return ""      -- groups file disabled by option --groups-
      x    -> return x       -- groups file given explicitly by option --groups=FILENAME

  -- Read the list of groups from the groups file
  group_strings  <-  if actual_group_file > ""
                         then parseFile 'i' actual_group_file      -- parse the groups file, honouring the character encoding and line separators
                                >>== map translatePath             -- turn every '\' into '/'
                                >>== deleteIfs [match ";*", null]  -- drop comment lines and empty ones
                         else return [reANY_FILE]     -- if no groups file is used, all files belong to one common group
  -- List of predicates checking membership in each group
  let group_predicates  =  map (match_FP fpBasename) group_strings
  -- The default group, which receives all files not matching any of the wildcards.
  -- Specified by the pseudo-mask "$default"; if absent, that mask is assumed to be appended to the end of the list
  let lower_group_strings = map strLower group_strings ++ ["$default"]
      default_group = "$default" `elemIndex` lower_group_strings .$ fromJust
  -- Function "PackedFilePath -> group number from arc.groups"
  let find_group    = findGroup group_predicates default_group

  -- List of file types ($text, $exe and so on) corresponding to each group from arc.groups
  let group_type_names = go "$binary" lower_group_strings  -- initial group is "$binary"
      go t []     = []           -- walk the group list, replacing the file masks
      go t (x:xs) = case x of    --   with the file type names that precede them ("$text", "$rgb" and so on)
                      '$':_ | x/="$default" -> x : go x xs
                      _                     -> t : go t xs
  -- List of compression method numbers from the `data_compressor` list matching each group from arc.groups
  let group_types =  map typeNum group_type_names
      typeNum t   =  t `elemIndex` map fst dataCompressor `defaultVal` 0
  -- List of predicates checking that a file belongs to one of the types listed in `data_compressor`
  let type_predicates  =  const False : map match_type [1..maximum group_types]
      match_type t     =  any_function$ concat$ zipWith (\a b->([b | a == t])) group_types group_predicates
  -- Function "PackedFilePath -> compressor number in the `data_compressor` list"
  let find_type  =  findGroup type_predicates 0

-------------------------------------------------------------------------------------
-- FILE FILTER
  let match_with            =  findNoArg    o "fullnames"          .$bool fpBasename fpFullname
      orig_include_list     =  findReqList  o "include"
      orig_exclude_list     =  findReqList  o "exclude"
      include_dirs          =  findNoArgs   o "dirs" "nodirs"
      clear_archive_bit     =  findNoArg    o "ClearArchiveBit"
      select_archive_bit    =  findNoArg    o "SelectArchiveBit"
      filesize_greater_than =  findReqArg   o "SizeMore"           "--"
      filesize_less_than    =  findReqArg   o "SizeLess"           "--"
      time_before           =  findReqArg   o "TimeBefore"         "--"
      time_after            =  findReqArg   o "TimeAfter"          "--"
      time_newer            =  findReqArg   o "TimeNewer"          "--"
      time_older            =  findReqArg   o "TimeOlder"          "--"

  -- Replace references to list files (@listfile/-n@listfile/-x@listfile) with their contents
  listed_filespecs <- pure_filespecs   .$ replace_list_files parseFile >>== map translatePath
  include_list     <- orig_include_list.$ replace_list_files parseFile >>== map translatePath
  exclude_list     <- orig_exclude_list.$ replace_list_files parseFile >>== map translatePath

  -- Predicates selecting the included (-n) and excluded (-x) files. For -n we check orig_include_list, since with an empty list file no file should pass the filter
  let match_included  =  orig_include_list &&& [match_filespecs match_with include_list]
      match_excluded  =  exclude_list      &&& [match_filespecs match_with exclude_list]

  -- -ao/--SelectArchiveBit: filter by DOS Archive bit (0x20). Windows-only:
  -- on Linux fiAttr is 0, so applying the filter would exclude everything;
  -- we skip it instead, matching FreeArc behavior.
#if defined(FREEARC_WIN)
  let attrib_filter = if select_archive_bit then [\attr -> attr .&. 0x20 /= 0] else []
#else
  let attrib_filter = []
#endif

  -- Selecting files by size
  let size_filter _  "--"   = []
      size_filter op option = [(`op` parseSize option)]

  -- Selecting files by modification time, with time in the format YYYYMMDDHHMMSS
  let time_filter _  "--" = []
      time_filter op time = [(`op` (time.$makeCalendarTime.$toClockTime.$convert_ClockTime_to_CTime))]
      -- Converts a string of the form YYYY-MM-DD_HH:MM:SS into a CalendarTime and sets the correct ctTZ depending on the time of year (for which toCalendarTime.toClockTime is done twice)
      makeCalendarTime str = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct2}
          where        ct2 = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct}
                       ct = CalendarTime
                            { ctYear    = readInt (take 4 s)
                            , ctMonth   = readInt (take 2 $ drop 4 s) .$ (\x->max (x-1) 0) .$ toEnum
                            , ctDay     = readInt (take 2 $ drop 6 s)
                            , ctHour    = readInt (take 2 $ drop 8 s)
                            , ctMin     = readInt (take 2 $ drop 10 s)
                            , ctSec     = readInt (take 2 $ drop 12 s)
                            , ctPicosec = 0
                            , ctWDay    = error "ctWDay"
                            , ctYDay    = error "ctYDay"
                            , ctTZName  = error "ctTZName"
                            , ctTZ      = 0
                            , ctIsDST   = error "ctIsDST"
                            }
                       s = filter isDigit str ++ repeat '0'

  -- Selecting files by "age", with time in the format [<ndays>d][<nhours>h][<nminutes>m][<nseconds>s]
  let oldness_filter _  "--" = []
      oldness_filter op time = [(`op` (time.$calcDiff.$(`addToClockTime` current_time).$convert_ClockTime_to_CTime))]

      calcDiff  =  foldl updateTD noTimeDiff . recursive (spanBreak isDigit)
      updateTD td x = case (last x) of
                        'd' -> td {tdDay  = -readInt (init x)}
                        'h' -> td {tdHour = -readInt (init x)}
                        'm' -> td {tdMin  = -readInt (init x)}
                        's' -> td {tdSec  = -readInt (init x)}
                        _   -> td {tdDay  = -readInt x}

  -- File selection filter combining all the selection criteria,
  -- given on the command line, apart from selection by filespecs.
  -- A separate function is used for the latter,
  -- because they are used differently by commands of different kinds.
  let file_filter = all_functions$
                      concat [                     attrib_filter          .$map (. fiAttr)
                             , map (\pred -> not . pred . fiFilteredName) match_excluded
                             , nst_filters
                             ]
      nst_filters =   concat [                     match_included         .$map (. fiFilteredName)
                             , size_filter    (>)  filesize_greater_than  .$map (. fiSize)
                             , size_filter    (<)  filesize_less_than     .$map (. fiSize)
                             , time_filter    (>=) time_after             .$map (. fiTime)
                             , time_filter    (<)  time_before            .$map (. fiTime)
                             , oldness_filter (>=) time_newer             .$map (. fiTime)
                             , oldness_filter (<)  time_older             .$map (. fiTime)
                             ]

  -- If no file names to process are given and the command is not cw/d, then process all files
  filespecs <- case listed_filespecs of
      [] | cmd `elem` (words "cw d")  ->  registerError$ CMDLINE_NO_FILENAMES args
         | otherwise                  ->  return aDEFAULT_FILESPECS
      _  | cmd.$is_CMD_WITHOUT_ARGS   ->  registerError$ CMDLINE_GENERAL ["0377 command \"%1\" shouldn't have additional arguments", cmd]
         | otherwise                  ->  return listed_filespecs

  -- 0.67 options not yet functionally implemented in DArc
  let archtype = findReqArg o "type" "arc"
  when (archtype `notElem` words "arc -- ")
    (registerError$ CMDLINE_GENERAL ["0380 --type=%1: only arc format is supported", archtype])

  -- Include directories in the processing? This variable is used only when listing/extracting
  let x_include_dirs  =  case include_dirs of
           Just x  -> x   -- according to the --dirs/--nodirs options
           _       -> -- YES, if all files are processed, there are no -n/-s*/-t* filters and the command is not "e"
                      filespecs==aDEFAULT_FILESPECS && null nst_filters && cmd/="e"

-------------------------------------------------------------------------------------
-- ENCRYPTION
  -- Encryption algorithm; validity check and normalization to canonical form ("aes" -> "aes-256/ctr")
  let ea = findReqArg o "encryption" aDEFAULT_ENCRYPTION_ALGORITHM
  encryptionAlgorithm <- join_compressor ==<< (foreach (split_compressor ea) $ \algorithm -> do
    unless (isEncryption algorithm) $ do
      registerError$ CMDLINE_GENERAL ["0378 bad name or parameters in encryption algorithm %1", algorithm]
    -- ":h1" -- the key and IV in the archive we are about to WRITE are real
    -- hexadecimal. Without it, decode16 in C_Encryption.cpp falls back to
    -- char2int_broken, which folds 'a'..'f' onto 0..5 and costs about 0.75 bits
    -- per nibble of the key. Archives written before this parameter existed
    -- carry no "h" and are still read the old way; nothing else changes.
    --
    -- Set here rather than in ENCRYPTION_METHOD's constructor precisely because
    -- the constructor is also what parses a method string READ FROM AN ARCHIVE,
    -- where the absence of ":h" has to keep meaning the old decoding.
    --
    -- "-ae aes:h0" writes an old-format archive on purpose. parse_ENCRYPTION
    -- applies parameters left to right, so this one goes immediately AFTER THE
    -- NAME and anything the user wrote overrides it. Appending it to the end
    -- instead would silently ignore "-ae aes:h0".
    return$ CompressionLib.canonizeCompressionMethod (addHexFix algorithm))

  -- Passwords for the archive data and headers
  let (dpwd,hpwd) = case (findReqArg o "password"        "--" .$changeTo [("-", "--")]
                         ,findReqArg o "HeadersPassword" "--" .$changeTo [("-", "--")])
                    of
                       (p,    "--")  ->  (p,  "--")    --  -p...
                       ("--", p   )  ->  (p,  p   )    --  -hp..,
                       (p,    ""  )  ->  (p,  p   )    --  -p[PWD] -hp
                       ("",   p   )  ->  (p,  p   )    --  -p -hpPWD
                       (p1,   p2  )  ->  (p1, p2  )    --  -pPWD1 -hpPWD2

  -- Forbid asking for the passwords needed for extraction if -op-/-p-/-hp- is given
  let dont_ask_passwords  =  last ("":op_opt) == "-" || findReqArg o "OldPassword" "" == "-"  ||  findReqArg o "password" "" == "-"  ||  findReqArg o "HeadersPassword" "" == "-"
  -- List of passwords used when decompressing
  mvar_unpack_passwords  <-  newMVar$ deleteIfs [(==""), (=="?"), (=="-"), (=="--")]$ op_opt ++ findReqList o "OldPassword" ++ findReqList o "password" ++ findReqList o "HeadersPassword"
  -- Contents of the keyfiles used when decompressing
  oldKeyfileContents     <-  mapM fileGetBinary (findReqList o "OldKeyfile" ++ findReqList o "keyfile")
  -- Contents of the keyfile used when compressing
  keyfileContents        <-  unlessNull fileGetBinary (findReqArg o "keyfile" "")
  -- A password must be typed in for -p? and for -p when there is no keyfile
  let askPwd pwd          =  pwd=="?" || (pwd=="" && keyfileContents=="")
  -- The recipe for preparing the command to use encryption, or Nothing until the recipe is created
  receipt                <-  newMVar Nothing

  -- Prepares command for the use of encryption, asking the user for a password
  -- and reading the keyfiles if necessary
  let cookPasswords command (ask_encryption_password, ask_decryption_password, bad_decryption_password) = do
        modifyMVar receipt $ \x -> do
          f <- x.$maybe makeReceipt return   -- create the recipe for preparing the command for encryption, if there isn't one yet
          return (Just f, f command)         -- apply the recipe to command and remember it for later uses
       where
        makeReceipt = do
          -- Ask the user for a password if we are going to need one later
          let ask_password | cmdType cmd==ADD_CMD = ask_encryption_password parseData
                           | otherwise            = ask_decryption_password parseData
          asked_password  <-  any askPwd [dpwd,hpwd]  &&&  ask_password
          -- Add the password typed by the user, and the empty password if a keyfile may be used for decryption, to the list of extraction passwords
          asked_password      &&&  modifyMVar_ mvar_unpack_passwords (return.(asked_password:))
          oldKeyfileContents  &&&  modifyMVar_ mvar_unpack_passwords (return.("":))
          -- Append the keyfile contents to the password and replace the "--"/"?" markers
          let cook "--"             = ""                                -- encryption is disabled
              cook pwd | askPwd pwd = asked_password++keyfileContents   -- password typed on the keyboard + the keyfile contents
                       | otherwise  = pwd++keyfileContents              -- password from the command line + the keyfile contents
          return$ \command ->
                   command { opt_data_password    = cook dpwd
                           , opt_headers_password = cook hpwd
                           , opt_decryption_info  = (dont_ask_passwords, mvar_unpack_passwords, oldKeyfileContents, ask_decryption_password parseData, bad_decryption_password)}

-------------------------------------------------------------------------------------
-- MISCELLANEOUS ODDS AND ENDS
  -- Archive update algorithm
  let update_type = case cmd of
        "f"                       -> 'f'  -- command f: refresh files with newer versions, do not add new files
        "u"                       -> 'u'  -- command u: refresh files with newer versions and add new files
        _ | findNoArg o "freshen" -> 'f'  -- option  -f: see above
          | findNoArg o "update"  -> 'u'  -- option  -u: see above
          | findNoArg o "sync"    -> 's'  -- option --sync: bring the files in the archive in line with the files on disk
          | otherwise             -> 'a'  -- otherwise: replace the files in the archive with those from disk and add new files

  -- Lock the archive against changes if the "-k" option or the "k" command is used
  let lock_archive  =  findNoArg o "lock" || cmd=="k"

  -- Delete the archived files if the "-d[f]" option or the "m[f]" command is used
  delete_files  <-  case (findNoArg o "delete"   || cmd=="m"
                         ,findNoArg o "delfiles" || cmd=="mf")
                      of
                         (False, False) -> return NO_DELETE
                         (False, True ) -> return DEL_FILES
                         (True , False) -> return DEL_FILES_AND_DIRS
                         (True , True ) -> registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m/-d" "mf/-df"

  -- Forbid the use of incompatible options
  when (clear_archive_bit && delete_files/=NO_DELETE) $
      registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m[f]/-d[f]" "-ac"

  -- Directory for temporary files - may be given explicitly or via an environment variable
  let create_in_workdir = findNoArg o "create-in-workdir"
  workdir <- case orig_workdir of
               "--" | create_in_workdir -> getEnv "TMPDIR" `catch` \(_::SomeException) -> getEnv "TEMP" `catch` \(_::SomeException) -> return "/tmp"
               "--"       -> return ""    -- Default (means create files directly in the output directory)
               '%':envvar -> getEnv envvar
               dir        -> return dir

  -- Determine the sort order of files in the archive
  let sort_order  =  case (orig_sort_order, group_data) of
        (Just "-", _)  -> ""                    -- If the sort order is given as "-", disable sorting
        (Just  x,  _)  -> x                     -- If the sort order was given explicitly, use it
        (_, [GroupNone]) -> ""                  -- If solid compression is not used, disable sorting
        _  -> if getMainCompressor dataCompressor
                 .$anyf [(==aNO_COMPRESSION), isFakeCompressor, isVeryFastCompressor]
                then ""                         -- If -m0/--nodata/--crconly/tor:1..4/lzp:h13..15 - also disable sorting
                else aDEFAULT_SOLID_SORT_ORDER  -- Otherwise - use the standard sort order for solid archives

  -- Check that the "-rr" option takes one of the allowed values
  -- "+" belongs here because ArcRecover.hs has a case for it: -rr+ means
  -- "keep the archive's own setting, or the recommended amount if it had
  -- none", exactly like a bare -rr. Without it that case was unreachable and
  -- the documented spelling was rejected.
  let rr_ok = recovery `elem` ["","+","-","--"]
              || snd (parseNumber recovery 'b') `elem` ['b','%','p']
              || ';' `elem` recovery
              || '*' `elem` recovery
  unless rr_ok $ do
    registerError$ INVALID_OPTION_VALUE "recovery" "rr" ["MEM", "N", "N%", "MEM;SS", "N%;SS", "N*SS", "+", "-", ""]

  -- State of the overwrite prompt shown to the user
  ref_overwrite  <-  newIORef$ case (yes,   overwrite) of
                                    (_,     "+")  ->  "a"
                                    (_,     "-")  ->  "s"
                                    (True,  _  )  ->  "a"
                                    (False, "p")  ->  " "

  -- List of actions to perform immediately before the command starts executing
  setup_command'  <-  listVal setup_command >>== sequence_

------------------------------------------------------------------------------------------------
-- Put all of this into the structure representing the command executed by the rest of the program
  return$ Just$ Command {
      cmd_args                 = args
    , cmd_additional_args      = additional_args
    , cmd_name                 = cmd
    , cmd_arcspec              = arcspec
    , cmd_arclist              = error "Using uninitialized cmd_arclist"
    , cmd_arcname              = error "Using uninitialized cmd_arcname"
    , cmd_archive_filter       = error "Using uninitialized cmd_archive_filter"
    , cmd_filespecs            = filespecs
    , cmd_added_arcnames       = return []
    , cmd_diskfiles            = return []
    , cmd_subcommand           = False
    , cmd_setup_command        = setup_command'

    , opt_scan_subdirs         = findNoArg    o "recursive"
    , opt_add_dir              = findNoArg    o "adddir"
    , opt_add_exclude_path     = add_exclude_path
    , opt_dir_exclude_path     = dir_exclude_path
    , opt_arc_basedir          = findReqArg   o "arcpath"   "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_disk_basedir         = findReqArg   o "diskpath"  "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_no_nst_filters       = null nst_filters
    , opt_file_filter          = file_filter
    , opt_group_dir            = group_dir
    , opt_group_data           = group_data
    , opt_data_compressor      = dataCompressor
    , opt_dir_compressor       = dirCompressor
    , opt_autodetect           = ma_opt
    , opt_include_dirs         = include_dirs
    , opt_indicator            = indicator
    , opt_display              = display
    , opt_overwrite            = ref_overwrite
    , opt_keep_time            = findNoArg    o "keeptime"
    , opt_time_to_last         = findNoArg    o "timetolast"
    , opt_nodates              = findNoArg    o "nodates"
    , opt_create_in_workdir    = findNoArg    o "create-in-workdir"
    , opt_pause_before_exit    = findOptArg   o "pause-before-exit" "off"
    , opt_queue                = findNoArg    o "queue"
    , opt_volumes              = map parseSize (findReqList o "volume")
    , opt_archive_type         = findReqArg   o "type" "arc"
    , opt_shutdown             = findNoArg    o "shutdown"
    , opt_arc_32bit_legacy     = findNoArg    o "arc-32bit-legacy"
    , opt_test                 = findNoArg    o "test"
    , opt_pretest              = readInt pretest
    , opt_keep_broken          = findNoArg    o "keepbroken"
    , opt_match_with           = match_with
    , opt_append               = findNoArg    o "append"
    , opt_recompress           = recompress
    , opt_keep_original        = keep_original
    , opt_noarcext             = noarcext
    , opt_nodir                = findNoArg    o "nodir"
    , opt_cache                = cache
    , opt_update_type          = update_type
    , opt_x_include_dirs       = x_include_dirs
    , opt_sort_order           = sort_order
    , opt_reorder              = False
    , opt_find_group           = find_group . fiFilteredName
    , opt_groups_count         = length group_strings
    , opt_find_type            = find_type  . fiFilteredName
    , opt_types_count          = maximum group_types + 1
    , opt_group2type           = (listArray0 group_types!)
    , opt_arccmt_file          = findOptArg   o "arccmt"            (if cmd=="c"  then ""  else "--")   -- the "c" command is equivalent to "ch -z"
    , opt_arccmt_str           = findReqArg   o "archive-comment"   ""
    , opt_lock_archive         = lock_archive
    , opt_sfx                  = sfx
    , opt_logfile              = findReqArg   o "logfile"           ""
    , opt_delete_files         = delete_files
    , opt_workdir              = workdir
    , opt_clear_archive_bit    = clear_archive_bit
    , opt_select_archive_bit   = select_archive_bit
    , opt_language             = language
    , opt_recovery             = recovery
    , opt_broken_archive       = broken_archive
    , opt_original             = findOptArg   o "original"          "--"
    , opt_save_bad_ranges      = findReqArg   o "save-bad-ranges"   ""
    , opt_limit_compression_memory   = climit
    , opt_limit_decompression_memory = dlimit

    , opt_encryption_algorithm = encryptionAlgorithm
    , opt_cook_passwords       = cookPasswords
    , opt_data_password        = error "opt_data_password used before cookPasswords!"
    , opt_headers_password     = error "opt_headers_password used before cookPasswords!"
    , opt_decryption_info      = error "opt_decryption_info used before cookPasswords!"

    , opt_parseFile            = parseFile
    , opt_unParseFile          = unParseFile
    , opt_parseData            = parseData
    , opt_unParseData          = unParseData
    }

{-# NOINLINE addHexFix #-}
-- |Insert the ":h1" parameter directly after the algorithm name, asking for the
-- key and IV to be real hexadecimal in the archive we are about to write.
--
-- Position matters. parse_ENCRYPTION walks the parameters left to right and the
-- last assignment wins, so putting this FIRST leaves "-ae aes:h0" -- writing an
-- old-format archive on purpose, for a build that predates the parameter -- in
-- charge. Appending it would override the user instead, silently.
addHexFix algorithm =
  case split_method algorithm of
    (name:params) -> joinWith ":" (name : "h1" : params)
    _             -> algorithm

{-# NOINLINE testOption #-}
-- |Check that the option takes one of the allowed values
testOption fullname shortname option valid_values = do
  unless (option `elem` valid_values) $ do
    registerError$ INVALID_OPTION_VALUE fullname shortname valid_values

{-# NOINLINE addArcExtension #-}
-- |If the archive name has no extension and the --noarcext option is not used,
-- then add the default extension to it
addArcExtension noarcext filespec =
  case (hasExtension filespec, noarcext) of
    (False, False)  ->  filespec ++ aDEFAULT_ARC_EXTENSION
    _               ->  filespec

{-# NOINLINE replace_list_files #-}
-- |Replace references to list files ("@listfile") with their contents
replace_list_files parseFile  =  concatMapM $ \filespec ->
  case (startFrom "@" filespec) of
    Just listfile  ->  parseFile 'l' listfile >>== deleteIf null
    _              ->  return [filespec]

-- |Parsing the parameters of the "-s" option
parseSolidOption opt =
  case (split ';' opt) of
    []        ->  ([aDEFAULT_DIR_GROUPING], [GroupAll], "")   -- "-s" enables one shared solid block for all files in a single archive directory
    ["-"]     ->  ([aDEFAULT_DIR_GROUPING], [GroupNone], "")  -- "-s-" disables solid compression; directories use the standard grouping
    ["7z"]    ->  ([GroupAll],  [GroupAll], "")               -- "-s=7z"  makes one shared compressed directory and one solid block for all files in the archive
    ["cab"]   ->  ([GroupAll],  [GroupAll],  "0")   --  -dm0  -- "-s=cab" makes one shared uncompressed directory and one solid block for all files in the archive
    ["zip"]   ->  ([GroupAll],  [GroupNone], "0")   --  -dm0  -- "-s=zip" makes a separate solid block for each file in the archive, and one shared uncompressed directory
    ["arj"]   ->  ([GroupNone], [GroupNone], "0")   --  -dm0  -- "-s=arj" makes a separate solid block and directory for each file in the archive
    [dat]     ->  ([aDEFAULT_DIR_GROUPING], parse dat, "")    -- "-sXXX" sets the grouping for solid blocks only; directories use the standard grouping
    [dir,dat] ->  (parse dir, parse dat, "")                  -- "-sXXX;YYY" sets the grouping for both directories and solid blocks
  where
    -- Parser for the file grouping description:
    --   "-s/-se/-s10m/-s100f" - group everything/by extension/by 10 mb/by 100 files, respectively.
    -- `parse1` handles a single grouping description,
    -- while `parse` handles a sequence of them, for example -se100f10m
    parse = map parse1 . recursive split
      where split ('e':xs) = ("e",xs)
            split xs       = spanBreak (anyf [isDigit, (=='e')]) xs
    parse1 s = case s of
                ""  -> GroupAll
                "e" -> GroupByExt
                _   -> case (parseNumber s 'f') of
                         (num, 'b') -> GroupBySize (i num)
                         (1,   'f') -> GroupNone
                         (num, 'f') -> GroupByNumber (i num)
