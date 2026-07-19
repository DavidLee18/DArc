----------------------------------------------------------------------------------------------------
---- Archive protection and recovery.                                                           ----
---- Implementation of the -rr option and the r command.                                        ----
---- The writeRecoveryBlocks procedure writes to the end of the archive the recovery info       ----
----   needed to recover damaged areas in the archive.                                          ----
---- The pretestArchive procedure checks whether the archive is intact, using the recovery info ----
----   and/or by testing the archive contents                                                   ----
---- The runArchiveRecovery procedure recovers the archive using the recovery info.             ----
----------------------------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
module ArcRecover where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Foreign.Ptr
import Foreign.C.Types (CChar)
import Foreign.Marshal
import Foreign.Marshal.Pool
import Foreign.Storable

import Utils
import Files
import Charsets          (linesCRLF)
import Errors
import ByteStream
import Compression
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessRead   (writeControlBlock)
import ArcCreate         (testArchive, writeSFX)

-- |Recovery info versions that we know how to use
aRecVersions = words "0.36 0.39"

-- |The recovery info version that we write into the archive depends on the number of recovery sectors
aRecVersion 0 = "0.39"
aRecVersion _ = "0.36"

{-
recovery info is written into the archive as follows:

1. After choosing the recovery sector size (it may be 512/1k/2k/4k/... bytes)
   the whole archive is split into sectors of that size. For each of them
   a CRC32 is computed, which is then stored in the recovery info.
2. At the same time N recovery sectors are created, and each archive sector
   (with number i) is mapped onto recovery sector (i `mod` N). All archive sectors
   mapped onto the same recovery sector are xored together, and the resulting
   sector is written into the recovery info. Thus, the recovery info contains
   N recovery sectors, each of which holds "generalized" information about
   the archive sectors corresponding to it.

Checking archive integrity boils down to computing the CRCs of the archive sectors. A CRC
that differs from the original one (stored in the recovery info) means that this
sector contains damage.

Recovering the archive is possible if no more than one damaged archive sector maps
onto a single recovery sector. In that case the correct contents of the damaged
sector are computed by xoring the contents of the recovery sector with all the
other archive sectors corresponding to that recovery sector.
-}

----------------------------------------------------------------------------------------------------
---- Writing the recovery information to the end of the archive ------------------------------------
----------------------------------------------------------------------------------------------------

-- |Size of one RECOVERY sector in RAR (used only for parsing the -rr option in the RAR-compatible form)
aRarRecSectorSize = 512

-- |Write a RECOVERY block into the archive
writeRecoveryBlocks archive oldarc init_pos command params bufOps = do
  rrPos <- archiveGetPos archive   -- Position where the recovery info starts in the archive
  let -- Archive size and 1% of it
      arcsize      = rrPos - init_pos
      arcsize_1p   = arcsize `divRoundUp` 100
      -- The default amount of recovery info depends on the archive size
      recommended_rr
        | arcsize<3*10^5 = "4%"
        | arcsize<2*10^6 = "2%"
        | otherwise = "1%"
      -- The old recovery info amount setting, stored in the archive itself
      old_recovery = ftRecovery (arcFooter oldarc)
      -- The new recovery info amount setting, determined by the -rr option and the old setting
      recovery = case opt_recovery command of
                   "-"     -> ""                                -- -rr-: disable adding recovery info to the archive
                   "--"    -> old_recovery                      -- by default: use the previous option setting, recorded in the archive itself
                   ""      -> old_recovery ||| recommended_rr   -- -rr: use the previous setting, or the recommended amount if recovery info was not added to the archive before
                   "+"     -> old_recovery ||| recommended_rr   -- -rr+: the same thing
                   "0.1%"  -> "0*4096"                          -- -rr0.1%: the minimum RR size, for recovery over the internet only
                   "0.01%" -> "0*65536"                         -- -rr0.01%: an even smaller RR
                   r       -> r                                 -- -rr...: add the specified amount of recovery info to the archive
  -- Exit immediately if the stars have shown that no recovery info needs to be added at all
  if recovery==""  then return ([],"")  else do
      -- Decode the -rr option value written as recovery_amount;sector_size or rec_sectors*sector_size,
      -- remembering the recovery sector size and/or their count if these values are given explicitly
  let (recovery_amount, explicit_rec_size, explicit_sector_size) = case () of
        _ | ';' `elem` recovery -> let (r,ss)  = split2 ';' recovery .$ mapSnd (i.parseSize) in
                                   (r,  Nothing,       Just ss)
          | '*' `elem` recovery -> let (ns,ss) = split2 '*' recovery .$ mapFstSnd (i.parseSize) in
                                   ("", Just (ns*ss),  Just ss)
          | otherwise           -> (recovery, Nothing, Nothing)
      -- Convert the recovery info size into bytes
      wanted_rec_size = (case parseNumber recovery_amount 's' of
                             (num,'b') -> num                        -- already given in bytes
                             (num,'s') -> num*aRarRecSectorSize   -- given in 512-byte sectors
                             (num,'%') -> arcsize_1p * num           -- given as a percentage
                             (num,'p') -> arcsize_1p * num           -- -.-
                        -- ... but must not exceed half of the RAM 8-)
                        ) `minI` (getPhysicalMemory `div` 2)
      -- The recovery sector size depends on what percentage of the archive size is taken up by
      -- the recovery info - the larger it is, the smaller the sector size can be made,
      -- without the risk that the archive sector CRCs take up too large a part of the recovery info.
      -- Reducing the recovery sector size increases the number of sectors the archive is
      -- split into, and consequently reduces the probability of them colliding on a shared
      -- recovery sector, i.e. it increases the chances of recovering the archive.
      -- With a small relative amount of recovery info (in particular, with a large archive size),
      -- the recovery sector size, on the contrary, grows without bound.
      -- The "recovery info amount -> sector size" relation is as follows: 4% -> 512, 2% -> 1024, 1% -> 2048...
      sector_size :: Integer
      sector_size =  explicit_sector_size `defaultVal`
                     case wanted_rec_size of
                       0 -> 4096  -- When -rr0% is given, only the CRCs of 4-kilobyte sectors are stored, which amounts to 0.1% of the archive size
                       _ -> (2^lb (40*arcsize `div` wanted_rec_size)) `atLeast` 512
      -- Size of the already existing part of the archive, in sectors
      arc_sectors :: Integer
      arc_sectors = i$ arcsize `divRoundUp` sector_size
      -- How many bytes the CRCs of these sectors will take up
      crcs_size0 :: Integer
      crcs_size0  = arc_sectors * toInteger (sizeOf (undefined::CRC))
      -- The real size of the recovery block
      rec_size :: Integer
      rec_size    = explicit_rec_size `defaultVal`
                    max wanted_rec_size (crcs_size0+0*sector_size)  -- The recovery block must hold at least the CRCs of the archive sectors plus 0 recovery sectors
      -- Number of recovery sectors and their total size
      rec_sectors :: Integer
      rec_sectors = (rec_size - crcs_size0) `divRoundUp` sector_size
      rec_sectors_size :: Integer
      rec_sectors_size = rec_sectors*sector_size
      -- The final size of the CRC buffer, including the CRCs of the recovery sectors themselves
      crcs_size :: Integer
      crcs_size   = crcs_size0 + rec_sectors * toInteger (sizeOf (undefined::CRC))

  -- All the parameters have been determined, now for the real work
  condPrintLineLn "r"$ "Protecting archive with "++show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++")..."
  uiStage              "0386 Protecting archive from damages"
  withPool $ \pool -> do
  sectors    <- (pooledMallocBytes pool (i rec_sectors_size) :: IO (Ptr CChar));   memset sectors 0 (i rec_sectors_size)
  buf        <- (pooledMallocBytes pool (i sector_size) :: IO (Ptr CChar))
  crcbuf     <- (pooledMallocBytes pool (i crcs_size+1) :: IO (Ptr CChar))
  crc_stream <- ByteStream.createMemBuf crcbuf (fromEnum (crcs_size+1))
  -- We start i not from zero so that the last sector of the archive maps onto the last sector
  -- of the recovery info (this guarantees that the archive can be recovered from damage in any rec_sectors
  -- consecutive sectors, including corruption of the run of sectors that spans
  -- the end of the data and the start of the archive's recovery info)
  i' <- ref ((-arc_sectors) `mod` rec_sectors)
  -- Loop over the sectors of the already written part of the archive, computing the CRC of each of them
  -- and xoring each sector into its corresponding recovery info sector
  archiveSeek archive init_pos
  uiWithProgressIndicator command arcsize $ do
    doChunks arcsize sector_size $ \(bytes :: Int) -> do
      uiUpdateProgressIndicator (i bytes)
      failOnTerminated
      len <- archiveReadBuf archive buf bytes
      crc <- calcCRC buf bytes
      ByteStream.write crc_stream crc
      when (rec_sectors>0) $ do
        idx <- val i';  i' =: (idx+1) `mod` rec_sectors
        let idxI = fromEnum idx
        let ssI = fromEnum sector_size
        memxor (sectors `plusPtr` (idxI * ssI)) buf bytes
  -- Write the CRCs of the recovery sectors themselves
  forM_ [0..rec_sectors-1] $ \i -> do
    let iI = fromEnum i
    let ssI2 = fromEnum sector_size
    crc <- calcCRC (sectors `plusPtr` (iI * ssI2)) sector_size
    ByteStream.write crc_stream crc
  -- Write two separate recovery blocks - one with the xor sectors and one with the CRCs of the archive blocks.
  -- The second block also includes service information describing the structure of the recovery info
  -- (version number, the size and the starting address in the archive of the protected data,
  --  the number of sectors and the sector size in each "compartment" the recovery info is split into).
  -- Moreover, for recovering the data only the integrity of the second, smaller block is required
  -- (spoiled recovery sectors from the first block will simply not be used in practice,
  --  since the CRC of the data sectors "recovered" with their help would simply come out wrong).
  archiveSeek archive rrPos
  r0 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Nothing::Maybe Int) sectors (fromEnum rec_sectors_size) bufOps
  curpos <- archiveGetPos archive
  let addinfo = (aRecVersion rec_sectors, arcsize::Integer, curpos-init_pos::Integer, [(toInteger sector_size, toInteger rec_sectors)])
  r1 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Just addinfo) crcbuf (fromEnum crcs_size) bufOps
  return ([r0,r1],recovery)


-- |Reading the recovery info header, which describes all of its parameters
readControlInfo crc_stream crcs_block = do
  ByteStream.rewindMemory crc_stream
  -- Besides the CRCs of the archive sectors, the second recovery block also contains meta-information.
  -- To guard against misinterpreting it, it starts with the version number of the program
  -- compatible with this meta-information format
  version <- ByteStream.read crc_stream
  if version `notElem` aRecVersions  then return$ Left version  else do
  -- Read the header of the second recovery block, which contains all the necessary data
  -- about this recovery information - the starting address of the protected data (encoded as
  -- an offset from the start of the second recovery block to the start of the protected data),
  -- the size of the protected data (arcsize), and finally the size and number of recovery sectors
  -- in each "compartment" of the recovery information
  (arcsize::Integer, offset::Integer) <- ByteStream.read crc_stream
  let init_pos = blPos crcs_block - offset
  (sector_size,rec_sectors):_ <- ByteStream.read crc_stream >>== mapFsts fromInteger >>== mapSnds fromInteger
  return$ Right (init_pos, arcsize, sector_size, rec_sectors)


----------------------------------------------------------------------------------------------------
---- Checking the archive with the help of the recovery information --------------------------------
----------------------------------------------------------------------------------------------------

-- |Checking the integrity of an archive containing recovery information,
-- and an emergency exit if the archive contains damage
pretestArchive command archive footer = do
  when (opt_pretest command>0) $ do
    result <- withPool$ scanArchive command archive footer False
    case result of
      Just (_, sector_size, bad_crcs)  |  bad_sectors <- genericLength bad_crcs, bad_sectors>0
              -> registerError$ BROKEN_ARCHIVE (archiveName archive) ["0352 found %1 errors (%2)", show3 bad_sectors, showMemory (bad_sectors*sector_size)]
      Just _  -> condPrintLineLn "r" "Archive integrity OK"
      _       -> return ()
    -- Full archive testing only with -pt3, or with -pt2 when the archive contains no recovery information
    when (opt_pretest command==3 || (opt_pretest command==2 && isNothing result)) $ do
      w <- count_warnings $ do
               testArchive command (cmd_arcname command) doNothing3
      -- Only continue working if there were no warnings
      when (w>0) $ do
        registerError$ BROKEN_ARCHIVE (archiveName archive) ["0353 there were %1 warnings due archive testing", show w]


-- |Scan the archive and return the list of damaged sectors
-- (they are identified by comparing the CRC of each sector with its CRC stored in the second recovery block)
scanArchive command archive footer recovery pool = do
  -- Find the recovery blocks in the archive. The current version can only process a single pair of recovery blocks
  let recovery_blocks  =  filter ((RECOVERY_BLOCK==) . blType) (ftBlocks footer)
  if length recovery_blocks < 2  then return Nothing  else do
  let sectors_block:crcs_block:_ = recovery_blocks
  when (length recovery_blocks > 2) $ do
      registerWarning$ GENERAL_ERROR ["0344 only first of %1 recovery records can be processed by this program version. Please use newer versions to process the rest", show (length recovery_blocks `div` 2)]

  -- Read the RECOVERY blocks (sectors+crcs)
  sectors <- if recovery  then archiveBlockReadUnchecked pool sectors_block
                          else return$ error "scanArchive:sectors undefined"
  (crcbuf, crcsize) <- archiveBlockReadAll pool (error "encrypted recovery block") crcs_block
  crc_stream <- ByteStream.openMemory crcbuf crcsize

  -- Read the crc_stream header, which contains all the necessary data about this recovery information
  info <- readControlInfo crc_stream crcs_block
  case info of
    Left version -> do registerWarning$ GENERAL_ERROR ["0345 you need FreeArc %1 or above to process this recovery info", version]
                       return Nothing
    Right (init_pos, arcsize, sector_size, rec_sectors) -> do
      -- Un-xor the archive sectors with the corresponding sectors of the RECOVERY block.
      -- Put into bad_crcs the list of archive sectors whose CRCs do not match the reference ones.
      condPrintLineLn "r"$ show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++") present"
      condPrintLineLn "r" "Scanning archive for damages..."
      uiStage              "0385 Scanning archive for damages"
      archiveSeek archive init_pos
      buf <- pooledMallocBytes pool sector_size
      -- Size of the protected part of the archive, in sectors
      let arc_sectors = i$ arcsize `divRoundUp` sector_size
      -- i does not start from zero because (see writeRecoveryBlocks)
      i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
      bad_crcs <- withList $ \bad_crcs -> do
        -- Loop over the archive sectors, displaying a progress indicator
        uiWithProgressIndicator command arcsize $ do
          doChunks arcsize sector_size $ \(bytes :: Int) -> do
            uiUpdateProgressIndicator (i bytes)
            failOnTerminated
            len <- archiveReadBuf archive buf bytes
            -- We xor the sectors corresponding to one recovery sector, in order to obtain the data for recovering a damaged sector
            when (recovery && rec_sectors>0) $ do
              idx <- val i';  i' =: (idx+1) `mod` rec_sectors
              let idxI4 = fromEnum idx
              let ssI4 = fromEnum sector_size
              memxor (sectors `plusPtr` (idxI4 * ssI4)) buf bytes
            -- We save the numbers of the damaged sectors (those whose CRC does not match the reference one)
            n <- val n';  n `seq` (n' =: n+1)
            crc          <- calcCRC buf bytes
            original_crc <- ByteStream.read crc_stream
            when (crc/=original_crc) $ do
              bad_crcs <<= n
      return$ Just ((crcs_block,crc_stream,sectors,buf), sector_size, bad_crcs)


----------------------------------------------------------------------------------------------------
---- Recovering the archive with the help of the recovery information ------------------------------
----------------------------------------------------------------------------------------------------

-- |Command for recovering a corrupt archive
runArchiveRecovery command@Command{ cmd_filespecs       = filespecs
                                  , cmd_arcname         = arcname
                                  , opt_original        = opt_original
                                  , opt_save_bad_ranges = opt_save_bad_ranges
                                  } = do
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []
  let arcname_fixed = arcname `replaceBaseName` ("fixed."++takeBaseName arcname)
  whenM (fileExist arcname_fixed) $ do
    registerError$ GENERAL_ERROR ["0346 file %1 already exists", arcname_fixed]
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- prepare the passwords in the command for use
  withPool $ \pool -> do   -- we use a memory pool so that the allocated buffers are freed automatically on exit
  bracketCtrlBreak "archiveClose1:ArcRecover" (archiveReadFooter command arcname) (archiveClose.fst) $ \(archive,footer) -> do
    -- First stage - scanning the archive and building the list of damaged sectors
    result <- scanArchive command archive footer True pool
    if isNothing result
        then registerError$ GENERAL_ERROR ["0347 archive can't be recovered - recovery data absent or corrupt"]
        else do
    -- Moving on to recovering the data
    let Just ((crcs_block,crc_stream,sectors,buf),_,bad_crcs) = result
    if null bad_crcs  then condPrintLine "n" "Archive ok, no need to restore it!"  else do
    -- Read the crc_stream header, which contains all the necessary data about this recovery information
    Right (init_pos, arcsize, sector_size, rec_sectors) <- readControlInfo crc_stream crcs_block

    -- Build the list of sectors that we will be able to recover, and of those that lay claim
    -- to one and the same recovery sector and therefore cannot be recovered
    let (recoverable,bad)  =  case rec_sectors of
           0 -> ([], bad_crcs)      -- if the RR contains no recovery sectors, then no archive sector can be recovered with their help :D
           _ -> bad_crcs .$ sort_and_groupOn (`mod` rec_sectors)   -- group together those damaged sectors that fall onto the same RECOVERY sector
                         .$ partition (null.tail)                  -- separate the groups with only one element (a sector that can be recovered unambiguously) from the rest
                         .$ mapFst concat .$ mapSnd concat
        bad_sectors = genericLength bad
        recoverable_sectors = genericLength recoverable

    -- This procedure writes to a file the list of corrupt byte ranges in the archive
    let arcPos sector = sector*sector_size+init_pos
    let save_bad_ranges bad_sectors = do
          when (opt_save_bad_ranges>"") $ do
            let byte_range sector = show start++"-"++show end
                                      where start = arcPos sector
                                            end   = start+sector_size-1
            filePutBinary opt_save_bad_ranges (joinWith "," $ map byte_range bad_sectors)

    -- If we can't recover anything, all that's left is to shoot ourselves :)
    originalName <- originalURL opt_original arcname
    when (null recoverable && originalName=="") $ do
      save_bad_ranges bad
      registerError$ GENERAL_ERROR ["0348 %1 unrecoverable errors (%2) found, can't restore anything!",
                                    show3 bad_sectors, showMemory (bad_sectors*sector_size)]

    -- Copy the file, substituting the correct contents for the damaged sectors from the recoverable list (the bad sectors cannot be recovered because of the ambiguity)
    condPrintLineLn "n"$ show3 recoverable_sectors++" recoverable errors ("++showMemory (recoverable_sectors*sector_size)++") "
                         ++(bad &&& "and "++show3 bad_sectors++" unrecoverable errors ("++showMemory (bad_sectors*sector_size)++") ")
                         ++"found"
    archiveFullSize <- archiveGetSize archive
    condPrintLineLn "n"$ "Recovering "++showMem archiveFullSize++" archive..."
    uiStage              "0387 Recovering archive"
    errors' <- ref bad
    -- Moving on to creating the archive with the recovered data
    handleCtrlBreak  "fileRemove arcname_fixed" (ignoreErrors$ fileRemove arcname_fixed) $ do
    bracketCtrlBreak "archiveClose2:ArcRecover" (archiveCreateRW arcname_fixed) archiveClose $ \new_archive -> do
    withJIT (fileOpen =<< originalURL originalName arcname) fileClose $ \original' -> do   -- Lazily open the file from which correct data can be loaded
    writeSFX (opt_sfx command) new_archive (dirlessArchive archive footer)   -- Start creating the archive by writing the SFX module
    archiveSeek archive init_pos
    -- Size of the protected part of the archive, in sectors
    let arc_sectors = i$ arcsize `divRoundUp` sector_size
    -- i does not start from zero because (see writeRecoveryBlocks)
    i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
    originalErr <- init_once

    -- Loop over the sectors of the archive being recovered, displaying a progress indicator
    uiWithProgressIndicator command arcsize $ do
      doChunks arcsize sector_size $ \(bytes :: Int) -> do
        uiUpdateProgressIndicator (i bytes)
        failOnTerminated
        idx <- val i';  when (rec_sectors>0) $  do idx `seq` (i' =: (idx+1) `mod` rec_sectors)
        n <- val n';  n' =: n+1
        len <- archiveReadBuf archive buf bytes
        original_crc <- ByteStream.read crc_stream

        -- If this is one of the recoverable sectors, then recover its contents by
        -- xoring it with the reference sector, which right now contains exactly
        -- the data needed for the recovery
        when (n `elem` recoverable) $ do
          let idxI2 = fromEnum idx
          let ssI3 = fromEnum sector_size
          let do_xor = memxor buf (sectors `plusPtr` (idxI2 * ssI3)) bytes
          do_xor
          -- If the CRC still does not match after that (which is possible when the reference sector itself is in error),
          -- then restore the sector's original contents and remember
          -- that unrecovered sectors remain in the archive
          crc <- calcCRC buf bytes
          when (crc/=original_crc) $ do
            do_xor;  errors' .= (n:)

        -- If this is a damaged sector that cannot be recovered with the information at hand,
        -- then simply download it again (if --original was specified)
        errors <- val errors'
        when (originalName>"" && n `elem` errors) $ do
          -- First of all, check that the original file could be opened
          eitherM_ (try$ valJIT original' :: IO (Either SomeException File))
            ( \exception -> once originalErr$ registerWarning$ GENERAL_ERROR ["0349 can't open original at %1", originalName])
            $ \original  -> do
          -- Now check that its size matches the archive being recovered
          dwnl_size <- fileGetSize original
          if dwnl_size /= archiveFullSize
            then once originalErr$ registerWarning$ GENERAL_ERROR
                      ["0350 %1 has size %2 so it can't be used to recover %3 having size %4",
                       originalName, show3 dwnl_size, arcname, show3 archiveFullSize]
            else do
          -- Read the damaged sector from the original
          allocaBytes bytes $ \temp -> do
          fileSeek    original (arcPos n)
          fileReadBuf original temp bytes
          -- If the sector we read has the correct CRC, use it to replace the sector read from the source archive
          crc <- calcCRC temp bytes
          when (crc==original_crc) $ do
            copyBytes buf temp bytes
            errors' .= delete n

        -- Write the [recovered] sector into the new archive
        archiveWriteBuf new_archive buf bytes

    -- Copy the recovery blocks (or rather, the entire remainder of the old archive file after the protected data)
    pos <- archiveGetPos archive
    archiveCopyData archive pos (archiveFullSize-pos) new_archive

    condPrintLineLn "n"$ "Recovered archive saved to "++arcname_fixed
    errors <- val errors'
    save_bad_ranges errors
    when (errors>[]) $ do
      let errnum = genericLength errors
      registerWarning$ GENERAL_ERROR ["0351 %1 errors (%2) remain unrecovered", show3 errnum, showMemory (errnum*sector_size)]
  return (1,0,0,0)



-- |Compute the URL of the original, based on the contents of the --original option and the archive name
originalURL opt_original arcname =
  case opt_original of
    "--"         -> return ""              -- disabled
    '?':command  -> run_command command    -- the URL is obtained by running the command `command arcname`
    ""           -> auto_url               -- the URL is determined automatically from files.bbs/descript.ion
    url          -> return url             -- the URL is given explicitly
 where

  -- Run the command and return its output as the URL
    run_command command  =  runProgram (command++" "++arcname)
                          >>== head.linesCRLF

  -- URL picked automatically from the archive description in files.bbs/descript.ion
    auto_url = mapMaybeM try_descr (words "files.bbs descript.ion") >>== catMaybes >>== listToMaybe >>== fromMaybe ""

  -- Look for the archive URL in the description file descr
    try_descr descr = do
      let descrname = takeDirectory arcname </> descr
          basename  = takeFileName  arcname
      fileExist descrname >>= bool (return Nothing) (do
          fileGetBinary descrname >>== linesCRLF
      -- Lines starting with spaces must be appended to the preceding ones (these are description continuation lines)
          >>== joinContLines ""
      -- The line we are looking for in files.bbs may start with name.arc or with "The Name.arc", followed by a space
          >>== listToMaybe . concatMap (filter (isSpace.head) . catMaybes . (\x -> [x.$startFrom basename
                                          ,x.$startFrom ("\""++basename++"\"")]))
      -- Extract the URL from the description line
          >>== fmap findURL)

      where
        findURL s = firstJust$ map (getURL s)$ strPositions s "://"
      -- Extract from the string s the URL whose "://" is located at offset n
        getURL s n = let (pre,post) = splitAt n s
                         prefix    = reverse$ takeWhile isURLPrefix$ reverse pre
                         postfix   = takeWhile isURLChar$ drop 3 post
                     in
                         prefix &&& postfix &&& Just (prefix++"://"++postfix)

  -- Characters that may occur in the prefix or the body of a URL
    isURLPrefix = anyf [isAsciiLower, isAsciiUpper]
    isURLChar   = anyf [flip elem "+-=._/*(),@'$:;&!?%", isDigit, isAsciiLower, isAsciiUpper]

  -- Merge continuation lines (those starting with spaces) with the preceding lines
    joinContLines prev (x@(c:_):xs) | isSpace c   =   joinContLines (prev++x) xs
    joinContLines prev (x:xs)                     =   prev : joinContLines x xs
    joinContLines prev []                         =   [prev]

