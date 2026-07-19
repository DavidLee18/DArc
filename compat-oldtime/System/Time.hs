-- MicroHs-compatible System.Time shim.
-- Implements the old-time API via C FFI (time.h / struct tm).
{-# LANGUAGE CPP #-}
module System.Time
  ( ClockTime(..)
  , CalendarTime(..)
  , TimeDiff(..)
  , Month(..)
  , Day(..)
  , noTimeDiff
  , getClockTime
  , toCalendarTime
  , toUTCTime
  , toClockTime
  , addToClockTime
  , diffClockTimes
  , formatCalendarTime
  ) where

import Foreign.C.Types   (CInt(..), CLong(..), CSize(..))
import Foreign.C.String  (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Ptr       (Ptr, castPtr, nullPtr)
import Foreign.Storable  (peek, peekByteOff)
import System.IO.Unsafe  (unsafePerformIO)
import System.Locale     (TimeLocale(..))

data ClockTime = TOD !Integer !Integer deriving (Eq, Ord, Show)

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
         deriving (Eq, Ord, Enum, Bounded, Show, Read)

data CalendarTime = CalendarTime
  { ctYear    :: Int
  , ctMonth   :: Month
  , ctDay     :: Int
  , ctHour    :: Int
  , ctMin     :: Int
  , ctSec     :: Int
  , ctPicosec :: Integer
  , ctWDay    :: Day
  , ctYDay    :: Int
  , ctTZName  :: String
  , ctTZ      :: Int
  , ctIsDST   :: Bool
  } deriving (Eq, Ord, Show)

data TimeDiff = TimeDiff
  { tdYear    :: Int, tdMonth   :: Int, tdDay     :: Int
  , tdHour    :: Int, tdMin     :: Int, tdSec     :: Int
  , tdPicosec :: Integer
  } deriving (Eq, Ord, Show)

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0

-- C helpers (defined in Environment.cpp)
-- MicroHs truncates FFI return values to 32 bits; use _w variants with pointer.
foreign import ccall "darc_time_w"       darc_time_w       :: Ptr CLong -> IO ()
foreign import ccall "darc_localtime"    darc_localtime     :: CLong -> Ptr CInt -> IO ()
foreign import ccall "darc_gmtime"       darc_gmtime        :: CLong -> Ptr CInt -> IO ()
foreign import ccall "darc_mktime_tz_w"  darc_mktime_tz_w   :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CLong -> IO ()
foreign import ccall "darc_strftime"     darc_strftime       :: Ptr () -> CSize -> CString -> Ptr CInt -> IO CInt

-- struct tm layout as array of CInt (indices 0-9: sec,min,hour,mday,mon,year,wday,yday,isdst,gmtoff_min)
-- This is a simplified flat layout used by our C helpers (not actual struct tm).
sizeof_tm_ints :: Int
sizeof_tm_ints = 10 * 4  -- 10 CInt values

readTm :: Ptr CInt -> IO CalendarTime
readTm p = do
  sec   <- fmap fromIntegral (peekByteOff p  0 :: IO CInt)
  min_  <- fmap fromIntegral (peekByteOff p  4 :: IO CInt)
  hour  <- fmap fromIntegral (peekByteOff p  8 :: IO CInt)
  mday  <- fmap fromIntegral (peekByteOff p 12 :: IO CInt)
  mon   <- fmap fromIntegral (peekByteOff p 16 :: IO CInt)
  year  <- fmap fromIntegral (peekByteOff p 20 :: IO CInt)
  wday  <- fmap fromIntegral (peekByteOff p 24 :: IO CInt)
  yday  <- fmap fromIntegral (peekByteOff p 28 :: IO CInt)
  isdst <- fmap fromIntegral (peekByteOff p 32 :: IO CInt)
  gmtoffMin <- fmap fromIntegral (peekByteOff p 36 :: IO CInt)
  return CalendarTime
    { ctYear    = 1900 + year
    , ctMonth   = toEnum (mon `mod` 12)
    , ctDay     = mday
    , ctHour    = hour
    , ctMin     = min_
    , ctSec     = sec
    , ctPicosec = 0
    , ctWDay    = toEnum (wday `mod` 7)
    , ctYDay    = yday
    , ctTZName  = ""
    , ctTZ      = gmtoffMin * 60
    , ctIsDST   = isdst /= 0
    }

getClockTime :: IO ClockTime
getClockTime = alloca $ \pOut -> do
  darc_time_w pOut
  t <- peek pOut
  return (TOD (fromIntegral t) 0)

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD secs _) = allocaBytes sizeof_tm_ints $ \p -> do
  darc_localtime (fromIntegral secs) p
  readTm p

toUTCTime :: ClockTime -> CalendarTime
toUTCTime (TOD secs _) = unsafePerformIO $ allocaBytes sizeof_tm_ints $ \p -> do
  darc_gmtime (fromIntegral secs) p
  readTm p

toClockTime :: CalendarTime -> ClockTime
toClockTime ct = unsafePerformIO $ alloca $ \pOut -> do
  darc_mktime_tz_w
         (fromIntegral (ctYear ct - 1900))
         (fromIntegral (fromEnum (ctMonth ct)))
         (fromIntegral (ctDay  ct))
         (fromIntegral (ctHour ct))
         (fromIntegral (ctMin  ct))
         (fromIntegral (ctSec  ct))
         (fromIntegral (ctTZ   ct `div` 60))
         pOut
  t <- peek pOut
  return (TOD (fromIntegral t) 0)

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (TOD secs ps) =
  let delta = fromIntegral (tdDay td) * 86400
            + fromIntegral (tdHour td) * 3600
            + fromIntegral (tdMin td) * 60
            + fromIntegral (tdSec td)
  in TOD (secs + delta) ps

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (TOD s1 _) (TOD s2 _) =
  let diff  = s1 - s2
      days  = diff `div` 86400
      rest  = diff `mod` 86400
      hrs   = rest `div` 3600
      rest2 = rest `mod` 3600
      mins  = rest2 `div` 60
      secs  = rest2 `mod` 60
  in noTimeDiff { tdDay = fromIntegral days, tdHour = fromIntegral hrs
                , tdMin = fromIntegral mins,  tdSec  = fromIntegral secs }

-- | Format a CalendarTime using strftime-style format string.
formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime _ fmt ct = unsafePerformIO $
  allocaBytes sizeof_tm_ints $ \p -> do
    -- Fill our flat tm layout from CalendarTime
    let w off v = (peekByteOff p off :: IO CInt) >> return ()
    -- Use darc_fill_tm to populate
    darc_fill_tm p
      (fromIntegral (ctSec  ct)) (fromIntegral (ctMin  ct)) (fromIntegral (ctHour ct))
      (fromIntegral (ctDay  ct)) (fromIntegral (fromEnum (ctMonth ct)))
      (fromIntegral (ctYear ct - 1900)) (fromIntegral (fromEnum (ctWDay ct)))
      (fromIntegral (ctYDay ct)) (fromIntegral (if ctIsDST ct then 1 else 0 :: Int))
      (fromIntegral (ctTZ ct `div` 60))
    let bufSize = 512
    allocaBytes bufSize $ \buf ->
      withCString fmt $ \cfmt -> do
        _ <- darc_strftime (castPtr buf) (fromIntegral bufSize) cfmt p
        peekCString (castPtr buf)

foreign import ccall "darc_fill_tm" darc_fill_tm
  :: Ptr CInt
  -> CInt -> CInt -> CInt  -- sec, min, hour
  -> CInt -> CInt -> CInt  -- mday, mon, year
  -> CInt -> CInt -> CInt  -- wday, yday, isdst
  -> CInt                  -- gmtoff_min
  -> IO ()
