-- Compatibility shim: System.Time for GHC
-- Wraps Data.Time to provide the old-time API
module System.Time (
  ClockTime(TOD),
  CalendarTime(..),
  TimeDiff(..),
  Month(..),
  Day(..),
  noTimeDiff,
  getClockTime,
  toCalendarTime,
  toUTCTime,
  toClockTime,
  addToClockTime,
  diffClockTimes,
  formatCalendarTime,
) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Time.Calendar as Cal
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.LocalTime
import Data.Time.Format (formatTime, TimeLocale)
import System.IO.Unsafe (unsafePerformIO)

-- | ClockTime is a POSIX time: seconds + picoseconds
data ClockTime = TOD !Integer !Integer deriving (Eq, Ord)
instance Show ClockTime where
  show (TOD s ps) = "TOD " ++ show s ++ " " ++ show ps

-- | Calendar time broken down into components
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
  , ctTZ      :: Int       -- offset in seconds from UTC
  , ctIsDST   :: Bool
  } deriving (Eq, Ord, Show)

-- | Month enumeration (January = 0 in old-time API)
data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Day of week (Sunday = 0 in old-time API)
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
         deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Time difference
data TimeDiff = TimeDiff
  { tdYear    :: Int
  , tdMonth   :: Int
  , tdDay     :: Int
  , tdHour    :: Int
  , tdMin     :: Int
  , tdSec     :: Int
  , tdPicosec :: Integer
  } deriving (Eq, Ord, Show)

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0

getClockTime :: IO ClockTime
getClockTime = do
  t <- getPOSIXTime
  let secs = floor t :: Integer
      ps   = round ((t - fromIntegral secs) * 1e12) :: Integer
  return (TOD secs ps)

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD secs _ps) = do  -- sub-second (_ps) precision is not preserved
  tz <- getCurrentTimeZone
  let utct = posixSecondsToUTCTime (fromIntegral secs)
      lt   = utcToLocalTime tz utct
      (y, m, d) = Cal.toGregorian (localDay lt)
      tod  = localTimeOfDay lt
      wday = toEnum $ (fromEnum (Cal.dayOfWeek (localDay lt)) `mod` 7) :: Day
      yday = snd (toOrdinalDate (localDay lt)) - 1
      tzOffset = timeZoneMinutes tz * 60
  return CalendarTime
    { ctYear    = fromIntegral y
    , ctMonth   = toEnum (m - 1)
    , ctDay     = d
    , ctHour    = todHour tod
    , ctMin     = todMin tod
    , ctSec     = floor (todSec tod)
    , ctPicosec = 0
    , ctWDay    = wday
    , ctYDay    = yday
    , ctTZName  = timeZoneName tz
    , ctTZ      = tzOffset
    , ctIsDST   = timeZoneSummerOnly tz
    }

toUTCTime :: ClockTime -> CalendarTime
toUTCTime (TOD secs _ps) =
  let utct = posixSecondsToUTCTime (fromIntegral secs)
      lt   = utcToLocalTime utc utct
      (y, m, d) = Cal.toGregorian (localDay lt)
      tod  = localTimeOfDay lt
      wday = toEnum $ (fromEnum (Cal.dayOfWeek (localDay lt)) `mod` 7) :: Day
      yday = snd (toOrdinalDate (localDay lt)) - 1
  in CalendarTime
    { ctYear    = fromIntegral y
    , ctMonth   = toEnum (m - 1)
    , ctDay     = d
    , ctHour    = todHour tod
    , ctMin     = todMin tod
    , ctSec     = floor (todSec tod)
    , ctPicosec = 0
    , ctWDay    = wday
    , ctYDay    = yday
    , ctTZName  = "UTC"
    , ctTZ      = 0
    , ctIsDST   = False
    }

toClockTime :: CalendarTime -> ClockTime
toClockTime ct =
  let day  = Cal.fromGregorian (fromIntegral (ctYear ct)) (fromEnum (ctMonth ct) + 1) (ctDay ct)
      tod  = TimeOfDay (ctHour ct) (ctMin ct) (fromIntegral (ctSec ct))
      lt   = LocalTime day tod
      tz   = minutesToTimeZone (ctTZ ct `div` 60)
      utct = localTimeToUTC tz lt
      secs = floor (utcTimeToPOSIXSeconds utct) :: Integer
  in TOD secs 0

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (TOD secs ps) =
  let deltaSecs = fromIntegral (tdDay td) * 86400
                + fromIntegral (tdHour td) * 3600
                + fromIntegral (tdMin td) * 60
                + fromIntegral (tdSec td)
  in TOD (secs + deltaSecs) ps

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (TOD s1 _) (TOD s2 _) =
  let diff = s1 - s2
      days = diff `div` 86400
      rest = diff `mod` 86400
      hrs  = rest `div` 3600
      rest2 = rest `mod` 3600
      mins = rest2 `div` 60
      secs = rest2 `mod` 60
  in noTimeDiff { tdDay = fromIntegral days, tdHour = fromIntegral hrs
                , tdMin = fromIntegral mins, tdSec = fromIntegral secs }

-- | Format a calendar time using a format string.
-- Uses Data.Time.Format's formatTime under the hood.
formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime locale fmt ct =
  let day  = Cal.fromGregorian (fromIntegral (ctYear ct)) (fromEnum (ctMonth ct) + 1) (ctDay ct)
      tod  = TimeOfDay (ctHour ct) (ctMin ct) (fromIntegral (ctSec ct))
      tz   = minutesToTimeZone (ctTZ ct `div` 60)
      zt   = ZonedTime (LocalTime day tod) tz
  in formatTime locale fmt zt
