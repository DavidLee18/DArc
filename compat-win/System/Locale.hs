-- MicroHs-compatible System.Locale shim.
-- TimeLocale is only needed as a parameter to formatCalendarTime; we
-- provide a minimal definition that carries the format strings System.Time uses.
module System.Locale (TimeLocale(..), defaultTimeLocale) where

data TimeLocale = TimeLocale
  { wDays        :: [(String, String)]   -- (full, abbreviated) weekday names
  , months       :: [(String, String)]   -- (full, abbreviated) month names
  , intervals    :: [(String, String)]
  , amPm         :: (String, String)
  , dateTimeFmt  :: String
  , dateFmt      :: String
  , timeFmt      :: String
  , time12Fmt    :: String
  } deriving (Show)

defaultTimeLocale :: TimeLocale
defaultTimeLocale = TimeLocale
  { wDays       = [ ("Sunday","Sun"),("Monday","Mon"),("Tuesday","Tue")
                  , ("Wednesday","Wed"),("Thursday","Thu"),("Friday","Fri")
                  , ("Saturday","Sat") ]
  , months      = [ ("January","Jan"),("February","Feb"),("March","Mar")
                  , ("April","Apr"),("May","May"),("June","Jun")
                  , ("July","Jul"),("August","Aug"),("September","Sep")
                  , ("October","Oct"),("November","Nov"),("December","Dec") ]
  , intervals   = []
  , amPm        = ("AM","PM")
  , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
  , dateFmt     = "%m/%d/%y"
  , timeFmt     = "%H:%M:%S"
  , time12Fmt   = "%I:%M:%S %p"
  }
