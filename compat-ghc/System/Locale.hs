-- Compatibility shim: System.Locale for GHC
-- Re-exports TimeLocale from Data.Time.Format (time package)
module System.Locale (
  TimeLocale(..),
  defaultTimeLocale,
) where
import Data.Time.Format (TimeLocale(..), defaultTimeLocale)
