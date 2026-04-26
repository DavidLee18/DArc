module GHC.Base (unsafeChr) where
import Data.Char (chr)
-- In MicroHs chr is safe enough; no bounds-checking performance concern.
unsafeChr :: Int -> Char
unsafeChr = chr
