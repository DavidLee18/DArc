{-# LANGUAGE CPP #-}
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

-- "7tr arc.russian.txt pl.txt" translates 7-zip language file pl.txt
--   into the FreeArc language file arc.polish.txt
-- "7tr arc.russian.txt pl.txt dir\arc.polish.txt" translates 7-zip language file pl.txt
--   and old FreeArc language file dir\arc.polish.txt into the new FreeArc language file arc.polish.txt
main = do (my:szip:old) <- getArgs
          dict    <-  readFile szip >>= return.makeDict
          oldLang <-  if null old then return []
                                  else readFile (head old) >>= return.makeOldLang
          let lang = dict .$lookup "00000000" .$fromMaybe "_New" .$replace ' ' '_' .$map toLower
              out  = "arc."++lang++".txt"
          readFile my >>= writeFile out.unlines.map (makeLine dict oldLang).lines

-- |Build a dictionary from a 7-zip localization file
makeDict x = ("copyright", c): xs
  where l  = x .$ lines
        xs = l .$ map (split2 '=')
               .$ map (\(key,str) -> (trim key, drop 1 $dropEnd 1 $trim str))
        c = l .$drop 2 .$takeWhile ((";"==).take 1) .$map (dropWhile isSpace.drop 1) .$filter (not.null) .$joinWith "\\n"

-- |Build a dictionary from an old FreeArc localization file
makeOldLang x = x .$ lines
                  .$ filter (\s -> length s > 4  &&  s `contains` '=')
                  .$ filter (all isDigit.take 4)
                  .$ map    (split2 '=')
                  .$ filter (("??"/=).snd)
                  .$ map    (\(a,b) -> (take 4 a, b))

-- |Convert the string x into the language described by dict
makeLine dict oldLang x =
  case x.$ split2 '=' of
    (x,xs) | (n,eng) <- split2 ' ' x,
             length n==4, all isDigit n
       -> x++"="++(n `lookup` oldLang `defaultVal`
                   makeSubst dict n .$replaceAll "{0}" "%1" .$replaceAll "{1}" "%2")
    _  -> x

makeSubst dict n = case n of
  "0000" -> d "00000000"++" ("++d "00000001"++")"
  "0159" -> "Automatic translation from 7-Zip language file.\\nPlease edit it to finish translation.\\nOriginal copyrights:\\n"++d "copyright"
  "0022" ->(d "02000301".$replaceAll "{0}" "{1}")++", "++d "02000982"
  "0023" ->(d "02000302".$replaceAll "{0}" "{1}")++", "++d "02000982"
  "0020" -> d "03020215".$replaceAll "{0}" "{1}"
  "0019" -> d "03020215".$replaceAll "{0}" "{1}"
  "0114" -> d "02000D0B"++" %1"
  "0115" -> d "02000D0E"++" %2, "++d "02000C04"++" %1"
  "0116" -> d "02000D0F"++" %2, "++d "02000C04"++" %1"
  "0024" -> d "02000800"++" %3"
  "0152" -> d "02000109"++" %3"
  "0172" ->(d "03010302".$tryToSkipAtEnd ":" .$replaceAll "7-Zip" "FreeArc" .$replaceAll "7-zip" "FreeArc") ++ " .arc"
  "0165" -> "%1\\n"++(d "02000982".$replaceAll "{0}" "%2")++"\\n"++d "02000983"++" %3"
  _      -> d (trans .$lookup n .$fromMaybe ":(")
  where d n = dict .$lookup n .$fromMaybe "??"

-- |Translation of message numbers from FreeArc to 7-zip
trans = map (split2 ' ')
        ["0050 03000102"
        ,"0066 03000103"
        ,"0259 03000105"
        ,"0260 02000D07"
        ,"0261 03000106"

        ,"0262 02000103"
        ,"0263 03000330"
        ,"0008 03000333"
        ,"0009 03000334"
        ,"0037 03020250"
        ,"0038 03020251"
        ,"0264 03000332"
        ,"0039 03000440"
        ,"0036 03000260"

        ,"0030 03020400"
        ,"0040 02000108"
        ,"0033 03000233"
        ,"0034 03020402"
        ,"0044 0200010A"
        ,"0086 03020423"
        ,"0035 03020401"
        ,"0045 02000106"
        ,"0064 03010400"

        ,"0268 03020290"
        ,"0272 02000D10"


        ,"0015 02000204"
        ,"0016 02000207"
        ,"0017 0200020C"
        ,"0018 02000140"

        ,"0134 02000108"
        ,"0135 02000108"
        ,"0136 02000108"

        ,"0107 02000D0B"
        ,"0129 02000D04"
        ,"0108 02000D83"
        ,"0110 02000D82"
        ,"0111 02000D84"
        ,"0112 02000D85"
        ,"0137 02000D13"
        ,"0138 02000322"
        ,"0139 02000320"

        ,"0119 02000D10"
        ,"0120 02000D0A"
        ,"0121 02000D11"
        ,"0131 02000D01"

        ,"0160 03020213"
        ,"0161 03020213"

        ,"0005 02000820"
        ,"0001 02000821"
        ,"0002 02000822"
        ,"0051 02000823"
        ,"0004 02000801"
        ,"0021 02000881"

        ,"0088 02000320"
        ,"0089 02000C03"
        ,"0090 02000323"
        ,"0091 02000C06"
        ,"0099 02000D0E"
        ,"0100 02000D0F"
        ,"0105 02000D0C"
        ,"0156 02000D0A"
        ,"0097 02000D11"
        ,"0098 03020291"

        ,"0067 03010400"
        ,"0068 01000401"
        ,"0069 03000221"
        ,"0292 03000220"

        ,"0079 02000705"
        ,"0080 02000709"
        ,"0081 02000711"
        ,"0362 02000702"
        ,"0364 02000713"

        ,"0052 02000C10"
        ,"0053 02000C12"
        ,"0054 02000C13"

        ,"0056 02000320"
        ,"0058 02000C05"
        ,"0059 02000C03"
        ,"0252 02000323"
        ,"0060 02000C06"
        ,"0061 02000C04"
        ,"0062 02000C01"

        ,"0078 02000900"
        ,"0162 02000901"
        ,"0163 02000902"
        ,"0164 02000903"
        ,"0082 02000707"
        ,"0083 0200070B"

        ,"0076 02000B00"
        ,"0077 02000B00"
        ,"0074 02000B01"
        ,"0075 02000B03"

        ,"0173 02000321"
        ,"0184 02000D10"
        ,"0183 03080002"
        ,"0106 03080002"
        ,"0186 03020291"
        ,"0227 02000D08"
        ,"0199 03020290"
        ,"0221 02000830"

        ,"0194 02000D02"
        ,"0195 02000DA1"
        ,"0196 02000DA2"
        ,"0197 02000DA3"
        ,"0198 02000DA4"
        ]


---------------------------------------------------------------------------------------------------
---- String operations ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Join a list of strings into a single text with a separator: "one, two, three"
joinWith :: [a] -> [[a]] -> [a]
joinWith x  =  concat . intersperse x

-- |Split a string into two substrings separated by the given character
split2 :: (Eq a) => a -> [a] -> ([a],[a])
split2 c s  =  (chunk, drop 1 rest)
  where (chunk, rest) = break (==c) s

contains a b = elem b a

-- |Drop n elements at the end of a list
dropEnd n  =  reverse . drop n . reverse

-- |True if `s` contains at least one of the elements of the set `set`
s `contains_one_of` set  =  any (`elem` set) s

-- |The last n elements
n `lastElems` xs  =  drop (length xs - n) xs

-- |Replace the n'th element (counting from 0) in the list `xs` with `x`
replaceAt n x xs  =  hd ++ x : drop 1 tl
    where (hd,tl) = splitAt n xs

-- |Change the n'th element (counting from 0) in the list `xs` from `x` to `f x`
updateAt n f xs  =  hd ++ f x : tl
    where (hd,x:tl) = splitAt n xs

-- |Replace every occurrence of the element 'from' in the list with 'to'
replace from to  =  map (\x -> if x==from  then to  else x)

-- |If the first string is a prefix of the second - return the remainder of the second string, otherwise Nothing
startFrom (x:xs) (y:ys) | x==y  =  startFrom xs ys
startFrom [] str                =  Just str
startFrom _  _                  =  Nothing

-- |Check that a string begins or ends with the given characters
beginWith s = isJust . startFrom s
endWith   s = beginWith (reverse s) . reverse

-- |Try to remove the string substr from the beginning of str
tryToSkip substr str  =  (startFrom substr str) `defaultVal` str

-- |Try to remove the string substr from the end of str
tryToSkipAtEnd substr str = reverse (tryToSkip (reverse substr) (reverse str))

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the second list is contained, wholy and intact,
-- anywhere within the first.
substr haystack needle  =  any (needle `isPrefixOf`) (tails haystack)

-- |List of positions of a substring within a string
strPositions haystack needle  =  elemIndices True$ map (needle `isPrefixOf`) (tails haystack)

-- |Replace every occurrence of `from` with `to` in the string `s`
replaceAll from to = repl
  where repl s      | Just remainder <- startFrom from s  =  to ++ repl remainder
        repl (c:cs)                                       =  c : repl cs
        repl []                                           =  []

-- |Replace %1 with the given string
format msg s  =  replaceAll "%1" s msg

-- |Replace %1..%9 with the given strings
formatn msg s  =  go msg
  where go ('%':d:rest) | isDigit d = (s !! (digitToInt d-1)) ++ go rest
        go (x:rest)                 = x : go rest
        go ""                       = ""

-- |Replace the prefix `from` with `to` in the string `s`
replaceAtStart from to s =
  case startFrom from s of
    Just remainder  -> to ++ remainder
    Nothing         -> s

-- |Replace the suffix `from` with `to` in the string `s`
replaceAtEnd from to s =
  case startFrom (reverse from) (reverse s) of
    Just remainder  -> reverse remainder ++ to
    Nothing         -> s

-- |Left/right-justify a string, padding it to the given width with spaces or something else
right_fill  c n s  =  s ++ replicate (n-length s) c
left_fill   c n s  =  replicate (n-length s) c ++ s
left_justify       =  right_fill ' '
right_justify      =  left_fill  ' '

-- Remove spaces at the start/end of a string or on both sides
trimLeft  = dropWhile (==' ')
trimRight = reverse.trimLeft.reverse
trim      = trimLeft.trimRight

-- |Convert a string to lower case
strLower = map toLower

-- |Compare two strings ignoring case
strLowerEq a b  =  strLower a == strLower b

-- |Map various parts of list
mapHead f []      =  []
mapHead f (x:xs)  =  f x : xs

mapTail f []      =  []
mapTail f (x:xs)  =  x : map f xs

mapInit f []      =  []
mapInit f xs      =  map f (init xs) : last xs

mapLast f []      =  []
mapLast f xs      =  init xs ++ [f (last xs)]

{-# NOINLINE replaceAll #-}
{-# NOINLINE replaceAtEnd #-}



-- |Replace Nothing with a default value
defaultVal = flip fromMaybe

infixl 9  .$
a.$b         =  b a                -- variant of $ with the argument order reversed

