{-# LANGUAGE CPP, FunctionalDependencies #-}
---------------------------------------------------------------------------------------------------
---- Helper functions: working with strings, lists, regular expressions,                       ----
----   the memory allocator, simplifying manipulation of IORef variables,                      ----
----   definition of convenient operations and control structures for the program.             ----
---------------------------------------------------------------------------------------------------
module Utils (module Utils, module CompressionLib) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Either
import Data.IORef
import Data.List hiding (sortOn)
import Data.Maybe
import Data.Word
import Debug.Trace
import Foreign.Marshal.Utils
import Foreign.Ptr

import CompressionLib (MemSize,b,kb,mb,gb,tb)

---------------------------------------------------------------------------------------------------
---- Check the define's ---------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

#if !defined(FREEARC_WIN) && !defined(FREEARC_UNIX)
#error "You must define OS!"
#endif

#if !defined(FREEARC_INTEL_BYTE_ORDER) && !defined(FREEARC_MOTOROLA_BYTE_ORDER)
#error "You must define byte order!"
#endif


---------------------------------------------------------------------------------------------------
---- Miscellaneous :) -----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Assemble a 4-byte number out of individual bytes
#if defined(FREEARC_INTEL_BYTE_ORDER)
make4byte b0 b1 b2 b3 = b0+256*(b1+256*(b2+256*b3)) :: Word32
#else
make4byte b0 b1 b2 b3 = b3+256*(b2+256*(b1+256*b0)) :: Word32
#endif

-- |Parser for the numbers given in all sorts of options. The meaning of a number is determined by the characters
-- written after it (b/k/f/...); if there are none, `default_specifier` is used.
-- The result is returned as a pair whose second element is `b` if the result is expressed in bytes,
-- or the other character written after the number, or the one passed as `default_specifier`.
parseNumber num default_specifier =
  case (span isDigit$ strLower$ num++[default_specifier]) of
    (digits, 'b':_)  ->  (readI digits     , 'b')
    (digits, 'k':_)  ->  (readI digits * kb, 'b')
    (digits, 'm':_)  ->  (readI digits * mb, 'b')
    (digits, 'g':_)  ->  (readI digits * gb, 'b')
    (digits, 't':_)  ->  (readI digits * tb, 'b')
    (digits, '^':_)  ->  (2 ^ readI digits , 'b')
    (digits,  c :_)  ->  (readI digits     ,  c )

-- |Decode a size specification, bytes by default
parseSize memstr =
    case (parseNumber memstr 'b') of
        (bytes, 'b')  ->  bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |Decode a memory amount specification: "512b", "32k", "8m" and so on. "24" means 24mb
parseMem memstr =
    case (parseNumber memstr 'm') of
        (bytes, 'b')  ->  clipToMaxMemSize bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |Same as parseMem, but with added support for the 75%/75p notation (of the total memory)
parseMemWithPercents memory memstr =
    case (parseNumber memstr 'm') of
        (bytes,    'b')  ->  clipToMaxMemSize$ bytes
        (percents, c) | c `elem` "%p"
                         ->  clipToMaxMemSize$ (memory * percents) `div` 100
        _                ->  error$ memstr++" - unrecognized size specifier"

-- Should it clamp to the largest number allowed in MemSize, or may it simply raise an error?
clipToMaxMemSize x | x < i(maxBound::MemSize) = i x
                   | otherwise                = i(maxBound::MemSize)

readI :: Num a => String -> a
readI = foldl f 0
  where f m c | isDigit c  =  fromIntegral (ord c - ord '0') + (m * 10)
              | otherwise  =  error ("Non-digit "++[c]++" in readI")

readInt :: String -> Int
readInt = readI

readSignedInt ('-':xs) = - readInt xs
readSignedInt      xs  =   readInt xs

isSignedInt :: String -> Bool
isSignedInt = all isDigit . tryToSkip "-"

lb :: Integral a =>  a -> Int
lb 0 = 0
lb 1 = 0
lb n = 1 + lb (n `div` 2)


{-# NOINLINE parseNumber          #-}
{-# NOINLINE parseSize            #-}
{-# NOINLINE parseMem             #-}
{-# NOINLINE parseMemWithPercents #-}
{-# NOINLINE readI                #-}
{-# NOINLINE readInt              #-}


-- |A few operations that make programs nicer to write
infixl 9  .$
infixl 1  >>==, ==<<, =<<., .>>=, .>>, .>>==, ==<<.
(.$)    :: a -> (a -> b) -> b
a.$b         =  b a                -- variant of $ with the argument order reversed
(>>==)  :: Monad m => m a -> (a -> b) -> m b
a>>==b       =  a >>= return . b     -- variant of >>= whose second argument needs lifting
(==<<)  :: Monad m => (a -> b) -> m a -> m b
a==<<b       =  return . a =<< b     -- variant of =<< whose first argument needs lifting
(=<<.)  :: Monad m => (a -> m b) -> (c -> m a) -> c -> m b
(a=<<.b) c   =  a =<< b c          -- variant of =<< for use in mapM and similar places
(.>>=)  :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(a.>>=b) c   =  a c >>= b          -- variant of >>= for use in mapM and similar places
(.>>)   :: Monad m => (a -> m b) -> m c -> a -> m c
(a.>>b)  c   =  a c >> b           -- variant of >> for use in mapM and similar places
(==<<.) :: Monad m => (a -> b) -> (c -> m a) -> c -> m b
(a==<<.b) c  =  return . a =<< b c   -- variant of ==<< for use in mapM and similar places
(.>>==) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(a.>>==b) c  =  a c >>= return . b   -- variant of >>== for use in mapM and similar places

-- Data types that have a default value
class    Defaults a      where defaultValue :: a
instance Defaults ()     where defaultValue = ()
instance Defaults Bool   where defaultValue = False
instance Defaults [a]    where defaultValue = []
instance Defaults (a->a)               where defaultValue = id
instance Defaults (Maybe a)            where defaultValue = Nothing
instance Defaults (a->IO a)            where defaultValue = return
instance {-# OVERLAPPING #-} Defaults a => Defaults (IO a) where defaultValue = return defaultValue
instance Defaults Int                  where defaultValue = 0
instance Defaults Integer              where defaultValue = 0
instance Defaults Double               where defaultValue = 0

class    TestDefaultValue a       where isDefaultValue :: a -> Bool
instance TestDefaultValue Bool    where isDefaultValue = not
instance TestDefaultValue [a]     where isDefaultValue = null
instance TestDefaultValue Int     where isDefaultValue = (==0)
instance TestDefaultValue Integer where isDefaultValue = (==0)
instance TestDefaultValue Double  where isDefaultValue = (==0)

infixr 3  &&&
infixr 2  |||

-- |Give a value its default value
a ||| b | isDefaultValue a = b
        | otherwise        = a

-- |Return the second value if the first one is not the default value
a &&& b | isDefaultValue a = defaultValue
        | otherwise        = b

-- |Apply the function f to the list only if it is not empty
unlessNull f xs  =  xs &&& f xs

-- |Monadic variant of concatMap
concatMapM :: Monad io => (a -> io [b]) -> [a] -> io [b]
concatMapM f x  =  mapM f x  >>==  concat

-- |Conditional execution
whenM cond action = do
  allow <- cond
  when allow
    action

unlessM = whenM . liftM not

-- |Perform `action` on the value returned by `x`, if it is not Nothing
whenJustM  x action  =  x >>= (`whenJust` action)

whenJustM_ x action  =  x >>= (`whenJust_` action)

whenJust   x action  =  x .$ maybe (return Nothing) (action .>>== Just)

whenJust_  x action  =  x .$ maybe (return ()) (action .>> return ())

-- |Perform `action` on the value returned by `x`, if it is "Right _"
whenRightM_ x action  =  x >>= either doNothing (action .>> return ())

-- |Perform onLeft/onRight on the value returned by `x`
eitherM_ x onLeft onRight  =  x >>= either (onLeft  .>> return ())
                                           (onRight .>> return ())

-- |Execute for every element of the list and return the results as a list
foreach = flip mapM

-- |Execute for every element of the list
for = flip mapM_

-- |Conditional execution with the condition at the end of the line
infixr 0 `on`
on = flip (&&&)

-- |A convenient way to write up front what must absolutely be done at the end :)
doFinally = flip finally

-- |Run onError if acquire fails, and action otherwise
handleErrors onError acquire action =
  (acquire >>= action) `catch` (\(_::SomeException) -> onError)

-- |Write at the beginning what has to be executed at the end
atExit a b = (b>>a)

-- |Perform the action only once, when var=True
once var action = do whenM (val var) action; var =: False
init_once       = ref True

-- |Stubs to put in place of commands that should not do anything
doNothing0       = return ()
doNothing  a     = return ()
doNothing2 a b   = return ()
doNothing3 a b c = return ()

-- |Ignore exceptions
ignoreErrors  =  handle (\(_::SomeException) -> return ())

-- |Create a new Channel and write an initial list of values into it
newChanWith xs = do c <- newChan
                    writeList2Chan c xs
                    return c

-- |Constant functions
const2 x _ _ = x
const3 x _ _ _ = x
const4 x _ _ _ _ = x

-- |What on earth do you need that ThreadId for??
forkIO_ action = forkIO action >> return ()

-- |Repeat forever
foreverM action = do
  action
  foreverM action

-- |A control structure analogous to the 'while' loop in ordinary languages
repeat_while inp cond out = do
  x <- inp
  if (cond x)
    then do out x
            repeat_while inp cond out
    else return x

-- |A control structure analogous to repeat-until in Pascal
repeat_until action = do
  done <- action
  when (not done) $ do
    repeat_until action

-- |A control structure that splits the execution of an operation of size size
-- into separate operations of at most chunk each
doChunks size chunk action =
  case size of
    0 -> return ()
    _ -> do let n = minI size chunk
            action (fromIntegral n)
            doChunks (size-n) chunk action

-- |Perform `action` on x, then on every element of the list returned by `action`, and so on recursively
recursiveM action x  =  action x >>= mapM_ (recursiveM action)

-- |Execute recursively while the condition `cond` holds, and once otherwise
recursiveIfM cond action x  =  if cond  then recursiveM action x  else (action x >> return ())

-- |Performs the action `action` on the elements of the list `list` one by one, returning
-- the list of results returned by `action` - broadly speaking, much like mapM.
-- But on top of that it checks the processed data against the criterion `crit_f` and leaves the loop
-- once that criterion is satisfied. Therefore it additionally returns the list of unprocessed
-- values from `list`
mapMConditional (init,map_f,sum_f,crit_f) action list = do
  let go []     ys summary = return (reverse ys, [])     -- finished because the list ran out
  let go (x:xs) ys summary = do
        y <- action x
        let summary  =  sum_f summary (map_f y)
        if (crit_f summary)
          then return (reverse$ y:ys, xs)                -- finished according to the criterion
          else go xs (y:ys) summary
  go list [] init

-- |Execute action with background computation
withThread thread  =  bracket (forkIO thread) killThread . const

-- |Perform an action in another thread and return the final result
bg action = do
  resultVar <- newEmptyMVar
  forkIO (action >>= putMVar resultVar)
  takeMVar resultVar

-- |Run an action for each element concurrently (using forkIO + MVar),
-- collecting all results.
-- All actions are launched in parallel; the caller blocks until all complete.
-- Exceptions in any thread are captured and re-thrown in the calling thread.
forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently xs f = do
  mvars <- mapM (\x -> do { mvar <- newEmptyMVar
                           ; forkIO (handle (\e -> putMVar mvar (Left (e::SomeException)))
                                            (f x >>= putMVar mvar . Right))
                           ; return mvar }) xs
  mapM (takeMVar >=> either throwIO return) mvars

-- |Run an action for each element concurrently, discarding results.
-- Exceptions in any thread are captured and re-thrown in the calling thread.
forConcurrently_ :: [a] -> (a -> IO ()) -> IO ()
forConcurrently_ xs f = do
  mvars <- mapM (\x -> do { mvar <- newEmptyMVar
                           ; forkIO (handle (\e -> putMVar mvar (Left (e::SomeException)))
                                            (f x >> putMVar mvar (Right ())))
                           ; return mvar }) xs
  mapM_ (takeMVar >=> either (throwIO :: SomeException -> IO ()) return) mvars


{-# NOINLINE foreverM #-}
{-# NOINLINE repeat_while #-}
{-# NOINLINE repeat_until #-}
{-# NOINLINE mapMConditional #-}
{-# NOINLINE bg #-}
{-# NOINLINE forConcurrently #-}
{-# NOINLINE forConcurrently_ #-}


-- |Filter a list using a monadic (executable) predicate
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM p  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  p x  >>=  bool (go    accum  xs)
                                           (go (x:accum) xs)

-- |mapMaybe lifted into the Monad class
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  f x  >>=  maybe (      go    accum  xs)
                                            (\r -> go (r:accum) xs)

-- |@firstJust@ takes a list of @Maybes@ and returns the
-- first @Just@ if there is one, or @Nothing@ otherwise.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : ms) = Just x
firstJust (Nothing : ms) = firstJust ms

-- |Return the first successful (Just) result of applying f to the list, or Nothing
firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f  =  firstJust . map f

-- |Replace Nothing with a default value
defaultVal = flip fromMaybe

-- |Replace Nothing with a default value - for an imperative operation
defaultValM = liftM2 defaultVal

-- |Choose one of two values depending on the last argument
bool onFalse onTrue False  =  onFalse
bool onFalse onTrue True   =  onTrue

-- |if without the syntactic overhead
iif True  onTrue onFalse  =  onTrue
iif False onTrue onFalse  =  onFalse

-- Apply one of two functions to a list depending on whether it is empty
list onNotNull onNull [] = onNull
list onNotNull onNull xs = onNotNull xs

-- |Return True if the value is not Nothing
maybe2bool (Just _) = True
maybe2bool Nothing  = False

-- |Test for Left
isLeft (Left _) = True
isLeft _        = False

-- |Remove the elements satisfying the given predicate
deleteIf p = filter (not . p)

-- |Remove the elements matching any of the predicates in the list
deleteIfs = deleteIf . anyf

-- |Updating a lookup list
update list a@(key,value)  =  a : [x | x@(k,v)<-list, k/=key]

-- |Replacing values according to a list
changeTo list value  =  lookup value list `defaultVal` value

-- |Print and return a single value
trace2 s = trace (show s) s

-- |Evaluate list elements
evalList (x:xs) = x `seq` evalList xs
evalList []     = ()

{-
-- Cale Gibbard

A useful little higher order function. Some examples of use:

swing map :: forall a b. [a -> b] -> a -> [b]
swing any :: forall a. [a -> Bool] -> a -> Bool
swing foldr :: forall a b. b -> a -> [a -> b -> b] -> b
swing zipWith :: forall a b c. [a -> b -> c] -> a -> [b] -> [c]
swing find :: forall a. [a -> Bool] -> a -> Maybe (a -> Bool) -- applies each of the predicates to the given value, returning the first predicate which succeeds, if any
swing partition :: forall a. [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])

-}

swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing f = flip (f . flip ($))


-- |Map on functions instead of its' arguments!
map_functions []     x  =  []
map_functions (f:fs) x  =  f x : map_functions fs x

-- |Check that every function in the list yields True on the (omitted here) argument. More efficient than swing all
allf x = all_functions x
all_functions []  = const True
all_functions [f] = f
all_functions fs  = and . map_functions fs

-- |Check that at least one function in the list yields True on the (omitted here) argument. More efficient than swing any
anyf x = any_function x
any_function []  = const False
any_function [f] = f
any_function fs  = or . map_functions fs

-- |Apply all the functions from the list to the argument one after another
applyAll []     x = x
applyAll (f:fs) x = applyAll fs (f x)

(f>>>g) x = g(f x)


---------------------------------------------------------------------------------------------------
---- String operations ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Split a string into two substrings separated by the given character
split2 :: (Eq a) => a -> [a] -> ([a],[a])
split2 c s  =  (chunk, drop 1 rest)
  where (chunk, rest) = break (==c) s

-- |Join them back the way they were :)
join2 :: [a] -> ([a],[a]) -> [a]
join2 between (a,b) = a++between++b

-- |Split a string into substrings separated by the given character
split :: (Eq a) => a -> [a] -> [[a]]
split c s =
  let (chunk, rest) = break (==c) s
  in case rest of  []     -> [chunk]
                   _:rest -> chunk : split c rest

-- |Join a list of strings into a single text with a separator: "one, two, three"
joinWith :: [a] -> [[a]] -> [a]
joinWith x  =  concat . intersperse x

-- |Join a list of strings into a single text using two different separators:
-- joinWith2 ", " " and " ["one","two","three","four"]  -->  "one, two, three and four"
joinWith2 :: [a] -> [a] -> [[a]] -> [a]
joinWith2 a b []    =  []
joinWith2 a b [x]   =  x
joinWith2 a b list  =  joinWith a (init list) ++ b ++ last list

-- |Put x between s1 and s2 if both strings are non-empty
between s1 x [] = s1
between [] x s2 = s2
between s1 x s2 = s1++x++s2

-- |Add double quotes around a string
quote :: String -> String
quote str  =  "\"" ++ str ++ "\""

-- |Remove the double quotes around a string (if there are any)
unquote :: String -> String
unquote ('"':str) | str>"" && x=='"'  =  xs     where (x:xs) = reverse str
unquote str = str

contains = flip elem

-- |Remove n elements from the end of the list
dropEnd n  =  reverse . drop n . reverse

-- |True if `s` contains at least one of the elements of the set `set`
s `contains_one_of` set  =  any (`elem` set) s

-- |The last n elements
n `lastElems` xs  =  drop (length xs - n) xs

-- |Replace the n-th element (counting from 0) of the list `xs` with `x`
replaceAt n x xs  =  hd ++ x : drop 1 tl
    where (hd,tl) = splitAt n xs

-- |Change the n-th element (counting from 0) of the list `xs` from `x` to `f x`
updateAt n f xs  =  hd ++ f x : tl
    where (hd,x:tl) = splitAt n xs

-- |Replace every occurrence of the element 'from' in the list with 'to'
replace from to  =  map (\x -> if x==from  then to  else x)

-- |If the first string is a prefix of the second - return the rest of the second string, otherwise Nothing
startFrom (x:xs) (y:ys) | x==y  =  startFrom xs ys
startFrom [] str                =  Just str
startFrom _  _                  =  Nothing

-- |Check that the string starts or ends with the given characters
beginWith s = isJust . startFrom s
endWith   s = beginWith (reverse s) . reverse

-- |Try to strip the string substr from the start of str
tryToSkip substr str  =  (startFrom substr str) `defaultVal` str

-- |Try to strip the string substr from the end of str
tryToSkipAtEnd substr str = reverse (tryToSkip (reverse substr) (reverse str))

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the second list is contained, wholy and intact,
-- anywhere within the first.
substr haystack needle  =  any (needle `isPrefixOf`) (tails haystack)

-- |List of the positions of a substring inside a string
strPositions haystack needle  =  elemIndices True$ map (needle `isPrefixOf`) (tails haystack)

-- |Replace every occurrence of `from` in the string `s` with `to`
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

-- |Replace the prefix `from` in the string `s` with `to`
replaceAtStart from to s =
  case startFrom from s of
    Just remainder  -> to ++ remainder
    Nothing         -> s

-- |Replace the suffix `from` in the string `s` with `to`
replaceAtEnd from to s =
  case startFrom (reverse from) (reverse s) of
    Just remainder  -> reverse remainder ++ to
    Nothing         -> s

-- |Encode the characters that are not allowed in a URL
urlEncode = concatMap (\c -> if isReservedChar(ord c) then '%':encode16 [c] else [c])
  where
        isReservedChar x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%', chr 34]

-- |Return the hexadecimal representation of a string of characters with codes <=255
encode16 (c:cs) | n<256 = [intToDigit(n `div` 16), intToDigit(n `mod` 16)] ++ encode16 cs
                             where n = ord c
encode16 "" = ""

-- |Decode the hexadecimal representation of a string of characters with codes <=255
decode16 (c1:c2:cs) = chr(digitToInt c1 * 16 + digitToInt c2) : decode16 cs
decode16 ""         = ""

-- |Take the first n elements of the list and append more to them to indicate that something was omitted
takeSome n more s | (y>[])    = x ++ more
                  | otherwise = x
                  where  (x,y) = splitAt n s

-- |Align a string to the left/right, padding it to the given width with spaces or something else
right_fill  c n s  =  s ++ replicate (n-length s) c
left_fill   c n s  =  replicate (n-length s) c ++ s
left_justify       =  right_fill ' '
right_justify      =  left_fill  ' '

-- Remove the spaces at the start/end of a string or on both sides
trimLeft  = dropWhile (==' ')
trimRight = reverse . trimLeft . reverse
trim      = trimLeft . trimRight

-- |Convert a string to lower case
strLower = map toLower

-- |Compare two strings ignoring case
strLowerEq a b  =  strLower a == strLower b

-- |break starting from the second element
break1 f (x:xs)  =  mapFst (x:) (break f xs)

-- |Return a default value instead of the head of the list when it is empty
head1 [] = defaultValue
head1 xs = head xs

-- Analogue of tail that copes calmly with empty lists
tail1 [] = []
tail1 xs = tail xs

-- Analogue of init that copes calmly with empty lists
init1 [] = []
init1 xs = init xs

-- Analogue of last that copes calmly with empty lists
last1 [] = defaultValue
last1 xs = last xs

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



---------------------------------------------------------------------------------------------------
---- List operations ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Sort list by function result (use Schwarznegian transform)
sortOn  f  =  map snd . sortOn' fst . map (keyval f)

-- |Sort list by function result (don't use Schwarznegian transform!)
sortOn' f  =  sortBy (map2cmp f)

-- |Group list by function result
groupOn f  =  groupBy (map2eq f)

-- |Sort and Group list by function result
sort_and_groupOn  f  =  groupOn f . sortOn  f
sort_and_groupOn' f  =  groupOn f . sortOn' f

-- |Group together all the elements (a.b) having the same 'a' value
groupFst :: (Ord a) =>  [(a,b)] -> [(a,[b])]
groupFst = map (\xs -> (fst (head xs), map snd xs)) . sort_and_groupOn fst

-- |Removes duplicates from a list
removeDups = removeDupsOn id

-- |Leaves only one element from each group having the same value of f
removeDupsOn f = map head . sort_and_groupOn f

-- |Check that all consecutive values in the list satisfy the given relation
isAll f []       = True
isAll f [x]      = True
isAll f (x:y:ys) = f x y  &&  isAll f (y:ys)

-- |Check that at least some two consecutive values in the list satisfy the given relation
isAny f []       = False
isAny f [x]      = False
isAny f (x:y:ys) = f x y  ||  isAny f (y:ys)

-- |Check that list is sorted by given field/critery
isSortedOn f  =  isAll (<=) . map f

-- |Check that all elements in list are equal by given field/critery
isEqOn f      =  isAll (==) . map f

-- |Find maximum element by given comparison critery
maxOn f (x:xs) = go x xs
  where go x [] = x
        go x (y:ys) | f x > f y  =  go x ys
                    | otherwise  =  go y ys

-- |Merge two lists, sorted by `cmp`, in one sorted list
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp xs [] = xs
merge cmp [] ys = ys
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

-- |Split the list into `numGroups` sublists according to the value returned by `crit_f`
partitionList numGroups crit_f list =
  elems $ accumArray (flip (:)) [] (0, numGroups-1) (map (keyval crit_f) (reverse list))

-- partitionList numGroups crit_f list =
--   let xs = map (keyval crit_f) list
--       go 0 [] all = all
--       go n list prev = let (this, next) = partition (\(a,b) -> a==n-1) list
--                        in go (n-1) next (map snd this:prev)
--   in go numGroups xs []

-- |Split the list into groups according to the predicates from the list `groups`:
--   splitList [(=='a'), (=='c')] 2 "cassa"  ->  ["aa","c","ss"]
--
splitList groups default_group filelist =
  let go [] filelist sorted  =  replaceAt default_group filelist (reverse sorted)
      go (group:groups) filelist sorted =
        let (found, notfound)  =  partition group filelist
        in go groups notfound (found:sorted)
  in go groups filelist []

-- |Find the index of the first predicate in the list `groups` that the value `value` satisfies
findGroup groups default_group value  =  (findIndex ($ value) groups) `defaultVal` default_group

-- Utility functions for list operations
keyval  f x    =  (f x, x)                -- |Return pair containing computed key and original value
map2cmp f x y  =  (f x) `compare` (f y)   -- |Converts "key_func" to "compare_func"
map2eq  f x y  =  (f x) == (f y)          -- |Converts "key_func" to "eq_func"


-- |Recursive processing of a list
recursive :: ([a]->(b,[a])) -> [a] -> [b]
recursive f list  =  list &&& (x:recursive f xs)   where (x,xs) = f list

-- |Split the list into sublists whose lengths are determined by calling the function `len_f` on the rest of the list
splitByLen :: ([a]->Int) -> [a] -> [[a]]
splitByLen len_f  =  recursive (\xs -> splitAt (len_f xs) xs)

-- |This function receives a list of sublist lengths and splits `xs` according to it
splitByLens (len:lens) list  =  (x:splitByLens lens xs)    where (x,xs) = splitAt len list
splitByLens []         []    =  []

-- |Returns the length of the initial segment of the list that satisfies the combined condition,
-- for example "groupLen (fiSize) (+) (<16*mb) files" returns the length of the initial segment of the list
-- that holds files with a total size of no more than 16 megabytes
groupLen mapper combinator tester  =  length . takeWhile tester . scanl1 combinator . map mapper

-- |Combine the results of span and break: spanBreak isDigit "100a10b2c" = ("100a", "10b2c")
spanBreak crit xs  = let (s1,tail1) = span  crit xs
                         (s2,tail2) = break crit tail1
                     in (s1++s2, tail2)

-- |Split the list into groups whose headers are the elements satisfying the criterion 'crit'
makeGroups              :: (a -> Bool) -> [a] -> [[a]]
makeGroups crit []      =  []
makeGroups crit (x:xs)  =  (x:ys) : makeGroups crit zs
                             where (ys,zs) = break crit xs

-- |Split the list into groups separated by the elements satisfying the criterion 'crit':
-- splitOn even [1,2,4,8,3,5,7] == [[1],[2],[4],[8],[3,5,7]]
splitOn crit []  =  []
splitOn crit xs  =  (not(null ys)  &&&  (ys :))
                    (not(null zs)  &&&  ([head zs] : splitOn crit (tail zs)))
                      where (ys,zs) = break crit xs

-- |Remove duplicates from the list by the given criterion. O(n^2), but it preserves the order of the elements
keepOnlyFirstOn f [] = []
keepOnlyFirstOn f (x:xs) = x : keepOnlyFirstOn f (filter (\a -> f x /= f a) xs)

-- |Keep only the last of the duplicates in the list by the given criterion
keepOnlyLastOn f = reverse . keepOnlyFirstOn f . reverse

-- |Remove the elements with the given indices from the list
deleteElems = go 0
  where go n xs [] = xs  -- Nothing left to delete
        go n (x:xs) iis@(i:is) | n<i  = x:go (n+1) xs iis  -- we have not reached the i-th element yet
                               | n==i =   go (n+1) xs is   -- reached it - delete!


{-# NOINLINE partitionList #-}
{-# NOINLINE splitList #-}


---------------------------------------------------------------------------------------------------
---- Array operations -----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Turn a list into a 0-based array
listArray0 list  =  listArray (0,length(list)-1) list

-- |Find the minimum and maximum indices in a list of pairs and build an array out of them,
populateArray defaultValue castValue pairs =
  accumArray (\a b -> castValue b) defaultValue (minimum indexes, maximum indexes) pairs
  where indexes = map fst pairs


---------------------------------------------------------------------------------------------------
---- Operations on tuples -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Operations on tuple/2
mapFst    f (a,b)  =  (f a,   b)
mapSnd    f (a,b)  =  (  a, f b)
mapFstSnd f (a,b)  =  (f a, f b)
map2      (f,g) a  =  (f a, g a)
mapFsts = map . mapFst
mapSnds = map . mapSnd
map2s   = map . map2

-- |Merge the second elements of the pairs in the list and keep a single (common) first element
concatSnds xs = (fst (head xs), concatMap snd xs)

-- Operations on tuple/3
fst3 (a,_,_)    =  a
snd3 (_,a,_)    =  a
thd3 (_,_,a)    =  a
map3 (f,g,h) a  =  (f a, g a, h a)


---------------------------------------------------------------------------------------------------
---- Emulation of ordinary variables --------------------------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 0 =:, +=, -=, ++=, =::, .=, .<-, <<=, <=>

-- Simple variables
class Variable v a | v->a where
  new  :: a -> IO v
  val  :: v -> IO a
  (=:) :: v -> a -> IO ()
  (.=) :: v -> (a->a) -> IO ()
  (=::) :: v -> IO a -> IO ()
  (.<-) :: v -> (a->IO a) -> IO ()
  -- Default implementations
  a.=f = do x<-val a; a=:f x
  a=::b = (a=:) =<< b
  a.<-f = do x<-val a>>=f; a=:x

ref = newIORef
instance Variable (IORef a) a where
  new = newIORef
  val = readIORef
  a=:b = writeIORef a b
  a.=b = modifyIORef a b
  a.<-b = modifyIORefIO a b

mvar = newMVar
instance Variable (MVar a) a where
  new = newMVar
  val = readMVar
  a=:b = swapMVar a b >> return ()
  a.=b = modifyMVar_ a (return . b)
  a.<-b = modifyMVar_ a b

a+=b = a.=(\a->a+b)
a-=b = a.=(\a->a-b)
a++=b = a.=(\a->a++b)
(<=>) :: Variable v x => v -> x -> IO x
a<=>b = do x <- val a; a =: b; return x
withRef init  =  with' (ref init) val


-- Accumulation lists
newtype AccList a = AccList [a]
newList   = ref$ AccList []
a<<=b     = a .= (\(AccList x) -> AccList$ b:x)
pushList  = (<<=)
listVal a = val a >>== (\(AccList x) -> reverse x)
withList  =  with' newList listVal


-- |Append a value to the list stored behind an IORef reference
addToIORef :: IORef [a] -> a -> IO ()
addToIORef var x  =  var .= (x:)

-- |Use the value stored behind an IORef reference in a procedure,
-- and store in its place the new value returned by that procedure
modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO var action = do
  readIORef var  >>=  action  >>=  writeIORef var

-- |One more useful control structure
with' init finish action  =  do a <- init;  action a;  finish a

-- |Perform an operation and return its result wrapped in init/finish operations
inside init finish action  =  do init;  x <- action;  finish; return x

-- |Perform "add key" with caching of the results
lookupMVarCache mvar add key = do
  modifyMVar mvar $ \assocs -> do
    case (lookup key assocs) of
      Just value -> return (assocs, value)
      Nothing    -> do value <- add key
                       return ((key,value):assocs, value)


-- JIT variables are initialized only at the moment of their first use
newJIT :: IO a -> IO (IORef (Either (IO a) a))
newJIT init        = ref (Left init)
delJIT :: IORef (Either (IO a) a) -> (a -> IO ()) -> IO ()
delJIT a    finish = whenRightM_ (readIORef a) finish
valJIT :: IORef (Either (IO a) a) -> IO a
valJIT a           = do x <- readIORef a
                        case x of
                          Left init -> do x<-init; writeIORef a (Right x); return x
                          Right x   -> return x

withJIT :: IO a -> (a -> IO ()) -> (IORef (Either (IO a) a) -> IO b) -> IO b
withJIT init finish action = do a <- newJIT init;  action a  `finally`  delJIT a finish


---------------------------------------------------------------------------------------------------
---- Reference arithmetic and operations on integers ----------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 6 +:, -:
ptr+:n   = ptr `plusPtr` (fromIntegral n)
ptr-:buf = fromIntegral  (ptr `minusPtr` buf)
copyBytesI dst src len  =  copyBytes dst src (fromIntegral len)
minI a b                =  i$ min (i a) (i b)
maxI a b                =  i$ max (i a) (i b)
clipToMaxInt            =  i. min (i (maxBound::Int))
atLeast                 =  max
i :: (Integral a, Num b) => a -> b
i                       =  fromIntegral
clipTo low high         =  min high . max low
divRoundUp   x chunk    = ((x-1) `div` i chunk) + 1
roundUp      x chunk    = divRoundUp x chunk * i chunk
divRoundDown x chunk    = x `div` i chunk
roundDown    x chunk    = divRoundDown x chunk * i chunk
roundTo      x chunk    = i (((((toInteger(x)*2) `divRoundDown` chunk)+1) `divRoundDown` 2) * i chunk) `asTypeOf` x


---------------------------------------------------------------------------------------------------
---- Memory allocation in a circular buffer -------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Memory allocation in a circular buffer with alignment of the allocated blocks
--   heapsize     - size of the buffer
--   aBUFFER_SIZE - maximum size of an allocated block
--   aALIGN       - every allocated block is aligned to a boundary that is a multiple of this number
--   returnBlock  - procedure that obtains a buffer released by the consumer. It is called
--                    when there is no longer enough memory to satisfy the next request
--
allocator heapsize aBUFFER_SIZE aALIGN returnBlock = do
  let aHEAP_START = 0          -- start of the buffer, must be = 0
      aHEAP_END   = heapsize   -- end of the buffer

  start <- ref aHEAP_START     -- pointer to the start of the free space in the buffer
  end   <- ref aHEAP_END       -- pointer to the end of the free space
                               -- if these pointers are equal, there is no free space
#if 0
  let debug = putStr         -- debug printing

  let printStats s = do      -- Print the state of the buffer when debugging
        astart <- val start
        aend <- val end
        debug$ left_justify 48 s++"STATE start:"++show astart++", end:"++show aend++", avail:"++show ((aend-astart) `mod` aHEAP_END)++"\n"

  debug "\n"
#else
  let debug      = return
      printStats = return
#endif

  -- Round a value up to the nearest multiple of aALIGN
  let align n  =  (((n-1) `div` aALIGN) + 1) * aALIGN

  -- Returns the address of a block: >=n, aligned to aALIGN and having at least aBUFFER_SIZE bytes left until the end of the buffer
  let nextAvail n = if (aHEAP_END-aligned<aBUFFER_SIZE)
                      then aHEAP_END
                      else aligned
                    where aligned = align n

  -- Return the amount of free memory in the buffer
  let available = do
        astart <- val start
        aend   <- val end
        if (astart<=aend) then
           return (aend-astart)
         else if (astart<aHEAP_END) then
           return (aHEAP_END-astart)
         else do
           -- Move the free-space start pointer back to the beginning of the buffer
           start =: aHEAP_START
           debug "===================================\n"
           printStats ""
           available

  -- Wait for a memory block to be released and mark it as released
  let waitReleasingMemory = do
        (addr,size) <- returnBlock
        astart <- val start
        aend   <- val end
        unless (addr == aend || (addr==aHEAP_START && (aHEAP_END-aend<aBUFFER_SIZE)))$  fail "addToAvail!"
        let new_end = nextAvail(addr+size)
        if new_end == astart
          then do start=:aHEAP_START; end=:aHEAP_END -- now all memory is free
          else end =: new_end
        printStats$ "*** returned buf:"++show addr++" size:"++show size++"   "

  -- Get the next block of size aBUFFER_SIZE. If there are no free blocks - wait for
  --   the required amount of previously allocated memory to be returned
  let getBlock = do
        avail <- available
        if (avail >= aBUFFER_SIZE) then do
           block <- val start
           start =: error "Block not shrinked"
           return block
         else do
           waitReleasingMemory
           getBlock

  -- Shrink the allocated block to the size `size`. Must always be called after getBlock
  let shrinkBlock block size = do
        astart <- val start
        --unless (astart == block)$      fail "Tryed to shrink another block"
        unless (size <= aBUFFER_SIZE)$  fail "Growing instead of shrinking :)"
        start =: nextAvail(block+size)
        printStats$ "getBlock buf:"++show block++", size: "++show aBUFFER_SIZE++" --> "++show size++"    "

  -- Return the interface for using the circular buffer
  return (getBlock, shrinkBlock)


-- |Circular allocator that uses the memory block `heap`.
-- Converts the functions that the `allocator` function works with
memoryAllocator heap size chunksize align returnBlock = do
  let returnBlock2            =  do (buf,len) <- returnBlock; return (buf-:heap, len)
  (getBlock2, shrinkBlock2)  <-  allocator size chunksize align returnBlock2
  let getBlock                =  do block <- getBlock2; return (heap+:block)
      shrinkBlock buf len     =  do shrinkBlock2 (buf-:heap) len
  return (getBlock, shrinkBlock)


---------------------------------------------------------------------------------------------------
---- Support for regular expressions.                                                          ----
---- todo: #define FULL_REGEXP enables the use of extended regular expressions: r[0-9][0-9]    ----
---------------------------------------------------------------------------------------------------

-- |Compiled representation of a regular expression                                 EXAMPLE
data RegExpr = RE_End                     -- end of the mask                        ""
             | RE_Anything                -- any string                             "*"
             | RE_AnyStr  RegExpr         -- '*' followed by further '*'s           '*':"bc*"
             | RE_FromEnd RegExpr         -- match RE against the end of the string '*':"bc"
             | RE_AnyChar RegExpr         -- any character, then RE                 '?':"bc"
             | RE_Char    Char RegExpr    -- the given character, then RE           'a':"bc"

-- |Check that the string contains one of the characters
-- that have a special meaning in regular expressions
is_wildcard s  =  s `contains_one_of` "?*"

-- |Compile the textual representation of a regular expression into a RegExpr structure
compile_RE s  =  case s of
  ""                         -> RE_End
  "*"                        -> RE_Anything
  '*':cs | cs `contains` '*' -> RE_AnyStr   (compile_RE  cs)
         | otherwise         -> RE_FromEnd  (compile_RE$ reverse s)
  '?':cs                     -> RE_AnyChar  (compile_RE  cs)
  c  :cs                     -> RE_Char   c (compile_RE  cs)

-- |Check whether a string matches a compiled regular expression
match_RE r = case r of
  RE_End        -> null
  RE_Anything   -> const True
  RE_AnyStr   r -> let re = match_RE r in \s -> any re (tails s)
  RE_FromEnd  r -> let re = match_RE r in re . reverse
  RE_AnyChar  r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     _:xs -> re xs
  RE_Char   c r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     x:xs -> x==c && re xs

-- |Check whether the string `s` matches the regular expression `re`
match re {-s-}  =  match_RE (compile_RE re) {-s-}

-- Perl-like names for matching routines
infix 4 ~=, !~
(~=)    = flip match
a !~ b  = not (a~=b)

-- |Convert a list to a 0-based array
toP :: [a] -> Array Int a
toP xs = listArray (0, length xs - 1) xs

-- |Array indexing as an operator (array on the left, index on the right)
infixl 9 !:
(!:) :: (Ix i) => Array i a -> i -> a
(!:) = (!)
