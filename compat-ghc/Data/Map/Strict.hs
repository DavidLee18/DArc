-- MicroHs compat shim for Data.Map.Strict.
-- Backed by sorted association lists; sufficient for DArc's usage.
module Data.Map.Strict
  ( Map
  , empty
  , singleton
  , insert
  , insertWith
  , delete
  , lookup
  , findWithDefault
  , member
  , notMember
  , fromList
  , toList
  , toAscList
  , keys
  , elems
  , size
  , null
  , unionWith
  , union
  , intersectionWith
  , difference
  , mapWithKey
  , foldlWithKey'
  , foldrWithKey
  , filterWithKey
  , alter
  , adjust
  ) where

import Prelude hiding (lookup, null)
import Data.List (sortBy, foldl')
import Data.Ord  (comparing)

-- | Ordered map implemented as a sorted association list.
newtype Map k v = Map [(k, v)] deriving (Show, Eq)

empty :: Map k v
empty = Map []

singleton :: k -> v -> Map k v
singleton k v = Map [(k, v)]

null :: Map k v -> Bool
null (Map []) = True
null _        = False

size :: Map k v -> Int
size (Map xs) = length xs

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map xs) = Map (go xs)
  where
    go [] = [(k, v)]
    go ((k', v') : rest)
      | k < k'    = (k, v) : (k', v') : rest
      | k == k'   = (k, v) : rest
      | otherwise = (k', v') : go rest

insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map xs) = Map (go xs)
  where
    go [] = [(k, v)]
    go ((k', v') : rest)
      | k < k'    = (k, v) : (k', v') : rest
      | k == k'   = (k, f v v') : rest
      | otherwise = (k', v') : go rest

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map xs) = Map (filter ((/= k) . fst) xs)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map xs) = go xs
  where
    go [] = Nothing
    go ((k', v') : rest)
      | k == k'   = Just v'
      | k < k'    = Nothing
      | otherwise = go rest

findWithDefault :: Ord k => v -> k -> Map k v -> v
findWithDefault def k m = maybe def id (lookup k m)

member :: Ord k => k -> Map k v -> Bool
member k m = case lookup k m of { Just _ -> True; Nothing -> False }

notMember :: Ord k => k -> Map k v -> Bool
notMember k m = not (member k m)

fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldl' (\m (k, v) -> insert k v m) empty

toList :: Map k v -> [(k, v)]
toList (Map xs) = xs

toAscList :: Map k v -> [(k, v)]
toAscList = toList

keys :: Map k v -> [k]
keys (Map xs) = map fst xs

elems :: Map k v -> [v]
elems (Map xs) = map snd xs

unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (Map xs) (Map ys) = Map (go xs ys)
  where
    go [] bs = bs
    go as [] = as
    go ((k, v) : as) ((k', v') : bs)
      | k < k'    = (k, v) : go as ((k', v') : bs)
      | k == k'   = (k, f v v') : go as bs
      | otherwise = (k', v') : go ((k, v) : as) bs

union :: Ord k => Map k v -> Map k v -> Map k v
union = unionWith const

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f (Map xs) (Map ys) = Map (go xs ys)
  where
    go [] _ = []
    go _ [] = []
    go ((k, v) : as) ((k', v') : bs)
      | k < k'    = go as ((k', v') : bs)
      | k == k'   = (k, f v v') : go as bs
      | otherwise = go ((k, v) : as) bs

difference :: Ord k => Map k a -> Map k b -> Map k a
difference (Map xs) (Map ys) = Map (go xs ys)
  where
    go [] _ = []
    go as [] = as
    go ((k, v) : as) ((k', w) : bs)
      | k < k'    = (k, v) : go as ((k', w) : bs)
      | k == k'   = go as bs
      | otherwise = go ((k, v) : as) bs

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map xs) = Map (map (\(k, v) -> (k, f k v)) xs)

foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f z (Map xs) = foldl' (\acc (k, v) -> f acc k v) z xs

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z (Map xs) = foldr (\(k, v) acc -> f k v acc) z xs

filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
filterWithKey p (Map xs) = Map (filter (\(k, v) -> p k v) xs)

alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m =
  case f (lookup k m) of
    Nothing -> delete k m
    Just v  -> insert k v m

adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v
adjust f k (Map xs) = Map (map (\(k', v) -> if k == k' then (k', f v) else (k', v)) xs)
