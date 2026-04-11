{-# LANGUAGE CPP #-}
-- |Compatibility shim for the removed Data.HashTable module.
-- Implements a mutable hash table using an IORef-backed association list,
-- preserving support for arbitrary equality predicates.
module HashTable
  ( HashTable
  , new
  , lookup
  , insert
  ) where

import Prelude hiding (lookup)
import Data.IORef
import Data.Int (Int32)

-- |Mutable hash table backed by an association list, storing the equality predicate.
data HashTable k v = HashTable (k -> k -> Bool) (IORef [(k, v)])

-- |Create a new empty hash table.
new :: (k -> k -> Bool) -> (k -> Int32) -> IO (HashTable k v)
new eq _hash = HashTable eq <$> newIORef []

-- |Look up a key in the hash table using the equality predicate supplied to 'new'.
lookup :: HashTable k v -> k -> IO (Maybe v)
lookup (HashTable eq ref) key = do
  xs <- readIORef ref
  return (go xs)
  where
    go []           = Nothing
    go ((k,v):rest) = if eq key k then Just v else go rest

-- |Insert a key-value pair into the hash table.
insert :: HashTable k v -> k -> v -> IO ()
insert (HashTable _eq ref) key val =
  modifyIORef ref ((key, val) :)
