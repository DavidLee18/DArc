-- MicroHs compat shim for Foreign.Marshal.Pool.
-- Provides a minimal Pool API; DArc only imports this but doesn't use it.
module Foreign.Marshal.Pool
  ( Pool
  , newPool
  , freePool
  , withPool
  , pooledMalloc
  , pooledMallocBytes
  , pooledRealloc
  , pooledReallocBytes
  , pooledNew
  , pooledNewArray
  , pooledNewArray0
  ) where

import Foreign.Ptr        (Ptr, castPtr)
import Foreign.C.Types    (CChar)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Data.IORef

-- | A memory pool (minimal implementation using a list of pointers).
newtype Pool = Pool (IORef [Ptr ()])

newPool :: IO Pool
newPool = Pool <$> newIORef []

freePool :: Pool -> IO ()
freePool (Pool ref) = do
  ptrs <- readIORef ref
  mapM_ free ptrs
  writeIORef ref []

withPool :: (Pool -> IO a) -> IO a
withPool action = do
  pool <- newPool
  result <- action pool
  freePool pool
  return result

-- Returns Ptr CChar (the most common use in DArc) so MicroHs infers the right type.
pooledMallocBytes :: Pool -> Int -> IO (Ptr CChar)
pooledMallocBytes (Pool ref) n = do
  ptr <- mallocBytes n :: IO (Ptr ())
  modifyIORef ref (ptr :)
  return (castPtr ptr)

pooledMalloc :: Pool -> IO (Ptr CChar)
pooledMalloc pool = pooledMallocBytes pool 8  -- minimal size

pooledReallocBytes :: Pool -> Ptr a -> Int -> IO (Ptr CChar)
pooledReallocBytes pool _old n = pooledMallocBytes pool n

pooledRealloc :: Pool -> Ptr a -> IO (Ptr CChar)
pooledRealloc pool ptr = pooledMalloc pool

pooledNew :: Pool -> a -> IO (Ptr CChar)
pooledNew pool _ = pooledMalloc pool

pooledNewArray :: Pool -> [a] -> IO (Ptr CChar)
pooledNewArray pool xs = pooledMallocBytes pool (length xs * 8)

pooledNewArray0 :: Pool -> a -> [a] -> IO (Ptr CChar)
pooledNewArray0 pool _end xs = pooledMallocBytes pool ((length xs + 1) * 8)
