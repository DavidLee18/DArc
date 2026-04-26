{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- (Де)шифрование данных.                                                                     ----
---- Интерфейс с написанными на Си процедурами, выполняющими всю реальную работу.               ----
----------------------------------------------------------------------------------------------------
module EncryptionLib where

import Control.Monad
import Data.Char (chr, ord)
import Data.Word (Word8)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (peekArray, withArrayLen)
import Foreign.Ptr
import System.IO.Unsafe

import CompressionLib

----------------------------------------------------------------------------------------------------
----- Encryption routines --------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Query encryption method for some parameter
encryptionGet = compressionGet

-- |Generate key based on password and salt using given number of hashing iterations
pbkdf2Hmac :: String -> String -> Int -> Int -> String
pbkdf2Hmac password salt iterations keySize = unsafePerformIO $
  -- Marshal as raw bytes (ord-low-8), not via locale encoding, since password/salt
  -- can contain binary bytes and the key is binary. peekCStringLen/withCStringLen
  -- would UTF-8-reinterpret them under GHC and corrupt the data.
  withArrayLen (map (fromIntegral . ord) password :: [Word8]) $ \pw_len pw_buf ->
    withArrayLen (map (fromIntegral . ord) salt :: [Word8]) $ \sa_len sa_buf ->
    allocaBytes keySize $ \c_key -> do
      c_Pbkdf2Hmac (castPtr pw_buf) (ii pw_len) (castPtr sa_buf) (ii sa_len) (ii iterations) c_key (ii keySize)
      ws <- peekArray keySize (castPtr c_key :: Ptr Word8)
      return (map (chr . fromIntegral) ws)


----------------------------------------------------------------------------------------------------
----- External encryption/PRNG routines ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Generate key based on password and salt using given number of hashing iterations
foreign import ccall unsafe  "Compression.h  Pbkdf2Hmac"
   c_Pbkdf2Hmac :: Ptr CChar -> CInt -> Ptr CChar -> CInt -> CInt -> Ptr CChar -> CInt -> IO ()

-- PRNG
-- Non-IO foreign value import; MHS needs the IO form + unsafePerformIO.
foreign import ccall unsafe  "Compression.h  fortuna_size"
   c_prng_size :: IO CInt
{-# NOINLINE prng_size #-}
prng_size :: CInt
prng_size = unsafePerformIO c_prng_size
foreign import ccall unsafe  "Compression.h  fortuna_start"
   prng_start :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_add_entropy"
   prng_add_entropy :: Ptr CChar -> CULong -> Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_ready"
   prng_ready :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_read"
   prng_read :: Ptr CChar -> CULong -> Ptr CChar -> IO CULong
