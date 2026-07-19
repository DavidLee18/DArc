{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------------------------------
---- Encryption, decryption and cryptographic PRNG.                                             ----
---- generateEncryption adds encryption algorithm(s) to the compression algorithm chain.        ----
---- generateDecryption adds keys to the compression+encryption algorithm record,               ----
---- generateRandomBytes returns a sequence of cryptographically random bytes                   ----
----------------------------------------------------------------------------------------------------
module Encryption (generateEncryption, generateDecryption, generateRandomBytes) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Data.Char
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable (peek)
import Data.Word (Word8)
import System.IO
import System.IO.Unsafe

import EncryptionLib
import Utils
import Errors
import Compression

---------------------------------------------------------------------------------------------------
---- Encryption and decryption --------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Returns two functions that add an encryption algorithm to the compression method:
-- the first one includes the encryption key (for real use)
-- the second one only the auxiliary data (for storing in the archive)
generateEncryption encryption password = do
    addRandomness
    result <- foreach (split_compressor encryption) $ \algorithm -> do
        let ivSz = encryptionGet "ivSize" algorithm
        initVector <- generateRandomBytes ivSz
        salt       <- generateRandomBytes (encryptionGet "keySize" algorithm)
        let numIterations = encryptionGet "numIterations" algorithm
            checkCodeSize = 2
            (key, checkCode) = deriveKey algorithm password salt numIterations checkCodeSize
        return (algorithm++":k"++encode16 key++":i"++encode16 initVector
               ,algorithm++":s"++encode16 salt++":c"++encode16 checkCode
                         ++":i"++encode16 initVector)
    return ((++map fst result), (++map snd result))


-- |Process a compressor read from the archive, adding to it the information
-- required for decryption
generateDecryption compressor decryption_info  =  mapM addKey compressor   where
  -- Process the encryption algorithm, adding to it the key derived
  -- from the user-supplied password and the salt stored in the algorithm parameters
  addKey algorithm | not (isEncryption algorithm)
                               = return (Just algorithm)    -- non-encryption algorithm stays untouched
                   | otherwise = do
    -- Extract from the algorithm record the parameters required to compute and verify the key
    let name:params   = split_method algorithm
        param c       = params .$map (splitAt 1) .$lookup c   -- find the value of parameter c
        salt          = param "s" `defaultVal` (error$ algorithm++" doesn't include salt") .$decode16
        checkCode     = param "c" `defaultVal` "" .$decode16
        numIterations = param "n" `defaultVal` (error$ algorithm++" doesn't include numIterations") .$readInt

    -- Use the list of decryption passwords and, if it comes to that,
    -- add to it the new passwords entered by the user
    let (dont_ask_passwords, mvar_passwords, keyfiles, ask_decryption_password, bad_decryption_password) = decryption_info
    modifyMVar mvar_passwords $ \passwords -> do
      passwords_list <- ref passwords

      -- Try decryption with all the possible keyfiles and then without them
      let checkPwd password  =  firstJust$ map doCheck (keyfiles++[""])
            where -- If verification against checkCode succeeds we return the found encryption algorithm key, otherwise - Nothing
                  doCheck keyfile  =  recheckCode==checkCode  &&&  Just key
                    -- Compute the decryption key key and the recheckCode used for a quick check that the password is correct
                    where (key, recheckCode) = deriveKey algorithm (password++keyfile) salt numIterations (length checkCode)

      -- Procedure that guesses the password for decrypting a block.
      -- Every likely password is checked with checkPwd.
      -- If none of the already known passwords fits, we ask the user for new ones
      let findDecryptionKey (password:pwds) = do
            case (checkPwd password) of            -- If verification in checkPwd succeeded
              Just key -> return (Just key)        --   then we return the found encryption algorithm key
              Nothing  -> findDecryptionKey pwds   --   otherwise - we try the next passwords

          findDecryptionKey [] = do   -- We get here if none of the old passwords fitted
            -- If the -p-/-op- option was used, or the user enters an empty string -
            -- it means we will not manage to decrypt this block
            if dont_ask_passwords  then return Nothing   else do
              password <- ask_decryption_password
              if password==""        then return Nothing   else do
                -- Add the new password to the list of passwords checked during extraction
                passwords_list .= (password:)
                case (checkPwd password) of               -- If verification in checkPwd succeeded
                  Just key -> return (Just key)           --   then we return the found encryption algorithm key
                  Nothing  -> do bad_decryption_password  --   otherwise - we ask for another password
                                 findDecryptionKey []
      --
      key <- findDecryptionKey passwords
      pl  <- val passwords_list
      return (pl, key.$fmap (\key -> algorithm++":k"++encode16 key))


-- |Derive the encryption key and the check code from password+salt
deriveKey algorithm password salt numIterations checkCodeSize =
    splitAt keySize $ pbkdf2Hmac password salt numIterations (keySize+checkCodeSize)
    where   keySize = encryptionGet "keySize" algorithm


-- |Check action result and abort on (internal) error
check test msg action = do
  res <- action
  unless (test res) (fail$ "Error in "++msg)

-- |OK return code for LibTomCrypt library
aCRYPT_OK :: CInt
aCRYPT_OK = 0


---------------------------------------------------------------------------------------------------
---- Cryptographic generator of random byte sequences ---------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Add random information obtained from the OS to the PRNG
addRandomness = withMVar prng_state addRandomnessTo
addRandomnessTo prng = do
  let size = 4096
  allocaBytes size $ \buf -> do
    bytes <- systemRandomData buf (i size)
    check (==aCRYPT_OK) "prng_add_entropy" $
      prng_add_entropy buf (i bytes) prng

-- |Generate a random byte sequence of the specified length
generateRandomBytes bytes = do
  withMVar prng_state $ \prng -> do
    allocaBytes bytes $ \buf -> do
      check (==i bytes) "prng_read" $
        prng_read buf (i bytes) prng
      -- Read as raw bytes; peekCAStringLen under GHC decodes via locale (UTF-8),
      -- which turns random bytes into multi-byte chars with ord >= 256.
      ws <- peekArray bytes (castPtr buf :: Ptr Word8)
      return (map (chr . fromIntegral) ws)

-- |The variable holding the PRNG state
{-# NOINLINE prng_state #-}
prng_state :: MVar (Ptr CChar)
prng_state = unsafePerformIO $ do
   prng <- mallocBytes (i prng_size)
   prng_start prng
   addRandomnessTo prng
   newMVar prng

-- |Fill buffer with system-generated pseudo-random data.
-- Reads from /dev/urandom on Unix; falls back to Windows CryptGenRandom on Windows.
systemRandomData :: Ptr CChar -> CInt -> IO CInt
#if defined(FREEARC_WIN)
systemRandomData buf size = c_systemRandomData buf size

foreign import ccall unsafe "Environment.h systemRandomData"
  c_systemRandomData :: Ptr CChar -> CInt -> IO CInt
#elif defined(__MHS__)
-- MicroHs: hGetBuf crashes (withHandleRd bug); use a direct C helper instead.
-- MHS truncates FFI return values to 32 bits, so use _w variant with pointer.
systemRandomData buf size = alloca $ \pOut -> do
  darc_urandom_read_w (castPtr buf) (fromIntegral size) pOut
  fmap fromIntegral (peek pOut)

foreign import ccall unsafe "Environment.h darc_urandom_read_w"
  darc_urandom_read_w :: Ptr () -> CLong -> Ptr CLong -> IO ()
#else
systemRandomData buf size = do
  withFile "/dev/urandom" ReadMode $ \h -> do
    n <- hGetBuf h buf (fromIntegral size)
    return (fromIntegral n)
#endif

