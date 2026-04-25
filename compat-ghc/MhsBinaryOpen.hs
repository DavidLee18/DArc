module MhsBinaryOpen(openBinaryFileBin) where
import qualified Prelude()
import Primitives(ForeignPtr, IO)
import Control.Monad
import Data.Function
import Data.Functor
import Data.Maybe
import Control.Error (error)
import Data.List ((++))
import Foreign.C.String
import Foreign.Ptr
import Mhs.Builtin
import System.IO.Internal

data FILE

foreign import ccall "fopen"    c_fopen    :: CString -> CString -> IO (Ptr FILE)
foreign import ccall "add_FILE" c_add_FILE :: Ptr FILE  -> IO (Ptr BFILE)

openBinaryFileBin :: FilePath -> IOMode -> IO Handle
openBinaryFileBin fn m = do
  let ms = case m of
        ReadMode      -> "rb"
        WriteMode     -> "wb"
        AppendMode    -> "ab"
        ReadWriteMode -> "w+b"
  fp <- withCAString fn $ \cp -> withCAString ms $ \cm -> c_fopen cp cm
  if fp == nullPtr
    then error ("openBinaryFileBin: cannot open " ++ fn)
    else do
      bf <- c_add_FILE fp
      mkHandle fn bf (ioModeToHMode m)
