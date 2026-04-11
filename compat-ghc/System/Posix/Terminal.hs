-- MicroHs shim for System.Posix.Terminal.
-- Provides the minimal interface used by DArc's CUI.hs / UIBase.hs.
module System.Posix.Terminal
  ( TerminalAttributes
  , TerminalState(..)
  , getTerminalAttributes
  , setTerminalAttributes
  , queryTerminal
  , withoutEcho
  , withMode
  , withoutMode
  , inputTime
  , inputSpeed
  , outputSpeed
  , controlChar
  , withBits
  , bitsOrWith
  , controlFlow
  , discardData
  , sendBreak
  , drainOutput
  , BaudRate(..)
  , TerminalMode(..)
  ) where

import Foreign.C.Types   (CInt(..))
import System.Posix.Types (Fd(..))
import Control.Exception (bracket_)

-- | Opaque terminal attributes (wraps struct termios).
newtype TerminalAttributes = TerminalAttributes ()

data TerminalState = Immediately | WhenDrained | WhenFlushed

data BaudRate = B0 | B50 | B75 | B110 | B134 | B150 | B200 | B300 | B600
              | B1200 | B1800 | B2400 | B4800 | B9600 | B19200 | B38400

data TerminalMode
  = InterruptOnBreak | MapCRtoLF | IgnoreBreak | IgnoreCR | IgnoreParityErrors
  | MapLFtoCR | CheckParity | StripHighBit | RestartOnAny | StartStopInput
  | StartStopOutput | MarkParityErrors | ProcessOutput | LocalMode | ReadEnable
  | TwoStopBits | HangupOnClose | ParityEnable | OddParity | EnableEcho
  | EchoErase | EchoKill | EchoLF | ProcessInput | ExtendedFunctions
  | KeyboardInterrupts | NoFlushOnInterrupt | BackgroundWriteInterrupt
  deriving (Show, Eq, Ord)

foreign import ccall "isatty" c_isatty :: CInt -> IO CInt

queryTerminal :: Fd -> IO Bool
queryTerminal (Fd fd) = fmap (/= 0) (c_isatty fd)

-- Stubs: return a dummy TerminalAttributes.
getTerminalAttributes :: Fd -> IO TerminalAttributes
getTerminalAttributes _ = return (TerminalAttributes ())

setTerminalAttributes :: Fd -> TerminalAttributes -> TerminalState -> IO ()
setTerminalAttributes _ _ _ = return ()

-- | Run an action with terminal echo disabled.
-- Stub: just run the action unchanged (echo remains on).
withoutEcho :: Fd -> IO a -> IO a
withoutEcho _ action = action

-- | Set/clear a terminal mode (stubs: return attrs unchanged).
withMode    :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withMode    a _ = a
withoutMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withoutMode a _ = a

-- Stub accessors — return sensible defaults.
inputTime         :: TerminalAttributes -> Int;      inputTime _ = 0
inputSpeed        :: TerminalAttributes -> BaudRate; inputSpeed _ = B9600
outputSpeed       :: TerminalAttributes -> BaudRate; outputSpeed _ = B9600
controlChar       :: TerminalAttributes -> Int -> Maybe Char; controlChar _ _ = Nothing
withBits          :: TerminalAttributes -> Int -> TerminalAttributes; withBits a _ = a
bitsOrWith        :: TerminalAttributes -> Int -> TerminalAttributes; bitsOrWith a _ = a
controlFlow       :: Fd -> Bool -> Bool -> IO (); controlFlow _ _ _ = return ()
discardData       :: Fd -> Int -> IO (); discardData _ _ = return ()
sendBreak         :: Fd -> Int -> IO (); sendBreak _ _ = return ()
drainOutput       :: Fd -> IO (); drainOutput _ = return ()
