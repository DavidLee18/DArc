---------------------------------------------------------------------------------------------------
---- "Communicating Sequential Processes", as described in Hoare's book.                       ----
---------------------------------------------------------------------------------------------------
-- |
-- Module      :  Process
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Concurrency model: pipeline of concurrent processes communicating via MVar/Chan.
-- Each stage runs in its own OS thread (forkIO) and passes data forward via
-- sendP/receiveP.  Back-pressure is provided by the one-element MVar channel
-- created by '|>', while '|>>>' creates an unbounded Chan for look-ahead I/O.
--
-----------------------------------------------------------------------------

module Process where
{-
Processes are chained together with the "|>" or "|>>>" operators and started for execution by the runP function:
    runP( read_files |>>> compress |> write_data )
Processes run in parallel thanks to the fact that forkOS is used to start them.
Each process is described by an ordinary function that receives an extra parameter of type Pipe.
  With this variable you can perform the receiveP operation to obtain data from the previous
  process in the list, and the sendP operation to send data to the next process in the list:
    compress pipe = foreverM (do data <- receiveP pipe; .....; sendP pipe compressed_data)
Data is passed from process to process "left to right". Depending on which operation was used
  when the link between the processes was created - "|>" or "|>>>" - the channel between those processes
  can hold either only one value or an unbounded number of values (implemented with MVar/Chan,
  respectively).
Data can also be sent in the opposite direction ("right to left") with the send_backP and receive_backP operations.
  The backward channel always has unbounded capacity. It can be used, for example,
  to acknowledge the completion of operations, for synchronization, and for returning used resources
  (for instance, I/O buffers):
    producer: sendP pipe (buf,len); receive_backP pipe; now the buffer is free
    consumer: (buf,len) <- receiveP pipe; hPutBuf file buf len; send_backP pipe ()
The runP operation runs synchronously; it finishes once the last process in the chain has finished
  (even if the remaining processes have not finished yet). If the first process in the list of processes
  being started tries to exchange data with a previous one (i.e. performs receiveP/send_backP), or
  the last process tries to exchange data with a following one - an error is signalled.
The runAsyncP operation starts a process or a chain of processes asynchronously and returns a Pipe for exchanging
  data with it/them. In this case the first process in the chain may also talk to a "previous" one, and the last - to a
  "following" one, although this is not required:
    pipe <- runAsyncP compress; sendP pipe data; compressed_data <- receiveP pipe
    pipe <- runAsyncP( compress |> write_data ); sendP pipe data
    pipe <- runAsyncP( read_files |>>> compress ); compressed_data <- receiveP pipe
    runAsyncP( read_files |>>> compress |> write_data )
  The input and output queues of an asynchronously started process are (for now) single-element.
-}

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

-- | Unbounded FIFO channel that uses takeMVar (not readMVar) for blocking reads.
-- MicroHs's put_mvar does not wake threads blocked via readMVar (mv_read queue),
-- so Control.Concurrent.Chan.readChan deadlocks when the reader arrives first.
-- This implementation uses takeMVar on holes so readers wait in mv_takeput,
-- which put_mvar handles correctly.
data OurStream a = OurStream (MVar (OurChItem a))
data OurChItem a = OurChItem a (OurStream a)

data OurChan a = OurChan !(MVar (OurStream a)) !(MVar (OurStream a))

newOurChan :: IO (OurChan a)
newOurChan = do
  hole     <- newEmptyMVar
  readVar  <- newMVar (OurStream hole)
  writeVar <- newMVar (OurStream hole)
  return (OurChan readVar writeVar)

writeOurChan :: OurChan a -> a -> IO ()
writeOurChan (OurChan _ writeVar) val = do
  new_hole <- newEmptyMVar
  OurStream old_hole <- takeMVar writeVar
  putMVar old_hole (OurChItem val (OurStream new_hole))
  putMVar writeVar (OurStream new_hole)

readOurChan :: OurChan a -> IO a
readOurChan (OurChan readVar _) = do
  OurStream read_end <- takeMVar readVar
  OurChItem val new_read_end <- takeMVar read_end
  putMVar readVar new_read_end
  return val

instance PipeElement OurChan where
  getP = readOurChan
  putP = writeOurChan

-- |The operation that connects two sequential processes:
-- the output channel of the first becomes the input channel of the second.
-- "|>" creates a single-element queue, while "|>>>" - a queue of unbounded length
infixl 1  |>, |>>>

p1 |>   p2 = createP p1 p2 newEmptyMVar
p1 |>>> p2 = createP p1 p2 newOurChan

createP p1 p2 create_inner (Pipe pid finished income income_back outcome outcome_back) = do
  inner       <- create_inner      -- Channel between p1 and p2 (MVar or Chan)
  inner_back  <- newOurChan        -- Backward channel between p1 and p2 (unbounded, uses takeMVar not readMVar)
  p1_finished <- newEmptyMVar      -- Flag signalling that p1 has finished

  -- Start the first process in a separate thread, and execute the second one directly
  p1_id <- forkIO$ (p1 (Pipe pid finished income income_back inner inner_back) >> return ())
                       `finally` (putMVar p1_finished ())
  --
  p2 (Pipe (Just p1_id) (Just p1_finished) inner inner_back outcome outcome_back)
  takeMVar p1_finished
  return ()


-- |Run the combined process created by the "|>" and "|>>>" operations
runP p = do
  p (Pipe Nothing
          Nothing
          (error "First process in runP tried to receive")
          (error "First process in runP tried to send_back")
          (error "Last process in runP tried to send")
          (error "Last process in runP tried to receive_back"))

-- |Run a process asynchronously and return the channel for exchanging data with it
runAsyncP p = do
  income  <- newEmptyMVar
  outcome <- newEmptyMVar
  income_back  <- newEmptyMVar
  outcome_back <- newEmptyMVar
  parent_id    <- myThreadId
  p_finished   <- newEmptyMVar
  p_id         <- forkIO (p (Pipe Nothing Nothing income income_back outcome outcome_back)
                            `catch` (\(e :: SomeException) -> do killThread parent_id; throwIO e)
                            `finally` putMVar p_finished ())
  return (Pipe (Just p_id) (Just p_finished) outcome outcome_back income income_back)


-- |The channel for exchanging data with the neighbouring processes, which every process receives for its own use.
-- The channel has 6 elements - the ID of the previous (asynchronously started) process,
--                           the MVar variable signalling its completion,
--                           the input data, the sending of acknowledgements,
--                           the output data, the receiving of acknowledgements
data Pipe a b c d  =  Pipe (Maybe ThreadId) (Maybe (MVar ())) a b c d
killP    pipe@(Pipe (Just pid) _ _ _ _ _)                                  = killThread pid >> joinP pipe
joinP         (Pipe _ (Just finished) _ _ _ _)                             = takeMVar finished
receiveP      (Pipe pid finished income income_back outcome outcome_back)  = getP income
sendP         (Pipe pid finished income income_back outcome outcome_back)  = putP outcome
receive_backP (Pipe pid finished income income_back outcome outcome_back)  = getP outcome_back
send_backP    (Pipe pid finished income income_back outcome outcome_back)  = putP income_back

-- |A rather strange operation - "returning" messages to oneself - just as if it had been done by
-- the following process in the queue. But it is needed in order to create the initial pool of resources used by
-- the process
send_back_itselfP (Pipe pid finished income income_back outcome outcome_back)  =  putP outcome_back


-- |An element of the channel between processes - it can be of type MVar as well as Chan
class PipeElement e where
  getP :: e a -> IO a
  putP :: e a -> a -> IO ()

instance PipeElement MVar where
  getP = takeMVar
  putP = putMVar

instance PipeElement Chan where
  getP = readChan
  putP = writeChan

-- |A pseudo-channel of a process - it consists of two explicitly given functions for receiving and sending data
data PairFunc a = PairFunc (IO a) (a -> IO ())

instance PipeElement PairFunc where
  getP (PairFunc get_f put_f) = get_f
  putP (PairFunc get_f put_f) = put_f

-- |The procedure for starting a process with 4 functions emulating the I/O channels
runFuncP :: (Pipe (PairFunc a) (PairFunc b) (PairFunc c) (PairFunc d) -> IO e)
         -> IO a -> (b -> IO ()) -> (c -> IO ()) -> IO d -> IO e
runFuncP p receive_f send_back_f send_f receive_back_f  =
  p (Pipe Nothing
          Nothing
          (PairFunc receive_f      undefined)
          (PairFunc undefined      send_back_f)
          (PairFunc undefined      send_f)
          (PairFunc receive_back_f undefined))

{-# NOINLINE createP #-}
{-# NOINLINE runP #-}
{-# NOINLINE runAsyncP #-}
{-# NOINLINE runFuncP #-}


-- Usage example:
{-
exampleP = do
  -- Demonstrates using of "runP"
  print "runP: before"
  runP( producer 5 |> transformer (++"*2") |> transformer (++"+1") |> printer "runP" )
  print "runP: after"

  -- Demonstrates using of "runAsyncP" to run computation as parallelly computed function
  pipe <- runAsyncP (transformer (++" modified"))
  sendP pipe "value"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" with "|>"
  pipe <- runAsyncP( transformer (++"*2") |> transformer (++"+1") )
  sendP pipe "7"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" to run asynchronous process
  print "runAsyncP: before"
  pipe <- runAsyncP( producer 7 |> printer "runAsyncP" )
  print "runAsyncP: after?"

producer n pipe = do
  mapM_ (sendP pipe.show) [1..n]
  sendP pipe "0"

transformer f pipe = do
  n <- receiveP pipe
  sendP pipe (f n)
  transformer f pipe

printer str pipe = do
  n <- receiveP pipe
  when (head n/='0')$  do print$ str ++ ": " ++ n
                          printer str pipe
-}

{- Design principles:
1. Processes in runP must be started left to right. With a small volume
of data being processed this will lead to the first process in the
pipeline producing all the necessary data and finishing before the
second and subsequent processes are started at all
2. runP must start all the processes in extra threads and wait for
them to finish. It is desirable to leave runP only after
all the processes have finished their work
3. When a process finishes, the previous process in the pipeline must receive
an exception in order to finish as quickly as possible (after which
the process preceding it must receive an exception in its turn).
The following process, however, must receive only the information that the
input data has ended, when it tries to read it (tryReceiveP, eofP)
4. When an unhandled exception arises in one of the processes, all
the remaining processes in the pipeline must be terminated (by sending the
KillThread signal) and that exception re-raised in the main process
5. runP (p1 |> p2 |> protectP p) protects process `p` from exceptions being raised in it;
instead the situations that arise are only signalled in the state of the channel
6. To wait for the completion of a process started by runAsyncP or of
the previous process in the pipeline, introduce the operation joinP pipe
7. "p |> yP p1 p2" sends the output of one process to two others
8. killP pipe kills all the processes of an asynchronously started pipeline
9. Convenient and efficient means are needed for creating processes that have
several input and/or output channels (use getP/putP?)
10. new_pipe <- insertOnInputP old_pipe process - insert a new process before one's own input
    new_pipe <- insertOnOutputP old_pipe process - insert a new process after one's own output

p1 |> p2   -->  PChain p1 p2 ?

(p1 |> p2) pipe{ MainThreadId, ref_threads... }
  p2_threadId <- forkIO $ (p2 pipe2  >> writeIORef pipe.isEof True - once the second process has ended)
                          `catch` (throwTo MainThreadId)
  addToMVar ref_threads p2_threadId
  p1 pipe1

p1 |> (p2 |> p3)
  forkIO (forkIO p3; p2)
  p1

runP p =
  p_threadId <- forkIO$ p pipe{ MainThreadId = MyThreadId, ref_threads = newIORef [], ...}
  addToMVar ref_threads p_threadId
  wait them all `catch` (\e -> mapM killThread ref_threads; throw e)

11. Around a started thread - a catch which kills the child, waits for it to finish and sends
    the exception it received to the parent
-}


{-New design guidelines:
1. a|>b is started as "fork a; b"
2. When b finishes, wait for a to finish
3. When an unhandled signal arises in any of the processes
   all the processes in the pipeline must be killed and this signal re-raised in the main program
-}
