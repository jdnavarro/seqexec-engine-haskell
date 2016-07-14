{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Foldable (traverse_)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)

import Control.Concurrent.Async (concurrently)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

type Sequence = [Step]

data Step = Step TcsConfig InstConfig Observation

newtype TcsConfig   = TcsConfig   (IO Result)
newtype InstConfig  = InstConfig  (IO Result)
newtype Observation = Observation (IO Result)

data Result = Done | Error deriving (Show)

data Event = EventSystem SystemEvent
           | EventUser   UserEvent
             deriving (Show)

data SystemEvent = Configured
                 | Observed
                 | Paused
                 | Completed
                   deriving (Show)

configured :: Event
configured = EventSystem Configured

observed :: Event
observed = EventSystem Configured

paused :: Event
paused = EventSystem Paused

completed :: Event
completed = EventSystem Completed

data UserEvent = Pausing
               | Resuming
                 deriving (Show)

data Status = Idling
            | Running
            | Stopping
            | Waiting
            | Failing
            | Finishing
              deriving (Show)

type Semaphore = MVar ()

pausing :: Event
pausing = EventUser Pausing

resuming :: Event
resuming = EventUser Resuming

execute :: Chan Event -> Sequence -> ReaderT Semaphore IO ()
execute chan = traverse_ step
  where
    step :: Step -> ReaderT Semaphore IO ()
    step (Step (TcsConfig tcs) (InstConfig inst) (Observation obsv)) = do
      red
      (tr, ir) <- lift $ concurrently tcs inst
      case (tr, ir) of
          (Done, Done) -> lift $ writeChan chan configured
          _            -> error "unhandled error in either TCS or Instrument"
      green
      red
      lift obsv >>= \case
          Done -> lift $ writeChan chan observed
          _    -> error "unhandled error in observation"
      lift $ writeChan chan completed
      green

    -- Acquire the lock
    red = ask >>= lift . takeMVar
    -- Release the lock
    green = ask >>= lift . flip putMVar ()

handle :: Chan Event -> Semaphore -> StateT Status IO ()
handle chan semaphore = get >>= \case
    Finishing -> lift $ putStrLn "Done"
    st -> go st *> handle chan semaphore
  where
    go = \case
      Idling  -> do
        lift $ putStrLn "Output: Starting sequence"
        put Running
      Failing -> error "Unimplemented"
      Waiting -> do
        lift $ putStrLn "Output: Waiting for commands"
        lift (readChan chan) >>= \case
          EventSystem es ->
            case es of
              Configured -> do
                lift $ putStrLn "Output: TCS and Instrument configured, paused."
                put Waiting
              Observed   -> do
                lift $ putStrLn "Output: Observation completed, paused"
                put Waiting
              Completed  -> lift $ putStrLn "Output: Step completed"
          EventUser ue ->
            case ue of
              Pausing    -> do
                lift $ putStrLn "Output: Already paused"
                put Waiting
              Resuming   -> do
                lift $ putStrLn "Output: Resuming..."
                lift $ putMVar semaphore ()
                put Running
      Running ->
        lift (readChan chan) >>= \case
          EventSystem es ->
            case es of
              Configured -> do lift $ putStrLn "Output: TCS and Instrument configured, continuing..."
                               put Running
              Observed   -> do lift $ putStrLn "Output: Observation completed, continuing..."
                               put Running
              Completed  -> lift $ putStrLn "Output: Step completed, continuing..."
          EventUser ue ->
            case ue of
              Pausing    -> do lift $ putStrLn "Output: Pausing..."
                               lift $ takeMVar semaphore
                               put Waiting
              Resuming   -> do lift $ putStrLn "Output: Already resumed"
                               put Running

input :: Chan Event -> IO ()
input chan = forever $ getChar >>= \case
    'p' -> writeChan chan pausing
    'r' -> writeChan chan resuming
    _   -> return ()

mkSequence :: Int -> Sequence
mkSequence n = [
    Step (TcsConfig   $ do putStrLn $ "System: Start TCS configuration for step " ++ show n
                           threadDelay 2000000
                           putStrLn $ "System: Complete TCS configuration for step " ++ show n
                           return Done)
         (InstConfig  $ do putStrLn $ "System: Start instrument configuration for step " ++ show n
                           threadDelay 2000000
                           putStrLn $ "System: Complete instrument configuration for step " ++ show n
                           return Done)
         (Observation $ do putStrLn $ "System: Start observation for step " ++ show n
                           threadDelay 2000000
                           putStrLn $ "System: Complete observation for step " ++ show n
                           return Done)]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  chan <- newChan
  semaphore <- newMVar ()
  _ <- forkIO $ input chan
  let sequence0 = concatMap mkSequence [1..3]
  _ <- concurrently (runReaderT (execute chan sequence0) semaphore)
                    (execStateT (handle chan semaphore) Idling)
  putStrLn "Bye"
