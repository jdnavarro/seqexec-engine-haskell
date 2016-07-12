module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)

import Control.Concurrent.Async (concurrently)

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
                 | Completed
                   deriving (Show)

configured :: Event
configured = EventSystem Configured

observed :: Event
observed = EventSystem Configured

completed :: Event
completed = EventSystem Completed

data UserEvent = Paused
               | Resumed
                 deriving (Show)

paused :: Event
paused = EventUser Paused

resumed :: Event
resumed = EventUser Resumed

execute :: Chan Event -> Sequence -> IO ()
execute chan = traverse_ step
  where
    step (Step (TcsConfig tcs) (InstConfig inst) (Observation obsv)) = do
        (tr, ir) <- concurrently tcs inst
        case (tr, ir) of
             (Done, Done) -> writeChan chan configured
             _            -> error "unhandled error in either TCS or Instrument"
        obr <- obsv
        case obr of
             Done -> writeChan chan observed
             _    -> error "unhandled error in observation"
        -- Poison pill
        writeChan chan completed

data Status = Idle
            | Running
            | Waiting
            | Failed
              deriving (Show)

handle :: Chan Event -> IO ()
handle chan = go Idle
  where
    go Idle = putStrLn "Output: Starting sequence" *> go Running
    go Failed = error "Unimplemented"
    go Waiting = do
      putStrLn "Output: Waiting for commands"
      ev <- readChan chan
      case ev of
        EventSystem es ->
          case es of
            Configured -> do putStrLn "Output: TCS and Instrument configured, paused."
                             go Waiting
            Observed   -> do putStrLn "Output: Observation completed, paused"
                             go Waiting
            Completed  -> putStrLn "Output: Sequence completed"
        EventUser ue ->
          case ue of
            Paused     -> do putStrLn "Output: Already paused"
                             go Waiting
            Resumed    -> do putStrLn "Output: Resuming..."
                             go Running
    go Running = do
      ev <- readChan chan
      case ev of
        EventSystem es ->
          case es of
            Configured -> do putStrLn "Output: TCS and Instrument configured, continuing..."
                             go Running
            Observed   -> do putStrLn "Output: Observation completed, continuing..."
                             go Running
            Completed  -> putStrLn "Output: Sequence completed, continuing..."
        EventUser ue ->
          case ue of
            Paused  -> do putStrLn "Output: Pausing..."
                          go Waiting
            Resumed -> do putStrLn "Output: Already resumed"
                          go Running

input :: Chan Event -> IO ()
input chan = forever $ do
  ic <- getChar
  case ic of
    'p' -> writeChan chan paused
    'r' -> writeChan chan resumed
    _   -> return ()

sequence1 :: Sequence
sequence1 = [
    Step (TcsConfig   $ do putStrLn "System: Start TCS configuration for step 1"
                           threadDelay 2000000
                           putStrLn "System: Complete TCS configuration for step 1"
                           return Done)
         (InstConfig  $ do putStrLn "System: Start instrument configuration for step 1"
                           threadDelay 2000000
                           putStrLn "System: Complete instrument configuration for step 1"
                           return Done)
         (Observation $ do putStrLn "System: Start observation for step 1"
                           threadDelay 2000000
                           putStrLn "System: Stop observation for step 1"
                           return Done)]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  chan <- newChan
  _ <- forkIO $ input chan
  _ <- concurrently (execute chan sequence1) (handle chan)
  putStrLn "Bye"
