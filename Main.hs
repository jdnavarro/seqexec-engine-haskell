module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Foldable (traverse_)

import Control.Concurrent.Async -- (concurrently)

type Sequence = [Step]

data Step = Step TcsConfig InstConfig Observation

newtype TcsConfig   = TcsConfig   (IO Result)
newtype InstConfig  = InstConfig  (IO Result)
newtype Observation = Observation (IO Result)

data Result = Done | Error deriving (Show)

data Event = Configured | Observed | Completed deriving (Show)

execute :: Chan Event -> Sequence -> IO ()
execute chan = traverse_ step
  where
    step (Step (TcsConfig tcs) (InstConfig inst) (Observation obsv)) = do
        (tr, ir) <- concurrently tcs inst
        case (tr, ir) of
             (Done, Done) -> writeChan chan Configured
             _            -> error "unhandled error in either TCS or Instrument"
        obr <- obsv
        case obr of
             Done -> writeChan chan Observed
             _    -> error "unhandled error in observation"
        -- Poison pill
        writeChan chan Completed

data Status = Idle
            | Running
            | Failed
              deriving (Show)

handler :: Chan Event -> IO ()
handler chan = handle Idle
  where
    handle Idle = putStrLn "Output: Starting sequence" *> handle Running
    handle Running = do
      ev <- readChan chan
      case ev of
        Configured -> putStrLn "Output: TCS and Instrument configured" *> handle Running
        Observed   -> putStrLn "Output: Observation completed" *>  handle Running
        Completed  -> putStrLn "Output: Sequence completed"
    handle Failed = error "Unimplemented"

sequence1 :: Sequence
sequence1 = [
    Step (TcsConfig   $ do putStrLn "System: Start TCS configuration for step 1"
                           threadDelay(2000000)
                           putStrLn "System: Complete TCS configuration for step 1"
                           return Done)
         (InstConfig  $ do putStrLn "System: Start instrument configuration for step 1"
                           threadDelay(2000000)
                           putStrLn "System: Complete instrument configuration for step 1"
                           return Done)
         (Observation $ do putStrLn "System: Start observation for step 1"
                           threadDelay(2000000)
                           putStrLn "System: Stop observation for step 1"
                           return Done)]

main :: IO ()
main = do
  chan <- newChan
  _ <- concurrently (execute chan sequence1) (handler chan)
  putStrLn "Bye"
