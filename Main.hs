module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Data.Foldable (traverse_)

import Control.Concurrent.Async -- (concurrently)

type Sequence = [Step]

data Step = Step TcsConfig InstConfig Observation

newtype TcsConfig   = TcsConfig   (IO Result)
newtype InstConfig  = InstConfig  (IO Result)
newtype Observation = Observation (IO Result)

data Result = Done
            | Error
              deriving (Show)

data Event = Configured
           | Observed
             deriving (Show)

sequence1 :: Sequence
sequence1 = [
    Step (TcsConfig   $ do putStrLn "Start: TCS configuration for step 1"
                           threadDelay(2000000)
                           putStrLn "Completion: TCS configuration for step 1"
                           return Done)
         (InstConfig  $ do putStrLn "Start: instrument configuration for step 1"
                           threadDelay(2000000)
                           putStrLn "Completion: instrument configuration for step 1"
                           return Done)
         (Observation $ do putStrLn "Start: observation for step 1"
                           threadDelay(2000000)
                           putStrLn "Completion: observation for step 1"
                           return Done)]

execute :: Chan Event -> Sequence -> IO ()
execute chan = traverse_ step
  where
    step :: Step -> IO ()
    step (Step (TcsConfig tcs) (InstConfig inst) (Observation obsv)) = do
        (tr, ir) <- concurrently tcs inst
        case (tr, ir) of
             (Done, Done) -> writeChan chan Configured
             _            -> error "unhandled error in either TCS or Instrument"
        obr <- obsv
        case obr of
             Done -> writeChan chan Observed
             _    -> error "unhandled error in observation"
        return ()

main :: IO ()
main = do
  chan <- newChan
  execute chan sequence1
  putStrLn "Sequence completed"
