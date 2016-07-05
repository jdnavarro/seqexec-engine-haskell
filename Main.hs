module Main where

import Control.Concurrent (threadDelay)
import Data.Foldable (traverse_)
import Control.Concurrent.Async (concurrently)

type Sequence = [Step]

data Step = Step TcsConfig InstConfig Observation

newtype TcsConfig   = TcsConfig   (IO Result)
newtype InstConfig  = InstConfig  (IO Result)
newtype Observation = Observation (IO Result)

data Result = Done
            | Error

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

execute :: Sequence -> IO ()
execute = traverse_ step
  where
    step :: Step -> IO ()
    step (Step (TcsConfig tcs) (InstConfig inst) (Observation obsv)) = do
        (_tcsResult, _instResult) <- concurrently tcs inst
        _obsvResult <- obsv
        return ()

main :: IO ()
main = do execute sequence1
          putStrLn "Done"
