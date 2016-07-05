module Main where

import Control.Concurrent (threadDelay)
import Data.Foldable (sequenceA_)
import Control.Concurrent.Async (Concurrently(..), runConcurrently)

type Action = [[IO ()]]

step1 :: Action
step1 =
  [ [ do putStrLn "Start: TCS configuration for step 1"
         threadDelay(2000000)
         putStrLn "Completion: TCS configuration for step 1"
    ]
  , [ do putStrLn "Start: instrument configuration for step 1"
         threadDelay(2000000)
         putStrLn "Completion: instrument configuration for step 1"
    , do putStrLn "Start: observation for step 1"
         threadDelay(2000000)
         putStrLn "Completion: observation for step 1"
    ]
  ]

execute :: Action -> IO ()
execute []          = pure ()
execute ([]:sios)   = execute sios
execute ([io]:sios) = io *> execute sios
execute (pios:sios) = concurrent pios *> execute sios
  where
    concurrent = runConcurrently . sequenceA_ . fmap Concurrently

main :: IO ()
main = do execute step1
          putStrLn "Done"
