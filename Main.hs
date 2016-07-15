{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (log)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Bifunctor (second)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT, get, gets, put, modify)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask)

type Sequence = [[Action]]

type Action = IO Result

data Status = Running
            | Waiting
            deriving (Show)

type SeqStatus = (Sequence, Status)

data Result = Done | Error deriving (Show)

data Event = Pause
           | Start
           | Completed -- Action completed
           | Failed    -- Action failed
           | Synced    -- Parallel actions completed
           | Finished
           deriving (Show)

configureTcs :: IO Result
configureTcs = do
    putStrLn "System: Start TCS configuration"
    threadDelay 2000000
    putStrLn "System: Complete TCS configuration"
    return Done

configureInst :: IO Result
configureInst = do
    putStrLn "System: Start Instrument configuration"
    threadDelay 2000000
    putStrLn "System: Complete Instrument configuration"
    return Done

observe :: IO Result
observe = do
    putStrLn "System: Start observation"
    threadDelay 2000000
    putStrLn "System: Complete observation"
    return Done

input :: Chan Event -> IO r
input chan = forever $ getChar >>= \case
    'p' -> writeChan chan Pause
    's' -> writeChan chan Start
    _   -> return ()

-- * Executor

type Telescope r = StateT SeqStatus (ReaderT (Chan Event) IO) r

handler :: Telescope r
handler = forever $ receive >>= \case
    Start -> log "Output: Starting" *> switch Running *> execute
    Pause -> log "Output: Paused" *> switch Waiting
    Completed -> log "Output: Action completed"
    Failed -> log "Output: Action failed"
    Synced -> log "Output: Parallel actions completed" *> execute
    Finished -> log "Output: Finished" *> switch Waiting

log :: String -> Telescope ()
log = lift . lift . putStrLn

receive :: Telescope Event
receive = lift ask
      >>= lift . lift . readChan

send :: Event -> Telescope ()
send ev = lift ask
      >>= lift . lift . flip writeChan ev

execute :: Telescope ()
execute = status >>= \case
    Running -> go
    Waiting -> return ()
  where
    go :: Telescope ()
    go = do
        actions <- pop
        chan <- lift ask
        _ <- lift . lift $ mapConcurrently (exe chan) actions
        send Synced
      where
        exe chan action = do
            r <- action
            case r of
              Done  -> writeChan chan Completed
              Error -> writeChan chan Failed

pop :: Telescope [Action]
pop = do
    (seq', st) <- get
    case seq' of
         (x:xs) -> put (xs, st) *> return x
         [] -> send Finished *> return []

switch :: Status -> Telescope ()
switch = modify . second . const

status :: Telescope Status
status = gets snd

sequence0 :: Sequence
sequence0 = [
    [ configureTcs, configureInst ]
  , [ observe ]
  , [ configureTcs, configureInst ]
  , [ observe ]
  ]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    chan <- newChan
    _ <- forkIO $ input chan
    _ <- flip runReaderT chan . flip execStateT (sequence0, Waiting) $ handler
    putStrLn "Done"
