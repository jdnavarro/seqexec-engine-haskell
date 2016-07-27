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

type Sequence = [Step]

type Step = [Action]

type Action = IO Result

data Status = Running | Waiting deriving (Show)

type SeqStatus = (Sequence, Status)

data Result = Done | Error deriving (Eq, Show)

data Event = Start
           | Pause
           | Completed -- Action completed
           | Failed    -- Action failed
           | Synced    -- Parallel actions completed
           | SyncFailed
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

type Telescope r = StateT SeqStatus (ReaderT (Chan Event) IO) r

handler :: Telescope r
handler = forever $ receive >>= \case
    Start -> log "Output: Started" *> switch Running *> run
    Pause -> log "Output: Paused" *> switch Waiting
    Completed -> log "Output: Action completed"
    Failed -> log "Output: Action failed"
    Synced -> log "Output: Parallel actions completed" *> run
    SyncFailed -> log "Output: One of the actions failed, repeating..."
    Finished -> log "Output: Finished" *> switch Waiting

run :: Telescope ()
run = status >>= \case
    Running -> do
        actions <- step
        chan <- lift ask
        rs <- lift . lift $ mapConcurrently (execute chan) actions
        if all (== Done) rs
           then remove *> send Synced
           else send SyncFailed
      where
        execute chan action = do
            r <- action
            case r of
              Done  -> writeChan chan Completed *> action
              Error -> writeChan chan Failed *> action
    Waiting -> return ()

switch :: Status -> Telescope ()
switch = modify . second . const

status :: Telescope Status
status = gets snd

log :: String -> Telescope ()
log = lift . lift . putStrLn

send :: Event -> Telescope ()
send ev = lift ask >>= lift . lift . flip writeChan ev

receive :: Telescope Event
receive = lift ask >>= lift . lift . readChan

step :: Telescope Step
step = do
    (seq', _) <- get
    case seq' of
         (x:_) -> return x
         [] -> send Finished *> return []

remove :: Telescope ()
remove = do
    (seq', st) <- get
    case seq' of
         (_:xs) -> put (xs, st)
         [] -> return ()

input :: Chan Event -> IO r
input chan = forever $ getChar >>= \case
    'p' -> writeChan chan Pause
    's' -> writeChan chan Start
    _   -> return ()

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
