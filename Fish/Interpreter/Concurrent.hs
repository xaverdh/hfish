{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Concurrent where

import Fish.Interpreter.Core
import Fish.Interpreter.Status
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar

import System.Process
import System.Exit
import System.IO
import Data.Functor
import Data.Text.IO as TextIO
import Data.Text as T

createHandleMVarPair :: Fish (MVar T.Text,Handle)
createHandleMVarPair =
  liftIO $ do
    (rE,wE) <- createPipe
    mvar <- newEmptyMVar
    forkOS
      ( TextIO.hGetContents rE >>= putMVar mvar )
    return (mvar,wE)

pipeFish :: (Handle -> Fish ()) -> (Handle -> Fish ()) -> Fish ()
pipeFish f1 f2 = do
  (rE,wE) <- liftIO createPipe
  forkFish (f1 wE)
  f2 rE

forkFish :: Fish () -> Fish (MVar FishState)
forkFish f = do
  r <- disallowK ask
  {- ^^ silently ignore attempts to
        jump out of forked fish-action -}
  s <- get
  liftIO $ do
    mvar <- newEmptyMVar
    forkOS $ do
      s' <- runFish f r s
      putMVar mvar s'
    return mvar

spliceInState :: MVar FishState -> Fish ()
spliceInState mvar = do
  st <- liftIO (takeMVar mvar)
  put st


