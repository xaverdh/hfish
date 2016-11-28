{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Concurrent where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar

import System.Process
import System.Exit
import System.IO
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import Data.Functor
import Data.Text.IO as TextIO
import Data.Text as T

createHandleMVarPair :: Fish (MVar T.Text,PT.Fd)
createHandleMVarPair =
  liftIO $ do
    (rE,wE) <- P.createPipe
    mvar <- newEmptyMVar
    forkOS
      ( P.fdToHandle rE >>= TextIO.hGetContents >>= putMVar mvar )
    return (mvar,wE)

pipeFish :: (PT.Fd -> Fish ()) -> (PT.Fd -> Fish ()) -> Fish ()
pipeFish f1 f2 = do
  (rE,wE) <- liftIO P.createPipe
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


