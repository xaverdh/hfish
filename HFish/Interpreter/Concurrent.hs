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
import System.Unix.IO
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import Data.Functor
-- import Data.Text.IO as TextIO
import Data.Text as T

createHandleMVarPair :: Fish (MVar Str,PT.Fd)
createHandleMVarPair =
  liftIO $ do
    (rE,wE) <- P.createPipe
    mvar <- newEmptyMVar
    forkIO ( fdGetContents rE >>= putMVar mvar )
    -- ( P.fdToHandle rE >>= TextIO.hGetContents >>= putMVar mvar )
    return (mvar,wE)

forkFish :: Fish () -> Fish (MVar FishState)
forkFish f = do
  r <- disallowK ask
  {- ^^ silently ignore attempts to
        jump out of forked fish-action -}
  s <- get
  liftIO $ do
    mvar <- newEmptyMVar
    forkIO $ do
      s' <- runFish f r s
      putMVar mvar s'
    return mvar


