{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Pid.Pid where

import Fish.Lang.Lang
import Fish.Interpreter.Core
import Fish.Interpreter.Globbed

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Text.Read
import System.Process
import System.Posix.Process

getPID :: T.Text -> Fish [Globbed]
getPID = \case
  "self" -> do
    pid <- liftIO getProcessID
    return [fromString $ show pid]
  "last" ->
    use lastPid >>= \case
      Just pid -> return [fromString $ show pid]
      Nothing -> error "Cannot obtain pid of last process."
  x -> case readMaybe $ T.unpack x of
    Just i -> do  
      gpid <- liftIO (getProcessGroupIDOf i)
      return [fromString $ show gpid]
    Nothing -> do
      pids <- liftIO $ readProcess "pidof" [T.unpack x] ""
      return (map fromString $ words pids)

