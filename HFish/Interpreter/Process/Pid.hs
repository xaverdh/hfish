{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.Pid where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Globbed
import HFish.Interpreter.Util
import qualified HFish.Interpreter.Stringy as Str

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Sequence
import Text.Read
import System.Process
import System.Posix.Process

getPID :: Str -> Fish (Seq Globbed)
getPID = \case
  "self" -> toSeq <$> liftIO getProcessID
  "last" ->
    use lastPid >>= \case
      Just pid -> return $ toSeq pid
      Nothing -> errork "Cannot obtain pid of last process."
  x -> case Str.readStrMaybe x of
    Just i -> toSeq <$> liftIO (getProcessGroupIDOf i)
    Nothing -> toSeq
      <$> liftIO (readProcess "pidof" [Str.toString x] "")
      -- ^ todo: do something better then calling pidof ?
  where
    toSeq :: Show a => a -> Seq Globbed
    toSeq = pure . fromString . show
