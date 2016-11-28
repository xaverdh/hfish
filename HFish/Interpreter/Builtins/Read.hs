{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Read (
  readF
) where

import HFish.Lang.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Var
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import Data.Text.IO as TextIO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit


readF :: Bool -> [T.Text] -> Fish ()
readF _ = \case
  [] -> errork "read: no arguments given"
  args -> do
    l <- readLineFrom Fd0
    loop args (T.words l)
    ok
  where
    loop [a] wds = 
      void $ setVarSafe localEnv a (Var False wds)
    loop (a:args) (w:wds) = do
      setVarSafe localEnv a (Var False [w])
      loop args wds
    loop args [] = 
      forM_ args $ \a ->
        setVarSafe localEnv a (Var False [])
