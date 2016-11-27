{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Read (
  readF
) where

import Fish.Lang.Lang

import Fish.Interpreter.Core
import Fish.Interpreter.IO
import Fish.Interpreter.Var
import Fish.Interpreter.Status

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
