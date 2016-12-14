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

-- Todo: proper support for scopes.

readF :: Bool -> [T.Text] -> Fish ()
readF _ = \case
  [] -> errork "read: no arguments given"
  args -> do
    l <- readLineFrom Fd0
    loop args (T.words l)
    ok
  where
    setIt vs = 
      flip ( setVarSafe ( EnvLens localEnv ) )
      ( Var UnExport vs )
    loop [a] wds =
      void $ setIt wds a
    loop (a:args) (w:wds) = do
      setIt [w] a
      loop args wds
    loop args [] = 
      forM_ args $ setIt []
