{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Cd (
  cdF
) where

import HFish.Interpreter.Cwd
import HFish.Interpreter.Core
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import System.Exit

cdF :: Builtin
cdF fork = \case
  [] -> do
    home <- getHOME
    cdF fork [home]
    ok
  [dir] -> setCWD dir >>= setStatus
  _ -> errork "cd: too many arguments given"

