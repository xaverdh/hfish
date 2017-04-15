{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Bool (
  trueF
  ,falseF  
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status

import qualified Data.Text as T
import System.Exit

trueF :: Builtin
trueF _ = \case
  [] -> setStatus ExitSuccess
  _ -> errork "true: too many arguments given"

falseF :: Builtin
falseF _  = \case
  [] -> setStatus (ExitFailure 1)
  _ -> errork "false: too many arguments given"


