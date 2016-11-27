{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Bool (
  trueF
  ,falseF  
) where

import Fish.Interpreter.Core
import Fish.Interpreter.Status

import qualified Data.Text as T
import System.Exit

trueF :: Bool -> [T.Text] -> Fish ()
trueF _ = \case
  [] -> setStatus ExitSuccess
  _ -> errork "true: too many arguments given"

falseF :: Bool -> [T.Text] -> Fish ()
falseF _  = \case
  [] -> setStatus (ExitFailure 1)
  _ -> errork "false: too many arguments given"


