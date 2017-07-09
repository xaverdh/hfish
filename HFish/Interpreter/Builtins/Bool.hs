{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Bool (
  trueF
  ,falseF  
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Args

import System.Exit

trueF :: Builtin
trueF _ = args0 $ setStatus ExitSuccess

falseF :: Builtin
falseF _  = args0 $ setStatus (ExitFailure 1)


