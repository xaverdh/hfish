{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Builtins.Cd (
  cdF
) where

import Fish.Interpreter.Cwd
import Fish.Interpreter.Core
import Fish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import System.Exit

cdF :: Bool -> [T.Text] -> Fish ()
cdF fork = \case
  [] -> do
    home <- getHOME
    cdF fork [T.pack home]
    ok
  [dir] -> setCWD $ T.unpack dir
  _ -> errork "cd: too many arguments given"

