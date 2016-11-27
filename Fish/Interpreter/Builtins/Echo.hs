{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Echo (
  echoF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.IO
import Fish.Interpreter.Status
import Fish.Lang.Lang

import qualified Data.Text as T
import Data.Text.IO as TextIO
import Data.Monoid
import Control.Lens
import Control.Monad.IO.Class
import System.Exit
import System.IO

echoF :: Bool -> [T.Text] -> Fish ()
echoF _ = \case
  ("-n":args) -> echo (T.unwords args) >> ok
  args -> 
    echoLn (T.unwords args) >> ok


