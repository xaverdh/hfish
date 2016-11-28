{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Echo (
  echoF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Lang.Lang

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


