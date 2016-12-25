{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Emit (
  emitF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import HFish.Interpreter.Events

import qualified Data.Text as T
import Data.NText
import Data.Functor
import Data.Monoid
import Data.Bool
import Data.Char (ord,chr)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.IO


emitF :: Bool -> [T.Text] -> Fish ()
emitF _ = \case
  [] -> errork "emit: too few arguments given"
  name:ts -> handleEvent (mkNText name) ts
