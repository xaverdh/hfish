{-# language OverloadedStrings #-}
module HFish.Interpreter.Builtins.Source (
  sourceF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Parsing
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str

import Data.Text.IO as TextIO
import Data.Monoid
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import System.Exit
import System.IO

sourceF :: Builtin
sourceF _ args = do
  results <- forM args (parseFish . Str.toString)
  forM_ results (`withProg` progA)

