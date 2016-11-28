{-# language OverloadedStrings #-}
module HFish.Interpreter.Builtins.Source (
  sourceF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Parsing
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Text.IO as TextIO
import Data.Monoid
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import System.Exit
import System.IO

sourceF :: Bool -> [T.Text] -> Fish ()
sourceF _ args = do
  results <- forM args (parseFish . T.unpack)
  forM_ results (`withProg` progA)
