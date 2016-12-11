{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Printf (
  printfF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Util
import HFish.Interpreter.Status

import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as T

import System.Posix.Types
import System.Posix.Files
import System.Posix.User

-- import Text.Parser.Combinators
-- import Text.Parser.Token
import Text.Parser.Expression
import Data.Attoparsec.Text
import Text.Parser.Char (alphaNum)

import Text.Printf

printF :: Bool -> [T.Text] -> Fish ()
printF _ (t:ts) =
  printf (T.unpack t) 
  
  
  
  



