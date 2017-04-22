{-# language OverloadedStrings #-}
module HFish.Interpreter.Builtins.Eval (
  evalF
  ,execF
  -- ,callF
) where

import HFish.Interpreter.Parsing
import HFish.Interpreter.Core
import HFish.Interpreter.Interpreter (progA)
import HFish.Interpreter.Process.Process (fishExec)
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import Data.Monoid
import Fish.Lang
import System.Posix.Process
import System.Exit
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

onArgs :: [Str] -> (Prog T.Text () -> Fish a) -> Fish (Maybe a)
onArgs ts k = do
  res <- parseFishInteractive $ Str.toText (Str.unwords ts <> "\n")
  withProg res k

evalF :: Builtin
evalF _ ts = void (onArgs ts progA)

execF :: Builtin
execF _ (name:args) =
  fishExec
    (Str.toString name)
    (map Str.toString args)


