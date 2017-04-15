{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Exit (
  exitF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Text.Read
import System.Exit
import System.Posix.Process

exitF :: Builtin
exitF _ = \case
  [] -> getStatus >>= (liftIO . exitImmediately) -- exitWith
  [arg] -> case Str.readStrMaybe arg of
    Just i -> do
      liftIO . exitImmediately $ toEnum i
      ok
    Nothing -> errork
      $ "exit: invalid argument: "
      <> Str.toString arg
  _ -> errork "exit: too many arguments given"

