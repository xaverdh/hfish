{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Exit (
  exitF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str

import Data.Semigroup
import Control.Monad.IO.Class
import Text.Read
import System.Exit
import System.Posix.Process

exitF :: Builtin
exitF _ = argsChoice [0,1] $ \case
  [] -> getStatus >>= (liftIO . exitImmediately) -- exitWith
  [arg] -> case Str.readStrMaybe arg of
    Just i -> do
      liftIO . exitImmediately $ toEnum i
      ok
    Nothing -> errork
      $ "Invalid argument: "
      <> Str.toString arg

