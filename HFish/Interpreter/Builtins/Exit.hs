{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Exit (
  exitF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import Control.Monad.IO.Class
import Text.Read
import System.Exit

exitF :: Bool -> [T.Text] -> Fish ()
exitF _ = \case
  [] -> getStatus >>= (liftIO . exitWith)
  [arg] -> case readMaybe $ T.unpack arg of
    Just i -> do
      liftIO . exitWith $ toEnum i
      ok
    Nothing -> errork $ "exit: invalid argument: " <> arg
  _ -> errork "exit: too many arguments given"
