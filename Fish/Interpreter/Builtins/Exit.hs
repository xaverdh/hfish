{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Exit (
  exitF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.Status

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

