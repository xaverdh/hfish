{-# language LambdaCase, OverloadedStrings #-} 
module Fish.Interpreter.Builtins.Loop (
  continueF
  ,breakF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.Status
import Fish.Interpreter.Util

import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Monoid
import Control.Lens
import System.Exit

mkInvalidErr e = errork $ e <> " invalid argument"
mkNoLoopErr e = errork $ e <> " called outside of loop"

impl l e i
  | i < 1 = mkInvalidErr e
  | otherwise = do
      ks <- view l
      case splitAtMaybe (i-1) ks of
        Nothing -> mkNoLoopErr e
        Just (_,k:_) -> k () >> return undefined

continueF :: Bool -> [T.Text] -> Fish a
continueF _ = \case
  [] -> jump 1
  args -> onMaybe
    (readTextMaybe $ mconcat args)
    (mkInvalidErr "continue:")
    jump
  where
    jump = impl continueK "continue:"
    

breakF :: Bool -> [T.Text] -> Fish a
breakF _ = \case
  [] -> jump 1
  args -> onMaybe
    (readTextMaybe $ mconcat args)
    (mkInvalidErr "break:")
    jump
  where
    jump = impl breakK "break:"

