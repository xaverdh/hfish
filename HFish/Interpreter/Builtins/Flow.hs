{-# language LambdaCase, OverloadedStrings #-} 
module HFish.Interpreter.Builtins.Flow (
  continueF
  ,breakF
  ,returnF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Util

import qualified Data.Text as T
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
    (readTextIntegralMaybe $ mconcat args)
    (mkInvalidErr "continue:")
    jump
  where
    jump = impl continueK "continue:"
    

breakF :: Bool -> [T.Text] -> Fish a
breakF _ = \case
  [] -> jump 1
  args -> onMaybe
    (readTextIntegralMaybe $ mconcat args)
    (mkInvalidErr "break:")
    jump
  where
    jump = impl breakK "break:"

returnF :: Bool -> [T.Text] -> Fish a
returnF _ = \case
  [] -> ret
  [t] -> maybe
           (noIntErr t)
           (setStatus . toEnum)
           (readTextIntegralMaybe t)
         >> ret
  _ -> errork "return: too many arguments given"
  where
    noIntErr t = errork ("return: expected integer, got: " <> t)
    ret = do
      k:_ <- view returnK
      k () >> return undefined

