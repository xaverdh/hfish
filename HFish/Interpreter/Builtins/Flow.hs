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
import qualified Data.Sequence as Seq
import Data.Sequence hiding (drop)
import Data.Monoid
import Control.Lens
import System.Exit

mkInvalidErr e = errork $ e <> " invalid argument"
mkNoLoopErr e = errork $ e <> " called outside of loop"

jumpWith l e i
  | i < 1 = mkInvalidErr e
  | otherwise = do
      ks <- view l
      case drop (i-1) ks of
        [] -> mkNoLoopErr e
        k:_ -> k () >> return undefined

continueF :: Bool -> [T.Text] -> Fish a
continueF _ = \case
  [] -> jump 1
  args -> onMaybe
    (readTextMaybe $ mconcat args)
    (mkInvalidErr "continue:")
    jump
  where
    jump = jumpWith continueK "continue:"
    

breakF :: Bool -> [T.Text] -> Fish a
breakF _ = \case
  [] -> jump 1
  args -> onMaybe
    (readTextMaybe $ mconcat args)
    (mkInvalidErr "break:")
    jump
  where
    jump = jumpWith breakK "break:"

returnF :: Bool -> [T.Text] -> Fish a
returnF _ = \case
  [] -> ret
  [t] -> maybe
           (noIntErr t)
           (setStatus . toEnum)
           (readTextMaybe t)
         >> ret
  _ -> errork "return: too many arguments given"
  where
    noIntErr t = errork ("return: expected integer, got: " <> t)
    ret = view returnK >>= \case
      [] -> errork "no function to return from"
      k:_ -> k () >> return undefined

