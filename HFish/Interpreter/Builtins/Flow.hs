{-# language LambdaCase, OverloadedStrings #-} 
module HFish.Interpreter.Builtins.Flow (
  continueF
  ,breakF
  ,returnF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import qualified HFish.Interpreter.Stringy as Str

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

continueF :: Builtin
continueF _ = \case
  [] -> jump 1
  args -> onMaybe
    (Str.readStrMaybe $ mconcat args)
    (mkInvalidErr "continue:")
    jump
  where
    jump = jumpWith continueK "continue:"
    

breakF :: Builtin
breakF _ = \case
  [] -> jump 1
  args -> onMaybe
    (Str.readStrMaybe $ mconcat args)
    (mkInvalidErr "break:")
    jump
  where
    jump = jumpWith breakK "break:"

returnF :: Builtin
returnF _ = \case
  [] -> ret
  [t] -> maybe
           (noIntErr t)
           (setStatus . toEnum)
           (Str.readStrMaybe t)
         >> ret
  _ -> errork "return: too many arguments given"
  where
    noIntErr t = errork
      $ "return: expected integer, got: "
      <> Str.toString t
    ret = view returnK >>= \case
      [] -> errork "no function to return from"
      k:_ -> k () >> return undefined

