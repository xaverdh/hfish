{-# language LambdaCase, OverloadedStrings, Rank2Types #-} 
module HFish.Interpreter.Builtins.Flow (
  continueF
  ,breakF
  ,returnF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.Args
import HFish.Interpreter.Stringy (readStrMaybe)
import HFish.Interpreter.Status

import Control.Lens


jumpWith :: Lens' FishReader (() -> Fish ()) -> Fish a
jumpWith lensK = (view lensK >>= ($()) ) *> pure undefined


continueF :: Builtin
continueF _ = args0 $ jumpWith continueK

breakF :: Builtin
breakF _ = args0 $ jumpWith breakK

returnF :: Builtin
returnF _ = args1 $ \s -> case readStrMaybe s of
  Nothing -> errork "return: called with invalid argument"
  Just n -> setStatus (toEnum n) *> jumpWith returnK


