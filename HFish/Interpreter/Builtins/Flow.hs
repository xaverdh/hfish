{-# language LambdaCase, OverloadedStrings, Rank2Types #-} 
module HFish.Interpreter.Builtins.Flow (
  continueF
  ,breakF
  ,returnF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.Args

import Control.Lens


jumpWith :: Lens' FishReader (() -> Fish ()) -> Fish a
jumpWith lensK = (view lensK >>= ($()) ) *> pure undefined


continueF :: Builtin
continueF _ = args0 $ jumpWith continueK

breakF :: Builtin
breakF _ = args0 $ jumpWith breakK

returnF :: Builtin
returnF _ = args0 $ jumpWith returnK


