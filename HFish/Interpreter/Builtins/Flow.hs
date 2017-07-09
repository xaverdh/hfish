{-# language LambdaCase, OverloadedStrings, Rank2Types #-} 
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


jumpWith :: Lens' FishReader (() -> Fish ()) -> Fish a
jumpWith lensK = (view lensK >>= ($()) ) >> pure undefined

tooManyArgsErr :: String -> Fish a
tooManyArgsErr name = errork $
  name <> ": Too many arguments given."

continueF :: Builtin
continueF _ = \case
  [] -> jumpWith continueK
  _ -> tooManyArgsErr "continue"

breakF :: Builtin
breakF _ = \case
  [] -> jumpWith breakK
  _ -> tooManyArgsErr "break"

returnF :: Builtin
returnF _ = \case
  [] -> jumpWith returnK
  _ -> tooManyArgsErr "return"


