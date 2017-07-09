{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Emit (
  emitF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.Events
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Sequence as Seq
import Data.NText
import Control.Monad


emitF :: Builtin
emitF _ = argsFrom 1 $ \case
  name:ts -> handleEvent (mkNText $ Str.toText name) (Seq.fromList ts)

