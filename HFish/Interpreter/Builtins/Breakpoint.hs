{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Breakpoint (
  breakpointF
) where


import HFish.Interpreter.Core
import qualified Data.Text as T
import HFish.Lang.Lang
import Control.Monad

breakpointF :: Builtin
breakpointF _ = \case
  [] -> setBreakpoint
  _ -> errork "breakpoint: too many arguments given"
