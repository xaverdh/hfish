{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Breakpoint (
  breakpointF
) where


import HFish.Interpreter.Core
import HFish.Interpreter.IO

import qualified Data.Text as T
import HFish.Lang.Lang
import Control.Monad

breakpointF :: Builtin
breakpointF _ = \case
  [] -> do
    warn "Entering breakpoint now. Press ^D to leave.\n"
    setBreakpoint
  _ -> errork "breakpoint: too many arguments given"
