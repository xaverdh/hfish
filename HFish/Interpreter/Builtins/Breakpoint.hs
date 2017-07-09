{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Breakpoint (
  breakpointF
) where


import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Args

import Fish.Lang
import Control.Monad

breakpointF :: Builtin
breakpointF _ = args0 $ do
    warn "Entering breakpoint now. Press ^D to leave.\n"
    setBreakpoint

