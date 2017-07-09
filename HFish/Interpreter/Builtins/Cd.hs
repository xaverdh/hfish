{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Cd (
  cdF
) where

import HFish.Interpreter.Cwd
import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Args


cdF :: Builtin
cdF fork = argsChoice [0,1] $ \case
  [] -> do
    home <- getHOME
    cdF fork [home]
    ok
  [dir] -> setCWD dir >>= setStatus

