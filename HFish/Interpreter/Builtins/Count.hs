module HFish.Interpreter.Builtins.Count (
  countF
) where

import HFish.Interpreter.Core

countF :: Builtin
countF _  = echo . show . length
