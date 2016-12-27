module HFish.Interpreter.Builtins.Count (
  countF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Util

countF :: Builtin
countF _  = echoLn . showText . length
