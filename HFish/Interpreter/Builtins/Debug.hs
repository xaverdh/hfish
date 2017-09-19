{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Debug (
  debugF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.FdTable

import Control.Monad
import Control.Monad.IO.Class

debugF :: Builtin
debugF _ = \case
  "fdtable":_ -> askFdTable >>= (liftIO . print)
  _ -> pure ()
  
