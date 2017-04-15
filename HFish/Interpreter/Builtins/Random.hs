{-# language LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module HFish.Interpreter.Builtins.Random (
  randomF
) where


import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Functor
import Control.Monad.IO.Class
import System.Exit
import System.Random
import Data.Word (Word16)

randomF :: Builtin
randomF _ = \case
  [] -> do
    r :: Word16 <- liftIO randomIO
    echo (T.pack $ show (div r 2))
    -- we use Word16 and (div r 2) for fish compat.
    ok
  _ -> errork "random: too many arguments given"
