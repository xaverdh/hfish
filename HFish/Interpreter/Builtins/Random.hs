{-# language LambdaCase, OverloadedStrings #-}
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

randomF :: Bool -> [T.Text] -> Fish ()
randomF _ = \case
  [] -> do
    r <- liftIO randomIO
    echo (T.pack $ show (div r 2 :: Word16))
    -- we use Word16 for fish compat.
    ok
  _ -> errork "random: too many arguments given"