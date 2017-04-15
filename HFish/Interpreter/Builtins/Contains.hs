{-# language LambdaCase, GADTs, OverloadedStrings, TupleSections #-}
module HFish.Interpreter.Builtins.Contains (
  containsF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Util

import qualified Data.Text as T
import Data.List as L
import Data.Bool
import Data.Functor

containsF :: Builtin
containsF _ = \case
  [] -> errork "contains: too few arguments given"
  "i":"--":key:vals ->
    containsI key vals
  "i":key:vals -> 
    containsI key vals
  "--":key:vals -> 
    bool bad ok (key `elem` vals)
  key:vals -> 
    bool bad ok (key `elem` vals)
  where
    containsI key vals = 
      case L.findIndex (==key) vals of
        Just i -> do
          echo (showText i)
          ok
        Nothing -> bad
