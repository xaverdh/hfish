{-# language LambdaCase, GADTs, OverloadedStrings, TupleSections #-}
module Fish.Interpreter.Builtins.Contains (
  containsF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.IO
import Fish.Interpreter.Status
import Fish.Interpreter.Util

import qualified Data.Text as T
import Data.List as L
import Data.Bool
import Data.Functor

containsF :: Bool -> [T.Text] -> Fish ()
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

containsI :: T.Text -> [T.Text] -> Fish ()
containsI key vals = 
  case L.findIndex (==key) vals of
    Just i -> do
      echo (showText i)
      ok
    Nothing -> bad
