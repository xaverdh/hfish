{-# language LambdaCase #-}
module HFish.Interpreter.Args
  ( args0
  , args1
  , args2
  , args3
  , args4
  , argsRange
  , argsFrom
  , argsUpto
  , argsChoice
  , argsCond )
where

import HFish.Interpreter.Core

import Data.Monoid
import qualified Data.List as L
import qualified Data.IntSet as S

args0 :: Fish b -> [a] -> Fish b
args0 f = \case
  [] -> f
  _ -> exactArgsErr 0

args1 :: (a -> Fish b) -> [a] -> Fish b
args1 f = \case
  [a] -> f a
  _ -> exactArgsErr 1

args2 :: (a -> a -> Fish b) -> [a] -> Fish b
args2 f = \case
  [a1,a2] -> f a1 a2
  _ -> exactArgsErr 2

args3 :: (a -> a -> a -> Fish b) -> [a] -> Fish b
args3 f = \case
  [a1,a2,a3] -> f a1 a2 a3
  _ -> exactArgsErr 3

args4 :: (a -> a -> a -> a -> Fish b) -> [a] -> Fish b
args4 f = \case
  [a1,a2,a3,a4] -> f a1 a2 a3 a4
  _ -> exactArgsErr 4


argsRange :: Int -> Int -> ([a] -> Fish b) -> [a] -> Fish b
argsRange i j = argsCond
  ("between " <> show i <> " and " <> show j <> " arguments.")
  (\n -> i <= n && n <= j)

argsUpto :: Int -> ([a] -> Fish b) -> [a] -> Fish b
argsUpto j = argsCond
  ("up to " <> show j <> "arguments.")
  (<= j)

argsFrom :: Int -> ([a] -> Fish b) -> [a] -> Fish b
argsFrom i = argsCond
  ("at least " <> show i <> "arguments.")
  (>= i)

argsChoice :: [Int] -> ([a] -> Fish b) -> [a] -> Fish b
argsChoice choice = argsCond msg (`S.member` choiceSet)
  where
    msg = "number of arguments to be one of "
      <> (L.intercalate "," $ map show choice)
    choiceSet = S.fromList choice

argsCond :: String
  -> (Int -> Bool)
  -> ([a] -> Fish b)
  -> [a] -> Fish b
argsCond errMsg cond f xs
  | cond (length xs) = f xs
  | True = errork $
    "Invalid number of arguments given; expected "
    <> errMsg


exactArgsErr :: Int ->  Fish a
exactArgsErr n = errork
  $ "Invalid number of arguments given; expected "
  <> show n <> " arguments."





