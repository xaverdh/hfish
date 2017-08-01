{-# language LambdaCase, GADTs, OverloadedStrings, TupleSections #-}
module HFish.Interpreter.Builtins.Seq (
  seqF
) where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.IO.Lazy
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Process.Process
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder as B
import Text.Read
import Data.Functor
import Data.Semigroup
import System.Exit
import System.IO

seqF :: Builtin
seqF fork = argsRange 1 3 $ \case
  [l] -> seqFWorker fork ((1,1,) <$> mread l)
  [f,l] -> seqFWorker fork ((,1,) <$> mread f <*> mread l)
  [f,i,l] -> seqFWorker fork ((,,) <$> mread f <*> mread i <*> mread l)
  where
    mread = Str.readStrMaybe

seqFWorker :: Bool -> Maybe (Int,Int,Int) -> Fish ()
seqFWorker fork = \case
    Nothing -> errork "seq: invalid argument(s) given"
    Just (a,b,c) -> do
      if fork
        then forkFish (writeList a b c) >> ok
        else writeList a b c >> ok

writeList :: Int -> Int -> Int -> Fish ()
writeList a b c
  | b == 1 && c `div` b > 10^5 =
    fishCreateProcess "seq" (map show [a,b,c])
    >>= fishWaitForProcess "seq"
  | otherwise = echo
      . B.toLazyByteString
      $ createList a b c

createList :: Int -> Int -> Int -> B.Builder
createList a b c = BP.primUnfoldrBounded intLn next a
  where
    next i = do
      guard (i <= c)
      Just ((i,'\n'),i+b)
    intLn = BP.intDec BP.>*< BP.charUtf8



