{-# language OverloadedStrings, MultiParamTypeClasses #-}
{-# language ConstraintKinds, FunctionalDependencies #-}
{-# language FlexibleContexts #-}
module HFish.Interpreter.Stringy (
  StringLike(..)
  , Stringy(..)
  , IsString(..)
) where

import Prelude hiding (span,break,length,drop,take,takeWhile,dropWhile)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.String (IsString(..))
import Text.Read

type Stringy s = StringLike s Char

class (Eq c,Eq s,Monoid s,IsString s) => StringLike s c | s -> c where
  singleton :: c -> s

  toString :: s -> String

  toText :: s -> T.Text
  toText = T.pack . toString

  fromText :: T.Text -> s
  fromText = fromString . T.unpack

  splitWith :: (c -> Bool) -> s -> [s]
  
  split :: c -> s -> [s]
  split c = splitOn (singleton c)
  
  splitOn :: s -> s -> [s]
  splitAt :: Int -> s -> (s,s)

  intercalate :: s -> [s] -> s

  words :: s -> [s]
  words = splitOn " "

  unwords :: [s] -> s
  unwords = intercalate " "

  lines :: s -> [s]
  lines = splitOn "\n"

  unlines :: [s] -> s
  unlines = intercalate "\n"

  length :: s -> Int
  null :: s -> Bool
  empty :: s
  
  take :: Int -> s -> s
  drop :: Int -> s -> s

  takeEnd :: Int -> s -> s
  takeEnd i = do
    l <- length
    drop (l-i)

  dropEnd :: Int -> s -> s
  dropEnd i = do
    l <- length
    take (l-i)
  
  find :: (c -> Bool) -> s -> Maybe c
  
  span :: (c -> Bool) -> s -> (s,s)
  span p = break (not . p)
  break :: (c -> Bool) -> s -> (s,s)
  break p = span (not . p)

  spanEnd :: (c -> Bool) -> s -> (s,s)
  spanEnd p = breakEnd (not . p)
  breakEnd :: (c -> Bool) -> s -> (s,s)
  breakEnd p = spanEnd (not . p)

  takeWhile :: (c -> Bool) -> s -> s
  takeWhile p = fst . span p
  
  dropWhile :: (c -> Bool) -> s -> s
  dropWhile p = snd . span p
  
  dropWhileEnd :: (c -> Bool) -> s -> s
  dropWhileEnd p = snd . spanEnd p

  takeWhileEnd :: (c -> Bool) -> s -> s
  takeWhileEnd p = fst . spanEnd p
  
  dropAround :: (c -> Bool) -> s -> s
  dropAround p = dropWhileEnd p . dropWhile p

  reverse :: s -> s

  readStr :: Read a => s -> a
  readStr = read . toString

  readStrMaybe :: Read a => s -> Maybe a
  readStrMaybe = readMaybe . toString

  showStr :: Show a => a -> s
  showStr = fromString . show

instance StringLike T.Text Char where
  singleton = T.singleton
  toString = T.unpack
  toText = id
  fromText = id
  splitWith = T.split
  splitOn = T.splitOn
  splitAt = T.splitAt
  intercalate = T.intercalate
  lines = T.lines
  unlines = T.unlines
  words = T.words
  unwords = T.unwords
  length = T.length
  null = T.null
  empty = T.empty
  take = T.take
  drop = T.drop
  dropEnd = T.dropEnd
  find = T.find
  span = T.span
  break = T.break
  spanEnd p s = (takeWhile p s,dropWhile p s)
  dropWhile = T.dropWhile
  takeWhile = T.takeWhile
  dropWhileEnd = T.dropWhileEnd
  takeWhileEnd = T.takeWhileEnd
  dropAround = T.dropAround

  reverse = T.reverse

instance StringLike B.ByteString Char where
  singleton = BC.singleton
  toString = T.unpack . decodeUtf8
  toText = decodeUtf8
  fromText = encodeUtf8
  splitWith = BC.splitWith
  split = BC.split
  splitOn = splitOnBs
  splitAt = B.splitAt
  intercalate = B.intercalate
  length = B.length
  null = B.null
  empty = B.empty
  take = B.take
  drop = B.drop
  find = BC.find
  span = BC.span
  break = BC.break
  spanEnd = BC.spanEnd
  breakEnd = BC.breakEnd
  takeWhile = BC.takeWhile
  dropWhile = BC.dropWhile
  reverse = B.reverse

splitOnBs :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOnBs s = splitOff
  where
    l = B.length s
    splitOff bs
      | B.null bs = []
      | True = let (hd,tl) = B.breakSubstring s bs
                in hd : splitOff (B.drop l tl)

