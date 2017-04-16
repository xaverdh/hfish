{-# language OverloadedStrings #-}
module HFish.Interpreter.Stringy (
  Stringy(..)
  , IsString(..)
) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.String (IsString(..))
import Text.Read

class IsString s => Stringy s where
  toString :: s -> String -- ^ deprecated
  toText :: s -> T.Text -- ^ deprecated
  toText = T.pack . toString
  fromText :: T.Text -> s -- ^ deprecated
  fromText = fromString . T.unpack
  -- splitWith :: (s -> Bool) -> s -> [s]
  split :: s -> s -> [s]
  intercalate :: s -> [s] -> s
  words :: s -> [s]
  words = split " "
  unwords :: [s] -> s
  unwords = intercalate " "
  lines :: s -> [s]
  lines = split "\n"
  unlines :: [s] -> s
  unlines = intercalate "\n"
  readStr :: Read a => s -> a
  readStr = read . toString
  readStrMaybe :: Read a => s -> Maybe a
  readStrMaybe = readMaybe . toString
  showStr :: Show a => a -> s
  showStr = fromString . show

instance Stringy T.Text where
  toString = T.unpack
  toText = id
  fromText = id
  split = T.splitOn
  intercalate = T.intercalate
  lines = T.lines
  unlines = T.unlines
  words = T.words
  unwords = T.unwords

instance Stringy B.ByteString where
  toString = BC.unpack
  split = splitBs
  intercalate = B.intercalate

splitBs :: B.ByteString -> B.ByteString -> [B.ByteString]
splitBs s = splitOff
  where
    l = B.length s
    splitOff bs
      | B.null bs = []
      | True = let (hd,tl) = B.breakSubstring s bs
                in hd : splitOff (B.drop l tl)

