{-# language LambdaCase, TupleSections #-}
module Fish.Interpreter.Util where

import Data.Bifunctor
import Text.Read
import Data.Text as T


onMaybe :: Maybe a -> b -> (a -> b) -> b
onMaybe ma b f = maybe b f ma

splitAtMaybe :: Int -> [a] -> Maybe ([a],[a])
splitAtMaybe i
  | i < 0 = const Nothing
  | i == 0 = Just . ([],)
  | otherwise = \case
    [] -> Nothing
    x:xs -> first (x:)
      <$> splitAtMaybe (i-1) xs

readText :: Read a => T.Text -> a
readText = read . T.unpack

readTextMaybe :: Read a => T.Text -> Maybe a
readTextMaybe = readMaybe . T.unpack

readTextsMaybe :: Read a => [T.Text] -> Maybe a
readTextsMaybe = readTextMaybe . T.unwords

showText :: Show a => a -> T.Text
showText = T.pack . show

