{-# language LambdaCase, TupleSections #-}
module HFish.Interpreter.Util where

import Data.Sequence
import Data.Bifunctor
import Text.Read
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Monad.IO.Class


mintcal :: (Foldable t,Monoid m) => m -> t m -> m
mintcal m ms = 
  if F.null ms then mempty else
  F.foldr1 (\x y -> x <> m <> y) ms

collapse :: (Traversable t,Monoid m)
  => t m -> m
collapse = F.foldr (<>) mempty

infixl 4 <$$>
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
-- ^ A flipped version of (<$>)

whenJust :: Applicative f
  => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())

onMaybe :: Maybe a -> b -> (a -> b) -> b
onMaybe ma b f = maybe b f ma

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither ma b = maybe (Left b) Right ma

splitAtMaybe :: Int -> Seq a -> Maybe (Seq a,Seq a)
splitAtMaybe i xs
  | i < 0 = Nothing
  | i == 0 = Just (empty,xs)
  | True = case viewl xs of
    EmptyL -> Nothing
    x :< xs -> first (x <|)
      <$> splitAtMaybe (i-1) xs

readText :: Read a => T.Text -> a
readText = read . T.unpack

readTextMaybe :: Read a => T.Text -> Maybe a
readTextMaybe = readMaybe . T.unpack

readTextsMaybe :: Read a => [T.Text] -> Maybe a
readTextsMaybe = readTextMaybe . T.unwords

showText :: Show a => a -> T.Text
showText = T.pack . show

showCall :: (MonadIO m,Show a) => String -> [a] -> m ()
showCall f args =
  ( liftIO . putStrLn )
    ( f <> " " <> mintcal " " (map show args) )

