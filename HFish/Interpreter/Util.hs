{-# language LambdaCase, TupleSections #-}
module HFish.Interpreter.Util where

import Data.Sequence
import Data.Bifunctor
import Text.Read
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence

import Data.Semigroup
import Control.Monad.IO.Class


mintcal :: (Foldable t,Semigroup m,Monoid m) => m -> t m -> m
mintcal m ms = 
  if F.null ms then mempty else
  F.foldr1 (\x y -> x <> m <> y) ms

infixl 4 <$$>
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
-- ^ A flipped version of (<$>)

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) b x y = if b then x else y
-- ^ A proper /if/ function

whenJust :: Applicative f
  => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())

onMaybe :: Maybe a -> b -> (a -> b) -> b
onMaybe ma b f = maybe b f ma

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither ma b = maybe (Left b) Right ma

splitAtMaybe :: Int -> Seq a -> Maybe (Seq a,Seq a)
splitAtMaybe i xs
  | i < 0 || i > Seq.length xs = Nothing
  | True = Just $ Seq.splitAt i xs

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

