module HFish.Utils where

(<$$>) :: Applicative f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)


