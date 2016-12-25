module HFish.Interpreter.IsText where

import qualified Data.Text as T

class IsText a where
  toText :: a -> T.Text
  fromText :: T.Text -> a

instance IsText T.Text where
  toText = id
  fromText = id


