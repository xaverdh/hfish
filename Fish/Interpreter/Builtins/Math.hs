{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Math (
  mathF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.Util
import Fish.Interpreter.Concurrent
import Fish.Interpreter.Status

import Data.Functor
import Control.Applicative
import Control.Monad
import qualified Data.Char as C
import Data.Foldable (foldl')
import qualified Data.Text as T
import Data.Text.IO as TextIO
import Data.Monoid
import Data.Bifunctor
import Control.Lens
import Control.Monad.IO.Class
import System.Exit
import System.IO

import Data.IntMap as IM
import GHC.Real ((%))

import Data.Scientific hiding (scientific)
import Text.Parser.Combinators
import Text.Parser.Expression
import Data.Attoparsec.Text 
-- import Text.Parser.Token
-- import Text.Parser.Char

mathF :: Bool -> [T.Text] -> Fish ()
mathF _ = \case
  "-t":args -> (echo . unparse) =<< tmathF args
  args -> tmathF args
    >>= extractSfc
    >>= (return . ser)
    >>= echo

ser :: Scientific -> T.Text
ser s = T.pack
  $ either show show (floatingOrInteger s)


tmathF :: [T.Text] -> Fish Math
tmathF = \case
  [] -> errork "math: not enough arguments given"
  ex:args -> compMath ex args

compMath :: T.Text -> [T.Text] -> Fish Math
compMath ex args = do
  e <- parseMath ex
  ms <- forM args parseMath
  xs <- forM ms extractSfc
  eval (length xs) (IM.fromList $ zip [1..] xs) e

extractSfc :: Math -> Fish Scientific
extractSfc = \case
  (Sfc a) -> return a
  m -> errork "math: too few arguments supplied"
  
eval :: Int -> IM.IntMap Scientific -> Math -> Fish Math
eval n im = ev
  where
    ev = \case
      Free i -> return $ maybe (Free (i - n)) Sfc (IM.lookup i im)
      Neg a -> negate <$> ev a
      Abs a -> abs <$> ev a
      Signum a -> signum <$> ev a
      Plus a b -> liftA2 (+) (ev a) (ev b)
      Minus a b -> liftA2 (-) (ev a) (ev b)
      Times a b -> liftA2 (*) (ev a) (ev b)
      Mod a b -> do { x <- ev a; y <- ev b; mathModFish x y }
      Div a b -> liftA2 mathDiv (ev a) (ev b)
      Pow a b -> liftA2 mathPow (ev a) (ev b)
      m -> return m

data Math = 
  Sfc Scientific
  | Free Int
  | Neg Math
  | Abs Math
  | Signum Math
  | Plus Math Math
  | Minus Math Math
  | Times Math Math
  | Div Math Math
  | Pow Math Math
  | Mod Math Math
  deriving (Eq,Ord,Show)

instance Num Math where
  Sfc a + Sfc b = Sfc (a+b)
  a + b = Plus a b
  Sfc a - Sfc b = Sfc (a-b)
  a - b = Minus a b  
  Sfc a * Sfc b = Sfc (a*b)
  a * b = Times a b
  negate (Sfc a) = Sfc (negate a)
  negate a = Neg a
  abs (Sfc a) = Sfc (abs a)
  abs a = Abs a
  signum (Sfc a) = Sfc (signum a)
  signum a = Signum a
  fromInteger = Sfc . fromInteger

instance Fractional Math where
  (/) = Div
  fromRational = undefined

mathModFish (Sfc a) (Sfc b) = Sfc <$> intArith mod a b
mathModFish a b = return (Mod a b)

mathDiv (Sfc a) (Sfc b) = Sfc $ imprecise (/) a b
mathDiv a b = Div a b

mathPow (Sfc a) (Sfc b) = Sfc $ imprecise (**) a b
mathPow a b = Pow a b

intArith :: (Integer -> Integer -> Integer) -> Scientific -> Scientific -> Fish Scientific
intArith f a b = do
  i <- castInt a
  j <- castInt b
  return . fromInteger $ f i j
  where
    castInt a = either
      (const . errork $ "math: expected integer, got " <> T.pack (show a))
      return
      (floatingOrInteger a)

imprecise :: (Double -> Double -> Double) -> Scientific -> Scientific -> Scientific        
imprecise f a b =
  let r = f (toRealFloat a) (toRealFloat b)
   in fromFloatDigits r

{- Parsing -}

prefix s f = Prefix (f <$ string s <* skipSpace)
postfix s f = Postfix (f <$ string s <* skipSpace)
binary s f assoc = Infix (f <$ string s <* skipSpace) assoc

opTable =
  [ [ prefix "+" id, prefix "-" negate ]
   ,[ postfix "++" ((+) (Sfc 1)), postfix "--" ((-) (Sfc 1)) ]
   ,[ binary "^" Pow AssocRight ]
   ,[ binary "*" (*) AssocLeft, binary "/" Div AssocLeft ,binary "%" Mod AssocLeft ]
   ,[ binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ] ]

term :: Parser Math
term = (bracketed <|> sfc <|> free) <* skipSpace
  where
  bracketed = char '(' *> math <* char ')'
  sfc = Sfc <$> scientific
  free = (Free . fromInteger) <$> (char '&' *> decimal)

math :: Parser Math
math = skipSpace *> buildExpressionParser opTable term

parseMath :: T.Text -> Fish Math
parseMath t = either onErr return
  $ parseOnly (math <* endOfInput) t
  where
    onErr err = errork
      "math: malformed expression"

{- Unparse -}

unparse :: Math -> T.Text
unparse = \case
  Sfc s -> ser s
  Free i -> "&" <> showT i
  Neg a -> "-" <> unparse a
  Abs a -> undefined
  Signum a -> undefined
  Plus a b -> unparse a <> " + " <> unparse b
  Minus a b -> unparse a <> " - " <> unparse b
  Times a b -> "(" <> unparse a <> " * " <> unparse b <> ")"
  Div a b -> "(" <> unparse a <> " / " <> unparse b <>  ")"
  Pow a b -> "(" <> unparse a <> " ^ " <> "(" <> unparse b <> ") )"
  Mod a b -> "(" <> unparse a <> " % " <> unparse b <> ")"
  where
    showT ::  Show a => a -> T.Text
    showT = T.pack . show


