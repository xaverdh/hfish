{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Math (
  mathF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Util
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str

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
import Data.Attoparsec.ByteString.Char8
-- import Text.Parser.Token
-- import Text.Parser.Char

mathF :: Builtin
mathF _ = \case
  [] -> errork "math: not enough arguments given"
  args ->
    compMath (Str.unwords args)
    >>= (return . ser)
    >>= echo

ser :: Scientific -> Str
ser s = Str.fromString
  $ either show show (floatingOrInteger s)

compMath :: Str -> Fish Scientific
compMath ex = parseMath ex >>= eval

eval :: Math -> Fish Scientific
eval = \case
  Sfc a -> return a
  Neg a -> negate <$> eval a
  Abs a -> abs <$> eval a
  Signum a -> signum <$> eval a
  Plus a b -> liftA2 (+) (eval a) (eval b)
  Minus a b -> liftA2 (-) (eval a) (eval b)
  Times a b -> liftA2 (*) (eval a) (eval b)
  Mod a b -> do { x <- eval a; y <- eval b; mathModFish x y }
  Div a b -> liftA2 mathDiv (eval a) (eval b)
  Pow a b -> liftA2 mathPow (eval a) (eval b)

data Math = 
  Sfc Scientific
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


mathModFish a b = intArith mod a b

mathDiv a b = imprecise (/) a b

mathPow a b = imprecise (**) a b

intArith :: (Integer -> Integer -> Integer)
  -> Scientific
  -> Scientific
  -> Fish Scientific
intArith f a b = do
  i <- castInt a
  j <- castInt b
  return . fromInteger $ f i j
  where
    castInt a = either
      (const . errork $ "math: expected integer, got " <> show a)
      return
      (floatingOrInteger a)

imprecise :: (Double -> Double -> Double)
  -> Scientific
  -> Scientific
  -> Scientific        
imprecise f a b =
  f (toRealFloat a) (toRealFloat b)
  & fromFloatDigits

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
term = ( bracketed <|> sfc ) <* skipSpace
  where
    bracketed = char '(' *> math <* char ')'
    sfc = Sfc <$> scientific

math :: Parser Math
math = skipSpace *> buildExpressionParser opTable term

parseMath :: Str -> Fish Math
parseMath t = either onErr return
  $ parseOnly (math <* endOfInput) t
  where
    onErr err = errork
      "math: malformed expression"

{- Unparse -}

unparse :: Math -> Str
unparse = \case
  Sfc s -> ser s
  Neg a -> "-" <> unparse a
  Abs a -> undefined
  Signum a -> undefined
  Plus a b -> unparse a <> " + " <> unparse b
  Minus a b -> unparse a <> " - " <> unparse b
  Times a b -> "(" <> unparse a <> " * " <> unparse b <> ")"
  Div a b -> "(" <> unparse a <> " / " <> unparse b <>  ")"
  Pow a b -> "(" <> unparse a <> " ^ " <> "(" <> unparse b <> ") )"
  Mod a b -> "(" <> unparse a <> " % " <> unparse b <> ")"



