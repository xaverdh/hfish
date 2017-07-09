{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Printf (
  printfF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str
import Fish.Lang

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Data.Text.IO as TextIO
import Data.Functor
import Data.Monoid
import Data.Bool
import Data.Char (ord,chr)
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E
import qualified Control.DeepSeq as DeepSeq
import System.Exit
import System.IO

import Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.Combinator
import Text.Parser.Char (hexDigit,octDigit)

import Text.Printf


printfF :: Builtin
printfF _ = argsFrom 1 $ \(t:ts) ->
  printWorker t ts

printWorker :: Str -- ^ The format
  -> [Str] -- ^ Arguments
  -> Fish ()
printWorker fmt args = do
  parts <- specifiers fmt
  let ts = combine args (join $ repeat parts)
  res <- liftIO $ E.try (E.evaluate $ DeepSeq.force ts)
  either onErr (echoLn . Str.intercalate "") res
  where
    onErr :: E.ErrorCall -> Fish ()
    onErr _ = errork "printf: invalid argument(s)"
    
    combine :: [Str] -> [Either Specifier Str] -> [Str]
    combine args (part:rest) =
      case part of
        Right t -> t : combine args rest
        Left SpecEnd -> []
        Left spec -> case args of
          [] -> insertEmpty spec : combine [] rest
          x:xs -> insert x spec : combine xs rest
    
    insert :: Str -> Specifier -> Str
    insert arg SpecS = arg
    insert arg spec = Str.fromString $ case spec of
      SpecD -> printf "%d" (Str.readStr arg :: Integer)
      SpecI -> printf "%i" (Str.readStr arg :: Integer)
      SpecO -> printf "%o" (Str.readStr arg :: Integer)
      SpecU -> printf "%u" (Str.readStr arg :: Int)
      SpecX -> printf "%x" (Str.readStr arg :: Integer)
      SpecF -> printf "%.6f" (Str.readStr arg :: Double)
      SpecE -> printf "%.6E" (Str.readStr arg :: Double)
      SpecB -> error "%b is not supported"
      
    insertEmpty :: Specifier -> Str
    insertEmpty = Str.fromString . \case
      SpecD -> printf "%d" (0::Integer)
      SpecI -> printf "%i" (0::Integer)
      SpecO -> printf "%o" (0::Integer)
      SpecU -> printf "%u" (0::Int)
      SpecX -> printf "%x" (0::Integer)
      SpecF -> printf "%.6f" (0::Double)
      SpecE -> printf "%.6E" (0::Double)
      SpecS -> ""
      SpecB -> ""
      

data Specifier =
  SpecD | SpecI | SpecO
  | SpecU | SpecX | SpecF
  | SpecE | SpecS | SpecB
  | SpecEnd
  deriving Show

specifiers :: Str -> Fish [Either Specifier Str]
specifiers t = either onErr finish
  $ parseOnly (many formatPart <* endOfInput) t
  where
    finish xs = do
      -- liftIO $ print xs -- for debugging
      return $ xs ++ [Left SpecEnd]
    
    onErr _ = errork $
      "printf: malformed escape sequence"


formatPart :: Atto.Parser (Either Specifier Str)
formatPart = spec <|> Right <$> plain

spec :: Atto.Parser (Either Specifier Str)
spec = char '%' *> choice
  [ Right <$> string "%"
    ,char 'd' $> Left SpecD
    ,char 'i' $> Left SpecI
    ,char 'o' $> Left SpecO
    ,char 'u' $> Left SpecU
    ,char 'x' $> Left SpecX
    ,char 'X' $> Left SpecX
    ,char 'f' $> Left SpecF
    ,char 'g' $> Left SpecF
    ,char 'G' $> Left SpecF
    ,char 'e' $> Left SpecE
    ,char 'E' $> Left SpecE
    ,char 's' $> Left SpecS
    ,char 'b' $> Left SpecB ]

plain :: Atto.Parser Str
plain = takeWhile1 (\c -> c /= '\\' && c /= '%')
  <|> char '\\' *> choice
      [ escaped
        ,cancel
        ,oct
        ,hex
        ,uni16
        ,uni32
        ,return "\\" ]
  where    
    escaped = choice
      [ string "\\"
        ,string "\"" -- ^ idiotic but thats what the manpage says
        ,char 'a' $> "\a"
        ,char 'b' $> "\b"
        ,char 'e' $> "\ESC"
        ,char 'f' $> "\f"
        ,char 'n' $> "\n"
        ,char 'r' $> "\r"
        ,char 't' $> "\t"
        ,char 'v' $> "\v" ]
    
    cancel = char 'c' *> takeLazyByteString *> return ""
    
    oct = try $ fromDigits 8 <$> count 3 octDigit
    
    hex = char 'x' *>
      (fromDigits 16 <$> count 2 hexDigit)
    
    uni16 = char 'u' *> 
      (fromDigits 16 <$> count 4 hexDigit)
    
    uni32 = char 'U' *> 
      (fromDigits 16 <$> count 8 hexDigit)
    
    ctoi c = ord c - 48
    
    compChar :: Int -> [Int] -> Str
    compChar base = 
      BC.singleton . chr
      . foldr (\x y -> y + base * x) 0
    
    fromDigits :: Int -> [Char] -> Str
    fromDigits base = compChar base . map ctoi
