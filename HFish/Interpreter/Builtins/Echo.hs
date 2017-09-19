{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Echo (
  echoF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import qualified HFish.Interpreter.Stringy as Str
import Fish.Lang

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Data.Text.IO as TextIO
import Data.Functor
import Data.Semigroup
import Data.Bool
import Data.Char (ord,chr)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.IO

import Data.Attoparsec.ByteString.Char8 as Atto
import Text.Parser.Char (hexDigit,octDigit)

import Options.Applicative
import Options.Applicative.Builder as OB


echoF :: Builtin
echoF _ ts =
  let (mbOpts,rest) = splitAt 5 ts 
  in execParserPure defaultPrefs
    (parser rest) (map Str.toString mbOpts)
  & \case
    Success f -> f
    Failure err -> errork . fst
      $ renderFailure err "read: invalid arguments given\n"
  where
    parser rest = info (echoOptions rest) idm

echoOptions rest = echoWorker
  <$> ( flag False True (short 'e')
        <|> flag False False (short 'E') )
  <*> switch (short 's')
  <*> switch (short 'n')
  <*> (fmap (<>rest) . many) ( OB.argument text (metavar "STRINGS...") )
  where
    text = maybeReader (Just . Str.fromString)

echoWorker :: Bool -- ^ Interpret escapes
  -> Bool -- ^ No space
  -> Bool -- ^ No newline
  -> [Str] -- ^ Arguments
  -> Fish ()
echoWorker esc noSpace noNewl args =
  Str.intercalate (bool " " "" noSpace) args
  & bool pure escape esc
  >>= \t -> bool echoLn echo noNewl t >> ok


escape :: Str -> Fish Str
escape t = either onErr pure
  $ parseOnly (escaper <* endOfInput) t
  where
    onErr _ = errork $
      "echo: malformed escape sequence"

escaper :: Atto.Parser Str
escaper = do
  t1 <- takeTill (=='\\')
  end <- atEnd
  if end
    then pure t1
    else do
      t2 <- char '\\' *> ( escaped <|> cancel <|> oct <|> hex )
      t3 <- escaper
      pure $ t1 <> t2 <> t3
  where
    escaped = choice
      [ string "\\"
        ,char 'a' $> "\a"
        ,char 'b' $> "\b"
        ,char 'e' $> "\ESC"
        ,char 'f' $> "\f"
        ,char 'n' $> "\n"
        ,char 'r' $> "\r"
        ,char 't' $> "\t"
        ,char 'v' $> "\v" ]
    
    cancel = char 'c' *> takeLazyByteString *> pure ""
    
    oct = char '0' *> do
      d1 <- octDigit
      d2 <- octDigit
      d3 <- octDigit
      pure $ compChar 8 $ map ctoi [d1,d2,d3]
    
    hex = char 'x' *> do
      d1 <- hexDigit
      d2 <- hexDigit
      pure $ compChar 16 $ map ctoi [d1,d2]
    
    ctoi c = ord c - 48
    
    compChar :: Int -> [Int] -> Str
    compChar base = 
      BC.singleton . chr
      . foldr (\x y -> y + base * x) 0
      
      
