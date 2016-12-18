{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Echo (
  echoF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Util
import Fish.Lang

import qualified Data.Text as T
import Data.Text.IO as TextIO
import Data.Functor
import Data.Monoid
import Data.Bool
import Data.Char (ord,chr)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.IO

import Data.Attoparsec.Text as Atto
import Text.Parser.Char (hexDigit,octDigit)

import Options.Applicative
import Options.Applicative.Builder as OB


echoF :: Bool -> [T.Text] -> Fish ()
echoF _ ts =
  execParserPure defaultPrefs parser (map T.unpack ts)
  & \case
    Success f -> f
    Failure err -> errork . T.pack . fst
      $ renderFailure err "read: invalid arguments given\n"
  where
    parser = info echoOptions idm

echoOptions = echoWorker
  <$> ( flag False True (short 'e')
        <|> flag False False (short 'E') )
  <*> switch (short 's')
  <*> switch (short 'n')
  <*> many ( OB.argument text (metavar "STRINGS...") )
  where
    text = maybeReader (Just . T.pack)

echoWorker :: Bool -- ^ Interpret escapes
  -> Bool -- ^ No space
  -> Bool -- ^ No newline
  -> [T.Text] -- ^ Arguments
  -> Fish ()
echoWorker esc noSpace noNewl args =
  T.intercalate (bool " " "" noSpace) args
  & bool return escape esc
  >>= \t -> bool echoLn echo noNewl t >> ok


escape :: T.Text -> Fish T.Text
escape t = either onErr return
  $ parseOnly (escaper <* endOfInput) t
  where
    onErr _ = errork $
      "echo: malformed escape sequence"

escaper :: Atto.Parser T.Text
escaper = do
  t1 <- takeTill (=='\\')
  end <- atEnd
  if end
    then return t1
    else do
      t2 <- char '\\' *> ( escaped <|> cancel <|> oct <|> hex )
      t3 <- escaper
      return $ t1 <> t2 <> t3
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
    
    cancel = char 'c' *> takeLazyText *> return ""
    
    oct = char '0' *> do
      d1 <- octDigit
      d2 <- octDigit
      d3 <- octDigit
      return $ compChar 8 $ map ctoi [d1,d2,d3]
    
    hex = char 'x' *> do
      d1 <- hexDigit
      d2 <- hexDigit
      return $ compChar 16 $ map ctoi [d1,d2]
    
    ctoi c = ord c - 48
    
    compChar :: Int -> [Int] -> T.Text
    compChar base = 
      T.singleton . chr
      . foldr (\x y -> y + base * x) 0
      
      
