{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Test (
  testF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Util
import HFish.Interpreter.Status

import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Text as T

import System.Posix.Types
import System.Posix.Files
import System.Posix.User
import System.Posix.Terminal (queryTerminal)

-- import Text.Parser.Combinators
-- import Text.Parser.Token
import Text.Parser.Expression
import Data.Attoparsec.Text
import Text.Parser.Char (alphaNum)

testF :: Bool -> [T.Text] -> Fish ()
testF _ ts = do
  f <- parseTestE (T.unwords ts)
  b <- liftIO f
  if b then ok else  bad

{- File Operations -}
hasMode ::
  FileMode -- the mode to check for
  -> FileMode -- the mode to be checked
  -> Bool
hasMode m1 m2 =
  (m2 `intersectFileModes` m1) == m1

hasGidSet :: FileStatus -> Bool
hasGidSet =
  hasMode setGroupIDMode
  . fileMode

hasUidSet :: FileStatus -> Bool
hasUidSet =
  hasMode setUserIDMode
  . fileMode

isUserOwned :: FileStatus -> IO Bool
isUserOwned stat = 
  (==fileOwner stat)
  <$> getEffectiveUserID
  
isUserGroup :: FileStatus -> IO Bool
isUserGroup stat =
  (==fileGroup stat)
  <$> getEffectiveGroupID

isTtyFd :: Int -> IO Bool
isTtyFd =
  queryTerminal
  . toEnum

isReadable :: FilePath -> IO Bool
isReadable p =
  fileAccess p True False False

isWriteable :: FilePath -> IO Bool
isWriteable p =
  fileAccess p False True False

isExecutable :: FilePath -> IO Bool
isExecutable p =
  fileAccess p False False True

{- Main Parsers -}

fileE :: Parser (IO Bool)
fileE = file1E <|> file2E <|> file3E <|> file4E
  where
    file = some alphaNum

    file4E = do
      f <- sym "-L" $> isSymbolicLink
      p <- file
      return (f <$> getSymbolicLinkStatus p)

    file1E = do
      f <- choice
        [ sym "-b" $> isBlockDevice
         ,sym "-c" $> isCharacterDevice
         ,sym "-d" $> isDirectory
         ,sym "-f" $> isRegularFile
         ,sym "-S" $> isSocket
         ,sym "-p" $> isNamedPipe
         ,sym "-s" $> fmap (>0) fileSize
         ]
      p <- file
      return (f <$> getFileStatus p)
     
    file2E = do
      f <- choice
        [ sym "-e" $> fileExist
         ,sym "-r" $> isReadable
         ,sym "-w" $> isWriteable
         ,sym "-x" $> isExecutable ]
      p <- file
      return (f p)

    file3E = do
      f <- choice
        [ sym "-g" $> (return . hasGidSet)
         ,sym "-u" $> (return . hasUidSet)
         ,sym "-G" $> isUserGroup
         ,sym "-O" $> isUserOwned
         ]
      p <- file
      return (f =<< getFileStatus p)

strE :: Parser Bool
strE = strBinary <|> strUnary
  where
    str = some alphaNum
    strBinary = do
      s <- str
      f <- choice
        [ sym "=" $> (==)
          ,sym "!=" $> (/=) ]
      f s <$> str
    
    strUnary = do
      f <- choice
        [ sym "-n" $> (/="")
         ,sym "-z" $> (=="") ]
      f <$> str

ttyTestE :: Parser (IO Bool)
ttyTestE = "-t" *> int
  >>= (return . isTtyFd)

numE :: Parser Bool
numE = do
  i <- int
  f <- choice
    [ sym "-eq" $> (==)
     ,sym "-ne" $> (/=)
     ,sym "-gt" $> (>)
     ,sym "-ge" $> (>=)
     ,sym "-lt" $> (<)
     ,sym "-le" $> (<=) ]
  f i <$> int

{- Parsing -}

prefix s f = Prefix (f <$ sym s <* skipSpace)
postfix s f = Postfix (f <$ sym s <* skipSpace)
binary s f assoc = Infix (f <$ sym s <* skipSpace) assoc

opTable =
  [ [ prefix "!" (fmap not) ]
   ,[ binary "-a" (liftM2 (&&)) AssocRight ]
   ,[ binary "-o" (liftM2 (||)) AssocLeft ] ]

term :: Parser (IO Bool)
term =
  ( bracketed
    <|> (return <$> numE)
    <|> (return <$> strE)
    <|> fileE )
  <* skipSpace
  where
  bracketed = char '(' *> testE <* char ')'

testE :: Parser (IO Bool)
testE = skipSpace *> buildExpressionParser opTable term

parseTestE :: T.Text -> Fish (IO Bool)
parseTestE t = either onErr return
  $ parseOnly (testE <* endOfInput) t
  where
    onErr _ = 
      errork "test: malformed expression"

int :: Parser Int
int = signed decimal <* skipSpace

sym :: T.Text -> Parser ()
sym s = string s *> skipSpace

