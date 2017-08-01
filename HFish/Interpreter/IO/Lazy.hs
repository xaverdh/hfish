{-# language FlexibleContexts #-}
module HFish.Interpreter.IO.Lazy where

import HFish.Interpreter.Util
import HFish.Interpreter.FdTable
import HFish.Interpreter.Core
import qualified Fish.Lang as L

import System.Unix.ByteString.Class
import System.Unix.IO.Lazy

import Data.String (IsString(..))
import Data.Semigroup
import qualified System.Posix.Types as PT
-- import qualified System.Posix.IO as P
-- import qualified Data.Text as T


readFrom :: FromLazyByteString Fish a => L.Fd -> Fish a
readFrom fd = do
  pfd <- lookupFd' fd
  fdGetContents pfd

readLineFrom :: FromLazyByteString Fish a => L.Fd -> Fish a
readLineFrom fd = do
  pfd <- lookupFd' fd
  fdGetLine pfd

writeTo :: ToLazyByteString Fish a => L.Fd -> a -> Fish ()
writeTo fd text = do
  pfd <- lookupFd' fd
  fdPut pfd text

echo :: ToLazyByteString Fish a => a -> Fish ()
echo = writeTo L.Fd1

echoLn :: (Semigroup a,IsString a,ToLazyByteString Fish a) => a -> Fish ()
echoLn t = echo (t <> fromString "\n")


lookupFd' :: L.Fd -> Fish PT.Fd
lookupFd' fd = lookupFd fd >>=
  maybe (notOpenErr fd) return

-- Errors:

mkFdErr :: String -> L.Fd -> Fish a
mkFdErr s fd = errork
  $ "file descriptor "
  <> (show . fromEnum) fd <> " is " <> s

notOpenErr = mkFdErr "not open"



