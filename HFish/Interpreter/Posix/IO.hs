{-# LANGUAGE LambdaCase, OverloadedStrings, PackageImports #-}
module HFish.Interpreter.Posix.IO (
  FdData(..)
) where

import HFish.Interpreter.Posix.IO.Text
import HFish.Interpreter.Posix.IO.ByteString

import System.Posix.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

class FdData a where
  fdGetContents :: Fd -> IO a
  fdGetLine :: Fd -> IO a
  fdPut :: Fd -> a -> IO ()

instance FdData B.ByteString where
  fdGetContents = fdGetContentsB
  fdGetLine = fdGetLineB
  fdPut = fdPutB

instance FdData LB.ByteString where
  fdGetContents = fdGetContentsLB  
  fdGetLine = fdGetLineLB
  fdPut = fdPutLB

instance FdData T.Text where
  fdGetContents = fdGetContentsT
  fdGetLine = fdGetLineT
  fdPut = fdPutT

instance FdData LT.Text where
  fdGetContents = fdGetContentsLT
  fdGetLine = fdGetLineLT
  fdPut = fdPutLT

