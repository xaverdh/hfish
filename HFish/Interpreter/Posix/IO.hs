{-# LANGUAGE LambdaCase, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module HFish.Interpreter.Posix.IO (
  FdData(..)
  ,FdReadable(..)
  ,FdWritable(..)
) where

import HFish.Interpreter.Posix.IO.ByteString
import HFish.Interpreter.Posix.IO.Text
import HFish.Interpreter.Posix.IO.String

import System.Posix.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

class (FdReadable a,FdWritable a) => FdData a

class FdReadable a where
  fdGetContents :: Fd -> IO a
  fdGetLine :: Fd -> IO a

class FdWritable a where
  fdPut :: Fd -> a -> IO ()


-- Strict ByteStrings
instance FdReadable B.ByteString where
  fdGetContents = fdGetContentsB
  fdGetLine = fdGetLineB

instance FdWritable B.ByteString where
  fdPut = fdPutB

instance FdData B.ByteString


-- Lazy ByteStrings
instance FdReadable LB.ByteString where
  fdGetContents = fdGetContentsLB  
  fdGetLine = fdGetLineLB
  
instance FdWritable LB.ByteString where
  fdPut = fdPutLB

instance FdData LB.ByteString


-- Strict Text
instance FdReadable T.Text where
  fdGetContents = fdGetContentsT
  fdGetLine = fdGetLineT

instance FdWritable T.Text where
  fdPut = fdPutT

instance FdData T.Text


-- Lazy Text
instance FdReadable LT.Text where
  fdGetContents = fdGetContentsLT
  fdGetLine = fdGetLineLT

instance FdWritable LT.Text where
  fdPut = fdPutLT

instance FdData LT.Text


-- Strings
instance FdReadable String where
  fdGetContents = fdGetContentsS
  fdGetLine = fdGetLineS

instance FdWritable String where
  fdPut = fdPutS

instance FdData String

