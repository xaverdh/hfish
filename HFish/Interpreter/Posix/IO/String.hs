module HFish.Interpreter.Posix.IO.String where

import HFish.Interpreter.Posix.IO.ByteString
import System.Posix.Types
import Data.ByteString.Char8 as B


fdGetContentsS :: Fd -> IO String
fdGetContentsS = fmap B.unpack . fdGetContentsB

fdGetLineS :: Fd -> IO String
fdGetLineS = fmap B.unpack . fdGetLineB

fdPutS :: Fd -> String -> IO ()
fdPutS fd = fdPutB fd . B.pack
