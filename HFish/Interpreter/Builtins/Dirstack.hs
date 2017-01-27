{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Dirstack (
  pushdF
  ,popdF
  ,dirsF
) where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Cwd
import HFish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import Data.Text.IO as TextIO
import Control.Lens
import Control.Monad.IO.Class
import System.Exit
import System.IO

pushdF :: Bool -> [T.Text] -> Fish ()
pushdF fork = \case
  [] -> do
    home <- getHOME
    pushdF fork [T.pack home]
    ok
  [dir] -> pushCWD $ T.unpack dir
  _ -> errork "pushd: too many arguments given"

popdF :: Bool -> [T.Text] -> Fish ()
popdF _ = \case
  [] -> popCWD >>= \case
    Nothing -> errork "popd: directory stack is empty"
    Just dir -> setCWD dir >>= setStatus
  _ -> errork "popd: too many arguments given"

dirsF :: Bool -> [T.Text] -> Fish ()
dirsF _ = \case 
  [] -> do
    dirs <- map T.pack <$> use dirstack
    wdir <- T.pack <$> getCWD
    writeTo Fd1 $ T.unwords (wdir:dirs)
    {- Adding the current working directory to the stack
       does not make a lot of sense to my mind, but since
       fish does that ... here we go -}
    ok
  ["-c"] -> do
    dirstack .= []
    ok
  _ -> errork "dirs: too many arguments given"
