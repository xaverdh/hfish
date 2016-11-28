{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Builtins.Dirstack (
  pushdF
  ,popdF
  ,dirsF
) where

import Fish.Lang.Lang
import Fish.Interpreter.Core
import Fish.Interpreter.IO
import Fish.Interpreter.Concurrent
import Fish.Interpreter.Cwd
import Fish.Interpreter.Status

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
    Just dir -> setCWD dir
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
