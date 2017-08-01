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
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import Data.Semigroup
import Data.Text.IO as TextIO
import Control.Lens
import Control.Monad.IO.Class
import System.Exit
import System.IO

pushdF :: Builtin
pushdF fork = argsChoice [0,1] $ \case
  [] -> do
    home <- getHOME
    pushdF fork [home]
    ok
  [dir] -> pushCWD dir

popdF :: Builtin
popdF _ = args0 $
  popCWD >>= \case
    Nothing -> errork "popd: directory stack is empty"
    Just dir -> setCWD dir >>= setStatus

dirsF :: Builtin
dirsF _ = argsChoice [0,1] $ \case
  [] -> do
    dirs <- use dirstack
    wdir <- getCWD
    writeTo Fd1 $ Str.unwords (wdir:dirs)
    {- Adding the current working directory to the stack
       does not make a lot of sense to my mind, but since
       fish does that ... here we go -}
    ok
  ["-c"] -> do
    dirstack .= []
    ok

