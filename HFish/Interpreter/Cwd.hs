{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Cwd where

import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.Env as Env

import qualified Data.Text as T
import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Exit

getHOME :: Fish FilePath
getHOME = liftIO getHomeDirectory

setCWD :: FilePath -> Fish ExitCode
setCWD dir = do
  d <- liftIO $ makeAbsolute dir
  liftIO (doesDirectoryExist d) >>= \case
    True -> do
      cwdir .= d
      readOnlyEnv %= Env.adjust (value .~ [T.pack d]) "PWD"
      return ExitSuccess
    False -> return $ ExitFailure 1

getCWD :: Fish FilePath
getCWD = use cwdir

pushCWD :: FilePath -> Fish ()
pushCWD dir = do
  oldDir <- getCWD
  setCWD dir >>= setStatus
  ifOk ( dirstack %= (oldDir:) )

popCWD :: Fish (Maybe FilePath)
popCWD = use dirstack >>= \case
  [] -> return Nothing
  (dir:dirs) -> do
    dirstack .= dirs
    return (Just dir)

