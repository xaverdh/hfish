{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Cwd where

import Fish.Interpreter.Core
import Fish.Interpreter.Var
import Fish.Interpreter.Status

import qualified Data.Text as T
import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Exit

getHOME :: Fish FilePath
getHOME = liftIO getHomeDirectory

setCWD :: FilePath -> Fish ()
setCWD dir = do
  d <- liftIO $ makeAbsolute dir
  liftIO (doesDirectoryExist d) >>= \case
    True -> do
      cwdir .= d
      readOnlyEnv . ix "PWD" . value .= [T.pack d]
      ok
    False -> setStatus (ExitFailure 1)

getCWD :: Fish FilePath
getCWD = use cwdir

pushCWD :: FilePath -> Fish ()
pushCWD dir = do
  oldDir <- getCWD
  setCWD dir
  ifOk ( dirstack %= (oldDir:) )

popCWD :: Fish (Maybe FilePath)
popCWD = use dirstack >>= \case
  [] -> return Nothing
  (dir:dirs) -> do
    dirstack .= dirs
    return (Just dir)

