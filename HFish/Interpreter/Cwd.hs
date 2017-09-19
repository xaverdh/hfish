{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Cwd where

import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.Semigroup
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Exit

getHOME :: Fish Str
getHOME = Str.fromString <$> liftIO getHomeDirectory

setCWD :: Str -> Fish ExitCode
setCWD dir = do
  d <- liftIO $ makeAbsoluteStr dir
  liftIO (doesDirectoryExistStr d) >>= \case
    True -> do
      cwdir .= d
      readOnlyEnv %= Env.adjust
        (value .~ pure d) "PWD"
      pure ExitSuccess
    False -> pure $ ExitFailure 1
  where
    makeAbsoluteStr :: Str -> IO Str
    makeAbsoluteStr = fmap Str.fromString . makeAbsolute . Str.toString

    doesDirectoryExistStr :: Str -> IO Bool
    doesDirectoryExistStr = doesDirectoryExist . Str.toString

getCWD :: Fish Str
getCWD = use cwdir

pushCWD :: Str -> Fish ()
pushCWD dir = do
  oldDir <- getCWD
  setCWD dir >>= setStatus
  ifOk ( dirstack %= (oldDir:) )

popCWD :: Fish (Maybe Str)
popCWD = use dirstack >>= \case
  [] -> pure Nothing
  (dir:dirs) -> do
    dirstack .= dirs
    pure (Just dir)

