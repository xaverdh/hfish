{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.Process where

import qualified Data.Text as T
import Data.Bifunctor
import Data.Monoid
import System.Process
import System.Posix.Process
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens

import HFish.Interpreter.IO
import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.Status
import HFish.Interpreter.Var
import HFish.Interpreter.Cwd
import HFish.Interpreter.Process.Pid

fishCreateProcess :: Bool -> T.Text -> [T.Text] -> Fish ()
fishCreateProcess forked name args = do
  env <- currentEnvironment
  pid <- forkWithFileDescriptors $
    executeFile nameString True argsStrings ( Just env )
  lastPid .= Just pid
  if forked then return ()
    else do
      liftIO ( getProcessStatus True{-block-} False pid )
      >>= \case
        Nothing -> errork $ "could not retrieve status of command \"" <> name <> "\""
        Just stat -> case stat of
          Exited exCode -> setStatus exCode
          Terminated sig _ -> errork $ "\"" <> name <> "\" was terminated by signal: " <> showText sig
          Stopped sig -> errork $ "\"" <> name <> "\" was stopped by signal: " <> showText sig        
  where
    nameString = T.unpack name
    argsStrings = map T.unpack args

currentEnvironment :: Fish [(String,String)]
currentEnvironment = 
  fmap (map $ bimap l r) exportVars
  where
    r = T.unpack . T.unwords . _value
    l = T.unpack

