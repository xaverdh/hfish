{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.Process (
  fishCreateProcess
  ,fishWaitForProcess
) where

import qualified Data.Text as T
import Data.Bifunctor
import Data.Monoid
import Data.NText
import System.Process
import System.Posix.Process
import System.Posix.Types
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
import HFish.Interpreter.Process.FdSetup


fishWaitForProcess :: NText -> ProcessID -> Fish ()
fishWaitForProcess name pid = 
  liftIO ( getProcessStatus True{-block-} False pid )
  >>= \case
    Nothing -> errNoStatus
    Just stat -> case stat of
      Exited exCode -> setStatus exCode
      Terminated sig _ -> errTerm sig
      Stopped sig -> errStop sig
  where
    name' = extractText name
    errNoStatus = errork
      $ "could not retrieve status of command \""
      <> name' <> "\""
    errTerm sig = errork
      $ "\"" <> name'
       <> "\" was terminated by signal: "
       <> showText sig
    errStop sig = errork
      $ "\"" <> name'
       <> "\" was stopped by signal: "
       <> showText sig    

fishCreateProcess ::NText -> [T.Text] -> Fish ProcessID
fishCreateProcess name args = do
  env <- currentEnvironment
  pid <- forkWithFileDescriptors $
    executeFile nameString True argsStrings ( Just env )
  lastPid .= Just pid
  return pid
  where
    nameString = T.unpack $ extractText name
    argsStrings = map T.unpack args
    

currentEnvironment :: Fish [(String,String)]
currentEnvironment = 
  fmap (map $ bimap l r) exportVars
  where
    r = T.unpack . T.unwords . _value
    l = T.unpack . extractText


