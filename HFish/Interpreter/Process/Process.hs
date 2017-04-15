{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.Process (
  fishCreateProcess
  ,fishWaitForProcess
  ,fishExec
) where

import qualified Data.Text as T
import Data.Foldable
import Data.Bifunctor
import Data.Monoid
import Data.NText
import System.Process
import System.Posix.Process
import System.Posix.Types
import System.IO
import System.IO.Error
import System.Directory (setCurrentDirectory)
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
import qualified HFish.Interpreter.Stringy as Str


fishWaitForProcess :: String -> ProcessID -> Fish ()
fishWaitForProcess name pid = 
  liftIO ( getProcessStatus True{-block-} False pid )
  >>= \case
    Nothing -> errNoStatus
    Just stat -> case stat of
      Exited exCode -> setStatus exCode
      Terminated sig _ -> errTerm sig
      Stopped sig -> errStop sig
  where
    errNoStatus = errork
      $ "could not retrieve status of command \""
      <> name <> "\""
    errTerm sig = errork
      $ "\"" <> name
       <> "\" was terminated by signal: "
       <> show sig
    errStop sig = errork
      $ "\"" <> name
       <> "\" was stopped by signal: "
       <> show sig

fishCreateProcess :: String -> [String] -> Fish ProcessID
fishCreateProcess name args = do
  getCWD >>= liftIO . setCurrentDirectory . Str.toString
  -- ^ This is necessary since the current working directory
  --   is separate from the environment passed to the process.
  --
  --   The kernel holds it in a separate variable which is 
  --   automatically inherited when a child is spawned.
  env <- currentEnvironment
  pid <- forkWithFileDescriptors $
    executeFile 
      name
      True{-search path-}
      args
      ( Just env )
  lastPid .= Just pid
  return pid

fishExec :: String -> [String] -> Fish a
fishExec name args = do
  getCWD >>= liftIO . setCurrentDirectory . Str.toString
  -- ^ This is necessary since the current working directory
  --   is separate from the environment passed to the process.
  --
  --   The kernel holds it in a separate variable which is 
  --   automatically inherited when a child is spawned.
  env <- currentEnvironment
  realiseFileDescriptors
  liftIO $ executeFile 
    name
    True{-search path-}
    args
    ( Just env )

currentEnvironment :: Fish [(String,String)]
currentEnvironment = 
  fmap (map $ bimap l r) exportVars
  where
    r = Str.toString . Str.unwords . toList . _value
    l = T.unpack . extractText


