{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.Process where

import qualified Data.Text as T
import Data.Bifunctor
import Data.Monoid
import System.Process
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
import HFish.Interpreter.Process.Unsafe (phGetPid)

fishCreateProcess :: Bool -> T.Text -> [T.Text] -> Fish ()
fishCreateProcess forked name args = do
  wdir <- use cwdir
  vars <- map (second $ T.unwords . _value) <$> exportVars
  phmvar <- liftIO newEmptyMVar
  forkWithFileDescriptors $
    -- createProcess_ throws an exception, which we really
    -- should catch, but unfortunately we dont know which one
    -- due to lack of documentation in System.Process.
    -- ... So we just catch all IO errors.
    tryIOError
      ( createProcess_ "createProcessWithRedirections"
        (proc nameText argsText){
          close_fds = False
          ,delegate_ctlc = not forked
          ,std_in = Inherit
          ,std_out = Inherit
          ,std_err = Inherit
          ,cwd = Just wdir
          ,env = Just $ map (bimap T.unpack T.unpack) vars } )
    >>= \case
      Left e -> putMVar phmvar (Left e)
      Right (_,_,_,ph) -> putMVar phmvar (Right ph)
  
  liftIO (takeMVar phmvar) >>= \case
    Left ex -> errork $ "could not create process \"" <> name <> "\" due to:\n" <> showText ex
    Right ph -> do
      mpid <- liftIO ( phGetPid ph )
      lastPid .= mpid
      unless forked
        ( liftIO ( waitForProcess ph )
          >>= setStatus )
  where
    nameText = T.unpack name
    argsText = map T.unpack args
