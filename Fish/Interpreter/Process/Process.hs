module Fish.Interpreter.Process.Process where

import qualified Data.Text as T
import Data.Bifunctor
import System.Process
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens

import Fish.Interpreter.IO
import Fish.Interpreter.Core
import Fish.Interpreter.Status
import Fish.Interpreter.Var
import Fish.Interpreter.Cwd
import Fish.Interpreter.Process.Pid
import Fish.Interpreter.Process.Unsafe (phGetPid)

fishCreateProcess :: Bool -> T.Text -> [T.Text] -> Fish ()
fishCreateProcess forked name args = do
  wdir <- use cwdir
  vars <- map (second $ T.unwords . _value) <$> exportVars
  phmvar <- liftIO newEmptyMVar
  forkWithFileDescriptors $ do
    (_,_,_,ph) <- createProcess_ "createProcessWithRedirections"
      (proc nameText argsText){
        close_fds = False
        ,delegate_ctlc = not forked
        ,std_in = Inherit
        ,std_out = Inherit
        ,std_err = Inherit
        ,cwd = Just wdir
        ,env = Just $ map (bimap T.unpack T.unpack) vars
      }
    putMVar phmvar ph
  ph <- liftIO (takeMVar phmvar)
  mpid <- liftIO ( phGetPid ph )
  lastPid .= mpid
  unless forked
    ( liftIO ( waitForProcess ph )
      >>= setStatus )
    
  where
    nameText = T.unpack name
    argsText = map T.unpack args
