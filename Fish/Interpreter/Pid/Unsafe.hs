{-# language LambdaCase #-}
module Fish.Interpreter.Pid.Unsafe (
  phGetPid
) where

{- Here be dragons. -}

import System.Posix.Types (CPid)
import System.Process.Internals

phGetPid :: ProcessHandle -> IO (Maybe CPid)
phGetPid ph = withProcessHandle ph
  $ return . \case 
    OpenHandle pid -> Just pid
    ClosedHandle _ -> Nothing
