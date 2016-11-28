module Fish.Interpreter.Builtins.Eval (
  evalF
  ,execF
  -- ,callF
) where

import Fish.Interpreter.Parsing
import Fish.Interpreter.Core
import Fish.Interpreter.Interpreter (progA)
import qualified Data.Text as T
import Fish.Lang.Lang
import System.Posix.Process
import System.Exit
import Control.Monad
import Control.Monad.IO.Class

onArgs :: [T.Text] -> (Prog () -> Fish a) -> Fish (Maybe a)
onArgs =
  withProg
  . parseFishInteractive 
  . T.unpack . T.unwords

evalF :: Builtin
evalF _ ts = void (onArgs ts progA)


execF :: Builtin
execF _ ts = 
  let name:args = map T.unpack ts
   in liftIO
    ( executeFile name True args Nothing
      >> exitImmediately ExitSuccess )

