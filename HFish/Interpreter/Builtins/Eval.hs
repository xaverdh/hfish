module HFish.Interpreter.Builtins.Eval (
  evalF
  ,execF
  -- ,callF
) where

import HFish.Interpreter.Parsing
import HFish.Interpreter.Core
import HFish.Interpreter.Interpreter (progA)
import HFish.Interpreter.Process.Process (fishExec)
import qualified HFish.Interpreter.Stringy as Str
import System.Unix.IO.Text (toUnicode)

import qualified Data.Text as T
import Fish.Lang
import System.Posix.Process
import System.Exit
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

onArgs :: [Str] -> (Prog T.Text () -> Fish a) -> Fish (Maybe a)
onArgs ts k = do
  text <- liftIO $ toUnicode (Str.unwords ts)
  res <- parseFishInteractive text
  withProg res k

evalF :: Builtin
evalF _ ts = void (onArgs ts progA)

execF :: Builtin
execF _ (name:args) =
  fishExec
    (Str.toString name)
    (map Str.toString args)


