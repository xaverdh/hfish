{-# language LambdaCase #-}
module HFish.Main.NonInteractive
  ( runProgram
  , getRunProgram )
where


import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
-- import Control.Exception as E
import qualified Data.Text as T
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Core
import HFish.Dispatch
import Fish.Lang
import Fish.Lang.Base
import Fish.Lang.Unit

runProgram :: Prog T.Text () -> Dispatch FishState
runProgram p = getRunProgram p >>= liftIO

getRunProgram :: Prog T.Text () -> Dispatch (IO FishState)
getRunProgram p =
  pure runFish
  <*> getAction p
  <*> use dReader
  <*> use dState

getAction :: Prog T.Text () -> Dispatch (Fish ())
getAction p = maybeM (pure $ progA p) action' (use dOnError)
  where
    action' k = pure $ setErrorK (progA p) >>= flip whenJust k

