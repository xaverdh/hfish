{-# language LambdaCase, OverloadedStrings #-}
module HFish.Main.Interactive
  ( runInterpreterLoop )
where

import Debug.Trace

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Reader.Class
import Control.Exception as E

import Data.Semigroup
import qualified Data.Text as T

import HFish.Interpreter.Interpreter
import HFish.Interpreter.Core
import HFish.Interpreter.IO (echo)
import HFish.Interpreter.Parsing
import HFish.Types
import HFish.Dispatch
import HFish.Main.NonInteractive (getRunProgram)

import Fish.Lang
import Fish.Lang.Unit
import Fish.Lang.Base
import Fish.Lang.Pretty

import System.Console.Haskeline
import System.Exit

import qualified Text.PrettyPrint.GenericPretty as GP
import qualified Text.PrettyPrint.ANSI.Leijen as PP


mkPrompt :: Bool -> FishState -> String
mkPrompt isbrkpt s = show $ case isbrkpt of
  True -> insStatus <> PP.blue ": "
  False -> PP.blue "~" <> insStatus <> PP.blue "> "
  where
    insStatus = case s ^. status of
      ExitSuccess -> PP.blue $ PP.int 0
      ExitFailure i -> PP.red $ PP.int i


runInterpreterLoop :: IsBreakPoint -> Dispatch ()
runInterpreterLoop (IsBreakPoint isbrkpt) = do
  let loop = interpreterLoop $ mkPrompt isbrkpt 
  runInputT defaultSettings loop

interpreterLoop :: ( FishState -> String ) -- the prompt
  -> InputT Dispatch ()
interpreterLoop prompt = loop
  where
    loop = whenJustM{- die on ctrl-d -} getInput
      $ \input -> ( lift $ runInput input ) *> loop
    
    runInput :: String -> Dispatch ()
    runInput input = do
      FishCompat fcompat <- use dCompat
      let res = parseInteractive fcompat input
      withProg' res $ \p ->
        runProgramInteractive p >>= (dState .=)

    getInput :: InputT Dispatch (Maybe String)
    getInput = do
      s <- lift $ use dState
      fmap (<>"\n") <$> getInputLineIgnoreSigInt (prompt s)



getInputLineIgnoreSigInt :: String
  -> InputT Dispatch (Maybe String)
getInputLineIgnoreSigInt p = withInterrupt loop
  where
    loop = handleInterrupt handleSigInt (getInputLine p)
    handleSigInt = loop


runProgramInteractive :: Prog T.Text () -> Dispatch FishState
runProgramInteractive p = do
  setInteractiveErrorK p
  run <- getRunProgram p
  s <- use dState
  liftIO $ E.catch run $ handleAsyncException s
  where 

    handleAsyncException :: FishState
      -> AsyncException -> IO FishState
    handleAsyncException s = \case
      UserInterrupt -> pure s
      e -> E.throwIO e
    

setInteractiveErrorK :: Prog T.Text () -> Dispatch ()
setInteractiveErrorK p = dOnError .= Just handle
  where
    handle :: String -> Fish ()
    handle e = echo . show $
      showErr e <> showTr p <> PP.hardline
    
    showTr p = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Occured while evaluating:"
      , (PP.yellow . PP.text . show) (GP.doc $ toBase p) ]

    showErr e = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Error:"
      , (PP.red . PP.string) e ]





