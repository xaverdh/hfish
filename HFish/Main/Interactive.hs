{-# language LambdaCase, OverloadedStrings #-}
module HFish.Main.Interactive
  ( runInterpreterLoop
  , runProgramInteractive )
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
import HFish.Interpreter.Parsing
import HFish.Types

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


runInterpreterLoop :: MonadIO io
  => FishCompat
  -> IsBreakPoint
  -> FishReader
  -> FishState
  -> io ()
runInterpreterLoop
  (FishCompat fishCompat)
  (IsBreakPoint isbrkpt) r s =
  liftIO $ runInputT defaultSettings loop
  where
    loop = interpreterLoop fishCompat (mkPrompt isbrkpt) r s

interpreterLoop :: Bool
  -> ( FishState -> String ) -- the prompt
  -> FishReader -> FishState -> InputT IO ()
interpreterLoop fishCompat prompt r = loop
  where
    loop :: FishState -> InputT IO ()
    loop s = whenJustM{- die on ctrl-d -} (getInput s)
      $ \input -> maybeM (loop s) loop
        $ withProg
          ( parseInteractive fishCompat $ input ++ "\n" )
          ( runProgramInteractive r s )
    
    getInput = getInputLineIgnoreSigInt . prompt

getInputLineIgnoreSigInt :: String -> InputT IO (Maybe String)
getInputLineIgnoreSigInt p = withInterrupt loop
  where
    loop = handleInterrupt handleSigInt (getInputLine p)
    handleSigInt = loop

runProgramInteractive :: MonadIO io
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> io FishState
runProgramInteractive r s p = liftIO
  $ E.catches ( runFish (progA p) r s )
  [ E.Handler handleAsyncException
  , E.Handler handleOtherException ]
  where
    handleAsyncException :: AsyncException -> IO FishState
    handleAsyncException = \case
      UserInterrupt -> pure s
      e -> E.throwIO e
    
    handleOtherException :: SomeException -> IO FishState
    handleOtherException e = do
      liftIO $ PP.putDoc (showError e)
      pure s

    showError :: SomeException -> PP.Doc
    showError e = PP.vsep [ showErr e, showTr p ] <> PP.hardline
    
    showErr e = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Error:"
      , (PP.red . PP.string . show) e ]
    
    showTr p = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Occured while evaluating:"
      , (PP.yellow . PP.text . show) (GP.doc $ toBase p) ]


