{-# language LambdaCase, OverloadedStrings #-}
{-# language FlexibleInstances, ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
module HFish.Main.Interactive
  ( runInterpreterLoop
  , runProgram )
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
import qualified Data.Sequence as Seq
import Data.Semigroup
import Data.Maybe
import Data.Bifunctor
import Data.Functor
import Data.String (IsString)
import Data.List as L
import qualified Data.Text as T
import HFish.Interpreter.Util
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Interpreter.Var
import qualified HFish.Interpreter.Version as IV
import qualified HFish.Version as V
import qualified Fish.Lang.Version as LV
import qualified Fish.Parser.Version as FPV
import qualified HFish.Parser.Version as HFPV
import HFish.Description (description)
import HFish.Types
import qualified HFish.Interpreter.Stringy as Str
import Fish.Lang
import Fish.Lang.Unit
import Fish.Lang.Base
import Fish.Lang.Pretty

import System.Console.Haskeline
import System.Environment (getArgs,getProgName)
import System.Directory (listDirectory)
import System.FilePath (takeExtensions,(</>))
import System.Exit

import Options.Applicative as O
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)

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
          ( runProgram r s )
    
    getInput = getInputLineIgnoreSigInt . prompt

getInputLineIgnoreSigInt :: String -> InputT IO (Maybe String)
getInputLineIgnoreSigInt p = withInterrupt loop
  where
    loop = handleInterrupt handleSigInt (getInputLine p)
    handleSigInt = loop

runProgram :: MonadIO io
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> io FishState
runProgram r s p = liftIO
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


