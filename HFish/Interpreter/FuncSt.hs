{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.FuncSt where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Util
import HFish.Interpreter.Events
import HFish.Interpreter.Env as Env

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.NText
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Map as M

import Options.Applicative
import Options.Applicative.Builder as OB

-- Todo: Event handling stuff.

funcStA :: (Prog T.Text t -> Fish ())
  -> FunIdent T.Text t
  -> [T.Text]
  -> Prog T.Text t
  -> Fish ()
funcStA progA (FunIdent _ name) ts prog =
  execParserPure defaultPrefs parser (map T.unpack ts)
  & \case
    Success f -> f
    Failure err ->
      (errork . T.pack . fst)
       (renderFailure err "read: invalid arguments given\n")
  where
    parser = info funcOptions idm
    funcOptions = funcWorker progA name prog
      <$> option text (short 'd' <> long "description"
        <> metavar "DESCRIPTION" <> OB.value (extractText name))
      <*> switch (short 'S' <> long "no-scope-shadowing")
      <*> many ( option nfcText (short 'e' <> long "on-event"
          <> metavar "EVENT_NAME" ) )
      <*> many ( option text (short 's' <> long "on-signal"
          <> metavar "SIGSPEC" ) )
      <*> many ( option nfcText (short 'V' <> long "inherit-variable"
          <> metavar "NAME" ) )
      <*> ( switch (short 'a' <> long "argument-names")
            *> many (OB.argument nfcText $ metavar "NAMES") )
    
    text = T.pack <$> str
    nfcText = mkNText <$> text

funcWorker :: (Prog T.Text t -> Fish ())
  -> NText -- ^ The function name
  -> Prog T.Text t -- ^ The function body
  -> T.Text -- ^ Description of the function
  -> Bool -- ^ Do not shadow the scope ?
  -> [NText] -- Run on named event
  -> [T.Text] -- Run on given signal
  -> [NText] -- Variables to inherit
  -> [NText] -- Argument identifiers
  -> Fish ()
funcWorker progA
  name prog desc noShadow
  events signals inherit idents = do
    functions %= Env.insert name f
    forM_ events $ flip setupEventHandler (EventHandler name)
    forM_ signals (flip setupSignalHandler $ SignalHandler name)
  where
    f args =
      localise flocalEnv $ do
        unless noShadow (flocalEnv %= filterInherit)
        setFLocal "argv" args
        assignLoop args idents
        progA prog
    
    filterInherit = Env.filterWithKey
      $ \k _ -> k `elem` inherit
    
    assignLoop :: [T.Text] -> [NText] -> Fish ()
    assignLoop = \case
      [] -> \idents -> forM_ idents $ flip setFLocal []
      (arg:args) -> \case
        [] -> return ()
        -- [ident] -> setFLocal ident $ arg:args
        -- would be more natural but break fish compat
        ident:idents -> 
          setFLocal ident [arg]
          >> assignLoop args idents

    setFLocal ident vs = (Var UnExport vs)
      & setVarSafe (EnvLens flocalEnv) ident

