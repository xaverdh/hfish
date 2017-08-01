{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.FuncSt where

import Fish.Lang hiding (Scope)
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Util
import HFish.Interpreter.Events
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str

import Control.Lens hiding ((:<))
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.NText
import Data.Semigroup hiding (option)

import Options.Applicative
import Options.Applicative.Builder as OB


funcStA :: (Prog T.Text t -> Fish ())
  -> FunIdent T.Text t
  -> Seq Str
  -> Prog T.Text t
  -> Fish ()
funcStA progA (FunIdent _ name) ts prog =
  execParserPure defaultPrefs parser (map Str.toString . F.toList $ ts)
  & \case
    Success f -> f
    Failure err -> errork . fst
      $ renderFailure err "read: invalid arguments given\n"
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


-- | 
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
    inherited <- uses flocalEnv $ \env ->
      map (\k -> (k,Env.lookupDefault emptyVar k env)) inherit
    functions %= Env.insert name (f $ Env.fromList inherited)
    forM_ events $ flip setupEventHandler $ EventHandler name
    forM_ signals $ flip setupSignalHandler $ SignalHandler name
  where
    f :: Env Var -> Seq Str -> Fish ()
    f inherited args = 
      ( if noShadow 
          then id
          else localise flocalEnv
             . localise localEnv ) $ do
        flocalEnv %= (Env.union inherited)
        setFLocal "argv" args
        assignLoop args idents
        progA prog
    
    assignLoop :: Seq Str -> [NText] -> Fish ()
    assignLoop vs = case viewl vs of
      EmptyL -> \idents -> forM_ idents $ flip setFLocal mempty
      arg :< args -> \case
        [] -> return ()
        -- [ident] -> setFLocal ident vs
        -- would be more natural but break fish compat
        ident:idents -> 
          setFLocal ident (pure arg)
          >> assignLoop args idents

    setFLocal ident vs = (Var UnExport vs)
      & setVarSafe FLocalScope ident

