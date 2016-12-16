{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.FuncSt where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Util

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Map as M

import Options.Applicative
import Options.Applicative.Builder as OB

-- Todo: Event handling stuff.

funcStA :: (Prog t -> Fish ())
  -> FunIdent t
  -> [T.Text]
  -> Prog t
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
      <$> textOption (short 'd' <> long "description"
        <> metavar "DESCRIPTION" <> OB.value name)
      <*> switch (short 'S' <> long "no-scope-shadowing")
      <*> many ( textOption (short 'V' <> long "inherit-variable"
          <> metavar "NAME") )
      <*> ( switch (short 'a' <> long "argument-names")
            *> many (OB.argument text $ metavar "NAMES") )
    
    text = T.pack <$> str
    textOption = option text

funcWorker :: (Prog t -> Fish ())
  -> T.Text -- ^ The function name
  -> Prog t -- ^ The function body
  -> T.Text -- ^ Description of the function
  -> Bool -- ^ Do not shadow the scope ?
  -> [T.Text] -- Variables to inherit
  -> [T.Text] -- Argument identifiers
  -> Fish ()
funcWorker progA name prog desc noShadow inherit idents = do
  modify (functions . at name .~ Just f)
  where
    f args =
      localise flocalEnv $ do
        unless noShadow (flocalEnv %= filterInherit)
        setFLocal "argv" args
        assignLoop args idents
        progA prog
    
    filterInherit = M.filterWithKey
      $ \k _ -> k `elem` inherit
    
    assignLoop :: [T.Text] -> [T.Text] -> Fish ()
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
