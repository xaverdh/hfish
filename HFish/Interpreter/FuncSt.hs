{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.FuncSt where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Util

import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T

{- This is _very_ rudimentary atm. -}
funcStA :: (Prog t -> Fish ())
  -> FunIdent t
  -> Args t
  -> Prog t
  -> Fish ()
funcStA progA (FunIdent _ ident) args prog =
  modify (functions . at ident .~ Just f)
  where
    f args' =
      localise flocalEnv $ do
        setVar (EnvLens flocalEnv) "argv" (Var UnExport args')
        progA prog

