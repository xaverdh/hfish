{-# language LambdaCase, OverloadedStrings, Rank2Types #-}
module HFish.Interpreter.Scope where

import qualified Fish.Lang as Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Env

import Control.Lens


data Scope =
  ReadOnlyScope
  | GlobalScope
  | LocalScope
  | FLocalScope
  {- | UniversalScope -}
  deriving (Eq,Ord,Show,Bounded,Enum)

asLens :: Scope -> Lens' FishState (Env Var)
asLens = \case
  ReadOnlyScope -> readOnlyEnv
  GlobalScope   -> globalEnv
  LocalScope    -> localEnv
  FLocalScope   -> flocalEnv
  {- UniversalScope -> universalEnv -}

fromLangScope :: Lang.Scope -> Scope
fromLangScope = \case
  Lang.ScopeGlobal -> GlobalScope
  Lang.ScopeLocal  -> LocalScope
  Lang.ScopeUniversal -> {- UniversalScope -}
    error "Universal scope not supported yet"
  
