{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Init (
  mkInitialFishState
  ,mkInitialFishReader
) where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.FdTable (initialFdTable)
import HFish.Interpreter.Var
import HFish.Interpreter.Builtins (allBuiltins)
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Bifunctor
import Text.Read
import System.IO
import System.Exit
import System.Environment
import System.Directory

readOnly = ["SHLVL","PWD"]

mkInitialFishState :: IO FishState
mkInitialFishState = do
  wdir <- Str.fromString <$> getCurrentDirectory
  inherited <- map (bimap Str.fromString (mkVarXp . pure . Str.fromString)) <$> getEnvironment
  teeVars inherited & \(ro,rw) ->
    pure $ emptyFishState {
      _functions = Env.empty
      ,_globalEnv = Env.fromTextList rw
      ,_readOnlyEnv = (initStatus . incShlvl . Env.fromTextList) ro
      ,_status = ExitSuccess
      ,_cwdir = wdir
    }
  where
    teeVars = \case
      [] -> ([],[])
      x:xs ->
        (if fst x `elem` readOnly then first else second)
        (x:) (teeVars xs)
    
    inc :: Maybe Var -> Maybe Var
    inc mv =
      (mkVarXp . pure . Str.fromString . show . (+1))
      <$> (mv >>= readVarMaybe)
    
    incShlvl = Env.alter inc "SHLVL"
    initStatus = Env.insert "status" $ mkVar (pure "0")


mkInitialFishReader :: Fish () -> Bool -> IO FishReader
mkInitialFishReader atBreakpoint fishcompat =
  pure FishReader {
    _fdTable = initialFdTable
    ,_builtins = allBuiltins
    ,_breakK = const warnB
    ,_continueK = const warnC
    ,_returnK = const warnR
    ,_errorK = []
    ,_breakpoint = atBreakpoint
    ,_fishCompatible = fishcompat
    ,_executionStack = []
  }
  where
    warnB = error "No loop left to break."
    warnC = error "No loop left to continue."
    warnR = error "No function to return from."

