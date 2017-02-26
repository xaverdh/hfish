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
  wdir <- getCurrentDirectory
  inherited <- map (bimap T.pack (mkVarXp . pure . T.pack)) <$> getEnvironment
  teeVars inherited & \(ro,rw) ->
    return $ emptyFishState {
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
      (mkVarXp . pure . T.pack . show . (+1))
      <$> (mv >>= readVarMaybe)
    
    incShlvl = Env.alter inc "SHLVL"
    initStatus = Env.insert "status" $ mkVar (pure "0")


mkInitialFishReader :: Fish () -> Bool -> IO FishReader
mkInitialFishReader atBreakpoint fishcompat =
  return FishReader {
    _fdTable = initialFdTable
    ,_builtins = allBuiltins
    ,_breakK = [const warnB]
    ,_continueK = [const warnC]
    ,_returnK = [const warnR]
    ,_errorK = [error . T.unpack]
    ,_breakpoint = atBreakpoint
    ,_fishCompatible = fishcompat
  }
  where
    warnB = error "no loop left to break"
    warnC = error "no loop left to continue"
    warnR = error "no function to return from"

