{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Init (
  mkInitialFishState
  ,mkInitialFishReader
) where

import HFish.Interpreter.Core
import HFish.Interpreter.FdTable (initialFdTable)
import HFish.Interpreter.Var
import HFish.Interpreter.Builtins (allBuiltins)

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
  inherited <- map (bimap T.pack (Var True . pure . T.pack)) <$> getEnvironment
  let (ro,rw) = teeVars inherited
  return $  emptyFishState {
      _functions = M.empty
      ,_globalEnv = M.fromList rw
      ,_readOnlyEnv = (initStatus . incShlvl . M.fromList) ro
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
      (Var True . pure . T.pack . show . (+1))
      <$> (mv >>= readVarMaybe)
    
    incShlvl = at "SHLVL" %~ inc    
    initStatus = M.insert "status" (Var False ["0"])


mkInitialFishReader :: Fish () -> IO FishReader
mkInitialFishReader atBreakpoint =
  return FishReader {
    _fdTable = initialFdTable
    ,_builtins = allBuiltins
    ,_breakK = [const warnB]
    ,_continueK = [const warnC]
    ,_returnK = [const warnR]
    ,_errorK = [error . T.unpack]
    ,_breakpoint = atBreakpoint
  }
  where
    warnB = error "no loop left to break"
    warnC = error "no loop left to continue"
    warnR = error "no function to return from"
                    
