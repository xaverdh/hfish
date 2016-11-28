{-# language LambdaCase, GADTs, FlexibleContexts, OverloadedStrings #-}
module HFish.Interpreter.Var where

import HFish.Interpreter.Core
import HFish.Interpreter.Util

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Data.Bool
import Text.Read
import Control.Lens
import Control.Monad
import System.Exit

instance Show Var where
  show (Var False vs) = show vs
  show (Var True vs) = "(exported) " ++ show vs

showVars :: Env Var -> T.Text
showVars env = T.unlines $ map show' (M.toList env)
  where
    show' (k,Var b vs) = case b of
      True -> T.unwords (k:"(exported)":vs)
      False -> T.unwords (k:vs)

getVar :: T.Text -> Fish Var
getVar ident = getVarMaybe ident >>= \case
  Just v -> return v
  Nothing -> errork
    $ "Lookup of variable " <> ident <> " failed."


getVarValue :: T.Text -> Fish [T.Text]
getVarValue t = do
  v <- getVar t
  return (v ^. value)

getVarMaybe :: T.Text -> Fish (Maybe Var)
getVarMaybe ident =
  msum <$> getOccurs
  where
    envs = [localEnv,flocalEnv,globalEnv,readOnlyEnv]
    getOccurs :: Fish [Maybe Var]
    getOccurs = forM envs (\env -> preuse $ env . ix ident)

allVars :: Fish [(T.Text,Var)]
allVars = do
    envs <- forM envs use
    return (join . map M.toList $ envs)
  where
    envs = [localEnv,flocalEnv,globalEnv,readOnlyEnv]

exportVars :: Fish [(T.Text,Var)]
exportVars = filter (isExport . snd) <$> allVars
  where
    isExport (Var b _) = b

setVar env ident var =
  env . at ident .= Just var

modifyVar env ident f =
  env . ix ident %= f

isReadOnlyVar :: T.Text -> Fish Bool
isReadOnlyVar ident = use (readOnlyEnv . at ident) >>= \case
  Just _ -> return True
  Nothing -> return False

setVarSafe env ident var =
  isReadOnlyVar ident >>= \case
    True -> errork "Will not set or shadow readonly variable"
    False -> setVar env ident var

withTextVar :: (T.Text -> a) -> Var -> a
withTextVar f var = f
  $ T.intercalate " "
  (var ^. value)

readVar :: Read a => Var -> a
readVar = withTextVar readText

readVarMaybe :: Read a => Var -> Maybe a
readVarMaybe = withTextVar readTextMaybe


