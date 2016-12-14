{-# language LambdaCase, GADTs, Rank2Types, OverloadedStrings, TupleSections, Strict #-}
module HFish.Interpreter.Var where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.Util

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Bool
import Text.Read
import Control.Lens
import Control.Monad
import System.Exit

-- TODO: catch errors thrown by read functions and rethrow as errork

newtype EnvLens a = EnvLens
  { envlens :: Lens' FishState (Env a) }

evironments =
  [ EnvLens localEnv
   ,EnvLens flocalEnv
   ,EnvLens globalEnv
   ,EnvLens readOnlyEnv ]

instance Show Var where
  show (Var UnExport vs) = show vs
  show (Var Export vs) = "(exported) " ++ show vs

getOccurs :: T.Text -> Fish [(EnvLens Var,Var)]
getOccurs ident = 
  forM evironments look
  <$$> mapMaybe (uncurry check)
  where
    look env = do
      mv <- preuse $ envlens env . ix ident
      return (env,mv)
    check env mv = (env,) <$> mv


getVarMaybe :: T.Text -> Fish (Maybe Var)
getVarMaybe ident =
  (fmap snd . listToMaybe) <$> getOccurs ident

getVar :: T.Text -> Fish Var
getVar ident = getVarMaybe ident >>= \case
  Just v -> return v
  Nothing -> errork
    $ "Lookup of variable " <> ident <> " failed."

getVarValue :: T.Text -> Fish [T.Text]
getVarValue t = do
  v <- getVar t
  return (v ^. value)

allVars :: Fish [(T.Text,Var)]
allVars = do
  envs <- forM evironments (use . envlens)
  return (join . map M.toList $ envs)

exportVars :: Fish [(T.Text,Var)]
exportVars = filter (isExport . snd) <$> allVars

showVarEnv :: Bool -> Env Var -> T.Text
showVarEnv namesOnly env = T.unlines $
  M.toList env <$$>
  ( if namesOnly
    then fst
    else \(k,Var _ vs) -> T.unwords (k:vs) )

isExport :: Var -> Bool
isExport (Var ex _) = 
  case ex of
    Export -> True
    UnExport -> False

setVar :: EnvLens Var -> T.Text -> Var -> Fish  ()
setVar env ident var = envlens env . at ident .= Just var

modifyVar :: EnvLens Var -> T.Text -> (Var -> Var) -> Fish  ()
modifyVar env ident f = envlens env . ix ident %= f

setVarSafe :: EnvLens Var -> T.Text -> Var -> Fish  ()
setVarSafe env ident var =
  use (readOnlyEnv . at ident) >>= \case
    Just _ -> errork "Will not set or shadow readonly variable"
    Nothing -> setVar env ident var

delVarSafe :: EnvLens Var -> T.Text -> Fish  ()
delVarSafe env ident =
  -- todo: rewrite EnvLens to do something better here
  use (readOnlyEnv . at ident) >>= \case
    Just _ -> errork "Will not delete readonly variable"
    Nothing -> envlens env . at ident .= Nothing

withTextVar :: (T.Text -> a) -> Var -> a
withTextVar f var = f $ T.unwords (var ^. value)

readVar :: Read a => Var -> a
readVar = withTextVar readText

readVarMaybe :: Read a => Var -> Maybe a
readVarMaybe = withTextVar readTextMaybe

mapSerialVar :: (Read a,Show b) => (a -> b) -> Var -> Var
mapSerialVar f = value %~ map (showText . f . readText)



