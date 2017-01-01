{-# language LambdaCase, GADTs, Rank2Types, OverloadedStrings, TupleSections, Strict #-}
module HFish.Interpreter.Var where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import Data.NText as NText
import HFish.Interpreter.Env as Env

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Bool
import Text.Read
import Control.Lens
import Control.Monad
import System.Exit

newtype EnvLens a = EnvLens
  { envlens :: Lens' FishState (Env a) }

evironments =
  [ EnvLens localEnv
   ,EnvLens flocalEnv
   ,EnvLens globalEnv
   ,EnvLens readOnlyEnv ]

instance Show Var where
  show (Var UnExport vs _) = show vs
  show (Var Export vs _) = "(exported) " ++ show vs

mkVar :: [T.Text] -> Var
mkVar ts = Var UnExport (length ts) ts

mkVarXp :: [T.Text] -> Var
mkVarXp ts = Var Export (length ts) ts

getOccurs :: NText -> Fish [(EnvLens Var,Var)]
getOccurs ident =
  forM evironments look
  <$$> mapMaybe (uncurry check)
  where
    look env = do
      mv <- uses (envlens env) (`Env.lookup` ident)
      return (env,mv)
    check env mv = (env,) <$> mv


getVarMaybe :: NText -> Fish (Maybe Var)
getVarMaybe ident =
  (fmap snd . listToMaybe) <$> getOccurs ident

getVar :: NText -> Fish Var
getVar ident = getVarMaybe ident >>= \case
  Just v -> return v
  Nothing -> errork
    $ "Lookup of variable " <> extractText ident <> " failed."

getVarValue :: NText -> Fish [T.Text]
getVarValue t = do
  v <- getVar t
  return (v ^. value)

allVars :: Fish [(NText,Var)]
allVars = do
  envs <- forM evironments (use . envlens)
  return (join . map Env.toList $ envs)

exportVars :: Fish [(NText,Var)]
exportVars = Prelude.filter (isExport . snd) <$> allVars

showVarEnv :: Bool -> Env Var -> T.Text
showVarEnv namesOnly env = T.unlines $
  Env.toList env <$$>
  ( if namesOnly
    then extractText . fst
    else \(k,Var _ _ vs) -> T.unwords (extractText k : vs) )

isExport :: Var -> Bool
isExport (Var ex _ _) = 
  case ex of
    Export -> True
    UnExport -> False

setVar :: EnvLens Var -> NText -> Var -> Fish  ()
setVar envl ident var = envlens envl %= insert ident var

modifyVar :: EnvLens Var -> NText -> (Var -> Var) -> Fish  ()
modifyVar envl ident f = envlens envl %= adjust f ident

setVarSafe :: EnvLens Var -> NText -> Var -> Fish  ()
setVarSafe env ident var =
  uses readOnlyEnv (`Env.lookup` ident) >>= \case
    Just _ -> errork "Will not set or shadow readonly variable"
    Nothing -> setVar env ident var

delVarSafe :: EnvLens Var -> NText -> Fish  ()
delVarSafe env ident =
  -- todo: rewrite EnvLens to do something better here
  uses readOnlyEnv (`Env.lookup` ident) >>= \case
    Just _ -> errork "Will not delete readonly variable"
    Nothing -> envlens env %= Env.delete ident

withTextVar :: (T.Text -> a) -> Var -> a
withTextVar f var = f $ T.unwords (var ^. value)

readVar :: Read a => Var -> a
readVar = withTextVar readText

readVarMaybe :: Read a => Var -> Maybe a
readVarMaybe = withTextVar readTextMaybe

mapSerialVar :: (Read a,Show b) => (a -> b) -> Var -> Var
mapSerialVar f = value %~ map (showText . f . readText)



