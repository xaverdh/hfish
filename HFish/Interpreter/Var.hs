{-# language LambdaCase, GADTs, Rank2Types, OverloadedStrings, TupleSections, Strict #-}
module HFish.Interpreter.Var where

import Fish.Lang hiding (Scope)
import HFish.Interpreter.Scope
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

-- Todo: separate this module into interal (i.e. *Safe) and external part

scopes =
  [ FLocalScope
   ,LocalScope
   ,GlobalScope
   ,ReadOnlyScope
   {- ,UniversalScope -} ]

instance Show Var where
  show (Var UnExport vs _) = show vs
  show (Var Export vs _) = "(exported) " ++ show vs

mkVar :: [T.Text] -> Var
mkVar ts = Var UnExport (length ts) ts

emptyVar :: Var
emptyVar = Var UnExport 0 []

emptyVarXp :: Var
emptyVarXp = Var Export 0 []

mkVarXp :: [T.Text] -> Var
mkVarXp ts = Var Export (length ts) ts

getOccurs :: NText -> Fish [(Scope,Var)]
getOccurs ident =
  forM scopes look
  <$$> mapMaybe (uncurry check)
  where
    look scp = do
      mv <- uses (asLens scp) (`Env.lookup` ident)
      return (scp,mv)
    check scp mv = (scp,) <$> mv


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
  envs <- forM scopes (use . asLens)
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

setVar :: Scope -> NText -> Var -> Fish  ()
setVar scp ident var = asLens scp %= insert ident var

modifyVar :: Scope -> NText -> (Var -> Var) -> Fish  ()
modifyVar scp ident f = asLens scp %= adjust f ident

setVarSafe :: Scope -> NText -> Var -> Fish  ()
setVarSafe scp ident var
  | ReadOnlyScope <- scp = errork "Will not set or shadow readonly variable"
  | True = setVar scp ident var

delVarSafe :: Scope -> NText -> Fish  ()
delVarSafe scp ident
  | ReadOnlyScope <- scp = errork "Will not delete readonly variable"  
  | True = asLens scp %= Env.delete ident

withTextVar :: (T.Text -> a) -> Var -> a
withTextVar f var = f $ T.unwords (var ^. value)

readVar :: Read a => Var -> a
readVar = withTextVar readText

readVarMaybe :: Read a => Var -> Maybe a
readVarMaybe = withTextVar readTextMaybe

mapSerialVar :: (Read a,Show b) => (a -> b) -> Var -> Var
mapSerialVar f = value %~ map (showText . f . readText)



