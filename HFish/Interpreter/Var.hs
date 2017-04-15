{-# language LambdaCase, GADTs, Rank2Types, OverloadedStrings, TupleSections #-}
module HFish.Interpreter.Var where

import Fish.Lang hiding (Scope)
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Util
import qualified Data.NText as NText
import Data.NText (NText,extractText)
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import Data.Sequence
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
  show (Var UnExport vs) = show vs
  show (Var Export vs) = "(exported) " ++ show vs

mkVar :: Seq Str -> Var
mkVar ts = Var UnExport ts

emptyVar :: Var
emptyVar = Var UnExport mempty

emptyVarXp :: Var
emptyVarXp = Var Export mempty

mkVarXp :: Seq Str -> Var
mkVarXp ts = Var Export ts

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
    $ "Lookup of variable "
    <> Str.toString (extractText ident)
    <> " failed."

getVarValue :: NText -> Fish (Seq Str)
getVarValue t = do
  v <- getVar t
  return (v ^. value)

allVars :: Fish [(NText,Var)]
allVars = do
  envs <- forM scopes (use . asLens)
  return (join . map Env.toList $ envs)

exportVars :: Fish [(NText,Var)]
exportVars = Prelude.filter (isExport . snd) <$> allVars

showVarEnv :: Bool -> Env Var -> String
showVarEnv namesOnly env = unlines $
  Env.toList env <$$>
  ( if namesOnly
    then T.unpack . extractText . fst
    else \(k,Var _ vs) -> unwords
      ( T.unpack (extractText k)
        : Fold.toList (fmap Str.toString vs) ) )

isExport :: Var -> Bool
isExport (Var ex _) = 
  case ex of
    Export -> True
    UnExport -> False

setVar :: Scope -> NText -> Var -> Fish  ()
setVar scp ident var = asLens scp %= insert ident var

modifyVar :: Scope -> NText -> (Var -> Var) -> Fish  ()
modifyVar scp ident f = asLens scp %= Env.adjust f ident

setVarSafe :: Scope -> NText -> Var -> Fish  ()
setVarSafe scp ident var
  | ReadOnlyScope <- scp = errork "Will not set or shadow readonly variable"
  | True = setVar scp ident var

delVarSafe :: Scope -> NText -> Fish  ()
delVarSafe scp ident
  | ReadOnlyScope <- scp = errork "Will not delete readonly variable"
  | True = asLens scp %= Env.delete ident

withVarStr :: Var -> (Str -> a) -> a
withVarStr var f = f . Str.unwords . Fold.toList $ (var ^. value)

readVar :: Read a => Var -> a
readVar var = withVarStr var Str.readStr

readVarMaybe :: Read a => Var -> Maybe a
readVarMaybe var = withVarStr var Str.readStrMaybe





