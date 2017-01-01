{-# language LambdaCase, OverloadedStrings, Strict #-} module HFish.Interpreter.SetCmd where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.IO
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.Slice
import Data.NText
import qualified HFish.Interpreter.Env as Env

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor
import Data.Monoid
import Data.Maybe
import Data.Bool
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Map as M


-- | Evaluate a SetCommand. We have to pass evalArgs,evalRef
--   explicitly due to the recursive modules problem.
setCommandA :: ( Args T.Text t -> Fish [T.Text] )
  -> ( Ref (Expr T.Text t) -> Fish [(Int,Int)] )
  -> SetCommand T.Text t
  -> Fish ()
setCommandA evalArgs evalRef = \case
  SetSetting mscp mex varDef args -> 
    evalArgs args >>= 
      setSetting evalRef mscp mex (extract varDef)
  SetList mscp mex namesOnly -> 
    listVars mscp mex namesOnly
  SetQuery mscp mex args -> 
    evalArgs args >>= queryVars mscp mex
  SetErase mscp varDefs -> 
    eraseVars evalRef mscp $ map extract $ N.toList varDefs
  where
    extract :: VarDef T.Text t -> (NText,Ref (Expr T.Text t))
    extract (VarDef _ (VarIdent _ ident) ref) = (ident,ref)


collectSetupData :: NText
  -> Maybe Scope
  -> Maybe Export
  -> (EnvLens Var -> Export -> Maybe ([T.Text],Int) -> Fish a)
  -> Fish a
collectSetupData ident mscope mexport k =
  maybe guessScope haveScope mscope
  where
    defEx :: Export -> Export
    defEx = flip fromMaybe mexport
    
    guessScope = getOccurs ident
      >>= \case
        [] -> k (EnvLens flocalEnv) (defEx UnExport) Nothing
        (envl,Var ex l vs):_ -> k envl (defEx ex) $ Just (vs,l)
          
    haveScope scope = scopeAsEnvLens scope
      & \envl -> do
        mv <- uses (envlens envl) (`Env.lookup` ident)
        onMaybe mv
          ( k envl (defEx UnExport) Nothing )
          ( \(Var ex l vs) -> k envl (defEx ex) $ Just (vs,l) )

setSetting :: (Ref a -> Fish [(Int,Int)])
  -> Maybe Scope
  -> Maybe Export
  -> (NText,Ref a)
  -> [T.Text]
  -> Fish ()
setSetting evalRef mscp mex (ident,ref) args =
  collectSetupData ident mscp mex $ \envl ex mvs ->
  if isNothing ref
    then setVarSafe envl ident $ Var ex (length args) args
    else case mvs of
      Nothing -> errork uninitErr
      Just (vs,l) -> do
        indices <- evalRef ref
        var <- writeIndices indices (Var ex l vs) args
        setVarSafe envl ident var
  where
    uninitErr = "set: Trying to set parts of uninitialised variable"

listVars :: Maybe Scope -> Maybe Export -> Bool -> Fish ()
listVars mscope mexport namesOnly =
  case mscope of
    Nothing -> forM_ evironments listVarsEnv
    Just scope -> listVarsEnv (scopeAsEnvLens scope)
  where
    listVarsEnv envl = do
      mp <- use $ envlens envl
      echo $ showVarEnv namesOnly $ case mexport of
        Nothing -> mp
        Just ex -> case ex of
          Export -> Env.filter isExport mp
          UnExport -> Env.filter (not . isExport) mp

queryVars :: Maybe Scope -> Maybe Export -> [T.Text] -> Fish ()
queryVars mscope mexport args = do
  i <- (length . filter id) <$> mapM isNotSet (map mkNText args)
  echoLn $ showText i
  where
    isNotSet :: NText -> Fish Bool
    isNotSet ident = case mscope of
      Nothing -> all id
        <$> forM evironments (flip isNotSetIn ident)
      Just scope -> 
        isNotSetIn (scopeAsEnvLens scope) ident
    
    isNotSetIn :: EnvLens Var -> NText -> Fish Bool
    isNotSetIn envl ident =
      uses (envlens envl) (`Env.lookup` ident) >>= \mv ->
      onMaybe mv (return True) $ \(Var ex _ _) -> 
        return $ case mexport of
          Nothing -> False
          Just ex' -> ex /= ex'

eraseVars :: (Ref a -> Fish [(Int,Int)])
  -> Maybe Scope
  -> [(NText,Ref a)]
  -> Fish ()
eraseVars evalRef mscope argsData =
  forM_ argsData eraseVar
  where
    -- eraseVar :: (T.Text,Ref a) -> Fish ()
    eraseVar d = onMaybe mscope
      (eraseNoScope d)
      (eraseWithScope d)
    
    eraseNoScope d =
      let f b envl = if b then return b else eraseVarIn envl d
       in foldM f False evironments >>= bool bad ok
    
    eraseWithScope d scope =
      eraseVarIn (scopeAsEnvLens scope) d
      >>= bool bad ok
    
    
    -- eraseVarIn :: EnvLens Var -> (T.Text,Ref a) -> Fish Bool
    eraseVarIn envl (ident,ref) = do
      mv <- uses (envlens envl) (`Env.lookup` ident)
      onMaybe mv (return False) $ \var -> True <$ do
        evalRef ref >>= \case
          [] -> delVarSafe envl ident
          indices -> 
            setVarSafe envl ident
            =<< dropIndices indices var

scopeAsEnvLens :: Scope -> EnvLens Var
scopeAsEnvLens scope = EnvLens $
  case scope of
    ScopeLocal -> localEnv
    ScopeGlobal -> globalEnv
    ScopeUniversal -> {- universalEnv -}
      error "universal scope not supported yet."

