{-# language LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module HFish.Interpreter.SetCmd where

import Fish.Lang hiding (Scope)
import qualified Fish.Lang as L
import HFish.Interpreter.Scope as VS
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
  -> Maybe L.Scope
  -> Maybe Export
  -> (Scope -> Export -> Maybe ([T.Text],Int) -> Fish a)
  -> Fish a
collectSetupData ident mlscope mexport k =
  maybe guessScope haveScope mlscope
  where
    defEx :: Export -> Export
    defEx = flip fromMaybe mexport
    
    -- guessScope :: Fish a
    guessScope = getOccurs ident
      >>= \case
        [] -> k FLocalScope (defEx UnExport) Nothing
        (scp,Var ex l vs):_ -> k scp (defEx ex) $ Just (vs,l)
    
    -- haveScope :: L.Scope -> Fish a
    haveScope scope = fromLangScope scope
      & \scp -> do
        mv <- uses (asLens scp) (`Env.lookup` ident)
        onMaybe mv
          ( k scp (defEx UnExport) Nothing )
          ( \(Var ex l vs) -> k scp (defEx ex) $ Just (vs,l) )

setSetting :: (Ref a -> Fish [(Int,Int)])
  -> Maybe L.Scope
  -> Maybe Export
  -> (NText,Ref a)
  -> [T.Text]
  -> Fish ()
setSetting evalRef mscp mex (ident,ref) args =
  collectSetupData ident mscp mex $ \scp ex mvs ->
  if isNothing ref
    then setVarSafe scp ident $ Var ex (length args) args
    else case mvs of
      Nothing -> errork uninitErr
      Just (vs,l) -> do
        indices <- evalRef ref
        var <- writeIndices indices (Var ex l vs) args
        setVarSafe scp ident var
  where
    uninitErr = "set: Trying to set parts of uninitialised variable"

listVars :: Maybe L.Scope -> Maybe Export -> Bool -> Fish ()
listVars mlscope mexport namesOnly =
  case mlscope of
    Nothing -> forM_ scopes listVarsScope
    Just lscope -> listVarsScope (fromLangScope lscope)
  where
    listVarsScope :: Scope -> Fish ()
    listVarsScope scp = do
      mp <- use $ asLens scp
      echo $ showVarEnv namesOnly $ case mexport of
        Nothing -> mp
        Just ex -> case ex of
          Export -> Env.filter isExport mp
          UnExport -> Env.filter (not . isExport) mp

queryVars :: Maybe L.Scope -> Maybe Export -> [T.Text] -> Fish ()
queryVars mlscope mexport args = do
  i <- (length . filter id) <$> mapM isNotSet (map mkNText args)
  echoLn $ showText i
  where
    isNotSet :: NText -> Fish Bool
    isNotSet ident = case mlscope of
      Nothing -> all id
        <$> forM scopes (flip isNotSetIn ident)
      Just lscope -> 
        isNotSetIn (fromLangScope lscope) ident
    
    isNotSetIn :: Scope -> NText -> Fish Bool
    isNotSetIn scp ident =
      uses (asLens scp) (`Env.lookup` ident) >>= \mv ->
      onMaybe mv (return True) $ \(Var ex _ _) -> 
        return $ case mexport of
          Nothing -> False
          Just ex' -> ex /= ex'

eraseVars :: (Ref a -> Fish [(Int,Int)])
  -> Maybe L.Scope
  -> [(NText,Ref a)]
  -> Fish ()
eraseVars evalRef mlscope argsData =
  forM_ argsData eraseVar
  where
    -- eraseVar :: (T.Text,Ref a) -> Fish ()
    eraseVar d = onMaybe mlscope
      (eraseNoScope d)
      (eraseWithScope d)
    
    eraseNoScope d =
      let f b scp = if b then return b else eraseVarIn scp d
       in foldM f False scopes >>= bool bad ok
    
    eraseWithScope d scope =
      eraseVarIn (fromLangScope scope) d
      >>= bool bad ok
    
    
    -- eraseVarIn :: _
    eraseVarIn scp (ident,ref) = do
      mv <- uses (asLens scp) (`Env.lookup` ident)
      onMaybe mv (return False) $ \var -> True <$ do
        evalRef ref >>= \case
          [] -> delVarSafe scp ident
          indices -> 
            setVarSafe scp ident
            =<< dropIndices indices var

