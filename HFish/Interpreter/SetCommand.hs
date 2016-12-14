{-# language LambdaCase, OverloadedStrings, Strict #-}
module HFish.Interpreter.SetCommand (
  setCommandA
) where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.IO
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.Slice

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Functor
import Data.Monoid
import Data.Maybe
import Data.Bool
import qualified Data.Text as T
import qualified Data.Map as M


-- | Evaluate a SetCommand. We have to pass evalArgs,evalRef
--   explicitly due to the recursive modules problem.
setCommandA :: ( Args t -> Fish [T.Text] )
  -> ( Ref (Expr t) -> Int -> Fish Slices )
  -> SetCommand t
  -> Fish ()
setCommandA evalArgs evalRef = \case
  SetSetting mscp mex varDef args -> 
    let VarDef _ (VarIdent _ ident) ref = varDef
     in evalArgs args >>= setSetting evalRef mscp mex ident ref
  SetList mscp mex namesOnly -> 
    listVars mscp mex namesOnly
  SetQuery mscp mex args -> 
    evalArgs args >>= queryVars mscp mex
  SetErase mscp args -> 
    evalArgs args >>= eraseVars mscp

collectSetupData :: T.Text
  -> Maybe Scope
  -> Maybe Export
  -> (EnvLens Var -> Export -> Maybe [T.Text] -> Fish a)
  -> Fish a
collectSetupData ident mscope mexport k =
  maybe guessScope haveScope mscope
  where
    defEx :: Export -> Export
    defEx = flip fromMaybe mexport
    
    guessScope = getOccurs ident
      >>= \case
        [] -> k (EnvLens flocalEnv) (defEx UnExport) Nothing
        (env,Var ex vs):_ -> k env (defEx ex) (Just vs)
          
    haveScope scope = do
      let env = scopeAsEnvLens scope
      mv <- preuse $ envlens env . ix ident
      onMaybe mv
        ( k env (defEx UnExport) Nothing )
        ( \(Var ex vs) -> k env (defEx ex) (Just vs) )

setSetting :: (Maybe a -> Int -> Fish Slices)
  -> Maybe Scope
  -> Maybe Export
  -> T.Text
  -> Maybe a
  -> [T.Text]
  -> Fish ()
setSetting evalRef mscp mex ident ref args =
  collectSetupData ident mscp mex $ \env ex mvs ->
  if isNothing ref
    then setVarSafe env ident $ Var ex args
    else case mvs of
      Nothing -> errork uninitErr
      Just vs -> do
        slcs <- evalRef ref (length vs)
        vs' <- writeSlices slcs vs args
        setVarSafe env ident $ Var ex vs'
  where
    uninitErr = "set: Trying to set parts of uninitialised variable"

listVars :: Maybe Scope -> Maybe Export -> Bool -> Fish ()
listVars mscope mexport namesOnly =
  case mscope of
    Nothing -> forM_ evironments listVarsEnv
    Just scope -> listVarsEnv (scopeAsEnvLens scope)
  where
    listVarsEnv env = do
      mp <- use $ envlens env
      echo $ showVarEnv namesOnly $ case mexport of
        Nothing -> mp
        Just ex -> case ex of
          Export -> M.filter isExport mp
          UnExport -> M.filter (not . isExport) mp

queryVars :: Maybe Scope -> Maybe Export -> [T.Text] -> Fish ()
queryVars mscope mexport args = do
  i <- (length . filter id) <$> mapM isNotSet args
  echoLn $ showText i
  where
    isNotSet :: T.Text -> Fish Bool
    isNotSet ident = case mscope of
      Nothing -> all id
        <$> forM evironments (flip isNotSetIn ident)
      Just scope -> 
        isNotSetIn (scopeAsEnvLens scope) ident
    
    isNotSetIn :: EnvLens Var -> T.Text -> Fish Bool
    isNotSetIn env ident =
      preuse (envlens env . ix ident) >>= \mv ->
      onMaybe mv (return True) $ \(Var ex _) -> 
        return $ case mexport of
          Nothing -> False
          Just ex' -> ex /= ex'

eraseVars :: Maybe Scope -> [T.Text] -> Fish ()
eraseVars mscope args = 
  forM_ args eraseVar
  where
    eraseVar :: T.Text -> Fish ()
    eraseVar name = 
      case mscope of
        Nothing ->
          let f b env = if b then return b else eraseVarIn env name
           in foldM f False evironments
              >>= bool bad ok
        Just scope -> do
          b <- eraseVarIn (scopeAsEnvLens scope) name
          bool bad ok b
    
    eraseVarIn :: EnvLens Var -> T.Text -> Fish Bool
    eraseVarIn env ident = do
      mv <- preuse $ envlens env . ix ident
      case mv of
        Nothing -> return False
        Just _ -> delVarSafe env ident $> True


scopeAsEnvLens :: Scope -> EnvLens Var
scopeAsEnvLens scope = EnvLens $
  case scope of
    ScopeLocal -> localEnv
    ScopeGlobal -> globalEnv
    ScopeUniversal -> error "universal scope not supported yet."

