{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Interpreter where

import Fish.Lang hiding (Scope)
import qualified Fish.Lang as L

import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.FdTable as FDT
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import HFish.Interpreter.Var
import HFish.Interpreter.Cwd
import HFish.Interpreter.Globbed
import HFish.Interpreter.Process.Process
import HFish.Interpreter.Process.Pid
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Slice
import HFish.Interpreter.Util
import HFish.Interpreter.ExMode
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str
import qualified HFish.Interpreter.SetCmd as SetCmd
import qualified HFish.Interpreter.FuncSt as FuncSt

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.Sequence
import Data.NText
import Data.Monoid
import Data.Maybe
import Data.Bool
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import qualified System.Posix.IO as PIO


progA :: Prog T.Text t -> Fish ()
progA (Prog _ cstmts) = forM_ cstmts compStmtA

compStmtA :: CompStmt T.Text t -> Fish ()
compStmtA = \case
  Simple _ st -> simpleStmtA st
  Piped _ d st cst -> pipedStmtA d st cst
  Forked _ st -> stmtA ForkedExM st

simpleStmtA :: Stmt T.Text t -> Fish ()
simpleStmtA = stmtA InOrderExM

pipedStmtA :: Fd -> Stmt T.Text t -> CompStmt T.Text t -> Fish ()
pipedStmtA fd st cst = 
  pipeFish fd (stmtA PipeExM st) (compStmtA cst)

stmtA :: ExMode -> Stmt T.Text t -> Fish ()
stmtA mode = \case
  CmdSt _ i args ->
    cmdStA mode i args
  SetSt _ setCmd -> 
    setStA setCmd
  FunctionSt _ i args prog ->
    functionStA i args prog
  WhileSt _ st prog ->
    whileStA st prog
  ForSt _ i args prog ->
    forStA i args prog
  IfSt _ branches elBranch ->
    ifStA (N.toList branches) elBranch
  SwitchSt _ e branches -> 
    switchStA e (N.toList branches)
  BeginSt _ prog -> 
    beginStA prog
  AndSt _ st ->
    andStA st
  OrSt _ st ->
    orStA st
  NotSt _ st ->
    notStA st
  RedirectedSt _ st redirects ->
    redirectedStmtA (stmtA mode st) (N.toList redirects)
  CommentSt _ _ -> return ()

cmdStA :: ExMode -> CmdIdent T.Text t -> Args T.Text t -> Fish ()
cmdStA mode (CmdIdent _ ident) args = do
  ts <- evalArgs args
  let ts' = F.toList ts
  let argStrings = map Str.toString ts'
  bn <- views builtins (`Env.lookup` ident)
  fn <- uses functions (`Env.lookup` ident)
  case (bn,fn) of
    (Just b,_) -> b (isFork mode) ts'
    (_,Just f) -> setReturnK $ f ts
    (Nothing,Nothing) -> do
      pid <- fishCreateProcess identString argStrings
      if isInOrder mode
        then fishWaitForProcess identString pid
        else return ()
  where
    identString = T.unpack $ extractText ident

setStA :: SetCommand T.Text t -> Fish ()
setStA = SetCmd.setCommandA evalArgs evalRef

functionStA :: FunIdent T.Text t -> Args T.Text t -> Prog T.Text t -> Fish ()
functionStA ident args prog = evalArgs args
  >>= \ts -> FuncSt.funcStA progA ident ts prog


whileStA :: Stmt T.Text t -> Prog T.Text t -> Fish ()
whileStA st prog = setBreakK loop
  where
    body = progA prog
    loop = do
      simpleStmtA st
      ifOk ( setContinueK body
             >> loop )

forStA :: VarIdent T.Text t -> Args T.Text t -> Prog T.Text t -> Fish ()
forStA (VarIdent _ varIdent) args prog = do
  xs <- evalArgs args
  setBreakK (loop xs)
  where
    lbind x f = localise localEnv $
      setVarSafe
        LocalScope
        varIdent
        (mkVar $ pure x)
      >> f
    
    body = progA prog
    
    loop =
      let f x m = lbind x (setContinueK body) >> m
       in F.foldr f $ return ()

ifStA :: [(Stmt T.Text t,Prog T.Text t)] -> Maybe (Prog T.Text t) -> Fish ()
ifStA [] (Just prog) = progA prog
ifStA [] Nothing = return ()
ifStA ((st,prog):blks) elblk = do
  simpleStmtA st
  onStatus
    (const $ ifStA blks elblk)
    (progA prog >> ok)

switchStA :: Expr T.Text t -> [(Expr T.Text t,Prog T.Text t)] -> Fish ()
switchStA e brnchs = view fishCompatible >>=
  bool (hfishSwitch e brnchs) (fishSwitch e brnchs)

-- | Match a string against a number of glob patterns.
--
--   This implementation deviates strongly from the original fish:
--
--   If given an array instead of a string, it will match against
--   the arrays serialisation.
--
--   Arguments to the /case/ branches are serialised as well
--   and then interpreted as glob patterns.
--
--   These glob patterns are matched directly against the string,
--   superseding the usual glob pattern expansion.
--
--   The matching is lazy and does not fall through, i.e.
--   when a branch is taken all branches following it
-- 
--   will not be evaluated. This seems to agree with the fish impl.
hfishSwitch ::  Expr T.Text t -> [(Expr T.Text t,Prog T.Text t)] -> Fish ()
hfishSwitch e branches = do
  str <- Str.unwords . F.toList <$> evalArg e
  loop str branches
  where
    loop :: Str -> [(Expr T.Text t1, Prog T.Text t2)] -> Fish ()
    loop _ [] = return ()
    loop str ((e,prog):branches) = do
      glob <- mintcal " " <$> evalExpr e
      matchGlobbed glob str & \case
        Just _ -> progA prog
        Nothing -> loop str branches

fishSwitch :: Expr T.Text t -> [(Expr T.Text t,Prog T.Text t)] -> Fish ()
fishSwitch e branches =
  fmap viewl (evalArg e) >>= \case
    EmptyL -> errork "switch: empty statement"
    str :< rest -> if Seq.null rest
      then loop str branches
      else tooManyErr
  where
    loop :: Str -> [(Expr T.Text t1, Prog T.Text t2)] -> Fish ()
    loop _ [] = return ()
    loop str ((e,prog):branches) = do
      globStr <- mintcal " " <$> evalArg e
      matchStr globStr str & \case
        Just _ -> progA prog
        Nothing -> loop str branches
    
    tooManyErr = errork
      $ "switch: too many arguments given"

beginStA :: Prog T.Text t -> Fish ()
beginStA prog =
  localise localEnv
  $ progA prog

andStA :: Stmt T.Text t -> Fish ()
andStA st = ifOk $ simpleStmtA st
  
orStA :: Stmt T.Text t -> Fish ()
orStA st = unlessOk $ simpleStmtA st

notStA :: Stmt T.Text t -> Fish ()
notStA st = 
  simpleStmtA st >> invertStatus

redirectedStmtA :: Fish () -> [Redirect T.Text t] -> Fish ()
redirectedStmtA f redirects = void (setupAll f)
  where
    setupAll :: Fish () -> Fish ()
    setupAll = foldr ((.) . setup) id redirects

    -- setup :: Redirect T.Text t1 -> Fish () -> Fish ()
    setup red f = red & \case
      RedirectClose fd -> close fd f
      RedirectIn fd t -> t & \case
        Left fd2 -> duplicate fd2 fd f
        Right e -> do
          name <- evalArg e >>= checkSingleton
          withFileR (Str.toString name) fd f
      RedirectOut fd t -> t & \case
        Left fd2 -> duplicate fd2 fd f
        Right (mode,e) -> do
          name <- evalArg e >>= checkSingleton
          withFileW (Str.toString name) mode fd f
    
    checkSingleton :: Seq a -> Fish a
    checkSingleton xs = case viewl xs of
      EmptyL -> errork "missing file name in redirection"
      x :< xs' -> if Seq.null xs' 
        then return x
        else errork "more then one file name in redirection"

{- Expression evaluation -}
evalArgs :: Args T.Text t -> Fish (Seq Str)
evalArgs (Args _ es) = join . Seq.fromList <$> forM es evalArg

evalArg :: Expr T.Text t -> Fish (Seq Str)
evalArg arg = do
  globs <- evalExpr arg
  vs <- forM globs globExpand
  return (join vs)

evalExpr :: Expr T.Text t -> Fish (Seq Globbed)
evalExpr = \case
  GlobE _ g -> return . pure $ fromGlob g
  ProcE _ e -> evalProcE e
  HomeDirE _ -> evalHomeDirE
  StringE _ t -> return . pure . fromStr $ encodeUtf8 t
  VarRefE _ q vref -> evalVarRefE q vref
  BracesE _ es -> evalBracesE es
  CmdSubstE _ cmdref -> evalCmdSubstE cmdref
  ConcatE _ e1 e2 -> evalConcatE e1 e2

evalProcE :: Expr T.Text t -> Fish (Seq Globbed)
evalProcE e =
  evalArg e >>= (getPID . F.fold)

evalHomeDirE :: Fish (Seq Globbed)
evalHomeDirE = getHOME
  <$$> pure . fromStr

evalBracesE :: [Expr T.Text t] -> Fish (Seq Globbed)
evalBracesE es = 
  join . Seq.fromList <$> forM es evalExpr

evalCmdSubstE :: CmdRef T.Text t -> Fish (Seq Globbed)
evalCmdSubstE (CmdRef _ prog ref) = do
  (mvar,wE) <- createHandleMVarPair
  FDT.insert Fd1 wE (progA prog) `finally` PIO.closeFd wE
  str <- liftIO $ takeMVar mvar
  Seq.fromList (Str.lines str) & \xs ->
    fmap fromStr <$> case ref of
      Nothing -> return $ xs
      Just _ -> do
        indices <- evalRef ref
        readIndices indices (mkVar xs)

evalVarRefE :: Bool -> VarRef T.Text t -> Fish (Seq Globbed)
evalVarRefE s vref = evalVarRef vref
  <$$> if s then ser else fmap fromStr
  where
    ser = pure . fromStr . Str.unwords . F.toList

evalVarRef :: VarRef T.Text t -> Fish (Seq Str)
evalVarRef (VarRef _ name ref) = do
  varIdents <- evalName name
  vs <- forM varIdents lookupVar
  return (join vs)
  where
    lookupVar ident = do
      var@(Var _ ts) <- getVar ident
      if isNothing ref
        then return ts
        else do
          indices <- evalRef ref
          readIndices indices var
    
    evalName = \case
      Left vref -> fmap (mkNText . decodeUtf8) <$> evalVarRef vref
      Right (VarIdent _ i) -> return (pure i)
    

evalRef :: Ref (Expr T.Text t) -> Fish (Seq (Int,Int))
evalRef ref =
  join . Seq.fromList <$> forM (onMaybe ref mempty id) indices
  where
    indices = \case
      Index a -> (\xs -> Seq.zip xs xs) <$> evalInt a
      Range a b -> liftA2 (,) <$> evalInt a <*> evalInt b
  
evalConcatE :: Expr T.Text t -> Expr T.Text t -> Fish (Seq Globbed)
evalConcatE e1 e2 = do
  gs1 <- evalExpr e1
  gs2 <- evalExpr e2
  return $ fmap Globbed
    (cartesian (fmap unGlob gs1) (fmap unGlob gs2))
  where
    cartesian = liftA2 (<>)

{- Try to interpret Expression as an Int -}

evalInt :: Expr T.Text t -> Fish (Seq Int)
evalInt e = do
  vs <- evalArg e
  forM (Seq.fromList . Str.words =<< vs) f
  where
    f :: Str -> Fish Int
    f v = maybe (errork $ mkerr v) return $ Str.readStrMaybe v
    mkerr v = "failed to interpret expression "
           <> "as integer: " <> Str.toString v


    
