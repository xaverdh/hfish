{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Interpreter where

import Fish.Lang

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
import qualified HFish.Interpreter.SetCmd as SetCmd
import qualified HFish.Interpreter.FuncSt as FuncSt

import Data.Monoid
import Data.Maybe
import Data.Bool
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import qualified System.Posix.IO as PIO


progA :: Prog t -> Fish ()
progA (Prog _ cstmts) = forM_ cstmts compStmtA

compStmtA :: CompStmt t -> Fish ()
compStmtA = \case
  Simple _ st -> simpleStmtA st
  Piped _ d st cst -> pipedStmtA d st cst
  Forked _ st -> stmtA ForkedExM st

simpleStmtA :: Stmt t -> Fish ()
simpleStmtA = stmtA InOrderExM

pipedStmtA :: Fd -> Stmt t -> CompStmt t -> Fish ()
pipedStmtA fd st cst = pipeFish
  ( \wE ->
      FDT.insert fd wE (stmtA PipeExM st)
      `finally` (PIO.closeFd wE) )
  ( \rE ->
      FDT.insert Fd0 rE (compStmtA cst) )

stmtA :: ExMode -> Stmt t -> Fish ()
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

cmdStA :: ExMode -> CmdIdent t -> Args t -> Fish ()
cmdStA mode (CmdIdent _ ident) args = do
  ts <- evalArgs args
  bn <- preview (builtins . ix ident)
  fn <- preuse (functions . ix ident)
  case (bn,fn) of
    (Just b,_) -> b (isFork mode) ts
    (_,Just f) -> setReturnK $ f ts
    (Nothing,Nothing) -> do
      pid <- fishCreateProcess ident ts
      if isInOrder mode
        then fishWaitForProcess ident pid
        else return ()

setStA :: SetCommand t -> Fish ()
setStA = SetCmd.setCommandA evalArgs evalRef

functionStA :: FunIdent t -> Args t -> Prog t -> Fish ()
functionStA ident args prog = evalArgs args
  >>= \ts -> FuncSt.funcStA progA ident ts prog


whileStA :: Stmt t -> Prog t -> Fish ()
whileStA st prog = setBreakK loop
  where
    body = progA prog
    loop = do
      simpleStmtA st
      ifOk ( setContinueK body
             >> loop )

forStA :: VarIdent t -> Args t -> Prog t -> Fish ()
forStA (VarIdent _ varIdent) args prog = do
  xs <- evalArgs args
  setBreakK (loop xs)
  where
    lbind x f = localise localEnv
      (setVarSafe (EnvLens localEnv) varIdent (Var UnExport [x]) >> f)
    
    body = progA prog
    
    loop [] = return ()
    loop (x:xs) = do
      lbind x $ setContinueK body
      loop xs

ifStA :: [(Stmt t,Prog t)] -> Maybe (Prog t) -> Fish ()
ifStA [] (Just prog) = progA prog
ifStA [] Nothing = return ()
ifStA ((st,prog):blks) elblk = do
  simpleStmtA st
  onStatus
    (const $ ifStA blks elblk)
    (progA prog >> ok)

switchStA :: Expr t -> [(Expr t,Prog t)] -> Fish ()
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
--   In addition the matching is lazy, i.e. when a branch is taken
--   all branches following it will not be evaluated.
hfishSwitch ::  Expr t -> [(Expr t,Prog t)] -> Fish ()
hfishSwitch e branches = do
  text <- T.unwords <$> evalArg e
  loop text branches
  where
    loop _ [] = return ()
    loop text ((e,prog):branches) = do
      glob <- mintcal " " <$> evalExpr e
      matchGlobbed glob text & \case
        Just _ -> progA prog
        Nothing -> loop text branches

fishSwitch :: Expr t -> [(Expr t,Prog t)] -> Fish ()
fishSwitch e branches = evalArg e >>= \case
  [text] -> loop text branches
  _ -> tooManyErr
  where
    loop _ [] = return ()
    loop text ((e,prog):branches) = do
      globText <- mintcal " " <$> evalArg e
      matchText globText text & \case
        Just _ -> progA prog
        Nothing -> loop text branches

    tooManyErr = errork
      $ "switch: too many arguments given"

beginStA :: Prog t -> Fish ()
beginStA prog =
  localise localEnv
  $ progA prog

andStA :: Stmt t -> Fish ()
andStA st = ifOk $ simpleStmtA st
  
orStA :: Stmt t -> Fish ()
orStA st = unlessOk $ simpleStmtA st

notStA :: Stmt t -> Fish ()
notStA st = 
  simpleStmtA st >> invertStatus

redirectedStmtA :: Fish () -> [Redirect t] -> Fish ()
redirectedStmtA f redirects = void (setupAll f)
  where
    setupAll = foldr ((.) . setup) id redirects

    setup red f = red & \case
      RedirectClose fd -> close fd f
      RedirectIn fd t -> t & \case
        Left fd2 -> duplicate fd2 fd f
        Right e -> do
          [name] <- evalArg e
          withFileR name fd f
      RedirectOut fd t -> t & \case
        Left fd2 -> duplicate fd2 fd f
        Right (mode,e) -> do
          [name] <- evalArg e
          withFileW name mode fd f


{- Expression evaluation -}
evalArgs :: Args t -> Fish [T.Text]
evalArgs (Args _ es) = join <$> forM es evalArg

evalArg :: Expr t -> Fish [T.Text]
evalArg arg = do
  globs <- evalExpr arg
  vs <- forM globs globExpand
  return (join vs)

evalExpr :: Expr t -> Fish [Globbed]
evalExpr = \case
  GlobE _ g -> return [Globbed [Left g]]
  ProcE _ e -> evalProcE e
  HomeDirE _ -> evalHomeDirE
  StringE _ t -> return [fromText t]
  VarRefE _ q vref -> evalVarRefE q vref
  BracesE _ es -> evalBracesE es
  CmdSubstE _ cmdref -> evalCmdSubstE cmdref
  ConcatE _ e1 e2 -> evalConcatE e1 e2

evalProcE :: Expr t -> Fish [Globbed]
evalProcE e = 
  evalArg e >>= (getPID . T.intercalate "")

evalHomeDirE :: Fish [Globbed]
evalHomeDirE = do
  home <- getHOME
  return [fromString home]

evalBracesE :: [Expr t] -> Fish [Globbed]
evalBracesE es = 
  join <$> forM es evalExpr

evalCmdSubstE :: CmdRef t -> Fish [Globbed]
evalCmdSubstE (CmdRef _ prog ref) = do
  (mvar,wE) <- createHandleMVarPair
  FDT.insert Fd1 wE (progA prog) `finally` PIO.closeFd wE
  text <- liftIO $ takeMVar mvar
  T.lines text & \ts ->
    map fromText <$> case ref of
      Nothing -> return ts
      Just _ -> do
        slcs <- evalRef ref (length ts)
        return (readSlices slcs ts)

evalVarRefE :: Bool -> VarRef t -> Fish [Globbed]
evalVarRefE q vref = do
  vs <- evalVarRef vref
  return $ map fromText (ser vs)
  where
    ser = if q then pure . T.unwords else id

evalVarRef :: VarRef t -> Fish [T.Text]
evalVarRef (VarRef _ name ref) = do
  varIdents <- evalName name
  vs <- forM varIdents lookupVar
  return (join vs)
  where
    lookupVar ident = do
      ts <- getVarValue ident
      if isNothing ref
        then return ts
        else do
          slcs <- evalRef ref (length ts)
          return (readSlices slcs ts)
    evalName = \case
      Left vref -> evalVarRef vref
      Right (VarIdent _ i) -> return [i]
    

evalRef :: Ref (Expr t) -> Int -> Fish Slices
evalRef ref l = do
    ijs <- forM (onMaybe ref [] id) indices
    makeSlices l (join ijs)
  where
    indices = \case
      Index a -> (\xs -> zip xs xs) <$> evalInt a
      Range a b -> liftA2 (,) <$> evalInt a <*> evalInt b  
  
evalConcatE :: Expr t -> Expr t -> Fish [Globbed]
evalConcatE e1 e2 = do
  gs1 <- evalExpr e1
  gs2 <- evalExpr e2
  return $ map Globbed (cartesian (map unGlob gs1) (map unGlob gs2))
  where
    cartesian = liftA2 (<>)

{- Try to interpret Expression as an Int -}

evalInt :: Expr t -> Fish [Int]
evalInt e = do
  vs <- evalArg e
  forM (T.words =<< vs) f
  where
    f v = case readTextMaybe v of
      Just x -> return x
      Nothing -> errork
        $ "failed to interpret expression "
          <> "as integer: " <> v

    
