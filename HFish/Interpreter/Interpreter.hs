{-# language LambdaCase, Rank2Types, OverloadedStrings, BangPatterns #-}
module HFish.Interpreter.Interpreter where

import HFish.Lang.Lang

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

import Data.Bifunctor
import Data.Monoid
import Data.Maybe
import Data.Bool
import Data.List (intercalate)
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar

import System.Process
import System.IO
import System.Posix.IO as P
import System.IO.Temp
import System.Exit
import System.Environment
import System.FilePath

progA :: Prog t -> Fish ()
progA (Prog _ cstmts) = forM_ cstmts compStmtA

compStmtA :: CompStmt t -> Fish ()
compStmtA cst =
  case cst of
    Simple _ st -> simpleStmtA st
    Piped _ d st cst -> pipedStmtA d st cst
    Forked _ st -> stmtA True st

simpleStmtA :: Stmt t -> Fish ()
simpleStmtA = stmtA False

pipedStmtA :: Fd -> Stmt t -> CompStmt t -> Fish ()
pipedStmtA fd st cst = pipeFish
  ( \wE ->
      FDT.insert fd wE (stmtA True st)
      `finally` (P.closeFd wE) )
  ( \rE ->
      FDT.insert Fd0 rE (compStmtA cst) )

stmtA :: Bool -> Stmt t -> Fish ()
stmtA fork = \case
  CmdSt _ i args ->
    cmdStA fork i args
  SetSt _ mvdef -> 
    setStA mvdef
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
    redirectedStmtA fork st (N.toList redirects)
  CommentSt _ _ -> return ()

cmdStA :: Bool -> CmdIdent t -> Args t -> Fish ()
cmdStA fork (CmdIdent _ ident) args = do
  ts <- evalArgs args
  bn <- preview (builtins . ix ident)
  fn <- preuse (functions . ix ident)
  case (bn,fn) of
    (Just b,_) -> b fork ts
    (_,Just f) -> setReturnK $ f ts
    (Nothing,Nothing) -> fishCreateProcess fork ident ts

setStA :: Maybe (VarDef t, Args t) -> Fish ()
setStA = maybe getStA
  $ \(VarDef _ (VarIdent _ ident) ref,args) -> do
    ro <- isReadOnlyVar ident
    when ro (errork readOnlyErr)
    if isNothing ref 
      then setF ident args
      else getVarMaybe ident >>= \case
        Nothing -> errork uninitErr
        Just (Var exp vs) -> do
          ts <- evalArgs args
          slcs <- evalRef ref (length vs)
          let !vs' = writeSlices slcs vs ts
          setVarSafe flocalEnv ident (Var exp vs')
  where
    setF ident args = do
      vs <- evalArgs args
      setVarSafe flocalEnv ident (Var False vs)

    uninitErr = "set: Trying to set parts of uninitialised variable"
    doesNotMatchErr = "set: number of arguments does not match"
    readOnlyErr = "set: Will not set or shadow readonly variable"    

getStA :: Fish ()
getStA = do
  fe <- use flocalEnv
  le <- use localEnv
  ge <- use globalEnv
  ro <- use readOnlyEnv
  echo 
    $ showVars le
    <> showVars fe
    <> showVars ge
    <> showVars ro


{- This is _very_ rudimentary atm. -}
functionStA :: FunIdent t -> Args t -> Prog t -> Fish ()
functionStA (FunIdent _ ident) args prog = 
  modify (functions . at ident .~ Just f)
  where
    f args' =
      localise flocalEnv $ do
        setVar flocalEnv "argv" (Var False args')
        progA prog

whileStA :: Stmt t -> Prog t -> Fish ()
whileStA st prog = setBreakK loop
  where
    body = progA prog
    loop = do
      simpleStmtA st
      use status >>= \case
        ExitSuccess -> do
          setContinueK body
          loop
        _ -> return ()

forStA :: VarIdent t -> Args t -> Prog t -> Fish ()
forStA (VarIdent _ varIdent) args prog = do
  xs <- evalArgs args
  setBreakK (loop xs)
  where
    lbind x f = localise localEnv
      (setVarSafe localEnv varIdent (Var False [x]) >> f)
    
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
  use status >>= \case
    ExitSuccess -> progA prog >> setStatus ExitSuccess
    _ -> ifStA blks elblk
    

-- | Match a string against a number of glob patterns.
--
--   The implementation deviates strongly from the original fish:
--
--   If given an array instead of a string, it will match against
--   the arrays serialisation.
--
--   Arguments to the /case/ branches are serialised as well
--   and then interpreted as glob patterns.
--
--   These glob patterns are matched directly against the string,
--   superseding the usual glob pattern expansion.
switchStA :: Expr t -> [(Expr t,Prog t)] -> Fish ()
switchStA e branches = do
  t <- T.unwords <$> evalArg e
  loop t branches
  where
    loop _ [] = return ()
    loop t ((e,prog):branches) = do
      glob <- mintcal " " <$> evalExpr e
      if isJust ( matchGlobbed glob t )
        then progA prog
        else loop t branches

beginStA :: Prog t -> Fish ()
beginStA prog =
  localise localEnv
  $ progA prog

andStA :: Stmt t -> Fish ()
andStA st =
  getStatus >>= \case
    ExitSuccess -> simpleStmtA st
    ExitFailure _ -> return ()
  
orStA :: Stmt t -> Fish ()
orStA st =
  getStatus >>= \case
    ExitSuccess -> return ()
    ExitFailure _ -> simpleStmtA st

notStA :: Stmt t -> Fish ()
notStA st = do
  simpleStmtA st
  modifyStatus $ \case
    ExitSuccess -> ExitFailure 1
    ExitFailure _ -> ExitSuccess

redirectedStmtA :: Bool -> Stmt t -> [Redirect t] -> Fish ()
redirectedStmtA fork st redirects =
  void (setupAll (stmtA fork st))
  where
    setupAll = foldr ((.) . setup) id redirects

    setup red f = case red of
      RedirectClose fd -> close fd f
      RedirectIn fd t -> case t of
        Left fd2 -> duplicate fd2 fd f
        Right e -> do
          [name] <- evalArg e
          withFileR name fd f
      RedirectOut fd t -> case t of
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
  stMVar <- FDT.insert Fd1 wE (forkFish $ progA prog)
  spliceInState stMVar
  liftIO (P.closeFd wE)
  text <- liftIO $ takeMVar mvar
  let ts = T.lines text
  map fromText
    <$> case ref of
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


{- Restore given scope after fish action exits. -}
localise ::
  ( forall f. Functor f => (b -> f b) -> FishState -> f FishState )
  -> Fish a -> Fish a
localise l f = do
  memory <- use l
  r <- f
  modify (l .~ memory)
  return r
    
