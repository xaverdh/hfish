{-# language LambdaCase, ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module HFish.Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.State
import Control.Monad.Reader.Class

import qualified Data.Sequence as Seq
import Data.Semigroup
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T

import HFish.Interpreter.Interpreter
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Interpreter.Var
import HFish.Interpreter.IO (echo)

import HFish.Main.Interactive
import HFish.Main.NonInteractive
import HFish.Types
import HFish.Debug
import HFish.Dispatch
import HFish.Startup (doStartup,setFileErrorK)
import qualified HFish.Interpreter.Str as Str

import Fish.Lang
import Fish.Lang.Unit
import Fish.Lang.Base
import Fish.Lang.Pretty

import System.Console.Haskeline

import qualified Text.PrettyPrint.GenericPretty as GP
import qualified Text.PrettyPrint.ANSI.Leijen as PP


hfishMain :: NoExecute
  -> ShowAst
  -> IsCommand
  -> FishCompat
  -> [Debug]
  -> [String]
  -> IO ()
hfishMain 
  (NoExecute noExecute)
  showAst
  (IsCommand isCommand)
  (fishCompat@(FishCompat fcompat))
  dflags
  args
  | noExecute = execute args ( const $ pure () )
  | ShowAst b <- showAst = execute args (printAST b)
  | NoAst <- showAst = do
    r <- mkInitialFishReader atBreakpoint fcompat 
    s <- mkInitialFishState
    flip evalDispatch
      ( DispatchState r s Nothing fishCompat mempty )
      ( dispatch isCommand dflags args )
  where
    execute :: [String] -> (Prog T.Text () -> IO a) -> IO ()
    execute
      | isCommand = exDirect fcompat . mkCommand
      | otherwise = exPaths
    
    exPaths xs = forM_ xs . flip (exPath fcompat)


atBreakpoint :: Fish ()
atBreakpoint = do
  r <- ask
  s <- get
  let fishCompat = FishCompat $ r ^. fishCompatible
  liftIO $ flip evalDispatch
    ( DispatchState r s Nothing fishCompat mempty )
    ( runInterpreterLoop $ IsBreakPoint True )


mkCommand :: [String] -> String
mkCommand args = L.unwords args <> "\n"


dispatch :: Bool -> [Debug] -> [String] -> Dispatch ()
dispatch isCommand dflags args = do
  setDebugFlags dflags
  fishCompat@(FishCompat fcompat) <- use dCompat
  case isCommand of
    True -> do
      let cmd = mkCommand args
      doStartup
      setCmdErrorK cmd
      exDirect fcompat cmd runProgram
    False -> case args of
      [] -> do
        setInteractive
        doStartup
        runInterpreterLoop $ IsBreakPoint False
      path:args' -> do
        doStartup
        injectArgs $ map Str.fromString args'
        setFileErrorK path
        exPath fcompat path runProgram


setCmdErrorK :: String -> Dispatch ()
setCmdErrorK cmd = dOnError .= Just handle
  where
    handle :: String -> Fish ()
    handle e = echo . show $
      showErr e <> PP.hardline
      <> PP.magenta "~> Ocurred in command: " <> PP.string cmd
      <> PP.hardline

    showErr e = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Error:"
      , (PP.red . PP.string) e ]

exDirect :: MonadIO io
  => Bool -> String
  -> ( Prog T.Text () -> io a ) -> io ()
exDirect fcompat cmd = do
  withProg' $ parseInteractive fcompat cmd


exPath :: MonadIO io
  => Bool -> FilePath
  -> ( Prog T.Text () -> io a ) -> io ()
exPath fcompat path f = do
  res <- parseFile fcompat path
  withProg' res f


setInteractive :: Dispatch ()
setInteractive = modify $ dReader . interactive .~ True

setDebugFlags :: [Debug] -> Dispatch ()
setDebugFlags = mapM_ setDFlag
  where
    setDFlag :: Debug -> Dispatch ()
    setDFlag dflag
      | DebugLib d <- dflag = 
        dReader . debugFlags %= S.insert d
      | DebugMain d <- dflag = dDebug %= S.insert d
      

injectArgs :: [Str.Str] -> Dispatch ()
injectArgs xs = do
  s <- liftIO =<< ( runFish inj <$> use dReader <*> use dState )
  dState .= s
  where
    inj = setVar FLocalScope "argv"
          ( mkVar $ Seq.fromList xs )

printAST :: Bool -> Prog T.Text () -> IO ()
printAST full = print
  . if full then GP.doc else GP.doc
  . toBase
    

