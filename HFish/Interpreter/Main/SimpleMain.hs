{-# language LambdaCase, OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module HFish.Interpreter.Main.SimpleMain where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Maybe
import Data.List as L
import Control.Exception (try)
import qualified Data.Text as T
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Interpreter.Var
import HFish.Interpreter.Version (version)
import HFish.Interpreter.Description (description)
import Fish.Lang

import System.Console.Haskeline
import System.Environment (getArgs)

import Options.Applicative as O
import Options.Applicative.Builder as OB

import Fish.Pretty
import Text.PrettyPrint.GenericPretty (doc)


main :: IO ()
main = execParserPure conf parser <$> getArgs
  >>= \case
    Success a -> a
    Failure err -> putStrLn . fst $ renderFailure err ""
  where
    conf = OB.defaultPrefs {
      prefDisambiguate = True
      ,prefShowHelpOnError = True
    }
    parser = info (helper <*> (versionOpt <|> hfishOptions))
      (fullDesc
        <> header "hfish: a fish-like shell, written in haskell"
        <> progDescDoc (Just description)
        <> failureCode 1)
    
    versionOpt = flag' (putStrLn version)
      (short 'v' <> long "version" <> help "Show version")

newtype NoExecute = NoExecute Bool
newtype ShowAst = ShowAst Bool
newtype IsCommand = IsCommand Bool
newtype FishCompat = FishCompat Bool

hfishOptions :: O.Parser (IO ())
hfishOptions = hfishMain
  <$> switchAs NoExecute (short 'n' <> long "no-execute"
    <> help "Do not execute, only parse")
  <*> switchAs ShowAst (short 'a' <> long "ast"
    <> help "Show the ast instead of executing")
  <*> switchAs IsCommand (short 'c' <> long "command"
    <> help "Execute command given on commandline")
  <*> switchAs FishCompat (short 'f' <> long "fish-compat"
    <> help "Try to be more fish compatible")
  <*> many (strArgument (metavar "ARGS"))
  where
    switchAs f = fmap f . switch

hfishMain :: NoExecute
  -> ShowAst
  -> IsCommand
  -> FishCompat
  -> [String]
  -> IO ()
hfishMain 
  (NoExecute noExecute)
  (ShowAst showAst)
  (IsCommand isCommand)
  (FishCompat fishCompat)
  args
  | noExecute = execute args (const $ return ())
  | showAst = execute args printAST
  | otherwise = do
    r <- mkInitialFishReader atBreakpoint fishCompat
    s <- executeStartupFile r =<< mkInitialFishState
    if isCommand
      then exDirect args (runProgram r s)
      else case args of
        [] -> runInterpreterLoop fishCompat False r s
        path:args' -> do
          s' <- injectArgs args' r s
          exPath path (runProgram r s')
  where
    executeStartupFile r s =
      try (parseFile fishCompat "/etc/hfish/config.hfish")
      >>= \case
        Left (e::IOException) -> return s
        Right res -> withProg res (runProgram r s)
          >>= return . fromMaybe s
    
    printAST = print . doc
    
    injectArgs xs = runFish
      $ setVar FLocalScope "argv"
        (mkVar . Seq.fromList $ map T.pack xs)
    
    execute = if isCommand then exDirect else exPaths
    
    exDirect xs = withProg' $
      parseInteractive fishCompat
      $ L.unwords xs <> "\n"
    
    exPaths xs = forM_ xs . flip exPath
    
    exPath path f = do
      res <- parseFile fishCompat path
      withProg' res f

    atBreakpoint :: Fish ()
    atBreakpoint = do
      r <- ask
      s <- get
      liftIO $ runInterpreterLoop fishCompat True r s

mkPrompt :: Bool -> FishState -> String
mkPrompt isbrkpt s
  | isbrkpt = insStatus <> ": "
  | otherwise = "~" <> insStatus <> "> "
  where
    insStatus = (show . fromEnum) (s ^. status)

runInterpreterLoop :: Bool
  -> Bool
  -> FishReader
  -> FishState
  -> IO ()
runInterpreterLoop fishCompat isbrkpt r s =
  runInputT defaultSettings
    ( interpreterLoop fishCompat (mkPrompt isbrkpt) r s )

interpreterLoop :: Bool
  -> (FishState -> String) -- The prompt
  -> FishReader -> FishState -> InputT IO ()
interpreterLoop fishCompat prompt r s =
  getInputLine (prompt s) >>= \case
    Nothing -> return () -- ctrl-d
    Just l -> do
      ms' <- withProg
        (parseInteractive fishCompat $ l ++ "\n")
        (runProgram r s)
      interpreterLoop fishCompat prompt r (fromMaybe s ms')

coerce :: IO (Either SomeException a) -> IO (Either SomeException a)
coerce = id

runProgram :: MonadIO m
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> m FishState
runProgram r s p =
  ( liftIO . coerce . try $ runFish (progA p) r s ) >>= \case
    Left e -> do
      liftIO . putStr $
        "~> Error:\n"
        ++ show e
        ++ "\n~> Occured while evaluating:\n"
        ++ show (doc p)
        ++ "\n"
      return s
    Right s' -> return s'

