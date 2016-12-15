{-# language LambdaCase, OverloadedStrings, FlexibleInstances #-}
module HFish.Interpreter.Main.SimpleMain where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Monoid
import Data.Maybe
import Data.List as L
import Control.Exception (try)
import qualified Data.Text as T
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Interpreter.Var
import Fish.Lang

import System.Console.Haskeline
import System.Environment (getArgs)

import Options.Applicative as O
import Options.Applicative.Builder as OB

import Fish.Pretty
import Text.PrettyPrint.GenericPretty (doc)
-- import Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = customExecParser conf parser >>= id
  where
    conf = OB.defaultPrefs {
      prefDisambiguate = True
      ,prefShowHelpOnError = True
    }
    parser = info hfishOptions idm

hfishOptions :: O.Parser (IO ())
hfishOptions = hfishMain
  <$> switch (short 'n' <> long "no-execute")
  <*> switch (short 'a' <> long "ast")
  <*> switch (short 'c' <> long "command")
  <*> switch (short 'f' <> long "fish-compat")
  <*> many (strArgument (metavar "ARGS"))

hfishMain :: Bool -> Bool -> Bool -> Bool -> [String] -> IO ()
hfishMain noexecute ast command fishcompat args
  | noexecute = forM_ args parseIt
  | ast && command = exDirect args printAST
  | ast = case args of
    [] -> putStrLn "hfish: missing argument."
    path:_ -> exPath path printAST
  | otherwise = do
    s <- mkInitialFishState
    r <- mkInitialFishReader atBreakpoint
    if command
      then exDirect args (runProgram r s)
      else case args of
        [] -> runInterpreterLoop fishcompat False r s
        path:args' -> do
          s' <- injectArgs args' r s
          exPath path (runProgram r s')
  where
    printAST = print . doc
    
    injectArgs args = runFish
      $ setVar (EnvLens flocalEnv)
        "argv" (Var UnExport $ map T.pack args)
    
    exDirect args = withProg'
      $ parseInteractive
        $ L.unwords args <> "\n"
    
    exPath path f = do
      res <- parseIt path
      withProg' res f

    parseInteractive
      | fishcompat = parseFishInteractive
      | otherwise = parseHFishInteractive 
    
    parseIt
      | fishcompat = parseFish
      | otherwise = parseHFish

    atBreakpoint :: Fish ()
    atBreakpoint = do
      r <- ask
      s <- get
      liftIO $ runInterpreterLoop fishcompat True r s

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
runInterpreterLoop fishcompat isbrkpt r s =
  runInputT defaultSettings

    ( interpreterLoop fishcompat (mkPrompt isbrkpt) r s )

interpreterLoop :: Bool
  -> (FishState -> String) -- The prompt
  -> FishReader -> FishState -> InputT IO ()
interpreterLoop fishcompat prompt r s =
  getInputLine (prompt s) >>= \case
    Nothing -> return ()
    Just l -> do
      ms' <- withProg (parseInteractive $ l ++ "\n") (runProgram r s)
      interpreterLoop fishcompat prompt r (fromMaybe s ms')
  where
    parseInteractive
      | fishcompat = parseFishInteractive
      | otherwise = parseHFishInteractive

coerce :: IO (Either SomeException a) -> IO (Either SomeException a)
coerce = id

runProgram :: MonadIO m
  => FishReader
  -> FishState
  -> Prog ()
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

