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

main :: IO ()
main = customExecParser conf parser >>= id
  where
    conf = OB.defaultPrefs {
      prefDisambiguate = True
      ,prefShowHelpOnError = True
      ,prefShowHelpOnEmpty = True
    }
    parser = info hfishOptions idm

hfishOptions :: O.Parser (IO ())
hfishOptions = hfishMain
  <$> switch (short 'n' <> long "no-execute")
  <*> switch (short 'a' <> long "ast")
  <*> switch (short 'c' <> long "command")
  <*> many (strArgument (metavar "ARGS"))

hfishMain :: Bool -> Bool -> Bool -> [String] -> IO ()
hfishMain noexecute ast command args
  | noexecute = forM_ args parseHFish
  | ast && command = exDirect args print
  | ast = case args of
    [] -> putStrLn "hfish: missing argument."
    path:_ -> exPath path print
  | otherwise = do
    s <- mkInitialFishState
    r <- mkInitialFishReader atBreakpoint
    if command
      then exDirect args (runProgram r s)
      else case args of
        [] -> runInterpreterLoop False r s
        path:args' -> do
          s' <- injectArgs args' r s
          exPath path (runProgram r s')
  where
    injectArgs args = runFish
      $ setVar (EnvLens flocalEnv)
        "argv" (Var UnExport $ map T.pack args)
      
    exDirect args = withProg'
      $ parseHFishInteractive
      $ L.unwords args <> "\n"
    
    exPath path f = do
      res <- parseHFish path
      withProg' res f

mkPrompt :: Bool -> FishState -> String
mkPrompt isbrkpt s
  | isbrkpt = insStatus <> ": "
  | otherwise = "~" <> insStatus <> "> "
  where
    insStatus = (show . fromEnum) (s ^. status)

runInterpreterLoop :: Bool -> FishReader -> FishState -> IO ()
runInterpreterLoop isbrkpt r s =
  runInputT defaultSettings
    ( interpreterLoop (mkPrompt isbrkpt) r s )

interpreterLoop ::
  (FishState -> String) -- The prompt
  -> FishReader -> FishState -> InputT IO ()
interpreterLoop prompt r s =
  getInputLine (prompt s) >>= \case
    Nothing -> return ()
    Just l -> do
      ms' <- withProg (parseHFishInteractive $ l ++ "\n") (runProgram r s)
      interpreterLoop prompt r (fromMaybe s ms')

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
        ++ show p
        -- ++ "\n~> From code: "
        -- ++ unparse (fmap T.unpack p)
        ++ "\n"
      return s
    Right s' -> return s'

atBreakpoint :: Fish ()
atBreakpoint = do
  r <- ask
  s <- get
  liftIO $ runInterpreterLoop True r s
