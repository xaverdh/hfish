{-# language LambdaCase, FlexibleInstances #-}
module HFish.Interpreter.Main.SimpleMain where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Maybe
import Data.List as L
import Control.Exception (try)
import qualified Data.Text as T
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Lang.Lang

import System.Console.Haskeline
import System.Environment (getArgs)


main = do
  args <- getArgs
  s <- mkInitialFishState
  r <- mkInitialFishReader
  case args of
    [] -> runInputT
      defaultSettings
      (simpleInterpreterLoop r s)
    "-p":rest -> void $
      withProg (parseFishInteractive (L.unwords rest <> "\n")) print
    "-c":rest -> do
      withProg (parseFishInteractive (L.unwords rest <> "\n")) (runProgram r s)
      return ()
    path:rest -> do
      res <- parseFish path
      withProg res (runProgram r s)
      return ()


prompt :: FishState -> String
prompt s =
  "~" ++ (show . fromEnum) (s ^. status) ++ "> "

simpleInterpreterLoop :: FishReader -> FishState -> InputT IO ()
simpleInterpreterLoop r s =
  getInputLine (prompt s) >>= \case
    Nothing -> return ()
    Just l -> do
      ms' <- withProg (parseFishInteractive $ l ++ "\n") (runProgram r s)
      simpleInterpreterLoop r (fromMaybe s ms')

coerce :: IO (Either SomeException a) -> IO (Either SomeException a)
coerce = id

runProgram :: MonadIO m => FishReader -> FishState -> Prog () -> m FishState
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



