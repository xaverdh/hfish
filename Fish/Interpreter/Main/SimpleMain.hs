{-# language LambdaCase, FlexibleInstances #-}
module Fish.Interpreter.Main.SimpleMain where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Maybe
import Data.List as L
import Control.Exception (try)
-- import qualified Text.Trifecta.Parser as Tri
-- import qualified Text.Trifecta.Result as TriR
import qualified Data.Text as T
-- import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
-- import qualified Fish.Parser.Trifecta.Parser as TriP
import Fish.Interpreter.Interpreter
import Fish.Interpreter.Core
import Fish.Interpreter.Init
import Fish.Interpreter.Parsing
import Fish.Lang.Lang

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
    "-c":rest -> do
      withProg (parseFishInteractive (L.unwords rest)) (runProgram r s)
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
      ms' <- withProg (parseFishInteractive l) (runProgram r s)
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



