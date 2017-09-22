{-# language LambdaCase #-}
module HFish.Main.NonInteractive
  ( runProgram )
where


import Control.Monad.IO.Class
-- import Control.Exception as E
import qualified Data.Text as T
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Core
import Fish.Lang
import Fish.Lang.Base
import Fish.Lang.Unit

runProgram :: MonadIO io
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> io FishState
runProgram r s p = liftIO $ runFish (progA p) r s

{-
runProgram :: MonadIO io
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> io FishState
runProgram r s p = liftIO
  $ E.catch ( runFish (progA p) r s )
  handleAsyncException
  where
    handleAsyncException :: AsyncException -> IO FishState
    handleAsyncException = \case
      UserInterrupt -> pure s
      e -> E.throwIO e
-}
