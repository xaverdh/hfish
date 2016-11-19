{-# language OverloadedStrings, LambdaCase #-}
module Fish.Interpreter.Builtins.Echo (
  echoF
) where

import Fish.Interpreter.Core
import Fish.Interpreter.Status

import qualified Data.Text as T
import Data.Text.IO as TextIO
import Data.Monoid
import Control.Lens
import Control.Monad.IO.Class
import System.Exit
import System.IO

echoF :: Bool -> [T.Text] -> Fish ()
echoF _ = \case
  ("-n":args) -> do
    outH <- view hout
    liftIO . TextIO.hPutStr outH $ T.unwords args
    ok
  args -> do
    outH <- view hout
    liftIO . TextIO.hPutStrLn outH $ T.unwords args
    ok

