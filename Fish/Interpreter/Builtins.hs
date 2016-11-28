{-# language LambdaCase, GADTs, OverloadedStrings, FlexibleContexts #-}
module Fish.Interpreter.Builtins (
  allBuiltins
) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Fish.Lang.Lang
import Fish.Interpreter.Core
import Fish.Interpreter.Interpreter
import Data.Bifunctor
import qualified Data.Map as M

import System.Directory
import System.IO
import System.Exit
import qualified Data.Text as T

import Fish.Interpreter.Builtins.Flow
import Fish.Interpreter.Builtins.Exit
import Fish.Interpreter.Builtins.Cd
import Fish.Interpreter.Builtins.Dirstack
import Fish.Interpreter.Builtins.Echo
import Fish.Interpreter.Builtins.Bool
import Fish.Interpreter.Builtins.Read
import Fish.Interpreter.Builtins.String
import Fish.Interpreter.Builtins.Random
import Fish.Interpreter.Builtins.Seq
import Fish.Interpreter.Builtins.Contains
import Fish.Interpreter.Builtins.Source
import Fish.Interpreter.Builtins.Math
import Fish.Interpreter.Builtins.Eval

allBuiltins :: Env (Bool -> [T.Text] -> Fish ())
allBuiltins =
  M.fromList [
    ("return",returnF)
    ,("break",breakF)
    ,("continue",continueF)
    ,("exit",exitF)
    ,("cd",cdF)
    ,("pushd",pushdF)
    ,("popd",popdF)
    ,("dirs",dirsF)
    ,("echo",echoF)
    ,("read",readF)
    ,("string",stringF)
    ,("random",randomF)
    ,("seq",seqF)
    ,("contains",containsF)
    ,("source",sourceF)
    ,("true",trueF)
    ,("false",falseF)
    ,("math",mathF)
    ,("eval",evalF)
    ,("exec",execF)
  ]

