{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Read (
  readF
) where

import Fish.Lang

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.SetCommand

import qualified Data.Text as T
import Data.Monoid
import Data.Text.IO as TextIO
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit

import Options.Applicative
import Options.Applicative.Builder as OB

-- Todo: support IFS variable for customizing the variable splitting

readF :: Bool -> [T.Text] -> Fish ()
readF _ ts =
  let res = execParserPure defaultPrefs parser (map T.unpack ts)
  in case res of
    Success f -> f
    Failure err ->
      (errork . T.pack . fst)
       (renderFailure err "read: invalid arguments given\n")
  where
    parser = info readOptions idm

readOptions = readWorker
  <$> switch (short 'a' <> long "array")
  <*> switch (short 'z' <> long "null")
  <*> scope
  <*> export
  <*> some ( OB.argument text (metavar "NAMES...") )
  where
    text = maybeReader (Just . T.pack)
    
    export = 
      flag Nothing (Just Export) (short 'x' <> long "export") <|> 
      flag Nothing (Just UnExport) (short 'u' <> long "unexport")
    
    scope =
      flag Nothing (Just ScopeLocal)
        (short 'l' <> long "local")
      <|> flag Nothing (Just ScopeGlobal)
        (short 'g' <> long "global")
      <|> flag Nothing (Just ScopeUniversal)
        (short 'U' <> long "universal")

readWorker
  :: Bool
  -> Bool
  -> Maybe Scope
  -> Maybe Export
  -> [T.Text]
  -> Fish ()
readWorker array nullTerm mscp mex names
  | array && nullTerm =
    case names of
      [name] -> do
        s <- readFrom Fd0
        setV name (T.lines s)
        ok
      _ -> arrayWrongArgNumErr
  | array = case names of
    [name] -> do
      l <- readLineFrom Fd0
      setV name (T.words l)
      ok
    _ -> arrayWrongArgNumErr
  | nullTerm = do
    s <- readFrom Fd0
    assignLoop names (T.lines s)
    ok
  | otherwise = do
    l <- readLineFrom Fd0
    assignLoop names (T.words l)
    ok
  where
    setV = flip setIt
    
    setIt vs ident = 
      collectSetupData ident mscp mex
        $ \env ex scp -> 
          setVarSafe env ident (Var ex vs)
    
    assignLoop [name] wds =
      void $ setIt wds name
    assignLoop (name:names) (w:wds) = do
      setIt [w] name
      assignLoop names wds
    assignLoop names [] = 
      forM_ names $ setIt []
    
    arrayWrongArgNumErr = errork
      $ "read: expecting exactly one identifier with -a"
