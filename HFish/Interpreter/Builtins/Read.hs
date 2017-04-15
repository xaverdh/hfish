{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Read (
  readF
) where

import Fish.Lang as L

import HFish.Interpreter.Core
import HFish.Interpreter.IO
import HFish.Interpreter.Var
import HFish.Interpreter.Status
import HFish.Interpreter.SetCmd
import HFish.Interpreter.Util
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NL
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.NText
import Data.Monoid
import Data.Text.IO as TextIO
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.IO.Class
import System.Exit

import Options.Applicative
import Options.Applicative.Builder as OB


readF :: Builtin
readF _ ts =
  execParserPure defaultPrefs parser (map Str.toString ts)
  & \case
    Success f -> f
    Failure err -> errork . fst
      $ renderFailure err "read: invalid arguments given\n"
  where
    parser = info readOptions idm

readOptions = readWorker
  <$> switch (short 'a' <> long "array")
  <*> switch (short 'z' <> long "null")
  <*> scope
  <*> export
  <*> NL.some1 ( OB.argument text (metavar "NAMES...") )
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
  -> Maybe L.Scope
  -> Maybe Export
  -> NL.NonEmpty T.Text
  -> Fish ()
readWorker array nullTerm mscp mex names
  | array && nullTerm = case NL.uncons names of
    (name,Nothing) -> do
      vs <- readFrom Fd0 >>= splitWords
      setV (mkNText name) vs
      ok
    _ -> arrayWrongArgNumErr
  | array = case NL.uncons names of
    (name,Nothing) -> do
      vs <- readLineFrom Fd0 >>= splitWords
      setV (mkNText name) vs
      ok
    _ -> arrayWrongArgNumErr
  | nullTerm = do
    vs <- readFrom Fd0 >>= splitWords
    assignLoop (fmap mkNText names) vs
    ok
  | otherwise = do
    vs <- readLineFrom Fd0 >>= splitWords
    assignLoop (fmap mkNText names) vs
    ok
  where
    splitWords :: Str -> Fish (Seq Str)
    splitWords s = getVarMaybe "IFS"
      <$$> Seq.fromList . \case
        Just var -> withVarStr var $ \str ->
          let cs = Str.toString str
           in BC.splitWith (`elem` cs) s
        Nothing -> map BC.singleton $ Str.toString s
    
    setV :: NText -> Seq Str -> Fish ()
    setV = flip setIt
    
    setIt :: Seq Str -> NText -> Fish ()
    setIt vs ident = 
      collectSetupData ident mscp mex
        $ \env ex _ -> 
          setVarSafe env ident (Var ex vs)
    
    assignLoop :: NL.NonEmpty NText -> Seq Str -> Fish ()
    assignLoop names vs = case NL.uncons names of
      (name,Nothing) -> void $ setIt vs name
      (name,Just names') -> case viewl vs of
        EmptyL -> forM_ names' $ setIt mempty
        w :< ws -> do
          setIt (pure w) name
          assignLoop names' ws
    
    arrayWrongArgNumErr = errork
      $ "read: expecting exactly one identifier with -a"

