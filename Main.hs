{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import HFish.Main (hfishMain)
import HFish.Types

import qualified HFish.Interpreter.Version as IV
import qualified HFish.Version as V
import qualified Fish.Lang.Version as LV
import qualified Fish.Parser.Version as FPV
import qualified HFish.Parser.Version as HFPV
import HFish.Description (description)

import Options.Applicative as O
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import System.Environment (getArgs,getProgName)
import System.Exit
import Data.Semigroup


main :: IO ()
main = execParserPure conf parser <$> getArgs
  >>= \case
    Success a -> a
    Failure e -> showError e
    CompletionInvoked compl -> do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      exitSuccess
  where
    showError :: ParserFailure ParserHelp -> IO ()
    showError e = putStrLn . fst
      $ renderFailure e "hfish"

    conf = OB.defaultPrefs {
      prefDisambiguate = True
      ,prefShowHelpOnError = True
    }

    parser = info (helper <*> (versionOpt <|> hfishOptions))
      (fullDesc
        <> header "hfish: a fish-like shell, written in haskell"
        <> progDescDoc (Just description)
        <> failureCode 1)

    versionOpt = flag' (PP.putDoc versionInfo)
      (short 'v' <> long "version" <> help "Show version")

    versionInfo = PP.vsep
      [ "Main: " <> PP.string V.version
      , "Interpreter: " <> PP.string IV.version
      , "Parser: " <> PP.string HFPV.version
      , "Parser (fish-compat): " <> PP.string FPV.version
      , "Lang: " <> PP.string LV.version ]
      <> PP.hardline



hfishOptions :: O.Parser (IO ())
hfishOptions = pure hfishMain
  <*> noExecSwitch
  <*> ( astFlag <|> fullAstFlag <|> pure NoAst )
  <*> cmdSwitch
  <*> compatSwitch
  <*> passedArgs
  where
    switchAs f = fmap f . switch

    astFlag = flag' (ShowAst False)
      ( short 'a'
        <> long "ast"
        <> help "Show the ast instead of executing" )

    fullAstFlag = flag' (ShowAst True)
      ( long "full-ast"
        <> help "Show the full (tagged) ast instead of executing" )

    noExecSwitch = switchAs NoExecute
      ( short 'n'
        <> long "no-execute"
        <> help "Do not execute, only parse" )

    cmdSwitch = switchAs IsCommand
      ( short 'c'
        <> long "command"
        <> help "Execute commands given on commandline" )

    compatSwitch = switchAs FishCompat
      ( short 'f'
        <> long "fish-compat"
        <> help "Try to be more fish compatible" )

    passedArgs = many $ strArgument $ metavar "ARGS"

