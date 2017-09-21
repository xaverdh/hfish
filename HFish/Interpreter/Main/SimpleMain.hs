{-# language LambdaCase, OverloadedStrings #-}
{-# language FlexibleInstances, ScopedTypeVariables #-}
module HFish.Interpreter.Main.SimpleMain where

import Debug.Trace

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Exception as E
import qualified Data.Sequence as Seq
import Data.Semigroup
import Data.Maybe
import Data.Functor
import Data.String (IsString)
import Data.List as L
import qualified Data.Text as T
import HFish.Interpreter.Util
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Scope
import HFish.Interpreter.Core
import HFish.Interpreter.Init
import HFish.Interpreter.Parsing
import HFish.Interpreter.Var
import HFish.Interpreter.Version (version)
import HFish.Interpreter.Description (description)
import qualified HFish.Interpreter.Stringy as Str
import Fish.Lang
import Fish.Lang.Unit
import Fish.Lang.Base
import Fish.Lang.Pretty

import System.Console.Haskeline
import System.Environment (getArgs,getProgName)
import System.Directory (listDirectory)
import System.FilePath (takeExtensions,(</>))
import System.Exit

import Options.Applicative as O
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)

import qualified Text.PrettyPrint.GenericPretty as GP
import qualified Text.PrettyPrint.ANSI.Leijen as PP


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
    
    versionOpt = flag' (putStrLn version)
      (short 'v' <> long "version" <> help "Show version")

data ShowAst = ShowAst Bool | NoAst
newtype NoExecute = NoExecute Bool
newtype IsCommand = IsCommand Bool
newtype FishCompat = FishCompat Bool

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

hfishMain :: NoExecute
  -> ShowAst
  -> IsCommand
  -> FishCompat
  -> [String]
  -> IO ()
hfishMain 
  (NoExecute noExecute)
  showAst
  (IsCommand isCommand)
  (FishCompat fishCompat)
  args
  | noExecute = execute args (const $ pure ())
  | ShowAst b <- showAst = execute args (printAST b)
  | NoAst <- showAst = do
    r <- mkInitialFishReader atBreakpoint fishCompat
    s <- executeStartupFiles fishCompat r =<< mkInitialFishState
    if isCommand
      then exDirect args (runProgram r s)
      else case args of
        [] -> runInterpreterLoop fishCompat False r s
        path:args' -> do
          s' <- injectArgs (map Str.fromString args') r s
          exPath path (runProgram r s')
  where
    printAST full = print . if full then GP.doc else GP.doc . toBase
    
    execute = if isCommand then exDirect else exPaths
    
    exDirect xs = withProg' $
      parseInteractive fishCompat
      $ L.unwords xs <> "\n"
    
    exPaths xs = forM_ xs . flip exPath
    
    exPath path f = do
      res <- parseFile fishCompat path
      withProg' res f

    atBreakpoint :: Fish ()
    atBreakpoint = do
      r <- ask
      s <- get
      liftIO $ runInterpreterLoop fishCompat True r s

injectArgs :: [Str] -> FishReader -> FishState -> IO FishState
injectArgs xs = runFish
  $ setVar FLocalScope "argv"
    (mkVar $ Seq.fromList xs)

executeStartupFiles :: Bool
  -> FishReader -> FishState -> IO FishState
executeStartupFiles fishCompat r s = do
  confFiles <- tryListDirectory confDir
    <$$> (>>= prepare confDir)
  funcFiles <- tryListDirectory funcDir
    <$$> (>>= prepare funcDir)
  foldM tryExecute s
    $ pure "/etc/hfish/config.hfish"
      ++ confFiles ++ funcFiles
  where
    prepare path x = do
      guard $ isHFishFile x
      pure $ path </> x
    
    tryListDirectory :: FilePath -> IO [FilePath]
    tryListDirectory path = try (listDirectory path)
      <$$> \case
        Left (e::IOException) -> []
        Right r -> r
    
    tryExecute :: FishState -> FilePath -> IO FishState
    tryExecute s path = try (parseFile fishCompat path)
      >>= \case
        Left (e::IOException) -> pure s
        Right res -> withProg res (runProgram r s)
          >>= pure . fromMaybe s
    
    
    
    isHFishFile p = takeExtensions p == ".hfish"
    
    confDir = "/etc/hfish/conf.d"
    funcDir = "/etc/hfish/functions"

mkPrompt :: Bool -> FishState -> String
mkPrompt isbrkpt s = show $ case isbrkpt of
  True -> insStatus <> PP.blue ": "
  False -> PP.blue "~" <> insStatus <> PP.blue "> "
  where
    insStatus = case s ^. status of
      ExitSuccess -> PP.blue $ PP.int 0
      ExitFailure i -> PP.red $ PP.int i

runInterpreterLoop :: Bool
  -> Bool
  -> FishReader
  -> FishState
  -> IO ()
runInterpreterLoop fishCompat isbrkpt r s =
  runInputT defaultSettings
    ( interpreterLoop fishCompat (mkPrompt isbrkpt) r s )

interpreterLoop :: Bool
  -> (FishState -> String) -- the prompt
  -> FishReader -> FishState -> InputT IO ()
interpreterLoop fishCompat prompt r s =
  getInputLineIgnoreSigInt (prompt s) >>= \case
    Nothing -> pure () -- ctrl-d
    Just l -> do
      ms' <- withProg
        (parseInteractive fishCompat $ l ++ "\n")
        (runProgram r s)
      interpreterLoop fishCompat prompt r (fromMaybe s ms')


getInputLineIgnoreSigInt :: String -> InputT IO (Maybe String)
getInputLineIgnoreSigInt p = withInterrupt loop
  where
    loop = handleInterrupt handleSigInt (getInputLine p)
    handleSigInt = loop

runProgram :: MonadIO io
  => FishReader
  -> FishState
  -> Prog T.Text ()
  -> io FishState
runProgram r s p = liftIO
  $ E.catches ( runFish (progA p) r s )
  [ E.Handler handleAsyncException
  , E.Handler handleOtherException ]
  where
    handleAsyncException :: AsyncException -> IO FishState
    handleAsyncException = \case
      UserInterrupt -> pure s
      e -> E.throwIO e
    
    handleOtherException :: SomeException -> IO FishState
    handleOtherException e = do
      liftIO $ PP.putDoc (showError e)
      pure s

    showError :: SomeException -> PP.Doc
    showError e = PP.vsep [ showErr e, showTr p ] <> PP.hardline
    
    showErr e = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Error:"
      , (PP.red . PP.string . show) e ]
    
    showTr p = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Occured while evaluating:"
      , (PP.yellow . PP.text . show) (GP.doc $ toBase p) ]


