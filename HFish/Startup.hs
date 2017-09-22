{-# LANGUAGE LambdaCase, ScopedTypeVariables, OverloadedStrings #-}
module HFish.Startup
  ( doStartup
  , setFileErrorK )
where

import HFish.Interpreter.Core
import HFish.Interpreter.Parsing
import HFish.Interpreter.IO (echo)
import HFish.Types
import HFish.Utils
import HFish.Dispatch
import HFish.Main.NonInteractive (runProgram)

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Exception
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Data.Semigroup
import Data.Maybe

import qualified Text.PrettyPrint.ANSI.Leijen as PP


setFileErrorK :: FilePath -> Dispatch ()
setFileErrorK fpath = dReader . errorK .= [handle]
  where
    handle :: String -> Fish ()
    handle e = echo . show $
      showErr e <> PP.hardline
      <> PP.magenta "~> Ocurred in file: " <> PP.string fpath
      <> PP.hardline

    showErr e = PP.hang 2 $ PP.vsep
      [ PP.magenta "~> Error:"
      , (PP.red . PP.string) e ]


doStartup :: Dispatch ()
doStartup = do
  confFiles <- tryListDirectory confDir
            <$$> (>>= prepare confDir)
  funcFiles <- tryListDirectory funcDir
            <$$> (>>= prepare funcDir)
  mapM_ tryExecute (mainConf : funcFiles <> confFiles)
  where
    prepare :: FilePath -> FilePath -> [FilePath]
    prepare path x = do
      guard $ isHFishFile x
      pure $ path </> x

    confDir = "/etc/hfish/conf.d"
    funcDir = "/etc/hfish/functions"
    mainConf = "/etc/hfish/config.hfish"

tryExecute :: FilePath -> Dispatch ()
tryExecute path = do
  FishCompat fcompat <- use dCompat
  whenJustM ( liftIO $ tryParse fcompat )
    $ \res -> do
      setFileErrorK path
      whenJustM ( onState runProgram $ withProg res )
        (dState .=)
  where
    tryParse fcompat = 
      try (parseFile fcompat path) <$$> \case
        Left (e::IOException) -> Nothing
        Right res -> Just res
      

tryListDirectory :: FilePath -> Dispatch [FilePath]
tryListDirectory path = liftIO $
  ignoreIOEx <$> try (listDirectory path)
  where
    ignoreIOEx = \case
      Left (e::IOException) -> []
      Right r -> r


isHFishFile :: FilePath -> Bool
isHFishFile p = takeExtensions p == ".hfish"



