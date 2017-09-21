{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module HFish.Startup
  ( executeStartupFiles )
where

import HFish.Interpreter.Core
import HFish.Interpreter.Parsing
import HFish.Types
import HFish.Main.Interactive (runProgram)
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Exception
import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import Data.Maybe


(<$$>) = flip (<$>)

executeStartupFiles :: MonadIO io
  => FishCompat -> FishReader -> FishState -> io FishState
executeStartupFiles fishCompat r s = liftIO $ do
  confFiles <- tryListDirectory confDir
    <$$> (>>= prepare confDir)
  funcFiles <- tryListDirectory funcDir
    <$$> (>>= prepare funcDir)
  foldM (tryExecute fishCompat r) s
    $ pure "/etc/hfish/config.hfish"
      ++ confFiles ++ funcFiles
  where
    prepare path x = do
      guard $ isHFishFile x
      pure $ path </> x

    confDir = "/etc/hfish/conf.d"
    funcDir = "/etc/hfish/functions"


tryListDirectory :: FilePath -> IO [FilePath]
tryListDirectory path =
  ignoreIOEx <$> try (listDirectory path)
  where
    ignoreIOEx = \case
      Left (e::IOException) -> []
      Right r -> r

tryExecute :: FishCompat
  -> FishReader -> FishState -> FilePath -> IO FishState
tryExecute (FishCompat fishCompat) r s path =
  try (parseFile fishCompat path) >>= \case
    Left (e::IOException) -> pure s
    Right res -> withProg res (runProgram r s)
      >>= pure . fromMaybe s


isHFishFile :: FilePath -> Bool
isHFishFile p = takeExtensions p == ".hfish"


