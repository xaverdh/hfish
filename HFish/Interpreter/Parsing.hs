{-# language LambdaCase #-}
module HFish.Interpreter.Parsing where

import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as TriR
import qualified HFish.Parser.Trifecta as HFTriP
import qualified Fish.Parser.Trifecta as FTriP
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Control.Monad
import Control.Monad.IO.Class
import Fish.Lang


parseHFishInteractive :: String -> TriR.Result (Prog ())
parseHFishInteractive = Tri.parseString HFTriP.program mempty

parseHFish :: MonadIO m => FilePath -> m (TriR.Result (Prog ()))
parseHFish = Tri.parseFromFileEx HFTriP.program

parseFishInteractive :: String -> TriR.Result (Prog ())
parseFishInteractive = Tri.parseString FTriP.program mempty

parseFish :: MonadIO m => FilePath -> m (TriR.Result (Prog ()))
parseFish = Tri.parseFromFileEx FTriP.program

withProg :: MonadIO m
  => TriR.Result (Prog ())
  -> (Prog () -> m a)
  -> m (Maybe a)
withProg res f = case res of
  TriR.Success p -> Just <$> f p
  TriR.Failure err -> do
    liftIO . putStr $
      "~> Error:\n"
      ++ ( Pretty.displayS
         . Pretty.renderPretty 0.8 80
         $ TriR._errDoc err ) "" -- renderSmart
      ++ "\n~> Occured while parsing interactive statement.\n"
    return Nothing


{-
hybridParseInteractive :: String -> TriR.Result (Prog ())
hybridParseInteractive s =
  case parseHFishInteractive s of
    TriR.Success p -> TriR.Success p
    TriR.Failure err -> 
      case parseFishInteractive s of
        TriR.Success p -> TriR.Success p
        TriR.Failure _ -> TriR.Failure err

hybridParse :: MonadIO m => FilePath -> m (TriR.Result (Prog ()))
hybridParse s = parseHFish s >>= \case
  TriR.Success p -> return $ TriR.Success p
  TriR.Failure err ->
    parseFish s >>= \case
      TriR.Success p -> return $ TriR.Success p
      TriR.Failure _ -> return $ TriR.Failure err
-}

