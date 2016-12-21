{-# language LambdaCase #-}
module HFish.Interpreter.Parsing where

import Fish.Lang
import HFish.Interpreter.Core
import qualified HFish.Parser.Trifecta as HFTriP
import qualified Fish.Parser.Trifecta as FTriP
import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as TriR
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Monoid
import System.IO
import qualified Data.ByteString as B

parseInteractive :: Bool -> String -> TriR.Result (Prog ())
parseInteractive fishcompat
  | fishcompat = Tri.parseString FTriP.program mempty
  | otherwise = Tri.parseString HFTriP.program mempty

parseFile :: MonadIO m
  => Bool -> FilePath -> m ( TriR.Result (Prog ()) )
parseFile fishcompat fpath
  | fishcompat = do
    bs <- liftIO $ B.readFile fpath
    return $ Tri.parseByteString FTriP.program mempty bs    
  | otherwise = do
    bs <- liftIO $ B.readFile fpath
    return $ Tri.parseByteString HFTriP.program mempty bs

parseFish :: FilePath -> Fish ( TriR.Result (Prog ()) )
parseFish fpath = do
  compat <- view fishCompatible
  parseFile compat fpath

parseFishInteractive :: FilePath -> Fish ( TriR.Result (Prog ()) )
parseFishInteractive fpath = do
  compat <- view fishCompatible
  return $ parseInteractive compat fpath

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

withProg' :: MonadIO m
  => TriR.Result (Prog ())
  -> (Prog () -> m a)
  -> m ()
withProg' res f = void $ withProg res f

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

