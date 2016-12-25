{-# language LambdaCase #-}
module HFish.Interpreter.Parsing where

import Fish.Lang
import HFish.Interpreter.Core
import qualified HFish.Parser.Trifecta as HFTriP
import qualified Fish.Parser.Trifecta as FTriP
import qualified HFish.Parser.Attoparsec as HFAttoP
import qualified Fish.Parser.Attoparsec as FAttoP

import System.Unix.IO.Text (toUnicode)
import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as TriR
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import Data.Attoparsec.Text as Atto
import Data.Attoparsec.ByteString as BAtto

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as T
import System.IO
import qualified Data.ByteString as B

parseInteractive :: Bool -> String -> TriR.Result (Prog T.Text ())
parseInteractive fishcompat
  | fishcompat = Tri.parseString FTriP.program mempty
  | otherwise = Tri.parseString HFTriP.program mempty

parseFile :: MonadIO m
  => Bool -> FilePath -> m ( TriR.Result (Prog T.Text ()) )
parseFile fishcompat fpath
  | fishcompat = do
    bs <- liftIO $ B.readFile fpath
    text <- liftIO $ toUnicode bs
    case Atto.parseOnly FAttoP.program text of
      Right prog -> return $ TriR.Success prog
      Left _ -> return $ Tri.parseByteString FTriP.program mempty bs    
  | otherwise = do
    bs <- liftIO $ B.readFile fpath
    text <- liftIO $ toUnicode bs
    case Atto.parseOnly HFAttoP.program text of
      Right prog -> return $ TriR.Success prog
      Left _ -> return $ Tri.parseByteString HFTriP.program mempty bs    

parseFish :: FilePath -> Fish ( TriR.Result (Prog T.Text ()) )
parseFish fpath = do
  compat <- view fishCompatible
  parseFile compat fpath

parseFishInteractive :: FilePath -> Fish ( TriR.Result (Prog T.Text ()) )
parseFishInteractive fpath = do
  compat <- view fishCompatible
  return $ parseInteractive compat fpath

withProg :: MonadIO m
  => TriR.Result (Prog T.Text ())
  -> (Prog T.Text () -> m a)
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
  => TriR.Result (Prog T.Text ())
  -> (Prog T.Text () -> m a)
  -> m ()
withProg' res f = void $ withProg res f

