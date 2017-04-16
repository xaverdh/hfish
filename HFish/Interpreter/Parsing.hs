{-# language LambdaCase, FlexibleInstances #-}
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

class TriParsable s where
  parseTri :: Tri.Parser a -> s -> TriR.Result a

{- Do /not/ use due to unclear Unicode handling.
instance TriParsable B.ByteString where
  parseTri = flip Tri.parseByteString mempty
-}

instance TriParsable String where
  parseTri = flip Tri.parseString mempty

instance TriParsable T.Text where
  parseTri p = Tri.parseString p mempty . T.unpack

parseInteractive :: TriParsable s
  => Bool -> s -> TriR.Result (Prog T.Text ())
parseInteractive fishcompat
  | fishcompat = parseTri FTriP.program
  | otherwise = parseTri HFTriP.program

parseFile :: MonadIO m
  => Bool -> FilePath -> m ( TriR.Result (Prog T.Text ()) )
parseFile fishcompat fpath
  | fishcompat = do
    bs <- liftIO $ B.readFile fpath
    text <- liftIO $ toUnicode bs
    pure $ case Atto.parseOnly FAttoP.program text of
      Right prog -> TriR.Success prog
      Left _ -> parseTri FTriP.program text
  | otherwise = do
    bs <- liftIO $ B.readFile fpath
    text <- liftIO $ toUnicode bs
    pure $ case Atto.parseOnly HFAttoP.program text of
      Right prog -> TriR.Success prog
      Left _ -> parseTri HFTriP.program text

parseFish :: FilePath -> Fish ( TriR.Result (Prog T.Text ()) )
parseFish fpath = do
  compat <- view fishCompatible
  parseFile compat fpath

parseFishInteractive :: TriParsable s
  => s -> Fish ( TriR.Result (Prog T.Text ()) )
parseFishInteractive s = do
  compat <- view fishCompatible
  return $ parseInteractive compat s

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

