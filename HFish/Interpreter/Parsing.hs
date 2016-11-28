module HFish.Interpreter.Parsing where

import qualified Text.Trifecta.Parser as Tri
import qualified Text.Trifecta.Result as TriR
import qualified HFish.Parser.Trifecta as TriP
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Control.Monad
import Control.Monad.IO.Class
import HFish.Lang.Lang

parseFishInteractive :: String -> TriR.Result (Prog ())
parseFishInteractive = Tri.parseString TriP.program mempty

parseFish ::  MonadIO m => FilePath -> m (TriR.Result (Prog ()))
parseFish = Tri.parseFromFileEx TriP.program

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


