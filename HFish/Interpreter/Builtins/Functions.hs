{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.Functions (
  functionsF
) where

import HFish.Interpreter.Core
import HFish.Interpreter.Util
import HFish.Interpreter.IO
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Stringy as Str

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Bool
import Data.Monoid
import Data.Functor
import Data.NText as NText
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Builder as OB


-- TODO: support -d

functionsF :: Builtin
functionsF _  ts =
  execParserPure defaultPrefs parser (map Str.toString ts)
  & \case
    Success f -> f
    Failure err -> errork . fst
      $ renderFailure err "read: invalid arguments given\n"
  where
    parser = info functionsOptions idm


functionsOptions =
  functionsList
    <$> switch (short 'a' <> long "all")
    <*> switch (short 'n' <> long "names")
  <|> switch (short 'e' <> long "erase")
    *> ( functionsErase <$> args )
  <|> switch (short 'q' <> long "query")
    *> ( functionsQuery <$> args )
  <|> functionsDesc
    <$> ( option text $ short 'd'
          <> long "description"
          <> metavar "DESCRIPTION" )
    <*> arg
  <|> switch (short 'c' <> long "copy")
    *> ( functionsCopy <$> arg <*> arg )
  where
    arg = OB.argument text idm
    args = some arg
    text = maybeReader (Just . T.pack)

functionsList :: Bool -> Bool -> Fish ()
functionsList all _ = do
  names <- uses functions $ map extractText . Env.identifiers
  echo
    . T.unlines
    . bool (filter $ not . T.isPrefixOf "_") id all
    $ names

functionsQuery :: [T.Text] -> Fish ()
functionsQuery fnames = do
  mbs <- forM fnames $ \fn ->
    uses functions (`Env.lookup` mkNText fn)
  bool bad ok
    (all maybeToBool mbs)
  where
    maybeToBool = maybe False (const True)

functionsErase :: [T.Text] -> Fish ()
functionsErase = mapM_ $ 
  \f -> functions %= (Env.delete $ mkNText f)

functionsCopy :: T.Text -> T.Text -> Fish ()
functionsCopy src dst = do
  mf <- uses functions (`Env.lookup` mkNText src)
  onMaybe mf doesNotExist
    $ \f -> functions %= Env.insert (mkNText dst) f
  where
    doesNotExist = errork
      $ "does not exist "
      <> Str.toString src

functionsDesc :: T.Text -> T.Text -> Fish ()
functionsDesc = undefined


