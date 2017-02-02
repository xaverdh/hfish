{-# language LambdaCase, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module HFish.Interpreter.Globbed (
  Globbed(..)
  ,fromText
  ,fromGlob
  ,fromString
  ,globExpand
  ,matchGlobbed
  ,matchText
) where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Util

import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.Maybe
import Data.Monoid
import Data.String (IsString (..))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import Text.Regex.Applicative

instance Monoid m => Monoid (RE a m) where
  mempty = pure mempty
  re1 `mappend` re2 = (<>) <$> re1 <*> re2

newtype Globbed = Globbed {
    unGlob :: Seq (Either Glob Str)
  }
  deriving (Eq,Ord,Show,Monoid)

{-
instance Monoid Globbed where
  mempty = Globbed mempty
  mappend a b = Globbed (unGlob a <> unGlob b)
-}

fromText :: T.Text -> Globbed
fromText t = Globbed $ pure (Right t)

fromGlob :: Glob -> Globbed
fromGlob g = Globbed $ pure (Left g)

instance IsString Globbed where
  fromString = fromText . T.pack

showGlobbed :: Globbed -> T.Text
showGlobbed = collapse . fmap f . unGlob
  where
    f :: Either Glob T.Text -> T.Text
    f = \case
      Left g -> case g of
        StarGl -> "*"
        DiStarGl -> "**"
        QMarkGl -> "?"
      Right s -> s


globExpand :: Globbed -> Fish (Seq Str)
globExpand globbed = 
  case optimisticCast globbed of
    Just s -> return (pure s)
    Nothing -> do
      wdir <- use cwdir
      paths <- recurseDirRel True wdir
      genParser globbed & \re ->
        mapMaybe (=~ re) paths & \case
          [] -> noMatchErr globbed
          ms -> return . fmap T.pack $ Seq.fromList ms
  where
    noMatchErr globbed = errork
      $ "No matches for glob pattern: "
        <> showGlobbed globbed

optimisticCast :: Globbed -> Maybe T.Text
optimisticCast = F.foldrM f "" . unGlob
  where
    f mg text = case mg of
      Left _ -> Nothing
      Right s -> Just $ s <> text

matchGlobbed :: Globbed -> T.Text -> Maybe String
matchGlobbed globbed text = 
  genParser globbed & (T.unpack text =~)

genParser :: Globbed -> RE Char String
genParser = collapse . fmap f . unGlob
  where
    f = \case
      Right s -> string $ T.unpack s
      Left g -> case g of
        StarGl -> few $ psym (/='/')
        DiStarGl -> few anySym
        QMarkGl -> pure <$> psym (/='/') 

-- Used by fishSwitch
genParserFromText :: T.Text -> RE Char String
genParserFromText = work . T.unpack
  where
    work :: String -> RE Char String
    work = \case
      [] -> pure ""
      g:gs -> (\p -> (++) <$> p <*> work gs)
        $ case g of
          '*' -> few $ anySym
          '?' -> pure <$> anySym
          _ -> pure <$> sym g

-- Used by fishSwitch
matchText :: T.Text -> T.Text -> Maybe String
matchText globText text =
  genParserFromText globText & (T.unpack text =~)

recurseDirRel :: Bool -> FilePath -> Fish [FilePath]
recurseDirRel b p = do
  wdir <- use cwdir
  paths <- liftIO (recurseDir b p)
  return $ map (makeRelative wdir) paths

recurseDir :: Bool -> FilePath -> IO [FilePath]
recurseDir ignoreHidden p = do
  content <-
    map (p </>)
    . Prelude.filter (not . isHidden)
    <$> listDirectory p
  mpaths <- forM content continue
  return $ content ++ join (catMaybes mpaths)
  where
    continue :: FilePath -> IO (Maybe [FilePath])
    continue p =
      doesDirectoryExist p >>= \case
        True -> do
          files <- recurseDir ignoreHidden p
          return $ Just files
        False -> return Nothing
    
    isHidden p =
      head (takeFileName p) == '.'
