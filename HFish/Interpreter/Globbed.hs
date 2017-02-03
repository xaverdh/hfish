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


fromText :: T.Text -> Globbed
fromText t = Globbed $ pure (Right t)

fromGlob :: Glob -> Globbed
fromGlob g = Globbed $ pure (Left g)

instance IsString Globbed where
  fromString = fromText . T.pack

showGlobbed :: Globbed -> T.Text
showGlobbed = F.fold . fmap f . unGlob
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
      paths <- getMatches <$> recurseDirRel True wdir
      if Seq.null paths
        then noMatchErr globbed
        else return . fmap T.pack $ paths
  where
    parser = genParser globbed
    getMatches = Seq.filter $ isJust . (=~ parser)
    
    noMatchErr globbed = errork
      $ "No matches for glob pattern: "
        <> showGlobbed globbed

optimisticCast :: Globbed -> Maybe Str
optimisticCast = F.foldrM f "" . unGlob
  where
    f mg text = case mg of
      Left _ -> Nothing
      Right s -> Just $ s <> text

matchGlobbed :: Globbed -> Str -> Maybe String
matchGlobbed globbed text = 
  genParser globbed & (T.unpack text =~)

genParser :: Globbed -> RE Char String
genParser = F.fold . fmap f . unGlob
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

recurseDirRel :: Bool -> FilePath -> Fish (Seq FilePath)
recurseDirRel b p = do
  wdir <- use cwdir
  paths <- liftIO (recurseDir b p)
  return $ fmap (makeRelative wdir) paths

recurseDir :: Bool -> FilePath -> IO (Seq FilePath)
recurseDir ignoreHidden p = do
  content <- Seq.fromList
    . map (p </>)
    . Prelude.filter (not . isHidden)
    <$> listDirectory p -- todo catch errors
  mpaths <- forM content continue
  return $ content <> F.foldr catf mempty mpaths
  where
    catf mx pths = case mx of
      Just x -> x <> pths
      Nothing -> pths
    
    continue :: FilePath -> IO (Maybe (Seq FilePath))
    continue p =
      doesDirectoryExist p >>= \case
        True -> do
          files <- recurseDir ignoreHidden p
          return $ Just files
        False -> return Nothing
    
    isHidden p = case takeFileName p of
      '.' : _ -> True
      _ -> False
      -- head (takeFileName p) == '.'
