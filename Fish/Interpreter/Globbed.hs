{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Globbed (
  Globbed(..)
  ,fromText
  ,fromString
  ,globExpand
) where

import Fish.Lang.Lang
import Fish.Interpreter.Core

import Data.Maybe
import qualified Data.Text as T
import Data.Monoid
import Data.String (IsString (..))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import Text.Regex.Applicative

data Globbed = Globbed {
    unGlob :: [Either Glob T.Text]
  }
  deriving (Eq,Ord,Show)

fromText :: T.Text -> Globbed
fromText t = Globbed [Right t]

instance IsString Globbed where
  fromString = fromText . T.pack

showGlobbed :: Globbed -> T.Text
showGlobbed = 
  (\f (Globbed g) -> mconcat $ map f g)
  $ \case
    Left g -> case g of
      StarGl -> "*"
      DiStarGl -> "**"
      QMarkGl -> "?"
    Right s -> s

globExpand :: Globbed -> Fish [T.Text]
globExpand globbed = 
  case optimisticCast globbed of
    Just s -> return [s]
    Nothing -> do
      wdir <- use cwdir
      paths <- recurseDirRel True wdir
      let re = genParser globbed
      let ms = mapMaybe (=~ re) paths
      case ms of
        [] -> errork
          $ "No matches for glob pattern: "
          <> showGlobbed globbed
        _ -> return (map T.pack ms)

optimisticCast :: Globbed -> Maybe T.Text
optimisticCast (Globbed g) = work g
  where
    work = \case
      [] -> Just ""
      (mg:mgs) -> case mg of
        Left _ -> Nothing
        Right s -> (<>) <$> Just s <*> work mgs

genParser :: Globbed -> RE Char String
genParser (Globbed g) = work g
  where 
    work = \case
      [] -> pure ""
      (mg:mgs) ->
        (\p -> (++) <$> p <*> work mgs)
        $ case mg of
          Right s -> string $ T.unpack s
          Left g -> case g of
            StarGl -> few $ psym (/='/')
            DiStarGl -> few anySym
            QMarkGl -> pure <$> psym (/='/') 

recurseDirRel :: Bool -> FilePath -> Fish [FilePath]
recurseDirRel b p = do
  wdir <- use cwdir
  paths <- liftIO (recurseDir b p)
  return $ map (makeRelative wdir) paths

recurseDir :: Bool -> FilePath -> IO [FilePath]
recurseDir ignoreHidden p = do
  content <-
    map (p </>)
    . filter (not . isHidden)
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
