{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.String (
  stringF
) where

import HFish.Interpreter.Core hiding (value)
import HFish.Interpreter.IO
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str

import qualified Data.Text as T
import Data.Functor
import Data.Bifunctor
import Data.Monoid
import Data.Maybe
import Data.Char (isSpace)
import Data.Bool
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.Environment

import Options.Applicative
import Options.Applicative.Builder as OB


{- TODO: escape -> wait for UnParser,
         match, replace -}

stringF :: Builtin
stringF _ ts = 
  execParserPure defaultPrefs parser (map Str.toString ts)
  & \case
    Success f -> f
    Failure err -> errork . fst
      $ renderFailure err "string: invalid arguments given\n"
  where
    parser = info opts idm
    opts = subparser $ mconcat
      [ cmd "length" lengthOpt
       ,cmd "sub" subOpt
       ,cmd "join" joinOpt
       ,cmd "split" splitOpt
       ,cmd "trim" trimOpt ]
    
    cmd n p = OB.command n (info p idm)
    rest = many $ OB.argument text (metavar "STRINGS...")
    
    lengthOpt = lengthF
      <$> switch (short 'q' <> long "quiet")
      <*> rest
    subOpt = subF
      <$> switch (short 'q' <> long "quiet")
      <*> option auto (short 's' <> long "start" <> metavar "START" <> value 1)
      <*> option (Just <$> auto) (short 'l' <> long "length" <> metavar "LENGTH" <> value Nothing)
      <*> rest
    joinOpt = joinF
      <$> switch (short 'q' <> long "quiet")
      <*> OB.argument text (metavar "SEP")
      <*> rest
    trimOpt = trimF
      <$> switch (short 'q' <> long "quiet")
      <*> switch (short 'l' <> long "left")
      <*> switch (short 'r' <> long "right")
      <*> option text (short 'c' <> long "chars" <> metavar "CHARS")
      <*> rest
    splitOpt = splitF
      <$> switch (short 'q' <> long "quiet")
      <*> option (Just <$> auto) (short 'm' <> long "max" <> metavar "MAX" <> value Nothing)
      <*> switch (short 'r' <> long "right")
      <*> OB.argument text (metavar "SEP")
      <*> rest
    text = maybeReader (Just . T.pack)
    
lengthF :: Bool -> [T.Text] -> Fish ()
lengthF q ts = do
  unless q $ forM_ ts (echo . T.pack . show . T.length)
  if all (==T.empty) ts then bad else ok

subF :: Bool -> Int -> Maybe Int -> [T.Text] -> Fish ()
subF q start mlen ts = 
  map sub ts & \ts' -> do
  unless q $ echo $ T.unlines ts'
  if ts == ts' then bad else ok
  where
    sub = (maybe id T.take mlen) . T.drop (start-1)    

joinF :: Bool -> T.Text -> [T.Text] -> Fish ()
joinF q sep ts = 
  T.intercalate sep ts & \ts' -> do
  unless q $ echo ts'
  ts & \case
    [] -> bad
    [_] -> bad
    _ -> ok

trimF :: Bool -> Bool -> Bool -> T.Text -> [T.Text] -> Fish ()
trimF q l r cs ts = 
  map trim ts & \ts' -> do
  unless q $ echo $ T.unlines ts'
  if ts == ts' then bad else ok
  where
    trim = f r l (`inText` cs)
    f True False = T.dropWhileEnd
    f False True = T.dropWhile
    f _ _ = T.dropAround


splitF :: Bool -> Maybe Int -> Bool -> T.Text -> [T.Text] -> Fish ()
splitF q m r sep ts = 
  map work ts & \ts' -> do
  unless q $ echo $ T.unlines ts'
  if ts == ts' then bad else ok
  where
    work :: T.Text -> T.Text
    work t = T.splitOn sep t
      & \ts -> length ts
        & \l -> fromMaybe l m 
          & \mx -> splitAt (if r then l - mx else mx) ts
            & \(a,b) -> if r 
              then newl (sA a) (nwlA b)
              else newl (nwlA a) (sA b)
    sA = T.intercalate sep
    nwlA = T.intercalate "\n"
    newl a b = a <> (if a == "" || b == "" then "" else "\n") <> b

inText c t =
  isJust (T.find (==c) t)

