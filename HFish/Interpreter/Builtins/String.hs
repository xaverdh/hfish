{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Builtins.String (
  stringF
) where

import HFish.Interpreter.Core hiding (value)
import HFish.Interpreter.IO
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Status

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
         match, replace, -q/--quiet option and status -}

stringF :: Bool -> [T.Text] -> Fish ()
stringF _ ts = 
  execParserPure defaultPrefs parser (map T.unpack ts)
  & \case
    Success f -> f
    Failure err -> 
      (errork . T.pack . fst)
      (renderFailure err "string: invalid arguments given\n")
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
    
    lengthOpt = lengthF <$> rest
    subOpt = subF
      <$> option auto (short 's' <> long "start" <> metavar "START" <> value 1)
      <*> option (Just <$> auto) (short 'l' <> long "length" <> metavar "LENGTH" <> value Nothing)
      <*> rest
    joinOpt = joinF
      <$> OB.argument text (metavar "SEP")
      <*> rest
    trimOpt = trimF
      <$> switch (short 'l' <> long "left")
      <*> switch (short 'r' <> long "right")
      <*> option text (short 'c' <> long "chars" <> metavar "CHARS")
      <*> rest
    splitOpt = splitF
      <$> option (Just <$> auto) (short 'm' <> long "max" <> metavar "MAX" <> value Nothing)
      <*> switch (short 'r' <> long "right")
      <*> OB.argument text (metavar "SEP")
      <*> rest
    text = maybeReader (Just . T.pack)
    
lengthF :: [T.Text] -> Fish ()
lengthF ts = do
  forM_ ts (echo . T.pack . show . T.length)
  ok

subF :: Int -> Maybe Int -> [T.Text] -> Fish ()
subF start mlen ts = do
  echo (T.unlines $ map sub ts)
  ok
  where
    sub = (maybe id T.take mlen) . T.drop (start-1)    

joinF :: T.Text -> [T.Text] -> Fish ()
joinF sep ts = do
  echo (T.intercalate sep ts)
  ok

trimF :: Bool -> Bool -> T.Text -> [T.Text] -> Fish ()
trimF l r cs ts = do
  echo trimmed
  ok
  where
    trimmed = f r l (`inText` cs) (T.unwords ts)
    f True False = T.dropWhileEnd
    f False True = T.dropWhile
    f _ _ = T.dropAround


splitF :: Maybe Int -> Bool -> T.Text -> [T.Text] -> Fish ()
splitF m r sep ts = do
  echo (T.unlines $ map work ts)
  ok  
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

