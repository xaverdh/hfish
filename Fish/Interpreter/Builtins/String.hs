{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Builtins.String (
  stringF
) where

import Fish.Interpreter.Core hiding (value)
import Fish.Interpreter.IO
import Fish.Interpreter.Concurrent
import Fish.Interpreter.Status

import qualified Data.Text as T
import Data.Functor
import Data.Bifunctor
import Data.Monoid
import Data.Maybe
import Data.Char (isSpace)
import Data.Bool
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.Environment

import Options.Applicative.Builder
import Options.Applicative

{- TODO: escape -> wait for UnParser,
         match, replace, -q/--quiet option and status -}

stringF :: Bool -> [T.Text] -> Fish ()
stringF _ ts = 
  let res = execParserPure defaultPrefs parser (map T.unpack ts)
  in case res of
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
    
    cmd n p = command n (info p idm)
    rest = many $ argument text (metavar "STRINGS...")
    
    lengthOpt = lengthF <$> rest
    subOpt = subF
      <$> option auto (short 's' <> long "start" <> metavar "START" <> value 1)
      <*> option (Just <$> auto) (short 'l' <> long "length" <> metavar "LENGTH" <> value Nothing)
      <*> rest
    joinOpt = joinF
      <$> argument text (metavar "SEP")
      <*> rest
    trimOpt = trimF
      <$> switch (short 'l' <> long "left")
      <*> switch (short 'r' <> long "right")
      <*> option text (short 'c' <> long "chars" <> metavar "CHARS")
      <*> rest
    splitOpt = splitF
      <$> option (Just <$> auto) (short 'm' <> long "max" <> metavar "MAX" <> value Nothing)
      <*> switch (short 'r' <> long "right")
      <*> argument text (metavar "SEP")
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
    work t = 
      let ts = T.splitOn sep t
          l = length ts
          mx = fromMaybe l m
          (a,b) = splitAt (if r then l - mx else mx) ts
      in if r 
        then newl (sA a) (nwlA b)
        else newl (nwlA a) (sA b)
    sA = T.intercalate sep
    nwlA = T.intercalate "\n"
    newl a b = a <> (if a == "" || b == "" then "" else "\n") <> b

inText c t =
  isJust (T.find (==c) t)
