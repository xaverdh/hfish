{-# language ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
{-# language FlexibleContexts #-}
module HFish.Interpreter.Builtins.String (
  stringF
) where

import HFish.Interpreter.Core hiding (value)
import HFish.Interpreter.IO
import HFish.Interpreter.Concurrent
import HFish.Interpreter.Status
import qualified HFish.Interpreter.Stringy as Str
import System.Unix.IO (FdWritable(..))

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
         match, replace

   CONVERT ME to operate on bytestrings with a flag for Unicode handling -}


stringF :: Builtin
stringF _ xs = do
  let ts = map T.unpack mbOpts
   in execParserPure defaultPrefs parser ts
    & \case
      Success f -> f
      Failure err -> errork . fst
        $ renderFailure err "string: invalid arguments given\n"
  where
    (mbOpts,rest) = splitAt 10 $ map Str.toText xs
    
    parser = info opts idm

    opts = subparser $ mconcat
      [ cmd "length" lengthOpt
       ,cmd "sub" subOpt
       ,cmd "join" joinOpt
       ,cmd "split" splitOpt
       ,cmd "trim" trimOpt ]
    
    cmd n p = OB.command n (info p idm)
    remainer = fmap (<> rest) . many
      $ OB.argument text (metavar "STRINGS...")
    
    lengthOpt = lengthF
      <$> switch (short 'q' <> long "quiet")
      <*> remainer
    subOpt = subF
      <$> switch (short 'q' <> long "quiet")
      <*> option auto (short 's' <> long "start" <> metavar "START" <> value 1)
      <*> option (Just <$> auto) (short 'l' <> long "length" <> metavar "LENGTH" <> value Nothing)
      <*> remainer
    joinOpt = joinF
      <$> switch (short 'q' <> long "quiet")
      <*> OB.argument text (metavar "SEP")
      <*> remainer
    trimOpt = trimF
      <$> switch (short 'q' <> long "quiet")
      <*> switch (short 'l' <> long "left")
      <*> switch (short 'r' <> long "right")
      <*> option text (short 'c' <> long "chars" <> metavar "CHARS")
      <*> remainer
    splitOpt = splitF
      <$> switch (short 'q' <> long "quiet")
      <*> option (Just <$> auto) (short 'm' <> long "max" <> metavar "MAX" <> value Nothing)
      <*> switch (short 'r' <> long "right")
      <*> OB.argument text (metavar "SEP")
      <*> remainer
    text = maybeReader (Just . T.pack)
    
lengthF :: Str.Stringy s => Bool -> [s] -> Fish ()
lengthF q ts = do
  unless q $ forM_ ts (echo . show . Str.length)
  if all (==Str.empty) ts then bad else ok

subF :: (FdWritable s,Str.Stringy s)
  => Bool -> Int -> Maybe Int -> [s] -> Fish ()
subF q start mlen ts = 
  map sub ts & \ts' -> do
  unless q $ echo $ Str.unlines ts'
  if ts == ts' then bad else ok
  where
    sub = (maybe id Str.take mlen) . Str.drop (start-1)    

joinF :: (FdWritable s,Str.Stringy s)
  => Bool -> s -> [s] -> Fish ()
joinF q sep ts = 
  Str.intercalate sep ts & \ts' -> do
  unless q $ echo ts'
  ts & \case
    [] -> bad
    [_] -> bad
    _ -> ok

trimF :: (FdWritable s,Str.Stringy s)
  => Bool -> Bool -> Bool -> s -> [s] -> Fish ()
trimF q l r cs ts = 
  map trim ts & \ts' -> do
  unless q $ echo $ Str.unlines ts'
  if ts == ts' then bad else ok
  where
    trim = f r l (`inStr` cs)
    f True False = Str.dropWhileEnd
    f False True = Str.dropWhile
    f _ _ = Str.dropAround

    inStr c t = isJust (Str.find (==c) t)


splitF :: (FdWritable s,Str.Stringy s)
  => Bool -> Maybe Int -> Bool -> s -> [s] -> Fish ()
splitF q m r sep ts = 
  let ts' = map (workSplitF m r sep) ts
  in do
    unless q $ echo $ Str.unlines ts'
    if ts == ts' then bad else ok


workSplitF :: Str.Stringy s
  => Maybe Int -> Bool -> s -> s -> s
workSplitF m r sep t =
  let ts = Str.splitOn sep t
      l = length ts
      mx = fromMaybe l m
      (a,b) = splitAt (if r then l - mx else mx) ts
   in if r
      then newl (sA a) (nwlA b)
      else newl (nwlA a) (sA b)
  where
    sA = Str.intercalate sep
    nwlA = Str.intercalate "\n"
    newl a b = a <> (if a == "" || b == "" then "" else "\n") <> b



