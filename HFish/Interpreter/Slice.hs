{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Slice (
  Slices
  ,makeSlices
  ,readSlices
  ,writeSlices
  ,dropSlices
  ,isEmptySlice
) where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Util

import qualified Data.List as L
import qualified Data.Text as T
import Data.Tuple
import Data.Monoid
import Data.Bool
import Data.Bifunctor
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E

{- Implements fish style array slicing -}


-- | A collection of slices, eachof which consists of:
--
--   * A boolean, indicating if the slice is "reversed"
--   * A pair of Ints, corresponding to the ends of the slice.
type Slices = [(Bool,(Int,Int))]

isEmptySlice :: Slices -> Bool
isEmptySlice = (==[])

makeSlices :: Int -> [(Int,Int)] -> Fish Slices
makeSlices l xs = 
  L.sortOn (fst . snd)
  . map markSwap
  <$> forM xs normalise
  where
    normalise (i,j) = do
      a <- index i
      b <- index j
      return (a,b)
    markSwap (i,j) = bool (False,(i,j)) (True,(j,i)) (i>j)
    index i
      | 0 < i && i <= l = return (i - 1)
      | -l <= i && i < 0 = return (l + i)
      | otherwise = errork
        $ "Index \"" <> showText i <> "\" is out of bounds"

readSlices :: Slices -> [a] -> [a]
readSlices = work 0
  where
    work n slcs xs = slcs & \case
      [] -> []
      (b,(i,j)):rest -> 
        splitAt (i-n) xs & \(_,xs') ->
          splitAt (j-i+1) xs' & \(ys,_) ->
            mbRev b ys ++ work i rest xs'

{- writeSlices may fail if the ranges overlap -}
writeSlices :: Show a => Slices -> [a] -> [a] -> Fish [a]
writeSlices slcs xs ys =
  liftIO (E.try $ E.evaluate $ work 0 slcs xs ys)
  >>= \case
    Left e -> errork $ showText (e :: E.ErrorCall)
    Right zs -> return zs
  where
    work n slcs xs ys = slcs & \case
      [] -> bool tooManyErr xs (isEmpty ys)
      (b,(i,j)):rest -> 
        triSplit (i-n) (j-i+1) xs & \case
          Nothing -> invalidIndicesErr slcs
          Just (zs,_,xs') ->
            splitAtMaybe (j-i+1) ys & \case
              Nothing -> tooFewErr
              Just (rs,ys') ->
                zs ++ mbRev b rs
                ++ work (j+1) rest xs' ys'
    
    tooFewErr = error "Too few values to write."
    tooManyErr = error "Too many values to write."
    invalidIndicesErr slcs = error
      $ "Invalid indices (out of bounds or overlapping): "
        ++ showSlices slcs


{- drop the slices from an array -}
dropSlices :: Show a => Slices -> [a] -> Fish [a]
dropSlices slcs xs =
  liftIO (E.try $ E.evaluate $ work 0 slcs xs)
  >>= \case
    Left e -> errork $ showText (e :: E.ErrorCall)
    Right zs -> return zs
  where
    work n slcs xs = slcs & \case
      [] -> xs
      (_,(i,j)):rest ->
        splitAt (i-n) xs & \(ys,xs') ->
          splitAt (j-i+1) xs' & \(_,zs) ->
            ys ++ work i rest zs

showSlices :: Slices -> String
showSlices slcs = 
  arrify . unwords
  $ map (sugar . unNormalise . unMarkSwap) slcs
  where
    unMarkSwap (b,(i,j)) = bool id swap b (i,j)
    unNormalise = bimap unIndex unIndex
    unIndex i = i+1
    arrify s = "[" ++ s ++ "]"
    sugar (i,j) = show i ++ ".." ++ show j
    

triSplit :: Int -> Int -> [a] -> Maybe ([a],[a],[a])
triSplit i j xs = do
  (zs,xs') <- splitAtMaybe i xs
  (xs'',ts) <- splitAtMaybe j xs'
  Just (zs,xs'',ts)

mbRev :: Bool -> [a] -> [a]
mbRev = bool id reverse

isEmpty :: [a] -> Bool
isEmpty = \case
  [] -> True
  _ -> False
