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
import qualified Control.DeepSeq as DeepSeq

-- todo: - Introduce 2 different slices,
--       one for internal use here, one for
--       one for the api.
--       - do mkSlices internally only



{- Implements fish style array slicing -}

-- | A collection of slices, eachof which consists of:
--
--   * A boolean, indicating if the slice is "reversed"
--   * A pair of Ints, corresponding to the ends of the slice.
type Slices = [(Bool,(Int,Int))]

mkSlices :: Int -> [(Int,Int)] -> Either T.Text Slices
mkSlices l xs = 
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
      | 0 < i && i <= l = Right (i - 1)
      | -l <= i && i < 0 = Right (l + i)
      | otherwise = Left $
        "Index \"" <> showText i <> "\" is out of bounds"

makeSlices :: Int -> [(Int,Int)] -> Fish Slices
makeSlices i xs = 
  either errork return $ mkSlices i xs

isEmptySlice :: Slices -> Bool
isEmptySlice = (==[])

readSlices :: Slices -> Var -> [T.Text]
readSlices slcs (Var _ _ xs) = work 0 slcs xs
  where
    work n slcs xs = slcs & \case
      [] -> []
      (b,(i,j)):rest -> 
        splitAt (i-n) xs & \(_,xs') ->
          splitAt (j-i+1) xs' & \(ys,_) ->
            mbRev b ys ++ work i rest xs'

{- writeSlices may fail if the ranges overlap -}
writeSlices :: Slices -> Var -> [T.Text] -> Fish Var
writeSlices slcs (Var ex l xs) ys =
  Var ex l
    <$> either errork return
      (work 0 slcs xs ys)
  where
    work :: Int -> Slices -> [T.Text] -> [T.Text] -> Either T.Text [T.Text]
    work n slcs xs ys = slcs & \case
      [] -> bool (Left tooManyErr) (Right xs) (isEmpty ys)
      (b,(i,j)):rest -> 
        triSplit (i-n) (j-i+1) xs & \case
          Nothing -> Left $ invalidIndicesErr (b,(i,j))
          Just (zs,_,xs') ->
            splitAtMaybe (j-i+1) ys & \case
              Nothing -> Left tooFewErr
              Just (rs,ys') ->
                (++) <$> Right (zs ++ mbRev b rs)
                <*> work (j+1) rest xs' ys'
    
    tooFewErr = "Too few values to write."
    tooManyErr = "Too many values to write."
    invalidIndicesErr slc =
      "Invalid indices (out of bounds or overlapping) at slice: "
       <> showSlices [slc]

    maybeToEither :: Maybe a -> b -> Either b a
    maybeToEither ma b = maybe (Left b) Right ma

{- drop the slices from an array -}
dropSlices :: Slices -> Var -> Fish Var
dropSlices slcs (Var ex l xs) =
  uncoverErrors (work 0 slcs xs)
  <$$> \ys -> Var ex (length ys) ys
  where
    work n slcs xs = slcs & \case
      [] -> xs
      (_,(i,j)):rest ->
        splitAt (i-n) xs & \(ys,xs') ->
          splitAt (j-i+1) xs' & \(_,zs) ->
            ys ++ work i rest zs

showSlices :: Slices -> T.Text
showSlices slcs = 
  T.pack . arrify . unwords
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

uncoverErrors :: DeepSeq.NFData a => a -> Fish a
uncoverErrors f =
  liftIO (E.try . E.evaluate . DeepSeq.force $ f)
  >>= \case
    Left e -> errork $ showText (e :: E.ErrorCall)
    Right r -> return r
