{-# language LambdaCase, OverloadedStrings #-}
{-| Module      : Slice
    Description : Implements fish style array slicing.
-}
module HFish.Interpreter.Slice (
  readIndices
  ,writeIndices
  ,dropIndices
) where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Util

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.Tuple
import Data.Monoid
import Data.Bool
import Data.Bifunctor
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

-- | A collection of slices, each of which consists of:
--
--   * A boolean, indicating if the slice is "reversed"
--   * A pair of Ints, corresponding to the ends of the slice.
type Slices = [(Bool,(Int,Int))]

-- | Pretty show slices.
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
    
-- | Create 'Slices' from the length of an array and given indices.
mkSlices :: Int -> Seq (Int,Int) -> Either T.Text Slices
mkSlices l xs = 
  L.sortOn (fst . snd)
  . map markSwap
  <$> forM (F.toList xs) normalise
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

-- | Variant of 'mkSlices'.
makeSlices :: Int -> Seq (Int,Int) -> Fish Slices
makeSlices len = 
  either errork return
  . mkSlices len

-- | Read values at given indices from variable.
readIndices :: Seq (Int,Int) -> Var -> Fish (Seq Str)
readIndices indices (Var _ l xs) = do
  slcs <- makeSlices l indices
  work 0 slcs xs & maybe err return
  where
    work :: Int -> Slices -> Seq Str -> Maybe (Seq Str)
    work n slcs xs = slcs & \case
      [] -> Just empty
      (b,(i,j)):rest -> do
        (_,xs') <- splitAtMaybe (i-n) xs
        (ys,_) <- splitAtMaybe (j-i+1) xs'
        (<>) (mbRev b ys) <$> work i rest xs'
    err = errork
      $ "readIndices: something went wrong..."
    

-- | Write values into variable at given indices.
--   May fail if the ranges overlap.
writeIndices :: Seq (Int,Int) -> Var -> Seq Str -> Fish Var
writeIndices indices (Var ex l xs) ys = do
  slcs <- makeSlices l indices
  Var ex l
    <$> either errork return
      (work 0 slcs xs ys)
  where
    work :: Int -> Slices -> Seq Str -> Seq Str -> Either T.Text (Seq Str)
    work n slcs xs ys = slcs & \case
      [] -> if Seq.null ys then Right xs else Left tooManyErr
      (b,(i,j)):rest -> do
        (zs,_,xs') <- triSplit (i-n) (j-n) xs
            `maybeToEither` invalidIndicesErr (b,(i,j))
        
        (rs,ys') <- splitAtMaybe (j-i+1) ys
            `maybeToEither` tooFewErr
        
        (<>) (zs <> mbRev b rs) <$> work (j+1) rest xs' ys'
    
    tooFewErr = "Too few values to write."
    tooManyErr = "Too many values to write."
    invalidIndicesErr slc =
      "Invalid indices (out of bounds or overlapping) at slice: "
       <> showSlices [slc]

-- | Drop indices from a variable.
dropIndices :: Seq (Int,Int) -> Var -> Fish Var
dropIndices indices (Var ex l xs) = do
  slcs <- makeSlices l indices
  ys <- maybe (err slcs) return (work 0 slcs xs)
  return $ Var ex (Seq.length ys) ys
  where
    work :: Int -> [(t, (Int, Int))] -> Seq a -> Maybe (Seq a)
    work n slcs xs = case slcs of
      [] -> Just xs
      (_,(i,j)):rest -> do
        (ys,_,zs) <- triSplit (i-n) (j-n) xs
        (<>) ys <$> work (j+1) rest zs
    err = errork . invalidIndicesErr
    invalidIndicesErr slcs =
      "Invalid indices (out of bounds or overlapping) at slice: "
       <> showSlices slcs


{- Helpers: -}

triSplit :: Int -> Int -> Seq a -> Maybe (Seq a,Seq a,Seq a)
triSplit i j xs = do
  (zs,xs') <- splitAtMaybe i xs
  (xs'',ts) <- splitAtMaybe (j-i+1) xs'
  Just (zs,xs'',ts)

mbRev :: Bool -> Seq a -> Seq a
mbRev = bool id Seq.reverse


