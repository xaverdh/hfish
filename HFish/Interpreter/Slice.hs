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
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.IO.Class

import Debug.Trace (trace)

-- | A collection of slices, each of which consists of:
--
--   * A boolean, indicating if the slice is "reversed"
--   * A pair of Ints, corresponding to the ends of the slice.
type Slices = Seq (Bool,(Int,Int))

    
-- | Create 'Slices' from the length of an array and given indices.
mkSlices :: Int -> Seq (Int,Int) -> Either T.Text Slices
mkSlices l xs = 
  fmap markSwap <$> forM xs normalise
  where
    normalise (i,j) = (,) <$> index i <*> index j
    markSwap (i,j) = bool (False,(i,j)) (True,(j,i)) (i>j)
    index i
      | 0 < i && i <= l = Right (i - 1)
      | -l <= i && i < 0 = Right (l + i)
      | otherwise = Left $
        "Index \"" <> showText i <> "\" is out of bounds"

-- | Fish variant of 'mkSlices'.
makeSlices :: Int -> Seq (Int,Int) -> Fish Slices
makeSlices len = eitherToFish . mkSlices len

-- | Read values at given indices from variable.
readIndices :: Seq (Int,Int) -> Var -> Fish (Seq Str)
readIndices indices (Var _ xs) = do
  slcs <- makeSlices (Seq.length xs) indices
  maybeToFish err $ F.foldrM f mempty slcs
  where
    f :: (Bool, (Int, Int)) -> Seq Str -> Maybe (Seq Str)
    f (b,(i,j)) acc = do
      (_,ys,_) <- triSplit i j xs
      return $ mbRev b ys <> acc
    err = "readIndices: something went wrong..."
    

-- | Write values into variable at given indices.
--   May fail if the ranges overlap.
writeIndices :: Seq (Int,Int) -> Var -> Seq Str -> Fish Var
writeIndices indices (Var ex xs) ys = do
  slcs <- makeSlices (Seq.length xs) indices
  (xs',ys') <- eitherToFish $ F.foldlM f (xs,ys) slcs
  if Seq.null ys'
    then return $ Var ex xs'
    else errork tooManyErr
  where
    f :: (Seq Str,Seq Str)
       -> (Bool, (Int, Int))
       -> Either T.Text (Seq Str,Seq Str)
    f (xs,ys) slc@(b,(i,j)) = do
      (hs,_,ts) <- triSplit i j xs
        `maybeToEither` err
      (rs,ys') <- splitAtMaybe (j-i+1) ys
        `maybeToEither` tooFewErr
      return (hs <> mbRev b rs <> ts, ys')
    
    tooFewErr = "Too few values to write."
    tooManyErr = "Too many values to write."
    err = "writeIndices: something went wrong..."

-- | Drop indices from a variable.
dropIndices :: Seq (Int,Int) -> Var -> Fish Var
dropIndices indices (Var ex xs) = do
  slcs <- makeSlices (Seq.length xs) indices
    <$$> sortOn (fst . snd)
  ys <- maybeToFish err (work 0 slcs xs)
  return $ Var ex ys
  where
    sortOn f = Seq.unstableSortBy $ \x y -> compare (f x) (f y)
    
    work :: Int -> Seq (t, (Int, Int)) -> Seq a -> Maybe (Seq a)
    work n slcs xs = case viewl slcs of
      EmptyL -> Just xs
      (_,(i,j)) :< rest ->
        n > j ? work n rest xs $ do
        (ys,_,zs) <- triSplit (max 0 (i-n)) (j-n) xs
        (<>) ys <$> work (j+1) rest zs
    
    err = "dropIndices: something went wrong..."

{- Helpers: -}

triSplit :: Int -> Int -> Seq a -> Maybe (Seq a,Seq a,Seq a)
triSplit i j xs = do
  (zs,xs') <- splitAtMaybe i xs
  (xs'',ts) <- splitAtMaybe (j-i+1) xs'
  Just (zs,xs'',ts)

mbRev :: Bool -> Seq a -> Seq a
mbRev = bool id Seq.reverse


