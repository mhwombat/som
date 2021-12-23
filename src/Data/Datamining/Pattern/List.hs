------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.List
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for using lists as patterns.
--
------------------------------------------------------------------------
module Data.Datamining.Pattern.List
  (
    makeSimilar,
    diff,
    weightedDiff,
    magnitudeSquared,
    euclideanDistanceSquared
  ) where

-- | @'makeSimilar' f ts r xs@ uses the function @f@ to adjust
--   each element of @x@ to move it closer to the corresponding element
--   of @ts@.
--   The result will be the length of @ts@ or @xs@, whichever is
--   shorter.
--   The amount of adjustment is controlled by @r@,
--   which should normally be between 0 and 1 (enforcement of that
--   constraint is left up to the function @f@).
--   Larger values of @r@ permit more adjustment.
--   If @r@=1, the result will be identical to the @ts@.
--   If @r@=0, the result will be identical to @xs@
--   (apart from the truncation that occurs when @ts@
--   is shorter than @xs@).
makeSimilar :: (a -> b -> a -> a) -> [a] -> b -> [a] -> [a]
makeSimilar f ts r xs = zipWith (flip f r) ts xs

-- | @'diff' f xs ys@ returns a number between 0 and 1 which indicates
--   how different the input lists @xs@ and @ys@ are.
--   If @xs@ and @ys@ have the same length, the result is the mean of
--   the difference between corresponding pairs of list elements,
--   as calculated by @f@.
--   If the lists are of different lengths, @1@ is returned.
diff :: Fractional b => (a -> a -> b) -> [a] -> [a] -> b
diff f xs ys
  | null xs && null ys = 0
  | null deltas       = 1 -- lists have different lengths
  | otherwise         = mean deltas
  where deltas = zipWith f xs ys
        mean zs = sum zs / (fromIntegral $ length zs)

-- | @'weightedDiff' ws f xs ys@ returns a number between 0 and 1
--   which indicates how different the input lists @xs@ and @ys@ are.
--   If @xs@ and @ys@ have the same length, the result
--   is the weighted sum of differences between corresponding pairs
--   of list elements, as calculated by @f@.
--   If the lists are of different lengths, @1@ is returned.
weightedDiff :: Num b => [b] -> (a -> a -> b) -> [a] -> [a] -> b
weightedDiff ws f xs ys
  | null xs && null ys = 0
  | null deltas       = 1 -- lists have different lengths
  | otherwise         = sum $ zipWith (*) ws deltas
  where deltas = zipWith f xs ys

--
-- Utilities for numeric vectors
--

-- | Returns the sum of the squares of the elements of a vector.
magnitudeSquared :: Num a => [a] -> a
magnitudeSquared xs =  sum $ map (\x -> x*x) xs

-- | Calculates the square of the Euclidean distance between two
--   vectors.
euclideanDistanceSquared :: Num a => [a] -> [a] -> a
euclideanDistanceSquared xs ys = magnitudeSquared $ zipWith (-) xs ys
