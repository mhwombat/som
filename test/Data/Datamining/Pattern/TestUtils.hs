------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.TestUtils
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Test utilities
--
------------------------------------------------------------------------
module Data.Datamining.Pattern.TestUtils
  (
    realFloatDiff,
    makeOrdFractionalSimilar
  ) where

import           Data.Datamining.Pattern.Numeric (maxNonInfiniteValue,
                                                  minNonInfiniteValue)
--
-- Patterns that are the same type as the learning rate.
-- If the learning rate falls outside the expected range [0,1],
-- you are likely to get strange behaviour.
-- Thus, these functions should not be used in production.
--

-- | Returns a number between 0 and 1 which indicates how different
--   the two inputs are.
--   A result of 0 indicates that the inputs are identical.
--   A result of 1 indicates that the inputs are maximally distant;
--   i.e., one is the most positive non-infinite value the type
--   can hold, and the other is the most negative non-infinite value.
realFloatDiff :: RealFloat a => a -> a -> a
realFloatDiff x y = abs (x/2 - y/2) / halfMaxDiff
  -- divide by two so we don't overflow or underflow
  where halfMaxDiff = maxVal/2 - minVal/2
        maxVal = maxNonInfiniteValue 0
        minVal = minNonInfiniteValue 0

-- | @'makeSimilar' t r x@ adjusts the pattern @x@ to make it
--   closer to the target @t@.
--   The amount of adjustment is controlled by @r@,
--   which should be between 0 and 1.
--   Larger values of @r@ permit more adjustment.
--   If @r@=1, the result will be identical to @t@.
--   If @r@=0, the result will be identical to @x@.
--   If @r@ < 0 or @r@ > 1, an error is reported.
--   The code itself only requires the contraints @Num a@ and @Ord a@.
--   But without the @Fractional a@ constraint, you would only be able
--   to have learning rates of 0 and 1, which isn't very useful.
makeOrdFractionalSimilar :: (Fractional a, Ord a) => a -> a -> a -> a
makeOrdFractionalSimilar t r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = x + r*(t - x)

