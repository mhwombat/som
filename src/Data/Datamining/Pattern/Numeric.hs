------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.Numeric
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for using numbers as patterns.
--
------------------------------------------------------------------------
module Data.Datamining.Pattern.Numeric
  (
    diff,
    makeSimilar,
    absDifference,
    minDouble,
    maxDouble,
    maxNonInfiniteValue,
    minNonInfiniteValue
  ) where

-- | @'makeSimilar' t r x@ adjusts the pattern @x@ to make it
--   closer to the target @t@.
--   The amount of adjustment is controlled by @r@,
--   which should be between 0 and 1.
--   Larger values of @r@ permit more adjustment.
--   If @r@=1, the result will be identical to @t@.
--   If @r@=0, the result will be identical to @x@.
--   If @r@ < 0 or @r@ > 1, an error is reported.
makeSimilar :: (Num a, Ord a) => a -> a -> a -> a
makeSimilar t r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = x + r*(t - x)

-- | Returns a number between 0 and 1 which indicates how different
--   the two inputs are.
--   A result of 0 indicates that the inputs are identical.
--   A result of 1 indicates that the inputs are maximally distant;
--   i.e., one is the most positive non-infinite value the type
--   can hold, and the other is the most negative non-infinite value.
diff :: RealFloat a => a -> a -> a
diff x y = abs (x/2 - y/2) / halfMaxDiff
  -- divide by two so we don't overflow or underflow
  where halfMaxDiff = maxVal/2 - minVal/2
        maxVal = maxNonInfiniteValue 0
        minVal = minNonInfiniteValue 0

maxNonInfiniteValue :: RealFloat a => a -> a
maxNonInfiniteValue a = encodeFloat m n
  where b = floatRadix a
        e = floatDigits a
        (_, e') = floatRange a
        m = b ^ e - 1
        n = e' - e

minNonInfiniteValue :: RealFloat a => a -> a
minNonInfiniteValue a = - (maxNonInfiniteValue a)

-- | Returns the absolute difference between two numbers.
absDifference :: Num a => a -> a -> a
absDifference x y = abs (x - y)

minDouble :: Double
minDouble = minNonInfiniteValue 0

maxDouble :: Double
maxDouble = maxNonInfiniteValue 0
