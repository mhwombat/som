------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern.Numeric
-- Copyright   :  (c) 2017-2022 Amy de Buitléir
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
    boundedFractionalDiff,
    makeRealFracSimilar,
    boundedIntegralDiff,
    -- realFloatDiff,
    doubleDiff,
    makeOrdFractionalSimilar,
    makeIntegralSimilar,
    absDifference,
    minFloat,
    maxFloat,
    minDouble,
    maxDouble,
    maxNonInfiniteValue,
    minNonInfiniteValue
  ) where

--
-- Patterns that are floating point types
--

-- NOTE: Use of realToFrac should be OK because we're working with
-- bounded types and won't have any infinite values?

boundedFractionalDiff
  :: (Bounded a, Fractional a, Real a, Fractional b)
  => a -> a -> b
boundedFractionalDiff x y = realToFrac $ abs (y' - x') / (b' - a')
  where x' = realToFrac x :: Double
        y' = realToFrac y :: Double
        a = minBound `asTypeOf` x
        b = maxBound `asTypeOf` x
        a' = realToFrac a :: Double
        b' = realToFrac b :: Double

makeRealFracSimilar :: (Real a, Fractional a, Real b) => a -> b -> a -> a
makeRealFracSimilar t r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = realToFrac $ x' + r'*(t' - x')
  where t' = realToFrac t :: Double
        r' = realToFrac r :: Double
        x' = realToFrac x :: Double

-- -- TODO: Decide how to handle infinite values
-- realFloatDiff :: RealFloat a => a -> a -> a
-- realFloatDiff x y = abs (x/2 - y/2) / halfMaxDiff
--   -- divide by two so we don't overflow or underflow
--   where halfMaxDiff = maxVal/2 - minVal/2
--         maxVal = maxNonInfiniteValue 0
--         minVal = minNonInfiniteValue 0

-- TODO: Decide how to handle infinite values
doubleDiff :: Double -> Double -> Double
doubleDiff x y = abs (x/2 - y/2) / halfMaxDiff
  -- divide by two so we don't overflow or underflow
  where halfMaxDiff = maxDouble/2 - minDouble/2

-- TODO: Decide how to handle infinite values
makeOrdFractionalSimilar :: (Fractional a, Ord a) => a -> a -> a -> a
makeOrdFractionalSimilar t r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = x + r*(t - x)

--
-- Patterns that are integers
--

boundedIntegralDiff
  :: (Bounded a, Integral a, Fractional b)
  => a -> a -> b
boundedIntegralDiff x y = realToFrac $ abs (y' - x') / (b' - a')
  where x' = fromIntegral x :: Double
        y' = fromIntegral y :: Double
        a = minBound `asTypeOf` x
        b = maxBound `asTypeOf` x
        a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double

makeIntegralSimilar :: (Integral a, Real b) => a -> b -> a -> a
makeIntegralSimilar t r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = round $ x' + r'*(t' - x')
  where t' = fromIntegral t :: Double
        r' = realToFrac r :: Double
        x' = fromIntegral x :: Double

maxNonInfiniteValue :: RealFloat a => a -> a
maxNonInfiniteValue a = encodeFloat m n
  where b = floatRadix a
        e = floatDigits a
        (_, e') = floatRange a
        m = b ^ e - 1
        n = e' - e

--
-- Utilities for working with real values
--

minNonInfiniteValue :: RealFloat a => a -> a
minNonInfiniteValue a = - (maxNonInfiniteValue a)

-- | Returns the absolute difference between two numbers.
absDifference :: Num a => a -> a -> a
absDifference x y = abs (x - y)


minFloat :: Float
minFloat = minNonInfiniteValue 0

maxFloat :: Float
maxFloat = maxNonInfiniteValue 0

minDouble :: Double
minDouble = minNonInfiniteValue 0

maxDouble :: Double
maxDouble = maxNonInfiniteValue 0

