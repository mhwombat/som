------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for identifying patterns in data.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Data.Datamining.Pattern
  (
    -- * Patterns
    Pattern(..),
    -- * Numbers as patterns
    -- $Num
    adjustNum,
    absDifference,
    -- * Numeric vectors as patterns
    -- ** Raw vectors
    -- $Vector
    adjustVector,
    euclideanDistanceSquared,
    magnitudeSquared,
    -- ** Normalised vectors
    NormalisedVector,
    normalise,
    -- ** Scaled vectors
    ScaledVector,
    scale,
    scaleAll
  ) where

import Data.List (foldl')

-- | A pattern to be learned or classified.
class Pattern p where
  type Metric p
  -- | Compares two patterns and returns a /non-negative/ number
  --   representing how different the patterns are. A result of @0@
  --   indicates that the patterns are identical.
  difference :: p -> p -> Metric p
  -- | @'makeSimilar' target amount pattern@ returns a modified copy of
  --   @pattern@ that is more similar to @target@ than @pattern@ is. The
  --   magnitude of the adjustment is controlled by the @amount@
  --   parameter, which should be a number between 0 and 1. Larger
  --   values for @amount@ permit greater adjustments. If @amount@=1,
  --   the result should be identical to the @target@. If @amount@=0,
  --   the result should be the unmodified @pattern@.
  makeSimilar :: p -> Metric p -> p -> p

--
-- Using numbers as patterns.
--

absDifference :: Num a => a -> a -> a
absDifference x y = abs (x - y)

adjustNum :: (Num a, Ord a, Eq a) => a -> a -> a -> a
adjustNum target r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | r == 1     = x
  | otherwise = adjustNum' r target x

-- Note that parameters are swapped
adjustNum' :: Num a => a -> a -> a -> a
adjustNum' r target x = x + r*(target - x)

{- $Num
If you wish to use, say, a @Double@ as a pattern, one option is to
use @no-warn-orphans@ and add the following to your code:

> instance Double => Pattern Double where
>   type Metric Double = Double
>   difference = absDifference
>   makeSimilar = adjustNum
-}

--
-- Using numeric vectors as patterns.
--

magnitudeSquared :: Num a => [a] -> a
magnitudeSquared xs =  sum $ map (\x -> x*x) xs

-- | Calculates the square of the Euclidean distance between two
--   vectors.
euclideanDistanceSquared :: Num a => [a] -> [a] -> a
euclideanDistanceSquared xs ys = magnitudeSquared $ zipWith (-) xs ys

-- | @'adjustVector' target amount vector@ adjusts @vector@ to move it
--   closer to @target@. The amount of adjustment is controlled by the
--   learning rate @r@, which is a number between 0 and 1. Larger values
--   of @r@ permit more adjustment. If @r@=1, the result will be
--   identical to the @target@. If @amount@=0, the result will be the
--   unmodified @pattern@.
adjustVector :: (Num a, Ord a, Eq a) => [a] -> a -> [a] -> [a]
adjustVector xs r ys
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | r == 1     = xs
  | otherwise = zipWith (adjustNum' r) xs ys

-- | A vector that has been normalised, i.e., the magnitude of the
--   vector = 1.
data NormalisedVector a = NormalisedVector [a] deriving Show

-- | Normalises a vector
normalise :: Floating a => [a] -> NormalisedVector a
normalise xs = NormalisedVector $ map (/x) xs
  where x = norm xs

norm :: Floating a => [a] -> a
norm xs = sqrt $ sum (map f xs)
  where f x = x*x

instance (Floating a, Fractional a, Ord a, Eq a) =>
    Pattern (NormalisedVector a) where
  type Metric (NormalisedVector a) = a
  difference (NormalisedVector xs) (NormalisedVector ys) =
    euclideanDistanceSquared xs ys
  makeSimilar (NormalisedVector xs) r (NormalisedVector ys) =
    normalise $ adjustVector xs r ys

-- | A vector that has been scaled so that all elements in the vector
--   are between zero and one. To scale a set of vectors, use
--   @'scaleAll'@. Alternatively, if you can identify a maximum and
--   minimum value for each element in a vector, you can scale
--   individual vectors using @'scale'@.
data ScaledVector a = ScaledVector [a] deriving Show

-- | Given a vector @qs@ of pairs of numbers, where each pair represents
--   the maximum and minimum value to be expected at each index in
--   @xs@, @'scale' qs xs@ scales the vector @xs@ element by element,
--   mapping the maximum value expected at that index to one, and the
--   minimum value to zero.
scale :: Fractional a => [(a,a)] -> [a] -> ScaledVector a
scale qs xs = ScaledVector $ zipWith scaleValue qs xs

-- | Scales a set of vectors by determining the maximum and minimum
--   values at each index in the vector, and mapping the maximum
--   value to one, and the minimum value to zero.
scaleAll :: (Fractional a, Ord a) => [[a]] -> [ScaledVector a]
scaleAll xss = map (scale qs) xss
  where qs = quantify xss

scaleValue :: Fractional a => (a,a) -> a -> a
scaleValue (minX,maxX) x = (x - minX) / (maxX-minX)

quantify :: Ord a => [[a]] -> [(a,a)]
quantify xss = foldl' quantify' qs (tail xss)
  where qs = zip (head xss) (head xss)

quantify' :: Ord a => [(a,a)] -> [a] -> [(a,a)]
quantify' = zipWith f
  where f (minX, maxX) x = (min minX x, max maxX x)

instance (Fractional a, Ord a, Eq a) => Pattern (ScaledVector a) where
  type Metric (ScaledVector a) = a
  difference (ScaledVector xs) (ScaledVector ys) =
    euclideanDistanceSquared xs ys
  makeSimilar (ScaledVector xs) r (ScaledVector ys) =
    ScaledVector $ adjustVector xs r ys

{- $Vector
If you wish to use raw numeric vectors as a pattern, one option is to
use @no-warn-orphans@ and add the following to your code:

> instance (Floating a, Fractional a, Ord a, Eq a) => Pattern [a] where
>   type Metric [a] = a
>   difference = euclideanDistanceSquared
>   makeSimilar = adjustVector
-}
