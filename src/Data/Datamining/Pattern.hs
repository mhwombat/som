------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Pattern
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for identifying patterns in data.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Datamining.Pattern
  (
    -- * Numbers as patterns
    adjustNum,
    absDifference,
    -- * Numeric vectors as patterns
    -- ** Raw vectors
    adjustVector,
    adjustVectorPreserveLength,
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

import           Data.List (foldl')

--
-- Using numbers as patterns.
--

-- | Returns the absolute difference between two numbers.
absDifference :: Num a => a -> a -> a
absDifference x y = abs (x - y)

-- | Adjusts a number to make it more similar to the target.
adjustNum :: (Num a, Ord a, Eq a) => a -> a -> a -> a
adjustNum target r x
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | otherwise = adjustNum' r target x

-- Note that parameters are swapped
adjustNum' :: Num a => a -> a -> a -> a
adjustNum' r target x = x + r*(target - x)

--
-- Using numeric vectors as patterns.
--

-- | Returns the sum of the squares of the elements of a vector.
magnitudeSquared :: Num a => [a] -> a
magnitudeSquared xs =  sum $ map (\x -> x*x) xs

-- | Calculates the square of the Euclidean distance between two
--   vectors.
euclideanDistanceSquared :: Num a => [a] -> [a] -> a
euclideanDistanceSquared xs ys = magnitudeSquared $ zipWith (-) xs ys

-- | @'adjustVector' target amount vector@ adjusts each element of
--   @vector@ to move it closer to the corresponding element of
--   @target@.
--   The amount of adjustment is controlled by the learning rate
--   @amount@, which is a number between 0 and 1.
--   Larger values of @amount@ permit more adjustment.
--   If @amount@=1, the result will be identical to the @target@.
--   If @amount@=0, the result will be the unmodified @pattern@.
--   If @target@ is shorter than @vector@, the result will be the same
--   length as @target@.
--   If @target@ is longer than @vector@, the result will be the same
--   length as @vector@.
adjustVector :: (Num a, Ord a, Eq a) => [a] -> a -> [a] -> [a]
adjustVector ts r xs
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | r == 1     = ts
  | otherwise = zipWith (adjustNum' r) ts xs

-- | Same as @'adjustVector'@, except that the result will always be
--   the same length as @vector@.
--   This means that if @target@ is shorter than @vector@, the
--   "leftover" elements of @vector@ will be copied the result,
--   unmodified.
adjustVectorPreserveLength :: (Num a, Ord a, Eq a) => [a] -> a -> [a] -> [a]
adjustVectorPreserveLength ts r xs
  | r < 0     = error "Negative learning rate"
  | r > 1     = error "Learning rate > 1"
  | r == 1     = ts
  | otherwise = avpl ts r xs

avpl :: (Num a, Ord a, Eq a) => [a] -> a -> [a] -> [a]
avpl _ _ []          = []
avpl [] _ x          = x
avpl (t:ts) r (x:xs) = adjustNum' r t x : avpl ts r xs

-- | A vector that has been normalised, i.e., the magnitude of the
--   vector = 1.
newtype NormalisedVector a = NormalisedVector [a] deriving Show

-- | Normalises a vector
normalise :: Floating a => [a] -> NormalisedVector a
normalise xs = NormalisedVector $ map (/x) xs
  where x = norm xs

norm :: Floating a => [a] -> a
norm xs = sqrt $ sum (map f xs)
  where f x = x*x

-- | A vector that has been scaled so that all elements in the vector
--   are between zero and one. To scale a set of vectors, use
--   @'scaleAll'@. Alternatively, if you can identify a maximum and
--   minimum value for each element in a vector, you can scale
--   individual vectors using @'scale'@.
newtype ScaledVector a = ScaledVector [a] deriving Show

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
