-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOMInternal
-- Copyright   :  (c) Amy de Buitléir 2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private @SOM@ internals. Most developers should
-- use @SOM@ instead. This module is subject to change without notice.
--
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, 
    FunctionalDependencies #-}

module Data.Datamining.Clustering.SOMInternal
  (
    adjustNode,
    adjustVector,
    classify,
    classifyAndTrain,
    differences,
    euclideanDistanceSquared,
    magnitudeSquared,
    normalise,
    NormalisedVector,
    scale,
    scaleAll,
    ScaledVector,
    train,
    trainBatch,
    Pattern(..)
  ) where

import Data.Eq.Unicode ((≡))
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Math.Geometry.Grid (distance, Grid)
import Math.Geometry.GridMap (GridMap, mapWithKey, toList)
import qualified Math.Geometry.GridMap as GM (map)

-- | A pattern to be learned or classified by a self-organising map.
class Pattern p v | p → v where
  -- | Compares two patterns and returns a /non-negative/ number representing 
  --   how different the patterns are. A result of @0@ indicates that the 
  --   patterns are identical.
  difference ∷ p → p → v
  -- | @'makeSimilar' target amount pattern@ returns a modified copy of
  --   @pattern@ that is more similar to @target@ than @pattern@ is. The 
  --   magnitude of the adjustment is controlled by the @amount@ parameter,
  --   which should be a number between 0 and 1. Larger values for @amount@
  --   permit greater adjustments. If @amount@=1, the result should be 
  --   identical to the @target@. If @amount@=0, the result should be the
  --   unmodified @pattern@.
  makeSimilar ∷ p → v → p → p

-- | @'classify' pattern c@ returns the position of the node in @c@ 
--   whose pattern best matches the input @pattern@.
classify ∷ (Ord v, Pattern p v) ⇒ GridMap g k p → p → k
classify c pattern = 
  fst $ minimumBy (comparing snd) $ toList $ differences pattern c

-- | @pattern \`'differences'\` c@ returns the positions of all nodes in 
--   @c@, paired with the difference between @pattern@ and the node's 
--   pattern.
differences ∷ Pattern p v ⇒ p → GridMap g k p → GridMap g k v
differences pattern = GM.map (pattern `difference`)

-- | If @f d@ is a function that returns the learning rate to apply to a node 
--   based on its distance @d@from the node that best matches the input 
--   pattern, then @'train' f c pattern@ returns a modified copy of the
--   classifier @c@ that has partially learned the @target@.
train ∷ (Ord v, Pattern p v, Grid g s k) ⇒
  (Int → v) → GridMap g k p → p → GridMap g k p
train f c pattern = snd $ classifyAndTrain f c pattern

-- | Same as @train@, but applied to multiple patterns.
trainBatch ∷ (Ord v, Grid g s k, Pattern p v) ⇒
  (Int → v) → GridMap g k p → [p] → GridMap g k p
trainBatch f = foldl' (train f)

-- | If @f@ is a function that returns the learning rate to apply to a node
--   based on its distance from the node that best matches the @target@, then 
--   @'classifyAndTrain' f c target@ returns a tuple containing the position
--   of the node in @c@ whose pattern best matches the input @target@, and a
--   modified copy of the classifier @c@ that has partially learned the 
--   @target@.
classifyAndTrain ∷ (Eq k, Ord v, Pattern p v, Grid g s k) ⇒ 
  (Int → v) → GridMap g k p → p → (k, GridMap g k p)
classifyAndTrain f c pattern = (bmu, c')
  where bmu = classify c pattern
        dMap = mapWithKey (\k p → (distance k bmu c, p)) c
        lrMap = GM.map (\(d,p) → (f d, p)) dMap
        c' = GM.map (adjustNode pattern) lrMap

adjustNode ∷ (Pattern p v) ⇒ p → (v,p) → p
adjustNode target (r,p) = makeSimilar target r p

--
-- Using numeric vectors as patterns.
-- 

magnitudeSquared ∷ Num a ⇒ [a] → a
magnitudeSquared xs =  sum $ map (\x → x*x) xs

-- | Calculates the square of the Euclidean distance between two vectors.
euclideanDistanceSquared ∷ Num a ⇒ [a] → [a] → a
euclideanDistanceSquared xs ys = magnitudeSquared $ zipWith (-) xs ys

-- | @'adjustVector' target amount vector@ adjusts @vector@ to move it closer 
--   to @target@. The amount of adjustment is controlled by the learning rate
--   @r@, which is a number between 0 and 1. Larger values of @r@ permit more
--   adjustment. If @r@=1, the result will be identical to the @target@. If 
--   @amount@=0, the result will be the unmodified @pattern@.
adjustVector ∷ (Num a, Ord a, Eq a) ⇒ [a] → a → [a] → [a]
adjustVector xs r ys
  | r < 0 = error "Negative learning rate"
  | r > 1 = error "Learning rate > 1"
  | r ≡ 1 = xs
  | otherwise        = zipWith (+) ys deltas
      where diffs = zipWith (-) xs ys
            deltas = map (r *) diffs

-- | A vector that has been normalised, i.e., the magnitude of the vector = 1.
data NormalisedVector a = NormalisedVector [a] deriving Show

-- | Normalises a vector
normalise ∷ Floating a ⇒ [a] → NormalisedVector a
normalise xs = NormalisedVector $ map (/x) xs
  where x = norm xs

norm ∷ Floating a ⇒ [a] → a
norm xs = sqrt $ sum (map f xs)
  where f x = x*x

instance (Floating a, Fractional a, Ord a, Eq a) ⇒ 
    Pattern (NormalisedVector a) a where
  difference (NormalisedVector xs) (NormalisedVector ys) = 
    euclideanDistanceSquared xs ys
  makeSimilar (NormalisedVector xs) r (NormalisedVector ys) = 
    normalise $ adjustVector xs r ys

-- | A vector that has been scaled so that all elements in the vector are
--   between zero and one. To scale a set of vectors, use @'scaleAll'@.
--   Alternatively, if you can identify a maximum and minimum value for
--   each element in a vector, you can scale individual vectors using
--   @'scale'@.
data ScaledVector a = ScaledVector [a] deriving Show

-- | Given a vector @qs@ of pairs of numbers, where each pair represents the
--   maximum and minimum value to be expected at each position in @xs@,
--   @'scale' qs xs@ scales the vector @xs@ element by element, mapping the 
--   maximum value expected at that position to one, and the minimum value to 
--   zero.
scale ∷ Fractional a ⇒ [(a,a)] → [a] → ScaledVector a
scale qs xs = ScaledVector $ zipWith scaleValue qs xs

-- | Scales a set of vectors by determining the maximum and minimum values at
--   each position in the vector, and mapping the maximum value to one, and 
--   the minimum value to zero.
scaleAll ∷ (Fractional a, Ord a) ⇒ [[a]] → [ScaledVector a]
scaleAll xss = map (scale qs) xss
  where qs = quantify xss

scaleValue ∷ Fractional a ⇒ (a,a) → a → a
scaleValue (minX,maxX) x = (x - minX) / (maxX-minX)

quantify ∷ Ord a ⇒ [[a]] → [(a,a)]
quantify xss = foldl' quantify' qs (tail xss)
  where qs = zip (head xss) (head xss)

quantify' ∷ Ord a ⇒ [(a,a)] → [a] → [(a,a)]
quantify' = zipWith f
  where f (minX, maxX) x = (min minX x, max maxX x)

instance (Fractional a, Ord a, Eq a) ⇒ Pattern (ScaledVector a) a where
  difference (ScaledVector xs) (ScaledVector ys) = 
    euclideanDistanceSquared xs ys
  makeSimilar (ScaledVector xs) r (ScaledVector ys) =
    ScaledVector $ adjustVector xs r ys

