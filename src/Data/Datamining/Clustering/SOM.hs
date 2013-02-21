------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOM
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A Kohonen Self-organising Map (SOM). A SOM maps input patterns onto a 
-- regular grid (usually two-dimensional) where each node in the grid is
-- a model of the input data, and does so using a method which ensures 
-- that any topological relationships within the input data are also 
-- represented in the grid. This implementation supports the use of 
-- non-numeric patterns.
--
-- In layman's terms, a SOM can be useful when you you want to discover
-- the underlying structure of some data. A tutorial is available at
-- <https://github.com/mhwombat/som/wiki>
--
-- References:
--
-- * Kohonen, T. (1982). Self-organized formation of topologically 
--   correct feature maps. Biological Cybernetics, 43 (1), 59–69.
--
------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Data.Datamining.Clustering.SOM
  (
    -- Patterns
    Pattern(..),
    -- * Using the SOM
    train,
    trainBatch,
    classify,
    classifyAndTrain,
    diffs,
    differences, -- TO BE REMOVED
    diffAndTrain,
    -- * Numeric vectors as patterns
    -- ** Normalised vectors
    normalise,
    NormalisedVector,
    -- ** Scaled vectors
    scale,
    ScaledVector,
    -- ** Useful functions
    -- $Vector
    adjustVector,
    euclideanDistanceSquared,
    gaussian
  ) where

import Data.Datamining.Clustering.SOMInternal (adjustVector, classify, 
  classifyAndTrain, diffs, differences, diffAndTrain, 
  euclideanDistanceSquared, normalise, NormalisedVector, scale,
  ScaledVector, train, trainBatch, Pattern(..))

-- | Calculates @c/e/^(-d^2/2w^2)@.
--   This form of the Gaussian function is useful as a learning rate
--   function. In @'gaussian' c w d@, @c@ specifies the highest learning
--   rate, which will be applied to the SOM node that best matches the
--   input pattern. The learning rate applied to other nodes will be 
--   applied based on their distance @d@ from the best matching node. 
--   The value @w@ controls the \'width\' of the Gaussian. Higher values
--   of @w@ cause the learning rate to fall off more slowly with 
--   distance.
gaussian ∷ Double → Double → Int → Double
gaussian c w d = c * exp (-d'*d'/(2*w*w))
  where d' = fromIntegral d

{- $Vector
If you wish to use a SOM with raw numeric vectors, use @no-warn-orphans@
and add the following to your code:

> instance (Floating a, Fractional a, Ord a, Eq a) ⇒ Pattern [a] a where
>   difference = euclideanDistanceSquared
>   makeSimilar = adjustVector
-}

