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
    SOM,
    defaultSOM,
    customSOM,
    gaussian,
    decayingGaussian,
    toGridMap
  ) where

import Data.Datamining.Clustering.SOMInternal (SOM, defaultSOM,
  customSOM, gaussian, decayingGaussian, toGridMap)

{- $Vector
If you wish to use a SOM with raw numeric vectors, use @no-warn-orphans@
and add the following to your code:

> instance (Floating a, Fractional a, Ord a, Eq a) ⇒ Pattern [a] a where
>   difference = euclideanDistanceSquared
>   makeSimilar = adjustVector
-}

