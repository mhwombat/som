------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOM
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A Kohonen Self-organising Map (SOM). A SOM maps input patterns onto
-- a regular grid (usually two-dimensional) where each node in the grid
-- is a model of the input data, and does so using a method which
-- ensures that any topological relationships within the input data are
-- also represented in the grid. This implementation supports the use
-- of non-numeric patterns.
--
-- In layman's terms, a SOM can be useful when you you want to discover
-- the underlying structure of some data. A tutorial is available at
-- <https://github.com/mhwombat/som/wiki>.
--
-- NOTES:
--
-- * Version 5.0 fixed a bug in the @`decayingGaussian`@ function. If
--   you use @`defaultSOM`@ (which uses this function), your SOM
--   should now learn more quickly.
--
-- * The @gaussian@ function has been removed because it is not as
--   useful for SOMs as I originally thought. It was originally designed
--   to be used as a factor in a learning function. However, in most
--   cases the user will want to introduce a time decay into the
--   exponent, rather than simply multiply by a factor.
--
-- References:
--
-- * Kohonen, T. (1982). Self-organized formation of topologically
--   correct feature maps. Biological Cybernetics, 43 (1), 59–69.
------------------------------------------------------------------------

module Data.Datamining.Clustering.SOM
  (
    -- * Construction
    SOM(..),
    -- * Deconstruction
    toGridMap,
    -- * Learning functions
    decayingGaussian,
    stepFunction,
    constantFunction,
    -- * Advanced control
    trainNeighbourhood
  ) where

import           Data.Datamining.Clustering.SOMInternal

