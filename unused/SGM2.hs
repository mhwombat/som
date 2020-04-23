------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM2
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A Self-generating Model (SGM). An SGM maps input patterns
-- onto a set, where each element in the set is a model of the input
-- data. An SGM is like a Kohonen Self-organising Map (SOM), except:
--
-- * Instead of a grid, it uses a simple set of unconnected models.
--   Since the models are unconnected, only the model that best matches
--   the input is ever updated. This makes it faster, however,
--   topological relationships within the input data are not preserved.
-- * New models are created on-the-fly when no existing model is
--   similar enough to an input pattern. If the SGM is at capacity,
--   the least useful model will be deleted.
--
-- This implementation supports the use of non-numeric patterns.
--
-- In layman's terms, a SGM can be useful when you you want to build
-- a set of models on some data. A tutorial is available at
-- <https://github.com/mhwombat/som/wiki>.
--
-- References:
--
-- * Amy de Buitléir, Mark Daly, and Michael Russell.
--   The Self-generating Model: an Adaptation of the Self-organizing Map
--   for Intelligent Agents and Data Mining.
--   In: Artificial Life and Intelligent Agents: Second International
--   Symposium, ALIA 2016, Birmingham, UK, June 14-15, 2016,
--   Revised Selected Papers.
--   Ed. by Peter R. Lewis et al. Springer International Publishing,
--   2018, pp. 59–72.
--   Available at http://amydebuitleir.eu/publications/.
--
-- * Amy de Buitléir, Michael Russell, and Mark Daly.
--   Wains: A pattern-seeking artificial life species.
--   Artificial Life, (18)4:399–423, 2012.
--   Available at http://amydebuitleir.eu/publications/.
--
-- * Kohonen, T. (1982). Self-organized formation of topologically
--   correct feature maps. Biological Cybernetics, 43 (1), 59–69.
------------------------------------------------------------------------

module Data.Datamining.Clustering.SGM2
  (
    -- * Construction
    SGM(..),
    makeSGM,
    -- * Deconstruction
    time,
    isEmpty,
    size,
    modelMap,
    counterMap,
    modelAt,
    -- * Learning and classification
    exponential,
    classify,
    trainAndClassify,
    train,
    trainBatch
  ) where

import           Data.Datamining.Clustering.SGM2Internal

