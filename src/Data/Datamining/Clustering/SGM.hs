------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SGM
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
-- * de Buitléir, Amy, Russell, Michael and Daly, Mark. (2012). Wains:
--   A pattern-seeking artificial life species. Artificial Life, 18 (4),
--   399-423. 
-- 
-- * Kohonen, T. (1982). Self-organized formation of topologically 
--   correct feature maps. Biological Cybernetics, 43 (1), 59–69.
------------------------------------------------------------------------

module Data.Datamining.Clustering.SGM
  (
    -- * Construction
    SGM(..),
    makeSGM,
    -- * Deconstruction
    time,
    isEmpty,
    numModels,
    modelMap,
    counterMap,
    -- models,
    -- counters,
    -- * Learning and classification
    exponential,
    classify,
    trainAndClassify,
    train,
    trainBatch
  ) where

import Data.Datamining.Clustering.SGMInternal

