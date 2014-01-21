------------------------------------------------------------------------
-- |
-- Module      :  Data.Datamining.Clustering.SOM
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A modified Kohonen Self-organising Map (SOM) which supports a
-- time-independent learning function. (See
-- @'Data.Datamining.Clustering.SOM'@ for a description of a SOM.)
--
-- References:
--
-- * Rougier, N. & Boniface, Y. (2011). Dynamic self-organising map.
--   Neurocomputing, 74 (11), 1840-1847. 
--
-- * Kohonen, T. (1982). Self-organized formation of topologically 
--   correct feature maps. Biological Cybernetics, 43 (1), 59–69.
------------------------------------------------------------------------

module Data.Datamining.Clustering.DSOM
  (
    -- * Construction
    DSOM,
    defaultDSOM,
    customDSOM,
    rougierLearningFunction,
    -- * Deconstruction
    toGridMap,
    -- * Advanced control
    trainNeighbourhood
  ) where

import Data.Datamining.Clustering.DSOMInternal (DSOM, defaultDSOM,
  customDSOM, rougierLearningFunction, toGridMap, trainNeighbourhood)

